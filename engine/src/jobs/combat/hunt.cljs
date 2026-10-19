(ns jobs.combat.hunt
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.hunting :as hunting]
            [jobs.lib.need :as need]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]
            [jobs.lib.area :as area]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.ready :as ready]
            [jobs.lib.waits :as waits]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.vantage :as vantage]))

(def doc
  "Kill :count adult animals of the mob kind :mob within :radius of :center (the body's place at the first run; it waits :outside-area
  while the body is farther), collecting their drops, in one call that yields only while a child waits on the world.
  :mob may be a list of kinds or :food-animals (cow, pig, sheep, chicken): one child hunt per kind, each with its own :count and :keep.
  Never takes the last :keep adults of the kind (nil: 2 for animals, 0 for hostile mobs); babies are never targets. Animals the body
  spares (jobs.lib.animal-spare: kinds in ::off-limits, animals in a zone or plan of its own or a closed enclosure unless ::spare-pens
  is false) are never targets either. Weapons: the call's :use :tools (default the best carried); a hostile kind with none gets a sword
  first (jobs.lib.need, :fetch; a failed fetch ends the job with its cause).
  Needs: fit? (waits :low-health); at its start (one kind only) it waits :unarmed (hostile, no weapon, :fetch off), :no-color or
  :too-few (:in-range, :radius, :keep, :nearest) unless the kind is heard and not in sight, when it first steps up to four times
  toward the sound (jobs.lib.vantage).
  Returns {:killed n :reason r :spared s :remaining m}. :done for :count and, for any other reason, when the call's :least (default 1)
  were killed; else :nothing-possible with r the cause (:keep, :spared with :spared-by per rule, :none, :gave-up after
  jobs.lib.hunting/max-skips skipped animals in a row). A list of kinds: reason :count or :partial, data :kinds {mob result}, the
  first failed kind's ending the :because.")

(a/defargs args
  {:mob {:doc "mob type name of the animals to hunt, a list of them, or :food-animals (cow, pig, sheep, chicken); one hunt per kind, :count and :keep apply to each" :spec (a/or-of a/name? (a/coll-of a/name?) #{:food-animals "food-animals"}) :default "cow"}
   :color {:doc "sheep only: kill only sheep of this wool colour (e.g. \"white\"); nil: any" :spec a/name? :default nil}
   :count {:doc "animals to kill" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "animals within this many blocks of the body count" :spec (a/num-in 0 nil) :default 24}
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec a/position? :default nil}
   :keep {:doc "never kill the last this many adults of the kind within :radius (babies do not count); nil: 2 for animals, 0 for hostile mobs; 0 turns the rule off" :spec (a/int-in 0 nil) :default nil}
   :drops {:doc "item names to collect after a kill; nil: the kind's entry in the drops table, else every item" :spec (a/coll-of a/item?) :default nil}
   :fetch {:doc "get a sword when a hostile kind is hunted unarmed, with jobs.lib.need: true, a set of kinds or a map of limits; false (or without :tool) ends :unarmed" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds a hunt may take." :spec (a/num-in 0 nil)}
   ::sight-radius {:default 64 :doc "How far the :too-few wait looks for the nearest adult of the kind, in blocks." :spec (a/num-in 0 nil)}})

(def target [:mob :center])

(def defaults {:spend {:job-time ::job-time-s} :least 1 :use {:tools :attack} :checks {:safe? '(not (exposed))}})
(def safe? ready/safe-if-lit)
(def fit? ready/fit?)

(defn adults-within
  "The adult entities named :mob within radius blocks, nearest first, never players, items or babies
  (an entity is an adult unless its baby field is true)."
  [c radius]
  (->> (look/seen-objs (:primitives c) {:radius radius :names [(:mob (:args c))] :max 64})
       (remove #(contains? #{"player" "item"} (.-kind %)))
       (remove #(true? (.-baby %)))
       vec))

(defn coloured
  "The entities of the :color arg (all when it is nil); with a :color the sheared ones are left out, they drop no wool."
  [c es]
  (let [color (:color (:args c))]
    (cond->> es color (filter #(and (= color (.-color %)) (not (true? (.-sheared %))))))))

(defn present-any
  "The adult entities named :mob within :radius, of any colour."
  [c]
  (adults-within c (:radius (:args c))))

(defn present
  "The adult entities named :mob within :radius, of the :color when given."
  [c]
  (vec (coloured c (present-any c))))

(defn spare-rules
  "The present animals of the :color the body leaves alone, as {id rule} (jobs.lib.animal-spare/why: :off-limits or :pen);
  the pair rule is :keep's."
  [c]
  (let [pens (atom [])
        args {:spare-pens (spare/spare-pens?) :keep-adults -1}]
    (into {} (keep #(some->> (spare/why args (.-name %) 0 (spare/penned? c pens %) false) (vector (.-id %)))) (present c))))

(defn candidates
  "The present animals of the :color not skipped and not spared, nearest first."
  [c]
  (let [skipped (set (:skipped (ctx/mem c)))
        left (spare-rules c)
        here (u/self-pos c)]
    (->> (present c)
         (remove #(or (contains? skipped (.-id %)) (contains? left (.-id %))))
         (sort-by #(u/dist here (u/pos-of (.-pos %)))))))

(defn hostile?
  "Is the kind hostile: by its name (jobs.lib.cost/hostile?), else any entity of it within :radius reporting kind hostile (babies included)?"
  [c]
  (let [{:keys [mob radius]} (:args c)
        p (:primitives c)]
    (boolean (or (cost/hostile? (game/version-of p) {:name mob})
                 (some #(= "hostile" (.-kind %))
                       (look/seen-objs p {:radius radius :names [mob] :max 64}))))))

(defn keep-of
  "Adults of the kind never taken: the :keep arg when a number, else the value
  remembered in job memory, else 2 for animals and 0 for hostile mobs."
  [c]
  (or (:keep (:args c))
      (:keep (ctx/mem c))
      (if (hostile? c) 0 2)))

(defn too-few
  "The :too-few wait: :mob :keep, the adults :in-range of :radius and the :nearest one's distance (when one is in sight)."
  [c]
  (let [{:keys [mob radius]} (:args c)
        nearest (first (adults-within c (max radius (settings/get settings ::sight-radius))))]
    (cond-> {:reason :too-few :mob mob :keep (keep-of c) :in-range (count (present c)) :radius radius}
      nearest (assoc :nearest (js/Math.round (.-distance nearest))))))

(def food-animals ["cow" "pig" "sheep" "chicken"])

(defn kinds
  "The kinds of :mob when it names several (a list, or :food-animals); nil for one kind."
  [c]
  (let [mob (:mob (:args c))]
    (cond (contains? #{:food-animals "food-animals"} mob) food-animals
          (coll? mob) (vec (distinct mob)))))

(defn armed?
  "Whether a carried item is one of the call's :use :tools (default jobs.lib.combat/default-weapons), each an item name or part of one."
  [c]
  (let [tools (or (ctx/use c :tools) combat/default-weapons)]
    (boolean (some (fn [t] (some #(str/includes? (:name %) (name t)) (u/inventory (:primitives c)))) tools))))

(defn too-few-text [{:keys [in-range keep nearest mob radius]}]
  (str "hunt stopped: " in-range " " mob " adults within " radius " blocks"
       (when (pos? keep) (str ", keeping " keep))
       (when nearest (str "; the nearest is " nearest " away"))))

(defn fetch-opts
  "The limits {:how :explore :minutes} for fetching a weapon, nil when :fetch does not allow tools."
  [c]
  (when-let [o (need/opts c 'jobs.combat.hunt)]
    (when (contains? (:what o) :tool) (select-keys o [:how :explore :minutes]))))

(defn ^:async fetch-weapon!
  "Get a sword with jobs.lib.need: :yield while it works, :again once got (the memory :fetching marks the fetch under way across
  rounds), else the failed ending."
  [c]
  (ctx/update-mem! c assoc :fetching true)
  (let [e (await (need/tool! c {:kind "sword"} (fetch-opts c)))]
    (cond (= :continue e) :yield
          (= :done (:kind e)) (do (ctx/update-mem! c dissoc :fetching) :again)
          :else (do (ctx/update-mem! c dissoc :fetching) e))))

(defn no-color
  "The cause :no-color while nothing of the :color is in range: the colours that are, in the data and text."
  [c]
  (let [{:keys [mob color]} (:args c)
        seen (vec (sort (distinct (keep #(.-color %) (present-any c)))))
        text (str "hunt stopped: no " color " " mob " within " (:radius (:args c)) " blocks"
                  (when (seq seen) (str "; seen: " (str/join ", " seen)))
                  (let [n (count (filter #(and (= color (.-color %)) (true? (.-sheared %)))
                                         (adults-within c (:radius (:args c)))))]
                    (when (pos? n) (str "; " n " sheared " color " sheep skipped"))))]
    {:reason :no-color :killed 0 :spared 0 :remaining (count (present-any c)) :seen-colors seen :text text}))

(defn too-few-cause [c]
  (let [m (too-few c)]
    (assoc m :killed 0 :text (too-few-text m))))

(defn heard-kind
  "The unseen animals of the kind within :radius that the body hears, while no adult of it is in sight and steps are
  left: the cue to step toward the sound."
  [c]
  (when (and (empty? (present-any c)) (not (vantage/done? c)))
    (seq (vantage/heard c [(:mob (:args c))] (:radius (:args c))))))

(defn unarmed-cause
  "The cause :unarmed: a hostile kind, no weapon carried and no fetch allowed (an unarmed body does not fist-fight)."
  [c]
  (when (and (hostile? c) (not (armed? c)) (not (fetch-opts c)))
    {:reason :unarmed :killed 0 :mob (:mob (:args c)) :text (str "hunt stopped: no weapon carried for a hostile " (:mob (:args c)))}))

(def start-fact
  "Later with the cause that bars a hunt's first step: no weapon for a hostile kind (:fetch off), nothing of the :color in range, or :keep or fewer adults.
  Ok while the kind is heard and not in sight: the first steps look for it."
  (checks/refusing
   :quarry "there is something to hunt"
   (fn [c]
     (when-not (or (:started (ctx/mem c)) (kinds c) (heard-kind c))
       (cond
         (unarmed-cause c) (unarmed-cause c)
         (and (:color (:args c)) (empty? (present c))) (no-color c)
         (<= (count (present c)) (keep-of c)) (too-few-cause c))))))

(def can-start?
  "Waits while the body is outside the call's area, then on a start fact (start-fact: not for a several-kinds call, whose kinds each check theirs)."
  (cond/and area/inside start-fact))

(defn done?
  "Done once the count asked is killed (of each kind) and the drops are collected; the data is the animals killed."
  [c]
  (let [m (ctx/mem c)
        ks (kinds c)
        killed (if ks (reduce + (map #(get-in m [:kinds % :killed] 0) ks)) (:killed m 0))
        wanted (:count (:args c))
        reached? (if ks
                   (every? #(= :count (get-in m [:kinds % :reason])) ks)
                   (and (>= killed wanted) (not (:target m)) (not (:collecting m))))]
    (checks/goal reached? [:hunted "the count is killed"] [:hunting "the hunt is on"]
                 {:units :animals :done killed :left (max 0 (- (* wanted (count (or ks [1]))) killed))
                  :result (if reached? {:killed killed :reason :count :spared 0 :remaining (count (present c))} {:killed killed})})))

(defn quote-of [c]
  {:uses {:job-time [0 (* 20 (:count (:args c) 1)) ##Inf]}})

(defn ending-of
  "The ending of a hunt of killed animals that stopped for reason: :done for :count or when the call's :least was killed,
  else :nothing-possible with reason as the cause; data its result."
  [c reason killed data]
  (let [least (:least (ctx/frozen-call c) 1)
        need (if (= :all least) (:count (:args c)) least)]
    (if (or (= :count reason) (>= killed need))
      (ctx/end c :done nil data)
      (ctx/end c :nothing-possible reason data))))

(defn finish!
  "Emit the outcome and end the call (ending-of)."
  [c reason]
  (let [killed (:killed (ctx/mem c) 0)
        spared (if (and (= :keep reason) (pos? (keep-of c))) (max 0 (- (:count (:args c)) killed)) 0)
        remaining (count (present c))
        by (when (= :spared reason) (frequencies (vals (spare-rules c))))
        out (cond-> {:killed killed :reason reason :spared spared :remaining remaining}
              (= :gave-up reason) (-> (merge (sh-inside/shelter-hint c)) (assoc :skipped (vec (:skipped (ctx/mem c)))))
              by (assoc :spared-by by))
        text (str "hunt done: " (name reason) ", killed " killed ", spared " spared " of the ask, " remaining " adults left"
                  (when by (str "; left alone: " (str/join ", " (map (fn [[k n]] (str n " " (name k))) by)))))]
    (ending-of c reason killed (assoc out :text text))))

(defn give-up!
  "End after too many skipped animals in a row."
  [c]
  (finish! c :gave-up))

(defn allowed-by
  "Which animal-spare rule let the hunt take the adult e: :pen-taken, :surplus or :starved."
  [c e]
  (let [adults (count (adults-within c (:radius (:args c))))]
    (spare/allowed-by {:keep-adults (keep-of c) :spare-pens (spare/spare-pens?)} adults (spare/penned? c (atom []) e))))

(defn attack-opts
  "Options for hunting.attack!: a wide search around the ask; ends the job after too many skips."
  [c]
  {:radius (+ (:radius (:args c)) 8) :give-up give-up! :job "jobs.combat.hunt" :rule-fn allowed-by})

(defn ^:async step
  "One step: attack the target, collect, finish, or pick the next target. :again, :yield (a child is waiting on the world) or :done."
  [c]
  (let [now (ctx/now c)
        wanted (:count (:args c))
        keep (keep-of c)
        first? (not (:started (ctx/mem c)))]
    (ctx/update-mem! c #(-> % (update :started (fn [t] (or t now))) (assoc :keep keep)))
    (let [{:keys [target collecting killed]} (ctx/mem c)
          next-target (first (candidates c))]
      (cond
        (and (or first? (:fetching (ctx/mem c))) (fetch-opts c) (hostile? c) (not (armed? c)))
        (await (fetch-weapon! c))
        target (await (hunting/attack! c (attack-opts c)))
        collecting (await (hunting/collect! c :yield))
        (>= (or killed 0) wanted) (finish! c :count)
        (heard-kind c) (let [r (await (vantage/step! c (heard-kind c) #(seq (present-any c))))]
                         (if (= :partial r) :yield :again))
        (<= (count (present c)) keep) (finish! c :keep)
        (and (nil? next-target) (seq (spare-rules c))) (finish! c :spared)
        (nil? next-target) (await (hunting/search! c #(finish! % :none)))
        :else (do (ctx/update-mem! c assoc :target (.-id next-target) :misses 0)
                  (await (hunting/attack! c (attack-opts c))))))))

(defn ^:async hunt-round
  "The whole hunt of one kind: steps until it ends; yields only while a child waits on the world."
  [c]
  (if-let [t (await (trip/ready! c))]
    t
    (let [r (await (pace/steps! c #(step c)))]
      (if (= :yield r) :continue r))))

(defn kind-slot [kind] (keyword (str "kind-" kind)))

(defn kind-args [c kind]
  (cond-> (assoc (:args c) :mob kind)
    (not= "sheep" kind) (dissoc :color)))

(defn child-result
  "A kind's ending e as the result kept in memory: its data, :reason (the data's for a done, else the cause) and :kind."
  [e]
  (assoc (merge {:killed 0} (:data e)) :kind (:kind e)
         :reason (if (= :done (:kind e)) (get-in e [:data :reason]) (contract/cause-key e))))

(defn finish-kinds!
  "End the batch: the sums and each kind's result. Too few kills end :nothing-possible :partial, the first failed kind's ending its :because."
  [c kinds]
  (let [results (into {} (map (fn [k] [k (get-in (ctx/mem c) [:kinds k])])) kinds)
        killed (reduce + (map #(:killed % 0) (vals results)))
        all-count? (every? #(= :count (:reason %)) (vals results))
        reason (if all-count? :count :partial)
        out {:killed killed :reason reason :kinds results
             :text (str "hunt done: killed " killed " of " (str/join ", " kinds) ", " (some-> reason name))}]
    (if (or all-count? (>= killed (let [least (:least (ctx/frozen-call c) 1)] (if (= :all least) (* (:count (:args c)) (count kinds)) least))))
      (ctx/end c :done nil out)
      (contract/with-because (ctx/end c :nothing-possible reason out) (:failed (ctx/mem c))))))

(defn ^:async kinds-round
  "One child hunt after another, one per kind; :continue only while a child waits on the world."
  [c kinds]
  (loop []
    (let [done (:kinds (ctx/mem c))
          kind (first (remove #(contains? done %) kinds))]
      (if-not kind
        (finish-kinds! c kinds)
        (let [e (waits/waiting-out c (await (ctx/call c (kind-slot kind) 'jobs.combat.hunt (kind-args c kind))))]
          (if (= :continue e)
            :continue
            (do (ctx/update-mem! c #(cond-> (assoc-in % [:kinds kind] (child-result e))
                                      (and (not= :done (:kind e)) (not (:failed %))) (assoc :failed e)))
                (recur))))))))

(defn ^:async round
  "The hunt of the one :mob kind, or one hunt after another for a list of kinds."
  [c]
  (if-let [ks (kinds c)]
    (await (kinds-round c ks))
    (await (hunt-round c))))

(def meta {:tags [:food :fight]})
