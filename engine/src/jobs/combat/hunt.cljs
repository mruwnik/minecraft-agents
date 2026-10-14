(ns jobs.combat.hunt
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.body :as body]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.hunting :as hunting]
            [jobs.lib.shelter :as sh]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]
            [jobs.lib.started :as started]
            [jobs.lib.util :as u]
            [jobs.lib.vantage :as vantage]))

(def doc
  "Kill :count adult animals of the mob kind :mob within :radius, collecting their drops.
  :mob may be a list of kinds, or :food-animals (cow, pig, sheep, chicken): one child hunt per kind, in order, each with
  its own :count and :keep (:color goes to sheep only); the result is {:killed n :kinds {mob result} :reason r}: reason :count
  when every kind reached its count, else :partial; no kill at all ends stopped with the first kind's reason.
  Never takes the last :keep adults of the kind, so they can breed. :keep nil means 2 for animals and 0 for hostile mobs
  (by the kind's name, else a kind any of whose entities in range reports kind hostile). 0 turns the rule off.
  The weapons are the call's :use :tools (default the best carried; the old :weapons substrings map in legacy-args).
  Babies are never targets and never counted. Animals the body spares (jobs.lib.animal-spare: kinds in :off-limits, animals in
  a zone or plan of its own or a closed enclosure unless :spare-pens is false) are never targets either; when only such animals
  are left it ends :spared (the data has :spared-by, a count per rule, and warn hunt.done names them when nothing was killed).
  A queued hunt waits (conditions) while the body is too hurt (:low-health, jobs.lib.body/too-hurt), and at its start (one kind
  only) while :no-color (a :color and none of it in range; :seen-colors) or :too-few (:keep or fewer adults within :radius; the
  cause has :in-range, :radius, :keep and :nearest, the distance of the nearest adult of the kind in sight up to ::sight-radius).
  The first round ends :condition-failed :unarmed (warn hunt.done) for a hostile kind and no weapon of the call's :use :tools carried.
  One call hunts until it ends, in steps; it yields (:continue) only while a child is waiting on the world:
  1. With a :target, the attack child fights it. It is booked killed, or skipped when attack gave up on it or lost it.
     :max-skips skips in a row end the job :gave-up (warn hunt.gave-up).
  2. After a target, the collect-drops child picks up :drops (nil: the kind's entry in the drops table,
     else every item within :collect-radius).
  3. :killed reaching :count ends :count.
  4. At most :keep adults present ends :keep. Skipped animals still count.
  With a :color only unsheared sheep of that colour count and are hunted (the :keep rule applies to them: the last :keep stay).
  5. No adult of the kind in sight within :radius (animals heard below a lip are not in sight): the body walks to up to four
     look-outs 3 blocks round where it started and looks round at each (jobs.lib.vantage), before step 4 or the end below.
     No candidate (present, not skipped) ends :none, on the second such round in a row, after a 1 s wait.
  6. Otherwise the nearest candidate becomes the :target and is attacked in the same round.
     The target stays until killed or given up on, even when a nearer animal turns up.
  Before the first step it deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!; :deposit-first? false skips it).
  Ends with info hunt.done and the data {:killed n :reason r :spared s :remaining m}. The ending: :done for :count, and for
  any other reason when the call's :least (default 1) animals were killed; else :nothing-possible with r as the cause
  (:keep, :spared, :none, :gave-up). A list of kinds ends :done :count / :partial, else with the first kind's ending.
  :spared is the kills asked for and not made because of the pair rule (0 unless :keep ended it).
  :remaining is the adults of the kind in range at the end.")

(a/defargs args
  {:mob {:doc "mob type name of the animals to hunt, a list of them, or :food-animals (cow, pig, sheep, chicken); one hunt per kind, :count and :keep apply to each" :spec (a/or-of a/name? (a/coll-of a/name?) #{:food-animals "food-animals"}) :default "cow"}
   :color {:doc "sheep only: kill only sheep of this wool colour (e.g. \"white\"); nil: any" :spec a/name? :default nil}
   :count {:doc "animals to kill" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "animals within this many blocks of the body count" :spec (a/num-in 0 nil) :default 24}
   :keep {:doc "never kill the last this many adults of the kind within :radius (babies do not count); nil: 2 for animals, 0 for hostile mobs; 0 turns the rule off" :spec (a/int-in 0 nil) :default nil}
   :collect-radius {:doc "how far around to collect drops after a kill" :spec (a/num-in 0 nil) :default 8}
   :deposit-first? {:doc "deposit at the known chest first when the bag is fullish and the chest is near (jobs.lib.trip/ready!)" :spec boolean? :default true}
   :drops {:doc "item names to collect after a kill; nil: the kind's entry in the drops table, else every item" :spec (a/coll-of a/item?) :default nil}
   :off-limits {:doc "animal kinds never hunted; nil: the jobs.lib.animal-spare/off-limits setting" :spec (a/or-of (a/pred "nil" nil?) (a/coll-of a/item?)) :default nil}
   :spare-pens {:doc "never hunt animals in a zone or plan of the body's own or in a closed enclosure (false: this hunt may take them)" :spec boolean? :default true}
   :max-skips {:doc "animals skipped in a row before the hunt gives up" :spec (a/int-in 1 nil) :default 3}})

(a/defargs settings
  {::sight-radius {:default 64 :doc "How far the :too-few wait looks for the nearest adult of the kind, in blocks." :spec (a/num-in 0 nil)}})

(def target [:mob])

(def defaults {:least 1 :use {:tools :attack}})

(defn legacy-args
  "An old caller's :weapons (item name parts) as the call's :use {:tools [...]}; [] means no weapons."
  [{:keys [weapons] :as args}]
  (cond-> (dissoc args :weapons)
    (and (some? weapons) (nil? (get-in args [:use :tools]))) (assoc-in [:use :tools] (mapv name weapons))))

(defn adults-within
  "The adult entities named :mob within radius blocks, nearest first, never players, items or babies
  (an entity is an adult unless its baby field is true)."
  [c radius]
  (->> (array-seq (.entities (:primitives c) #js {:radius radius :names #js [(:mob (:args c))] :max 64}))
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
        args {:off-limits (:off-limits (:args c)) :spare-pens (:spare-pens (:args c)) :keep-adults -1}]
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
                       (array-seq (.entities p #js {:radius radius :names #js [mob] :max 64})))))))

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

(defn low-health
  "The cause {:reason :low-health ..} while the body is too hurt (jobs.lib.body/too-hurt)."
  [c]
  (body/too-hurt (:primitives c)))

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

(defn unarmed!
  "End at once: a hostile kind and no weapon carried (an unarmed body does not fist-fight)."
  [c]
  (let [mob (:mob (:args c))
        out {:killed 0 :reason :unarmed :mob mob :text (str "hunt stopped: no weapon carried for a hostile " mob)}]
    (ctx/emit! c :hunt.done :warn out)
    (ctx/end c :condition-failed :unarmed out)))

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

(defn unseen-kind?
  "Whether no adult of the kind is in sight within :radius and not every look-out was tried: animals heard below a lip
  are looked for before the hunt says there are none."
  [c]
  (and (empty? (present-any c)) (not (vantage/done? c))))

(defn start-fact
  "The cause that bars a hunt's first step, or nil: nothing of the :color in range, or :keep or fewer adults.
  Nil while none of the kind is in sight and the look-outs are untried: the first steps look for them."
  [c]
  (when-not (or (:started (ctx/mem c)) (kinds c) (unseen-kind? c))
    (cond
      (and (:color (:args c)) (empty? (present c))) (no-color c)
      (<= (count (present c)) (keep-of c)) (too-few-cause c))))

(def conditions
  "Waits while too hurt or on a start fact (start-fact: not for a several-kinds call, whose kinds each check theirs)."
  [[:fn (started/at-start low-health)] [:fn start-fact]])

(defn done?
  "The animals killed against the count asked. Never :done? itself: the hunt ends with its event and data."
  [c]
  (let [killed (if-let [ks (kinds c)]
                 (reduce + (map #(get-in (ctx/mem c) [:kinds % :killed] 0) ks))
                 (:killed (ctx/mem c) 0))]
    {:done? false :units :animals :done killed :left (max 0 (- (* (:count (:args c)) (count (or (kinds c) [1]))) killed))}))

(defn quote-of [c]
  {:uses {:time [0 (* 20 (:count (:args c) 1)) ##Inf]}})

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
              (= :gave-up reason) (merge (sh/shelter-hint c))
              by (assoc :spared-by by))
        text (str "hunt done: " (name reason) ", killed " killed ", spared " spared " of the ask, " remaining " adults left"
                  (when by (str "; left alone: " (str/join ", " (map (fn [[k n]] (str n " " (name k))) by)))))]
    (ctx/emit! c :hunt.done (if (and by (zero? killed)) :warn :info) (assoc out :text text))
    (ending-of c reason killed out)))

(defn give-up!
  "Warn and end after too many skipped animals in a row."
  [c]
  (let [{:keys [killed skipped]} (ctx/mem c)]
    (ctx/emit! c :hunt.gave-up :warn (merge {:killed (or killed 0) :skipped (vec skipped)
                                             :text (str "hunt gave up: " (count skipped) " animals skipped, killed " (or killed 0))}
                                            (sh/shelter-hint c)))
    (finish! c :gave-up)))

(defn allowed-by
  "Which animal-spare rule let the hunt take the adult e: :pen-taken, :surplus or :starved."
  [c e]
  (let [adults (count (adults-within c (:radius (:args c))))]
    (spare/allowed-by {:keep-adults (keep-of c) :spare-pens (:spare-pens (:args c))} adults (spare/penned? c (atom []) e))))

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
        (and first? (hostile? c) (not (armed? c))) (unarmed! c)
        target (await (hunting/attack! c (attack-opts c)))
        collecting (await (hunting/collect! c :yield))
        (>= (or killed 0) wanted) (finish! c :count)
        (unseen-kind? c) (or (await (hunting/look-further! c #(seq (present-any c)))) (await (step c)))
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
  "End the batch: the sums and each kind's result. No kill ends with the first kind's ending."
  [c kinds]
  (let [results (into {} (map (fn [k] [k (get-in (ctx/mem c) [:kinds k])])) kinds)
        killed (reduce + (map #(:killed % 0) (vals results)))
        all-count? (every? #(= :count (:reason %)) (vals results))
        first-kind (get results (first kinds))
        reason (cond all-count? :count (zero? killed) (:reason first-kind) :else :partial)
        out {:killed killed :reason reason :kinds results}]
    (ctx/emit! c :hunt.batch-done (if (zero? killed) :warn :info)
               (assoc out :text (str "hunt done: killed " killed " of " (str/join ", " kinds) ", " (some-> reason name))))
    (if (or all-count? (>= killed (let [least (:least (ctx/frozen-call c) 1)] (if (= :all least) (* (:count (:args c)) (count kinds)) least))))
      (ctx/end c :done nil out)
      (ctx/end c (if (= :done (:kind first-kind)) :nothing-possible (:kind first-kind)) reason out))))

(defn ^:async kinds-round
  "One child hunt after another, one per kind; :continue only while a child waits on the world."
  [c kinds]
  (loop []
    (let [done (:kinds (ctx/mem c))
          kind (first (remove #(contains? done %) kinds))]
      (if-not kind
        (finish-kinds! c kinds)
        (let [e (await (ctx/call c (kind-slot kind) 'jobs.combat.hunt (kind-args c kind)))]
          (if (= :continue e)
            :continue
            (do (ctx/update-mem! c assoc-in [:kinds kind] (child-result e))
                (recur))))))))

(defn ^:async round
  "The hunt of the one :mob kind, or one hunt after another for a list of kinds."
  [c]
  (if-let [ks (kinds c)]
    (await (kinds-round c ks))
    (await (hunt-round c))))
