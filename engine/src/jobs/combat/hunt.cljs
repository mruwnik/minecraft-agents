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
            [jobs.lib.work-place :as work-place]
            [jobs.lib.ready :as ready]
            [jobs.lib.waits :as waits]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.vantage :as vantage]))

(def doc
  "Get the item :item (e.g. \"white_wool\", \"beef\", \"rotten_flesh\") by killing adult animals or mobs that drop it,
  within :radius of :center (the body's place at the first run),
  collecting the drops until :count of it are carried, in one call that yields only while a child waits on the world. The mobs come from the drops table
  (jobs.lib.hunting/raw-drops); an item no mob drops is impossible (:bad-args). A wool item hunts only sheep of that colour.
  Never takes the last :keep adults of the kind (nil: 2 for animals, 0 for hostile mobs); babies are never targets. Animals the body
  spares (jobs.lib.animal-spare: kinds in ::off-limits, animals in a zone or plan of its own or a closed enclosure unless ::spare-pens
  is false) are never targets either. Weapons: the call's :use :tools (default the best carried); a hostile kind with none gets a sword
  first (jobs.lib.need; a failed fetch ends the job with its cause).
  Needs: fit? (waits :low-health for a hostile kind only); at its start it first looks round (an animal behind the body is not in view), then waits :unarmed (hostile, no weapon, ways #{:carried}), :no-color or
  :too-few (:in-range, :radius, :keep, :nearest) unless the kind is heard and not in sight, when it first steps up to four times
  toward the sound (jobs.lib.vantage).
  done? holds once :count of the item are carried.
  Returns {:killed n :gained g :reason r :spared s :remaining m}. :done for :count and, for any other reason, when the call's :least
  (default 1) were gained; else :nothing-possible with r the cause (:keep, :spared with :spared-by per rule, :none, :gave-up after
  jobs.lib.hunting/max-skips skipped animals in a row).")

(a/defargs args
  {:item {:doc "item name to get from the drops of the mobs that give it, e.g. white_wool, beef, rotten_flesh" :spec a/item? :default nil}
   :count {:doc "items to carry in all" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "animals within this many blocks of the body count" :spec (a/num-in 0 nil) :default 24}
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec a/position? :default nil}
   :keep {:doc "never kill the last this many adults of the kind within :radius (babies do not count); nil: 2 for animals, 0 for hostile mobs; 0 turns the rule off" :spec (a/int-in 0 nil) :default nil}})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds a hunt may take." :spec (a/num-in 0 nil)}
   ::sight-radius {:default 64 :doc "How far the :too-few wait looks for the nearest adult of the kind, in blocks." :spec (a/num-in 0 nil)}})

(def ways need/fetch-ways)

(def target [:item :center])

(def defaults {:spend {:job-time ::job-time-s} :least 1 :use {:tools :attack} :checks {:safe? '(not (exposed))}})
(def safe? (ready/safe-at-or-roaming (fn [c] (work-place/nearest-mob c nil (:radius (:args c))))))
(declare hostile?)

(defn fit?
  "ready/fit? for a hostile kind; a passive animal cannot hurt the body, and hunting it is the way to the food that heals (survival mode only)."
  [c]
  (if (hostile? c) (ready/fit? c) (checks/survival-mode c)))

(defn mobs
  "The mob names that drop the call's :item."
  [c]
  (hunting/mobs c))

(defn mob-name "The first mob of the call, for texts." [c] (first (mobs c)))

(defn adults-within
  "The adult entities of the call's mobs within radius blocks, nearest first, never players, items or babies
  (an entity is an adult unless its baby field is true)."
  [c radius]
  (->> (look/seen-objs (:primitives c) {:radius radius :names (mobs c) :max 64})
       (remove #(contains? #{"player" "item"} (.-kind %)))
       (remove #(true? (.-baby %)))
       vec))

(defn color-of "The colour of a wool :item, else nil." [c] (hunting/wool-color (:item (:args c))))

(defn coloured
  "The entities of the wool colour (all when :item is no wool); with a colour the sheared ones are left out, they drop no wool."
  [c es]
  (let [color (color-of c)]
    (cond->> es color (filter #(and (= color (.-color %)) (not (true? (.-sheared %))))))))

(defn present-any
  "The adult entities of the call's mobs within :radius, of any colour."
  [c]
  (adults-within c (:radius (:args c))))

(defn present
  "The adult entities of the call's mobs within :radius, of the wool colour when the item is wool."
  [c]
  (vec (coloured c (present-any c))))

(defn spare-rules
  "The present animals of the :color the body leaves alone, as {id rule} (jobs.lib.animal-spare/why: :off-limits or :pen);
  the pair rule is :keep's."
  [c]
  (let [pens (atom [])]
    (into {} (keep #(some->> (spare/why -1 (.-name %) 0 (spare/penned? c pens %) false) (vector (.-id %)))) (present c))))

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
  (let [{:keys [radius]} (:args c)
        p (:primitives c)]
    (boolean (or (cost/hostile? (game/version-of p) {:name (mob-name c)})
                 (some #(= "hostile" (.-kind %))
                       (look/seen-objs p {:radius radius :names (mobs c) :max 64}))))))

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
  (let [{:keys [radius]} (:args c)
        mob (mob-name c)
        seen (some-> (first (adults-within c (max radius (settings/get settings ::sight-radius)))) .-distance)
        nearest (if seen (min seen (:nearest-seen (ctx/mem c) ##Inf)) (:nearest-seen (ctx/mem c)))]
    (cond-> {:reason :too-few :mob mob :keep (keep-of c) :in-range (count (present c)) :radius radius}
      nearest (assoc :nearest (js/Math.round nearest)))))

(defn note-nearest!
  "Remembers the nearest adult in view now: the survey's last look may face away from the one the wait names."
  [c]
  (when-let [d (some-> (first (adults-within c (max (:radius (:args c)) (settings/get settings ::sight-radius)))) .-distance)]
    (ctx/update-mem! c update :nearest-seen #(min d (or % ##Inf)))))

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
  "The limits {:how} for fetching a weapon, nil when the ways leave only :carried."
  [c]
  (need/fetch-opts c))

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
  (let [mob (mob-name c)
        color (color-of c)
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
    (seq (vantage/heard c (mobs c) (:radius (:args c))))))

(defn unarmed-cause
  "The cause :unarmed: a hostile kind, no weapon carried and no fetch allowed (an unarmed body does not fist-fight)."
  [c]
  (when (and (hostile? c) (not (armed? c)) (not (fetch-opts c)))
    {:reason :unarmed :killed 0 :mob (mob-name c) :text (str "hunt stopped: no weapon carried for a hostile " (mob-name c))}))

(def start-fact
  "Later with the cause that bars a hunt's first step: no weapon for a hostile kind (ways #{:carried}), nothing of the :color in range, or :keep or fewer adults.
  Ok while the kind is heard and not in sight: the first steps look for it, and while the job has not looked round (see survey)."
  (checks/refusing
   :quarry "there is something to hunt"
   (fn [c]
     (when-not (or (:started (ctx/mem c)) (area/outside c) (heard-kind c))
       (cond
         (unarmed-cause c) (unarmed-cause c)
         (and (not (look/surveyed? c)) (empty? (present-any c))) nil
         (and (color-of c) (empty? (present c))) (no-color c)
         (<= (count (present c)) (keep-of c)) (too-few-cause c))))))

(defn bad-args
  "Never :bad-args unless a mob drops the call's :item."
  [c]
  (if (seq (mobs c))
    (contract/ok :args "an item some mob drops")
    (contract/never :bad-args "no mob drops that item" :why "no mob drops that item")))

(def can-start?
  "Waits on a start fact (none while the body is outside the area: the first step walks there)."
  (cond/and bad-args start-fact))

(def can-run? "The start fact again between steps, until the first step is made." start-fact)

(defn carried "How many of the call's :item are carried." [c] (u/carried-count (:primitives c) (:item (:args c))))

(defn base
  "What was carried of the item at the first run (now, before it): the gained items are counted from it."
  [c]
  (or (:base (ctx/mem c)) (carried c)))

(defn gained "Items got since the first run." [c] (max 0 (- (carried c) (base c))))

(defn done?
  "Done once :count of the item are carried and the drops are collected; the data is the animals killed."
  [c]
  (let [m (ctx/mem c)
        killed (:killed m 0)
        wanted (:count (:args c))
        reached? (and (>= (carried c) wanted) (not (:target m)) (not (:collecting m)))]
    (checks/goal reached? [:hunted "the count is carried"] [:hunting "the hunt is on"]
                 {:units :items :done (carried c) :left (max 0 (- wanted (carried c)))
                  :result (if reached? {:killed killed :gained (gained c) :reason :count :spared 0 :remaining (count (present c))} {:killed killed :gained (gained c)})})))

(defn quote-of [c]
  {:uses {:job-time [0 (* 20 (:count (:args c) 1)) ##Inf]}})

(defn ending-of
  "The ending of a hunt that stopped for reason: :done for :count or when the call's :least items were gained,
  else :nothing-possible with reason as the cause; data its result."
  [c reason data]
  (let [least (:least (ctx/frozen-call c) 1)
        need (if (= :all least) (:count (:args c)) least)]
    (if (or (= :count reason) (>= (gained c) need))
      (ctx/end c :done nil data)
      (ctx/end c :nothing-possible reason data))))

(defn finish!
  "Emit the outcome and end the call (ending-of)."
  [c reason]
  (let [killed (:killed (ctx/mem c) 0)
        spared (if (and (= :keep reason) (pos? (keep-of c))) (max 0 (- (:count (:args c)) (carried c))) 0)
        remaining (count (present c))
        by (when (= :spared reason) (frequencies (vals (spare-rules c))))
        out (cond-> {:killed killed :gained (gained c) :reason reason :spared spared :remaining remaining}
              (= :gave-up reason) (-> (merge (sh-inside/shelter-hint c)) (assoc :skipped (vec (:skipped (ctx/mem c)))))
              by (assoc :spared-by by))
        text (str "hunt done: " (name reason) ", killed " killed ", spared " spared " of the ask, " remaining " adults left"
                  (when by (str "; left alone: " (str/join ", " (map (fn [[k n]] (str n " " (name k))) by)))))]
    (ending-of c reason (assoc out :text text))))

(defn give-up!
  "End after too many skipped animals in a row."
  [c]
  (finish! c :gave-up))

(defn allowed-by
  "Which animal-spare rule let the hunt take the adult e: :pen-taken, :surplus or :starved."
  [c e]
  (let [adults (count (adults-within c (:radius (:args c))))]
    (spare/allowed-by (keep-of c) adults (spare/penned? c (atom []) e))))

(defn attack-opts
  "Options for hunting.attack!: a wide search around the ask; ends the job after too many skips."
  [c]
  {:radius (+ (:radius (:args c)) 8) :give-up give-up! :job "jobs.combat.hunt" :rule-fn allowed-by})

(defn ^:async survey
  "Before the first step, with no animal in view: look round (an animal behind the body is not in view until it turns), and
  face the first one seen, so the next step sees it. :again, which runs can-run? (the start fact) before the next step: a hunt with
  nothing in sight then waits :too-few."
  [c]
  (when-let [e (await (look/survey-until! c #(do (note-nearest! c) (first (present-any c)))))]
    (ctx/update-mem! c assoc :looked (look/cell-of (u/self-pos c)))
    (await (ctx/act c :look (clj->js {:pos (.-pos e)}))))
  :again)

(defn ^:async hunt-step
  "The step proper: attack the target, collect, finish, or pick the next target."
  [c]
  (let [now (ctx/now c)
        wanted (:count (:args c))
        keep (keep-of c)
        first? (not (:started (ctx/mem c)))]
    (ctx/update-mem! c #(-> % (update :started (fn [t] (or t now))) (update :base (fn [b] (or b (carried c)))) (assoc :keep keep)))
    (let [{:keys [target collecting]} (ctx/mem c)
          next-target (first (candidates c))]
      (cond
        (and (or first? (:fetching (ctx/mem c))) (fetch-opts c) (hostile? c) (not (armed? c)))
        (await (fetch-weapon! c))
        target (await (hunting/attack! c (attack-opts c)))
        collecting (await (hunting/collect! c :yield))
        (>= (carried c) wanted) (finish! c :count)
        (heard-kind c) (let [r (await (vantage/step! c (heard-kind c) #(seq (present-any c))))]
                         (if (= :partial r) :yield :again))
        (<= (count (present c)) keep) (finish! c :keep)
        (and (nil? next-target) (seq (spare-rules c))) (finish! c :spared)
        (nil? next-target) (await (hunting/search! c #(finish! % :none)))
        :else (do (ctx/update-mem! c assoc :target (.-id next-target) :misses 0)
                  (await (hunting/attack! c (attack-opts c))))))))

(defn ^:async step
  "One step: attack the target, collect, finish, or pick the next target. :again, :yield (a child is waiting on the world) or :done."
  [c]
  (let [w (when-not (:started (ctx/mem c)) (await (area/walk-in! c)))]
    (cond
      (= :there w) :again
      (= :continue w) :yield
      w w
      (and (not (:started (ctx/mem c))) (not (look/surveyed? c)) (empty? (present-any c))) (await (survey c))
      :else (await (hunt-step c)))))

(defn ^:async hunt-round
  "The whole hunt of one kind: steps until it ends; yields only while a child waits on the world."
  [c]
  (if-let [t (await (trip/ready! c))]
    t
    (let [r (await (pace/steps! c #(step c)))]
      (if (= :yield r) :continue r))))

(defn ^:async round
  "The whole hunt: steps until it ends; yields only while a child waits on the world."
  [c]
  (await (hunt-round c)))

(def meta {:tags [:food :fight]})
