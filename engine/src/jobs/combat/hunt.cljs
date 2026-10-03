(ns jobs.combat.hunt
  (:require [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]))

(def doc
  "Kill :count adult animals of the mob kind :mob within :radius, collecting
  their drops, and never take the last :keep adults of the kind (they breed). :keep defaults
  to nil: 2 for animals, 0 for hostile mobs (a kind any of whose entities in
  range reports kind hostile); 0 turns the pair rule off, any number wins.
  Babies are never targets and never counted (an entity is an adult unless it
  reports baby true). The check passes while more than :keep adults of the
  kind are within :radius, and always once
  the job has started. One step per round: (1) with a :target, the attack
  child fights it (every round it is not done); it is booked killed, or
  skipped when attack gave up on it or lost it, and :skips counts skipped
  animals in a row; :max-skips of them in a row ends the job :gave-up (warn
  hunt.gave-up); (2) after a target the collect-drops child picks up :drops
  (nil: the kind's entry in the drops table, or every item within
  :collect-radius for a kind not in it); (3) :killed reaching :count ends
  :count; (4) at most :keep adults of the kind still present ends :keep (the pair rule,
  resolved on the first round and remembered; skipped
  animals still count, they still breed); (5) no candidate (present, not
  skipped, nearest first) ends :none on the second round in a row that finds
  none, after a 1 s wait (look twice); (6) else the nearest candidate becomes
  the :target and is attacked in the same round. The target stays until it is
  killed or given up on, even when a nearer animal turns up. Hands over
  {:killed n :reason r :spared s :remaining m} (info hunt.done): :spared is the
  kills asked for and not made because of the pair rule (0 unless :keep ended
  it, and 0 when the rule is off), :remaining the adults of the kind in range at the end.")

(def raw-meats
  {"beef" "cooked_beef" "porkchop" "cooked_porkchop" "mutton" "cooked_mutton"
   "chicken" "cooked_chicken" "rabbit" "cooked_rabbit"})

(def wool-colors
  ["white" "orange" "magenta" "light_blue" "yellow" "lime" "pink" "gray"
   "light_gray" "cyan" "purple" "blue" "brown" "green" "red" "black"])

(def drops
  "What each huntable kind drops, raw and cooked meat included, by mob name."
  (let [raw {"cow" ["beef" "leather"]
             "mooshroom" ["beef" "leather"]
             "pig" ["porkchop"]
             "sheep" (into ["mutton"] (map #(str % "_wool")) wool-colors)
             "chicken" ["chicken" "feather"]
             "rabbit" ["rabbit" "rabbit_hide" "rabbit_foot"]}]
    (update-vals raw (fn [items] (into items (keep raw-meats) items)))))

(def args
  {:mob {:doc "mob type name of the animals to hunt" :default "cow"}
   :count {:doc "animals to kill" :default 1}
   :radius {:doc "animals within this many blocks of the body count" :default 24}
   :keep {:doc "never kill the last this many adults of the kind within :radius (babies do not count); nil: 2 for animals, 0 for hostile mobs; 0 turns the rule off" :default nil}
   :collect-radius {:doc "how far around to collect drops after a kill" :default 8}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}
   :drops {:doc "item names to collect after a kill; nil: the kind's entry in the drops table, else every item" :default nil}
   :max-skips {:doc "animals skipped in a row before the hunt gives up" :default 3}})

(defn present
  "The adult entities named :mob within :radius, never players, items or babies
  (an entity is an adult unless its baby field is true)."
  [c]
  (let [{:keys [mob radius]} (:args c)]
    (->> (array-seq (.entities (:primitives c) #js {:radius radius :names #js [mob] :max 64}))
         (remove #(contains? #{"player" "item"} (.-kind %)))
         (remove #(true? (.-baby %)))
         vec)))

(defn candidates
  "The present animals not skipped, nearest first."
  [c]
  (let [skipped (set (:skipped (ctx/mem c)))
        here (u/self-pos c)]
    (->> (present c)
         (remove #(contains? skipped (.-id %)))
         (sort-by #(u/dist here (u/pos-of (.-pos %)))))))

(defn hostile?
  "Does any entity of the kind within :radius report kind hostile (babies included)?"
  [c]
  (let [{:keys [mob radius]} (:args c)]
    (boolean (some #(= "hostile" (.-kind %))
                   (array-seq (.entities (:primitives c) #js {:radius radius :names #js [mob] :max 64}))))))

(defn keep-of
  "Adults of the kind never taken: the :keep arg when a number, else the value
  remembered in job memory, else 2 for animals and 0 for hostile mobs."
  [c]
  (or (:keep (:args c))
      (:keep (ctx/mem c))
      (if (hostile? c) 0 2)))

(defn check [c]
  (boolean (or (:started (ctx/mem c))
               (> (count (present c)) (keep-of c)))))

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [killed (:killed (ctx/mem c) 0)
        spared (if (and (= :keep reason) (pos? (keep-of c))) (max 0 (- (:count (:args c)) killed)) 0)
        remaining (count (present c))
        out {:killed killed :reason reason :spared spared :remaining remaining}]
    (ctx/emit! c :hunt.done :info (assoc out :text (str "hunt done: " (name reason) ", killed " killed
                                                        ", spared " spared " of the ask, " remaining " adults left")))
    (ctx/result! c out)
    :done))

(defn give-up!
  "Warn and end after too many skipped animals in a row."
  [c]
  (let [{:keys [killed skipped]} (ctx/mem c)]
    (ctx/emit! c :hunt.gave-up :warn {:killed (or killed 0) :skipped (vec skipped)
                                      :text (str "hunt gave up: " (count skipped) " animals skipped, killed " (or killed 0))})
    (finish! c :gave-up)))

(defn book-outcome!
  "Book the attack child's result for target: a kill, or a skipped animal. Then
  start collecting."
  [c target]
  (let [killed? (some #{target} (:killed (ctx/child-result c :attack)))]
    (ctx/update-mem! c (fn [m]
                         (-> (if killed?
                               (-> m (update :killed (fnil inc 0)) (assoc :skips 0))
                               (-> m (update :skipped (fnil conj []) target) (update :skips (fnil inc 0))))
                             (dissoc :target)
                             (assoc :collecting true))))))

(defn ^:async attack!
  "One round of the attack child on the target; books its end."
  [c]
  (let [{:keys [radius weapons]} (:args c)
        target (:target (ctx/mem c))
        r (await (ctx/call-child c :attack 'jobs.combat.attack
                                 {:targets [target] :radius (+ radius 8) :weapons weapons :lost-s 1 :absent :done}))]
    (cond
      (not= :done r) :continue
      :else (do (book-outcome! c target)
                (if (>= (:skips (ctx/mem c) 0) (:max-skips (:args c)))
                  (give-up! c)
                  :continue)))))

(defn ^:async collect!
  "One round of the collect-drops child; done collecting when it is."
  [c]
  (let [{:keys [mob collect-radius] :as a} (:args c)
        r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 {:radius collect-radius :filter (or (:drops a) (get drops mob))}))]
    (when (= :done r) (ctx/update-mem! c dissoc :collecting))
    :continue))

(defn ^:async search!
  "Nothing to attack: wait and look once more, then end :none."
  [c]
  (let [misses (inc (:misses (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :misses misses)
    (if (>= misses 2)
      (finish! c :none)
      (do (await (ctx/act c :wait #js {:ms 1000}))
          :continue))))

(defn ^:async round [c]
  (let [now (ctx/now c)
        wanted (:count (:args c))
        keep (keep-of c)]
    (ctx/update-mem! c #(-> % (update :started (fn [t] (or t now))) (assoc :keep keep)))
    (let [{:keys [target collecting killed]} (ctx/mem c)
          next-target (first (candidates c))]
      (cond
        target (await (attack! c))
        collecting (await (collect! c))
        (>= (or killed 0) wanted) (finish! c :count)
        (<= (count (present c)) keep) (finish! c :keep)
        (nil? next-target) (await (search! c))
        :else (do (ctx/update-mem! c assoc :target (.-id next-target) :misses 0)
                  (await (attack! c)))))))
