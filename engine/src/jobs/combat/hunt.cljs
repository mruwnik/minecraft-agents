(ns jobs.combat.hunt
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.hunting :as hunting]
            [jobs.lib.shelter :as sh]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(def doc
  "Kill :count adult animals of the mob kind :mob within :radius, collecting their drops.
  Never takes the last :keep adults of the kind, so they can breed. :keep nil means 2 for animals and 0 for hostile mobs
  (a kind any of whose entities in range reports kind hostile). 0 turns the rule off.
  Babies are never targets and never counted.
  Declines (waiting :too-few) unless more than :keep adults are within :radius. Once started it always passes.
  One call hunts until it ends, in steps; it yields (:continue) only while a child is waiting on the world:
  1. With a :target, the attack child fights it. It is booked killed, or skipped when attack gave up on it or lost it.
     :max-skips skips in a row end the job :gave-up (warn hunt.gave-up).
  2. After a target, the collect-drops child picks up :drops (nil: the kind's entry in the drops table,
     else every item within :collect-radius).
  3. :killed reaching :count ends :count.
  4. At most :keep adults present ends :keep. Skipped animals still count.
  5. No candidate (present, not skipped) ends :none, on the second such round in a row, after a 1 s wait.
  6. Otherwise the nearest candidate becomes the :target and is attacked in the same round.
     The target stays until killed or given up on, even when a nearer animal turns up.
  Ends with info hunt.done and the result {:killed n :reason r :spared s :remaining m}.
  :spared is the kills asked for and not made because of the pair rule (0 unless :keep ended it).
  :remaining is the adults of the kind in range at the end.")

(a/defargs args
  {:mob {:doc "mob type name of the animals to hunt" :spec a/name? :default "cow"}
   :count {:doc "animals to kill" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "animals within this many blocks of the body count" :spec (a/num-in 0 nil) :default 24}
   :keep {:doc "never kill the last this many adults of the kind within :radius (babies do not count); nil: 2 for animals, 0 for hostile mobs; 0 turns the rule off" :spec (a/int-in 0 nil) :default nil}
   :collect-radius {:doc "how far around to collect drops after a kill" :spec (a/num-in 0 nil) :default 8}
   :weapons {:doc "item name substrings that count as weapons" :spec (a/coll-of (a/or-of string? keyword?)) :default combat/default-weapons}
   :drops {:doc "item names to collect after a kill; nil: the kind's entry in the drops table, else every item" :spec (a/coll-of a/item?) :default nil}
   :max-skips {:doc "animals skipped in a row before the hunt gives up" :spec (a/int-in 1 nil) :default 3}})

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

(defn check
  "Started, or more adults of :mob in :radius than the pair rule keeps. Else it waits with reason :too-few (and :mob
  :keep)."
  [c]
  (or (boolean (or (:started (ctx/mem c))
                   (> (count (present c)) (keep-of c))))
      (ctx/wait c {:reason :too-few :mob (:mob (:args c)) :keep (keep-of c)})))

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [killed (:killed (ctx/mem c) 0)
        spared (if (and (= :keep reason) (pos? (keep-of c))) (max 0 (- (:count (:args c)) killed)) 0)
        remaining (count (present c))
        out (cond-> {:killed killed :reason reason :spared spared :remaining remaining}
              (= :gave-up reason) (merge (sh/shelter-hint c)))]
    (ctx/emit! c :hunt.done :info (assoc out :text (str "hunt done: " (name reason) ", killed " killed
                                                        ", spared " spared " of the ask, " remaining " adults left")))
    (ctx/result! c out)
    :done))

(defn give-up!
  "Warn and end after too many skipped animals in a row."
  [c]
  (let [{:keys [killed skipped]} (ctx/mem c)]
    (ctx/emit! c :hunt.gave-up :warn (merge {:killed (or killed 0) :skipped (vec skipped)
                                             :text (str "hunt gave up: " (count skipped) " animals skipped, killed " (or killed 0))}
                                            (sh/shelter-hint c)))
    (finish! c :gave-up)))

(defn attack-opts
  "Options for hunting.attack!: a wide search around the ask; ends the job after too many skips."
  [c]
  {:radius (+ (:radius (:args c)) 8) :waiting :yield :give-up give-up!})

(defn ^:async step
  "One step: attack the target, collect, finish, or pick the next target. :again, :yield (a child is waiting on the world) or :done."
  [c]
  (let [now (ctx/now c)
        wanted (:count (:args c))
        keep (keep-of c)]
    (ctx/update-mem! c #(-> % (update :started (fn [t] (or t now))) (assoc :keep keep)))
    (let [{:keys [target collecting killed]} (ctx/mem c)
          next-target (first (candidates c))]
      (cond
        target (await (hunting/attack! c (attack-opts c)))
        collecting (await (hunting/collect! c :yield))
        (>= (or killed 0) wanted) (finish! c :count)
        (<= (count (present c)) keep) (finish! c :keep)
        (nil? next-target) (await (hunting/search! c #(finish! % :none)))
        :else (do (ctx/update-mem! c assoc :target (.-id next-target) :misses 0)
                  (await (hunting/attack! c (attack-opts c))))))))

(defn ^:async round
  "The whole hunt: steps until it ends; yields only while a child waits on the world."
  [c]
  (let [r (await (pace/steps! c #(step c)))]
    (if (= :yield r) :continue r)))
