(ns jobs.animals.cull
  (:require [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.combat :as combat]
            [jobs.lib.util :as u]
            [jobs.combat.hunt :as hunt]))

(def doc
  "Thin a herd of one kind (:mob) down to :keep adults and collect the drops.

  The herd is the animals of the kind inside a bound: :box when set, else :radius blocks around :centre
  ({:x :z}, horizontal), else :radius blocks around the body. Babies are never targets and never count as
  adults. The check waits (:too-few) while :keep or fewer adults are in the bound. A started job always passes,
  so a cut job resumes.

  Each round does one of these:
  - With a target: the attack child fights it. It is booked killed, or skipped when attack gave up or lost it.
    :max-skips skips in a row end the job :gave-up (warn cull.gave-up).
  - After a kill: collect-drops picks up :drops (nil: the kind's entry in jobs.combat.hunt/drops, else every
    item within :collect-radius).
  - Otherwise it picks the next candidate: adults in the bound not skipped, those that are not unhittable first
    (an animal behind a wall comes last), then nearest first. The target stays until killed or given up on.

  Ends with info cull.done and {:killed n :remaining adults-in-bound :babies n :reason r :skipped [ids]}. Skipped
  animals stay alive and still count as adults, so they still breed.
  Reasons:
  - :count: :killed reached :count (nil: no cap).
  - :keep: at most :keep adults remain. The census is live every round, so the last :keep are never taken.
  - :unreachable: no candidate is left, some were skipped (warn cull.gave-up).
  - :none: no candidate on two rounds in a row, with a 1 s wait between (warn cull.gave-up); :refused (or
    :no-zones) instead when the zone rules refused the adults.
  - :gave-up: too many skips in a row.

  Zones: an adult standing in another owner's zone or claim, or in a plan's footprint, is left alone (warn
  cull.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(def args
  {:mob {:doc "mob type name of the animals to thin" :default "cow"}
   :keep {:doc "adults of the kind to leave alive in the bound (babies do not count)" :default 2}
   :centre {:doc "nil, or {:x :z}: the bound is :radius blocks (horizontal) around it" :default nil}
   :radius {:doc "animals within this many blocks of :centre, or of the body when there is no :centre, are the herd" :default 16}
   :box {:doc "nil, or {:min {:x :y :z} :max {:x :y :z}}: inclusive bound that replaces :centre and :radius" :default nil}
   :count {:doc "most animals to kill in one run; nil: no cap" :default nil}
   :collect-radius {:doc "how far around to collect drops after a kill" :default 8}
   :drops {:doc "item names to collect after a kill; nil: the kind's entry in the drops table, else every item" :default nil}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}
   :max-skips {:doc "animals skipped in a row before the cull gives up" :default 3}
   :ignore-zones? animals/ignore-zones-arg})

(defn corners
  "The eight corners of a box."
  [{lo :min hi :max}]
  (for [x [(:x lo) (:x hi)] y [(:y lo) (:y hi)] z [(:z lo) (:z hi)]]
    {:x x :y y :z z}))

(defn reach
  "How far from the body the entity query must look to cover the bound."
  [c]
  (let [{:keys [box centre radius]} (:args c)
        here (u/self-pos c)]
    (cond
      box (apply max (map #(u/dist here %) (corners box)))
      centre (+ radius (js/Math.hypot (- (:x centre) (:x here)) (- (:z centre) (:z here))))
      :else radius)))

(defn in-bound?
  "True when the position lies inside the bound (a box, else around the centre, else around the body)."
  [c pos]
  (let [{:keys [box centre radius]} (:args c)
        {:keys [x y z]} pos]
    (cond
      box (and (<= (:x (:min box)) x (:x (:max box)))
               (<= (:y (:min box)) y (:y (:max box)))
               (<= (:z (:min box)) z (:z (:max box))))
      centre (<= (js/Math.hypot (- x (:x centre)) (- z (:z centre))) radius)
      :else (<= (u/dist (u/self-pos c) pos) radius))))

(defn herd
  "The entities named :mob in the bound, never players or items."
  [c]
  (->> (array-seq (.entities (:primitives c) #js {:radius (reach c) :names #js [(:mob (:args c))] :max 64}))
       (remove #(contains? #{"player" "item"} (.-kind %)))
       (filter #(in-bound? c (u/pos-of (.-pos %))))
       vec))

(defn census
  "{:adults [...] :babies [...]} of the herd in the bound."
  [c]
  (let [{babies true adults false} (group-by #(true? (.-baby %)) (herd c))]
    {:adults (vec adults) :babies (vec babies)}))

(defn candidates
  "The adults in the bound not skipped: reachable-looking ones first, then nearest."
  [c adults]
  (let [skipped (set (:skipped (ctx/mem c)))
        here (u/self-pos c)]
    (->> (animals/allowed c :cull.declined "cull" :harvest adults)
         (remove #(contains? skipped (.-id %)))
         (sort-by (juxt #(if (false? (.-hittable %)) 1 0)
                        #(u/dist here (u/pos-of (.-pos %))))))))

(defn check
  "Started, or more adults of :mob inside the bound than :keep. Else it waits with reason :too-few (and :mob :keep)."
  [c]
  (or (boolean (or (:started (ctx/mem c))
                   (> (count (:adults (census c))) (:keep (:args c)))))
      (ctx/wait c {:reason :too-few :mob (:mob (:args c)) :keep (:keep (:args c))})))

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [killed (:killed (ctx/mem c) 0)
        {:keys [adults babies]} (census c)
        out {:killed killed :remaining (count adults) :babies (count babies)
             :reason reason :skipped (vec (:skipped (ctx/mem c)))}]
    (ctx/emit! c :cull.done :info (assoc out :text (str "cull done: " (name reason) ", killed " killed
                                                       ", " (count adults) " adults left")))
    (ctx/result! c out)
    :done))

(defn give-up!
  "Warn about the animals left alive, then end with reason."
  [c reason]
  (let [{:keys [killed skipped]} (ctx/mem c)]
    (ctx/emit! c :cull.gave-up :warn {:reason reason :killed (or killed 0) :skipped (vec skipped)
                                      :text (str "cull gave up (" (name reason) "): " (count skipped)
                                                 " animals skipped, killed " (or killed 0))})
    (finish! c reason)))

(defn ^:async attack!
  "One round of the attack child on the target; books its end."
  [c]
  (let [{:keys [weapons max-skips]} (:args c)
        target (:target (ctx/mem c))
        r (await (ctx/call-child c :attack 'jobs.combat.attack
                                 {:targets [target] :radius (+ (reach c) 16) :weapons weapons :lost-s 1 :absent :done}))]
    (cond
      (not= :done r) :continue
      :else (do (hunt/book-outcome! c target)
                (if (>= (:skips (ctx/mem c) 0) max-skips)
                  (give-up! c :gave-up)
                  :continue)))))

(defn ^:async collect!
  "One round of the collect-drops child; done collecting when it is."
  [c]
  (let [{:keys [mob collect-radius] :as a} (:args c)
        r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 {:radius collect-radius :filter (or (:drops a) (get hunt/drops mob))}))]
    (when (= :done r) (ctx/update-mem! c dissoc :collecting))
    :continue))

(defn ^:async search!
  "Nothing to attack: wait and look once more, then end :none."
  [c]
  (let [misses (inc (:misses (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :misses misses)
    (if (>= misses 2)
      (give-up! c (or (animals/refusal c) :none))
      (do (await (ctx/act c :wait #js {:ms 1000}))
          :continue))))

(defn ^:async round [c]
  (let [now (ctx/now c)
        {:keys [keep] wanted :count} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [{:keys [target collecting killed skipped]} (ctx/mem c)]
      (if target
        (await (attack! c))
        (if collecting
          (await (collect! c))
          (let [adults (:adults (census c))
                next-target (first (candidates c adults))]
            (cond
              (and wanted (>= (or killed 0) wanted)) (finish! c :count)
              (<= (count adults) keep) (finish! c :keep)
              (and (nil? next-target) (seq skipped)) (give-up! c :unreachable)
              (nil? next-target) (await (search! c))
              :else (do (ctx/update-mem! c assoc :target (.-id next-target) :misses 0)
                        (await (attack! c))))))))))
