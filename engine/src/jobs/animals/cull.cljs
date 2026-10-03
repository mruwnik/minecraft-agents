(ns jobs.animals.cull
  (:require [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]
            [jobs.combat.hunt :as hunt]))

(def doc
  "Thin a herd of one kind (:mob) down to :keep adults, collecting the drops.
  The herd is the animals of the kind inside a bound: :box ({:min {:x :y :z}
  :max {:x :y :z}}, inclusive) when set, else :radius blocks around :centre
  ({:x :z}, horizontal) when set, else :radius blocks around the body. Babies
  are never targets and never counted as adults (an entity is an adult unless it
  reports baby true). The check passes while more than :keep adults are in the
  bound, and always once the job has started (a cut job resumes); otherwise it
  declines and the job does nothing. One step per round: (1) with a :target, the
  attack child fights it (every round it is not done); it is booked killed, or
  skipped when attack gave up on it or lost it, and :skips counts skipped
  animals in a row; :max-skips of them in a row ends the job :gave-up (warn
  cull.gave-up); (2) after a target the collect-drops child picks up :drops
  (nil: the kind's entry in jobs.combat.hunt/drops, else every item within
  :collect-radius); (3) :killed reaching :count (nil: no cap) ends :count;
  (4) at most :keep adults in the bound ends :keep (skipped animals still count,
  they still breed; the census is live every round, so the last :keep adults are
  never taken); (5) no candidate: when some animal was skipped as unreachable the
  job ends :unreachable at once (warn cull.gave-up), else it ends :none on the
  second round in a row that finds none, after a 1 s wait (look twice; warn
  cull.gave-up); (6) else the best candidate becomes the :target and is attacked
  in the same round. Candidates are the adults in the bound not skipped, those
  whose hittable is not false first (an animal behind a wall comes last), then
  nearest first. The target stays until it is killed or given up on. Hands over
  {:killed n :remaining adults-in-bound :babies babies-in-bound :reason r
  :skipped [ids]} (info cull.done) with reason :keep, :count, :unreachable
  (adults above :keep remain but every one was given up on), :gave-up or :none.")

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
   :max-skips {:doc "animals skipped in a row before the cull gives up" :default 3}})

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
    (->> adults
         (remove #(contains? skipped (.-id %)))
         (sort-by (juxt #(if (false? (.-hittable %)) 1 0)
                        #(u/dist here (u/pos-of (.-pos %))))))))

(defn check [c]
  (boolean (or (:started (ctx/mem c))
               (> (count (:adults (census c))) (:keep (:args c))))))

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
      (give-up! c :none)
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
