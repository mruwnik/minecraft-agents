(ns jobs.animals.cull
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.started :as started]
            [jobs.lib.animals :as animals]
            [jobs.lib.util :as u]
            [jobs.lib.hunting :as hunting]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]))

(def doc
  "Thin a herd of one kind (:mob) down to :keep adults and collect the drops.

  The herd is the animals of the kind inside a bound: :box when set, else :radius blocks around :centre
  ({:x :z}, horizontal), else :radius blocks around the body. Babies are never targets and never count as
  adults. With :keep or fewer adults in the bound at the start the goal holds and it ends done :already; it waits
  while the zone rules refuse every adult, so a held cull never blocks the queue. The weapons are the call's :use :tools (default
  the best carried).

  One call is the whole run, a loop of these:
  - With a target: the attack child fights it. It is booked killed, or skipped when attack gave up or lost it.
    :max-skips skips in a row end the job :gave-up (warn cull.gave-up).
  - After a kill: collect-drops picks up :drops (nil: the kind's entry in jobs.lib.hunting/drops, else every
    item within :collect-radius).
  - Otherwise it picks the next candidate: adults in the bound not skipped, those that are not unhittable first
    (an animal behind a wall comes last), then nearest first. The target stays until killed or given up on.

  With :deposit-first? true it first deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!).

  Ends with info cull.done and the data {:killed n :remaining adults-in-bound :babies n :reason r :skipped [ids]}.
  Skipped animals stay alive and still count as adults, so they still breed. The ending: :done for :count and :keep,
  and for any other reason when the call's :least (default 1) animals were killed; else condition-failed for
  :refused and :no-zones found after the start, nothing-possible for the rest, with r as the cause. It yields (:continue) only while a
  child waits on the world.
  Reasons:
  - :count: :killed reached :count (nil: no cap).
  - :keep: at most :keep adults remain. The census is live at every step, so the last :keep are never taken.
  - :unreachable: no candidate is left, some were skipped (warn cull.gave-up).
  - :none: no candidate on two looks in a row, with a 1 s wait between (warn cull.gave-up); :refused (or
    :no-zones) instead when the zone rules refused the adults.
  - :gave-up: too many skips in a row.

  Zones: an adult standing in another owner's zone or claim, or in a plan's footprint, is left alone (warn
  cull.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:mob {:doc "mob type name of the animals to thin" :spec a/name? :default "cow"}
   :keep {:doc "adults of the kind to leave alive in the bound (babies do not count)" :spec (a/int-in 0 nil) :default 2}
   :centre {:doc "nil, or {:x :z}: the bound is :radius blocks (horizontal) around it" :spec (a/map-with {:x number? :z number?}) :default nil}
   :radius {:doc "animals within this many blocks of :centre, or of the body when there is no :centre, are the herd" :spec (a/num-in 0 nil) :default 16}
   :box {:doc "nil, or {:min {:x :y :z} :max {:x :y :z}}: inclusive bound that replaces :centre and :radius" :spec a/box? :default nil}
   :color {:doc "sheep only: kill only sheep of this wool colour (e.g. \"white\"); nil: any. :keep still counts every adult" :spec a/name? :default nil}
   :count {:doc "most animals to kill in one run; nil: no cap" :spec (a/int-in 1 nil) :default nil}
   :collect-radius {:doc "how far around to collect drops after a kill" :spec (a/num-in 0 nil) :default 8}
   :drops {:doc "item names to collect after a kill; nil: the kind's entry in the drops table, else every item" :spec (a/coll-of a/item?) :default nil}
   :deposit-first? {:doc "deposit at the known chest first when the bag is fullish and the chest is near (jobs.lib.trip/ready!)" :spec boolean? :default false}
   :max-skips {:doc "animals skipped in a row before the cull gives up" :spec (a/int-in 1 nil) :default 3}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false :spec boolean?}})

(def target [:mob :box :centre])

(a/defargs settings
  {::time-s {:default 300 :doc "Seconds a cull may take." :spec (a/num-in 0 nil)}})

(def defaults {:least 1 :spend {:time ::time-s} :use {:tools :attack}})

(defn corners
  "The eight corners of a box of cells: the far corners are the outer edges of the max cells."
  [{lo :min hi :max}]
  (for [x [(:x lo) (inc (:x hi))] y [(:y lo) (inc (:y hi))] z [(:z lo) (inc (:z hi))]]
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
      box (animals/in-box? box pos)
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
        color (:color (:args c))
        here (u/self-pos c)]
    (->> (animals/allowed c :cull.declined "cull" :harvest (cond->> adults color (filter #(= color (.-color %)))))
         (remove #(contains? skipped (.-id %)))
         (sort-by (juxt #(if (false? (.-hittable %)) 1 0)
                        #(u/dist here (u/pos-of (.-pos %))))))))

(defn too-few
  "The :too-few wait: :mob, :keep and the adults :in-range of the bound."
  [c]
  (let [{:keys [mob keep]} (:args c)
        in-range (count (:adults (census c)))]
    {:reason :too-few :mob mob :keep keep :in-range in-range
     :text (str "cull waits: " in-range " " mob " adults in the bound, keeping " keep)}))

(defn start-fact
  "The cause that bars the first step, or nil: :keep or fewer adults, or every adult refused by the zone rules
  (:refused, :no-zones; a quiet look, the decline warn comes with the first round)."
  [c]
  (let [{:keys [keep color ignore-zones?]} (:args c)
        adults (:adults (census c))
        targets (cond->> adults color (filter #(= color (.-color %))))]
    (cond
      (<= (count adults) keep) (too-few c)
      (and (seq targets) (not ignore-zones?))
      (when-let [why (animals/refusal-of c :harvest targets)]
        {:reason why :text (str "cull waits: every " (:mob (:args c)) " in the bound is refused by the zone rules (" (name why) ")")}))))

(def conditions "Waits on a start fact (start-fact), at the call's start only." [[:fn (started/at-start start-fact)]])

(defn done?
  "The animals killed, and the adults above :keep that are left: done once no more than :keep stand in the bound."
  [c]
  (let [left (max 0 (- (count (:adults (census c))) (:keep (:args c))))]
    {:done? (zero? left) :units :animals :done (:killed (ctx/mem c) 0) :left left}))

(defn quote-of [c]
  {:uses {:time [0 (* 20 (or (:count (:args c)) 4)) ##Inf]}})

(defn ending-of
  "The ending of a cull of killed animals that stopped for reason: :done for :count and :keep or when the call's :least
  was killed, :condition-failed for a refusal, else :nothing-possible with reason as the cause; data its result."
  [c reason killed data]
  (let [least (:least (ctx/frozen-call c) 1)
        need (if (= :all least) (or (:count (:args c)) 1) least)]
    (cond
      (or (#{:count :keep} reason) (>= killed need)) (ctx/end c :done nil data)
      (#{:refused :no-zones} reason) (ctx/end c :condition-failed reason data)
      :else (ctx/end c :nothing-possible reason data))))

(defn finish!
  "Emit the outcome and end the call (ending-of)."
  [c reason]
  (let [killed (:killed (ctx/mem c) 0)
        {:keys [adults babies]} (census c)
        out {:killed killed :remaining (count adults) :babies (count babies)
             :reason reason :skipped (vec (:skipped (ctx/mem c)))}]
    (ctx/emit! c :cull.done :info (assoc out :text (str "cull done: " (name reason) ", killed " killed
                                                       ", " (count adults) " adults left")))
    (ending-of c reason killed out)))

(defn give-up!
  "Warn about the animals left alive, then end with reason."
  [c reason]
  (let [{:keys [killed skipped]} (ctx/mem c)]
    (ctx/emit! c :cull.gave-up :warn {:reason reason :killed (or killed 0) :skipped (vec skipped)
                                      :text (str "cull gave up (" (name reason) "): " (count skipped)
                                                 " animals skipped, killed " (or killed 0))})
    (finish! c reason)))

(defn attack-opts
  "Options for hunting.attack!: a search around the reach; ends with :gave-up."
  [c]
  {:radius (+ (reach c) 16) :give-up #(give-up! % :gave-up)
   :job "jobs.animals.cull" :rule-fn (constantly :cull)})

(defn none!
  "Nothing left to attack: end with the refusal, else :none."
  [c]
  (give-up! c (or (animals/refusal c) :none)))

(defn ^:async step
  "One step: attack the target, collect, finish, or pick the next target. :again, :yield (a child waits on the world) or an ending."
  [c]
  (let [{:keys [keep] wanted :count} (:args c)]
    (let [{:keys [target collecting killed skipped]} (ctx/mem c)]
      (cond
        target (await (hunting/attack! c (attack-opts c)))
        collecting (await (hunting/collect! c :yield))
        :else
        (let [adults (:adults (census c))
              next-target (first (candidates c adults))]
          (cond
            (and wanted (>= (or killed 0) wanted)) (finish! c :count)
            (<= (count adults) keep) (finish! c :keep)
            (and (nil? next-target) (seq skipped)) (give-up! c :unreachable)
            (nil? next-target) (await (hunting/search! c none!))
            :else (do (ctx/update-mem! c assoc :target (.-id next-target) :misses 0)
                      (await (hunting/attack! c (attack-opts c))))))))))

(defn ^:async round
  "The whole attempt: loop the steps until one ends or yields."
  [c]
  (if-let [t (and (:deposit-first? (:args c)) (await (trip/ready! c)))]
    t
    (let [r (await (pace/steps! c #(step c)))]
      (if (= :yield r) :continue r))))
