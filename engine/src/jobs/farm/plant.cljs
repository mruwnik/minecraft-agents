(ns jobs.farm.plant
  (:require [clojure.string :as str]
            [jobs.farm.permit :as permit]
            [engine.ctx :as ctx]
            [jobs.lib.crops :as crops]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]
            [jobs.farm.harvest :as harvest]
            [jobs.lib.world :as known]))

(def doc
  "Sow the bare farmland of a :box. A cell is bare when (x, min.y, z) is farmland and the block above is air.
  The seed is :seed, else the carried seed with the largest stack. Carrots and potatoes are sown only above the
  food reserve (jobs.lib.cost/food-reserve); harvest replants its own cut cells whatever the
  reserve. That pick is kept while it is carried, so one run sows one crop.
  Each round plants the bare cells within :reach, or walks to the nearest. A cell whose place is refused or
  unreachable, or whose walk is blocked, three times is skipped (warn plant.gave-up).
  Result: {:planted n :skipped [cells] :reason r}. :reason is :done, :none (no bare cell), :gave-up (cells
  were skipped) or :no-seed.
  :plan (optionally :part) makes the plan's crop cells the field instead (:box and :seed are then unused).
  Every planned cell standing bare is sown with the seed of the crop the plan wants there, when it is carried.
  Zones and the footprints of other active plans are asked before choosing a cell and again before the place.
  The result adds :refused [{:pos :reason}] and :short [seeds not carried]. :reason is :no-seed when the
  only cells left lack a seed. Cells another plan also claims are refused; one plant.declined warn (:reason :refused,
  :plans) names it, also when no cell is left to sow. The job declines (one plant.declined warn naming the plan and the reason) while
  the plan is missing, unreadable, has no crop cells, or no zone list has been read.
  Box mode zones: a cell in another owner's zone or claim, or in a plan's footprint, is left bare. If every
  cell is refused the job ends with :none. The job warns plant.declined once, with :reason :refused (or
  :no-zones when no zone list was read). :ignore-zones? true skips the check, so cells shared with another plan are sown too.")

(def args
  {:box {:doc "the field: {:min {:x :y :z} :max {:x :y :z}}, inclusive; the ground layer is y = (:y :min); required (without it the check declines)" :default nil}
   :seed {:doc "item name to plant; the carried seed with the largest count when nil" :default nil}
   :reach {:doc "cells whose centre is this close to the eye (place accepts 4.5) are planted without walking, in blocks" :default 4.2}
   :plan {:doc "id of a plan of the body's world whose crop cells are the field (then :box and :seed are not used)" :default nil}
   :part {:doc "with :plan, only the cells of this part" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def max-fails 3)

;; ------------------------------------------------------------------ pure helpers

(defn bare-cells
  "The bare cells of box (farmland with air above, ground layer y = min y) not in skipped, in x then z order."
  [p box skipped]
  (let [skipped (set skipped)
        {:keys [min max]} box
        y (:y min)]
    (->> (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))] {:x x :y y :z z})
         (remove skipped)
         (filter #(and (= "farmland" (u/block-name p %))
                       (= "air" (u/block-name p (update % :y inc))))))))

(defn sowable
  "{name count} of the carried items that may be sown: food crops (carrots, potatoes) only above the food reserve,
  everything else in full. A cell just harvested is replanted by jobs.farm.harvest and ignores the reserve."
  [inventory]
  (harvest/sowable-counts inventory))

(defn pick-seed
  "The seed to sow given the inventory: seed when it may be sown, else the largest sowable stack of a known seed;
  nil when none. Food crops count only above the food reserve."
  [seed inventory]
  (let [counts (sowable inventory)
        carried #(get counts % 0)]
    (if seed
      (when (pos? (carried seed)) seed)
      (->> (vals crops/seed-of)
           (filter #(pos? (carried %)))
           (sort-by #(- (carried %)))
           first))))

(declare planned sowing)

(defn count-fail
  "Count a fail of kind k (:fails or :walk-fails) on the cell at pos; the cell is skipped at the max-fails-th."
  [m k pos]
  (crops/count-entry m k pos max-fails #(update %1 :skipped (fnil conj []) %2)))

(defn owes?
  "A box is given and the run has started, or a bare cell that is not skipped exists and a seed is carried.
  A started run always passes, so that the round that finishes it runs."
  [p args m]
  (boolean
   (and (:box args)
        (or (:started m)
            (and (seq (bare-cells p (:box args) (:skipped m)))
                 (pick-seed (:seed args) (u/inventory p)))))))

(defn plan-check
  "A plan that can be worked and either a started run or a sowable cell."
  [c field]
  (cond
    (:trouble field) (ctx/wait c {:reason :plan-trouble :why (:trouble field)})
    (or (:started (ctx/mem c)) (seq (:ready (sowing c (:cells field))))) true
    :else (ctx/wait c {:reason :nothing-to-do})))

(defn sowable-cells
  "The ground cells of cells whose sowing (the cell above) the job may do; one warn when some are refused."
  [c cells]
  (let [ok (set (gate/allowed c :plant.declined "plant" :sow (map #(update % :y inc) cells)))]
    (filterv #(ok (update % :y inc)) cells)))

(defn box-check
  "owes?, and unless the run has started a bare cell that zones and claims let the job sow."
  [c]
  (let [{:keys [box] :as a} (:args c)
        m (ctx/mem c)]
    (or (and (owes? (:primitives c) a m)
             (boolean (or (:started m) (seq (sowable-cells c (bare-cells (:primitives c) box (:skipped m)))))))
        (ctx/wait c {:reason :nothing-to-do}))))

(defn check [c]
  (if (:plan (:args c))
    (plan-check c (planned c))
    (box-check c)))

;; ------------------------------------------------------------------ plan mode

(defn planned
  "Without :plan nil; else {:cells {pos crop}}, or {:trouble text} (warned once per reason) when the plan cannot be
  worked."
  [c]
  (when-let [id (:plan (:args c))]
    (let [part (:part (:args c))
          answer (known/plan c id)
          cells (harvest/crop-cells answer part)
          trouble (or (harvest/plan-trouble answer cells)
                      (when (nil? (known/zones c)) "no zone list has been read"))]
      (if-not trouble
        {:cells cells}
        (do (ctx/warn-once! c [id trouble] :plant.declined
                            {:plan id :part part :reason trouble
                             :text (str "plant declines plan " id (when part (str " part " part)) ": " trouble)})
            {:trouble trouble})))))

(defn sowing
  "How the planned bare cells divide: {:ready [{:pos :seed}] (seed carried, not skipped, permitted) :short #{seed}
  (carried none) :refused [{:pos :reason}]}. One plant.declined warn names what refuses the refused cells."
  [c cells]
  (let [plan (:plan (:args c))
        have (set (map key (filter (comp pos? val) (sowable (u/inventory (:primitives c))))))
        skipped (set (:skipped (ctx/mem c)))
        {:keys [short seeded]} (reduce (fn [acc {:keys [pos seed] :as debt}]
                                         (cond
                                           (skipped pos) acc
                                           (not (have seed)) (update acc :short conj seed)
                                           :else (update acc :seeded conj debt)))
                                       {:short #{} :seeded []}
                                       (harvest/planned-bare (:primitives c) cells))
        ok (set (gate/allowed c :plant.declined "plant" :sow (map :pos seeded) {:except plan}))]
    (reduce (fn [acc {:keys [pos] :as debt}]
              (if (ok pos)
                (update acc :ready conj debt)
                (if-let [reason (permit/refusal c plan :sow pos)]
                  (update acc :refused conj {:pos pos :reason reason})
                  acc)))
            {:ready [] :short short :refused []}
            seeded)))

(defn ^:async plan-place!
  "Place the seed of debt at its cell after asking the rules once more; a failure is counted against the cell."
  [c {:keys [pos seed]}]
  (when (permit/ok? c (:plan (:args c)) :sow pos)
    (let [r (await (ctx/act c :place (clj->js {:pos pos :item seed})))]
      (case (.-status r)
        ("placed" "occupied") (ctx/update-mem! c crops/inc-in :planted)
        "no-item" nil
        (ctx/update-mem! c count-fail :fails pos)))))

(defn finish-plan!
  [c {:keys [short refused]}]
  (let [m (ctx/mem c)
        skipped (vec (:skipped m))
        reason (cond (seq skipped) :gave-up (seq short) :no-seed (pos? (:planted m 0)) :done :else :none)
        result {:planted (:planted m 0) :skipped skipped :refused refused :short (vec (sort short)) :reason reason}]
    (when (seq skipped)
      (ctx/emit! c :plant.gave-up :warn
                 {:cells skipped
                  :text (str "plant gave up on " (count skipped) " cells: " (str/join ", " (map pr-str skipped)))}))
    (ctx/emit! c :plant.done :info
               {:planted (:planted result) :skipped-count (count skipped) :reason reason
                :text (str "plant done: planted " (:planted result) ", skipped " (count skipped) ", reason " (name reason))})
    (ctx/result! c result)
    :done))

(defn ^:async plan-round [c field]
  (when-not (:started (ctx/mem c))
    (ctx/update-mem! c assoc :started true))
  (let [{:keys [ready] :as found} (update (sowing c (:cells field)) :ready #(harvest/above-reserve (u/inventory (:primitives c)) %))]
    (if (empty? ready)
      (finish-plan! c found)
      (let [here (u/self-pos c)
            near (filterv #(<= (u/eye-dist here (:pos %)) (:reach (:args c))) ready)
            target (first (sort-by #(u/dist here (:pos %)) ready))
            walked (when (empty? near) (await (harvest/walk! c (:pos target) 3)))]
        (case walked
          :partial :continue
          :no-path (do (ctx/update-mem! c count-fail :walk-fails (:pos target)) :continue)
          :blocked (do (ctx/update-mem! c count-fail :walk-fails (:pos target)) :continue)
          (do (loop [todo (if (seq near) near [target])]
                (when (seq todo)
                  (await (plan-place! c (first todo)))
                  (recur (rest todo))))
              :continue))))))

;; ------------------------------------------------------------------ steps

(defn finish!
  [c reason]
  (let [m (ctx/mem c)
        skipped (vec (:skipped m))
        result {:planted (:planted m 0) :skipped skipped :reason reason}]
    (when (seq skipped)
      (ctx/emit! c :plant.gave-up :warn
                 {:cells skipped
                  :text (str "plant gave up on " (count skipped) " cells: " (str/join ", " (map pr-str skipped)))}))
    (ctx/emit! c :plant.done :info
               {:planted (:planted result) :skipped-count (count skipped) :reason reason
                :text (str "plant done: planted " (:planted result) ", skipped " (count skipped) ", reason " (name reason))})
    (ctx/result! c result)
    :done))

(defn ^:async plant-one!
  "Place seed on cell and settle by the status: :no-seed when the seed is gone, else nil."
  [c cell seed]
  (when (seq (sowable-cells c [cell]))
    (let [r (await (ctx/act c :place (clj->js {:pos (update cell :y inc) :item seed})))]
      (case (.-status r)
        ("placed" "occupied") (do (ctx/update-mem! c crops/inc-in :planted) nil)
        "no-item" :no-seed
        (do (ctx/update-mem! c count-fail :fails cell) nil)))))

(defn ^:async plant-all!
  "Plant each cell in turn; :no-seed when the seeds ran out, else nil."
  [c cells seed]
  (loop [todo cells]
    (when (seq todo)
      (or (await (plant-one! c (first todo) seed))
          (recur (rest todo))))))

(defn ^:async box-round [c]
  (when-not (:started (ctx/mem c))
    (ctx/update-mem! c assoc :started true))
  (let [p (:primitives c)
        m (ctx/mem c)
        cells (sowable-cells c (bare-cells p (:box (:args c)) (:skipped m)))
        inventory (u/inventory p)
        kept (:sowing m)                ; the seed picked first is kept while carried: one run, one crop
        seed (if (and (nil? (:seed (:args c))) kept (pick-seed kept inventory))
               kept
               (pick-seed (:seed (:args c)) inventory))]
    (when (and seed (not= seed kept))
      (ctx/update-mem! c assoc :sowing seed))
    (cond
      (empty? cells) (finish! c (cond (seq (:skipped m)) :gave-up (pos? (:planted m 0)) :done :else :none))
      (nil? seed) (finish! c :no-seed)
      :else
      (let [here (u/self-pos c)
            near (filterv #(<= (u/eye-dist here %) (:reach (:args c))) cells)
            target (harvest/nearest p cells)
            walked (when (empty? near) (await (harvest/walk! c target 3)))]
        (case walked
          :partial :continue
          :no-path (do (ctx/update-mem! c count-fail :walk-fails target) :continue)
          :blocked (do (ctx/update-mem! c count-fail :walk-fails target) :continue)
          (if (= :no-seed (await (plant-all! c (vec (take (max 1 (get (sowable inventory) seed 0)) (if (seq near) near [target]))) seed)))
            (finish! c :no-seed)
            :continue))))))

(defn ^:async round [c]
  (let [field (planned c)]
    (cond
      (nil? field) (await (box-round c))
      (:trouble field) :declined
      :else (await (plan-round c field)))))
