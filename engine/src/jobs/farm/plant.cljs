(ns jobs.farm.plant
  (:require [clojure.string :as str]
            [jobs.farm.permit :as permit]
            [engine.ctx :as ctx]
            [jobs.lib.crops :as crops]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.farm.harvest :as harvest]
            [jobs.lib.world :as known]))

(def doc
  "Sow the bare farmland of a :box. A cell is bare when (x, min.y, z) is farmland and the block above is air.
  A missing seed is fetched (jobs.lib.fetch, child :fetch) before the run starts, unless :fetch is false; the check then waits :need.
  The seed is :seed, else the carried seed with the largest stack. Carrots and potatoes are sown only above the
  food reserve (jobs.lib.cost/food-reserve); harvest replants its own cut cells whatever the
  reserve. That pick is kept while it is carried, so one run sows one crop.
  One call plants the bare cells within :reach, walking to the nearest when none is, until the field is done. A cell whose place is refused or
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
  :no-zones when no zone list was read). The check waits :need (seed) when none is carried, :nothing-to-do when no cell
  is bare, :refused / :no-zones when zones refuse. :ignore-zones? true skips the check, so cells shared with another plan are sown too.")

(def args
  {:box {:doc "the field: {:min {:x :y :z} :max {:x :y :z}}, inclusive; the ground layer is y = (:y :min); required (without it the check declines)" :default nil}
   :seed {:doc "item name to plant; the carried seed with the largest count when nil" :default nil}
   :reach {:doc "cells whose centre is this close to the eye (place accepts 4.5) are planted without walking, in blocks" :default 4.2}
   :plan {:doc "id of a plan of the body's world whose crop cells are the field (then :box and :seed are not used)" :default nil}
   :part {:doc "with :plan, only the cells of this part" :default nil}
   :fetch {:doc "get a missing seed (jobs.lib.fetch): true, a set of kinds or a map of limits; false waits :need" :default true}
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

(def job-sym 'jobs.farm.plant)

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

(defn need-of
  "The :need wait for n seeds of a set of item names (non-empty)."
  [seeds n]
  (if (= 1 (count seeds))
    {:reason :need :item (first seeds) :count n}
    {:reason :need :any-of (vec (sort seeds)) :count n}))

(defn plan-check
  "A plan that can be worked and either a started run or a sowable cell. Otherwise waits :need (a seed the plan wants
  is not carried, fetched when :fetch allows) or the gate's wait / :nothing-to-do."
  [c field]
  (if (:trouble field)
    (ctx/wait c {:reason :plan-trouble :why (:trouble field)})
    (let [{:keys [ready short]} (when-not (:started (ctx/mem c)) (sowing c (:cells field)))]
      (cond
        (:started (ctx/mem c)) true
        (seq ready) true
        (seq short) (fetch/check c job-sym (need-of short (count (harvest/planned-bare (:primitives c) (:cells field)))))
        :else (gate/wait-unless-set c {:reason :nothing-to-do})))))

(defn sowable-cells
  "The ground cells of cells whose sowing (the cell above) the job may do; one warn when some are refused."
  [c cells]
  (let [ok (set (gate/allowed c :plant.declined "plant" :sow (map #(update % :y inc) cells)))]
    (filterv #(ok (update % :y inc)) cells)))

(defn seed-need
  "The :need wait for n of the named seed, or of any seed."
  [seed n]
  (if seed
    {:reason :need :item seed :count n}
    {:reason :need :any-of (vec (distinct (vals crops/seed-of))) :count n}))

(defn problem
  "The :need wait for a missing seed before the run has started (box: bare cells and none carried; plan: no cell
  ready and a seed short), else nil."
  [c]
  (let [m (ctx/mem c)
        p (:primitives c)]
    (when-not (:started m)
      (if (:plan (:args c))
        (let [field (planned c)
              {:keys [ready short]} (when (:cells field) (sowing c (:cells field)))]
          (when (and (empty? ready) (seq short))
            (need-of short (count (harvest/planned-bare p (:cells field))))))
        (let [{:keys [box seed]} (:args c)
              bare (when box (bare-cells p box (:skipped m)))]
          (when (and (seq bare) (not (pick-seed seed (u/inventory p))))
            (seed-need seed (count bare))))))))

(defn box-check
  "A started run, or a bare cell that zones and claims let the job sow with a carried seed. Otherwise waits with
  :nothing-to-do (no bare cell), :need (no seed carried) or the gate's :refused / :no-zones."
  [c]
  (let [{:keys [box seed]} (:args c)
        p (:primitives c)
        m (ctx/mem c)
        bare (when box (bare-cells p box (:skipped m)))]
    (cond
      (:started m) true
      (empty? bare) (ctx/wait c {:reason :nothing-to-do})
      (not (pick-seed seed (u/inventory p)))
      (fetch/check c job-sym (seed-need seed (count bare)))
      :else (or (boolean (seq (sowable-cells c bare)))
                (gate/wait-unless-set c {:reason :nothing-to-do})))))

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
          :no-path (do (ctx/update-mem! c count-fail :walk-fails (:pos target)) :again)
          :blocked (do (ctx/update-mem! c count-fail :walk-fails (:pos target)) :again)
          (do (loop [todo (if (seq near) near [target])]
                (when (seq todo)
                  (await (plan-place! c (first todo)))
                  (recur (rest todo))))
              :again))))))

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
          :no-path (do (ctx/update-mem! c count-fail :walk-fails target) :again)
          :blocked (do (ctx/update-mem! c count-fail :walk-fails target) :again)
          (if (= :no-seed (await (plant-all! c (vec (take (max 1 (get (sowable inventory) seed 0)) (if (seq near) near [target]))) seed)))
            (finish! c :no-seed)
            :again))))))

(defn ^:async step [c]
  (let [field (planned c)
        fetched (when (problem c) (await (fetch/fetch! c job-sym problem)))]
    (cond
      fetched fetched
      (nil? field) (await (box-round c))
      (:trouble field) :declined
      :else (await (plan-round c field)))))

(defn ^:async round
  "The whole attempt: loop the steps until the field is sown or no seed or cell is left; :continue only while a walk or
  fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
