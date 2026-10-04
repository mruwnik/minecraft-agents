(ns jobs.farm.plant
  (:require [clojure.string :as str]
            [engine.access.permit :as permit]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [jobs.farm.harvest :as harvest]))

(def doc
  "Sow the bare farmland of a :box. A cell is bare when the block at (x, min.y, z) is farmland and
  the block above it is air (an unloaded block is not bare). Each round takes the bare cells that
  are not skipped and the seed (the :seed arg, else the carried seed with the largest stack):
  no cell ends the job with reason :none (:done once something was planted, :gave-up when cells
  were skipped), no seed with :no-seed; otherwise the cells within :reach are planted, or the body
  walks to the nearest. A cell whose place is refused or unreachable, or whose walk is blocked,
  three times is skipped (a plant.gave-up warn). Result {:planted n :skipped [cells] :reason r}.

  With :plan (and optionally :part) the field is that plan's crop cells instead (wants {:crop c} of a crop harvest
  knows; the :box and :seed arguments are not used): every planned cell standing bare (air over farmland) is sown with
  the seed of the crop the plan wants there, when that seed is carried. Every sowing is asked of
  engine.access.rules/may-place? with the zones and the footprints of the OTHER active plans, when the cell is chosen
  and again right before the place; a refused cell is left bare and listed in the result's :refused [{:pos :reason}].
  The result is {:planted :skipped :refused :short [seed names carried none of] :reason r}, :reason :no-seed when
  nothing else was left to sow and some seed was short. The check declines, with one plant.declined warn naming the
  plan and the reason, while the plan is missing, unreadable, holds no crop cells (in :part) or no zone
  list has been read.")

(def args
  {:box {:doc "the field: {:min {:x :y :z} :max {:x :y :z}}, inclusive; the ground layer is y = (:y :min); required (without it the check declines)" :default nil}
   :seed {:doc "item name to plant; the carried seed with the largest count when nil" :default nil}
   :reach {:doc "cells whose centre is this close to the eye (place accepts 4.5) are planted without walking, in blocks" :default 4.2}
   :plan {:doc "id of a plan of the body's world whose crop cells are the field (then :box and :seed are not used)" :default nil}
   :part {:doc "with :plan, only the cells of this part" :default nil}})

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

(defn pick-seed
  "The seed to plant given the inventory: seed when it is carried, else the largest carried stack of a known seed; nil when none."
  [seed inventory]
  (let [counts (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} inventory)]
    (if seed
      (when (pos? (get counts seed 0)) seed)
      (->> (vals harvest/seed-of)
           (filter #(pos? (get counts % 0)))
           (sort-by #(- (counts %)))
           first))))

(declare planned sowing)

(defn count-fail
  "Count a fail of kind k (:fails or :walk-fails) on the cell at pos; the cell is skipped at the max-fails-th."
  [m k pos]
  (let [n (inc (or (:n (first (filter #(= pos (:pos %)) (k m)))) 0))]
    (if (>= n max-fails)
      (update m :skipped (fnil conj []) pos)
      (update m k (fn [entries] (conj (vec (remove #(= pos (:pos %)) entries)) {:pos pos :n n}))))))

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
  (boolean (and (not (:trouble field))
                (or (:started (ctx/mem c)) (seq (:ready (sowing c (:cells field))))))))

(defn check [c]
  (if (:plan (:args c))
    (plan-check c (planned c))
    (owes? (:primitives c) (:args c) (ctx/mem c))))

;; ------------------------------------------------------------------ plan mode

(defn planned
  "Without :plan nil; else {:cells {pos crop}}, or {:trouble text} (warned once per reason) when the plan cannot be
  worked."
  [c]
  (when-let [id (:plan (:args c))]
    (let [part (:part (:args c))
          answer (ctx/plan c id)
          cells (harvest/crop-cells answer part)
          trouble (or (harvest/plan-trouble answer cells)
                      (when (nil? (ctx/zones c)) "no zone list has been read"))]
      (if-not trouble
        {:cells cells}
        (do (ctx/warn-once! c [id trouble] :plant.declined
                            {:plan id :part part :reason trouble
                             :text (str "plant declines plan " id (when part (str " part " part)) ": " trouble)})
            {:trouble trouble})))))

(defn sowing
  "How the planned bare cells divide: {:ready [{:pos :seed}] (seed carried, not skipped, permitted) :short #{seed}
  (carried none) :refused [{:pos :reason}]}."
  [c cells]
  (let [plan (:plan (:args c))
        have (set (map :name (u/inventory (:primitives c))))
        skipped (set (:skipped (ctx/mem c)))]
    (reduce (fn [acc {:keys [pos seed] :as debt}]
              (cond
                (skipped pos) acc
                (not (have seed)) (update acc :short conj seed)
                (permit/ok? c plan :sow pos) (update acc :ready conj debt)
                :else (if-let [reason (permit/refusal c plan :sow pos)]
                        (update acc :refused conj {:pos pos :reason reason})
                        acc)))
            {:ready [] :short #{} :refused []}
            (harvest/planned-bare (:primitives c) cells))))

(defn ^:async plan-place!
  "Place the seed of debt at its cell after asking the rules once more; a failure is counted against the cell."
  [c {:keys [pos seed]}]
  (when (permit/ok? c (:plan (:args c)) :sow pos)
    (let [r (await (ctx/act c :place (clj->js {:pos pos :item seed})))]
      (case (.-status r)
        ("placed" "occupied") (ctx/update-mem! c harvest/inc-in :planted)
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
  (let [{:keys [ready] :as found} (sowing c (:cells field))]
    (if (empty? ready)
      (finish-plan! c found)
      (let [here (u/self-pos c)
            near (filterv #(<= (harvest/eye-dist here (:pos %)) (:reach (:args c))) ready)
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
  (let [r (await (ctx/act c :place (clj->js {:pos (update cell :y inc) :item seed})))]
    (case (.-status r)
      ("placed" "occupied") (do (ctx/update-mem! c harvest/inc-in :planted) nil)
      "no-item" :no-seed
      (do (ctx/update-mem! c count-fail :fails cell) nil))))

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
        cells (vec (bare-cells p (:box (:args c)) (:skipped m)))
        seed (pick-seed (:seed (:args c)) (u/inventory p))]
    (cond
      (empty? cells) (finish! c (cond (seq (:skipped m)) :gave-up (pos? (:planted m 0)) :done :else :none))
      (nil? seed) (finish! c :no-seed)
      :else
      (let [here (u/self-pos c)
            near (filterv #(<= (harvest/eye-dist here %) (:reach (:args c))) cells)
            target (harvest/nearest p cells)
            walked (when (empty? near) (await (harvest/walk! c target 3)))]
        (case walked
          :partial :continue
          :no-path (do (ctx/update-mem! c count-fail :walk-fails target) :continue)
          :blocked (do (ctx/update-mem! c count-fail :walk-fails target) :continue)
          (if (= :no-seed (await (plant-all! c (if (seq near) near [target]) seed)))
            (finish! c :no-seed)
            :continue))))))

(defn ^:async round [c]
  (let [field (planned c)]
    (cond
      (nil? field) (await (box-round c))
      (:trouble field) :declined
      :else (await (plan-round c field)))))
