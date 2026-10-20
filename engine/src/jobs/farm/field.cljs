(ns jobs.farm.field
  "Reading a farm field: the crop cells of a plan, which are ripe or bare, where to walk."
  (:require [jobs.lib.crops :as crops]
            [jobs.lib.farm :as farm]
            [jobs.lib.util :as u]))

(def ripe-age crops/ripe-age)

(def seed-of crops/seed-of)

(defn crop-cells
  "{pos crop} of the cells of a plan answer (of part, when given) that want a crop harvest knows."
  [answer part]
  (into {} (keep (fn [{:keys [pos want] :as cell}]
                   (when (and (map? want) (ripe-age (:crop want)) (or (nil? part) (= part (:part cell))))
                     [(u/cell-pos pos) (:crop want)])))
        (:cells answer)))

(defn ripe-at?
  "Whether p shows crop ripe at pos (nil, an unloaded cell, is not)."
  [p pos crop]
  (let [b (u/seen-block p pos)]
    (boolean (and b (= crop (.-name b)) (some-> (.-age b) (>= (ripe-age crop)))))))

(defn unwatched?
  "Whether a planned crop cell (cells {pos crop}) is remembered, not in view, and not known ripe: it may have ripened
  since the body last looked."
  [p cells]
  (boolean (and (some? (.-sensedAt p))
                (some (fn [[pos crop]]
                        (let [b (u/sensed p pos)]
                          (and b (false? (.-visible b)) (not (ripe-at? p pos crop)))))
                      cells))))

(defn planned-ripe
  "The planned cells (cells {pos crop}) ripe with the crop the plan wants there, of a wanted crop, not in skipped,
  nearest to the body first."
  [p args cells skipped]
  (let [crops (set (or (:crops args) (keys ripe-age)))
        skipped (set skipped)
        here (u/pos-of (.-pos (.self p)))]
    (->> cells
         (filter (fn [[pos crop]] (and (crops crop) (not (skipped pos)) (ripe-at? p pos crop))))
         (map key)
         (sort-by #(u/dist here %)))))

(defn planned-bare
  "Debts {:pos :seed} of the planned cells standing bare: air over farmland."
  [p cells]
  (keep (fn [[pos crop]]
          (when (and (= "air" (u/seen-name p pos)) (= "farmland" (u/seen-name p (update pos :y dec))))
            {:pos pos :seed (seed-of crop)}))
        cells))

(defn above-reserve
  "The debts of bare cells without those whose seed is carried but held back by the reserve register."
  [c debts]
  (let [carried (into {} (map (juxt :name :count)) (u/inventory (:primitives c)))
        left (volatile! (farm/sowable c))]
    (filterv (fn [{:keys [seed]}]
               (or (not (pos? (get carried seed 0)))
                   (when (pos? (get @left seed 0))
                     (vswap! left update seed dec)
                     true)))
             debts)))

(defn plan-field
  "[center radius] around cells {pos crop}: their mean and the farthest cell's distance from it, plus 2."
  [cells]
  (let [ps (keys cells)
        mean (fn [k] (js/Math.round (/ (reduce + (map k ps)) (count ps))))
        center {:x (mean :x) :y (mean :y) :z (mean :z)}]
    [center (+ 2 (js/Math.ceil (reduce max 0 (map #(u/dist center %) ps))))]))

(defn ripe-crops
  "Ripe crop cells of a wanted crop within :radius of center, not in skipped, nearest to the body first."
  [p args center skipped]
  (let [crops (or (:crops args) (keys ripe-age))
        skipped (set skipped)
        here (u/pos-of (.-pos (.self p)))
        radius (+ (:radius args) (u/dist here center))]
    (->> (crops/seen-crops p crops radius 4096)
         (filter #(some-> (:age %) (>= (ripe-age (:name %)))))
         (map :pos)
         (filter #(<= (u/dist % center) (:radius args)))
         (remove skipped))))

(defn nearest [p cells]
  (let [here (u/pos-of (.-pos (.self p)))]
    (first (sort-by #(u/dist here %) cells))))

(defn sync-debts
  "m with :replant made the debts of bare (those given up as :bare excepted), each keeping the counts of its old debt,
  followed by the debts of cells this job cut (:cut) that bare no longer names: what it cut it still owes, also when
  the plan dropped the cell meanwhile."
  [m bare]
  (let [old (into {} (map (juxt :pos identity)) (:replant m))
        given-up (set (:bare m))
        planned (set (map :pos bare))]
    (assoc m :replant (vec (concat (for [{:keys [pos] :as debt} bare :when (not (given-up pos))]
                                     (merge (dissoc (old pos) :seed) debt))
                                   (filter #(and (:cut %) (not (planned (:pos %)))) (:replant m)))))))

(defn below [pos] (update pos :y dec))

(defn drop-debt [m pos]
  (update m :replant (fn [debts] (vec (remove #(= pos (:pos %)) debts)))))

(defn bare-debt [m pos]
  (-> (drop-debt m pos)
      (update :bare (fn [cells] (vec (distinct (conj (vec cells) pos)))))))

(defn skip-crop [m pos]
  (-> (drop-debt m pos)
      (update :skipped (fnil conj []) pos)
      (update :unreachable (fnil inc 0))))

(defn sort-debts
  "{:owed [debts to plant] :gone [pos no longer to plant] :bare [pos with no farmland]} for the debts, read off p."
  [p debts]
  (reduce
   (fn [acc {:keys [pos] :as debt}]
     (let [here (u/seen-name p pos)]
       (cond
         (nil? here) (update acc :owed conj debt)
         (not= "air" here) (update acc :gone conj pos)
         (not= "farmland" (u/seen-name p (below pos))) (update acc :bare conj pos)
         :else (update acc :owed conj debt))))
   {:owed [] :gone [] :bare []}
   debts))
