(ns jobs.lib.farm
  "Helpers the farm jobs share: which cells are bare, which seeds may be sown, and the hoe and bone meal a body carries."
  (:require [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.util :as u]))

(defn bare-cells
  "The bare cells of box (farmland with air above, ground layer y = min y) not in skipped, in x then z order."
  [p box skipped]
  (let [skipped (set skipped)
        {:keys [min max]} box
        y (:y min)]
    (->> (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))] {:x x :y y :z z})
         (remove skipped)
         (filter #(and (= "farmland" (u/seen-name p %))
                       (= "air" (u/seen-name p (update % :y inc))))))))

(defn sowable
  "{name count} of the carried items that may be sown: food crops (carrots, potatoes) only above the food reserve,
  everything else in full. A cell just harvested is replanted by jobs.farm.harvest and ignores the reserve."
  [inventory]
  (let [kept (cost/food-reserve inventory)
        totals (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} inventory)]
    (reduce-kv (fn [acc name n] (assoc acc name (- n (get kept name 0)))) {} totals)))

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

(defn hoe-of
  "The name of a carried item ending in _hoe, or nil."
  [p]
  (some #(when (.endsWith (:name %) "_hoe") (:name %)) (u/inventory p)))

(defn has-meal? [p]
  (some #(= "bone_meal" (:name %)) (u/inventory p)))
