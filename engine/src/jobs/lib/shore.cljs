(ns jobs.lib.shore
  "Where a boat can land the body, read only from what the body has seen: a stand cell (feet and head air over a solid, non-fluid,
  non-hazard block) with water at the level of that block beside it and air over the water."
  (:require [jobs.lib.blocks :as b]
            [jobs.lib.land :as land]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(defn seen-air? [p cell] (contains? b/air (u/seen-name p cell)))

(defn standable?
  "Whether the cell is a seen stand cell: feet and head air, a seen block below that is solid and no hazard."
  [p cell]
  (land/land-cell? #(u/seen-name p %) cell))

(defn water-beside
  "A seen water cell with seen air over it level with the support of land cell (beside it), or nil."
  [p {:keys [x y z]}]
  (some (fn [[dx dz]]
          (let [w {:x (+ x dx) :y (dec y) :z (+ z dz)}]
            (when (and (= "water" (u/seen-name p w)) (seen-air? p (update w :y inc))) w)))
        sides))

(defn spot-at
  "{:land :water} for a given land cell, or nil when it is no seen shore."
  [p land]
  (when (standable? p land)
    (when-let [w (water-beside p land)] {:land land :water w})))

(defn spots
  "The shore spots {:land :water} within :radius of from, nearest to from first. Water is looked for among the seen blocks."
  [p from {:keys [radius max] :or {radius 12 max 128}}]
  (->> (look/seen-blocks p {:names ["water"] :radius radius :max max :live? true})
       (filter #(seen-air? p (update (:pos %) :y inc)))
       (mapcat (fn [{{:keys [x y z]} :pos}]
                 (for [[dx dz] sides
                       :let [land {:x (+ x dx) :y (inc y) :z (+ z dz)}]
                       :when (standable? p land)]
                   {:land land :water {:x x :y y :z z}})))
       (sort-by #(u/dist from (vehicle/centre (:land %))))))
