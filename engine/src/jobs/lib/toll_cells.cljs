(ns jobs.lib.toll-cells
  "The cells a job's walk would rather not cross, as go-to's :tolls (jobs.lib.cost farm-tolls and zone-tolls price them):
  the crops and farmland the body has seen near the walk, and the cells of zones that are not the body's. Only what the
  body knows (memory of what it saw, the zone list) is tolled. The planner looks a toll up per node in a map, so the lists
  are bounded here, not there."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.world :as world]))

(def farm-names (conj (vec (keys crops/ripe-age)) "farmland"))

(def margin "Blocks round the walk's ends and between them that get tolls." 8)

(def max-cells "The most cells one list holds." 8000)

(defn farm-cells
  "The feet cells [x y z] over the crops and farmland the body has seen within radius of it: a crop's own cell, and for
  farmland its own cell (a body walks on it with its feet in the block) and the one above."
  [p radius]
  (->> (look/seen-blocks p {:names farm-names :radius (min 64 radius) :max 4096 :live? true})
       (mapcat (fn [{:keys [name pos]}]
                 (let [cell [(:x pos) (:y pos) (:z pos)]]
                   (if (= "farmland" name) [cell (update cell 1 inc)] [cell]))))
       distinct
       vec))

(defn window
  "{:min [x y z] :max [x y z]} of the feet cells a walk from a to b ({:x :y :z}) may touch."
  [a b]
  (let [lo (fn [k] (js/Math.floor (min (k a) (k b))))
        hi (fn [k] (js/Math.floor (max (k a) (k b))))]
    {:min [(- (lo :x) margin) (- (lo :y) 2) (- (lo :z) margin)]
     :max [(+ (hi :x) margin) (+ (hi :y) 3) (+ (hi :z) margin)]}))

(defn box-in-window
  "The cells of zone (a box {:min :max}) inside the window, at most max-cells."
  [zone {[x0 y0 z0] :min [x1 y1 z1] :max}]
  (let [[zx0 zy0 zz0] (:min zone) [zx1 zy1 zz1] (:max zone)]
    (when (and (<= (max x0 zx0) (min x1 zx1)) (<= (max y0 zy0) (min y1 zy1)) (<= (max z0 zz0) (min z1 zz1)))
      (vec (take max-cells (for [x (range (max x0 zx0) (inc (min x1 zx1)))
                                 y (range (max y0 zy0) (inc (min y1 zy1)))
                                 z (range (max z0 zz0) (inc (min z1 zz1)))]
                             [x y z]))))))

(defn zone-cells
  "The feet cells inside the window of a walk from a to b that lie in a zone owned by another body (none without a zone list)."
  [c a b]
  (let [self (ctx/self-name c)
        win (window a b)]
    (->> (world/zones c)
         (filter #(zones/foreign-owner? (:owner %) self))
         (mapcat #(box-in-window % win))
         (take max-cells)
         vec)))

(defn walk-tolls
  "go-to's :tolls for a walk from the body to pos ({:x :y :z}): the farm cells it has seen near the walk and, unless the job
  ignores zones, the cells of other bodies' zones. nil when there are none."
  [c pos]
  (when pos
   (let [from (u/self-pos c)
        radius (+ margin (js/Math.ceil (u/dist from pos)))
        farm (farm-cells (:primitives c) radius)
        zone (when-not (:ignore-zones? (:args c)) (zone-cells c from pos))
        tolls (into (cost/farm-tolls farm) (cost/zone-tolls zone))]
    (not-empty tolls))))
