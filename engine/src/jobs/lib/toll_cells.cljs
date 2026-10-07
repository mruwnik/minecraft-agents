(ns jobs.lib.toll-cells
  "The cells a job's walk would rather not cross, as go-to's :tolls (jobs.lib.cost farm-tolls and zone-tolls price them):
  the crops and farmland the body has seen near the walk, and the cells of zones that are not the body's (zone-walk-tolls, for walks that ignore crops). Only what the
  body knows (memory of what it saw, the zone list) is tolled. The planner looks a toll up per node in a map, so the lists
  are bounded here, not there."
  (:require [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.world :as world]))

(def settings
  {::margin {:default 8 :doc "Blocks round the walk's ends and between them that get tolls." :type :int :min 0}
   ::max-cells {:default 8000 :doc "The most cells one toll list holds (the farm and the zone list each)." :type :int :min 1}})

(def farm-names (conj (vec (keys crops/ripe-age)) "farmland"))

(defn margin [] (settings/get settings ::margin))

(defn max-cells [] (settings/get settings ::max-cells))

(defn farm-cells
  "The feet cells [x y z] over the crops and farmland the body has seen within radius of it: a crop's own cell, and for
  farmland its own cell (a body walks on it with its feet in the block) and the one above (so at most max-cells)."
  [p radius]
  (->> (look/seen-blocks p {:names farm-names :radius (min 64 radius) :max (quot (max-cells) 2) :live? true})
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
    {:min [(- (lo :x) (margin)) (- (lo :y) 2) (- (lo :z) (margin))]
     :max [(+ (hi :x) (margin)) (+ (hi :y) 3) (+ (hi :z) (margin))]}))

(defn clip
  "{:min [x y z] :max [x y z]} of zone (a box) inside the window, nil when they do not meet."
  [zone {[x0 y0 z0] :min [x1 y1 z1] :max}]
  (let [[zx0 zy0 zz0] (:min zone) [zx1 zy1 zz1] (:max zone)
        lo [(max x0 zx0) (max y0 zy0) (max z0 zz0)]
        hi [(min x1 zx1) (min y1 zy1) (min z1 zz1)]]
    (when (every? true? (map <= lo hi))
      {:min lo :max hi})))

(defn volume [{[x0 y0 z0] :min [x1 y1 z1] :max}]
  (* (inc (- x1 x0)) (inc (- y1 y0)) (inc (- z1 z0))))

(defn box-cells
  "Every cell [x y z] of box ({:min :max})."
  [{[x0 y0 z0] :min [x1 y1 z1] :max}]
  (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [x y z]))

(defn segment-distance
  "Distance in x and z from the point (px, pz) to the segment from a to b ({:x :z})."
  [a b px pz]
  (let [ax (:x a) az (:z a) dx (- (:x b) ax) dz (- (:z b) az)
        len2 (+ (* dx dx) (* dz dz))
        t (if (zero? len2) 0 (max 0 (min 1 (/ (+ (* (- px ax) dx) (* (- pz az) dz)) len2))))]
    (js/Math.hypot (- px (+ ax (* t dx))) (- pz (+ az (* t dz))))))

(defn nearest-columns
  "At most max-cells cells of boxes, whole columns (all y of one x, z) nearest the straight line from a to b first."
  [boxes a b]
  (let [columns (for [{[x0 y0 z0] :min [x1 y1 z1] :max} boxes
                      x (range x0 (inc x1)) z (range z0 (inc z1))]
                  [(segment-distance a b (+ x 0.5) (+ z 0.5)) x z y0 y1])]
    (loop [[[_ x z y0 y1] & more] (sort-by first columns) left (max-cells) out []]
      (if (or (nil? x) (< left (inc (- y1 y0))))
        out
        (recur more (- left (inc (- y1 y0))) (into out (map (fn [y] [x y z])) (range y0 (inc y1))))))))

(defn zone-cells
  "The feet cells inside the window of a walk from a to b that lie in a zone owned by another body (none without a zone
  list). Over max-cells the ones nearest the straight line from a to b are kept (a warn, once)."
  [c a b]
  (let [self (ctx/self-name c)
        win (window a b)
        boxes (->> (world/zones c)
                   (filter #(zones/foreign-owner? (:owner %) self))
                   (keep #(clip % win)))]
    (if (<= (reduce + (map volume boxes)) (max-cells))
      (vec (mapcat box-cells boxes))
      (do (ctx/warn-once! c :zone-cells :toll-cells.capped
                          {:text (str "other bodies' zones cover more than " (max-cells) " cells near the walk; only the ones nearest the way are tolled")})
          (nearest-columns boxes a b)))))

(defn zone-walk-tolls
  "go-to's :tolls for a walk from the body to pos ({:x :y :z}): the cells of other bodies' zones, none when the job
  ignores zones. nil when there are none."
  [c pos]
  (when (and pos (not (:ignore-zones? (:args c))))
    (not-empty (cost/zone-tolls (zone-cells c (u/self-pos c) pos)))))

(defn walk-tolls
  "go-to's :tolls for a walk from the body to pos ({:x :y :z}): the farm cells it has seen near the walk and, unless the job
  ignores zones, the cells of other bodies' zones. nil when there are none."
  [c pos]
  (when pos
    (let [from (u/self-pos c)
          radius (+ (margin) (js/Math.ceil (u/dist from pos)))
          tolls (into (cost/farm-tolls (farm-cells (:primitives c) radius)) (zone-walk-tolls c pos))]
      (not-empty tolls))))
