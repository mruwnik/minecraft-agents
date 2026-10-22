(ns jobs.lib.field-area
  "The field a farm job works, its :area arg (till, plant, harvest, fertilize, tend, maintain): a box {:min :max} (corners
  inclusive, any order, each [x y z] or {:x :y :z}; the ground layer is the lowest y), a circle {:pos :radius} (a sphere
  for harvest and fertilize, the square |dx|,|dz| <= radius at pos's y for till and plant), {:cells [pos ...]}, or
  {:plan id} with :part, or :parts for tend and maintain (the plan's crop cells). Each job lists the forms it takes."
  (:require [engine.args :as a]
            [jobs.lib.util :as u]))

(def spec
  (a/or-of (a/map-with {:min a/position? :max a/position?})
           (a/map-with {:pos a/position? :radius (a/num-in 0 nil)})
           (a/map-with {:cells (a/coll-of a/position?)})
           (a/map-with {:plan a/name?})))

(defn kind
  "The form of an area: :plan, :cells, :circle or :box; nil for no area."
  [area]
  (cond (:plan area) :plan (:cells area) :cells (:pos area) :circle (:min area) :box))

(defn of [c] (:area (:args c)))
(defn plan [c] (:plan (of c)))
(defn part [c] (:part (of c)))
(defn parts [c] (:parts (of c)))

(defn with-part
  "c working one part of its plan."
  [c part]
  (assoc-in c [:args :area :part] part))

(defn box
  "{:min :max} of a box area, ordered, as cells; a circle as its square at the centre's y; nil for other forms."
  [area]
  (case (kind area)
    :box (let [lo (a/cell (:min area)) hi (a/cell (:max area))]
           (when (and lo hi) {:min (merge-with min lo hi) :max (merge-with max lo hi)}))
    :circle (let [{:keys [x y z]} (a/cell (:pos area)) r (:radius area)]
              (when (and x (number? r))
                {:min {:x (- x r) :y y :z (- z r)} :max {:x (+ x r) :y y :z (+ z r)}}))
    nil))

(defn arg-box [c] (box (of c)))

(defn cell-count
  "How many cells a box covers."
  [{lo :min hi :max}]
  (apply * (map #(inc (- (% hi) (% lo))) [:x :y :z])))

(defn centre
  "The box centre rounded to cells."
  [{lo :min hi :max}]
  (into {} (map (fn [k] [k (js/Math.round (/ (+ (k lo) (k hi)) 2))])) [:x :y :z]))

(defn cover-radius
  "How far a sphere around mid must reach to cover the box: the farthest corner, rounded up, plus 1."
  [{lo :min hi :max} mid]
  (inc (js/Math.ceil (apply js/Math.max (for [x [(:x lo) (:x hi)] y [(:y lo) (:y hi)] z [(:z lo) (:z hi)]]
                                          (u/dist mid {:x x :y y :z z}))))))

(defn sphere
  "[center radius] of a box (the sphere covering it) or circle area; nil for other forms."
  [area]
  (case (kind area)
    :circle (when (box area) [(a/cell (:pos area)) (:radius area)])
    :box (when-let [b (box area)] (let [mid (centre b)] [mid (cover-radius b mid)]))
    nil))

(defn contains-pos?
  "Whether a crop cell pos lies in a box (its ground layer is the lowest y, the crops stand one above: a one-layer box holds them too), circle (within radius of the centre) or cells area."
  [area pos]
  (case (kind area)
    :box (let [{lo :min hi :max} (box area)]
           (and (<= (:x lo) (:x pos) (:x hi)) (<= (:y lo) (:y pos) (max (:y hi) (inc (:y lo)))) (<= (:z lo) (:z pos) (:z hi))))
    :circle (<= (u/dist (a/cell (:pos area)) pos) (:radius area))
    :cells (let [p (a/cell pos)] (boolean (some #(= (a/cell %) p) (:cells area))))
    false))
