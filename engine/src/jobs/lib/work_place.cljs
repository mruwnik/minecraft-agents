(ns jobs.lib.work-place
  "Where a farm or build job works, for its safe? export (ready/safe-at): the cell at the middle of the place its args
  name, nil (here) when they name none."
  (:require [engine.args :as a]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(defn middle
  "The cell at the middle of the positions ([x y z] vectors), nil for none."
  [ps]
  (when (seq ps)
    (let [[lo hi] [(apply map min ps) (apply map max ps)]
          [x y z] (map #(js/Math.floor (/ (+ %1 %2) 2)) lo hi)]
      {:x x :y y :z z})))

(defn dist2 [[x y z] {x' :x y' :y z' :z}]
  (+ (* (- x x') (- x x')) (* (- y y') (- y y')) (* (- z z') (- z z'))))

(defn plan-spot
  "The cell of the plan :plan (of :part, when given) nearest the middle of its cells (a long plan's middle may be no
  cell of it), nil when it is not readable."
  [{:keys [plan part]} c]
  (when plan
    (let [answer (known/plan c plan)]
      (when-not (known/unreadable? answer)
        (let [ps (keep #(when (or (nil? part) (= part (:part %))) (:pos %)) (:cells answer))
              mid (middle ps)]
          (when mid (let [[x y z] (apply min-key #(dist2 % mid) ps)] {:x x :y y :z z})))))))

(defn corner-vec [p] (let [{:keys [x y z]} (a/cell p)] [x y z]))

(defn area-spot
  "The cell at the middle of a field :area (a box, circle, cells or plan), nil for none."
  [area c]
  (cond
    (:plan area) (plan-spot area c)
    (:cells area) (middle (map corner-vec (:cells area)))
    (:pos area) (a/cell (:pos area))
    (:min area) (middle (map corner-vec [(:min area) (:max area)]))))

(defn spot
  "The place of c's args: :at or :pos, else the middle of :area, :plan, of :box, of :from to :to, else :center."
  [c]
  (let [{:keys [at pos box from to center area] :as args} (:args c)]
    (or (a/cell (or at pos))
        (area-spot area c)
        (plan-spot args c)
        (when box (middle (map corner-vec [(:min box) (:max box)])))
        (when (and from to) (middle (map corner-vec [from to])))
        (a/cell center)
        (when (:x center) (select-keys center [:x :z])))))

(defn nearest-mob
  "The cell of the nearest mob the body sees within radius, of the names given (a coll) or, with none, any passive one; nil for none."
  [c names radius]
  (let [p (:primitives c)
        feet (u/pos-of (.-pos (.self p)))
        opts (cond-> {:radius radius :max 16} (seq names) (assoc :names (vec names)) (empty? names) (assoc :kind "passive"))]
    (some->> (look/seen-objs p opts)
             (map #(u/pos-of (.-pos %)))
             (sort-by #(u/dist feet %))
             first
             (#(update-vals % js/Math.floor)))))

(defn nearest-block
  "The cell of the nearest seen live block of name, nil for none."
  [c block radius]
  (when block
    (some-> (look/seen-blocks (:primitives c) {:names [block] :radius radius :max 1 :live? true}) first :pos (update-vals js/Math.floor))))

(defn dig-spot
  "Where a dig works: the place of c's args (spot), else the nearest seen block of :block within :radius."
  [c]
  (or (spot c) (nearest-block c (:block (:args c)) (or (:radius (:args c)) 16))))

(defn ahead
  "The cell one block along :heading from the tunnel's :origin or the body's cell, nil without a heading."
  [c]
  (let [{:keys [heading origin]} (:args c)
        [dx dz] (get {"north" [0 -1] "south" [0 1] "east" [1 0] "west" [-1 0]} heading)
        {:keys [x y z]} (or (a/cell origin) (sh-sense/feet (:primitives c)))]
    (when dx {:x (+ x dx) :y y :z (+ z dz)})))
