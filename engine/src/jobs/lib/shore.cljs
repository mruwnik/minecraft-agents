(ns jobs.lib.shore
  "Where a boat can land the body, read only from what the body has seen: a stand cell (feet and head air over a solid, non-fluid,
  non-hazard block) with water at the level of that block beside it and air over the water."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.blocks :as b]
            [jobs.lib.breath :as breath]
            [jobs.lib.land :as land]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(a/defargs settings
  {::swim-cells {:default 6000 :doc "Cells the flood from a body in water reads for the nearest dry stand (swim-to-land); a shore not found by then is at least as far as the flood reached." :spec (a/int-in 1 nil)}
   ::land-cells {:default 1500 :doc "Cells read for the land a body can walk on from the stand it swims to (land-run)." :spec (a/int-in 1 nil)}})

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(defn seen-air? [p cell] (contains? bs/air (u/seen-name p cell)))

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

(defn same-way?
  "cell lies within 45 degrees of a failed target as seen from the offset (dx, dz): a swim toward that way already
  failed (a wall), so its neighbours would too."
  [dx dz failed fx fz]
  (some (fn [t]
          (let [tx (- (:x t) fx) tz (- (:z t) fz)
                dot (+ (* dx tx) (* dz tz))]
            (and (pos? dot)
                 (>= (* dot dot) (* 0.5 (+ (* dx dx) (* dz dz)) (+ (* tx tx) (* tz tz)))))))
        failed))

(defn nearest-land
  "The nearest land cell within radius sideways of self-pos, feet y from one below to two above, that keep? accepts;
  nil if none. A failed target (a seq of cells) and the cells in its direction are skipped."
  ([p self-pos radius] (nearest-land p self-pos radius nil (constantly true)))
  ([p self-pos radius failed keep?]
   (let [fx (js/Math.floor (:x self-pos))
         fy (js/Math.floor (:y self-pos))
         fz (js/Math.floor (:z self-pos))]
     (->> (for [[dx dz] (breath/columns radius)
                :when (not (same-way? dx dz failed fx fz))
                dy [0 -1 1 2]]
            {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)})
          (remove (set failed))
          (filter #(and (standable? p %) (keep? p %)))
          first))))

(defn reachable-land
  "The nearest dry stand cell joined to the body by seen water and air cells (a flooded stair or shaft, however far up),
  within radius sideways and radius above the feet; nil if none. nearest-land only reads columns near the feet's height."
  ([p self-pos radius failed] (reachable-land p self-pos radius failed (constantly true)))
  ([p self-pos radius failed keep?]
   (let [skip (set failed)]
     (breath/find-joined p self-pos radius radius
                         (fn [cell] (and (not (contains? skip cell)) (bs/air (u/seen-name p cell)) (standable? p cell) (keep? p cell)))))))

(def ^:private flat-moves
  "The moves of a swim: the six neighbours and the four flat diagonals (a diagonal needs both cells beside it open), each with its length."
  (concat (map (fn [[dx dy dz]] [dx dy dz 1]) [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])
          (map (fn [[dx dz]] [dx 0 dz js/Math.SQRT2]) [[1 1] [1 -1] [-1 1] [-1 -1]])))

(defn- cell-key [x y z] (+ (* (+ (* (+ x 32768) 65536) (+ z 32768)) 512) (+ y 64)))

(defn swim-to-land
  "{:stand cell :swim blocks} of the nearest dry stand joined to the body by seen water and air cells (a flooded stair or
  shaft, however far up), the swim the length of that way (flat diagonals count root 2), nearest by steps first. A body
  swims up through water but not through air: it leaves an air cell sideways or down only, and an air cell is a cell of the
  way only over water (or a stand). The flood reads at most
  ::swim-cells cells; with no stand found by then {:swim blocks :open? true}: the shore is at least that far. nil when the
  joined water ends with no stand in it."
  [p self-pos]
  (let [work (settings/get settings ::swim-cells)
        fx (js/Math.floor (:x self-pos)) fy (js/Math.floor (:y self-pos)) fz (js/Math.floor (:z self-pos))
        names (js/Map.)
        name-of (fn [x y z]
                  (let [k (cell-key x y z)]
                    (if (.has names k)
                      (.get names k)
                      (let [n (breath/seen-water-name p {:x x :y y :z z})] (.set names k n) n))))
        water? (fn [x y z] (contains? breath/swimmable (name-of x y z)))
        open? (fn [x y z]
                (and (>= y (dec fy))
                     (let [n (name-of x y z)]
                       (if (bs/air n)
                         (or (water? x (dec y) z) (standable? p {:x x :y y :z z}))
                         (breath/passable-water-or-air? n)))))
        visited (js/Set. #js [(cell-key fx fy fz)])]
    (loop [layer [[fx fy fz 0]]]
      (let [stands (keep (fn [[x y z g]] (when (and (bs/air (name-of x y z)) (standable? p {:x x :y y :z z})) [g {:x x :y y :z z}])) layer)]
        (cond
          (seq stands) (let [[g stand] (apply min-key first stands)] {:stand stand :swim g})
          (empty? layer) nil
          (<= work (.-size visited)) {:swim (reduce min ##Inf (map #(nth % 3) layer)) :open? true}
          :else
          (recur (vec (for [[x y z g] layer
                            :let [in-air? (bs/air (name-of x y z))]
                            [dx dy dz len] flat-moves
                            :when (not (and in-air? (pos? dy)))
                            :let [nx (+ x dx) ny (+ y dy) nz (+ z dz) k (cell-key nx ny nz)]
                            :when (and (not (.has visited k))
                                       (open? nx ny nz)
                                       (or (zero? dx) (zero? dz) (and (open? nx y z) (open? x y nz))))
                            :let [_ (.add visited k)]]
                        [nx ny nz (+ g len)]))))))))

(defn land-run
  "Blocks from stand, at most limit, the body can walk away over dry stand cells (a step up or down at a time); ##Inf when
  ::land-cells cells were read and it still went on (that much land is room enough). A ledge with no land beyond it is 0."
  [p stand limit]
  (let [work (settings/get settings ::land-cells)
        {sx :x sy :y sz :z} stand
        visited (js/Set. #js [(cell-key sx sy sz)])]
    (loop [layer [stand] far 0]
      (if (or (empty? layer) (>= far limit) (<= work (.-size visited)))
        (if (<= work (.-size visited)) ##Inf (min far limit))
        (let [next (vec (for [{:keys [x y z]} layer [dx dz] sides dy [0 1 -1]
                              :let [c {:x (+ x dx) :y (+ y dy) :z (+ z dz)} k (cell-key (:x c) (:y c) (:z c))]
                              :when (and (not (.has visited k)) (standable? p c))
                              :let [_ (.add visited k)]]
                          c))]
          (recur next (reduce max far (map #(js/Math.hypot (- (:x %) sx) (- (:z %) sz)) next))))))))
