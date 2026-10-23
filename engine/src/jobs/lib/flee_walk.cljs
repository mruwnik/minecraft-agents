(ns jobs.lib.flee-walk
  "Where a fleeing body can walk along a direction: the open cells, what is wet or roofed, and the way back up the
  stairs it cut (pure, over a block-at fn). Where a flight walks to is jobs.lib.flee-search's."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.breath :as breath]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-descent {:default 3 :doc "Blocks lower than where a flight began that it may end a step under rock (a cave, not a hillside): an end further down is not taken (jobs.survival.respond.cover/flight-target!)." :spec (a/num-in 0 nil)}
   ::roof-reach {:default 8 :doc "Blocks above the head searched for rock over a cell." :spec (a/int-in 0 nil)}})
(defn roof-reach [] (settings/get settings ::roof-reach))
(defn max-descent [] (settings/get settings ::max-descent))

(defn passable?
  "Whether a block name lets the body walk through (water is swum); an unloaded cell (nil) counts as open."
  [block-name]
  (or (nil? block-name) (bs/passable-or-water? block-name)))

(defn floorless?
  "Whether a block name under a walker's feet is no floor: a loaded passable
  block other than water (which a walker swims on). An unloaded cell (nil) is not."
  [block-name]
  (and (some? block-name) (passable? block-name) (not= "water" block-name)))

(defn unit
  "[ux uz] for the vector (dx dz), or nil when it is zero."
  [dx dz]
  (let [n (js/Math.hypot dx dz)]
    (when (pos? n) [(/ dx n) (/ dz n)])))

(defn rotate [[ux uz] degrees]
  (let [a (* degrees (/ js/Math.PI 180))
        c (js/Math.cos a)
        s (js/Math.sin a)]
    [(- (* ux c) (* uz s)) (+ (* ux s) (* uz c))]))

(defn column-along
  "The [x z] column k blocks along [ux uz] from the centre of from's cell (a
  body at x 58.5 stands in column 58 and probes from 58.5, not 59)."
  [from [ux uz] k]
  [(js/Math.floor (+ (js/Math.floor (:x from)) 0.5 (* ux k)))
   (js/Math.floor (+ (js/Math.floor (:z from)) 0.5 (* uz k)))])

(defn free-at?
  "Whether feet and head cells at feet height y in column x z are passable."
  [block-at x y z]
  (and (passable? (block-at {:x x :y y :z z})) (passable? (block-at {:x x :y (inc y) :z z}))))

(defn drop-to
  "The feet height of a jump down a ledge of two or three blocks from feet height y in column x z (it costs no health), or
  nil: the first floor within three blocks must be a loaded solid block that does not harm."
  [block-at x y z]
  (some (fn [d] (let [floor (block-at {:x x :y (- y d 1) :z z})
                      shaft (map #(block-at {:x x :y (- y %) :z z}) (range 1 (inc d)))]
                  (when (and (some? floor) (not (passable? floor)) (not (contains? bs/bad-floor floor))
                             (every? passable? shaft) (not-any? bs/harms-in-cell shaft))
                    (- y d))))
        [2 3]))

(defn next-y
  "The feet height a walker at feet height y in column [px pz] reaches in the
  next column [x z], or nil when it cannot enter it: the same height, one
  lower when the floor there is open and the cell under it is not (a step
  down, never into water over the head: a swimmer keeps to the surface; a column with no floor, a drop of two or more, is not entered), else one higher when that is free and there is headroom above the
  column it steps from (a step up)."
  [block-at [px pz] [x z] y]
  (cond
    (free-at? block-at x y z)
    (let [below (block-at {:x x :y (dec y) :z z})
          below2 (block-at {:x x :y (- y 2) :z z})]
      (cond
        (and (passable? below) (not (passable? below2)) (not= "water" (block-at {:x x :y y :z z}))) (dec y)
        (floorless? below) (drop-to block-at x y z)
        :else y))
    (and (free-at? block-at x (inc y) z) (passable? (block-at {:x px :y (+ y 2) :z pz}))) (inc y)
    :else nil))

(defn corner-shut?
  "Whether a diagonal step from column [px pz] to [x z] at feet height y passes between two blocked columns: a body
  0.6 wide cannot squeeze through that corner."
  [block-at [px pz] [x z] y]
  (and (not= px x) (not= pz z)
       (not (free-at? block-at px y z))
       (not (free-at? block-at x y pz))))

(defn walk-cells
  "The feet cells {:x :y :z}, one per block along [ux uz] from from, up to n,
  that a walker passes before the first column it cannot enter, per block-at
  (a cell -> block name or nil). A column met twice (a diagonal) repeats its cell;
  a diagonal step between two blocked columns (corner-shut?) is not taken."
  [block-at from dir n]
  (loop [k 1
         prev [(js/Math.floor (:x from)) (js/Math.floor (:z from))]
         y (js/Math.floor (:y from))
         out []]
    (let [col (column-along from dir k)
          ny (when (<= k n)
               (cond
                 (= col prev) y
                 (corner-shut? block-at prev col y) nil
                 :else (next-y block-at prev col y)))]
      (if (nil? ny)
        out
        (recur (inc k) col ny (conj out {:x (first col) :y ny :z (second col)}))))))

(defn open-cells
  "How many blocks along [ux uz] from from, up to n, a walker gets before the
  first obstacle (see walk-cells)."
  [block-at from dir n]
  (count (walk-cells block-at from dir n)))

(defn wet?
  "Whether a walker in feet cell swims there: water (or kelp, seagrass, a bubble column: breath/swimmable) at its feet,
  or as its floor (it sinks into it), or a waterlogged? block (cell -> bool: a block name says nothing of it) in either."
  ([block-at cell] (wet? block-at (constantly false) cell))
  ([block-at waterlogged? {:keys [x y z]}]
   (let [feet {:x x :y y :z z} floor {:x x :y (dec y) :z z}]
     (or (contains? breath/swimmable (block-at feet)) (contains? breath/swimmable (block-at floor))
         (boolean (waterlogged? feet)) (boolean (waterlogged? floor))))))

(defn roofed?
  "Whether a block that is not passable lies within roof-reach above the head of feet cell end."
  [block-at {:keys [x y z]}]
  (some #(not (passable? (block-at {:x x :y (+ y 2 %) :z z}))) (range (roof-reach))))

(defn way-target
  "The feet cell, a map, of the highest floor of floors ([x y z], the stairs this body cut) within reach blocks and
  above the body whose centre is no nearer any threat than the body and whose way does not run past one (a stair's
  height makes its top look far from a mob that stands in its mouth): the way back up. nil when none."
  [from threats floors reach]
  (let [near-threat (fn [pos] (apply min (map #(u/dist pos %) threats)))
        now (near-threat from)
        blocked? (fn [centre]
                   (let [dx (- (:x centre) (:x from)) dz (- (:z centre) (:z from))
                         len2 (+ (* dx dx) (* dz dz))]
                     (some (fn [t] (let [tx (- (:x t) (:x from)) tz (- (:z t) (:z from))
                                         along (/ (+ (* tx dx) (* tz dz)) (max len2 1e-6))]
                                     (and (pos? along)
                                          (< (js/Math.hypot (- tx (* (min along 1) dx)) (- tz (* (min along 1) dz))) 1.5))))
                           threats)))
        cells (for [[x y z] floors
                    :let [cell {:x x :y (inc y) :z z} centre {:x (+ x 0.5) :y (inc y) :z (+ z 0.5)}]
                    :when (and (> (inc y) (+ (:y from) 0.5)) (<= (u/dist from centre) reach) (>= (near-threat centre) now) (not (blocked? centre)))]
                [cell (u/dist from centre)])]
    (some->> (seq cells) (sort-by (juxt (comp - :y first) second)) ffirst)))
