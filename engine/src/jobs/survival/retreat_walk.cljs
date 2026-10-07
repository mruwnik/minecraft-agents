(ns jobs.survival.retreat-walk
  "Where a fleeing body can walk: the open cells along a direction and the walk target away from the threats (pure,
  over a block-at fn)."
  (:require [jobs.lib.util :as u]))

(def hazard-clearance 2.5)

(def turns
  "Angles to try, in degrees from the preferred direction, best first."
  [0 30 -30 60 -60 90 -90 120 -120])

(def min-open
  "Fewest clear cells a direction needs to be worth walking."
  2)

(def passable-names
  #{"air" "cave_air" "void_air" "short_grass" "tall_grass" "grass" "fern" "large_fern" "dead_bush" "snow"
    "water" "dandelion" "poppy" "torch" "sweet_berry_bush" "vine"})

(defn passable?
  "Whether a block name lets the body walk through; an unloaded or unseen cell (nil) counts as open."
  [block-name]
  (or (nil? block-name) (contains? passable-names block-name)
      (some #(.endsWith block-name %) ["_sapling" "_flower" "_carpet" "_tulip" "_orchid" "_button" "_pressure_plate"])))

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

(defn direction
  "The preferred unit [ux uz] from from: directly away from the threats (positions), each weighing 1/distance so the
  nearer pushes harder (+x when they cancel out), blended with the direction of home when that lies on the away side
  (not through the threats)."
  [from threats home]
  (let [push (fn [t] (let [d (max 1 (js/Math.hypot (- (:x from) (:x t)) (- (:z from) (:z t))))
                           [ux uz] (or (unit (- (:x from) (:x t)) (- (:z from) (:z t))) [0 0])]
                       [(/ ux d) (/ uz d)]))
        sum (reduce (fn [[ax az] t] (let [[ux uz] (push t)] [(+ ax ux) (+ az uz)])) [0 0] threats)
        away (or (unit (first sum) (second sum)) [1 0])
        to-home (when home (unit (- (:x home) (:x from)) (- (:z home) (:z from))))
        along (when to-home (+ (* (first away) (first to-home)) (* (second away) (second to-home))))]
    (if (and along (pos? along))
      (or (unit (+ (first away) (first to-home)) (+ (second away) (second to-home))) away)
      away)))

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

(defn point-along
  "The cell step blocks from from along [ux uz], same height."
  [from dir step]
  (let [[x z] (column-along from dir step)]
    {:x x :y (:y from) :z z}))

(defn near-hazard?
  "Whether the walk from from to target passes within clearance of a hazard
  (checked at the middle and the end)."
  [hazards from target]
  (let [mid {:x (/ (+ (:x from) (:x target)) 2) :y (:y from) :z (/ (+ (:z from) (:z target)) 2)}]
    (boolean (some #(or (< (u/dist % target) hazard-clearance) (< (u/dist % mid) hazard-clearance)) hazards))))

(defn free-at?
  "Whether feet and head cells at feet height y in column x z are passable."
  [block-at x y z]
  (and (passable? (block-at {:x x :y y :z z})) (passable? (block-at {:x x :y (inc y) :z z}))))

(defn next-y
  "The feet height a walker at feet height y in column [px pz] reaches in the
  next column [x z], or nil when it cannot enter it: the same height, one
  lower when the floor there is open and the cell under it is not (a step
  down; a column with no floor, a drop of two or more, is not entered), else one higher when that is free and there is headroom above the
  column it steps from (a step up)."
  [block-at [px pz] [x z] y]
  (cond
    (free-at? block-at x y z)
    (let [below (block-at {:x x :y (dec y) :z z})
          below2 (block-at {:x x :y (- y 2) :z z})]
      (cond
        (and (passable? below) (not (passable? below2))) (dec y)
        (floorless? below) nil
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

(defn worth?
  "Whether walking the open blocks along a direction from from, to the cell
  end, is worth it against threat: it must not end closer, and must either be
  a real walk (min-open + 1 blocks) or gain at least 2 blocks of distance. A
  short side step in a dead end is neither, so a body that only has those
  left is cornered."
  [from threats {:keys [open end]}]
  (and (>= open min-open)
       (let [now (apply min (map #(u/dist from %) threats))
             then (apply min (map #(u/dist end %) threats))]
         (and (>= then now)
              (or (> open min-open) (>= (- then now) 2))))))

(defn choose-target
  "The walk target, a feet cell: the end of the first direction, turning away
  from the preferred one as needed, that avoids every hazard and is open for a
  full step; else the most open one that is still worth walking (see
  worth?). nil when cornered."
  [block-at from threats home hazards step]
  (let [dir (direction from threats home)
        options (->> turns
                     (map #(rotate dir %))
                     (map (fn [d] (let [cells (walk-cells block-at from d step)]
                                    {:open (count cells) :dir d :end (peek cells)})))
                     (remove #(near-hazard? hazards from (point-along from (:dir %) step)))
                     (filter #(worth? from threats %)))
        pick (or (first (filter #(>= (:open %) step) options))
                 (last (sort-by :open options)))]
    (:end pick)))
