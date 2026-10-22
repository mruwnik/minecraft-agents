(ns jobs.build.from-plan.cells
  "Where the body can reach, stand and dig for jobs.build.from-plan: the cells of a plan read against the body's spot."
  (:require [engine.ctx :as ctx]
            [jobs.lib.tools :as tools]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.util :as u]
            [plan.shape :as shape]
            [jobs.lib.build.plan :as plan :refer [body-cells facing-ok? facing-of]]))

(defn pos-map [pos] (zipmap [:x :y :z] pos))

(defn diggable
  "The cells of digs not closed (given up or refused); a block no carried tool harvests is given up here as :no-tool."
  [c digs closed]
  (let [open (remove #(contains? closed (:pos %)) digs)
        {no-tool true ok false} (group-by #(tools/needs-tool-to-clear? (:primitives c) (:found %)) open)]
    (when (seq no-tool)
      (ctx/update-mem! c update :given-up merge (into {} (map (fn [cell] [(:pos cell) :no-tool])) no-tool)))
    (vec ok)))

(defn in-reach
  "The buildable cells the body can place from where it stands, lowest first, then nearest."
  [c todo]
  (let [body (u/self-pos c)
        mine (body-cells body)]
    (->> todo
         (filter #(and (<= (u/eye-dist body (:pos %)) (plan/reach))
                       (not (mine (:pos %)))
                       (or (:click %) (facing-ok? (facing-of (:want %)) (:pos %) body))))
         (sort-by (juxt #(get (:pos %) 1) #(u/eye-dist body (:pos %)))))))

(defn in-dig-reach
  "The cells to dig the body can reach from where it stands, lowest first, then nearest."
  [c cells]
  (let [body (u/self-pos c)
        mine (body-cells body)]
    (->> cells
         (filter #(and (<= (u/eye-dist body (:pos %)) (plan/reach)) (not (mine (:pos %)))))
         (sort-by (juxt #(get (:pos %) 1) #(u/eye-dist body (:pos %)))))))

(defn seals?
  "Whether placing at cell would shut the body in: it has room now and would have too little with the cell solid
  (jobs.lib.reach.room/closes-in?). Only a cell at the body's own height band (feet to a block above the head) can."
  [c cell]
  (let [p (:primitives c)
        by (js/Math.floor (:y (u/self-pos c)))
        cy (second cell)]
    (boolean (and (<= by cy (+ by 2))
                  (reach-room/closes-in? p cell)))))

(defn shuts-out?
  "Whether placing cell would wall in the floor under another cell still owed at its height or above, which can then only
  be placed from inside (the interior of a roof): the cell waits while anything else can be placed."
  [c cell owed]
  (let [y (get (:pos cell) 1)
        [cx _ cz] (:pos cell)
        close? #(and (>= (get (:pos %) 1) y) (not= (:pos %) (:pos cell)) (<= (js/Math.abs (- cx (nth (:pos %) 0))) 4) (<= (js/Math.abs (- cz (nth (:pos %) 2))) 4))
        above (vals (into {} (map (fn [o] [[(nth (:pos o) 0) (nth (:pos o) 2)] (:pos o)])) (filter close? owed)))]
    (boolean (some #(reach-room/walls-in? (:primitives c) % (:pos cell)) above))))

(defn exit-point
  "A cell two blocks outside the box of the planned cells, on the side nearest the body, at its feet height."
  [cells body]
  (let [xs (map #(nth (:pos %) 0) cells)
        zs (map #(nth (:pos %) 2) cells)
        y (js/Math.floor (:y body))
        [x0 x1 z0 z1] [(apply min xs) (apply max xs) (apply min zs) (apply max zs)]
        bx (js/Math.floor (:x body))
        bz (js/Math.floor (:z body))
        clamp (fn [v lo hi] (max lo (min hi v)))]
    (->> [[(- x0 2) y (clamp bz z0 z1)] [(+ x1 2) y (clamp bz z0 z1)]
          [(clamp bx x0 x1) y (- z0 2)] [(clamp bx x0 x1) y (+ z1 2)]]
         (sort-by (fn [[x _ z]] (+ (js/Math.abs (- x bx)) (js/Math.abs (- z bz)))))
         first)))

(defn off-low-floor
  "The nearest cell within 2 that the body can stand in on a full-height floor, when it stands on a seen bed or bottom slab (half
  a block high, its head inside the next cell up: the roof cells over it are refused, and the walks keep out of its cell, so
  none leaves it); nil elsewhere."
  [c]
  (let [p (:primitives c)
        {:keys [x y z]} (u/self-pos c)
        [fx fy fz] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]
        low? #(u/low-floor? (u/seen-block p {:x %1 :y %2 :z %3}))]
    (when (low? fx fy fz)
      (->> (for [dx (range -2 3) dz (range -2 3)
                 :let [cell [(+ fx dx) fy (+ fz dz)]]
                 :when (and (not= [dx dz] [0 0])
                            (reach/standable-cell? p (zipmap [:x :y :z] cell))
                            (not (low? (first cell) fy (peek cell)))
                            (not (low? (first cell) (dec fy) (peek cell))))]
             cell)
           (sort-by (fn [[cx _ cz]] (+ (js/Math.abs (- cx fx)) (js/Math.abs (- cz fz)))))
           first))))

(defn buried?
  "Whether every neighbour of cell [x y z] is a seen solid block: the cell is inside the ground, so it is never seen
  (as opposed to unloaded)."
  [p [x y z]]
  (every? (fn [[dx dy dz]]
            (let [n (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})]
              (and n (not (shape/air? n)) (not (#{"water" "lava"} n)))))
          [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))

(defn reaching
  "The stands from which the body reaches cell pos wherever it stands in the stand cell (its eye within :reach, less the
  half diagonal of a cell, from the cell's centre)."
  [c stands pos]
  (filter (fn [[x y z]] (<= (u/eye-dist {:x (+ x 0.5) :y y :z (+ z 0.5)} pos) (- (plan/reach) 0.71))) stands))

(defn support-looks
  "[cell neighbour] pairs: a cell refused :no-support with a neighbour never seen and not yet looked at."
  [c cells]
  (let [done (set (:support-looked (ctx/mem c)))
        refused (set (keys (filter (comp #{:no-support} val) (:unplaceable (ctx/mem c)))))]
    (vec (for [cell cells :when (refused (:pos cell))
               n (plan/unseen-support c (:pos cell)) :when (not (done n))]
           [cell n]))))

(defn rock-bound?
  "Whether rock holds cell in on every side that is not another cell of the plan: no air beside it to stand in or see it
  from (a face not loaded yet counts as open: it will load)."
  [p cell planned]
  (let [[x y z] (:pos cell)
        solid? (fn [n] (let [b (u/sensed p (pos-map n))]
                         (and (some? b) (or (true? (.-unknown b)) (not (or (bs/passable? (.-name b)) (bs/fluids (.-name b))))))))]
    (every? #(or (planned %) (solid? %))
            [[(inc x) y z] [(dec x) y z] [x (inc y) z] [x (dec y) z] [x y (inc z)] [x y (dec z)]])))
