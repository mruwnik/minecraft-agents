(ns jobs.lib.dig-route
  "The fewest cells to dig for a walkable way from one feet cell to another through stone: a pure search over what the
  caller says each cell is (block-at cell -> name, or nil when never seen)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.reach :as reach]))

(a/defargs settings
  {::max-digs {:default 16 :doc "The most cells a route digs." :spec (a/int-in 0 nil)}
   ::margin {:default 3 :doc "How far (blocks) round the two ends a route may stray." :spec (a/int-in 0 nil)}
   ::max-fall {:default 3 :doc "The longest fall (blocks) a route's step may drop." :spec (a/int-in 0 nil)}})

(def air #{"air" "cave_air" "void_air"})
(def fluids #{"water" "lava" "bubble_column"})
(def falling #{"sand" "red_sand" "gravel"})

(defn open? [n] (boolean (and n (or (air n) (re-find reach/arrow-passes n)))))

(defn cell-cost
  "Digs it takes to open a cell: 0 when open, 1 for a block (an unseen cell is dug to see), nil when it cannot be
  passed (a fluid, a hazard, bedrock, a falling block)."
  [n]
  (cond
    (open? n) 0
    (nil? n) 1
    (or (fluids n) (reach/hazard-blocks n) (rules/unbreakable n) (falling n)) nil
    :else 1))

(defn floor-ok? [n] (boolean (and n (not (open? n)) (not (fluids n)) (not (reach/hazard-blocks n)))))

(defn neighbours [[x y z] max-fall]
  (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] dy (cons 1 (map - (range max-fall -1 -1)))] [(+ x dx) (+ y dy) (+ z dz)]))

(defn step-cells
  "The cells that must be open to walk from feet cell m to the adjacent feet cell n (a fall included)."
  [[_ my _ :as m] [nx ny nz :as n]]
  (cond
    (> ny my) [[(m 0) (+ my 2) (m 2)] n [nx (inc ny) nz]]
    (< ny my) (mapv (fn [y] [nx y nz]) (range (inc my) (dec ny) -1))
    :else [n [nx (inc ny) nz]]))

(defn route
  "The cells to dig (head cell first within a step, in walking order) of the cheapest route from feet cell from to feet
  cell goal, at most :max-digs of them, within :margin cells of both ends, falls of at most :max-fall; each step's floor must be a seen solid block.
  A* with the walked steps' price as the lower bound. Priced :step-s a step and (:dig-s name) a cell dug (default 0 and 1: the fewest digs); a block :dig-s prices nil is
  not dug; nor is a cell under sand or gravel (it would fall on the body walking in). {:dig [cell ..]} or {:problem :no-route}."
  [block-at from goal {:keys [dig-s step-s] :or {dig-s (constantly 1) step-s 0} :as opts}]
  (let [block-at (memoize block-at)
        dig-s (memoize dig-s)
        max-digs (get opts :max-digs (settings/get settings ::max-digs))
        margin (get opts :margin (settings/get settings ::margin))
        max-fall (get opts :max-fall (settings/get settings ::max-fall))
        lo (mapv #(- (min %1 %2) margin) from goal)
        hi (mapv #(+ (max %1 %2) margin) from goal)
        heuristic (fn [[x _ z]] (* step-s (+ (js/Math.abs (- x (goal 0))) (js/Math.abs (- z (goal 2))))))
        inside? (fn [c] (every? true? (map #(<= %1 %2 %3) lo c hi)))
        step (fn [m n]
               (when (floor-ok? (block-at [(n 0) (dec (n 1)) (n 2)]))
                 (let [cells (step-cells m n)
                       names (map block-at cells)
                       roofed? (fn [cell name] (and (pos? (or (cell-cost name) 1)) (falling (block-at (update cell 1 inc)))))
                       digs (map #(if (roofed? %1 %2) nil (cell-cost %2)) cells names)
                       prices (map (fn [n d] (if (pos? d) (dig-s n) 0)) names digs)]
                   (when (every? some? (concat digs prices))
                     [(+ step-s (reduce + prices)) (reduce + digs)]))))]
    (loop [dist {from 0} queue (sorted-set [(heuristic from) 0 from]) tick 1 digs {from 0} prev {} done #{}]
      (let [[_ _ cur :as top] (first queue)
            d (dist cur)]
        (cond
          (nil? top) {:problem :no-route}
          (done cur) (recur dist (disj queue top) tick digs prev done)
          (= cur goal)
          (let [path (reverse (take-while some? (iterate prev goal)))]
            {:dig (vec (distinct (for [[m n] (map vector path (rest path))
                                       c (sort-by #(- (% 1)) (step-cells m n))
                                       :when (pos? (cell-cost (block-at c)))]
                                   c)))})
          :else
          (let [relax (for [n (neighbours cur max-fall) :when (and (inside? n) (not (done n)))
                            :let [[s k] (step cur n)] :when (and s (<= (+ (digs cur) k) max-digs))]
                        [n (+ d s) (+ (digs cur) k)])
                better (filter (fn [[n nd]] (< nd (get dist n js/Infinity))) relax)]
            (recur (into dist (map (fn [[n nd _]] [n nd])) better)
                   (into (disj queue top) (map-indexed (fn [i [n nd _]] [(+ nd (heuristic n)) (+ tick i) n])) better)
                   (+ tick (count better))
                   (into digs (map (fn [[n _ k]] [n k])) better)
                   (into prev (map (fn [[n _]] [n cur])) better)
                   (conj done cur))))))))
