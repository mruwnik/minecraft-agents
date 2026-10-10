(ns jobs.lib.dig-route
  "The fewest cells to dig for a walkable way from one feet cell to another through stone: a pure search over what the
  caller says each cell is (block-at cell -> name, or nil when never seen)."
  (:require [jobs.lib.reach :as reach]))

(def air #{"air" "cave_air" "void_air"})
(def fluids #{"water" "lava" "bubble_column"})
(def unbreakable #{"bedrock" "barrier" "end_portal_frame" "end_portal" "nether_portal" "command_block" "structure_block" "jigsaw"})
(def falling #{"sand" "red_sand" "gravel"})

(defn open? [n] (boolean (and n (or (air n) (re-find reach/arrow-passes n)))))

(defn cell-cost
  "Digs it takes to open a cell: 0 when open, 1 for a block (an unseen cell is dug to see), nil when it cannot be
  passed (a fluid, a hazard, bedrock, a falling block)."
  [n]
  (cond
    (open? n) 0
    (nil? n) 1
    (or (fluids n) (reach/hazard-blocks n) (unbreakable n) (falling n)) nil
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
  {:dig [cell ..]} or {:problem :no-route}."
  [block-at from goal {:keys [max-digs margin max-fall] :or {max-digs 16 margin 3 max-fall 3}}]
  (let [lo (mapv #(- (min %1 %2) margin) from goal)
        hi (mapv #(+ (max %1 %2) margin) from goal)
        inside? (fn [c] (every? true? (map #(<= %1 %2 %3) lo c hi)))
        step (fn [m n]
               (when (floor-ok? (block-at [(n 0) (dec (n 1)) (n 2)]))
                 (let [cs (map (comp cell-cost block-at) (step-cells m n))]
                   (when (every? some? cs) (reduce + cs)))))]
    (loop [dist {from 0} prev {} done #{}]
      (let [[cur d] (->> dist (remove (fn [[k _]] (done k))) (sort-by val) first)]
        (cond
          (nil? cur) {:problem :no-route}
          (= cur goal)
          (let [path (reverse (take-while some? (iterate prev goal)))]
            {:dig (vec (distinct (for [[m n] (map vector path (rest path))
                                       c (sort-by #(- (% 1)) (step-cells m n))
                                       :when (pos? (cell-cost (block-at c)))]
                                   c)))})
          (> d max-digs) {:problem :no-route}
          :else
          (let [relax (for [n (neighbours cur max-fall) :when (and (inside? n) (not (done n)))
                            :let [s (step cur n)] :when s] [n (+ d s)])
                better (filter (fn [[n nd]] (< nd (get dist n js/Infinity))) relax)]
            (recur (into dist better) (into prev (map (fn [[n _]] [n cur]) better)) (conj done cur))))))))
