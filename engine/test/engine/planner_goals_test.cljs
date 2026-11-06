(ns engine.planner-goals-test
  "engine.path.planner.goals: the indexed goal set answers as a scan of every goal does."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.planner.base :refer [OCTILE-SLACK SQRT2]]
            [engine.path.planner.goals :as goals]))

(defn- rng [seed]
  (let [s (atom seed)]
    (fn [n] (swap! s #(mod (+ (* % 1103515245) 12345) 2147483648)) (mod (quot @s 65536) n))))

(defn- goal-set
  "Square, row, column and two-far-apart-clusters shapes, so the grid's thin cases are met."
  [rand]
  (let [spread (nth [3 40 300] (rand 3))
        [sx sz] (nth [[spread spread] [spread 1] [1 spread] [spread spread]] (rand 4))
        centers (nth [[[0 0]] [[0 0]] [[0 0]] [[-2000 -1500] [2500 1800]]] (rand 4))
        ranges [0 0 0 1 2.5 4.5 20]]
    (vec (for [_ (range (inc (rand (nth [4 60 1500] (rand 3)))))
               :let [[cx cz] (nth centers (rand (count centers)))]]
           {:x (+ cx (- (rand sx) (quot sx 2))) :y (+ 60 (rand 8)) :z (+ cz (- (rand sz) (quot sz 2)))
            :r (nth ranges (rand (count ranges))) :near (rand 2)}))))

(defn- indexed [gs]
  (let [arr (fn [ctor f] (.from ctor (to-array (map f gs))))]
    (goals/index (count gs) (arr js/Int32Array :x) (arr js/Int32Array :y) (arr js/Int32Array :z)
                 (arr js/Float64Array #(* (:r %) (:r %))) (arr js/Float64Array #(* OCTILE-SLACK (:r %))) (arr js/Uint8Array :near))))

(defn- scan-goal-at [gs x y z]
  (or (first (keep-indexed (fn [i {gx :x gy :y gz :z r :r near :near}]
                             (let [dy (if (= 1 near) (- y gy) 0)]
                               (when (<= (+ (* (- x gx) (- x gx)) (* dy dy) (* (- z gz) (- z gz))) (* r r)) i)))
                           gs))
      -1))

(defn- scan-least [gs mode x y z]
  (reduce min ##Inf
          (for [{gx :x gy :y gz :z r :r near :near} gs]
            (let [h (goals/octile (- x gx) (- z gz))
                  dy (max 0 (- (abs (- y gy)) (js/Math.sqrt (* r r))))]
              (case mode
                0 (max 0 (- h (* OCTILE-SLACK r)))
                1 h
                2 (if (= 1 near) (+ (max h dy) (* (- SQRT2 1) (min h dy))) h))))))

(defn- random-query [rand _gs]
  (let [far (nth [4 40 2000] (rand 3))]
    [(- (rand (* 2 far)) far) (- (rand (* 2 far)) far)]))

(defn- query-near-a-goal [rand gs]
  (let [{gx :x gz :z} (nth gs (rand (count gs)))]
    [(+ gx (rand 5) -2) (+ gz (rand 5) -2)]))

(deftest the-indexed-goal-set-answers-as-a-scan-of-every-goal
  (let [rand (rng 7)]
    (dotimes [_ 60]
      (let [gs (goal-set rand)
            g (indexed gs)]
        (doseq [query (take 40 (cycle [query-near-a-goal random-query random-query]))]
          (let [[x z] (query rand gs)
                y (+ 56 (rand 16))]
            (is (= (scan-goal-at gs x y z) (goals/goal-at g x y z)) "the first goal holding the cell")
            (doseq [mode [0 1 2]]
              (is (= (scan-least gs mode x y z) (goals/least g mode x y z)) "the same least: the heuristic neither looser nor above it"))))))))
