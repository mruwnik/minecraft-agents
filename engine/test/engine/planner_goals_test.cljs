(ns engine.planner-goals-test
  "engine.path.planner.goals: the indexed goal set answers as a scan of every goal does."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.planner.base :refer [OCTILE-SLACK SQRT2]]
            [engine.path.planner.goals :as goals]))

(defn- rng [seed]
  (let [s (atom seed)]
    (fn [n] (swap! s #(mod (+ (* % 1103515245) 12345) 2147483648)) (mod (quot @s 65536) n))))

(defn- goal-set [rand]
  (let [spread (nth [3 40 300] (rand 3))
        ranges [0 0 0 1 2.5 4.5 20]]
    (vec (for [_ (range (inc (rand (nth [4 60 1500] (rand 3)))))]
           {:x (- (rand spread) (quot spread 2)) :y (+ 60 (rand 8)) :z (- (rand spread) (quot spread 2))
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

(deftest the-indexed-goal-set-answers-as-a-scan-of-every-goal
  (let [rand (rng 7)]
    (dotimes [_ 30]
      (let [gs (goal-set rand)
            g (indexed gs)]
        (dotimes [_ 40]
          (let [far (nth [4 40 2000] (rand 3))
                x (- (rand (* 2 far)) far)
                y (+ 56 (rand 16))
                z (- (rand (* 2 far)) far)
                [x z] (if (zero? (rand 3)) (let [{gx :x gz :z} (nth gs (rand (count gs)))] [(+ gx (rand 5) -2) (+ gz (rand 5) -2)]) [x z])]
            (is (= (scan-goal-at gs x y z) (goals/goal-at g x y z)) "the first goal holding the cell")
            (doseq [mode [0 1 2]]
              (is (= (scan-least gs mode x y z) (goals/least g mode x y z)) "the same least: the heuristic neither looser nor above it"))))))))
