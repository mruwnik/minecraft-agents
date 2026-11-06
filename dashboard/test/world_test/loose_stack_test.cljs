(ns world-test.loose-stack-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.loose-stack :as w]))

(defn world
  "A column at 0,0: stone to y 0, gravel 1-3, sand 4, air above; a column at 1,0 with one gravel under water."
  [x y z]
  (cond (= [x z] [1 0]) (case y 1 "gravel" 2 "water" nil)
        (= [x z] [0 0]) (cond (<= y 0) "stone" (<= y 3) "gravel" (= y 4) "sand" :else "air")
        :else nil))

(deftest pairs-count-by-the-run-of-loose-cells-under-the-cell
  (is (= {:h1 {:all 1 :loose 1} :h2 {:all 1 :loose 1} :h3 {:all 1 :loose 1}} (w/tally world 0 0 0 0))
      "gravel 1 under gravel, 2 under gravel, 3 under sand; the sand under air is no pair")
  (is (= {} (w/tally world 1 1 0 0)) "a loose cell under water is no pair"))

(deftest the-prior-pools-columns-that-agree
  (is (= [2 2] (:prior (:h1 (w/report [{:h1 {:all 2 :loose 1}} {:h1 {:all 2 :loose 1}}] 1)))) "no spread: pooled"))
