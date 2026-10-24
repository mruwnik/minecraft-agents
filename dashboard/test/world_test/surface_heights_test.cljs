(ns world-test.surface-heights-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.surface-heights :as w]))

;; ground of stone up to y 69 at x 0, up to y 69 under a tree and grass at x 1, nothing recorded at x 2, y 4 at x 3
(defn at [x y z]
  (case x
    0 {:name (if (> y 69) "air" "stone")}
    1 {:name (cond (> y 76) "air" (> y 72) "oak_leaves" (> y 70) "oak_log" (= y 70) "short_grass" :else "stone")}
    3 {:name (if (> y 3) "air" "stone")}
    4 {:name (cond (> y 62) "air" (> y 50) "water" :else "stone")}
    nil))

(deftest a-column-counts-at-one-over-its-ground-not-its-trees-or-grass
  (is (= 70 (w/stand-y at 0 0)))
  (is (= 70 (w/stand-y at 1 0)) "leaves, logs and grass above the ground are no ground")
  (is (nil? (w/stand-y at 2 0)) "an unrecorded column"))

(deftest a-column-under-water-has-no-stand
  (is (nil? (w/stand-y at 4 0)) "a stair exit cannot stand on the sea")
  (is (= {70 1} (w/tally at [[0 0] [4 0]]))))

(deftest heights-are-binned-with-the-middle-of-the-bin-as-key
  (is (= {6 1 70 3} (w/tally at [[0 0] [1 0] [2 0] [3 0] [0 1]]))))
