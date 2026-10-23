(ns world-test.water-lags-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.water-lags :as w]))

;; a pond 3 wide along x (x 0..2, z 0) at y 62 in stone, air above it; columns past x 6 never recorded
(defn at [x y z]
  (when (<= -10 x 6)
    {:name (cond (> y 62) "air" (and (= y 62) (= z 0) (<= 0 x 2)) "water" :else "stone")}))

(deftest each-lag-counts-the-cells-that-far-from-surface-water-and-the-water-among-them
  (let [lags (w/tally at [[0 0] [5 0]])]
    (is (= {:n 4 :k 1} (get lags 1)) "from x 0: x 1 water, x -1 and z +-1 stone; x 5 is no water")
    (is (= {:n 4 :k 1} (get lags 2)))
    (is (= {:n 3 :k 0} (get lags 7)) "x 7 was never recorded")))
