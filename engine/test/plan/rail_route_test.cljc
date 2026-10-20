(ns plan.rail-route-test
  (:require [clojure.test :refer [deftest is are]]
            [plan.rail :as rail]
            [plan.rail-route :as route]))

(defn flat [y] (fn [_ _] y))

(defn with-walls
  "A height fn: y everywhere except the columns in `walls` (no rail can run there)."
  [y walls]
  (fn [x z] (when-not (walls [x z]) y)))

(defn laid? [waypoints] (nil? (:error (rail/layout waypoints {}))))

(deftest a-straight-run-over-flat-ground-is-one-leg
  (is (= {:waypoints [[0 64 0] [20 64 0]]} (route/connect (flat 64) [0 64 0] [20 64 0] {}))))

(deftest a-wall-across-the-way-is-gone-round-with-corners-and-the-result-lays
  (let [wall (set (for [z (range -6 7)] [10 z]))
        {:keys [waypoints error]} (route/connect (with-walls 64 wall) [0 64 0] [20 64 0] {})]
    (is (nil? error))
    (is (= [0 64 0] (first waypoints)))
    (is (= [20 64 0] (peek waypoints)))
    (is (< 2 (count waypoints)))
    (is (laid? waypoints))
    (is (not-any? (fn [[x _ z]] (wall [x z])) waypoints))))

(deftest a-hill-is-climbed-by-a-slope-of-one-per-cell
  (let [height (fn [x _] (cond (< x 10) 64 (<= x 12) (+ 64 (- x 9)) :else 67))
        {:keys [waypoints error]} (route/connect height [0 64 0] [30 67 0] {})]
    (is (nil? error))
    (is (= [[0 64 0] [9 64 0] [12 67 0] [30 67 0]] waypoints))))

(deftest ground-that-steps-more-than-one-gives-no-route
  (let [height (fn [x _] (if (< x 10) 64 70))]
    (is (= {:error :no-route} (route/connect height [0 64 0] [20 70 0] {:max-nodes 2000})))))
