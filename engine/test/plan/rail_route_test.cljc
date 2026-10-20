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
  (is (= {:waypoints [[0 64 0] [20 64 0]] :spans []} (route/connect (flat 64) [0 64 0] [20 64 0] {}))))

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

(def hill (fn [x _] (if (<= 10 x 19) 70 64)))
(def chasm (fn [x _] (when-not (<= 10 x 19) 64)))

(deftest a-hill-too-steep-to-climb-is-tunnelled-straight-through
  (is (= {:waypoints [[0 64 0] [30 64 0]] :spans [{:kind :tunnel :from [10 64 0] :to [19 64 0] :length 10}]}
         (route/connect hill [0 64 0] [30 64 0] {:tunnel 12})))
  (is (= {:error :no-route} (route/connect hill [0 64 0] [30 64 0] {:tunnel 9 :max-nodes 3000}))))

(deftest a-gap-is-bridged-straight-across-only-where-known-to-be-one
  (is (= {:waypoints [[0 64 0] [30 64 0]] :spans [{:kind :bridge :from [10 64 0] :to [19 64 0] :length 10}]}
         (route/connect chasm [0 64 0] [30 64 0] {:bridge 12 :gap? (constantly true)})))
  (is (= {:error :no-route} (route/connect chasm [0 64 0] [30 64 0] {:bridge 12 :gap? (constantly false) :max-nodes 3000})))
  (is (= [:bridge] (map :kind (:spans (route/connect (fn [x _] (if (<= 10 x 19) 60 64)) [0 64 0] [30 64 0] {:bridge 12}))))))

(deftest without-span-limits-the-route-has-no-spans
  (is (= [] (:spans (route/connect (flat 64) [0 64 0] [20 64 0] {})))))

(deftest built-spans-are-the-straight-level-runs-of-one-kind-with-a-cell-to-stand-on
  (let [cells (vec (concat (for [x (range 0 10)] [x 64 0]) (for [x (range 10 14)] [x 65 0]) (for [x (range 14 20)] [x 65 0])))
        kind (fn [[x]] (cond (<= 3 x 4) :tunnel (<= 6 x 9) :bridge (<= 14 x 16) :tunnel))]
    (is (= [{:kind :bridge :heading [1 0] :stand [5 64 0] :cells [[6 64 0] [7 64 0] [8 64 0] [9 64 0]]}
            {:kind :tunnel :heading [1 0] :stand [13 65 0] :cells [[14 65 0] [15 65 0] [16 65 0]]}]
           (route/built-spans cells kind)))))
