(ns plan.rail-route-test
  (:require [clojure.test :refer [deftest is are]]
            [plan.rail :as rail]
            [plan.rail-route :as route]))

(defn flat [y] (fn [_ _] y))

(defn with-walls
  "A height fn: y everywhere except the columns in `walls` (no rail can run there)."
  [y walls]
  (fn [x z] (when-not (walls [x z]) y)))

(def prices {:cell 1 :dig (constantly 1) :fill (constantly 1) :dark 0})

(defn connect [height from to opts] (route/connect height from to (merge {:prices prices} opts)))

(defn laid? [waypoints] (nil? (:error (rail/layout waypoints {}))))

(deftest a-straight-run-over-flat-ground-is-one-leg
  (is (= {:waypoints [[0 64 0] [20 64 0]] :spans []} (connect (flat 64) [0 64 0] [20 64 0] {}))))

(deftest a-wall-across-the-way-is-gone-round-with-corners-and-the-result-lays
  (let [wall (set (for [z (range -6 7)] [10 z]))
        {:keys [waypoints error]} (connect (with-walls 64 wall) [0 64 0] [20 64 0] {})]
    (is (nil? error))
    (is (= [0 64 0] (first waypoints)))
    (is (= [20 64 0] (peek waypoints)))
    (is (< 2 (count waypoints)))
    (is (laid? waypoints))
    (is (not-any? (fn [[x _ z]] (wall [x z])) waypoints))))

(deftest a-hill-is-climbed-by-a-slope-of-one-per-cell
  (let [height (fn [x _] (cond (< x 10) 64 (<= x 12) (+ 64 (- x 9)) :else 67))
        {:keys [waypoints error]} (connect height [0 64 0] [30 67 0] {})]
    (is (nil? error))
    (is (= [[0 64 0] [9 64 0] [12 67 0] [30 67 0]] waypoints))))

(deftest ground-that-steps-more-than-one-gives-no-route
  (let [height (fn [x _] (if (< x 10) 64 70))]
    (is (= :no-route (:error (connect height [0 64 0] [20 70 0] {:max-nodes 2000}))))))

(def bump (fn [x _] (if (<= 10 x 12) 65 64)))
(def dip (fn [x _] (if (<= 10 x 12) 63 64)))
(def bumpy (fn [x _] (if (odd? (quot x 2)) 65 64)))

(deftest a-bump-is-cut-and-a-dip-filled-at-rail-height-when-cheaper-than-climbing-it
  (are [ground] (= {:waypoints [[0 64 0] [20 64 0]] :spans []} (connect ground [0 64 0] [20 64 0] {}))
    bump
    dip
    bumpy))

(deftest earthwork-dearer-than-the-way-round-is-gone-round
  (let [bank (fn [x z] (if (and (= x 10) (<= -1 z 1)) 66 64))
        {:keys [waypoints]} (connect bank [0 64 0] [40 64 0] {:prices (assoc prices :dig (constantly 100) :fill (constantly 100))})]
    (is (laid? waypoints))
    (is (not-any? (fn [[x _ z]] (and (= x 10) (<= -1 z 1))) (:cells (rail/route waypoints))))))

(deftest a-cell-that-may-not-be-dug-is-never-cut
  (let [{:keys [waypoints]} (connect bump [0 64 0] [20 64 0] {:prices (assoc prices :dig (fn [[x]] (when-not (= 11 x) 1)))})]
    (is (laid? waypoints))
    (is (= [11 65 0] (some (fn [[x _ z :as cell]] (when (= [11 0] [x z]) cell)) (:cells (rail/route waypoints)))))))

(def hill (fn [x _] (if (<= 10 x 19) 70 64)))
(def chasm (fn [x _] (when-not (<= 10 x 19) 64)))

(deftest a-hill-too-steep-to-climb-is-tunnelled-straight-through
  (is (= {:waypoints [[0 64 0] [30 64 0]] :spans [{:kind :tunnel :from [10 64 0] :to [19 64 0] :length 10}]}
         (connect hill [0 64 0] [30 64 0] {})))
  (is (= :no-route (:error (connect hill [0 64 0] [30 64 0] {:tunnel 9 :max-nodes 3000})))))

(deftest a-gap-is-bridged-straight-across-only-where-known-to-be-one-the-rail-clears
  (is (= {:waypoints [[0 64 0] [30 64 0]] :spans [{:kind :bridge :from [10 64 0] :to [19 64 0] :length 10}]}
         (connect chasm [0 64 0] [30 64 0] {:gap (constantly 64)})))
  (is (= :no-route (:error (connect chasm [0 64 0] [30 64 0] {:gap (constantly 65) :max-nodes 3000}))))
  (is (= :no-route (:error (connect chasm [0 64 0] [30 64 0] {:max-nodes 3000}))))
  (is (= [:bridge] (map :kind (:spans (connect (fn [x _] (if (<= 10 x 19) 60 64)) [0 64 0] [30 64 0] {}))))))

(deftest built-spans-are-the-straight-level-runs-of-one-kind-with-a-cell-to-stand-on
  (let [cells (vec (concat (for [x (range 0 10)] [x 64 0]) (for [x (range 10 14)] [x 65 0]) (for [x (range 14 20)] [x 65 0])))
        kind (fn [[x]] (cond (<= 3 x 4) :tunnel (<= 6 x 9) :bridge (<= 14 x 16) :tunnel))]
    (is (= [{:kind :bridge :heading [1 0] :stand [5 64 0] :cells [[6 64 0] [7 64 0] [8 64 0] [9 64 0]]}
            {:kind :tunnel :heading [1 0] :stand [13 65 0] :cells [[14 65 0] [15 65 0] [16 65 0]]}]
           (route/built-spans cells kind)))))
