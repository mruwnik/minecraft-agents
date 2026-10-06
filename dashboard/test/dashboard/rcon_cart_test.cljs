(ns dashboard.rcon-cart-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.rcon-cart :as cart]))

(defn at-speed
  "Samples one tick apart moving along +x: speeds is the per-tick step before each sample after the first."
  [speeds]
  (vec (map-indexed (fn [i x] {:t i :pos [x 64 0.5]})
                    (reductions + 0.5 speeds))))

(deftest parse-replies
  (are [f reply expected] (= expected (f reply))
    cart/parse-triple "Steve has the following entity data: [12.5d, 64.0d, -3.25d]" [12.5 64.0 -3.25]
    cart/parse-triple "x has the following entity data: [0.0d, -0.0784d, 1.0E-4d]" [0.0 -0.0784 1.0e-4]
    cart/parse-gametime "The game time is 12345 tick(s)" 12345)
  (is (thrown-with-msg? js/Error #"No entity" (cart/parse-triple "No entity was found")))
  (is (thrown-with-msg? js/Error #"time" (cart/parse-gametime "nope"))))

(deftest uuid-from-int-array
  (is (= "a1b2c3d4-0000-4000-8000-000000000001"
         (cart/int-array->uuid "x has the following entity data: [I; -1582119980, 16384, -2147483648, 1]"))))

(deftest windowed-speed-is-distance-over-ticks
  (let [samples (at-speed (concat (repeat 12 0.4) (repeat 12 0.1)))
        w (cart/windowed-speeds samples 10)]
    (is (every? #(< (js/Math.abs (- 0.4 (:speed %))) 1e-9) (take 3 w)))
    (is (= 10 (:ticks (first w))))
    (is (< (js/Math.abs (- 0.1 (:speed (last w)))) 1e-9))
    (is (= 15 (count w)))))

(deftest windowed-speed-uses-game-ticks-not-sample-count
  (let [samples [{:t 0 :pos [0 0 0]} {:t 5 :pos [2 0 0]} {:t 10 :pos [4 0 0]}]]
    (is (= [0.4] (map :speed (cart/windowed-speeds samples 10))))))

(deftest dedupe-by-tick
  (is (= [0 1 2] (map :t (cart/dedupe-ticks [{:t 0} {:t 0} {:t 1} {:t 1} {:t 2}])))))

(deftest minimum-speed-and-where
  (let [samples (at-speed (concat (repeat 15 0.4) (repeat 10 0.05) (repeat 15 0.4)))
        {:keys [speed pos t]} (cart/min-window samples 10)]
    (is (< (js/Math.abs (- 0.05 speed)) 1e-9))
    (is (<= 25 t 26))
    (is (= 64 (second pos)))))

(deftest per-cell-speeds
  (let [samples (at-speed (repeat 30 0.5))
        cells (cart/cell-speeds (cart/windowed-speeds samples 10))]
    (is (= [5 0] (:cell (first cells))))
    (is (= 1 (:n (first cells))))
    (is (every? #(< (js/Math.abs (- 0.5 (:min %))) 1e-9) cells))))

(deftest recovery-distance-from-a-cell
  (let [;; fast, a slow stretch of 10 ticks, then fast again
        samples (at-speed (concat (repeat 12 0.4) (repeat 10 0.05) (repeat 20 0.4)))
        w (cart/windowed-speeds samples 10)
        dip (first (filter #(< (:speed %) 0.3) w))
        cell (cart/cell-of (:pos dip))
        {:keys [distance ticks]} (cart/recovery w cell 0.3)]
    (is (< 3.6 distance 3.8))
    (is (= 18 ticks))
    (is (nil? (:distance (cart/recovery w cell 5.0))))
    (is (nil? (cart/recovery w [999 999] 0.3)))))

(deftest recovery-is-zero-when-already-fast
  (let [w (cart/windowed-speeds (at-speed (repeat 30 0.5)) 10)]
    (is (= 0.0 (:distance (cart/recovery w [12 0] 0.3))))))

(deftest stopped-detection
  (let [samples (at-speed (concat (repeat 15 0.4) (repeat 25 0.0)))]
    (is (not (cart/stopped? (subvec samples 0 20) 10)))
    (is (cart/stopped? samples 10))))

(deftest parse-args
  (are [argv expected] (= expected (select-keys (cart/parse-args (clj->js argv)) (keys expected)))
    ["--rider" "Wren" "--secs" "12"] {:select {:rider "Wren"} :secs 12}
    ["--uuid" "a1b2c3d4-0000-4000-8000-000000000001" "--until-stop"] {:select {:uuid "a1b2c3d4-0000-4000-8000-000000000001"} :until-stop true}
    ["--near" "10" "64" "-5.5"] {:select {:near [10 64 -5.5]}}
    ["--rider" "Wren" "--cell" "3" "-4" "--threshold" "0.25"] {:cell [3 -4] :threshold 0.25})
  (is (thrown-with-msg? js/Error #"exactly one" (cart/parse-args #js [])))
  (is (thrown-with-msg? js/Error #"name" (cart/parse-args #js ["--rider" "a b; op"])))
  (is (thrown-with-msg? js/Error #"unknown" (cart/parse-args #js ["--bogus"]))))

(deftest windowed-speed-is-path-length-round-a-corner
  ;; 0.4 b/tick, 5 ticks east then 5 ticks south: the 10-tick displacement is 0.4*5*sqrt2/10 = 0.28, the path is 0.40
  (let [east (map (fn [i] [(* 0.4 i) 64 0.5]) (range 6))
        south (map (fn [i] [2.0 64 (+ 0.5 (* 0.4 i))]) (range 1 6))
        samples (vec (map-indexed (fn [t p] {:t t :pos p}) (concat east south)))
        w (cart/windowed-speeds samples 10)]
    (is (< 0.27 (/ (cart/distance (:pos (last samples)) (:pos (first samples))) 10) 0.29) "displacement per tick is the artifact")
    (is (< (js/Math.abs (- 0.4 (:speed (last w)))) 1e-9))))
