(ns dashboard.tiles-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.tiles :as t]))

(deftest hex-to-rgb
  (are [hex expected] (= expected (t/rgb hex))
    "#000000" [0 0 0]
    "#ff8000" [255 128 0]
    "#5c8f4a" [92 143 74]))

(deftest height-light-rises-with-height
  (are [y expected] (< (js/Math.abs (- expected (t/height-light y))) 0.001)
    -64 0.78
    50 0.78
    105 0.94
    160 1.10
    320 1.10))

(deftest slope-light-follows-the-north-neighbour
  (are [y north expected] (< (js/Math.abs (- expected (t/slope-light y north))) 0.001)
    70 70 1
    71 70 1.06
    75 70 1.24
    69 70 0.94
    60 70 0.76))

(deftest land-cell-colour
  (are [cell expected] (= expected (t/cell-rgb cell))
    ;; flat ground at y 105: light 0.94
    {:name "grass_block" :y 105 :north-y 105 :depth 0} [89 145 65]
    {:name "stone" :y 160 :north-y 160 :depth 0} [134 140 150]))

(deftest water-cell-colour-darkens-with-depth
  (let [lum (fn [depth] (apply + (t/cell-rgb {:name "water" :y 62 :north-y 62 :depth depth :floor-name "sand"})))]
    (are [shallow deep] (> (lum shallow) (lum deep))
      1 5
      5 12
      12 40)))

(deftest water-over-sand-is-bluer-than-the-sand
  (let [[r _ b] (t/cell-rgb {:name "water" :y 62 :north-y 62 :depth 2 :floor-name "sand"})
        [sr _ sb] (t/cell-rgb {:name "sand" :y 60 :north-y 60 :depth 0})]
    (are [a b*] (< a b*)
      r sr
      sb b)))

(def column
  {:palette ["grass_block" "water" "sand"]
   :top (js/Uint16Array. (clj->js (concat [0 1] (repeat 254 0))))
   :floor (js/Uint16Array. (clj->js (concat [0 2] (repeat 254 0))))
   :y (js/Int16Array. (clj->js (repeat 256 64)))
   :depth (js/Uint8Array. (clj->js (concat [0 3] (repeat 254 0))))})

(deftest tile-rgba-shape
  (let [rgba (t/tile-rgba column)]
    (are [expected actual] (= expected actual)
      1024 (.-length rgba)
      255 (aget rgba 3)
      255 (aget rgba 7)
      true (> (aget rgba 6) (aget rgba 4)))))

(deftest tile-file-names
  (are [file expected] (= expected (t/parse-column-file file))
    "1.-6.bin" [1 -6]
    "-12.340.bin" [-12 340]
    "0.0.bin" [0 0]
    "x.bin" nil
    "1.2.bin.tmp" nil
    "1.2.png" nil))

(deftest tile-index-entries
  (is (= [[1 -6 1500] [0 0 20]]
         (t/index-entries [["1.-6.bin" 1500.7] ["junk" 1] ["0.0.bin" 20]]))))

(deftest since-filter
  (are [since expected] (= expected (t/newer-than [[1 2 100] [3 4 200]] since))
    nil [[1 2 100] [3 4 200]]
    0 [[1 2 100] [3 4 200]]
    100 [[3 4 200]]
    200 []))

(deftest lru-evicts-the-least-recently-used
  (are [cap ops expected] (= expected (t/lru-keys (reduce (fn [m [op k]] (case op :put (t/lru-put! m cap k 1) :get (do (t/lru-get! m k) m)))
                                                         (t/lru) ops)))
    2 [[:put "a"] [:put "b"] [:put "c"]] ["b" "c"]
    2 [[:put "a"] [:put "b"] [:get "a"] [:put "c"]] ["a" "c"]
    3 [[:put "a"] [:put "a"] [:put "b"]] ["a" "b"]
    1 [[:put "a"] [:put "b"]] ["b"]))

(deftest lru-values
  (let [m (t/lru)]
    (t/lru-put! m 2 "a" {:mtime 5})
    (are [k expected] (= expected (t/lru-get! m k))
      "a" {:mtime 5}
      "b" nil)))
