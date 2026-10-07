(ns view.stats-test
  "Pixel statistics (ported from test/view-stats.test.mjs)."
  (:require [clojure.test :refer [deftest are is]]
            [view.stats :as stats]))

(defn image [w h pixel]
  (let [rgba (js/Uint8Array. (* w h 4))]
    (doseq [y (range h) x (range w) [c v] (map-indexed vector (pixel x y))]
      (aset rgba (+ (* 4 (+ (* y w) x)) c) v))
    rgba))

(def flat (image 4 4 (fn [_ _] [10 20 30 255])))
(def halves (image 4 4 (fn [x _] (if (< x 2) [0 0 0 255] [100 100 100 255]))))
(def all {:x0 0 :y0 0 :x1 3 :y1 3})

(deftest region-stats-mean-and-std
  (are [rgba region mean std] (let [s (stats/region-stats 4 rgba region)]
                                (and (= mean (:mean s)) (< (abs (- std (:std s))) 1e-9)))
    flat all [10 20 30] 0
    halves all [50 50 50] 50
    halves {:x0 2 :y0 1 :x1 3 :y1 2} [100 100 100] 0))

(deftest fraction-counts-matching-pixels
  (let [{:keys [fraction]} (stats/region-stats 4 halves all)]
    (is (= 0.5 (fraction (fn [[r]] (> r 50)))))
    (is (= 1 (fraction (fn [_] true))))))

(deftest luminance-weights-green-most
  (is (> (stats/luminance [0 255 0]) (stats/luminance [255 0 0])))
  (is (< (abs (- (stats/luminance [255 255 255]) 255)) 1e-9)))
