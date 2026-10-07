(ns view.stats
  "Pixel statistics over a rectangle {:x0 :y0 :x1 :y1} (inclusive) of a decoded image {width, height, rgba}, for the Node pixel check.")

(defn luminance [[r g b]] (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))

(defn- pixels-of [width rgba {:keys [x0 y0 x1 y1]}]
  (for [y (range y0 (inc y1)) x (range x0 (inc x1))
        :let [i (* 4 (+ (* y width) x))]]
    [(aget rgba i) (aget rgba (+ i 1)) (aget rgba (+ i 2))]))

(defn region-stats
  "{:n :mean :std :fraction}: mean per channel, std (the mean of the per-channel standard deviations), fraction(predicate over [r g b])."
  [width rgba region]
  (let [pixels (vec (pixels-of width rgba region))
        n (count pixels)
        mean (mapv (fn [c] (/ (reduce + (map #(nth % c) pixels)) n)) [0 1 2])
        std (/ (reduce + (map (fn [c] (js/Math.sqrt (/ (reduce + (map #(let [d (- (nth % c) (nth mean c))] (* d d)) pixels)) n))) [0 1 2])) 3)]
    {:n n :mean mean :std std :fraction (fn [pred] (/ (count (filter pred pixels)) n))}))

(defn js-luminance [rgb] (luminance (vec rgb)))

(defn js-region-stats [^js image ^js region]
  (let [{:keys [n mean std fraction]} (region-stats (.-width image) (.-rgba image)
                                                    {:x0 (.-x0 region) :y0 (.-y0 region) :x1 (.-x1 region) :y1 (.-y1 region)})]
    #js {:n n :mean (to-array mean) :std std :fraction (fn [pred] (fraction #(pred (to-array %))))}))
