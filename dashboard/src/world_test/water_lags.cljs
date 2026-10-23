(ns world-test.water-lags
  "tools/event-counts.mjs water-lags: counts the measured prior jobs.lib.cost.answer ::unseen-water-lags from the recorded
  chunk columns (worlds/<world>/chunks). For each surface water cell (water under air, the highest cell of its column
  that is not air between bottom-y and top-y) and each lag d of 1..max-lag along the four axes, the cell d off at the same
  level: n counts those in a recorded column, k those that are water too (water by name, as jobs.lib.look/water-fn reads it). The fixture plot grid (world-test.fixture
  default-grid and its lanes) is left out: built courses, not terrain. Every stride-th column file in name order is read.
  Pure part: tally."
  (:require ["fs" :as fs]
            [world-test.fixture :as fixture]))

(def top-y 140)
(def bottom-y 30)
(def max-lag 24)
(def dirs [[1 0] [-1 0] [0 1] [0 -1]])
(def air #{"air" "cave_air" "void_air"})

(defn surface-y
  "The y of x z's surface water by at ((fn [x y z]) -> {:name} or nil), or nil."
  [at x z]
  (loop [y top-y]
    (when (>= y bottom-y)
      (let [n (:name (at x y z))]
        (cond (nil? n) nil
              (air n) (recur (dec y))
              (= "water" n) y)))))

(defn tally
  "{d {:k :n}} of the cells d off along the axes of the surface water cells of columns ([x z] each) by at."
  [at columns]
  (reduce (fn [m [x z]]
            (if-let [y (surface-y at x z)]
              (reduce (fn [m [[dx dz] d]]
                        (if-let [c (at (+ x (* dx d)) y (+ z (* dz d)))]
                          (-> m (update-in [d :n] (fnil inc 0)) (update-in [d :k] (fnil + 0) (if (= "water" (:name c)) 1 0)))
                          m))
                      m (for [dir dirs d (range 1 (inc max-lag))] [dir d]))
              m))
          (sorted-map) columns))

(defn reserved?
  "Whether chunk column cx cz lies in the fixture plot grid or its lanes."
  [cx cz]
  (let [{:keys [x0 z0]} fixture/default-grid]
    (and (>= (* 16 cx) (- x0 64)) (>= (* 16 cz) (- z0 64)))))

(defn count-in
  "{:lags (tally) :columns read} of every stride-th recorded column file in dir, read by at."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (reserved? cx cz)))
                  sort
                  (take-nth stride))]
    {:lags (tally at (for [[cx cz] cols x (range 16) z (range 16)] [(+ (* 16 cx) x) (+ (* 16 cz) z)]))
     :columns (count cols) :stride stride}))
