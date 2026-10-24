(ns world-test.surface-heights
  "tools/event-counts.mjs surface-heights: counts the measured prior jobs.lib.cost.rise ::surface-prior from the recorded chunk
  columns (worlds/<world>/chunks): the y a body would stand at on the ground of a column (one over its highest block that is
  not air, a plant or a tree, jobs.lib.surface/open? and canopy?; water counts as ground), in bins of bin blocks, as
  {bin-middle columns}. The fixture plot grid (world-test.water-lags/reserved?) is left out: built courses, not terrain.
  Every stride-th column file in name order is read. Pure part: tally."
  (:require ["fs" :as fs]
            [jobs.lib.surface :as surface]
            [world-test.water-lags :as water-lags]))

(def top-y 319)
(def bottom-y 0)
(def bin "Blocks one count row spans." 4)

(defn stand-y
  "The y a body stands at on the ground of column x z by at ((fn [x y z]) -> {:name} or nil), or nil when the column holds no ground."
  [at x z]
  (loop [y top-y]
    (when (>= y bottom-y)
      (let [n (:name (at x y z))]
        (cond (nil? n) nil
              (or (surface/open? n) (surface/canopy? n)) (recur (dec y))
              :else (inc y))))))

(defn tally
  "{bin-middle columns} of the stand heights of columns ([x z] each) by at."
  [at columns]
  (reduce (fn [m [x z]]
            (if-let [y (stand-y at x z)]
              (update m (+ (* bin (quot y bin)) (quot bin 2)) (fnil inc 0))
              m))
          (sorted-map) columns))

(defn count-in
  "{:heights (tally) :columns read} of every stride-th recorded column file in dir, read by at."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))]
    {:heights (tally at (for [[cx cz] cols x (range 16) z (range 16)] [(+ (* 16 cx) x) (+ (* 16 cz) z)]))
     :columns (* 256 (count cols)) :files (count cols) :stride stride}))
