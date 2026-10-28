(ns world-test.floor-neighbours
  "tools/event-counts.mjs floor-neighbours: counts the measured prior jobs.lib.cost.value/::unseen-floor-counts from the
  recorded chunk columns (worlds/<world>/chunks): from a stand whose floor is there (a block that is no air, plant, tree,
  fluid or ice, jobs.lib.surface), the floor of the next stand one block over (x or z, inside the chunk), by the kind of the
  step (jobs.lib.walk.sight/floor-kind): :level (its feet and head open), :up (its feet and head open, and the cell over
  the stand's head), :down (open from over its head down to its feet one lower), :drop (likewise two or three lower).
  Present: the step's floor is a floor; absent: anything else. Split by the stand's sky light (:lit over 0, :dark 0), as
  surface and caves differ. Per chunk the shares are kept so the prior's spread is fitted by moments over chunks (a call
  walks one place). Every stride-th column file in name order is read, the fixture plot grid left out. Pure parts: tally,
  report."
  (:require ["fs" :as fs]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.surface :as surface]
            [world-test.field-passes :as field-passes]
            [world-test.water-lags :as water-lags]))

(def bottom-y -63)
(def top-y 318)

(defn open? [n] (boolean (and n (surface/open? n))))
(defn floor? [n] (boolean (and n (not (surface/open? n)) (not (surface/canopy? n)) (not (surface/no-floor n)) (not (bs/fluids n)))))

(defn step-floor
  "[kind floor-y] of the step from a stand at feet y in column a to column b, by name of (y) of each, or nil when b's cells
  leave no step of a kind there. Kinds are tried shallowest first, as the planner plans an unseen cell as a floor."
  [a b y]
  (cond
    (and (open? (b y)) (open? (b (inc y)))) [:level (dec y)]
    (and (open? (b (inc y))) (open? (b (+ y 2))) (open? (a (+ y 2)))) [:up y]))

(defn down-floors
  "[[kind floor-y]] of the steps down from a stand at feet y in column a to column b whose feet and head are open (a :level
  step's floor not there): one lower :down, two or three :drop, each while b stays open down to its feet."
  [b y]
  (when (and (open? (b y)) (open? (b (inc y))))
    (loop [d 1 out []]
      (if (and (<= d 3) (open? (b (- y d))))
        (recur (inc d) (conj out [(if (= 1 d) :down :drop) (- y d 1)]))
        out))))

(defn tally
  "{[kind light] {:present n :absent n}} of the steps out of every stand of columns a (fn [y] -> name or nil) to their
  neighbours bs, sky (fn [y] -> sky light) of a's feet."
  [a sky bs]
  (reduce (fn [m y]
            (if-not (and (floor? (a (dec y))) (open? (a y)) (open? (a (inc y))))
              m
              (let [light (if (pos? (or (sky y) 0)) :lit :dark)]
                (reduce (fn [m b]
                          (reduce (fn [m [kind fy]]
                                    (update-in m [[kind light] (if (floor? (b fy)) :present :absent)] (fnil inc 0)))
                                  m (concat (some-> (step-floor a b y) vector) (down-floors b y))))
                        m bs))))
          {} (range bottom-y top-y)))

(defn chunk-tally
  "tally over the 256 columns of chunk cx cz by at, each to its x and z neighbours inside the chunk."
  [at cx cz]
  (let [col (memoize (fn [x z] (let [names (into {} (map (fn [y] [y (:name (at (+ (* 16 cx) x) y (+ (* 16 cz) z)))])) (range (- bottom-y 5) (+ top-y 4)))] names)))
        sky (fn [x z] (fn [y] (:sky (at (+ (* 16 cx) x) y (+ (* 16 cz) z)))))]
    (apply merge-with (partial merge-with +)
           {}
           (for [x (range 16) z (range 16)
                 :let [bs (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                                :let [x' (+ x dx) z' (+ z dz)]
                                :when (and (< -1 x' 16) (< -1 z' 16))]
                            (col x' z'))]]
             (tally (col x z) (sky x z) bs)))))

(defn report
  "Of chunk tallies: per [kind light] the pooled {:present :absent}, :p-absent pooled, and :prior [absent present] Beta
  pseudo-counts fitted by moments over the chunks with at least min-n steps of it (field-passes/beta-fit), with :n their sum."
  [tallies min-n]
  (into {}
        (for [k (distinct (mapcat keys tallies))
              :let [rows (keep #(get % k) tallies)
                    absent (reduce + (map #(:absent % 0) rows))
                    present (reduce + (map #(:present % 0) rows))
                    shares (keep (fn [{:keys [absent present] :or {absent 0 present 0}}]
                                   (when (>= (+ absent present) min-n) (/ absent (+ absent present))))
                                 rows)
                    [a b] (if (> (count shares) 1) (field-passes/beta-fit shares absent (+ absent present)) [absent present])]]
          [k {:absent absent :present present :chunks (count shares)
              :p-absent (/ (js/Math.round (* 1000 (/ absent (max 1 (+ absent present))))) 1000)
              :prior [a b] :n (/ (js/Math.round (* 10 (+ a b))) 10)}])))

(defn count-in
  "report of every stride-th recorded column file in dir, read by at."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))]
    {:steps (report (map (fn [[cx cz]] (chunk-tally at cx cz)) cols) 20) :files (count cols) :stride stride}))
