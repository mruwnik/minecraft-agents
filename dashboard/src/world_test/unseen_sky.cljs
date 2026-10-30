(ns world-test.unseen-sky
  "tools/event-counts.mjs unseen-sky: counts the measured prior jobs.lib.cost.base-rates/sky-under from the recorded chunk
  columns (worlds/<world>/chunks): of an open cell a body standing there has not seen, under a seen open cell with sky
  light 15 with n unseen cells between (1..n-max, farther as n-max), the share with sky light L (1..15) or more, by
  whether any of that seen cell's four sides is seen solid (:narrow: a hole in a roof or floor) or none is (:wide: open
  sky over a wall or a drop). Sight and stands as world-test.unseen-solid's. Per chunk the shares are kept so each
  bucket's Beta is fitted by moments over chunks (a body sees one place). Pure parts: tally, report."
  (:require [world-test.field-passes :as field-passes]
            [world-test.unseen-solid :as us]
            [world-test.water-lags :as water-lags]
            ["fs" :as fs]))

(def n-max "The most unseen cells between that the prior tells apart: more read as this many." 8)

(def ^:private side us/side)
(defn- idx [x y z] (+ x (* side (+ z (* side y)))))

(defn- seen-solid? [^js b ^js seen x y z]
  (and (< -1 x side) (< -1 z side) (let [i (idx x y z)] (and (== 1 (aget seen i)) (== 1 (aget (.-solid b) i))))))

(defn tally
  "{[kind n] {:n cells :sky [cells with sky light L or more, L 1..15]}} of the unseen loaded open cells of the box b
  under a seen open cell of sky light 15 (kind :wide or :narrow, n the unseen cells between)."
  [^js b ^js seen]
  (let [^js solid (.-solid b) ^js lit (.-light b) ^js loaded (.-loaded b)]
    (reduce
     (fn [m i]
       (let [x (mod i side) z (mod (quot i side) side) y (quot i (* side side))]
         (if-not (and (== 1 (aget loaded i)) (zero? (aget seen i)) (zero? (aget solid i)))
           m
           (let [top (loop [y' (inc y)] (when (< y' side) (if (== 1 (aget seen (idx x y' z))) y' (recur (inc y')))))
                 j (when top (idx x top z))]
             (if-not (and j (> top (inc y)) (zero? (aget solid j)) (== 15 (bit-shift-right (aget lit j) 4)))
               m
               (let [kind (if (some #(apply seen-solid? b seen %) [[(inc x) top z] [(dec x) top z] [x top (inc z)] [x top (dec z)]]) :narrow :wide)
                     sky (bit-shift-right (aget lit i) 4)]
                 (update m [kind (min n-max (- top y 1))]
                         (fn [{:keys [n] :as v}]
                           {:n (inc (or n 0)) :sky (mapv #(+ (if (>= sky %2) 1 0) %1) (or (:sky v) (repeat 15 0)) (range 1 16))}))))))))
     {} (range (* side side side)))))

(defn stand-tally
  "tally from the stand at feet x y z by at."
  [at x y z]
  (let [b (us/box at (us/open-ids) x y z)]
    (tally b (us/sight b us/visible-by-day))))

(defn chunk-tally
  "The tallies of every stand of the sampled columns of chunk cx cz by at, summed."
  [at cx cz]
  (let [^js open (us/open-ids)
        solid? (fn [x y z] (when-let [c (at x y z)] (not (and (:id c) (== 1 (aget open (:id c)))))))]
    (apply merge-with (fn [a b] {:n (+ (:n a) (:n b)) :sky (mapv + (:sky a) (:sky b))}) {}
           (for [[lx lz] us/columns
                 :let [x (+ (* 16 cx) lx) z (+ (* 16 cz) lz)]
                 y (range -63 318)
                 :when (and (true? (solid? x (dec y) z)) (false? (solid? x y z)) (false? (solid? x (inc y) z)))]
             (stand-tally at x y z)))))

(defn report
  "Of chunk tallies: {:n-max :wide :narrow :n-wide :n-narrow}, :wide and :narrow per n 1..n-max {:a :b} by L - 1, the Beta
  of the share with sky light L or more fitted by moments over the chunks with at least min-n cells of the bucket."
  [tallies min-n]
  (into {:n-max n-max}
        (for [kind [:wide :narrow]
              :let [rows (fn [n] (keep #(get % [kind n]) tallies))]
              [k v] [[kind (vec (for [n (range 1 (inc n-max))]
                                  (let [rs (rows n)
                                        total (reduce + (map :n rs))
                                        fits (for [l (range 15)]
                                               (let [k (reduce + (map #(nth (:sky %) l) rs))
                                                     shares (keep #(when (>= (:n %) min-n) (/ (nth (:sky %) l) (:n %))) rs)]
                                                 (if (> (count shares) 1) (field-passes/beta-fit shares k total) [k (- total k)])))]
                                    {:a (mapv first fits) :b (mapv second fits)})))]
                     [(keyword (str "n-" (name kind))) (vec (for [n (range 1 (inc n-max))] (reduce + (map :n (rows n)))))]]]
          [k v])))

(defn count-in
  "report of every stride-th recorded column file in dir, read by at."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))]
    (assoc (report (map (fn [[cx cz]] (chunk-tally at cx cz)) cols) 50) :files (count cols) :stride stride)))
