(ns world-test.tree-spots
  "tools/event-counts.mjs tree-spots: counts the measured prior jobs.lib.forestry.plant-sapling/::unseen-spot-counts from the
  recorded chunk columns (worlds/<world>/chunks): the base of a tree (a log on soil, no log under it) as a replant debt
  once the tree is felled, whether a sapling of its species could be planted there, its trunk and the leaves gone: the
  growth space over it (jobs.forestry.prepare-rule/headroom-of cells) holds nothing a tree cannot grow through (logs count
  as felled), and one of its open sides has light of 9 or more (the better of sky and block light). Trees of a species the
  job does not support, and bases with no recorded open side, are left out. Per chunk the shares are kept so the prior's
  spread is fitted by moments over chunks. Every stride-th column file in name order is read, the fixture plot grid left
  out. Pure parts: base?, plantable?, tally, report."
  (:require ["fs" :as fs]
            [clojure.string :as str]
            [engine.settings :as settings]
            [jobs.forestry.plant-sapling :as plant-sapling]
            [jobs.forestry.prepare-rule :as rule]
            [jobs.lib.forestry :as forestry]
            [jobs.lib.trees :as trees]
            [world-test.field-passes :as field-passes]
            [world-test.water-lags :as water-lags]))

(def bottom-y -63)
(def top-y 318)

(defn log-species
  "The species of a log's name, or nil for a block that is no log."
  [n]
  (when (and n (str/ends-with? n "_log")) (subs n 0 (- (count n) 4))))

(defn base?
  "The species of the tree whose base is the cell (x y z) read by at, or nil: a log on soil with no log under it."
  [at x y z]
  (let [species (log-species (:name (at x y z)))]
    (when (and species
               (rule/headroom-of species)
               ((forestry/soil-for species) (:name (at x (dec y) z)))
               (not (log-species (:name (at x (dec y) z)))))
      species)))

(defn plantable?
  "True, false, or nil (no open side recorded) for a sapling of species at the base cell (x y z) read by at."
  [at species x y z]
  (let [room? (every? (fn [dy] (let [n (:name (at x (+ y dy) z))] (or (nil? n) (trees/tree-free? n) (log-species n))))
                      (range 1 (rule/headroom-of species)))
        lights (keep (fn [[dx dz]] (let [c (at (+ x dx) y (+ z dz))]
                                      (when (and c (trees/tree-free? (:name c))) (max (or (:sky c) 0) (or (:block c) 0)))))
                     [[1 0] [-1 0] [0 1] [0 -1]])]
    (when (seq lights)
      (boolean (and room? (<= (settings/get plant-sapling/settings ::plant-sapling/min-light) (reduce max lights)))))))

(defn tally
  "{:plantable n :unfit n} of the tree bases in the columns xzs ([x z]) read by at."
  [at xzs]
  (reduce (fn [m [x z]]
            (reduce (fn [m y]
                      (let [species (base? at x y z)
                            ok (when species (plantable? at species x y z))]
                        (if (nil? ok) m (update m (if ok :plantable :unfit) (fnil inc 0)))))
                    m (range bottom-y top-y)))
          {} xzs))

(defn report
  "Of chunk tallies: the pooled {:plantable :unfit}, :p-plantable pooled, and :prior [plantable unfit] Beta pseudo-counts
  fitted by moments over the chunks with at least min-n bases (field-passes/beta-fit), with :n their sum."
  [tallies min-n]
  (let [plantable (reduce + (map #(:plantable % 0) tallies))
        unfit (reduce + (map #(:unfit % 0) tallies))
        shares (keep (fn [{:keys [plantable unfit] :or {plantable 0 unfit 0}}]
                       (when (>= (+ plantable unfit) min-n) (/ plantable (+ plantable unfit))))
                     tallies)
        [a b] (if (> (count shares) 1) (field-passes/beta-fit shares plantable (+ plantable unfit)) [plantable unfit])]
    {:plantable plantable :unfit unfit :chunks (count shares)
     :p-plantable (/ (js/Math.round (* 1000 (/ plantable (max 1 (+ plantable unfit))))) 1000)
     :prior [a b] :n (/ (js/Math.round (* 10 (+ a b))) 10)}))

(defn count-in
  "report of every stride-th recorded column file in dir, read by at."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))]
    {:spots (report (map (fn [[cx cz]] (tally at (for [x (range 16) z (range 16)] [(+ (* 16 cx) x) (+ (* 16 cz) z)]))) cols) 5)
     :files (count cols) :stride stride}))
