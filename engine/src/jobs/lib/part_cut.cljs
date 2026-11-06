(ns jobs.lib.part-cut
  "Reading part-cut trunks: a natural tree (logs with leaves near the top, as jobs.lib.trees reads one) whose lowest log
  is raised, the block under it seen as air, because the logs under it were cut (or one over the gap above a stump). A raised log joined (side or corner
  touching, through logs) to a log that may stand on the ground is part of a living tree (an acacia or fancy oak branch, a
  mangrove root) and is left; so is one under a crown over a standing trunk. Unseen cells are unknown both ways: no
  evidence of a cut, and maybe ground under a log (sight rarely enters the block under a trunk).
  Not a job namespace."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.trees :as trees :refer [seen-name]]))

(a/defargs settings
  {::ground-depth {:default 24 :doc "Blocks looked down under a raised log for the ground: the cell to replant on and where a cut column ends." :spec (a/int-in 1 nil)}})

(defn raised?
  "Whether the block under pos is seen as air."
  [p pos]
  (contains? bs/air (seen-name p (update pos :y dec))))

(defn ground-cell
  "The cell over the first seen block below pos that is not air, or nil when none is seen within the ground-depth
  setting (or an unseen cell comes first)."
  [p {:keys [y] :as pos}]
  (loop [dy 1]
    (when (<= dy (settings/get settings ::ground-depth))
      (let [below (assoc pos :y (- y dy))
            n (seen-name p below)]
        (cond
          (nil? n) nil
          (contains? bs/air n) (recur (inc dy))
          :else (assoc below :y (inc (:y below))))))))

(defn neighbours [{:keys [x y z]}]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] :when (not (and (zero? dx) (zero? dy) (zero? dz)))]
    {:x (+ x dx) :y (+ y dy) :z (+ z dz)}))

(defn grounded?
  "Whether the log at pos may stand on the ground: the cell under it is not seen as air or a log (unseen is maybe ground)."
  [p pos]
  (let [n (seen-name p (update pos :y dec))]
    (not (or (contains? bs/air n) (trees/log-name? n)))))

(defn living?
  "Whether the log at pos is joined to a log that may stand on the ground (grounded?): a flood fill over the seen logs (a set of
  positions), side or corner touching."
  [p logs pos]
  (loop [todo [pos] seen #{pos}]
    (when-let [at (peek todo)]
      (if (grounded? p at)
        true
        (let [more (remove seen (filter logs (neighbours at)))]
          (recur (into (pop todo) more) (into seen more)))))))

(defn cut-column?
  "Whether the column under the hanging log h is seen cut down to the ground: seen air, or seen air over a seen stump of
  logs, then a seen block that is neither air, log nor leaves, within the ground-depth setting. An unseen cell is no
  evidence of a cut, except under a stump, whose underside is buried."
  [p {:keys [y] :as h}]
  (loop [dy 1 stump? false]
    (when (<= dy (settings/get settings ::ground-depth))
      (let [n (seen-name p (assoc h :y (- y dy)))]
        (cond
          (nil? n) stump?
          (contains? bs/air n) (when-not stump? (recur (inc dy) false))
          (trees/log-name? n) (recur (inc dy) true)
          :else (not (trees/leaves-name? n)))))))

(def leaf-decay-steps
  "Vanilla: a leaf more steps than this (face to face, through leaves) from any log decays, so a tree's leaves lie within it."
  6)

(defn crown
  "The seen leaves (a set of positions) that col (the logs of one column) owns: within leaf-decay-steps (taxicab, so unseen
  cells on the way do not matter) of a log of col, and no nearer to a seen log of another column (vanilla decay counts from
  the nearest log), so a neighbour's own leaves are not part of this tree's crown. A tie stays with col."
  [leaves col others]
  (let [steps (fn [a b] (+ (abs (- (:x a) (:x b))) (abs (- (:y a) (:y b))) (abs (- (:z a) (:z b)))))
        nearest (fn [l logs] (transduce (map #(steps l %)) min ##Inf logs))]
    (into #{} (filter (fn [l] (let [own (nearest l col)] (and (<= own leaf-decay-steps) (<= own (nearest l others)))))) leaves)))

(defn standing-under?
  "Whether the seen log g may stand on the ground (grounded?) with its column going up, through seen logs and unseen cells, to touch a
  leaf of crown: a standing trunk under that crown."
  [p crown {:keys [y] :as g}]
  (and (grounded? p g)
       (->> (range trees/max-column)
            (map #(assoc g :y (+ y %)))
            (take-while #(let [n (seen-name p %)] (or (nil? n) (trees/log-name? n))))
            (some #(some crown (neighbours %)))
            boolean)))

(defn hanging
  "The lowest log of base's column (base or one of logs) that hangs over seen air: base, or the log over the gap left
  where the trunk was cut above a stump; nil when none does."
  [p logs {:keys [x z] :as base}]
  (->> (cons base (sort-by :y (filter #(and (= x (:x %)) (= z (:z %))) logs)))
       (filter #(raised? p %))
       first))

(defn seen-around
  "The seen logs and leaves within radius, as sets of positions {:logs :leaves}."
  [p radius]
  {:logs (into #{} (map :pos) (trees/scan-logs p radius nil))
   :leaves (into #{} (map :pos) (trees/scan p (+ radius 4) trees/leaves-name? trees/max-leaves))})

(defn part-cut?
  "Whether the tree t (as trees-near gives it) is a part-cut trunk, read only from seen evidence of a cut: a log of its
  column hangs over a column seen cut to the ground (cut-column?), it is joined to no standing log, and no seen standing
  trunk rises under its crown (the leaves its column's logs own). seen is a delay of seen-around."
  [p seen t]
  (let [{:keys [logs leaves]} @seen
        {:keys [x y z] :as h} (hanging p logs (:base t))]
    (boolean
     (and h (cut-column? p h) (not (living? p logs h))
          (let [c (crown leaves (cons h (filter #(and (= x (:x %)) (= z (:z %)) (> (:y %) y)) logs))
                        (remove #(and (= x (:x %)) (= z (:z %))) logs))]
            (not-any? #(standing-under? p c %) logs))))))

(defn part-cut-trees
  "The part-cut trunks within radius of species (any when nil), nearest first. Cheap when no seen log is raised."
  [p radius species]
  (let [seen-logs (trees/scan-logs p radius nil)]
    (when (some #(raised? p (:pos %)) seen-logs)
      (let [seen (delay (seen-around p radius))]
        (filterv #(part-cut? p seen %) (trees/trees-near p radius species))))))
