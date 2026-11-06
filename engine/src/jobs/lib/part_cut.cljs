(ns jobs.lib.part-cut
  "Reading part-cut trunks: a natural tree (logs with leaves near the top, as jobs.lib.trees reads one) whose lowest log
  is raised, the block under it seen as air, because the logs under it were cut (or one over the gap above a stump). A raised log joined (side or corner
  touching, through logs) to a log standing on the ground is part of a living tree (an acacia or fancy oak branch, a
  mangrove root) and is left; so is one under a crown over a seen standing trunk. Unseen cells are no evidence of a cut.
  Not a job namespace."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.trees :as trees :refer [seen-name]]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::ground-depth {:default 24 :doc "Blocks looked down under a raised log for the ground to replant on, (a cut column ends there)." :spec (a/int-in 1 nil)}})

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
  "Whether the log at pos stands on a seen block that is neither air nor a log."
  [p pos]
  (let [n (seen-name p (update pos :y dec))]
    (and n (not (contains? bs/air n)) (not (trees/log-name? n)))))

(defn living?
  "Whether the log at pos is joined to a log standing on the ground: a flood fill over the seen logs (a set of
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
  evidence of a cut."
  [p {:keys [y] :as h}]
  (loop [dy 1 stump? false]
    (when (<= dy (settings/get settings ::ground-depth))
      (let [n (seen-name p (assoc h :y (- y dy)))]
        (cond
          (nil? n) false
          (contains? bs/air n) (when-not stump? (recur (inc dy) false))
          (trees/log-name? n) (recur (inc dy) true)
          :else (not (trees/leaves-name? n)))))))

(defn crown
  "The seen leaves (a set of positions) touching, through seen leaves, the leaves within leaf reach of top."
  [leaves top]
  (loop [todo (filterv #(<= (u/dist top %) (trees/leaf-reach)) leaves) seen (set todo)]
    (if-let [at (peek todo)]
      (let [more (remove seen (filter leaves (neighbours at)))]
        (recur (into (pop todo) more) (into seen more)))
      seen)))

(defn standing-under?
  "Whether the seen log g stands on the ground with its column going up, through seen logs and unseen cells, to touch a
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
  trunk rises under its crown (the leaves round its column's top). seen is a delay of seen-around."
  [p seen t]
  (let [{:keys [logs leaves]} @seen
        {:keys [x z] :as h} (hanging p logs (:base t))]
    (boolean
     (and h (cut-column? p h) (not (living? p logs h))
          (let [top (apply max-key :y h (filter #(and (= x (:x %)) (= z (:z %))) logs))
                c (crown leaves top)]
            (not-any? #(standing-under? p c %) logs))))))

(defn part-cut-trees
  "The part-cut trunks within radius of species (any when nil), nearest first. Cheap when no seen log is raised."
  [p radius species]
  (let [seen-logs (trees/scan-logs p radius nil)]
    (when (some #(raised? p (:pos %)) seen-logs)
      (let [seen (delay (seen-around p radius))]
        (filterv #(part-cut? p seen %) (trees/trees-near p radius species))))))
