(ns jobs.lib.part-cut
  "Reading part-cut trunks: a natural tree (logs with leaves near the top, as jobs.lib.trees reads one) whose lowest log
  is raised, the block under it seen as air, because the logs under it were cut. A raised log joined (side or corner
  touching, through logs) to a log standing on the ground is part of a living tree (an acacia or fancy oak branch, a
  mangrove root) and is left. Not a job namespace."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.trees :as trees :refer [seen-name]]))

(a/defargs settings
  {::ground-depth {:default 24 :doc "Blocks under a raised log looked down for the ground to replant on." :spec (a/int-in 1 nil)}})

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

(defn part-cut?
  "Whether the tree t (as trees-near gives it) is a part-cut trunk; logs is the lazily read set of seen log positions
  (a delay)."
  [p logs t]
  (and (raised? p (:base t)) (not (living? p @logs (:base t)))))

(defn part-cut-trees
  "The part-cut trunks within radius of species (any when nil), nearest first. Cheap when no seen log is raised."
  [p radius species]
  (let [seen-logs (trees/scan-logs p radius nil)]
    (when (some #(raised? p (:pos %)) seen-logs)
      (let [logs (delay (into #{} (map :pos) seen-logs))]
        (filterv #(part-cut? p logs %) (trees/trees-near p radius species))))))
