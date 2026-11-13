(ns jobs.lib.part-cut
  "Reading part-cut trunks: a tree of a column fell-tree left half-cut (trees/part-cut-owed) whose lowest log is raised,
  the block under it seen as air (or one over the gap above a stump). A seen tree not owed is never part-cut, whoever cut it.
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

(defn hanging
  "The lowest log of base's column (base or one of logs) that hangs over seen air: base, or the log over the gap left
  where the trunk was cut above a stump; nil when none does."
  [p logs {:keys [x z] :as base}]
  (->> (cons base (sort-by :y (filter #(and (= x (:x %)) (= z (:z %))) logs)))
       (filter #(raised? p %))
       first))

(defn part-cut?
  "Whether the tree t (as trees-near gives it) is one the body left half-cut: its column is owed (trees/part-cut-owed) and
  a log of the column hangs over seen air. logs is the set of seen log positions."
  [p owed logs t]
  (let [{:keys [x z]} (:base t)]
    (boolean (and (contains? owed [x z]) (hanging p logs (:base t))))))

(defn part-cut-trees
  "The trunks within radius of species (any when nil) in owed (the columns fell-tree left half-cut), nearest first,
  that hang over seen air. Cheap when no seen log is raised."
  [p owed radius species]
  (when (seq owed)
    (let [seen-logs (trees/scan-logs p radius nil)]
      (when (some #(raised? p (:pos %)) seen-logs)
        (let [logs (into #{} (map :pos) seen-logs)]
          (filterv #(part-cut? p owed logs %) (trees/trees-near p radius species)))))))
