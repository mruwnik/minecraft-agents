(ns jobs.forestry.fell-tree
  (:require [engine.ctx :as ctx]
            [engine.jobs.forestry :refer [scan-logs tree-near tree-at logs-at unreachable-set debts replant-kind
                                          replant-policy default-radius logs-per-round max-partials eye-dist dig-reach]]
            [engine.jobs.util :as u]))

(def doc
  "Fell the nearest tree (a log column with leaves near its top), two logs a
  round, lowest first; write a :forestry/replant debt before the base log is dug.")

(def args
  {:species {:doc "log species such as \"oak\"; any when nil" :default nil}
   :radius {:doc "search radius in blocks" :default default-radius}
   :at {:doc "{:x :y :z} of a base log: fell that one column, wherever the body is (the radius and species are not used), instead of the nearest tree" :default nil}})

(defn column-logs
  "Logs standing in the chosen column, lowest first."
  [p radius {:keys [column species]}]
  (->> (scan-logs p radius species)
       (filter #(and (= (:x column) (get-in % [:pos :x])) (= (:z column) (get-in % [:pos :z]))))
       (sort-by #(get-in % [:pos :y]))))

(defn record-debt!
  "Write the replant debt for the tree this job chose, once. True when it wrote one."
  [c]
  (let [{:keys [base species]} (ctx/mem c)]
    (when-not (some #(= base (:pos %)) (debts c))
      (ctx/remember! c replant-kind {:pos base :species species} replant-policy)
      true)))

(def untouched-statuses
  "Dig statuses that leave the log standing, so the debt written ahead is void."
  #{"unreachable" "cannot"})

(defn ^:async dig-up!
  "Dig the logs in order, walking in reach first. Commits the replant debt
  before the base log is dug (write-ahead; withdrawn when the dig leaves the log standing). Resolves to :ok, :partial (the walk made progress
  but is not in reach yet; call again) or a non-ok walk or dig status for the
  caller to count as a failure (:unreachable, :cannot and :out-of-reach mean
  the tree cannot be dug from here and are marked unreachable by the round)."
  [c logs]
  (loop [[l & more] logs]
    (if-not l
      :ok
      (let [w (if (<= (eye-dist (u/self-pos c) (:pos l)) dig-reach)
                :there
                (await (u/walk-near! c (assoc (:pos l) :y (:y (:base (ctx/mem c)))) 2)))]
        (case w
          :blocked :blocked
          :partial :partial
          (let [base? (= (:pos l) (:base (ctx/mem c)))
                wrote? (and base? (record-debt! c))
                r (await (ctx/act c :dig (clj->js {:pos (:pos l)})))]
            (when (and wrote? (untouched-statuses (.-status r)))
              (ctx/forget-where! c replant-kind #(= (:pos l) (:pos %))))
            (if (#{"dug" "missing"} (.-status r))
              (recur more)
              (keyword (.-status r)))))))))

(defn candidate
  "The tree to fell: the one at :at (nil once marked unreachable), else the nearest of species within radius."
  [c radius species]
  (let [p (:primitives c)
        excluded (unreachable-set (ctx/mem c))]
    (if-let [at (:at (:args c))]
      (when-let [t (tree-at p at)]
        (when-not (excluded [(:x at) (:z at)]) t))
      (tree-near p radius species excluded))))

(defn tree-logs
  "The logs of the chosen column: read from :at up when the job was given one, else those in radius."
  [c radius]
  (let [m (ctx/mem c)]
    (if-let [at (:at (:args c))]
      (logs-at (:primitives c) at (:species m))
      (column-logs (:primitives c) radius m))))

(defn choose-tree!
  "Commit the column, species and base of the tree to fell (see candidate); nil when no candidate is in sight."
  [c radius species]
  (when-let [t (candidate c radius species)]
    (ctx/update-mem! c #(-> % (merge (select-keys t [:column :species :base])) (assoc :partials 0)))
    t))

(defn mark-unreachable!
  "Remember the chosen column as unreachable and forget the choice."
  [c]
  (ctx/update-mem! c (fn [m] (-> m
                             (update :unreachable (fnil conj []) [(get-in m [:column :x]) (get-in m [:column :z])])
                             (dissoc :column :species :base)
                             (assoc :partials 0)))))

(defn walk-failed!
  "Book a walk result of :blocked or :partial against the chosen tree."
  [c r]
  (let [partials (inc (:partials (ctx/mem c) 0))]
    (if (or (= :blocked r) (>= partials max-partials))
      (mark-unreachable! c)
      (ctx/update-mem! c assoc :partials partials))
    :continue))

(defn ^:async round
  "args {:species name-or-nil :radius 16}. Picks a tree (log column with
  leaves) the first round and remembers the column, then digs up to two logs
  bottom-up per round. Writes the replant debt {:pos base :species} to body
  memory kind :forestry/replant when the base log is dug. A tree whose walk is
  :blocked, or partial three times in a row, is remembered as unreachable and
  the next candidate is chosen; with none left it warns tree_blocked and
  finishes. Done when the column holds no logs."
  [c]
  (let [{:keys [species radius]} (:args c)
        chosen (or (:column (ctx/mem c)) (choose-tree! c radius species))]
    (cond
      (and (not chosen) (seq (:unreachable (ctx/mem c))))
      (do (ctx/emit! c :tree_blocked :warn {:text "no reachable tree"})
          :done)

      (not chosen) :continue

      :else
      (let [logs (tree-logs c radius)]
        (if (empty? logs)
          :done
          (let [r (await (dig-up! c (take logs-per-round logs)))]
            (case r
              :ok (do (ctx/update-mem! c assoc :partials 0) :continue)
              (:partial :blocked) (walk-failed! c r)
              (:unreachable :cannot :out-of-reach) (do (mark-unreachable! c) :continue)
              (u/fail! c :tree_blocked (str "cannot dig the tree: " (name r))))))))))

(defn check
  "A tree is chosen, or every candidate was unreachable (the round warns and
  finishes), or a tree is in sight."
  [c]
  (let [m (ctx/mem c)
        {:keys [radius species]} (:args c)]
    (boolean (or (:column m)
                 (seq (:unreachable m))
                 (candidate c radius species)))))
