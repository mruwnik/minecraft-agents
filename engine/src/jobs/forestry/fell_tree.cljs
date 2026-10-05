(ns jobs.forestry.fell-tree
  (:require [engine.ctx :as ctx]
            [engine.jobs.watch :as watch]
            [engine.jobs.blocks :as blocks]
            [engine.jobs.gate :as gate]
            [engine.jobs.forestry :refer [scan-logs tree-near trees-near tree-at logs-at unreachable-set debts replant-kind
                                          replant-policy default-radius max-partials eye-dist dig-reach log-name?]]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [engine.path.targets :as targets]))

(def doc
  "Fell the nearest tree: a log column with leaves near its top. Digs one log a round, lowest first.
  The base log's :forestry/replant debt is written before it is dug.
  With :at it fells that one column instead (:radius and :species are then unused).
  The tree is chosen by a bounded search over at most 32 candidates, nearest in a line first (engine.path.targets).
  It picks the tree the body walks to soonest, so a walled-off or cliff-top tree is passed over for a reachable
  one. The search continues over several rounds if needed. If it proves every candidate out of reach, the job
  warns tree_blocked and ends. If it runs out of nodes, it takes the nearest in a line and the walk decides.
  A tree whose walk is blocked, or partial three times in a row, is marked unreachable and the next one is chosen.
  Each log is dug by a jobs.blocks.dig child. That child holds the best carried axe and leaves the drop on the
  ground (jobs.forestry.harvest-wood collects it).
  Waits (check) with :reason :no-tree when no tree is in sight.
  Zones: a tree whose base log is in another owner's zone or claim, or in a plan's footprint (but :for-plan's
  own), is not a candidate. A later log that turns out refused makes the tree count as unreachable. The job warns
  fell-tree.declined once, with :reason :refused (or :no-zones when no zone list was read). :ignore-zones? true
  skips the check.")

(def args
  {:species {:doc "log species such as \"oak\"; any when nil" :default nil}
   :radius {:doc "search radius in blocks" :default default-radius}
   :at {:doc "{:x :y :z} of a base log: fell that one column, wherever the body is (the radius and species are not used), instead of the nearest tree" :type :pos :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(defn log-allowed?
  "Whether the job may dig the log at pos (one warn per job when refused)."
  [c pos]
  (gate/allowed? c :fell-tree.declined "fell-tree" :dig pos {:except (:for-plan (:args c))}))

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

(defn ^:async approach!
  "Get the log at pos within reach to dig: nothing when its centre is within eye reach already, else walk to within 2
  of the column's foot (3 when that has no path: the foot may be the trunk itself). :there, :partial or :blocked."
  [c pos]
  (if (<= (eye-dist (u/self-pos c) pos) dig-reach)
    :there
    (let [foot (assoc pos :y (:y (:base (ctx/mem c))))
          w (await (near/walk-near! c foot 2))]
      (if (= :blocked w)
        (await (near/walk-near! c foot 3))
        w))))

(defn log-dig-args
  "The jobs.blocks.dig args for the log at pos: no tool needed (an axe is held when carried), the drop left on the
  ground, every dig hazard taken (as a player felling a tree does)."
  [c pos]
  (merge (select-keys (:args c) [:for-plan :ignore-zones?])
         {:pos pos :collect false :need-drop false :accept #{:fluid-adjacent :falling-block :under-feet}}))

(defn outcome
  "What one round of the dig child (r, its result res, the reason its check waits with) means for the tree: :ok
  (dug, gone, or still under way), :refused, :unreachable, :cannot, or the failed dig's status as a keyword."
  [r res waits]
  (case r
    :continue :ok
    :declined (if (= :not-allowed (:reason waits)) :refused :unreachable)
    (case (:reason res)
      (:dug :already-clear) :ok
      (:cannot :fluid) :cannot
      (keyword (or (:status res) (:reason res))))))

(defn ^:async dig-log!
  "Dig the log l (one blocks.dig child round), walking in reach first. Commits the replant debt before the base log
  is dug (write-ahead; withdrawn when the log is still standing after the round). Resolves to :ok, :partial (the walk
  made progress but is not in reach yet; call again) or a non-ok outcome for the round to count as a failure
  (:unreachable, :cannot, :refused and :out-of-reach mean the tree cannot be dug from here)."
  [c l]
  (let [pos (:pos l)
        w (await (approach! c pos))]
    (case w
      :blocked :blocked
      :partial :partial
      (if-not (log-allowed? c pos)
        :refused
        (let [base? (= pos (:base (ctx/mem c)))
              wrote? (and base? (record-debt! c))
              args (log-dig-args c pos)
              r (await (ctx/call-child c :dig 'jobs.blocks.dig args))
              res (ctx/child-result c :dig)
              waits (when (= :declined r) (blocks/child-wait c :dig 'jobs.blocks.dig args))]
          (when (and wrote? (some-> (u/block-name (:primitives c) pos) log-name?))
            (ctx/forget-where! c replant-kind #(= pos (:pos %))))
          (outcome r res waits))))))

(defn candidate
  "The tree to fell: the one at :at (nil once marked unreachable), else the nearest of species within radius."
  [c radius species]
  (let [p (:primitives c)
        excluded (unreachable-set (ctx/mem c))]
    (if-let [at (:at (:args c))]
      (when-let [t (tree-at p at)]
        (when-not (or (excluded [(:x at) (:z at)]) (not (log-allowed? c (:base t)))) t))
      (loop [excluded excluded]
        (when-let [t (tree-near p radius species excluded)]
          (if (log-allowed? c (:base t))
            t
            (recur (conj excluded [(:x (:column t)) (:z (:column t))]))))))))

(def approach-range
  "Blocks from a tree's base the search for the tree to fell counts as reaching it: approach!'s looser range."
  3)

(defn candidates
  "The trees the job may fell, nearest in a line first (lazily): the one at :at (none once marked unreachable), else
  those of species within radius not marked unreachable; a tree whose base log the zones refuse is left out."
  [c radius species]
  (let [p (:primitives c)
        excluded (unreachable-set (ctx/mem c))]
    (if-let [at (:at (:args c))]
      (when-let [t (tree-at p at)]
        (when-not (or (excluded [(:x at) (:z at)]) (not (log-allowed? c (:base t)))) [t]))
      (filter #(log-allowed? c (:base %)) (trees-near p radius species excluded)))))

(defn tree-logs
  "The logs of the chosen column: read from :at up when the job was given one, else those in radius."
  [c radius]
  (let [m (ctx/mem c)]
    (if-let [at (:at (:args c))]
      (logs-at (:primitives c) at (:species m))
      (column-logs (:primitives c) radius m))))

(defn commit-tree!
  "Commit the column, species and base of the tree t to fell."
  [c t]
  (ctx/update-mem! c #(-> % (merge (select-keys t [:column :species :base])) (assoc :partials 0)))
  t)

(defn mark-all-unreachable!
  "Remember the columns of trees as unreachable."
  [c trees]
  (ctx/update-mem! c update :unreachable (fnil into []) (map (fn [t] [(get-in t [:column :x]) (get-in t [:column :z])]) trees)))

(defn ^:async choose-tree!
  "Choose the tree to fell among the candidates (at most targets/max-targets): the one the body walks to soonest
  (targets/nearest!), committed (commit-tree!). :searching while that search goes on; nil when no candidate is in
  sight, or when the search proved every one out of reach (all marked unreachable); the nearest in a line when it ran
  out of nodes (the walk decides)."
  [c radius species]
  (let [ts (vec (take targets/max-targets (candidates c radius species)))]
    (when (seq ts)
      (let [a (await (targets/nearest! c (mapv :base ts) approach-range {:tag :fell-tree}))]
        (case (:status a)
          :found (commit-tree! c (nth ts (:index a)))
          :searching :searching
          (if (:proved a)
            (do (mark-all-unreachable! c ts) nil)
            (commit-tree! c (first ts))))))))

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
  [c]
  (let [{:keys [species radius]} (:args c)
        chosen (or (:column (ctx/mem c)) (await (choose-tree! c radius species)))]
    (cond
      (= :searching chosen) :continue

      (and (not chosen) (seq (:unreachable (ctx/mem c))))
      (do (ctx/emit! c :tree_blocked :warn {:text "no reachable tree"})
          :done)

      (not chosen) :continue

      :else
      (let [logs (tree-logs c radius)]
        (if (empty? logs)
          :done
          (let [_ (await (watch/watch! c {:before-dig (:pos (first logs))}))
                r (await (dig-log! c (first logs)))]
            (case r
              :ok (do (ctx/update-mem! c assoc :partials 0) :continue)
              (:partial :blocked) (walk-failed! c r)
              (:unreachable :cannot :out-of-reach :refused) (do (mark-unreachable! c) :continue)
              (u/fail! c :tree_blocked (str "cannot dig the tree: " (name r))))))))))

(defn check
  "A tree is chosen, or every candidate was unreachable (the round warns and
  finishes), or a tree is in sight. Else it waits with reason :no-tree (and :radius, :species when given)."
  [c]
  (let [m (ctx/mem c)
        {:keys [radius species]} (:args c)]
    (or (boolean (or (:column m)
                     (seq (:unreachable m))
                     (candidate c radius species)))
        (ctx/wait c (cond-> {:reason :no-tree :radius radius} species (assoc :species species))))))
