(ns jobs.forestry.fell-tree
  (:require [engine.ctx :as ctx]
            [engine.jobs.blocks :as blocks]
            [engine.jobs.gate :as gate]
            [engine.jobs.forestry :refer [scan-logs tree-near tree-at logs-at unreachable-set debts replant-kind
                                          replant-policy default-radius max-partials eye-dist dig-reach log-name?]]
            [engine.jobs.util :as u]
            [engine.path.near :as near]))

(def doc
  "Fell the nearest tree (a log column with leaves near its top), one log a
  round, lowest first; write a :forestry/replant debt before the base log is dug.
  Each log is dug by a jobs.blocks.dig child (:dig), which holds the best carried axe, records a log of another's
  dug with :ignore-zones? for tidying, and leaves the drop on the ground (jobs.forestry.harvest-wood collects it).

  Zones and claims are a rule the job consults: a tree whose base log is in a zone or claim of another owner, or in
  a plan's footprint (but :for-plan's own), is not a candidate; a log of the chosen tree that turns out to be refused
  (a zone edge through the trunk) is asked again right before the dig and the tree is left, as an unreachable one
  is. One fell-tree.declined warn per job names the zones, claims and plans ({:reason :refused ...}); without a zone
  list it declines with {:reason :no-zones}. A job whose every tree is refused ends like one with no tree in sight.
  :ignore-zones? acts regardless.")

(def args
  {:species {:doc "log species such as \"oak\"; any when nil" :default nil}
   :radius {:doc "search radius in blocks" :default default-radius}
   :at {:doc "{:x :y :z} of a base log: fell that one column, wherever the body is (the radius and species are not used), instead of the nearest tree" :default nil}
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
  leaves) the first round and remembers the column, then digs one log
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
          (let [r (await (dig-log! c (first logs)))]
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
