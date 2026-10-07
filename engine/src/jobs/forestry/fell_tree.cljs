(ns jobs.forestry.fell-tree
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.watch :as watch]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.pillar :as pl]
            [jobs.lib.access :as access]
            [jobs.lib.access.approach :as approach]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.trees :refer [scan-logs tree-near trees-near tree-at logs-at unreachable-set debts replant-kind
                                          replant-policy default-radius max-partials dig-reach log-name?]]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.targets :as targets]))

(def doc
  "Fell the nearest tree: a log column with leaves near its top. One call fells the whole tree, lowest log first; it
  yields (:continue) only while a child waits on the world.
  The base log's :forestry/replant debt is written before it is dug.
  With :at it fells that one column instead (:radius and :species are then unused).
  The tree is chosen by a bounded search over at most 32 candidates, nearest in a line first (jobs.lib.targets).
  It picks the tree the body walks to soonest, so a walled-off or cliff-top tree is passed over for a reachable
  one. The search goes on, slice by slice, within the call. If it proves every candidate out of reach, the job
  warns tree_blocked and ends. If it runs out of nodes, it takes the nearest in a line and the walk decides.
  A tree whose walk is blocked, or partial three times in a row, is marked unreachable and the next one is chosen.
  A log still out of reach once the body stands at the foot is felled from a pillar: jobs.lib.access.approach/plan picks
  the stand or the pillar base for the highest logs, the body walks there (go-to), builds the pillar
  (jobs.access.pillar, every block in the scaffold ledger), digs the logs in reach top-down, then jobs.access.cleanup
  takes the pillar back before the tree counts as felled (the same cleanup runs when a round starts with this job's
  scaffold still open). With too few dirt or cobblestone carried it fetches the height in blocks (jobs.lib.fetch,
  unless :fetch is false); else, or when the fetch failed, it waits (check) with :reason :need (:any-of the two,
  :count the height). A tree whose pillar a zone, claim or footprint refuses (warn fell-tree.declined, :reason
  :refused; other refusals warn with their :reason), or that has no ground or stand to reach it from, counts as unreachable,
  and so does one whose pillar digs no log. A cleanup that leaves the scaffold open is a failed round (tree_blocked
  after a few). :pillar? false leaves such a tree.
  Each log is dug by a jobs.blocks.dig child. That child holds the best carried axe and leaves the drop on the
  ground (jobs.forestry.harvest-wood collects it).
  Waits (check) with :reason :no-tree when no tree is in sight, after one look around from where it stands.
  Zones: a tree whose base log is in another owner's zone or claim, or in a plan's footprint (but :for-plan's
  own), is not a candidate. A later log that turns out refused makes the tree count as unreachable. The job warns
  fell-tree.declined once, with :reason :refused (or :no-zones when no zone list was read). :ignore-zones? true
  skips the check.
  Result: {:base pos} of the tree it felled, when it did.")

(a/defargs args
  {:species {:doc "log species such as \"oak\"; any when nil" :spec a/name? :default nil}
   :radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :at {:doc "{:x :y :z} of a base log: fell that one column, wherever the body is (the radius and species are not used), instead of the nearest tree" :spec ::a/pos :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :spare-own-builds {:doc "a log in a plan this body made is not felled; false: it may be" :spec boolean? :default true}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :falling-block :under-feet" :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block :under-feet}) :default #{:fluid-adjacent :falling-block :under-feet}}
   :fetch {:doc "get the dirt or cobblestone a pillar needs (jobs.lib.fetch): true, a set of kinds or a map of limits; false waits :need" :spec fetch/option? :default true}
   :pillar? {:doc "fell a log out of reach of the ground from a pillar (blocks placed, then taken back); false: such a tree is left" :spec boolean? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(defn log-allowed?
  "Whether the job may dig the log at pos (one warn per job when refused)."
  [c pos]
  (gate/allowed? c :fell-tree.declined "fell-tree" :dig pos {:except (:for-plan (:args c)) :own-plans-ok? (false? (:spare-own-builds (:args c)))}))

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
  (if (<= (u/eye-dist (u/self-pos c) pos) dig-reach)
    :there
    (let [foot (assoc pos :y (:y (:base (ctx/mem c))))
          w (await (near/go-near! c foot 2 {:zone-tolls true}))]
      (if (= :blocked w)
        (await (near/go-near! c foot 3 {:zone-tolls true}))
        w))))

(defn log-dig-args
  "The jobs.blocks.dig args for the log at pos: no tool needed (an axe is held when carried), the drop left on the
  ground, the dig hazards of :accept taken."
  [c pos]
  (merge (select-keys (:args c) [:for-plan :ignore-zones? :accept])
         {:pos pos :collect false :need-drop false}))

(defn outcome
  "What one round of the dig child (r, its result res, the reason its check waits with) means for the tree: :ok
  (dug, gone, or still under way), :refused, :unreachable, :cannot, or the failed dig's status as a keyword."
  [r res waits]
  (case r
    :continue :ok
    :declined (if (#{:not-allowed :hazard} (:reason waits)) :refused :unreachable)
    (case (:reason res)
      (:dug :already-clear) :ok
      (:cannot :fluid) :cannot
      (keyword (or (:status res) (:reason res))))))

(defn ^:async dig-log!
  "Dig the log l (one blocks.dig child round), walking in reach first. Commits the replant debt before the base log
  is dug (write-ahead; withdrawn when the log is still standing after the round). Resolves to :ok, :partial (the walk
  made progress but is not in reach yet; call again), :high (the log is out of reach from the ground: a pillar) or a non-ok outcome for the round to count as a failure
  (:unreachable, :cannot, :refused and :out-of-reach mean the tree cannot be dug from here)."
  [c l]
  (let [pos (:pos l)
        w (await (approach! c pos))]
    (case w
      :blocked :blocked
      :partial :partial
      (cond
        (not (log-allowed? c pos)) :refused
        (and (:pillar? (:args c)) (> (u/eye-dist (u/self-pos c) pos) dig-reach)) :high
        :else
        (let [base? (= pos (:base (ctx/mem c)))
              wrote? (and base? (record-debt! c))
              args (log-dig-args c pos)
              r (await (ctx/call-child c :dig 'jobs.blocks.dig args))
              res (ctx/child-result c :dig)
              waits (when (= :declined r) (blocks/child-wait c :dig 'jobs.blocks.dig args))]
          (when (and wrote? (some-> (u/seen-name (:primitives c) pos) log-name?))
            (ctx/forget-where! c replant-kind #(= pos (:pos %))))
          (outcome r res waits))))))

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

;; ------------------------------------------------------------------ the pillar

(def pillar-items pl/default-items)

(defn cell-pos [[x y z]] {:x x :y y :z z})

(defn log-cell [l] (let [{:keys [x y z]} (:pos l)] [x y z]))

(defn pillar-plan
  "The approach plan (jobs.lib.access.approach/plan) for the highest logs: every log, then without the lowest one at a
  time until the planner finds a stand or a pillar (a zone refusal ends the tries)."
  [c logs]
  (let [in (assoc (access/rules-input c {:except (:for-plan (:args c)) :own-plans-ok? true}) :reach dig-reach)
        top-down (vec (sort-by #(- (:y (:pos %))) logs))]
    (loop [n (count top-down)]
      (let [r (approach/plan (assoc in :targets (set (map log-cell (take n top-down)))))]
        (if (and (:reason r) (not= :zone (:reason r)) (> n 1))
          (recur (dec n))
          r)))))

(defn set-pillar! [c m] (ctx/update-mem! c assoc :pillar m))

(defn pillar-failed!
  "Take back what was built, then count the tree unreachable."
  [c]
  (ctx/update-mem! c update :pillar assoc :phase :clean :failed true)
  :again)

(defn blocks-carried
  "How many of the pillar items (dirt, cobblestone) are carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (reduce + (map #(get have % 0) pillar-items))))

(defn pillar-problem
  "The wait {:reason :need :any-of :count} while a planned pillar is not begun and fewer blocks than its height are carried."
  [c]
  (let [{:keys [phase plan]} (:pillar (ctx/mem c))]
    (when (and (= :walk phase) (:height plan) (< (blocks-carried c) (:height plan)))
      {:reason :need :any-of pillar-items :count (:height plan)})))

(defn ^:async start-pillar!
  "The next log is out of reach from the ground: plan a stand or a pillar for the highest logs and remember it."
  [c logs]
  (let [r (pillar-plan c logs)]
    (cond
      (:stand r) (set-pillar! c {:phase :walk :plan {:stand (first (:stand r))}})
      (:pillar r) (set-pillar! c {:phase :walk :plan (:pillar r)})
      :else (do (ctx/warn-once! c [:pillar-refused] :fell-tree.declined
                                (if (= :zone (:reason r))
                                  {:reason :refused :why :pillar :text "a zone or plan refuses a pillar by the tree"}
                                  {:reason (:reason r) :why :pillar :text (str "no way to reach the high logs: " (name (:reason r)))}))
                (mark-unreachable! c)))
    :again))

(defn ^:async pillar-walk!
  [c {:keys [plan]}]
  (let [r (await (ctx/call-child c :pwalk 'jobs.movement.go-to {:pos (cell-pos (or (:base plan) (:stand plan))) :range 0 :escalate false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))
        res (ctx/child-result c :pwalk)]
    (cond
      (= :continue r) :continue
      (and (= :done r) (:arrived res)) (do (ctx/update-mem! c assoc-in [:pillar :phase] (if (:height plan) :build :dig)) :again)
      :else (pillar-failed! c))))

(defn ^:async pillar-build!
  [c {:keys [plan]}]
  (let [r (await (ctx/call-child c :pillar 'jobs.access.pillar {:height (:height plan) :ignore-zones? (:ignore-zones? (:args c))}))]
    (cond
      (= :continue r) :continue
      (and (= :done r) (= :done (:status (ctx/child-result c :pillar)))) (do (ctx/update-mem! c assoc-in [:pillar :phase] :dig) :again)
      :else (pillar-failed! c))))

(defn ^:async pillar-dig!
  "Centre on the stand cell the plan counted reach from, then dig the highest log in reach; none in reach: take the
  pillar back."
  [c {:keys [plan]} logs]
  (let [[sx _ sz] (:stand plan)
        _ (await (walk/centre! c (+ sx 0.5) (+ sz 0.5)))
        here (u/self-pos c)
        in-reach (filter #(<= (u/eye-dist here (:pos %)) dig-reach) logs)
        l (first (sort-by #(- (:y (:pos %))) in-reach))]
    (if-not l
      (do (ctx/update-mem! c update :pillar #(cond-> (assoc % :phase :clean) (not (:dug %)) (assoc :failed true))) :again)
      (let [r (await (dig-log! c l))]
        (if (= :ok r)
          (do (ctx/update-mem! c assoc-in [:pillar :dug] true) (u/progress! c) :again)
          (pillar-failed! c))))))

(defn open-scaffold?
  "Whether this job's scaffold ledger entries are still open (a pillar left by a cut or a restart)."
  [c]
  (boolean (some #(ledger/of-instance? (:id c) %) (ledger/open-entries (ctx/view c)))))

(defn ^:async pillar-clean!
  "Take the pillar back (jobs.access.cleanup child for this job's blocks); then the tree is unreachable when the pillar
  failed. A cleanup that ends with the ledger still open is a failed round (tried again, tree_blocked after a few)."
  [c {:keys [failed]}]
  (let [r (await (ctx/call-child c :cleanup 'jobs.access.cleanup {:job (:id c)}))]
    (cond
      (= :continue r) :continue
      (open-scaffold? c) (u/fail! c :tree_blocked "cannot take the pillar back")
      :else (do (ctx/update-mem! c dissoc :pillar)
                (when failed (mark-unreachable! c))
                :again))))

(defn ^:async pillar-round!
  "One round of the pillar: walk to its base (or stand), build it, dig from it, take it back."
  [c logs]
  (let [m (:pillar (ctx/mem c))]
    (case (:phase m)
      :walk (await (pillar-walk! c m))
      :build (await (pillar-build! c m))
      :dig (await (pillar-dig! c m logs))
      :clean (await (pillar-clean! c m)))))

(defn walk-failed!
  "Book a walk result of :blocked or :partial against the chosen tree: :again once the tree is given up, :continue
  while the walk (its child) waits."
  [c r]
  (let [partials (inc (:partials (ctx/mem c) 0))]
    (if (or (= :blocked r) (>= partials max-partials))
      (do (mark-unreachable! c) :again)
      (do (ctx/update-mem! c assoc :partials partials) :continue))))

(defn ^:async step
  "One piece of the felling: :again, :continue (a child waits on the world, or nothing is in sight), :done or a stop."
  [c]
  (let [{:keys [species radius]} (:args c)
        _ (when (and (not (:column (ctx/mem c))) (not (first (candidates c radius species))))
            (await (look/survey! c :cell)))
        _ (when (and (not (:pillar (ctx/mem c))) (open-scaffold? c))
            (set-pillar! c {:phase :clean}))
        chosen (or (:column (ctx/mem c)) (await (choose-tree! c radius species)))]
    (cond
      (= :searching chosen) :again

      (and (not chosen) (seq (:unreachable (ctx/mem c))))
      (do (ctx/emit! c :tree_blocked :warn {:text "no reachable tree"})
          :done)

      (not chosen) :continue

      (:pillar (ctx/mem c))
      (or (await (fetch/fetch! c 'jobs.forestry.fell-tree pillar-problem))
          (if (pillar-problem c) :continue (await (pillar-round! c (tree-logs c radius)))))

      :else
      (let [logs (tree-logs c radius)]
        (if (empty? logs)
          (do (ctx/result! c {:base (:base (ctx/mem c))})
              :done)
          (let [_ (await (watch/watch! c {:before-dig (:pos (first logs))}))
                r (await (dig-log! c (first logs)))]
            (case r
              :ok (do (ctx/update-mem! c assoc :partials 0)
                      (u/progress! c)
                      :again)
              :high (await (start-pillar! c logs))
              (:partial :blocked) (walk-failed! c r)
              (:unreachable :cannot :out-of-reach :refused) (do (mark-unreachable! c) :again)
              (u/fail! c :tree_blocked (str "cannot dig the tree: " (name r))))))))))

(def max-steps "Pieces of work of one call before it gives the round back with :continue." 400)

(defn ^:async round
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue)))))

(defn check
  "A tree is chosen, or every candidate was unreachable (the round warns and
  finishes), or a tree is in sight. Else it waits with reason :no-tree (and :radius, :species when given)."
  [c]
  (let [m (ctx/mem c)
        {:keys [radius species]} (:args c)]
    (if-let [w (pillar-problem c)]
      (fetch/check c 'jobs.forestry.fell-tree w)
      (or (boolean (or (:column m)
                     (seq (:unreachable m))
                     (first (candidates c radius species))
                     (not (look/surveyed? c :cell))))
        (ctx/wait c (cond-> {:reason :no-tree :radius radius} species (assoc :species species)))))))
