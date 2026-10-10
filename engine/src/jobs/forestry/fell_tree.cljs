(ns jobs.forestry.fell-tree
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.watch :as watch]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.pillar :as pl]
            [jobs.lib.access :as access]
            [jobs.lib.access.approach :as approach]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.loose :as loose]
            [jobs.lib.reach :as reach]
            [jobs.lib.trip :as trip]
            [jobs.lib.trees :refer [axe-worth-fetching? axe-fetch-limits scan-logs tree-near trees-near tree-at logs-at unreachable-set debts replant-kind
                                          replant-policy default-radius max-partials dig-reach log-name? log-names]]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter :as sh]
            [jobs.lib.sense :as sense]
            [jobs.lib.targets :as targets]))

(def doc
  "Fell the nearest tree: a log column with leaves near its top. One call fells the whole tree, lowest log first; it
  yields (:continue) only while a child waits on the world.
  The base log's :forestry/replant debt is written before it is dug.
  It ends on the ground: from the canopy it walks down (go-to) beside the base, warning fell-tree.stranded when it cannot.
  A lower log of the column seen later becomes the base (and its debt moves down): the sapling goes on the ground.
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
  scaffold still open). The pillar is built of dirt, cobblestone and the logs cut so far (collected first); with too
  few of those carried it fetches the height in blocks (jobs.lib.fetch,
  unless :fetch is false); else, or when the fetch failed, it waits (check) with :reason :need (:any-of the two,
  :count the height). A tree whose pillar a zone, claim or footprint refuses (warn fell-tree.declined, :reason
  :refused; other refusals warn with their :reason), or that has no ground or stand to reach it from, counts as unreachable,
  and so does one whose pillar digs no log. A cleanup that leaves the scaffold open is a failed round (tree_blocked
  after a few). :pillar? false leaves such a tree.
  Once a tree is chosen the rest of it is a comfort section (ctx/atomic), the pillar's build, dig and clean an urgent
  one: reflexes at or below those levels wait for them, up to their caps.
  Before the first log of a tree big enough for an axe to pay (jobs.lib.trees/axe-worth-fetching?) with none carried it
  fetches one (jobs.items.get-tool, a seen chest or crafted) unless :fetch is false or lacks :tool; a failed fetch is
  warned (fetch.failed) and the tree is chopped by hand.
  Before the first log, a carried axe that is the last of its kind and worn gets a spare (jobs.lib.trip/tool-ready!,
  unless :fetch is false or lacks :tool).
  Each log is dug by a jobs.blocks.dig child. That child holds the best carried axe and leaves the drop on the
  ground (jobs.forestry.harvest-wood collects it).
  Waits (check) with :reason :no-tree when no tree is in sight, after one look around from where it stands.
  Zones: a tree whose base log is in another owner's zone or claim, or in a plan's footprint (but :for-plan's
  own), is not a candidate. A later log that turns out refused makes the tree count as unreachable. The job warns
  fell-tree.declined once, with :reason :refused (or :no-zones when no zone list was read). :ignore-zones? true
  skips the check.
  Result: {:base pos :logs n :axe why} of the tree it felled, when it did: n logs dug; :axe is :carried, :skipped-fetch-off,
  :skipped-small-tree or :none-found (fetch failed: chopped by hand).")

(a/defargs args
  {:species {:doc "log species such as \"oak\"; any when nil" :spec a/name? :default nil}
   :radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :at {:doc "{:x :y :z} of a base log: fell that one column, wherever the body is (the radius and species are not used), instead of the nearest tree" :spec ::a/pos :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :spare-own-builds {:doc "a log in a plan this body made is not felled; false: it may be" :spec boolean? :default true}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :lava-adjacent :falling-block" :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}
   :fetch {:doc "get an axe when none is carried, and the dirt or cobblestone a pillar needs (jobs.lib.fetch): true, a set of kinds or a map of limits; false chops by hand and waits :need for a pillar" :spec fetch/option? :default true}
   :pillar? {:doc "fell a log out of reach of the ground from a pillar (blocks placed, then taken back); false: such a tree is left" :spec boolean? :default true}
   :avoid {:doc "columns [x z] of trees to leave alone (ones an earlier run found cut off)" :spec (a/coll-of (a/pred "an [x z] column" vector?)) :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(defn gate-opts [c]
  {:except (:for-plan (:args c)) :own-plans-ok? (false? (:spare-own-builds (:args c)))})

(defn log-allowed?
  "Whether the job may dig the log at pos (one warn per job when refused)."
  [c pos]
  (gate/allowed? c :fell-tree.declined "fell-tree" :dig pos (gate-opts c)))

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

(defn lower-base
  "The position of the lowest of logs when it lies under the committed base of mem (lower logs of the trunk seen late),
  else nil."
  [mem logs]
  (let [low (:pos (first logs))
        base (:base mem)]
    (when (and low base (< (:y low) (:y base)) (= (:x low) (:x base)) (= (:z low) (:z base)))
      low)))

(defn rebase!
  "Move the committed base (and its replant debt) down to pos: the sapling goes on the ground, not on a stump."
  [c pos]
  (ctx/forget-where! c replant-kind #(= (:base (ctx/mem c)) (:pos %)))
  (ctx/update-mem! c assoc :base pos))

(defn on-canopy?
  "Whether the body stands more than a block over the base: up in the leaves."
  [c]
  (let [base (:base (ctx/mem c))]
    (boolean (and base (> (js/Math.floor (:y (u/self-pos c))) (inc (:y base)))))))

(defn ^:async come-down!
  "Walk to the ground beside the base (one go-to call, slot :down) when the body stands up in the canopy. :continue while
  go-to waits on the world, else nil; a walk that does not arrive warns fell-tree.stranded."
  [c]
  (when (on-canopy? c)
    (let [{:keys [x y z]} (:base (ctx/mem c))
          r (await (child/run! c :down 'jobs.movement.go-to {:pos [x y z] :range 2 :escalate false}))]
      (cond
        (= :continue r) :continue
        (not (:arrived (ctx/child-result c :down)))
        (ctx/warn-once! c :stranded :fell-tree.stranded {:text "felled the tree but could not come down from the canopy"})))))

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
          (when (= :dug (:reason res)) (ctx/update-mem! c update :dug (fnil inc 0)))
          (when (and wrote? (some-> (u/seen-name (:primitives c) pos) log-name?))
            (ctx/forget-where! c replant-kind #(= pos (:pos %))))
          (outcome r res waits))))))

(def approach-range
  "Blocks from a tree's base the search for the tree to fell counts as reaching it: approach!'s looser range."
  3)

(a/defargs settings
  {::drop-radius {:default 6 :doc "Blocks round the base the cut logs are picked up from before a pillar is built." :spec (a/num-in 0 nil)}})

(defn own-spared?
  "Whether the dig at pos is refused only by a plan this body made: a build it spares, not a refusal to report."
  [in pos]
  (let [v (access/may? in :dig pos)]
    (and (= :footprint (:reason v)) (contains? (:own-plans in) (:plan v)))))

(defn candidates
  "The trees the job may fell, nearest in a line first (lazily): the one at :at (none once marked unreachable), else
  those of species within radius not marked unreachable; a tree whose base log the zones refuse is left out."
  [c radius species]
  (let [p (:primitives c)
        excluded (into (unreachable-set (ctx/mem c)) (map vec) (:avoid (:args c)))]
    (if-let [at (:at (:args c))]
      (when-let [t (tree-at p at)]
        (when-not (or (excluded [(:x at) (:z at)]) (not (log-allowed? c (:base t)))) [t]))
      (let [in (access/rules-input c (gate-opts c))]
        (filter #(and (not (own-spared? in (:base %))) (log-allowed? c (:base %)))
                (trees-near p radius species excluded))))))

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

(defn night-sheltered
  "The kind of shelter the body is shut in at night (jobs.lib.shelter/night-sheltered), else nil; never under a reflex job."
  [c]
  (when-not (:reflex c)
    (sh/night-sheltered (:primitives c) (:data (ctx/latest c :shelter)))))

(defn mark-all-unreachable!
  "Remember the columns of trees as unreachable."
  [c trees]
  (ctx/update-mem! c update :unreachable (fnil into []) (map (fn [t] [(get-in t [:column :x]) (get-in t [:column :z])]) trees)))

(defn ^:async choose-tree!
  "Choose the tree to fell among the candidates (at most targets/max-targets): the one the body walks to soonest
  (targets/nearest!), committed (commit-tree!). :searching while that search goes on; nil when no candidate is in
  sight, or when the search proved every one out of reach (all marked unreachable); the nearest in a line when it ran
  out of nodes, or when the body is shut in (its walk digs or builds out, which the search does not count): the
  nearest in a line (the walk decides)."
  [c radius species]
  (let [ts (vec (take targets/max-targets (candidates c radius species)))]
    (when (seq ts)
      (let [a (await (targets/nearest! c (mapv :base ts) approach-range {:tag :fell-tree}))]
        (case (:status a)
          :found (commit-tree! c (nth ts (:index a)))
          :searching :searching
          (if (and (:proved a) (not (reach/enclosed? (:primitives c))))
            (do (when-not (night-sheltered c) (mark-all-unreachable! c ts)) nil)
            (commit-tree! c (first ts))))))))

(defn mark-unreachable!
  "Remember the chosen column as unreachable and forget the choice; why (the walk's give-up reason) is kept for the end."
  ([c] (mark-unreachable! c nil))
  ([c why]
   (ctx/update-mem! c (fn [m] (-> m
                             (cond-> why (assoc :unreachable-why why))
                             (update :unreachable (fnil conj []) [(get-in m [:column :x]) (get-in m [:column :z])])
                             (dissoc :column :species :base)
                             (assoc :partials 0))))))

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

(defn pillar-blocks
  "The blocks the pillar is built from: dirt and cobblestone, then every kind of log carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (into pillar-items (filter #(and (pos? (get have % 0)) (log-name? %)) (sort (keys have))))))

(defn logs-carried
  "How many logs are carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (reduce + (map #(get have % 0) (remove (set pillar-items) (pillar-blocks c))))))

(defn blocks-carried
  "How many pillar blocks (dirt, cobblestone, logs) are carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (reduce + (map #(get have % 0) (pillar-blocks c)))))

(defn pillar-problem
  "The wait {:reason :need :any-of :count} while a planned pillar is not begun and fewer blocks than its height are
  carried; :count is the dirt or cobblestone wanted beside the logs carried."
  [c]
  (let [{:keys [phase plan]} (:pillar (ctx/mem c))]
    (when (and (= :walk phase) (:height plan) (< (blocks-carried c) (:height plan)))
      {:reason :need :any-of pillar-items :count (- (:height plan) (logs-carried c))})))

(defn ^:async start-pillar!
  "The next log is out of reach from the ground: plan a stand or a pillar for the highest logs and remember it. A
  refusal that read unseen floor cells is decided again after a look at them (jobs.lib.sense/decide!)."
  [c logs]
  (let [r (await (sense/decide! c ::pillar #(pillar-plan c logs) {:retry? :reason}))]
    (cond
      (:stand r) (set-pillar! c {:phase :collect :plan {:stand (first (:stand r))}})
      (:pillar r) (set-pillar! c {:phase :collect :plan (:pillar r)})
      :else (do (ctx/warn-once! c [:pillar-refused] :fell-tree.declined
                                (if (= :zone (:reason r))
                                  {:reason :refused :why :pillar :text "a zone or plan refuses a pillar by the tree"}
                                  {:reason (:reason r) :why :pillar :text (str "no way to reach the high logs: " (name (:reason r)))}))
                (mark-unreachable! c)))
    :again))

(defn own-log-drops
  "The ids of the logs lying within :drop-radius of the base that jobs.lib.loose lets the body take (not another
  player's throw, not in a zone that refuses taking)."
  [c base]
  (let [r (settings/get settings ::drop-radius)]
    (->> (loose/candidates (:primitives c) (ctx/view c) (:world (:engine c)) {:radius (+ r (u/dist base (u/self-pos c))) :always log-names} (:root c))
         (filter #(and (log-name? (some-> (.-item %) .-name)) (<= (u/dist base (u/pos-of (.-pos %))) r)))
         (mapv #(.-id %)))))

(defn ^:async pillar-collect!
  "Pick up the logs cut so far (they are the pillar's blocks when no dirt is carried): a collect-drops child for the
  ones lying round the base that the body may take, one call. Finding none is no failure."
  [c]
  (let [base (:base (ctx/mem c))
        ids (own-log-drops c base)
        r (if (seq ids)
            (await (child/run! c :pcollect 'jobs.forestry.collect-drops
                               {:radius (settings/get settings ::drop-radius) :ids ids :near base :search 0 :want 0}))
            :done)]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c assoc-in [:pillar :phase] :walk) :again))))

(defn ^:async pillar-walk!
  [c {:keys [plan]}]
  (let [r (await (child/call-walk c :pwalk {:pos (cell-pos (or (:base plan) (:stand plan))) :range 0 :escalate false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))
        res (ctx/child-result c :pwalk)]
    (cond
      (= :continue r) :continue
      (and (= :done r) (:arrived res)) (do (ctx/update-mem! c assoc-in [:pillar :phase] (if (:height plan) :build :dig)) :again)
      :else (pillar-failed! c))))

(defn ^:async pillar-build!
  [c {:keys [plan]}]
  (let [r (await (ctx/call-child c :pillar 'jobs.access.pillar {:height (:height plan) :items (pillar-blocks c) :ignore-zones? (:ignore-zones? (:args c))}))]
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

(defn ^:async pillar-phase!
  "One phase of the pillar: walk to its base (or stand), build it, dig from it, take it back."
  [c logs]
  (let [m (:pillar (ctx/mem c))]
    (case (:phase m)
      :collect (await (pillar-collect! c))
      :walk (await (pillar-walk! c m))
      :build (await (pillar-build! c m))
      :dig (await (pillar-dig! c m logs))
      :clean (await (pillar-clean! c m)))))

(def climb-phases #{:build :dig :clean})

(defn climbing? [c] (contains? climb-phases (:phase (:pillar (ctx/mem c)))))

(defn ^:async pillar-round!
  "The pillar's next phase. Build, dig and clean run as one urgent section (ctx/atomic :pillar): a night or low-food
  reflex waits for the pillar to be taken back, up to its cap."
  [c radius]
  (if (climbing? c)
    (await (ctx/atomic c :urgent :pillar
                       #(pace/steps! c (fn ^:async climb []
                                         (let [r (await (pillar-phase! c (tree-logs c radius)))]
                                           (if (and (= :again r) (not (climbing? c))) :climbed r))))))
    (await (pillar-phase! c (tree-logs c radius)))))

(defn walk-failed!
  "Book a walk result of :blocked or :partial against the chosen tree: :again once the tree is given up, :continue
  while the walk (its child) waits."
  [c r]
  (let [partials (inc (:partials (ctx/mem c) 0))]
    (if (or (= :blocked r) (>= partials max-partials))
      (do (mark-unreachable! c (some-> (ctx/child-result c :walk) :why name)) :again)
      (do (ctx/update-mem! c assoc :partials partials) :continue))))

(defn axe-carried? [c]
  (boolean (some #(str/ends-with? (:name %) "_axe") (u/inventory (:primitives c)))))

(defn axe-problem
  "The wait {:reason :no-tool :kind \"axe\"} before the first log of a tree an axe pays for, while none is carried
  (and while its fetch runs); nil otherwise."
  [c logs]
  (when (and (zero? (:dug (ctx/mem c) 0))
             (not (axe-carried? c))
             (or (:fetching (ctx/mem c)) (axe-worth-fetching? (count logs))))
    {:reason :no-tool :kind "axe" :limits (axe-fetch-limits)}))

(defn axe-status
  "Why the tree was felled with or without an axe: :carried, :skipped-fetch-off, :skipped-small-tree or :none-found."
  [c]
  (cond
    (axe-carried? c) :carried
    (not (contains? (:what (fetch/opts c 'jobs.forestry.fell-tree)) :tool)) :skipped-fetch-off
    (not (axe-worth-fetching? (:first-logs (ctx/mem c) 0))) :skipped-small-tree
    :else :none-found))

(defn ^:async step
  "One piece of the felling: :again, :continue (a child waits on the world, or nothing is in sight), :done or a stop."
  [c]
  (let [{:keys [species radius]} (:args c)
        _ (when (and (not (:column (ctx/mem c))) (not (first (candidates c radius species))))
            (await (look/survey! c :cell)))
        _ (when (and (not (:pillar (ctx/mem c))) (open-scaffold? c))
            (set-pillar! c {:phase :clean}))
        fresh? (not (:column (ctx/mem c)))
        chosen (or (:column (ctx/mem c)) (await (choose-tree! c radius species)))]
    (cond
      (= :searching chosen) :again

      (and fresh? chosen (trip/food-short? c))
      (do (ctx/emit! c :tree.hungry :warn {:text "felling stopped: hungry, no food carried and get-food found none"})
          (ctx/result! c {:status :stopped :reason :hungry :logs (:dug (ctx/mem c) 0)})
          :done)

      (and (not chosen) (seq (:unreachable (ctx/mem c))))
      (let [why (:unreachable-why (ctx/mem c))]
        (ctx/emit! c :tree_blocked :warn (cond-> {:text "no reachable tree"} why (assoc :why why)))
        (ctx/result! c (cond-> {:unreachable (count (:unreachable (ctx/mem c))) :columns (vec (:unreachable (ctx/mem c)))} why (assoc :why why)))
        :done)

      (not chosen) :continue

      ;; the tree is chosen: the round felling it runs as one section
      fresh? :again

      (:pillar (ctx/mem c))
      (or (await (fetch/fetch! c 'jobs.forestry.fell-tree pillar-problem))
          (if (pillar-problem c) :continue (let [r (await (pillar-round! c radius))] (if (= :climbed r) :again r))))

      :else
      (let [logs (await (sense/decide! c ::logs #(tree-logs c radius) {:retry? empty?}))]
        (if (empty? logs)
          (if (= :continue (await (come-down! c)))
            :continue
            (do (ctx/result! c {:base (:base (ctx/mem c)) :logs (:dug (ctx/mem c) 0) :axe (axe-status c)})
                :done))
          (if-let [f (do (when (and (zero? (:dug (ctx/mem c) 0)) (not (:first-logs (ctx/mem c))))
                           (ctx/update-mem! c assoc :first-logs (count logs)))
                         (or (await (fetch/fetch! c 'jobs.forestry.fell-tree #(axe-problem % logs)))
                             (when (and (zero? (:dug (ctx/mem c) 0)) (axe-carried? c))
                               (await (trip/tool-ready! c (:name (first logs)))))))]
            f
            (let [_ (when-let [low (lower-base (ctx/mem c) logs)] (rebase! c low))
                  _ (await (watch/watch! c {:before-dig (:pos (first logs))}))
                  r (await (dig-log! c (first logs)))]
              (case r
                :ok (do (ctx/update-mem! c assoc :partials 0)
                        (u/progress! c)
                        :again)
                :high (await (start-pillar! c logs))
                (:partial :blocked) (walk-failed! c r)
                (:unreachable :cannot :out-of-reach :refused) (do (mark-unreachable! c) :again)
                (u/fail! c :tree_blocked (str "cannot dig the tree: " (name r)))))))))))

(def max-steps "Pieces of work of one call before it gives the round back with :continue." 400)

(defn ^:async round
  "Steps until a tree is chosen, then that tree as one comfort section (ctx/atomic :tree): hunger, a full bag or
  tidying waits for the tree, up to its cap. A tree given up ends its section before the next is chosen."
  [c]
  (let [n (atom 0)
        step! #(if (< (swap! n inc) max-steps) (step c) :continue)
        chosen? #(some? (:column (ctx/mem c)))
        steps-while (fn [want] (pace/steps! c (fn ^:async part []
                                                (let [r (await (step!))]
                                                  (if (and (= :again r) (not= want (chosen?))) :switch r)))))]
    (loop []
      (let [r (if (chosen?)
                (await (ctx/atomic c :comfort :tree #(steps-while true)))
                (await (steps-while false)))]
        (if (= :switch r) (recur) r)))))

(defn check
  "A tree is chosen, or every candidate was unreachable (the round warns and
  finishes), or a tree is in sight. Else it waits with reason :no-tree (and :radius, :species when given).
  At night in a shelter it waits :night-sheltered: the trees are outside, and a search from inside proves none reachable."
  [c]
  (let [m (ctx/mem c)
        {:keys [radius species]} (:args c)]
    (cond
      (night-sheltered c) (ctx/wait c {:reason :night-sheltered :shelter (night-sheltered c)})
      (pillar-problem c) (fetch/check c 'jobs.forestry.fell-tree (pillar-problem c))
      :else (or (boolean (or (:column m)
                             (seq (:unreachable m))
                             (first (candidates c radius species))
                             (not (look/surveyed? c :cell))))
                (ctx/wait c (cond-> {:reason :no-tree :radius radius} species (assoc :species species)))))))
