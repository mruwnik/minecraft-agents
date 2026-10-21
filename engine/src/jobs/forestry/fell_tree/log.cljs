(ns jobs.forestry.fell-tree.log
  "What fell-tree and its pillar way share: the zone rule for a log, the logs of the chosen column, the walk within
  reach of a log and one dig round of it."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.aim :as aim]
            [jobs.lib.child :as child]
            [jobs.lib.gate :as gate]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.places :as places]
            [jobs.lib.ready :as ready]
            [jobs.lib.result :as result]
            [jobs.lib.shelter.inside :as inside]
            [jobs.lib.part-cut :as part-cut]
            [jobs.lib.trees :as trees :refer [scan-logs logs-at debts replant-kind dig-reach log-name?]]
            [jobs.lib.util :as u]))

(defn gate-opts [c]
  {:except (:for-plan (:args c)) :own-plans-ok? (false? (access/spare-own-builds))})

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

(def max-tree-logs "The most logs of one tree the flood reads." 64)

(a/defargs settings
  {::max-spread {:default 6 :doc "Blocks (either axis) from the first seed log's column the flood of a tree's logs may reach: acacia and dark oak branches fit, a log wall touching the tree is cut off." :spec (a/num-in 0 nil)}})
(defn max-spread [] (settings/get settings ::max-spread))

(defn flood-allowed?
  "A predicate on a position: whether the flood may take that log, as the base check would (zones, claims, plan footprints;
  not :ignore-zones?, not own plans with the spare-own-builds setting false). Silent: the refused log is simply no part of the tree."
  [c]
  (let [in (access/rules-input c (gate-opts c))]
    #(not (gate/refused? (access/may? in :dig %)))))

(defn connected-logs
  "The logs of the same species as the seed logs, seen, that touch them (by face, edge or corner) or each other, at most
  max-tree-logs, lowest first, then nearest the first seed: branches and the other columns of a 2x2 trunk. Cells in skip
  (a pillar's scaffold, logs placed beside the trunk) and logs failing allowed? or more than max-spread from the first
  seed's column are no part of the tree."
  [p seeds skip allowed?]
  (let [spread (max-spread)
        {:keys [x z]} (:pos (first seeds))
        item (:name (first seeds))
        key (fn [pos] [(:x pos) (:y pos) (:z pos)])
        near (fn [{:keys [pos]}] (let [{px :x pz :z} pos] (+ (* (- px x) (- px x)) (* (- pz z) (- pz z)))))]
    (loop [found (into {} (map (juxt (comp key :pos) identity)) seeds)
           queue (vec seeds)]
      (if (or (empty? queue) (>= (count found) max-tree-logs))
        (->> (vals found) (sort-by (juxt #(get-in % [:pos :y]) near)) vec)
        (let [{:keys [pos]} (peek queue)
              next (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]
                         :let [q {:x (+ (:x pos) dx) :y (+ (:y pos) dy) :z (+ (:z pos) dz)}]
                         :when (and (not (found (key q))) (not (skip (key q)))
                                (<= (Math/abs (- (:x q) x)) spread) (<= (Math/abs (- (:z q) z)) spread)
                                (= item (u/seen-name p q)) (allowed? q))]
                     {:name item :pos q})
              next (take (- max-tree-logs (count found)) next)]
          (recur (into found (map (juxt (comp key :pos) identity)) next)
                 (into (pop queue) next)))))))

(defn tree-logs
  "The logs of the tree {:column :species}: the logs of its column, read from :at (the tree's, else the call's) up when there is one, else those in
  radius, and the ones still standing of those it had (:logs-seen), and every log joined to them (connected-logs). Lowest
  first. The column alone would lose the branches once its own logs are dug."
  [c radius {:keys [species logs-seen] :as tree}]
  (let [p (:primitives c)
        item (str species "_log")
        column (if-let [at (or (:at tree) (:at (:args c)))]
                 (logs-at p at species)
                 (column-logs p radius tree))
        skip (set (map vec (ledger/cells (ledger/open-entries (ctx/view c)))))
        skip? #(skip [(:x %) (:y %) (:z %)])
        seeds (->> (into (vec column) (keep #(when (= item (u/seen-name p %)) {:name item :pos %})) logs-seen)
                   (remove (comp skip? :pos))
                   distinct vec)]
    (if (seq seeds) (connected-logs p seeds skip (flood-allowed? c)) seeds)))

(defn debt-pos
  "Where the sapling of the log cell pos goes: the cell itself, or the ground cell under it when the log hangs over air
  and ground is seen below."
  [p pos]
  (or (when (part-cut/raised? p pos) (part-cut/ground-cell p pos)) pos))

(defn record-debt!
  "Write the replant debt for the log cell pos of the tree of species, once. The cell of the debt when it wrote one."
  [c pos species]
  (when-let [at (debt-pos (:primitives c) pos)]
    (when-not (some #(= at (:pos %)) (debts c))
      (ctx/remember! c replant-kind {:pos at :species species} {:cap 50 :ttl :forever :durable? true})
      at)))

(defn mark-unreachable!
  "Remember the chosen column as unreachable and forget the choice; why (the walk's give-up reason) is kept for the end."
  ([c] (mark-unreachable! c nil))
  ([c why]
   (ctx/update-mem! c (fn [m] (-> m
                             (cond-> why (assoc :unreachable-why why))
                             (update :unreachable (fnil conj []) [(get-in m [:column :x]) (get-in m [:column :z])])
                             (dissoc :column :species :base :pillar :logs-seen :stand-avoid)
                             (assoc :partials 0))))))

(defn blocked!
  "A log that cannot be dug, or a pillar that cannot be taken back: a failed round (:continue), the ending at the third in a row."
  [c text]
  (if-not (u/count-fail! c)
    :continue
    (ctx/end c :nothing-possible :tree-blocked {:text text :logs (:dug (ctx/mem c) 0)})))

(defn trunk-cell?
  "Whether pos is the base or lies beside it at its height: a cell of a 2x2 trunk (its far columns are hidden from the
  first sight, so each is recorded as it is dug)."
  [base pos]
  (and (= (:y pos) (:y base))
       (<= (Math/abs (- (:x pos) (:x base))) 1)
       (<= (Math/abs (- (:z pos) (:z base))) 1)))

(defn open-scaffold?
  "Whether this job's scaffold ledger entries (its own or a child's) are still open: a pillar left by a cut or a restart.
  Cells a cleanup held (it could not take them back) are not: they stay for the scaffold-left trigger."
  [c]
  (let [view (ctx/view c)
        held (ledger/held-cells view)]
    (boolean (some #(and (ledger/of-instance? (:id c) %) (not (held (:cell %)))) (ledger/open-entries view)))))

(defn ^:async walk-near!
  "Walk until the body's cell is within range of pos with a go-to child (slot :walk, no retry): :there, :partial (go-to
  waits on the world, a leg got nearer, or the body is shut in its shelter and no tree is to blame: call again; such rounds are not counted against the tree) or :blocked (it gave up, or pos is no cell)."
  [c pos range]
  (let [column? (nil? (:y pos))
        cell (if column? pos (:pos (places/parse-pos pos)))]
    (cond
      (nil? cell) :blocked
      (and (not column?) (u/within? (u/self-pos c) cell range)) :there
      :else (let [e (await (child/call-walk* c :walk {:pos cell :range range :doors :shut
                                                      :one-way :closed :see false :spend {:tries 1}
                                                      :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
              (cond
                (= :continue e) (do (when (inside/sheltered-in c) (ctx/update-mem! c assoc :partials 0)) :partial)
                (and (= :done (:kind e)) (:arrived (:data e))) :there
                (result/body-side? e) (do (ctx/update-mem! c assoc :partials 0) :partial)
                :else (do (ctx/update-mem! c assoc :walk-why (some-> (:why (:data e)) name))
                          :blocked))))))

(defn ^:async approach!
  "Get the log at pos within reach to dig: nothing when its centre is within eye reach already, else walk to within 2
  of the column's foot (3 when that has no path: the foot may be the trunk itself). When the cell under the foot is
  unseen the trunk may go on down, and when it is air the log hangs (cut below), so the walk aims at the column's ground
  (go-to's column goal), not at that height: no walk reaches a cell in the air.
  :there, :partial or :blocked."
  [c pos base-y]
  (if (<= (u/eye-dist (u/self-pos c) pos) dig-reach)
    :there
    (let [foot (if (let [under (trees/seen-name (:primitives c) {:x (:x pos) :y (dec base-y) :z (:z pos)})]
                     (or (nil? under) (contains? bs/air under)))
                 (select-keys pos [:x :z])
                 (assoc pos :y base-y))
          w (await (walk-near! c foot 2))]
      (if (= :blocked w)
        (await (walk-near! c foot 3))
        w))))

(defn log-dig-args
  "The jobs.blocks.dig args for the log at pos: no tool needed (an axe is held when carried), the drop left on the
  ground, the dig hazards of :accept taken."
  [c pos]
  (merge (select-keys (:args c) [:for-plan :ignore-zones? :accept])
         {:pos pos :collect false :need-drop false}))

(defn outcome
  "What one round of the dig child (its return e: :continue or its ending) means for the tree: :ok (dug, gone, or still
  under way), :refused, :cannot, :unreachable (a condition that refused the call), or the failed dig's cause as a keyword."
  [e]
  (if (or (= :continue e) (= :done (:kind e)))
    :ok
    (let [k (contract/cause-key e)]
      (cond
        (#{:not-allowed :hazard} k) :refused
        (#{:cannot :fluid} k) :cannot
        (= :condition-failed (:kind e)) :unreachable
        :else (keyword (name k))))))

(defn collect-args
  "args for a collect-drops child of the felling: the drops lie round the tree the body just worked at, so only a threat
  holds the pick-up back, not the dark sky over the roaming a plain collect guards against."
  [c args]
  (assoc args :checks (ready/unforced c {:safe? '(not (threatened))})))

(defn blocking-leaf
  "The leaf cell nearest the eye on the line to the log at pos when the eye does not see the log, nil when it sees it or
  something else than leaves is in the way."
  [c pos]
  (let [at (some->> pos u/cell-vec (aim/first-blocker (aim/seen-kind-at (:primitives c)) (aim/body-eye c)))]
    (when (some-> at u/cell-pos (->> (u/seen-name (:primitives c))) (trees/leaves-name?))
      (u/cell-pos at))))

(defn ^:async dig-log!
  "Dig the log l of the tree at base (one blocks.dig child round), walking in reach first. Commits the replant debt of a trunk log at the base's height
  before it is dug (write-ahead; withdrawn when the log is still standing after the round). Resolves to :ok,
  :partial (the walk made progress but is not in reach yet; call again), :high (the log is out of reach from the ground
  and a pillar is allowed, and the body is not on one: climbing?) or a non-ok outcome for the round to count as a failure (:unreachable, :cannot, :refused and
  :out-of-reach mean the tree cannot be dug from here). A log in reach that leaves hide from the eye has the leaf in the way dug first (as a player cuts in), not a
  walk round the tree to a sight: :ok, the log is met again next round."
  [c l {:keys [base species climbing?]}]
  (let [pos (:pos l)
        w (await (approach! c pos (:y base)))]
    (case w
      :blocked :blocked
      :partial :partial
      (cond
        (not (log-allowed? c pos)) :refused
        (and (ctx/way? c :pillar) (not climbing?) (> (u/eye-dist (u/self-pos c) pos) dig-reach)) :high
        (some->> (blocking-leaf c pos) (log-allowed? c))
        (outcome (await (ctx/call c :dig 'jobs.blocks.dig (log-dig-args c (blocking-leaf c pos)))))
        :else
        (let [wrote? (and (:replant? (:args c)) (trunk-cell? base pos) (record-debt! c pos species))
              e (await (ctx/call c :dig 'jobs.blocks.dig (log-dig-args c pos)))]
          (when (:dug (:data e))
            (ctx/update-mem! c #(-> % (update :dug (fnil inc 0)) (update :drop-ids (fnil into []) (:ids (:data e))))))
          (when (and wrote? (some-> (u/seen-name (:primitives c) pos) log-name?))
            (ctx/forget-where! c replant-kind #(= wrote? (:pos %))))
          (outcome e))))))
