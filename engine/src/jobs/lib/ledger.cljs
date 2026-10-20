(ns jobs.lib.ledger
  "The scaffold ledger: every temporary block this body placed (pillars, tunnel torches, retreat plugs and the
  like), so a cleanup can take back exactly its own blocks, even after a cut, a discarded job or a restart.

  Storage: body memory, not job memory (a discarded job's memory is gone, its blocks are not). One entry of
  kind :scaffold (cap 1, forever) with data {:entries [entry ...]}. An entry is
    {:cell [x y z] :item \"dirt\" :before \"air\" :job \"j4\" :purpose :pillar :state :intent|:placed|:removing}
  with at most one entry per cell. Every entry stays open until a cleanup drops it.

  Placing: a job writes the entry as :intent before the placing act (act saves memory before it calls the
  primitive) and confirms it (:placed) once it sees the item in the cell. An :intent left by a cut or a
  restart is decided from the cell (decide, reconcile):
  - it holds the item: placed
  - anything else: not placed, dropped
  - not loaded: undecided, kept

  Cleanup (jobs.access.cleanup): marks an entry :removing before its dig, then settles each entry it works on
  from the cell (settle-entry):
  - the item is there (dirt grown to grass_block, podzol or mycelium counts): kept as :placed
  - a :removing cell that is now air: removed
  - anything else: dropped, it is not ours any more
  - not loaded: kept
  Cells a cleanup could not take are held (kind :scaffold-held, 10 minutes) so the standing offer does not
  repeat them every tick.

  Owner: the root instance of an entry's :job (j4 for j4/pillar). It is live while body memory holds that
  instance's job memory (listed, or a running reflex).

  The lifecycle functions are pure over the entry vector. open-entries reads it from a memory view, write!
  and remember! store it."
  (:require [jobs.lib.block-sets :as bs]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-below {:default 4 :doc "Blocks below the body a scaffold may lie to be walked to; a deeper one (a cave, a pit) is held :far-below by cleanup, and the scaffold-left trigger ignores it." :spec (a/int-in 0 nil)}})

(defn max-below [] (settings/get settings ::max-below))

(def kind :scaffold)

(def policy {:cap 1 :ttl :forever})

(defn entry-at [l cell] (first (filter #(= cell (:cell %)) l)))

(defn drop-cell [l cell] (filterv #(not= cell (:cell %)) l))

(defn intend
  "l with e as an :intent, replacing any entry at its cell."
  [l e]
  (conj (drop-cell l (:cell e)) (assoc e :state :intent)))

(defn confirm
  "l with the entry at cell marked :placed."
  [l cell]
  (mapv #(if (= cell (:cell %)) (assoc % :state :placed) %) l))

(defn decide
  "What became of an :intent entry, from the cell's block now (block-at: [x y z] -> name, nil when not loaded):
  :placed, :not-placed or :unknown."
  [{:keys [cell item]} block-at]
  (let [n (block-at cell)]
    (cond
      (nil? n) :unknown
      (= item n) :placed
      :else :not-placed)))

(defn reconcile
  "l with every :intent decided against the world: placed ones confirmed, not-placed ones dropped, unknown ones kept.
  :placed entries are left alone (a block gone since is the cleanup's business)."
  [l block-at]
  (into []
        (keep (fn [e]
                (if (not= :intent (:state e))
                  e
                  (case (decide e block-at)
                    :placed (assoc e :state :placed)
                    :not-placed nil
                    :unknown e))))
        l))

(defn cells [l] (set (map :cell l)))

(defn of-job [l job purpose] (filterv #(and (= job (:job %)) (= purpose (:purpose %))) l))

(defn open-entries
  "The ledger in a body-memory view {:data :now} (ctx/view in a job): every entry not yet cleaned up, oldest first."
  [view]
  (vec (:entries (:data (mem/latest view kind)))))

(defn write!
  "Store l in body-memory store."
  [store l]
  (mem/write! store kind {:entries (vec l)} policy))

(defn remember!
  "Store l in body memory from a job's round."
  [c l]
  (ctx/remember! c kind {:entries (vec l)} policy))

;; ------------------------------------------------------------------ cleanup

(defn begin-removal
  "l with the entry at cell marked :removing (written before the dig)."
  [l cell]
  (mapv #(if (= cell (:cell %)) (assoc % :state :removing) %) l))

(defn same-block?
  "Whether block n is the item an entry recorded: dirt left in the open turns to grass, podzol or mycelium, and still digs as dirt."
  [item n]
  (or (= item n) (and (= "dirt" item) (contains? #{"grass_block" "podzol" "mycelium"} n))))

(defn settle-entry
  "[what entry] for an entry, from its cell now: [:keep e] (the item is there, e :placed; or not loaded, e as it was),
  [:removed e] (a :removing cell that is air) or [:dropped e] (anything else, e with :found, the block there)."
  [{:keys [cell item state] :as e} block-at]
  (let [n (block-at cell)]
    (cond
      (nil? n) [:keep e]
      (same-block? item n) [:keep (assoc e :state :placed)]
      (and (= :removing state) (bs/air n)) [:removed e]
      :else [:dropped (assoc e :found n)])))

(defn settle
  "{:ledger :removed :dropped}: l with every entry for which pick? holds settled from the world (settle-entry); the
  removed and dropped entries are out of :ledger."
  [l pick? block-at]
  (reduce (fn [acc e]
            (if-not (pick? e)
              (update acc :ledger conj e)
              (let [[what e'] (settle-entry e block-at)]
                (update acc (if (= :keep what) :ledger what) conj e'))))
          {:ledger [] :removed [] :dropped []}
          l))

(defn owner
  "The root instance id of an entry's :job: \"j4\" for \"j4\" and \"j4/pillar\"."
  [e]
  (first (str/split (str (:job e)) #"/")))

(defn owner-live?
  "Whether the instance that owns e still has job memory in the view (listed, or a running reflex)."
  [view e]
  (some? (mem/latest view (mem/job-kind (owner e)))))

(defn of-instance?
  "Whether e was placed by instance id or one of its children."
  [id e]
  (let [j (str (:job e))]
    (or (= id j) (str/starts-with? j (str id "/")))))

(def held-kind :scaffold-held)

(def held-policy {:cap 1 :ttl 600000})

(defn held-cells [view] (set (:cells (:data (mem/latest view held-kind)))))

(defn near-of
  "The entries of l whose cell lies within the :radius of the :pos of near ({:pos {:x :y :z} :radius n}); all of l when near is nil."
  [l near]
  (if-not near
    l
    (let [{:keys [x y z]} (:pos near)]
      (filterv (fn [{[cx cy cz] :cell}] (<= (js/Math.hypot (- cx x) (- cy y) (- cz z)) (:radius near))) l))))

(defn select
  "The entries of l a cleanup with :job job works on: nil, those whose owner is not live and that are not held (a dig-in's :shelter blocks stay: the dig-in's leave takes them back); :all,
  every one; an instance id, those of that instance and its children. near narrows them to a pos and radius (near-of)."
  ([l view job] (select l view job nil))
  ([l view job near]
   (near-of (cond
              (nil? job) (let [held (held-cells view)]
                           (filterv #(and (not= :shelter (:purpose %)) (not (owner-live? view %)) (not (held (:cell %)))) l))
              (= :all job) (vec l)
              :else (filterv #(of-instance? job %) l))
            near)))

(defn offered
  "The entries a cleanup with :job job (and :near near) would work on now: selected and loaded."
  ([view block-at job] (offered view block-at job nil))
  ([view block-at job near]
   (filterv #(some? (block-at (:cell %))) (select (open-entries view) view job near))))

(defn hold!
  "Hold cells in body-memory store."
  [store cells]
  (mem/write! store held-kind {:cells (vec cells)} held-policy))

(defn remember-held!
  "Hold cells from a job's round."
  [c cells]
  (ctx/remember! c held-kind {:cells (vec cells)} held-policy))

(defn ways-of
  "The set of floors of the stairs this body cut earlier in its current dimension (memory :stair-way)."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))]
    (set (mapcat #(when (= dim (:dim (:data %))) (:floors (:data %))) (ctx/entries c :stair-way)))))

(defn stair-stands
  "The set of feet cells [x y z] of the steps of the stairs this body cut (ways-of)."
  [c]
  (set (map (fn [[x y z]] [x (inc y) z]) (ways-of c))))

(def tunnel-way-policy {:cap 30 :ttl :forever})

(defn record-tunnel!
  "Remember a level tunnel this body cut (memory :tunnel-way: dim, origin feet cell [x y z], delta [dx dz], steps), so a
  later stay-level run does not take its air for a cave and go-to's own way up links it."
  [c origin delta steps]
  (when (pos? steps)
    (ctx/remember! c :tunnel-way {:dim (.-dimension (.self (:primitives c))) :origin (vec origin) :delta (vec delta) :steps steps}
                   tunnel-way-policy)))

(defn tunnel-stands
  "The set of feet cells [x y z] of the level tunnels this body cut in its current dimension (memory :tunnel-way)."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))]
    (set (for [{{[x y z] :origin [dx dz] :delta :keys [steps] :as data} :data} (ctx/entries c :tunnel-way)
               :when (= dim (:dim data))
               k (range (inc steps))]
           [(+ x (* k dx)) y (+ z (* k dz))]))))

(defn own-cells
  "The cells of the passages this body cut itself in its dimension: feet, head and the cell over the head (a step cuts it) of each step of its stairs (memory :stair-way) and of its tunnels (:tunnel-way), as [x y z]."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))
        here? #(= dim (:dim (:data %)))
        stairs (for [e (ctx/entries c :stair-way) :when (here? e)
                     [x y z] (:floors (:data e)) dy [1 2 3]]
                 [x (+ y dy) z])
        tunnels (for [e (ctx/entries c :tunnel-way) :when (here? e)
                      :let [{[x y z] :origin [dx dz] :delta :keys [steps]} (:data e)]
                      k (range (inc steps)) dy [0 1]]
                  [(+ x (* k dx)) (+ y dy) (+ z (* k dz))])]
    (set (concat stairs tunnels))))

(defn near-of-args
  "The :near the entries are narrowed to: the arg, else the body's position and :within, else (unowned entries, :job
  nil) the body's position and 32, so a saved trigger register whose job spec has no :within stays bounded, else nil."
  [c {:keys [near within job]}]
  (or near
      (when-let [r (or within (when (nil? job) 32))] {:pos (u/self-pos c) :radius r})))
