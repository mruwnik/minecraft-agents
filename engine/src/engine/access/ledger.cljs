(ns engine.access.ledger
  "The scaffold ledger: every temporary block this body placed (pillars, later bridges and stair plugs), so a cleanup
  can take back exactly its own blocks, even after a cut, a discarded job or a restart.

  It lives in body memory (not job memory: a discarded job's memory is gone, its blocks are not), as the single
  entry of kind :scaffold (cap 1, forever) whose data is {:entries [entry ...]}. An entry is
    {:cell [x y z] :item \"dirt\" :before \"air\" :job \"j4\" :purpose :pillar :state :intent|:placed}
  with at most one entry per cell. A job writes the entry as an :intent before the placing act (act saves memory
  before it calls the primitive) and confirms it once it sees the item in the cell. An :intent left by a cut or a
  restart is decided by looking at the cell (decide, reconcile): it holds the item -> placed; anything else -> not
  placed, dropped; not loaded -> undecided, kept. Every entry is open until a cleanup drops it.

  The lifecycle functions are pure over the entry vector; open-entries reads it from a memory view, write! and
  remember! store it.

  Cleanup (jobs.access.cleanup) marks an entry :removing before its dig and settles every entry it works on from the
  cell (settle-entry): the item there -> kept (:placed); a :removing cell now air -> removed; anything else -> dropped,
  not ours any more; not loaded -> kept. The owner of an entry is the root instance of its :job (j4 for
  j4/pillar); it is live while body memory holds that instance's job memory (listed, or a running reflex).
  Cells a cleanup could not take are held (kind :scaffold-held, 10 minutes) so the standing offer does not repeat
  them every tick."
  (:require [clojure.string :as str]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.memory :as mem]))

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

(defn settle-entry
  "[what entry] for an entry, from its cell now: [:keep e] (the item is there, e :placed; or not loaded, e as it was),
  [:removed e] (a :removing cell that is air) or [:dropped e] (anything else, e with :found, the block there)."
  [{:keys [cell item state] :as e} block-at]
  (let [n (block-at cell)]
    (cond
      (nil? n) [:keep e]
      (= item n) [:keep (assoc e :state :placed)]
      (and (= :removing state) (rules/air n)) [:removed e]
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

(defn select
  "The entries of l a cleanup with :job job works on: nil, those whose owner is not live and that are not held; :all,
  every one; an instance id, those of that instance and its children."
  [l view job]
  (cond
    (nil? job) (let [held (held-cells view)]
                 (filterv #(and (not (owner-live? view %)) (not (held (:cell %)))) l))
    (= :all job) (vec l)
    :else (filterv #(of-instance? job %) l)))

(defn offered
  "The entries a cleanup with :job job would work on now: selected and loaded."
  [view block-at job]
  (filterv #(some? (block-at (:cell %))) (select (open-entries view) view job)))

(defn hold!
  "Hold cells in body-memory store."
  [store cells]
  (mem/write! store held-kind {:cells (vec cells)} held-policy))

(defn remember-held!
  "Hold cells from a job's round."
  [c cells]
  (ctx/remember! c held-kind {:cells (vec cells)} held-policy))
