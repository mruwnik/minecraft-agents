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
  remember! store it."
  (:require [engine.ctx :as ctx]
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
