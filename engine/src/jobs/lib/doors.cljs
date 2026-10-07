(ns jobs.lib.doors
  "Reading of doors, gates and trapdoors a walk left open, shared by the door-left trigger and jobs.maintenance.shut-doors.

  A walk (jobs.lib.pass) writes an :opened entry for each door, gate or trapdoor it opens and drops it when it shuts
  the block behind the body, a moment later. An entry that is older than :open-s (10 s) for a block that still stands
  open is one a walk left: its round was cut or cancelled between the open and the shut, it ended in the doorway, or the
  shutting click did nothing. A block shut by anything else ends the entry's part in this: it is no longer open. The
  trigger reads whether a block stands open as the body last saw it (perception's memory), never through a wall."
  (:require [jobs.lib.click :as click]
            [jobs.lib.look :as look]
            [clojure.string :as str]
            [engine.memory :as mem]
            [jobs.lib.pass :as pass]))

(def defaults {:radius 16 :open-s 10})

(defn self-pos [p]
  (let [pos (.-pos (.self p))] {:x (.-x pos) :y (.-y pos) :z (.-z pos)}))

(defn feet-cell [{:keys [x y z]}] {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)})

(defn distance
  "Blocks from the body's feet at self to the middle of cell."
  [self {:keys [x y z]}]
  (js/Math.hypot (- (:x self) (+ x 0.5)) (- (:y self) y) (- (:z self) (+ z 0.5))))

(defn open-block
  "{:cell :column} of the block at cell when it is an openable block standing open now, else nil (shut, gone, unloaded).
  For the job, which stands beside the block."
  [p cell]
  (let [b (.blockAt p (clj->js cell))
        props (click/props-of b)]
    (when (and b (= :openable (click/kind-of (.-name b))) (click/reached? :open props))
      {:cell cell :column (pass/column-of cell props)})))

(defn seen-open-block
  "open-block as the body last saw the block (perception's memory); a block never seen is not open. For the trigger: what
  the body could not see is not read."
  [p cell]
  (let [{:keys [name properties]} (look/seen-block p cell)]
    (when (and name (= :openable (click/kind-of name)) (click/reached? :open properties))
      {:cell cell :column (pass/column-of cell properties)})))

(defn owner-live?
  "Whether the root job of the walk id `by` (\"j4\" for \"j4/walk\") still has job memory in the view."
  [view by]
  (some? (mem/latest view (mem/job-kind (first (str/split (str by) #"/"))))))

(defn left-open
  "The blocks the walker opened to shut again (:opened entries in view, not those a :leave-open walk of a job that still
  lives left on purpose) that
  stand open, whose entry is at least open-ms old, within radius of the body: [{:cell :column :by :dist}] nearest first,
  one per cell. read is open-block or seen-open-block."
  ([p view args open-ms] (left-open p view args open-ms open-block))
  ([p view {:keys [radius]} open-ms read]
    (let [self (self-pos p)]
      (->> (mem/entries view :opened)
           (filter (fn [{:keys [t]}] (>= (- (:now view) t) open-ms)))
           (map :data)
           (remove #(and (false? (:shut? %)) (owner-live? view (:by %))))
           (keep (fn [{:keys [cell by]}]
                   (when-let [o (read p cell)]
                     (assoc o :by by :dist (distance self cell)))))
           (filter #(<= (:dist %) radius))
           (sort-by :dist)
           (reduce (fn [acc o] (if (some #(= (:cell o) (:cell %)) acc) acc (conj acc o))) [])))))

(defn holds?
  "Whether a block a walk left open has stood so for :open-s, within :radius, with the body out of its column."
  [p view args]
  (let [{:keys [open-s] :as args} (merge defaults args)
        here (feet-cell (self-pos p))]
    (boolean (some #(not (pass/in-column? (:column %) here))
                   (left-open p view args (* 1000 open-s) seen-open-block)))))
