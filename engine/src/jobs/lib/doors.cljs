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
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.pass :as pass]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

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
  For the job, once it stands beside the block."
  [p cell]
  (let [b (.blockAt p (clj->js cell))
        props (click/props-of b)]
    (when (and b (= :openable (click/kind-of (.-name b))) (click/reached? :open props))
      {:cell cell :column (pass/column-of cell props)})))

(defn seen-open-block
  "open-block as the body last saw the block (perception's memory); a block never seen is not open. For the trigger: what
  the body could not see is not read. A cell that touches the body (it stands in the gate) is felt, never in memory."
  [p cell]
  (let [{:keys [name properties]} (let [m (look/seen-block p cell)
                                         b (when-not (:name m) (u/seen-block p cell))]
                                    (if b {:name (.-name b) :properties (click/props-of b)} m))]
    (when (and name (= :openable (click/kind-of name)) (click/reached? :open properties))
      {:cell cell :column (pass/column-of cell properties)})))

(defn owner-live?
  "Whether the root job of the walk id `by` (\"j4\" for \"j4/walk\") still has job memory in the view."
  [view by]
  (some? (mem/latest view (mem/job-kind (first (str/split (str by) #"/"))))))

(defn recorded
  "The :opened entries' data in view at least open-ms old (times 1 + its :fails: the retry pace), minus those a :leave-open walk of a job that still lives left on purpose."
  [view open-ms]
  (->> (mem/entries view :opened)
       (map :data)
       (filter (fn [{:keys [t fails]}] (>= (- (:now view) t) (* open-ms (inc (or fails 0))))))
       (remove #(and (false? (:shut? %)) (owner-live? view (:by %))))))

(defn left-open
  "The blocks the walker opened to shut again (:opened entries in view, not those a :leave-open walk of a job that still
  lives left on purpose) that
  stand open, whose entry is at least open-ms old, within radius of the body: [{:cell :column :by :dist}] nearest first,
  one per cell. read is open-block or seen-open-block."
  ([p view args open-ms] (left-open p view args open-ms open-block))
  ([p view {:keys [radius]} open-ms read]
    (let [self (self-pos p)]
      (->> (recorded view open-ms)
           (keep (fn [{:keys [cell by fails t]}]
                   (when-let [o (read p cell)]
                     (assoc o :by by :t t :fails (or fails 0) :dist (distance self cell)))))
           (filter #(<= (:dist %) radius))
           (sort-by :dist)
           (reduce (fn [acc o] (if (some #(= (:cell o) (:cell %)) acc) acc (conj acc o))) [])))))

(defn unseen-recorded
  "The cells of the :opened entries within radius that the body has never seen, as {:x :y :z} nearest first: whether they
  stand open is not known."
  [p view {:keys [radius]}]
  (let [self (self-pos p)]
    (->> (recorded view 0)
         (map :cell)
         (filter #(<= (distance self %) radius))
         (filter #(look/unseen? p [%]))
         (distinct)
         (sort-by #(distance self %)))))

(defn holds?
  "Whether a block a walk left open has stood so for :open-s, within :radius, with the body out of its column."
  [p view args]
  (let [{:keys [open-s] :as args} (merge defaults args)
        here (feet-cell (self-pos p))]
    (boolean (some #(not (pass/in-column? (:column %) here))
                   (left-open p view args (* 1000 open-s) seen-open-block)))))

(defn unrecorded-open
  "The wooden doors seen standing open within radius of the body that no :opened entry names (a door left open after a
  failure whose entry was dropped, by a night dig-in or by hand) and that own? (a fn of the cell map {:x :y :z}) calls the
  body's own, read as open-block now, like left-open's blocks: [{:cell :column :by nil :fails 0 :dist}] nearest first.
  Gates and trapdoors are never taken (a pen's gate stands open on purpose)."
  [p view {:keys [radius own?]}]
  (let [self (self-pos p)
        recorded (set (map (comp :cell :data) (mem/entries view :opened)))]
    (->> (look/seen-blocks p {:match #(re-find #"_door$" %) :radius radius :live? true})
         (map :pos)
         (remove recorded)
         (filter own?)
         (keep (fn [cell]
                 (when-let [o (open-block p cell)]
                   (assoc o :by nil :fails 0 :dist (distance self cell)))))
         (filter #(<= (:dist %) radius))
         (sort-by :dist)
         (reduce (fn [acc o] (if (some #(= (:column o) (:column %)) acc) acc (conj acc o))) []))))
