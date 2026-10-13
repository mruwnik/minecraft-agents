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
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-cornered :as cornered]
            [jobs.survival.retreat-home :as retreat-home]))

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
           (keep (fn [{:keys [cell by fails]}]
                   (when-let [o (read p cell)]
                     (assoc o :by by :fails (or fails 0) :dist (distance self cell)))))
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

;; ------------------------------------------------------------------ shutting a door in a flight (retreat, night)

(def door-reach
  "Farthest (blocks, feet to the cell's middle) an open door may be for the flight to shut it: within a hand's reach."
  4)

(defn door-key [{:keys [x y z]}] [x y z])

(defn lower-half
  "The cell of a door's lower half (a gate's or trapdoor's own cell) for an openable block b at cell."
  [b cell]
  (if (= "upper" (:half (click/props-of b))) (update cell :y dec) cell))

(defn door-to-shut
  "The nearest door, gate or trapdoor a hand shuts that stands open within door-reach of the body, is nearer the
  threat at threat-pos than the body is (it lies between them, or beyond the body towards the threat), is not the
  one the body stands in, and has not been clicked this flight (:doors-clicked): {:cell :name}, or nil."
  [c threat-pos]
  (let [p (:primitives c)
        self (u/self-pos c)
        {fx :x fy :y fz :z} (sh/feet p)
        r (js/Math.ceil door-reach)
        clicked (:doors-clicked (ctx/mem c) #{})
        mid retreat-home/mid]
    (->> (for [x (range (- fx r) (+ fx r 1)) y (range (dec fy) (+ fy 3)) z (range (- fz r) (+ fz r 1))]
           {:x x :y y :z z})
         (keep (fn [cell]
                 (when-let [b (dig-cells/open-openable p cell)]
                   (when (= :openable (click/kind-of (.-name b)))
                     (let [low (lower-half b cell)]
                       {:cell low :name (.-name b) :half (:half (click/props-of b))})))))
         distinct
         (remove #(clicked (door-key (:cell %))))
         (remove #(click/standing-in? self (:cell %) (:half %)))
         (filter #(<= (u/dist self (mid (:cell %))) door-reach))
         (filter #(< (u/dist threat-pos (mid (:cell %))) (u/dist threat-pos self)))
         (sort-by #(u/dist self (mid (:cell %))))
         first)))

(defn ^:async shut-door!
  "Shut door (door-to-shut) with one click; every door is clicked at most once a flight, so a door that will not stay
  shut does not hold the flight. :again: the next step sees whether the danger is still there."
  [c {:keys [cell name]}]
  (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell))
  (let [r (await (dig-cells/shut-open! c cell))]
    (when (= :shut r)
      (ctx/emit! c :retreat.door-shut :info {:cell (door-key cell) :text (str "shut the " name " at " (door-key cell)
                                                                             " on the hostile")})))
  :again)


(defn home-door
  "The door of the home room (jobs.lib.reach/room) that stands open and has not been clicked this flight, or nil."
  [c room]
  (let [clicked (:doors-clicked (ctx/mem c) #{})]
    (->> (:doors room)
         (filter #(dig-cells/open-openable (:primitives c) (:cell %)))
         (remove #(clicked (door-key (:cell %))))
         (sort-by #(u/dist (u/self-pos c) (retreat-home/mid (:cell %))))
         first)))

(defn ^:async shut-home-door!
  "Shut an open door of the home room the body is in: from where it stands within door-reach, else a step to the room
  cell inside it first, as also when it stands in the door's own cell (a step that gets no nearer gives the door up for
  the flight). :again."
  [c {:keys [cell inside]}]
  (let [b (dig-cells/open-openable (:primitives c) cell)
        in-door? (click/standing-in? (u/self-pos c) cell (:half (click/props-of b)))]
    (if (and (not in-door?) (<= (u/dist (u/self-pos c) (retreat-home/mid cell)) door-reach))
      (await (shut-door! c {:cell cell :name (.-name b)}))
      (do (when-not (await (cornered/step! c :home inside 0))
            (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell)))
          :again))))
