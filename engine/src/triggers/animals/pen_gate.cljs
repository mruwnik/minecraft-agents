(ns triggers.animals.pen-gate
  "The pen-gate trigger, and the pure reading the shut-gate job shares with it.

  A gate is a cell of a plan whose want is a fence gate. The index {cell plan-id} is built once per plan change
  (jobs.lib.world-files/derived), never per tick.

  The trigger holds when a planned gate was last seen open (perception's memory, never through a wall), within :radius
  (8) of the body and farther than :min-dist (2). It keeps no state: a gate a job opened on purpose is hidden by memory
  entries, three kinds:
    :gate-gave-up  written by jobs.animals.shut-gate; the cell is skipped for :quiet-s (10 min)
    :gate-held     written by a job leading animals through, dropped after it shuts the gate; skipped while
                   younger than :held-s (30)
    :opened        written by a walk (jobs.lib.pass) that opened it; the shut-doors job owns that gate"
  (:require [jobs.animals.pen :as pen]
            [jobs.lib.click :as click]
            [jobs.lib.look :as look]
            [engine.memory :as mem]
            [jobs.lib.world-files :as world]
            [clojure.string :as str]))

(def defaults {:radius 8 :min-dist 2 :quiet-s 600 :held-s 30})

;; ------------------------------------------------------------------ the index

(defn want-names
  "The block names a want can be: its name, those of every choice of an :any, the :block of a block with state."
  [want]
  (cond
    (string? want) [want]
    (vector? want) (mapcat want-names (rest want))
    (map? want) (some-> (:block want) vector)
    :else nil))

(defn gate-want? [want]
  (some #(str/ends-with? % "_fence_gate") (want-names want)))

(defn gate-cells
  "{[x y z] plan-id} of the cells of the plans (answers, see jobs.lib.world-files/answer) that want a fence gate;
  a cell several plans want belongs to the first by id. A broken plan has none."
  [answers]
  (into {}
        (for [{:keys [id cells broken] :as a} (reverse (sort-by :id (remove nil? answers)))
              :when (not broken)
              {:keys [pos want]} cells
              :when (gate-want? want)]
          [(vec pos) id])))

(defn gate-index
  "gate-cells of the world's plans, rebuilt only when a plan or blueprint changed; nil for a nil world."
  [kn]
  (world/derived kn :pen-gates gate-cells))

;; ------------------------------------------------------------------ reading the world

(defn open-gate?
  "Whether block b is a fence gate standing open: the reading jobs.animals.pen counts an open gate by."
  [b]
  (boolean (and b (pen/gate? b) (pen/on? b "open"))))

(defn open-cells
  "The cells ([x y z]) among cells that hold an open gate now; an unloaded cell is not open."
  [block-at cells]
  (filterv (fn [[x y z]] (open-gate? (block-at {:x x :y y :z z}))) cells))

(defn seen-open-cells
  "The cells ([x y z]) among cells the body last saw as a fence gate standing open (never seen: not open)."
  [p cells]
  (filterv (fn [[x y z]]
             (let [{:keys [name properties]} (look/seen-block p {:x x :y y :z z})]
               (and name (str/ends-with? name "_fence_gate") (click/reached? :open properties))))
           cells))

(defn distance
  "Blocks from the body at self ({:x :y :z}, feet) to the middle of the cell."
  [self [x y z]]
  (js/Math.hypot (- (:x self) (+ x 0.5)) (- (:y self) y) (- (:z self) (+ z 0.5))))

(defn candidates
  "The cells farther than :min-dist and within :radius of the body."
  [self cells {:keys [radius min-dist]}]
  (filterv #(let [d (distance self %)] (and (> d min-dist) (<= d radius))) cells))

(defn standing-in?
  "Whether the body's feet are inside the cell."
  [self [x y z]]
  (= [x y z] (mapv #(js/Math.floor %) [(:x self) (:y self) (:z self)])))

;; ------------------------------------------------------------------ the memory

(defn quiet-cells
  "The gates the job gave up on less than quiet-ms ago: a set of [x y z]."
  [view quiet-ms]
  (into #{} (keep (fn [{:keys [t data]}] (when (< (- (:now view) t) quiet-ms) (some-> (:cell data) vec))))
        (mem/entries view :gate-gave-up)))

;; ------------------------------------------------------------------ the trigger

(defn held-cells
  "The gates a job holds open on purpose, by a :gate-held entry younger than held-ms: a set of [x y z]."
  [view held-ms]
  (into #{} (keep (fn [{:keys [t data]}] (when (< (- (:now view) t) held-ms) (some-> (:cell data) vec))))
        (mem/entries view :gate-held)))

(defn opened-cells
  "The gates a walk opened (:opened entries): a set of [x y z]."
  [view]
  (into #{} (keep (fn [{:keys [data]}] (some-> (:cell data) ((juxt :x :y :z)))))
        (mem/entries view :opened)))

(defn holds?
  "Whether a planned gate near the body was last seen open, with the body away, and no memory entry hides it."
  [p view args kn]
  (let [{:keys [quiet-s held-s] :as args} (merge defaults args)
        gates (some-> (gate-index kn) keys)]
    (if (empty? gates)
      false
      (let [pos (.-pos (.self p))
            self {:x (.-x pos) :y (.-y pos) :z (.-z pos)}
            hidden (into (opened-cells view) (concat (quiet-cells view (* 1000 quiet-s)) (held-cells view (* 1000 held-s))))
            open (seen-open-cells p (remove hidden (candidates self gates args)))]
        (boolean (seq open))))))

(defn pen-gate
  "Holds when holds? says so; args :radius :min-dist :quiet-s :held-s. The job it starts shuts the gate."
  [world view args kn]
  (if kn (holds? world view args kn) false))
