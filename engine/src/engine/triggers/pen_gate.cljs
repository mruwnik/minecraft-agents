(ns engine.triggers.pen-gate
  "The pen-gate trigger, and the pure reading the shut-gate job shares with it.

  Where the gates are comes from the plans: a gate is a cell of an :active plan whose want is a fence gate. The index
  {cell plan-id} is built once per plan change (engine.world/derived), never per tick.

  The trigger holds when a planned gate within :radius (8) blocks of the body stands open, the body is farther than
  :min-dist (2) from it, and that has been so for :open-s (4) seconds. A job that opened a gate on purpose
  (leading animals through, standing in the doorway) is therefore left alone: it is at the gate, or passing it, and the
  clock restarts whenever the body is near again. A gate the job gave up on (a gate-gave-up entry in memory, written by
  jobs.animals.shut-gate) is not looked at again for :quiet-s (10 min).

  A gate cell with a :gate-held entry younger than :held-s (30) is not looked at; a job that leads animals through
  writes it before opening the gate and drops it after shutting."
  (:require [engine.jobs.apiary :as apiary]
            [engine.jobs.pen :as pen]
            [engine.memory :as mem]
            [engine.world :as world]
            [clojure.string :as str]))

(def defaults {:radius 8 :min-dist 2 :open-s 4 :quiet-s 600 :held-s 30})

(def gap-ms
  "A clock whose last look is older than this was not watched in between: it starts again."
  2000)

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
  "{[x y z] plan-id} of the cells of the :active plans (answers, see engine.world/answer) that want a fence gate;
  a cell several plans want belongs to the first by id. A broken or unactive plan has none."
  [answers]
  (into {}
        (for [{:keys [id status cells broken] :as a} (reverse (sort-by :id (remove nil? answers)))
              :when (and (not broken) (= :active status))
              {:keys [pos want]} cells
              :when (gate-want? want)]
          [(vec pos) id])))

(defn gate-index
  "gate-cells of the world's plans, rebuilt only when a plan or blueprint changed; nil for a nil world."
  [kn]
  (world/derived kn :pen-gates gate-cells))

;; ------------------------------------------------------------------ reading the world

(defn open-gate?
  "Whether block b is a fence gate standing open: the reading engine.jobs.pen counts an open gate by."
  [b]
  (boolean (and b (pen/gate? b) (pen/on? b "open"))))

(defn open-cells
  "The cells ([x y z]) among cells that hold an open gate now; an unloaded cell is not open."
  [block-at cells]
  (filterv (fn [[x y z]] (open-gate? (block-at {:x x :y y :z z}))) cells))

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

;; ------------------------------------------------------------------ the clock

(defn track
  "The clock {:at now :since {cell t}} after a look at now that found cells open and far: each keeps the time it was
  first seen so; one not seen is forgotten, and a clock last looked at more than gap-ms ago starts from nothing."
  [clock now cells]
  (let [fresh (if (and clock (<= (- now (:at clock)) gap-ms)) (:since clock) {})]
    {:at now :since (into {} (map (fn [c] [c (get fresh c now)])) cells)}))

(defn settled
  "The cells of the clock that have been seen open for at least open-ms at now."
  [clock now open-ms]
  (vec (keep (fn [[cell t]] (when (>= (- now t) open-ms) cell)) (:since clock))))

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

(defonce clocks (js/WeakMap.))

(defn holds?
  "Whether a planned gate near the body has stood open, with the body away, for :open-s seconds."
  [p view args kn]
  (let [{:keys [open-s quiet-s held-s] :as args} (merge defaults args)
        gates (some-> (gate-index kn) keys)]
    (if (empty? gates)
      false
      (let [self (let [pos (.-pos (.self p))] {:x (.-x pos) :y (.-y pos) :z (.-z pos)})
            quiet (quiet-cells view (* 1000 quiet-s))
            held (held-cells view (* 1000 held-s))
            open (open-cells (apiary/block-at-fn p) (remove (some-fn quiet held) gates))
            clock (track (.get clocks kn) (:now view) (candidates self open args))]
        (.set clocks kn clock)
        (boolean (seq (settled clock (:now view) (* 1000 open-s))))))))

(def trigger
  "Holds when holds? says so; args :radius :min-dist :open-s :quiet-s :held-s. The job it starts shuts the gate."
  {:name :pen-gate
   :when (fn [world view args kn]
           (if kn (holds? world view args kn) false))
   :job '(jobs.animals.shut-gate)
   :args defaults
   :persistence :cooldown
   :cooldown-s 5})
