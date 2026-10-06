(ns engine.triggers.door-left
  "The door-left trigger, and the reading jobs.maintenance.shut-doors shares with it.

  A walk (engine.path.pass) writes an :opened entry for each door, gate or trapdoor it opens and drops it when it shuts
  the block behind the body, a moment later. An entry that is older than :open-s (10 s) for a block that still stands
  open is one a walk left: its round was cut or cancelled between the open and the shut, it ended in the doorway, or the
  shutting click did nothing. The trigger holds while such a block lies within :radius (16) of the body and the body is
  out of its column; its job walks back and shuts it (jobs tidy up after themselves). A block shut by anything else
  ends the entry's part in this: it is no longer open."
  (:require [engine.access.click :as click]
            [engine.memory :as mem]
            [engine.path.pass :as pass]))

(def defaults {:radius 16 :open-s 10})

(defn self-pos [p]
  (let [pos (.-pos (.self p))] {:x (.-x pos) :y (.-y pos) :z (.-z pos)}))

(defn feet-cell [{:keys [x y z]}] {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)})

(defn distance
  "Blocks from the body's feet at self to the middle of cell."
  [self {:keys [x y z]}]
  (js/Math.hypot (- (:x self) (+ x 0.5)) (- (:y self) y) (- (:z self) (+ z 0.5))))

(defn open-block
  "{:cell :column} of the block at cell when it is an openable block standing open, else nil (shut, gone, unloaded)."
  [p cell]
  (let [b (.blockAt p (clj->js cell))
        props (click/props-of b)]
    (when (and b (= :openable (click/kind-of (.-name b))) (click/reached? :open props))
      {:cell cell :column (pass/column-of cell props)})))

(defn left-open
  "The blocks the walker opened to shut again (:opened entries in view, not those a :leave-open walk left on purpose) that
  stand open, whose entry is at least open-ms old, within radius of the body: [{:cell :column :by :dist}] nearest first,
  one per cell."
  [p view {:keys [radius]} open-ms]
  (let [self (self-pos p)]
    (->> (mem/entries view :opened)
         (filter (fn [{:keys [t]}] (>= (- (:now view) t) open-ms)))
         (map :data)
         (remove #(false? (:shut? %)))
         (keep (fn [{:keys [cell by]}]
                 (when-let [o (open-block p cell)]
                   (assoc o :by by :dist (distance self cell)))))
         (filter #(<= (:dist %) radius))
         (sort-by :dist)
         (reduce (fn [acc o] (if (some #(= (:cell o) (:cell %)) acc) acc (conj acc o))) []))))

(defn holds?
  "Whether a block a walk left open has stood so for :open-s, within :radius, with the body out of its column."
  [p view args]
  (let [{:keys [open-s] :as args} (merge defaults args)
        here (feet-cell (self-pos p))]
    (boolean (some #(not (pass/in-column? (:column %) here))
                   (left-open p view args (* 1000 open-s))))))

(defn door-left
  "Holds when holds? says so; args :radius :open-s. The job it starts shuts the blocks."
  [world view args _kn]
  (holds? world view args))
