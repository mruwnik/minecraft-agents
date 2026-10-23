(ns jobs.lib.walk.edges
  "The steps the walker failed at, per body: an edge (from cell, move, to cell) with its k failures of n tries, and the
  same counts pooled over every to cell of that move out of the from cell. Every plan prices them (tolls, the planner's
  options.tolls.edges), so later walks, later calls too, take another way out. A record lasts while the cells round its
  step read the same to the body; one that changed is dropped."
  (:require [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.cost :as cost]
            [jobs.lib.util :as u]))

(def max-records "Records one body keeps, the oldest dropped." 64)

(def fail-s "Seconds a failed try costs: the walker's no-progress window (20 game ticks a second)." (/ (:no-progress-ticks executor/policy) 20))

(defonce memories (js/WeakMap.))

(defn records [p] (or (.get memories p) (let [a (atom {})] (.set memories p a) a)))

(defn cell [{:keys [x y z]}] [x y z])

(defn edge-at
  "[from move to] of the step at index i of steps, a run of steps in one cell (a bend) counting as the first of them; nil
  when no step before it is in another cell."
  [steps i]
  (let [to (cell (nth steps i))
        j (loop [j i] (if (and (pos? j) (= to (cell (nth steps (dec j))))) (recur (dec j)) j))]
    (when (pos? j)
      [(cell (nth steps (dec j))) (:move (nth steps j)) to])))

(defn cells-round
  "The cells a step from from to to reads: the box over both columns from the floor to above the head."
  [[fx fy fz] [tx ty tz]]
  (vec (for [x (range (min fx tx) (inc (max fx tx)))
             z (range (min fz tz) (inc (max fz tz)))
             y (range (dec (min fy ty)) (+ 3 (max fy ty)))]
         [x y z])))

(defn reading
  "What the body knows of cells, one entry each: name and state, :unknown, or nil when not loaded."
  [p cells]
  (mapv (fn [[x y z]] (when-let [b (u/sensed p {:x x :y y :z z})] (if (true? (.-unknown b)) :unknown [(.-name b) (.-stateId b)]))) cells))

(defn note!
  "Adds a try of the edge [from move to] to its record and its pool's ([from move nil]), a failure when failed?. A
  failure makes both records when missing; a passed try counts only where one is kept."
  [p [from move to] failed?]
  (swap! (records p)
         (fn [rs]
           (reduce (fn [rs [k cells]]
                     (let [r (get rs k)]
                       (cond
                         r (assoc rs k (cond-> (update r :n inc) failed? (update :k inc)))
                         failed? (let [rs (if (>= (count rs) max-records) (dissoc rs (key (apply min-key (comp :t val) rs))) rs)]
                                   (assoc rs k {:k 1 :n 1 :t (js/Date.now) :cells cells :read (reading p cells)}))
                         :else rs)))
                   rs
                   [[[from move to] (cells-round from to)] [[from move nil] (cells-round from from)]]))))

(defn note-walk!
  "The tries of a walk of steps that got to step reached (the executor's index) and ended done: every edge before reached
  passed, the edge into the step it was stuck on failed."
  [p steps reached {:keys [status step]}]
  (let [end (if (= :arrived status) (count steps) (min reached (count steps)))]
    (doseq [i (range 1 end)]
      (when-let [e (edge-at steps i)] (note! p e false)))
    (when (and (= :stuck status) (some? step) (< step (count steps)))
      (when-let [e (edge-at steps step)] (note! p e true)))))

(defn kept
  "The records of p whose cells read the same, the others dropped."
  [p]
  (swap! (records p) (fn [rs] (into {} (filter (fn [[_ {:keys [cells read]}]] (= read (reading p cells)))) rs))))

(def move-ids (zipmap executor/move-names (range)))

(defn tolls
  "[edges rows] for the records of p, nil when none: edges the planner's options.tolls.edges (from cell-key -> #js [move
  to-key seconds ...], to-key -1 for a pool), each priced cost/failing-step-s; rows [from move to seconds] each, in order
  (a kept search's key)."
  [p]
  (when-let [rs (seq (kept p))]
    (let [m (js/Map.)
          rows (mapv (fn [[[from move to] {:keys [k n]}]] [from move to (cost/failing-step-s k n fail-s)]) (sort-by key rs))]
      (doseq [[[fx fy fz] move to secs] rows]
        (let [fk (planner/cell-key fx fy fz)
              ^js row (or (.get m fk) (let [a #js []] (.set m fk a) a))]
          (.push row (move-ids move) (if to (apply planner/cell-key to) -1) secs)))
      [m rows])))
