(ns jobs.lib.reach.proofs
  "Walk proofs shared by the mobs of one danger query (one body cell): a search for one mob proves cells dead (no way to
  the body) or alive (a way), and the next mob's search reuses them. See jobs.lib.reach for the searches themselves."
  (:require [jobs.lib.reach :as reach]))

(defn proofs
  "Facts the walk searches of one danger query have proved, shared with the next mob's search. Keyed by one key fn.
  :dead  cells with no way to the body (a closed search saw them all).
  :alive cells with a way to the body (on a found way).
  :reach near-closure's result, set once a search ran out of budget: the set of cells with a way, or :budget."
  [[bx _ bz]]
  #js {:key (reach/keyer bx bz) :dead (js/Set.) :alive (js/Set.) :reach nil})

(defn forward-stepper
  "A fn that expands one cell per call of a search from the mob's cell over forward moves, using proofs: a dead cell is
  not expanded, an alive one counts as reaching the body. It answers nil while the search goes on, then :found (its way
  marked alive), :closed (every cell it saw marked dead) or :budget."
  [kind-at ^js pr [sx sy sz] [tx ty tz]]
  (let [key (.-key pr) dead (.-dead pr) alive (.-alive pr)
        parent (js/Map.)
        open #js []
        k0 (key sx sy sz)
        n (volatile! 0)]
    (.set parent k0 -1)
    (reach/heap-push! open #js [(reach/heuristic sx sy sz tx ty tz) 0 sx sy sz k0])
    (fn []
      (cond
        (zero? (.-length open)) (do (.forEach parent (fn [_ k] (.add dead k))) :closed)
        (>= @n reach/node-budget) :budget
        :else
        (let [top (reach/heap-pop! open)
              g (aget top 1) x (aget top 2) y (aget top 3) z (aget top 4) k (aget top 5)]
          (vswap! n inc)
          (cond
            (or (reach/near-cell? x y z tx ty tz) (.has alive k))
            (do (loop [c k] (when-not (== c -1) (.add alive c) (recur (.get parent c)))) :found)
            (.has dead k) nil
            :else
            (let [nexts (reach/forward kind-at x y z)
                  g' (inc g)]
              (dotimes [i (.-length nexts)]
                (let [c (aget nexts i)
                      cx (aget c 0) cy (aget c 1) cz (aget c 2)
                      ck (key cx cy cz)]
                  (when-not (.has parent ck)
                    (.set parent ck k)
                    (reach/heap-push! open #js [(+ g' (reach/heuristic cx cy cz tx ty tz)) g' cx cy cz ck]))))
              nil)))))))

(defn forward-proving
  "forward-stepper run to its end."
  [kind-at pr mob body]
  (let [step (forward-stepper kind-at pr mob body)]
    (loop [r (step)] (if (nil? r) (recur (step)) r))))

(def near-budget
  "Cells near-closure expands before it gives up."
  reach/node-budget)

(def near-offsets
  "Offsets of the cells near-cell? counts as near: one either way on each axis."
  (vec (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]] [dx dy dz])))

(defn closure-stepper
  "A fn that expands one cell per call of a breadth-first search back from the cells near the body over backward moves.
  It answers nil while the search goes on, then the set of keys of every cell with a way to the body, or :budget when
  more than near-budget cells were expanded. Every cell met is added to pr's alive."
  [kind-at ^js pr [bx by bz]]
  (let [key (.-key pr)
        ^js alive (.-alive pr)
        seen (js/Set.)
        queue #js []
        meet! (fn [x y z c]
                (let [k (key x y z)]
                  (when-not (.has seen k)
                    (.add seen k)
                    (.add alive k)
                    (.push queue c))))
        head (volatile! 0)]
    (doseq [[dx dy dz] near-offsets]
      (let [x (+ bx dx) y (+ by dy) z (+ bz dz)]
        (meet! x y z #js [x y z])))
    (fn []
      (cond
        (== @head (.-length queue)) seen
        (>= @head near-budget) :budget
        :else (let [c (aget queue @head)
                    nexts (reach/backward kind-at (aget c 0) (aget c 1) (aget c 2))]
                (vswap! head inc)
                (dotimes [i (.-length nexts)]
                  (let [n (aget nexts i)]
                    (meet! (aget n 0) (aget n 1) (aget n 2) n)))
                nil)))))

(defn prove-both
  "Steps the forward search and the closure back from the body in turn until one answers, so a body no mob can reach (a
  pillar top) is settled by its small closure and a reachable mob by its short forward search, neither paying for the
  other's flood. [forward-answer closure-answer]: a closure that ran out of budget leaves the forward search to finish
  alone, a forward search that did leaves the closure to."
  [fwd close]
  (loop [f nil c nil]
    (let [f (or f (fwd))
          c (if (and (nil? f) (nil? c)) (close) c)]
      (cond
        (and (some? f) (not= :budget f)) [f c]
        (= :budget f) [f (or c (loop [r (close)] (if (nil? r) (recur (close)) r)))]
        (instance? js/Set c) [nil c]
        (some? c) [(let [r (loop [r (fwd)] (if (nil? r) (recur (fwd)) r))] r) c]
        :else (recur nil nil)))))

(defn proved-way?
  "way? using the proofs pr of the query's earlier mobs.
  Once near-closure has closed, the answer is a set lookup.
  Otherwise the forward search from the mob (it skips dead cells and stops at alive ones) and the closure back from the
  body run in turn (prove-both), and the first to answer decides.
  If both run out of budget, a search back from the body decides, and unknown counts as a way.
  After one such unknown, every later mob counts as having a way."
  [kind-at ^js pr mob body]
  (let [[mx my mz] mob
        key (.-key pr)
        ^js alive (.-alive pr)
        mk (key mx my mz)
        ^js closure (.-reach pr)]
    (cond
      (instance? js/Set closure) (.has closure mk)
      (keyword-identical? closure :open) (not= :closed (forward-proving kind-at pr mob body))
      :else
      (let [[f c] (if closure
                    [(forward-proving kind-at pr mob body) nil]
                    (prove-both (forward-stepper kind-at pr mob body) (closure-stepper kind-at pr body)))]
        (when (and (not closure) (some? c)) (set! (.-reach pr) c))
        (cond
          (= :found f) true
          (= :closed f) false
          (instance? js/Set c) (.has c mk)
          :else
          (let [back (reach/search (fn [x y z]
                                     (let [ns (reach/backward kind-at x y z)]
                                       (dotimes [i (.-length ns)]
                                         (let [n (aget ns i)] (.add alive (key (aget n 0) (aget n 1) (aget n 2)))))
                                       ns))
                                   body mob)]
            (when (keyword-identical? back :budget) (set! (.-reach pr) :open))
            (not= :closed back)))))))
