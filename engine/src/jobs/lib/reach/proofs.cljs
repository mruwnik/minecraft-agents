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

(defn forward-proving
  "search from the mob's cell over forward moves, using proofs: a dead cell is not expanded, an alive one counts as
  reaching the body. A closed search marks every cell it saw dead, a found one marks its way alive."
  [kind-at ^js pr [sx sy sz] [tx ty tz]]
  (let [key (.-key pr) dead (.-dead pr) alive (.-alive pr)
        parent (js/Map.)
        open #js []
        k0 (key sx sy sz)]
    (.set parent k0 -1)
    (reach/heap-push! open #js [(reach/heuristic sx sy sz tx ty tz) 0 sx sy sz k0])
    (loop [n 0]
      (cond
        (zero? (.-length open)) (do (.forEach parent (fn [_ k] (.add dead k))) :closed)
        (>= n reach/node-budget) :budget
        :else
        (let [top (reach/heap-pop! open)
              g (aget top 1) x (aget top 2) y (aget top 3) z (aget top 4) k (aget top 5)]
          (cond
            (or (reach/near-cell? x y z tx ty tz) (.has alive k))
            (do (loop [c k] (when-not (== c -1) (.add alive c) (recur (.get parent c)))) :found)
            (.has dead k) (recur (inc n))
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
              (recur (inc n)))))))))

(def near-budget
  "Cells near-closure expands before it gives up."
  reach/node-budget)

(def near-offsets
  "Offsets of the cells near-cell? counts as near: one either way on each axis."
  (vec (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]] [dx dy dz])))

(defn near-closure
  "Breadth-first back from the cells near the body over backward moves. Returns the set of keys of every cell with a
  way to the body, or :budget when more than near-budget cells were expanded. Every cell met is added to pr's alive."
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
                    (.push queue c))))]
    (doseq [[dx dy dz] near-offsets]
      (let [x (+ bx dx) y (+ by dy) z (+ bz dz)]
        (meet! x y z #js [x y z])))
    (loop [head 0]
      (cond
        (== head (.-length queue)) seen
        (>= head near-budget) :budget
        :else (let [c (aget queue head)
                    nexts (reach/backward kind-at (aget c 0) (aget c 1) (aget c 2))]
                (dotimes [i (.-length nexts)]
                  (let [n (aget nexts i)]
                    (meet! (aget n 0) (aget n 1) (aget n 2) n)))
                (recur (inc head)))))))

(defn proved-way?
  "way? using the proofs pr of the query's earlier mobs.
  Once near-closure has closed, the answer is a set lookup.
  Otherwise a forward search skips dead cells and stops at alive ones.
  If that runs out of budget, near-closure runs once for the query.
  If near-closure also runs out, a search back from the body decides, and unknown counts as a way.
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
      (case (forward-proving kind-at pr mob body)
        :found true
        :closed false
        (let [r (or closure (set! (.-reach pr) (near-closure kind-at pr body)))]
          (if (instance? js/Set r)
            (.has r mk)
            (let [back (reach/search (fn [x y z]
                                       (let [ns (reach/backward kind-at x y z)]
                                         (dotimes [i (.-length ns)]
                                           (let [n (aget ns i)] (.add alive (key (aget n 0) (aget n 1) (aget n 2)))))
                                         ns))
                                     body mob)]
              (when (keyword-identical? back :budget) (set! (.-reach pr) :open))
              (not= :closed back))))))))
