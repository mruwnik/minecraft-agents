(ns engine.path.planner.nodes
  "Search methods: the goal test and heuristic, and node storage (hash, heap, recording an arrival, refused moves)."
  (:require [engine.path.planner.base :refer [AIR-STEP AVOID-CLIMB AVOID-OPEN AVOID-WATER HALF JUMP-UP MOVE-CLIMB-UP MOVE-DROP MOVE-GAP MOVE-OPEN MOVE-SWIM REGIONS SPAN SQRT2 WALK-S cell-key grown next-pow2]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; ---- goal ----
  (reached [s x y z]
    (cond
      (pos? (.-n-goals s))
      (>= (.goalAt s x y z) 0)
      :else
      (let [dx (- x (.-goal-x s))
            dz (- z (.-goal-z s))]
        (if ^boolean (.-near s)
          (<= (+ (* dx dx) (* (- y (.-goal-y s)) (- y (.-goal-y s))) (* dz dz)) (* (.-goal-range s) (.-goal-range s)))
          (<= (+ (* dx dx) (* dz dz)) (* (.-goal-range s) (.-goal-range s)))))))

  ;; the index of the first goal of the set whose area holds the cell, -1 for none
  (goalAt [s x y z]
    (loop [i 0]
      (if (< i (.-n-goals s))
        (let [dx (- x (aget ^js (.-g-xs s) i))
              dy (if (== 1 (aget ^js (.-g-near s) i)) (- y (aget ^js (.-g-ys s) i)) 0)
              dz (- z (aget ^js (.-g-zs s) i))]
          (if (<= (+ (* dx dx) (* dy dy) (* dz dz)) (aget ^js (.-g-r2 s) i))
            i
            (recur (inc i))))
        -1)))

  ;; the cell, or the one under it (a head cell), is within the goal: where a portal may be entered
  (inGoal [s x y z]
    (or ^boolean (.reached s x y z) ^boolean (.reached s x (dec y) z)))

  (octileTo [s x z gx gz]
    (let [a (js/Math.abs (- x gx))
          b (js/Math.abs (- z gz))]
      (+ (js/Math.max a b) (* (- SQRT2 1) (js/Math.min a b)))))

  ;; octile x-z distance to the goal; to the nearest goal of a set
  (distanceTo [s x z]
    (if (pos? (.-n-goals s))
      (loop [i 0 best js/Infinity]
        (if (< i (.-n-goals s))
          (recur (inc i) (js/Math.min best (.octileTo s x z (aget ^js (.-g-xs s) i) (aget ^js (.-g-zs s) i))))
          best))
      (.octileTo s x z (.-goal-x s) (.-goal-z s))))

  ;; a goal set's heuristic is the least of its goals' (each admissible and consistent, so their least is too)
  (heuristic [s x z]
    (if (pos? (.-n-goals s))
      (loop [i 0 best js/Infinity]
        (if (< i (.-n-goals s))
          (recur (inc i) (js/Math.min best (js/Math.max 0 (- (.octileTo s x z (aget ^js (.-g-xs s) i) (aget ^js (.-g-zs s) i)) (aget ^js (.-g-slack s) i)))))
          (* best WALK-S)))
      (* (js/Math.max 0 (- (.distanceTo s x z) (.-slack s))) WALK-S)))

  ;; ---- node storage ----
  (hashOf [s x y z region]
    (unsigned-bit-shift-right
     (bit-xor (js/Math.imul (+ (- x (.-from-x s)) HALF) 73856093)
              (js/Math.imul (- y (.-min-y s)) 19349663)
              (js/Math.imul (+ (- z (.-from-z s)) HALF) 83492791)
              (js/Math.imul region 668265263))
     0))

  (keyOf [s x y z region]
    (+ (* (+ (* (+ (* (- y (.-min-y s)) SPAN) (+ (- x (.-from-x s)) HALF)) SPAN) (+ (- z (.-from-z s)) HALF)) REGIONS) region))

  ;; the slot holding the node with this key, or the empty slot where it belongs
  (findSlot [s key start]
    (loop [slot start]
      (let [node (aget (.-hash-table s) slot)]
        (if (or (== node -1) (== (aget (.-node-keys s) node) key))
          slot
          (recur (bit-and (inc slot) (dec (.-slots s))))))))

  ;; the first empty slot from `start`, whatever keys sit before it
  (emptySlot [s start]
    (loop [slot start]
      (if (== (aget (.-hash-table s) slot) -1)
        slot
        (recur (bit-and (inc slot) (dec (.-slots s)))))))

  (grow [s]
    (set! (.-cap s) (js/Math.min (.-max-nodes s) (* (.-cap s) 2)))
    (set! (.-node-keys s) (grown (.-node-keys s) (.-cap s)))
    (set! (.-xs s) (grown (.-xs s) (.-cap s)))
    (set! (.-ys s) (grown (.-ys s) (.-cap s)))
    (set! (.-zs s) (grown (.-zs s) (.-cap s)))
    (set! (.-hs s) (grown (.-hs s) (.-cap s)))
    (set! (.-moves s) (grown (.-moves s) (.-cap s)))
    (set! (.-slows s) (grown (.-slows s) (.-cap s)))
    (set! (.-corners s) (grown (.-corners s) (.-cap s)))
    (set! (.-shapes s) (grown (.-shapes s) (.-cap s)))
    (set! (.-parents s) (grown (.-parents s) (.-cap s)))
    (set! (.-secs s) (grown (.-secs s) (.-cap s)))
    (set! (.-risks s) (grown (.-risks s) (.-cap s)))
    (set! (.-darks s) (grown (.-darks s) (.-cap s)))
    (set! (.-airs s) (grown (.-airs s) (.-cap s)))
    (set! (.-peaks s) (grown (.-peaks s) (.-cap s)))
    (set! (.-wsecs s) (grown (.-wsecs s) (.-cap s)))
    (set! (.-opens s) (grown (.-opens s) (.-cap s)))
    (set! (.-gs s) (grown (.-gs s) (.-cap s)))
    (set! (.-fs s) (grown (.-fs s) (.-cap s)))
    (set! (.-heap-pos s) (grown (.-heap-pos s) (.-cap s)))
    (set! (.-heap s) (grown (.-heap s) (.-cap s)))
    (set! (.-slots s) (next-pow2 (* (.-cap s) 2)))
    (set! (.-hash-table s) (.fill (js/Int32Array. (.-slots s)) -1))
    ;; newest first: a rival record of a node (same key) sits ahead of the one it rivals in the probe, so lookups find it
    (loop [i (dec (.-n-nodes s))]
      (when (>= i 0)
        (let [x (aget (.-xs s) i)
              y (aget (.-ys s) i)
              z (aget (.-zs s) i)]
          (aset (.-hash-table s) (.emptySlot s (bit-and (.hashOf s x y z (bit-and (aget (.-shapes s) i) 15)) (dec (.-slots s)))) i))
        (recur (dec i)))))

  ;; the empty (or, for a rival record, the first record's) slot for a new key after making room
  (grownSlot [s x y z region key]
    (.grow s)
    (.findSlot s key (bit-and (.hashOf s x y z region) (dec (.-slots s)))))

  (addNode [s x y z region key slot]
    (let [free (if (== (.-n-nodes s) (.-cap s)) (.grownSlot s x y z region key) slot)
          node (.-n-nodes s)]
      (set! (.-n-nodes s) (inc node))
      (aset (.-hash-table s) free node)
      (aset (.-node-keys s) node key)
      (aset (.-xs s) node x)
      (aset (.-ys s) node y)
      (aset (.-zs s) node z)
      (aset (.-heap-pos s) node -1)
      node))

  ;; best first by f, then deeper (larger g), then earlier discovered: ties never depend on memory layout
  (before [s a b]
    (or (< (aget (.-fs s) a) (aget (.-fs s) b))
        (and (== (aget (.-fs s) a) (aget (.-fs s) b))
             (or (> (aget (.-gs s) a) (aget (.-gs s) b))
                 (and (== (aget (.-gs s) a) (aget (.-gs s) b)) (< a b))))))

  (siftUp [s from node]
    (loop [i from]
      (if (pos? i)
        (let [p (bit-shift-right (dec i) 1)
              above (aget (.-heap s) p)]
          (if ^boolean (.before s node above)
            (do (aset (.-heap s) i above)
                (aset (.-heap-pos s) above i)
                (recur p))
            (do (aset (.-heap s) i node)
                (aset (.-heap-pos s) node i))))
        (do (aset (.-heap s) i node)
            (aset (.-heap-pos s) node i)))))

  (siftDown [s from node]
    (loop [i from]
      (let [left (inc (* 2 i))]
        (if (>= left (.-heap-n s))
          (do (aset (.-heap s) i node)
              (aset (.-heap-pos s) node i))
          (let [c (if (and (< (inc left) (.-heap-n s)) ^boolean (.before s (aget (.-heap s) (inc left)) (aget (.-heap s) left))) (inc left) left)
                child (aget (.-heap s) c)]
            (if ^boolean (.before s child node)
              (do (aset (.-heap s) i child)
                  (aset (.-heap-pos s) child i)
                  (recur c))
              (do (aset (.-heap s) i node)
                  (aset (.-heap-pos s) node i))))))))

  (popMin [s]
    (let [node (aget (.-heap s) 0)]
      (set! (.-heap-n s) (dec (.-heap-n s)))
      (when (pos? (.-heap-n s)) (.siftDown s 0 (aget (.-heap s) (.-heap-n s))))
      (aset (.-heap-pos s) node -2)
      node))

  ;; set the node's way in and put it in its place in the heap
  (relax [s node x z h move parent-node sec risk dark g slow-to corner shape]
    (aset (.-hs s) node h)
    (aset (.-moves s) node move)
    (aset (.-slows s) node slow-to)
    (aset (.-corners s) node corner)
    (aset (.-shapes s) node shape)
    (aset (.-parents s) node parent-node)
    (aset (.-secs s) node sec)
    (aset (.-risks s) node risk)
    (aset (.-darks s) node dark)
    (aset (.-airs s) node (.-move-air s))
    (aset (.-peaks s) node (js/Math.max (aget (.-peaks s) parent-node) (.-move-peak s)))
    (aset (.-wsecs s) node (+ (aget (.-wsecs s) parent-node) (.-move-water s)))
    (aset (.-opens s) node (.-move-open s))
    (aset (.-gs s) node g)
    (aset (.-fs s) node (+ g (* (.-weight s) (.heuristic s x z))))
    (if (== (aget (.-heap-pos s) node) -1)
      (do (set! (.-heap-n s) (inc (.-heap-n s)))
          (.siftUp s (dec (.-heap-n s)) node))
      (.siftUp s (aget (.-heap-pos s) node) node)))

  ;; a record of its own for a node: unless the node budget is spent
  (insertNode [s x y z h move parent-node sec risk dark g slow-to corner shape region key slot]
    (if (== (.-n-nodes s) (.-max-nodes s))
      (set! (.-over-budget s) true)
      (.relax s (.addNode s x y z region key slot) x z h move parent-node sec risk dark g slow-to corner shape)))

  ;; relax the edge to a node: insert it, or lower its cost if this way is cheaper. Known dangers add to the move's risk
  ;; (dangerRisk), after holdsBack: a held drop replayed through here is charged once.
  ;; a tight cell's node also has its region, the region's point and the crossing the move came in by: `shape`
  ;; A node reached again with a very different air use gets a record of its own: the hash points at the newest.
  (consider [s x y z h move parent-node dsec drisk slow-to corner shape]
    (let [region (bit-and shape 15)
          rx (+ (- x (.-from-x s)) HALF)
          rz (+ (- z (.-from-z s)) HALF)]
      (cond
        (or (neg? rx) (>= rx SPAN) (neg? rz) (>= rz SPAN)) nil
        (or (< x (.-bx0 s)) (> x (.-bx1 s)) (< z (.-bz0 s)) (> z (.-bz1 s)) (< y (.-by0 s)) (> y (.-by1 s))) (set! (.-boxed s) true)
        ;; a gap jump or a drop never lands on farmland: a landing after a fall of over 0.5 blocks tramples it (a farmland node
        ;; is the farmland's own cell; a jump up one block falls about 0.3 from the top of its arc, so it may land there)
        (and (or (== move MOVE-GAP) (== move MOVE-DROP)) (== (aget (.-tbl-farmland s) (.stateAt ^js (.-snapshot s) x y z)) 1)) nil
        (and ^boolean (.-returnable s) (not ^boolean (.-replaying s)) ^boolean (.holdsBack s x y z h move parent-node dsec drisk slow-to corner shape)) nil
        ^boolean (.refusedKind s (.-limit-kinds s) x y z move) (do (set! (.-limit-refused s) true) nil)
        :else
        (let [drisk (if (pos? (.-n-dangers s)) (+ drisk (.dangerRisk s x y z dsec)) drisk)
              extra (if ^boolean (.-avoiding s) (.avoidCost s x y z move dsec drisk) 0)
              ddark (if (some? (.-dark-at s)) (* (.-dark-factor s) dsec (.darkOf s x y z)) 0)]
          (when-not (neg? extra)
            (let [key (.keyOf s x y z region)
                  slot (.findSlot s key (bit-and (.hashOf s x y z region) (dec (.-slots s))))
                  found (aget (.-hash-table s) slot)
                  sec (+ (aget (.-secs s) parent-node) dsec)
                  risk (+ (aget (.-risks s) parent-node) drisk)
                  ;; an alternative's search orders by its penalised cost; secs and risks stay the true cost of the walk
                  dark (+ (aget (.-darks s) parent-node) ddark)
                  g (if ^boolean (.-avoiding s) (+ (aget (.-gs s) parent-node) dsec (* (.-risk-weight s) drisk) extra ddark) (+ sec (* (.-risk-weight s) risk) dark))]
              (if (== found -1)
                (.insertNode s x y z h move parent-node sec risk dark g slow-to corner shape region key slot)
                (let [d-air (- (.-move-air s) (aget (.-airs s) found))
                      g-found (aget (.-gs s) found)]
                  (cond
                    (and (< g g-found) (<= d-air AIR-STEP))
                    (when-not (== (aget (.-heap-pos s) found) -2)
                      (.relax s found x z h move parent-node sec risk dark g slow-to corner shape))

                    (or (< g g-found) (< d-air (- AIR-STEP)))
                    (.insertNode s x y z h move parent-node sec risk dark g slow-to corner shape region key slot)

                    :else nil)))))))))

  (stand16 [s node] (+ (* (aget (.-ys s) node) 16) (aget (.-hs s) node)))

  ;; The returnable search plans no step the body cannot undo. True when the move is not considered now.
  ;; A gap jump down and a drop of more than JUMP-UP are refused. Any other drop is held back until the probe
  ;; (canReturn) says the body can climb back. The probe runs the moves of another cell, so it cannot run inside an
  ;; expansion: see flushHeld.
  (holdsBack [s x y z h move parent-node dsec drisk slow-to corner shape]
    (cond
      (== move MOVE-GAP) (< (+ (* y 16) h) (.stand16 s parent-node))
      (== move MOVE-DROP) (do (when-not (> (- (.stand16 s parent-node) (+ (* y 16) h)) JUMP-UP)
                                (.push ^js (.-held s) #js [x y z h move parent-node dsec drisk slow-to corner shape (.-move-open s) (.-move-air s) (.-move-peak s) (.-move-water s)]))
                              true)
      :else false))

  ;; is entering x,y,z by `move` of one of `kinds` (bits: climbing, water, opening something)?
  (refusedKind [s kinds x y z move]
    (cond
      (zero? kinds) false
      (and (not (zero? (bit-and kinds AVOID-CLIMB))) (>= move MOVE-CLIMB-UP) (<= move MOVE-OPEN)) true
      (and (not (zero? (bit-and kinds AVOID-WATER))) (or (>= move MOVE-SWIM) ^boolean (.isWater s x y z))) true
      (and (not (zero? (bit-and kinds AVOID-OPEN))) (or (== move MOVE-OPEN) (pos? (.-move-open s)))) true
      :else false))

  ;; what entering x,y,z by `move` adds to its cost in a search for an alternative path: -1 refuses a move of a kind avoided;
  ;; a cell within 1 block of an earlier path costs avoid-factor times its own cost more
  (avoidCost [s x y z move dsec drisk]
    (cond
      ^boolean (.refusedKind s (.-avoid-kinds s) x y z move) -1
      (true? (.has ^js (.-avoid-cells s) (cell-key x y z))) (* (.-avoid-factor s) (+ dsec (* (.-risk-weight s) drisk)))
      :else 0))

  ;; where moves go: into the search, or the goal flood's probe
  (sink [s x y z h move parent-node dsec drisk slow-to corner shape]
    (cond
      (some? ^js (.-flood-out s)) (.push ^js (.-flood-out s) (.keyOf s x y z (bit-and shape 15)) (- -1 (.keyOf s x y z 0)))
      ^boolean (.-flooding s)
      (when (and (== x (.-fx s)) (== y (.-fy s)) (== z (.-fz s)) (or (neg? (.-fr s)) (== (.-fr s) (bit-and shape 15)))) (set! (.-hit s) true))
      :else
      (.consider s x y z h move parent-node dsec drisk slow-to corner shape)))

  ;; the edge of a move: straight to the sink, or through one of the passes over an expansion near a door (see expandAt)
  (edge [s x y z h move parent-node dsec drisk slow-to corner shape]
    (cond
      (== (.-edge-mode s) 0) (.sink s x y z h move parent-node dsec drisk slow-to corner shape)
      (== (.-edge-mode s) 2) (do (.add ^js (.-seen-edges s) (.keyOf s x y z (bit-and shape 15)))
                           (.sink s x y z h move parent-node dsec drisk slow-to corner shape))
      :else (.openingEdge s x y z h move parent-node dsec drisk slow-to corner shape))))
