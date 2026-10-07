(ns engine.path.planner.run
  "Search methods: starting the search, expanding nodes in slices (step), and the steps the body cannot undo."
  (:require [engine.path.planner.base :refer [FLOOD-GROWTH FLOOD-SPACING GRID JUMP-UP MOVE-DROP MOVE-GAP OPEN-REACH REGIONS SLOW]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(def ^:const START-DRAIN 512) ; forward expansions finishEnclosed spends to see whether the start is sealed

(extend-type Search
  Object

  ;; ---- the search ----
  (finish [s why]
    (set! (.-finished s) true)
    (set! (.-reason s) why)
    (set! (.-elapsed s) (- (js/performance.now) (.-t0 s))))

  ;; The goal flood proved the goal walled in or cut off: finish so, unless the start's own region is sealed (the forward search
  ;; drains within a small budget, no loaded-edge node), when the start is what keeps the body from the goal.
  (finishEnclosed [s why]
    (set! (.-flood-pending s) false)
    (let [expanded (.-expanded s) best-distance (.-best-distance s) best-node (.-best-node s)]
      (loop [n 0]
        (cond
          (and (not ^boolean (.-finished s)) (< n START-DRAIN)) (do (.expandNext s) (recur (inc n)))
          (and ^boolean (.-finished s) (nil? (.-reason s))) nil
          :else (let [sealed (and ^boolean (.-finished s) (identical? (.-reason s) "exhausted") ^boolean (.startEnclosed s) (not ^boolean (.startCliff s)))]
                  (when-not sealed ; the drain only labels: the partial plan is the one the search had
                    (set! (.-expanded s) expanded)
                    (set! (.-best-distance s) best-distance)
                    (set! (.-best-node s) best-node))
                  (.finish s (if sealed "start-enclosed" why)))))))

  ;; does any node the search expanded stand beside a cliff or gap (a way off the start's land, so its walls are not what holds it)?
  (startCliff [s]
    (loop [i 0]
      (cond
        (>= i (.-n-nodes s)) false
        ^boolean (.cliffBeside s (aget (.-xs s) i) (aget (.-ys s) i) (aget (.-zs s) i) (aget (.-hs s) i)) true
        :else (recur (inc i)))))

  ;; the free position nearest where the body is, as an index of the mask; -1 when none is free
  (nearestFree [s ^js mask want-x want-z]
    (loop [k 0
           best -1
           best-dist js/Infinity]
      (if (< k (.-length mask))
        (let [dx (- (js-mod k GRID) want-x)
              dz (- (js/Math.floor (/ k GRID)) want-z)
              d (+ (* dx dx) (* dz dz))]
          (if (and (not (zero? (aget mask k))) (< d best-dist))
            (recur (inc k) k d)
            (recur (inc k) best best-dist)))
        best)))

  ;; the start node's shape: in a tight cell its region is the one holding the free position nearest where the body is;
  ;; -1 when the cell has none
  (startShape [s]
    (if-not ^boolean (.isTight s (.-from-x s) (.-from-y s) (.-from-z s))
      0
      (let [^js shape (.shapeOf s (.-from-x s) (.-from-y s) (.-from-z s) (+ (* (.-from-y s) 16) (.-start-h s)))
            labels (.-labels shape)
            best (.nearestFree s (.-mask shape) (* (- (.-from-px s) (.-from-x s)) 16) (* (- (.-from-pz s) (.-from-z s)) 16))]
        (if (or (neg? best) (>= (aget labels best) REGIONS))
          -1
          (let [region (aget labels best)
                ^js rep (aget (.-regs shape) region)]
            (.packShape s region true (.-px rep) (.-pz rep) false 0 0))))))

  (init [s]
    (set! (.-view s) (js-obj "stateAt" (fn [x y z] (.viewAt s x y z))))
    (set! (.-start-h s) (.nodeH s (.-from-x s) (.-from-y s) (.-from-z s)))
    ;; (before goalNotStandable reuses standH)
    (set! (.-start-slow s) (if (and (== (aget (.-tbl-hazard s) (.-support s)) SLOW) (not ^boolean (.isWater s (.-from-x s) (.-from-y s) (.-from-z s)))) 1 0)))

  (begin [s]
    (set! (.-started s) true)
    (set! (.-t0 s) (js/performance.now))
    (cond
      (neg? (.-start-h s)) (.finish s "start-not-standable")
      ^boolean (.goalNotStandable s) (.finish s "goal-not-standable")
      :else
      (do
        (set! (.-start-distance s) (.distanceTo s (.-from-x s) (.-from-z s)))
        (let [shape (.startShape s)
              region (bit-and shape 15)]
          (if (neg? shape)
            (.finish s "start-not-standable")
            (do
              ;; the start is node 0
              (aset (.-shapes s) 0 shape)
              (aset (.-node-keys s) 0 (.keyOf s (.-from-x s) (.-from-y s) (.-from-z s) region))
              (aset (.-xs s) 0 (.-from-x s))
              (aset (.-ys s) 0 (.-from-y s))
              (aset (.-zs s) 0 (.-from-z s))
              (aset (.-hs s) 0 (.-start-h s))
              (aset (.-parents s) 0 -1)
              (aset (.-gs s) 0 0)
              (aset (.-fs s) 0 (.heuristic s (.-from-x s) (.-from-z s)))
              (aset (.-hash-table s) (bit-and (.hashOf s (.-from-x s) (.-from-y s) (.-from-z s) region) (dec (.-slots s))) 0)
              (aset (.-slows s) 0 (.-start-slow s))
              (set! (.-n-nodes s) 1)
              (set! (.-heap-n s) 1)
              (aset (.-heap s) 0 0)
              (aset (.-heap-pos s) 0 0)))))))

  ;; the body died idling on magma: a route never ends on a magma block or in a magma column, so such a node is passed
  ;; through (it is searched on from) but is neither the goal nor the best partial end
  (endsOnMagma [s x y z h]
    (or (== (aget (.-tbl-bubble s) (.stateAt ^js (.-snapshot s) x y z)) 2)
        (and (zero? h) (== (aget (.-tbl-magma s) (.stateAt s x (dec y) z)) 1))))

  ;; pop the best open node and expand it
  (expandNext [s]
    (if (zero? (.-heap-n s))
      (when-not ^boolean (.floodAtEnd s)
        (.finish s (if ^boolean (.-boxed s) "box" "exhausted")))
      (let [i (.popMin s)
            x (aget (.-xs s) i)
            y (aget (.-ys s) i)
            z (aget (.-zs s) i)
            deadly ^boolean (.endsOnMagma s x y z (aget (.-hs s) i))]
        (set! (.-expanded s) (inc (.-expanded s)))
        (if (and (not deadly) ^boolean (.reached s x y z))
          (do (set! (.-goal-node s) i)
              (.finish s nil))
          (let [d (.distanceTo s x z)]
            (when (and (not deadly) (< d (.-best-distance s)))
              (set! (.-best-distance s) d)
              (set! (.-best-node s) i))
            (if (and ^boolean (.-stop-at-edge s) ^boolean (.edgeStop s i))
              (do (set! (.-edge-node s) i)
                  (.finish s "exhausted"))
              (do (.expandAt s x y z (aget (.-hs s) i) (aget (.-slows s) i) i (bit-and (aget (.-shapes s) i) 15))
                  (when (pos? (.-length ^js (.-held s))) (.flushHeld s))
                  (when ^boolean (.-over-budget s) (.finish s "budget")))))))))

  ;; Does the search end at node i (stop-at-edge; i is not on magma)? When it is a frontier node (frontierNode's test: within
  ;; OPEN-REACH of a chunk's side, within frontier-reach of the goal, at the loaded edge) not in options.knownCells. The goal
  ;; is unloaded, so every way to it crosses the loaded edge, and nodes come out in order of cost plus (weighted) heuristic:
  ;; the first such node is the frontier the search would name once it had searched all loaded land (with weight 1 the very
  ;; same node, ties aside), found without searching the rest (card 7a031d15: up to 200000 nodes, over 100 go-to rounds).
  ;; A known node does not end it: frontierNode takes one only when no other edge is left. Nor does the start (node 0): a body
  ;; within OPEN-REACH of unloaded land with an unknown start cell (a teleport, a chunk gap) would get a one-step path and
  ;; never try another edge; the search goes on, and when the start is the only edge node, frontierNode's scan names it.
  (edgeStop [s i]
    (let [x (aget (.-xs s) i) z (aget (.-zs s) i)
          mx (bit-and x 15) mz (bit-and z 15)]
      (and (pos? i)
           (or (< mx OPEN-REACH) (>= mx (- 16 OPEN-REACH)) (< mz OPEN-REACH) (>= mz (- 16 OPEN-REACH)))
           (<= (js/Math.max (js/Math.abs (- x (.-goal-x s))) (js/Math.abs (- z (.-goal-z s)))) (.-frontier-reach s))
           (not (and (some? ^js (.-known-cells s)) ^boolean (.has ^js (.-known-cells s) (.knownKey s x (aget (.-ys s) i) z))))
           ^boolean (.atLoadedEdge s i))))

  ;; ---- steps the body cannot undo ----

  ;; Can the planner's own moves take the body from the lower cell back to the upper one? Runs the moves out of the lower cell
  ;; and looks for the one that enters the upper cell (the goal flood's probe, for one cell).
  (canReturn [s lx ly lz lh ux uy uz]
    (set! (.-flooding s) true)
    (set! (.-fr s) -1)
    (set! (.-fx s) ux)
    (set! (.-fy s) uy)
    (set! (.-fz s) uz)
    (set! (.-hit s) false)
    (.expandAt s lx ly lz lh 0 -1 -1)
    (set! (.-flooding s) false)
    ^boolean (.-hit s))

  ;; A one-way step cannot be undone with the body's own moves: a gap jump down, a drop of more than JUMP-UP, or a
  ;; drop the planner has no step-up move back from.
  (isOneWay [s node]
    (let [p (aget (.-parents s) node)
          m (aget (.-moves s) node)]
      (cond
        (== m MOVE-GAP) (< (.stand16 s node) (.stand16 s p))
        (not (== m MOVE-DROP)) false
        :else (or (> (- (.stand16 s p) (.stand16 s node)) JUMP-UP)
                  (not ^boolean (.canReturn s (aget (.-xs s) node) (aget (.-ys s) node) (aget (.-zs s) node) (aget (.-hs s) node) (aget (.-xs s) p) (aget (.-ys s) p) (aget (.-zs s) p)))))))

  ;; the first one-way step on the way to the node (nearest the start), or -1
  (firstOneWay [s node]
    (loop [i node
           first -1]
      (if (== (aget (.-parents s) i) -1)
        first
        (recur (aget (.-parents s) i) (if ^boolean (.isOneWay s i) i first)))))

  (flushHeld [s]
    (set! (.-replaying s) true)
    (let [held-now (.splice ^js (.-held s) 0)]
      (dotimes [k (.-length held-now)]
        (let [^js d (aget held-now k)
              x (aget d 0) y (aget d 1) z (aget d 2) p (aget d 5)]
          (when ^boolean (.canReturn s x y z (aget d 3) (aget (.-xs s) p) (aget (.-ys s) p) (aget (.-zs s) p))
            (set! (.-move-open s) (aget d 11))
            (set! (.-move-air s) (aget d 12))
            (set! (.-move-peak s) (aget d 13))
            (set! (.-move-water s) (aget d 14))
            (set! (.-move-dmg s) (aget d 15))
            (.consider s x y z (aget d 3) (aget d 4) p (aget d 6) (aget d 7) (aget d 8) (aget d 9) (aget d 10))))))
    (set! (.-replaying s) false)
    (set! (.-move-open s) 0)
    (set! (.-move-air s) 0)
    (set! (.-move-peak s) 0)
    (set! (.-move-water s) 0)
    (set! (.-move-dmg s) 0))

  ;; After a late flood with budget: one that ran out of it (and neither leaked nor met the start) is due again
  ;; after FLOOD-SPACING times the expansions, with FLOOD-GROWTH times the budget (at most max-nodes). That keeps its
  ;; cost a share of the search's, and a large walled-in region is still proved. Any other late flood is the last.
  (growFlood [s budget]
    (if (and (> (.-flooded s) budget) (not ^boolean (.-leaked s)) (< budget (.-max-nodes s)))
      (do (set! (.-goal-flood s) (js/Math.min (.-max-nodes s) (* FLOOD-GROWTH budget)))
          (set! (.-flood-after s) (* FLOOD-SPACING (.-flood-after s))))
      (set! (.-flood-pending s) false)))

  ;; A search that ran out of nodes with its late flood still due floods once more, so a walled-in goal is named.
  ;; The budget is the flood budget if no late flood ran yet, else at most what the search expanded. A ladder turned
  ;; away at a gap or a swim for air does not skip it: the flood takes the search's own moves and leaks at any water,
  ;; and a flood that reaches the top of a ladder with a gap is not enclosed (floodGap), so a walled-in goal is walled in
  ;; whatever else the search refused (settle names ladder-gap or air only when it is not). True when that flood begins
  ;; (step runs it and finishes the search: goal-enclosed, box or exhausted).
  (floodAtEnd [s]
    (if (or (not ^boolean (.-flood-pending s)) ^boolean (.-goal-unloaded s))
      false
      (do (.lateFloodBegin s (if (pos? (.-flooded s)) (js/Math.min (.-goal-flood s) (.-expanded s)) (.-goal-flood s)) true)
          true)))

  ;; The goal flood costs about 30 ms, so easy queries must not see it: it runs after flood-after forward
  ;; expansions (of this search and the earlier ones of a kept flood). A flood that grew is due again by then, or once
  ;; the search has made half its max-nodes, so the last growth comes before the search runs out of nodes. A slice of
  ;; max-expansions counts each newly flooded cell as one.
  (step [s max-expansions]
    (when-not ^boolean (.-started s)
      (.begin s)
      (.takeSchedule s)
      (when (and (not ^boolean (.-finished s)) ^boolean (.goalEnclosedEarly s))
        (.finishEnclosed s "goal-enclosed")))
    (loop [n 0]
      (when (and (< n max-expansions) (not ^boolean (.-finished s)))
        (cond
          ^boolean (.-lf-active s)
          (let [size0 (.-size ^js (.-lf-seen s))
                over ^boolean (.lateFloodRun s (- max-expansions n))
                used (inc (- (.-size ^js (.-lf-seen s)) size0))]
            (cond
              (not over) (recur (+ n used))
              :else
              (let [enclosed ^boolean (.lateFloodEnd s)]
                (set! (.-expanded s) (+ (.-expanded s) (- (.-flooded s) (.-lf-imported s))))
                (cond
                  ;; cells taken over from the memo read an older world: a fresh flood checks the answer
                  (and enclosed (pos? (.-lf-imported s))) (do (.freshFlood s)
                                                        (set! (.-verifying s) true)
                                                        (recur (+ n used)))
                  ^boolean (.-lf-end s) (do (set! (.-flood-pending s) false)
                             (if enclosed
                               (.finishEnclosed s (if ^boolean (.-cut-off s) "goal-cut-off" "goal-enclosed"))
                               (.finish s (if ^boolean (.-boxed s) "box" "exhausted"))))
                  :else (do (.growFlood s (.-lf-budget s))
                            (if enclosed
                              (.finishEnclosed s (if ^boolean (.-cut-off s) "goal-cut-off" "goal-enclosed"))
                              (do (.expandNext s)
                                  (recur (+ n used)))))))))

          (and ^boolean (.-flood-pending s) (not ^boolean (.-goal-unloaded s))
               (or (>= (+ (.-flood-base s) (.-expanded s)) (.-flood-after s)) (and (some? ^js (.-lf-seen s)) (>= (* 2 (.-n-nodes s)) (.-max-nodes s)))))
          (do (.lateFloodBegin s (.-goal-flood s) false)
              (recur n))

          :else
          (do (.expandNext s)
              (recur (inc n))))))
    (.keepFlood s)
    ^boolean (.-finished s)))
