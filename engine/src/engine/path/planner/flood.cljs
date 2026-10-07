(ns engine.path.planner.flood
  "Search methods: the goal flood (is the goal walled in?), and the late flood kept over the searches of one goal."
  (:require [engine.path.planner.base :refer [BODY FLOOD-MEMO FLOOD-TABLE HALF REGIONS SPAN UNLOADED WATER]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; ---- the goal flood ----
  (inSpan [s x z]
    (let [rx (+ (- x (.-from-x s)) HALF)
          rz (+ (- z (.-from-z s)) HALF)]
      (and (>= rx 0) (< rx SPAN) (>= rz 0) (< rz SPAN))))

  (goalNotStandable [s]
    (if (or (not ^boolean (.-near s)) ^boolean (.-goal-unloaded s))
      false
      (let [r (js/Math.ceil (.-goal-range s))]
        (loop [dx (- r)
               dy (- r)
               dz (- r)]
          (cond
            (> dx r) true
            (> dy r) (recur (inc dx) (- r) (- r))
            (> dz r) (recur dx (inc dy) (- r))
            (and (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* (.-goal-range s) (.-goal-range s)))
                 (>= (.nodeH s (+ (.-goal-x s) dx) (+ (.-goal-y s) dy) (+ (.-goal-z s) dz)) 0)) false
            :else (recur dx dy (inc dz)))))))

  ;; a bubble column lifts and drags in ways the flood does not enumerate: meeting one marks the flood leaked
  (leakBubble [s x y z]
    (when-not (zero? (aget (.-tbl-bubble s) (.stateAt s x y z))) (set! (.-leaked s) true)))

  ;; the standable cells of the goal start the flood
  (floodSeeds [s ^js seen ^js queue]
    (let [r (js/Math.ceil (.-goal-range s))]
      (loop [dx (- r)
             dy (- r)
             dz (- r)]
        (cond
          (> dx r) nil
          (> dy r) (recur (inc dx) (- r) (- r))
          (> dz r) (recur dx (inc dy) (- r))
          :else
          (let [x (+ (.-goal-x s) dx)
                y (+ (.-goal-y s) dy)
                z (+ (.-goal-z s) dz)]
            (when (and (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* (.-goal-range s) (.-goal-range s)))
                       ^boolean (.inSpan s x z)
                       (>= (.nodeH s x y z) 0))
              (dotimes [r (.floodRegions s x y z (.nodeH s x y z))]
                (.add seen (.keyOf s x y z r))
                (.push queue x y z r))
              (.leakBubble s x y z))
            (recur dx dy (inc dz)))))))

  ;; the flood's nodes in a cell standing at h: one per region of a tight cell, as the search's nodes are (the strips either
  ;; side of a fence line are two, and no move joins them), else one
  (floodRegions [s x y z h]
    (if ^boolean (.isTight s x y z)
      (js/Math.min REGIONS (.-length (.-regs ^js (.shapeOf s x y z (+ (* y 16) h)))))
      1))

  ;; a cell the flood cannot see into: out of the span, or in a column the snapshot has not loaded
  (unseen [s x y z]
    (or (not ^boolean (.inSpan s x z))
        (and (== (.stateAt ^js (.-snapshot s) x y z) UNLOADED)
             (false? (.hasColumn ^js (.-snapshot s) (bit-shift-right x 4) (bit-shift-right z 4))))))

  ;; the cell's stand height as the search can reach it: as it stands, or with a closed door, gate or trapdoor in its column
  ;; read as open (the opening pass makes nodes the body only fits in once something is opened)
  (floodH [s x y z]
    (when (nil? ^js (.-flood-h-keys s))
      (set! (.-flood-h-keys s) (js/Float64Array. FLOOD-TABLE))
      (set! (.-flood-h-vals s) (js/Int16Array. FLOOD-TABLE)))
    (let [key (inc (.keyOf s x y z 0))
          slot (bit-and (.hashOf s x y z 0) (dec FLOOD-TABLE))]
      (if (== (aget ^js (.-flood-h-keys s) slot) key)
        (aget ^js (.-flood-h-vals s) slot)
        (let [h (.floodHeight s x y z)]
          (aset ^js (.-flood-h-keys s) slot key)
          (aset ^js (.-flood-h-vals s) slot h)
          h))))

  (floodHeight [s x y z]
    (let [h (.nodeH s x y z)]
      (if (or (>= h 0)
              (not (or (pos? (aget (.-tbl-openable s) (.stateAt ^js (.-snapshot s) x (dec y) z)))
                       (pos? (aget (.-tbl-openable s) (.stateAt ^js (.-snapshot s) x y z)))
                       (pos? (aget (.-tbl-openable s) (.stateAt ^js (.-snapshot s) x (inc y) z))))))
        h
        (do (set! (.-open-mode s) true)
            (let [opened (.nodeH s x y z)]
              (set! (.-open-mode s) false)
              opened)))))

  ;; can a gap jump in direction c from a takeoff dy above the flood's current cell come over the column beside it (free at
  ;; body height over the takeoff)?
  (jumpOver [s c dy]
    (let [lo (* (+ (.-fy s) dy) 16)]
      (.clear s (+ (.-fx s) (aget (.-adx s) c)) (+ (.-fz s) (aget (.-adz s) c)) lo (+ lo BODY))))

  ;; Adds the cell when it is standable and a forward move of it reaches the flood's current cell. True when it is
  ;; the start. A cell the flood cannot see is a way in it does not know, so it marks the flood leaked. That counts
  ;; for a cell beside the current one (jump -1), and for a gap jump's takeoff in direction jump only when the jump
  ;; can come over the column between.
  (floodVisit [s ^js seen ^js queue start-key x y z jump]
    (if ^boolean (.unseen s x y z)
      (do (when (or (neg? jump) ^boolean (.jumpOver s jump (- y (.-fy s)))) (set! (.-leaked s) true))
          false)
      (let [h (.floodH s x y z)]
        (cond
          (neg? h) false
          ;; a cell the body fits in only once something is opened: one node for all its regions (the opening pass's
          ;; regions are its own), as wide as the flood was before it knew regions
          (neg? (.nodeH s x y z)) (.floodNode s seen queue start-key x y z h -1)
          :else (loop [r 0
                       n (.floodRegions s x y z h)]
                  (cond
                    (>= r n) false
                    ^boolean (.floodNode s seen queue start-key x y z h r) true
                    :else (recur (inc r) n)))))))

  ;; what the moves out of region r of the cell (-1: every region) enter, run once per search (flood-moves): an array of
  ;; the entered nodes' keys, each followed by -1 - the key of its cell's region 0 (so a probe of any region matches). A
  ;; flood visits a cell from every flooded cell within a gap jump of it, so without the memo it expands it again each time.
  (floodMovesOf [s x y z h r key]
    (let [memo-key (if (neg? r) (- -1 key) key)
          known (.get ^js (.-flood-moves s) memo-key)]
      (if (some? known)
        known
        (let [out #js []]
          (set! (.-flood-out s) out)
          (.expandAt s x y z h 0 -1 r)
          (set! (.-flood-out s) nil)
          (when (>= (.-size ^js (.-flood-moves s)) FLOOD-MEMO) (.clear ^js (.-flood-moves s))) ; bounded memory; a flood's front is what asks again
          (.set ^js (.-flood-moves s) memo-key out)
          out))))

  ;; adds region r of the cell (-1: every region, as one node) when a forward move out of it reaches the flood's current
  ;; node; true when it is the start
  (floodNode [s ^js seen ^js queue start-key x y z h r]
    (let [key (.keyOf s x y z (js/Math.max r 0))]
      (if (true? (.has seen key))
        false
        (do
          (set! (.-hit s) (.includes (.floodMovesOf s x y z h r key) (.-flood-target s)))
          (if ^boolean (.-hit s)
            (do (.add seen key)
                (.push queue x y z r)
                (.leakBubble s x y z)
                (== key start-key))
            false)))))

  ;; cells a gap jump could come from: 2..4 along each cardinal, level, one up or one down (a jump up a block)
  (floodAhead [s seen queue start-key]
    (loop [c 0
           n 2
           dy -1]
      (cond
        (== c 4) false
        (> n 4) (recur (inc c) 2 -1)
        (> dy 1) (recur c (inc n) -1)
        ^boolean (.floodVisit s seen queue start-key (+ (.-fx s) (* (aget (.-adx s) c) n)) (+ (.-fy s) dy) (+ (.-fz s) (* (aget (.-adz s) c) n)) c) true
        :else (recur c n (inc dy)))))

  ;; the highest takeoff (dy above the flood's current cell) a move into it can come from: a fall off an edge into surface
  ;; water starts up to maxWaterDrop above it, down a shaft over the water that is free of blocks and water
  (floodTakeoff [s]
    (if-not (and ^boolean (.isWater s (.-fx s) (.-fy s) (.-fz s)) (not ^boolean (.isWater s (.-fx s) (inc (.-fy s)) (.-fz s))))
      (inc (.-max-drop s))
      (loop [k 1]
        (let [id (.stateAt s (.-fx s) (+ (.-fy s) k) (.-fz s))]
          (if (and (<= k (.-c-max-water-drop s)) (not (== id UNLOADED)) (zero? (aget (.-tbl-top s) id)) (not (== (aget (.-tbl-kind s) id) WATER)))
            (recur (inc k))
            (js/Math.max (inc (.-max-drop s)) k))))))

  ;; the predecessors of the flood's current cell; true when the start is among them
  (floodAround [s seen queue start-key]
    (loop [c 0
           dy -1
           top (.floodTakeoff s)]
      (cond
        (== c 8) (.floodAhead s seen queue start-key)
        (> dy top) (recur (inc c) -1 top)
        ^boolean (.floodVisit s seen queue start-key (+ (.-fx s) (aget (.-adx s) c)) (+ (.-fy s) dy) (+ (.-fz s) (aget (.-adz s) c)) -1) true
        :else (recur c (inc dy) top))))

  ;; climbs and falls in the column
  (floodColumn [s seen queue start-key]
    (loop [dy -1]
      (cond
        (> dy (inc (.-max-drop s))) false
        (zero? dy) (recur (inc dy))
        ^boolean (.floodVisit s seen queue start-key (.-fx s) (+ (.-fy s) dy) (.-fz s) -1) true
        :else (recur (inc dy)))))

  (floodRun [s ^js seen ^js queue start-key budget]
    (loop [head 0]
      (if (and (not ^boolean (.-leaked s)) (< head (.-length queue)) (<= (.-size seen) budget))
        (do
          (set! (.-fx s) (aget queue head))
          (set! (.-fy s) (aget queue (+ head 1)))
          (set! (.-fz s) (aget queue (+ head 2)))
          (set! (.-fr s) (aget queue (+ head 3)))
          (set! (.-flood-target s) (if (neg? (.-fr s)) (- -1 (.keyOf s (.-fx s) (.-fy s) (.-fz s) 0)) (.keyOf s (.-fx s) (.-fy s) (.-fz s) (.-fr s))))
          (if (or ^boolean (.floodColumn s seen queue start-key) ^boolean (.floodAround s seen queue start-key))
            true
            (recur (+ head 4))))
        false)))

  ;; Backward flood from the standable goal cells over predecessors: nodes n (a cell and, in a tight cell, a region;
  ;; the queue holds x y z region) with a forward move n -> c. Predecessors are found by running n's own moves, so the
  ;; flood cannot disagree with the search. Slow, but bounded by the budget.
  ;; True when the flood exhausts within budget nodes without meeting the start: nothing reaches the goal.
  ;; False when the budget runs out, the start is met, or the flood leaks. It leaks into an unloaded or out-of-span cell
  ;; (what lies there is unknown).
  (goalEnclosed [s budget sealed]
    (let [start-key (aget (.-node-keys s) 0) ; the start node's key, its region in a tight cell (begin)
          seen (js/Set.)
          queue #js []]
      (set! (.-leaked s) false)
      (.floodSeeds s seen queue)
      (if (true? (.has seen start-key))
        false
        (do
          (set! (.-flooding s) true)
          (set! (.-allow-shut s) true) ; a shut trapdoor is a way through, only dearer: the flood must not call its far side enclosed
          (let [open ^boolean (.floodRun s seen queue start-key budget)
                enclosed (and (not open) (not ^boolean (.-leaked s)) (<= (.-size seen) budget))
                leaks (and enclosed (or (and ^boolean sealed ^boolean (.floodLeaks s queue)) ^boolean (.floodGap s queue)))]
            (set! (.-flooding s) false)
            (set! (.-allow-shut s) false)
            (set! (.-fr s) -1)
            (set! (.-flooded s) (.-size seen))
            (and enclosed (not leaks)))))))

  ;; a body-high free column beside the cell with nothing to stand on within a drop below it: a cliff or a gap, an edge the body
  ;; can walk off to nowhere (a wall top above the floor is no cliff: its drop lands)
  (cliffBeside [s x y z h]
    (loop [c 0]
      (if (== c 4)
        false
        (let [x2 (+ x (aget (.-adx s) c))
              z2 (+ z (aget (.-adz s) c))]
          (if (and ^boolean (.clear s x2 z2 (+ (* y 16) h) (+ (* y 16) h BODY))
                   (loop [dy (- (inc (.-max-drop s)))]
                     (cond
                       (> dy 1) true
                       (>= (.nodeH s x2 (+ y dy) z2) 0) false
                       :else (recur (inc dy)))))
            true
            (recur (inc c)))))))

  ;; a ladder with a gap ends below the climbable cell x y z: climbUp from its cell below the gap turns it away (gap-seen)
  (gapBelow [s x y z]
    (and ^boolean (.freeCell s (.stateAt s x (dec y) z))
         (or ^boolean (.climbHere s x (- y 2) z)
             (and ^boolean (.freeCell s (.stateAt s x (- y 2) z)) ^boolean (.climbHere s x (- y 3) z)))))

  ;; true when a flooded cell is the top of a ladder with a gap: the goal is not walled in, its way in is that ladder, and
  ;; the search says so (ladder-gap: lad-gap, the course whose only way up is such a ladder)
  (floodGap [s ^js queue]
    (loop [head 0]
      (cond
        (>= head (.-length queue)) false
        (and ^boolean (.climbHere s (aget queue head) (aget queue (+ head 1)) (aget queue (+ head 2)))
             ^boolean (.gapBelow s (aget queue head) (aget queue (+ head 1)) (aget queue (+ head 2)))) true
        :else (recur (+ head 4)))))

  ;; true when the flooded region has a cliff edge: the goal sits on an island or above a drop, not in a pocket
  (floodLeaks [s ^js queue]
    (loop [head 0]
      (cond
        (>= head (.-length queue)) false
        ^boolean (.cliffBeside s (aget queue head) (aget queue (+ head 1)) (aget queue (+ head 2))
                               (.nodeH s (aget queue head) (aget queue (+ head 1)) (aget queue (+ head 2)))) true
        :else (recur (+ head 4)))))

  ;; the early pass of the flood, before the first expansion: only a small enclosed goal is caught, so it stays cheap, and
  ;; only a sealed one (no cliff edge beside the flooded cells: a goal on an island or above a drop is left to the search). It
  ;; leaves flooded and expanded alone (the late flood owns them) and reports its size as stats.preFlooded.
  (goalEnclosedEarly [s]
    (let [budget (js/Math.min (.-pre-flood s) (.-goal-flood s))]
      (if (or (not ^boolean (.-near s)) (<= budget 0) ^boolean (.-goal-unloaded s))
        false
        (let [enclosed ^boolean (.goalEnclosed s budget true)]
          (set! (.-pre-flooded s) (.-flooded s))
          (set! (.-flooded s) 0)
          enclosed))))

  ;; ---- the late flood ----
  ;; The late floods and the flood at the end of one search are one backward flood (goalEnclosed's, without the cliff
  ;; test), continued: a run with a bigger budget goes on from where the last one stopped. It is breadth-first over the
  ;; same seeds and moves, so it answers what a fresh flood with that budget would. Cells flooded before are not
  ;; flooded again, and a run can stop and go on later (step's slices).
  (lateFloodBegin [s budget end]
    (set! (.-lf-budget s) budget)
    (set! (.-lf-end s) end)
    (set! (.-lf-active s) true)
    (when (nil? ^js (.-lf-seen s))
      (set! (.-lf-seen s) (js/Set.))
      (set! (.-leaked s) false)
      (when-not ^boolean (.takeFlood s)
        (.freshFlood s))))

  ;; the late flood begun afresh from the goal's cells in this search's world
  (freshFlood [s]
    (set! (.-lf-seen s) (js/Set.))
    (set! (.-lf-queue s) #js [])
    (set! (.-lf-head s) 0)
    (set! (.-lf-imported s) 0)
    (set! (.-leaked s) false)
    (set! (.-lf-active s) true)
    (.floodSeeds s ^js (.-lf-seen s) ^js (.-lf-queue s))
    (when (true? (.has ^js (.-lf-seen s) (aget (.-node-keys s) 0)))
      (set! (.-lf-open s) true)
      (set! (.-lf-seed-open s) true)))

  ;; ---- the late flood kept over the searches of one goal (options.goalFloodMemo) ----
  ;; The backward flood does not depend on the start. Searches toward one goal (go-to's, one after each walk) share one
  ;; memo: each writes its flood, budget and schedule there (keepFlood), the next goes on with them (takeSchedule,
  ;; takeFlood). Cells it took over read an older world, so their enclosed answer is checked by a fresh flood (step). A
  ;; flood that leaked or met the start is dropped. The key holds the options that change the flood's moves (the box,
  ;; avoid and the walker's limits only refuse the search's nodes, not the flood's).
  (goalId [s] (str (.-goal-x s) "," (.-goal-y s) "," (.-goal-z s) "," (.-goal-range s) "," (.-max-drop s) "," (.-c-air-supply s) "," (.-c-air-limit s) "," (.-c-air-drain s) "," (.-c-max-water-drop s)))

  (memoOfGoal [s]
    (and (some? ^js (.-flood-memo s)) (identical? (.-goal ^js (.-flood-memo s)) (.goalId s))))

  (dropMemo [s]
    (set! (.-goal ^js (.-flood-memo s)) nil)
    (set! (.-queue ^js (.-flood-memo s)) nil)
    (set! (.-after ^js (.-flood-memo s)) nil))

  ;; at the first step: the schedule of the earlier searches; a memo of another goal is emptied
  (takeSchedule [s]
    (when (some? ^js (.-flood-memo s))
      (cond
        (not ^boolean (.memoOfGoal s)) (.dropMemo s)
        (some? (.-after ^js (.-flood-memo s))) (do (set! (.-flood-base s) (.-expanded ^js (.-flood-memo s)))
                                         (set! (.-flood-after s) (.-after ^js (.-flood-memo s)))
                                         (set! (.-goal-flood s) (.-budget ^js (.-flood-memo s)))))))

  ;; the kept flood made this search's late flood (its cells keyed afresh: keyOf is relative to the start); false when
  ;; there is none or a cell lies out of this search's span. The start among its cells has met it.
  (takeFlood [s]
    (let [^js q (when ^boolean (.memoOfGoal s) (.-queue ^js (.-flood-memo s)))]
      (if (nil? q)
        false
        (let [n (.-length q)]
          (loop [i 0]
            (cond
              (>= i n) (do (set! (.-lf-queue s) q)
                           (set! (.-lf-head s) (.-head ^js (.-flood-memo s)))
                           (set! (.-lf-imported s) (.-size ^js (.-lf-seen s)))
                           (when (true? (.has ^js (.-lf-seen s) (aget (.-node-keys s) 0))) (set! (.-lf-open s) true))
                           true)
              (not ^boolean (.inSpan s (aget q i) (aget q (+ i 2)))) false
              :else (do (.add ^js (.-lf-seen s) (.keyOf s (aget q i) (aget q (+ i 1)) (aget q (+ i 2)) (js/Math.max 0 (aget q (+ i 3)))))
                        (recur (+ i 4)))))))))

  ;; after each step: the flood and schedule into the memo; a flood that leaked or met the start is dropped
  (keepFlood [s]
    (when (some? ^js (.-flood-memo s))
      (if (or ^boolean (.-leaked s) ^boolean (.-lf-open s))
        (.dropMemo s)
        (do (set! (.-goal ^js (.-flood-memo s)) (.goalId s))
            (set! (.-after ^js (.-flood-memo s)) (.-flood-after s))
            (set! (.-budget ^js (.-flood-memo s)) (.-goal-flood s))
            (set! (.-expanded ^js (.-flood-memo s)) (+ (.-flood-base s) (.-expanded s)))
            (when (some? ^js (.-lf-seen s))
              (set! (.-queue ^js (.-flood-memo s)) ^js (.-lf-queue s))
              (set! (.-head ^js (.-flood-memo s)) (.-lf-head s)))))))

  ;; floods at most about allowance more cells of the run in progress; true when the run is over (the start met, the
  ;; flood exhausted, or past its budget), false when it stopped for the allowance
  (lateFloodRun [s allowance]
    (if ^boolean (.-lf-open s)
      true
      (let [start-key (aget (.-node-keys s) 0)
            limit (+ (.-size ^js (.-lf-seen s)) allowance)]
        (set! (.-flooding s) true)
        (set! (.-allow-shut s) true) ; a shut trapdoor is a way through, only dearer: the flood must not call its far side enclosed
        (let [state (loop []
                      (cond
                        (not (and (not ^boolean (.-leaked s)) (< (.-lf-head s) (.-length ^js (.-lf-queue s))) (<= (.-size ^js (.-lf-seen s)) (.-lf-budget s)))) 0
                        (> (.-size ^js (.-lf-seen s)) limit) 1
                        :else
                        (do
                          (set! (.-fx s) (aget ^js (.-lf-queue s) (.-lf-head s)))
                          (set! (.-fy s) (aget ^js (.-lf-queue s) (+ (.-lf-head s) 1)))
                          (set! (.-fz s) (aget ^js (.-lf-queue s) (+ (.-lf-head s) 2)))
                          (set! (.-fr s) (aget ^js (.-lf-queue s) (+ (.-lf-head s) 3)))
                          (set! (.-flood-target s) (if (neg? (.-fr s)) (- -1 (.keyOf s (.-fx s) (.-fy s) (.-fz s) 0)) (.keyOf s (.-fx s) (.-fy s) (.-fz s) (.-fr s))))
                          (if (or ^boolean (.floodColumn s ^js (.-lf-seen s) ^js (.-lf-queue s) start-key) ^boolean (.floodAround s ^js (.-lf-seen s) ^js (.-lf-queue s) start-key))
                            2
                            (do (set! (.-lf-head s) (+ (.-lf-head s) 4))
                                (recur))))))]
          (set! (.-flooding s) false)
          (set! (.-allow-shut s) false)
          (set! (.-fr s) -1)
          (when (== state 2) (set! (.-lf-open s) true))
          (not (== state 1))))))

  ;; the run that is over: true when the goal is walled in or cut off (the flood exhausted within its budget, neither meeting the start
  ;; nor leaking; cut-off says it has a cliff edge); flooded is the flood's size (left alone when the start is one of the goal's own cells, as goalEnclosed)
  (lateFloodEnd [s]
    (set! (.-lf-active s) false)
    (set! (.-verifying s) false)
    (when-not ^boolean (.-lf-seed-open s) (set! (.-flooded s) (.-size ^js (.-lf-seen s))))
    (let [enclosed (and (not ^boolean (.-lf-open s)) (not ^boolean (.-leaked s)) (<= (.-size ^js (.-lf-seen s)) (.-lf-budget s)) (not ^boolean (.floodGap s ^js (.-lf-queue s))))]
      (set! (.-cut-off s) (and enclosed ^boolean (.floodLeaks s ^js (.-lf-queue s))))
      enclosed)))
