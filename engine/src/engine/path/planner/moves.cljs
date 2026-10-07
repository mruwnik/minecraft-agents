(ns engine.path.planner.moves
  "Search methods: the moves out of a node: walks, diagonals, jumps, drops, gap jumps and climbing."
  (:require [engine.path.planner.base :refer [ARC ARC-UP BODY CLIMB-TRAP-SHUT CORNER-S GAP-PIT-RISK GAP-S GAP-UP-S GRID HAZARD-SLIDE-RISK JUMP-S JUMP-UP LAVA MOVE-CLIMB-DOWN MOVE-CLIMB-UP MOVE-CORNER MOVE-DIAGONAL MOVE-DROP MOVE-EXIT MOVE-GAP MOVE-JUMP MOVE-JUMP-CLIMB MOVE-OPEN MOVE-SWIM MOVE-WALK SLOW-EXTRA SNAP SPRINT-S SQRT2 STEP UNLOADED WALK-S WATER WHOLE FREE-FALL]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; ---- moves ----

  ;; the standable cell beside the feet: level, one up, or a step down of at most STEP; else -1 (a bigger fall is a drop).
  ;; `ty` is the y of the cell found.
  (neighbour [s x2 z2 y h0]
    (set! (.-ty s) y)
    (let [level (.landing s x2 y z2)]
      (if (>= level 0)
        level
        (do
          (set! (.-ty s) (inc y))
          (let [up (.landing s x2 (inc y) z2)]
            (if (>= up 0)
              up
              (do
                (set! (.-ty s) (dec y))
                (let [down (.landing s x2 (dec y) z2)
                      lo (+ (* (dec y) 16) down)]
                  (cond
                    (or (neg? down) (< (- lo h0) (- STEP))) -1
                    ;; the body also leaves the higher level through this column (a tight cell's mask checks that itself)
                    (or ^boolean (.tightAt s x2 (dec y) z2) ^boolean (.clear s x2 z2 lo (+ h0 BODY))) down
                    :else -1)))))))))

  ;; The step after leaving a soul-sand column sideways must not be into a magma column cell, unless the path is going down it
  ;; (the goal well below the cell): the body slips in.
  (magmaTrap [s x y z]
    (and ^boolean (.-after-exit s) (== (aget (.-tbl-bubble s) (.stateAt ^js (.-snapshot s) x y z)) 2) (not (< (.-goal-y s) (- y 1)))))

  (dropIntoWater [s i x y z h region c x2 y2 z2 h0 slow-from ^boolean tight-src]
    (let [fall (- h0 (* y2 16))]
      (when-not (or (< (.swimAt s x2 y2 z2) 0) (> fall (* (.-c-max-water-drop s) 16)))
        (let [sec (+ (* WALK-S (+ 1 (* SLOW-EXTRA slow-from))) (* 0.25 (js/Math.sqrt (/ fall 16))))]
          (if (or tight-src ^boolean (.isTight s x2 y2 z2))
            (.tightMove s i x y z h region c x2 y2 z2 0 MOVE-DROP sec (.swimRisk s x2 y2 z2) 0 SNAP 0)
            (.edge s x2 y2 z2 0 MOVE-DROP i sec (.swimRisk s x2 y2 z2) 0 0 0))))))

  ;; the hp a fall of fall16 (1/16 blocks) onto the block `id` (the support of the landing) takes: over FREE-FALL blocks, scaled by the block's landing factor
  ;; (options.landing), the fall factor and the drop factor; -1 when the block takes no fall over FREE-FALL (a negative factor)
  (dropDamage [s id fall16]
    (let [over (- (/ fall16 16) FREE-FALL)]
      (if (<= over 0)
        0
        (let [land (if (some? (.-land-factors s)) (.get ^js (.-land-factors s) id) nil)
              f (if (some? land) land 1)]
          (if (neg? f)
            -1
            (* (.-c-drop-factor s) (.-fall-factor s) (js/Math.ceil (* over f))))))))

  ;; walk off an edge into the first standable cell below the neighbour column, or into water of any depth up to maxWaterDrop
  ;; (the fall is cancelled there). A tight cell at either end: the body falls straight down from the crossing point, which
  ;; the masks must leave free all the way (a climbable below is grabbed as it falls past, so it takes any position).
  (expandDrop [s i x y z h region c x2 z2 h0 slow-from ^boolean tight-src]
    (loop [y2 (dec y)]
      (when (>= y2 (- y (.-c-max-water-drop s) 1))
        (let [id (.stateAt s x2 y2 z2)]
          (when-not (or (== id UNLOADED) (== (aget (.-tbl-kind s) id) LAVA) ^boolean (.avoids s id x2 y2 z2))
            (if (== (aget (.-tbl-kind s) id) WATER)
              (when-not ^boolean (.magmaTrap s x2 y2 z2)
                (.dropIntoWater s i x y z h region c x2 y2 z2 h0 slow-from tight-src))
              (let [h1 (.landing s x2 y2 z2)]
                (if (neg? h1)
                  (when-not (pos? (aget (.-tbl-top s) id)) (recur (dec y2)))
                  (let [sup (.-support s)
                        tight-drop (or tight-src ^boolean (.isTight s x2 y2 z2))
                        fall (- h0 (+ (* y2 16) h1))
                        dmg (.dropDamage s sup fall)]
                    (when-not (or (> fall (* (.-max-drop s) 16)) (neg? dmg))
                      (let [sec (+ (* WALK-S (+ 1 (* SLOW-EXTRA (+ slow-from (.-enter-slow s)))))
                                   (* (.-c-drop-factor s) 0.25 (js/Math.sqrt (/ (js/Math.max 0 fall) 16)))
                                   (.-enter-extra s))]
                        (set! (.-move-dmg s) (+ dmg (.-enter-dmg s)))
                        (if tight-drop
                          (.tightMove s i x y z h region c x2 y2 z2 h1 MOVE-DROP sec (+ (.-enter-risk s) dmg) (.-enter-slow s)
                                      SNAP (if ^boolean (.climbHere s x2 y2 z2) GRID 0))
                          (.edge s x2 y2 z2 h1 MOVE-DROP i sec (+ (.-enter-risk s) dmg) (.-enter-slow s) 0 0))
                        (set! (.-move-dmg s) 0))))))))))))

  ;; where a gap jump lands: level, else one up (when the higher arc is clear), else one down; `gap-y` is the cell's y
  (gapLanding [s lx y lz ^boolean up]
    (set! (.-gap-y s) y)
    (let [h1 (.landing s lx y lz)]
      (cond
        (>= h1 0) h1
        up (.gapLandingUp s lx y lz)
        :else (.gapLandingDown s lx y lz))))

  (gapLandingUp [s lx y lz]
    (set! (.-gap-y s) (inc y))
    (let [h1 (.landing s lx (inc y) lz)]
      (if (>= h1 0)
        h1
        (.gapLandingDown s lx y lz))))

  (gapLandingDown [s lx y lz]
    (set! (.-gap-y s) (dec y))
    (.landing s lx (dec y) lz))

  ;; sprint across 1..3 empty cells in a cardinal line, landing level or one lower; across 1 or 2, also up to one block
  ;; higher. A jump over 3 above a pit (pitBelow) has GAP-PIT-RISK: one that falls short traps the body, so a short way round
  ;; is taken instead
  (expandGap [s i x y z c h0]
    (let [dx (aget (.-adx s) c)
          dz (aget (.-adz s) c)]
      (loop [n 1
             hole 0
             ;; some gap cell so far is over a pit (pitBelow): a jump over 3 then carries GAP-PIT-RISK
             pit false
             ;; a jump up needs the higher arc over the start and every gap cell
             up-arc ^boolean (.clear s x z h0 (+ h0 ARC-UP))]
        (when (<= n 3)
          (let [gx (+ x (* dx n))
                gz (+ z (* dz n))]
            (when-not (and (> n 1) (or (>= (.landing s gx y gz) 0) (>= (.landing s gx (inc y) gz) 0)))
              (when ^boolean (.clear s gx gz h0 (+ h0 ARC))
                (let [up-arc (and up-arc ^boolean (.clear s gx gz h0 (+ h0 ARC-UP)))
                      hole (js/Math.max hole (.holeRisk s gx y gz))
                      pit (or pit ^boolean (.pitBelow s gx y gz))
                      risk (if (and (== n 3) pit) (js/Math.max hole GAP-PIT-RISK) hole)
                      lx (+ gx dx)
                      lz (+ gz dz)
                      up (and (<= n 2) up-arc)
                      h1 (.gapLanding s lx y lz up)
                      ly (.-gap-y s)]
                  (when-not (or (neg? h1) ^boolean (.isTight s lx ly lz))
                    (let [delta (- (+ (* ly 16) h1) h0)]
                      (when-not (or (< delta -16) (> delta (if up WHOLE 0))
                                    (and (some? ^js (.-limit-gap s))
                                         ;; (the goal flood expands cells nothing reached: i is -1, the takeoff a plain walk)
                                         (not (true? (^js (.-limit-gap s) x y z (- h0 (* y 16)) (if (neg? i) MOVE-WALK (aget (.-moves s) i)) lx ly lz h1)))
                                         (do (set! (.-limit-refused s) true) true)))
                        (set! (.-move-dmg s) (.-enter-dmg s))
                        (.edge s lx ly lz h1 MOVE-GAP i
                               (+ (* (inc n) SPRINT-S) GAP-S (if (pos? delta) GAP-UP-S 0) (.-enter-extra s))
                               (+ (.-enter-risk s) risk) (.-enter-slow s) 0 0)
                        (set! (.-move-dmg s) 0))))
                  (recur (inc n) hole pit up-arc)))))))))

  (expandCardinal [s x y z h slow-from i region c h0 ^boolean tight-src ^boolean climbing]
    (let [x2 (+ x (aget (.-adx s) c))
          z2 (+ z (aget (.-adz s) c))
          h1 (.neighbour s x2 z2 y h0)]
      (if (>= h1 0)
        (let [y2 (.-ty s)
              delta (- (+ (* y2 16) h1) h0)
              walk (* WALK-S (+ 1 (* SLOW-EXTRA (+ slow-from (.-enter-slow s)))))
              ;; climbing a stairs block in its direction is a walk, though the node above it is a whole block up
              climbs (and (== (aget (.-tbl-stair s) (.-support s)) (inc c)) (<= delta WHOLE))
              ;; (the body steps off a climbable without a jump: it is already rising)
              walks (or (<= delta STEP) climbs (and climbing (<= delta JUMP-UP)))]
          ;; a jump needs headroom over the start column; a tight start's mask checks that itself
          (when (and (or walks (and (<= delta JUMP-UP) (or tight-src ^boolean (.clear s x z h0 (+ (* y2 16) h1 BODY)))))
                     ;; a walk up lifts the body into the slab above its old top: that must be free in the start column
                     (or (not walks) (<= delta 0) tight-src climbing ^boolean (.clear s x z (+ h0 BODY) (+ (* y2 16) h1 BODY))))
            (let [sec (+ (if walks walk (+ walk JUMP-S)) (.-enter-extra s))
                  move (if walks MOVE-WALK MOVE-JUMP)]
              (set! (.-move-dmg s) (.-enter-dmg s))
              (if (or tight-src ^boolean (.tightAt s x2 y2 z2))
                (.tightMove s i x y z h region c x2 y2 z2 h1 move sec (.-enter-risk s) (.-enter-slow s)
                            (if (and climbing (> delta STEP)) GRID SNAP) SNAP)
                (.edge s x2 y2 z2 h1 move i sec (.-enter-risk s) (.-enter-slow s) 0 0))
              (set! (.-move-dmg s) 0))))
        (if (>= (.swimAt s x2 y z2) 0)
          ;; water ahead at our level: walk in and swim
          (when-not ^boolean (.magmaTrap s x2 y z2)
            (let [risk (.swimRisk s x2 y z2)
                  tight (or tight-src ^boolean (.tightAt s x2 y z2))
                  base (.-c-swim-h s)
                  extra (if (== (aget (.-tbl-flowing s) (.stateAt ^js (.-snapshot s) x2 y z2)) 1) (.-c-current s) 0)
                  sec (+ base extra (.dragPrice s x2 y z2))]
              (when ^boolean (.swimBegin s i true x2 y z2 base extra false)
                (if tight
                  (.tightMove s i x y z h region c x2 y z2 0 MOVE-SWIM sec risk 0 SNAP SNAP)
                  (.edge s x2 y z2 0 MOVE-SWIM i sec risk 0 0 0))
                (.swimEnd s))))
          ;; no ground ahead at our level: the body must at least fit in the column to leave the edge
          (when ^boolean (.clear s x2 z2 h0 (+ h0 BODY))
            (.expandDrop s i x y z h region c x2 z2 h0 slow-from tight-src)
            ;; no gap jumps out of a tight cell, none over a door (in the opening pass)
            (when (and (not tight-src) (not ^boolean (.-open-mode s)))
              (.expandGap s i x y z c h0)))))))

  (expandDiagonal [s x y z slow-from i c h0]
    (let [dx (aget (.-adx s) c)
          dz (aget (.-adz s) c)
          x2 (+ x dx)
          z2 (+ z dz)
          h1 (.neighbour s x2 z2 y h0)
          y2 (.-ty s)]
      (when (and (>= h1 0) (not ^boolean (.tightAt s x2 y2 z2)))
        (let [h2 (+ (* y2 16) h1)
              jump (> (- h2 h0) STEP)]
          (when (<= (- h2 h0) JUMP-UP)
            ;; The 0.62 wide body brushes both side cells near their shared corner, for its whole height. Neither may
            ;; hold anything it must not touch (water or a hole is fine). One side holding plain collision is a
            ;; corner slide: the body presses on it and slides over the other side cell. If that has no floor the
            ;; body dips, and the step height lifts it out.
            (let [lo (js/Math.min h0 h2)
                  hi (+ (js/Math.max h0 h2) BODY)
                  sa (.side s x2 z lo hi)
                  sb (.side s x z2 lo hi)
                  slide (+ sa sb)] ; 1 when exactly one side is blocked
              (when (and (< slide 2)
                         (not ^boolean (.slipsOnFarmland s x2 z y))
                         (not ^boolean (.slipsOnFarmland s x z2 y))
                         ;; a step up lifts the body: its start column must have the room
                         (or (<= h2 h0) ^boolean (.clear s x z (if jump h0 (+ h0 BODY)) hi))
                         ;; (the walker's test of a jump that slides along a corner: limit-corner)
                         (or (not jump) (zero? slide) (nil? ^js (.-limit-corner s))
                             (true? (^js (.-limit-corner s) x y z (- h0 (* y 16)) x2 y2 z2 h1))
                             (do (set! (.-limit-refused s) true) false)))
                (let [walk (+ (* WALK-S SQRT2 (+ 1 (* SLOW-EXTRA (+ slow-from (.-enter-slow s))))) (* slide CORNER-S))
                      touched (+ (.sideTouch s x2 z lo hi) (.sideTouch s x z2 lo hi))
                      brushed (+ touched
                                 ;; a slide over a hole onto lava or fire (the open side is x2 z when sb holds the corner)
                                 (if (== slide 1)
                                   (* HAZARD-SLIDE-RISK (if (zero? sa) (.slideHoleRisk s x2 z lo) (.slideHoleRisk s x z2 lo)))
                                   0))]
                  (set! (.-move-dmg s) (+ (.-enter-dmg s) touched))
                  (.edge s x2 y2 z2 h1
                         (cond jump MOVE-JUMP (== slide 1) MOVE-CORNER :else MOVE-DIAGONAL)
                         i (+ (if jump (+ walk JUMP-S) walk) (.-enter-extra s)) (+ (.-enter-risk s) brushed) (.-enter-slow s) slide 0)
                  (set! (.-move-dmg s) 0)))))))))

  ;; region -1: every region of a tight cell (the goal flood does not know which one it comes from)
  (expandMoves [s x y z h slow-from i region]
    (if (== (aget (.-tbl-kind s) (.stateAt ^js (.-snapshot s) x y z)) WATER)
      (.expandSwim s x y z i region)
      (let [h0 (+ (* y 16) h)
            tight-src ^boolean (.tightAt s x y z)
            ;; (the same quiet test as for tight cells: no climbable within reach of the cell either)
            climbing (and (not ^boolean (.-quiet s)) ^boolean (.climbHere s x y z))]
        (set! (.-after-exit s) (and (>= i 0) (== (aget (.-moves s) i) MOVE-EXIT)))
        (if climbing
          (do (.climbUp s i x y z h region)
              (.climbDown s i x y z h region))
          (when-not ^boolean (.-quiet s)
            (.jumpClimb s i x y z h region)
            ;; standing on scaffolding: sneak down into it
            (when ^boolean (.climbHere s x (dec y) z)
              (.climbDown s i x y z h region))))
        (loop [c 0]
          (when (< c 4)
            (.expandCardinal s x y z h slow-from i region c h0 tight-src climbing)
            (recur (inc c))))
        (when-not tight-src
          (loop [c 4]
            (when (< c 8)
              (.expandDiagonal s x y z slow-from i c h0)
              (recur (inc c))))))))

  ;; ---- climbing ----

  ;; the cell (x, y2, z) as the next node of a vertical move: standH with a shut trapdoor allowed (its cost added), or -1
  (enterCell [s x y2 z]
    (let [was ^boolean (.-allow-shut s)]
      (set! (.-allow-shut s) true)
      (set! (.-enters-shut s) ^boolean (.shutAt s x y2 z))
      (let [h2 (.landing s x y2 z)]
        (set! (.-allow-shut s) was)
        h2)))

  (freeCell [s id]
    (and (not (== id UNLOADED)) (zero? (aget (.-tbl-top s) id)) (not (== (aget (.-tbl-kind s) id) WATER)) (not (== (aget (.-tbl-kind s) id) LAVA))))

  ;; a climbable one block up (or the deck of scaffolding, or a trapdoor to open); a gap above refuses the climb
  (climbUp [s i x y z h region]
    (let [h2 (.enterCell s x (inc y) z)]
      (if (neg? h2)
        (loop [k 2]
          (when (and (<= k 3) (not ^boolean (.-gap-seen s)))
            (set! (.-gap-seen s) (and ^boolean (.climbHere s x (+ y k) z) ^boolean (.freeCell s (.stateAt s x (inc y) z))
                                (or (== k 2) ^boolean (.freeCell s (.stateAt s x (+ y 2) z)))))
            (recur (inc k))))
        (let [sec (+ (.-c-climb-up s) (if ^boolean (.-enters-shut s) (.-c-open s) 0))]
          (when ^boolean (.-enters-shut s) (set! (.-move-open s) (.push ^js (.-open-lists s) #js [#js {:x x :y (inc y) :z z}])))
          (.verticalMove s i x y z h region (inc y) h2 (if ^boolean (.-enters-shut s) MOVE-OPEN MOVE-CLIMB-UP) sec (.-enter-risk s) (.-enter-slow s))
          (set! (.-move-open s) 0)))))

  ;; a climbable or standable cell one block down; else, through free cells, a short fall onto the first climbable or floor
  (climbDown [s i x y z h region]
    (let [h2 (.enterCell s x (dec y) z)]
      (if (>= h2 0)
        (let [sec (+ (.-c-climb-down s) (if ^boolean (.-enters-shut s) (.-c-open s) 0))]
          (when ^boolean (.-enters-shut s) (set! (.-move-open s) (.push ^js (.-open-lists s) #js [#js {:x x :y (dec y) :z z}])))
          (.verticalMove s i x y z h region (dec y) h2 (if ^boolean (.-enters-shut s) MOVE-OPEN MOVE-CLIMB-DOWN) sec (.-enter-risk s) (.-enter-slow s))
          (set! (.-move-open s) 0))
        (let [free (.stateAt s x (dec y) z)]
          (when-not (or (== free UNLOADED) (pos? (aget (.-tbl-top s) free)) (== (aget (.-tbl-kind s) free) WATER) (== (aget (.-tbl-kind s) free) LAVA)
                        ^boolean (.avoids s free x (dec y) z))
            (.fallThrough s i x y z h region (+ (* y 16) h)))))))

  (fallThrough [s i x y z h region from16]
    (loop [y3 (- y 2)]
      (when (>= y3 (- y (.-max-drop s) 1))
        (let [id (.stateAt s x y3 z)]
          (when-not (or (== id UNLOADED) (== (aget (.-tbl-kind s) id) LAVA) (== (aget (.-tbl-kind s) id) WATER) ^boolean (.avoids s id x y3 z))
            (let [h3 (.landing s x y3 z)]
              (if (neg? h3)
                (when-not (pos? (aget (.-tbl-top s) id)) (recur (dec y3)))
                (let [dmg (.dropDamage s (.-support s) (- from16 (+ (* y3 16) h3)))
                      fall (- from16 (+ (* y3 16) h3))]
                  (when-not (or (> fall (* (.-max-drop s) 16)) (neg? dmg))
                    (set! (.-move-dmg s) (+ dmg (.-enter-dmg s)))
                    (.verticalMove s i x y z h region y3 h3 MOVE-DROP (* (.-c-drop-factor s) 0.25 (js/Math.sqrt (/ fall 16)))
                                   (+ (.-enter-risk s) dmg) (.-enter-slow s))
                    (set! (.-move-dmg s) 0))))))))))

  ;; from the floor, a jump puts the feet into a climbable one block up
  (jumpClimb [s i x y z h region]
    (let [id (.stateAt ^js (.-snapshot s) x (inc y) z)]
      (when-not (or (== id UNLOADED) (zero? (aget (.-tbl-climb s) id)) (== (aget (.-tbl-climb s) id) CLIMB-TRAP-SHUT))
        (let [h2 (.landing s x (inc y) z)]
          (when-not (neg? h2)
            (.verticalMove s i x y z h region (inc y) h2 MOVE-JUMP-CLIMB (.-c-jump-climb s) (.-enter-risk s) (.-enter-slow s))))))))
