(ns engine.path.planner.moves
  "Search methods: the moves out of a node: walks, diagonals, jumps, drops, gap jumps and climbing."
  (:require [engine.path.planner.base :refer [CLIMB-FALL-P CLIMB-FALL-SCAN FULL-HEALTH ARC ARC-UP BODY CLIMB-TRAP-SHUT CORNER-S GAP-PIT-RISK GAP-UP-MISS GAP-S GAP-UP-S GRID HAZARD-SLIDE-RISK JUMP-S JUMP-UP LAVA MOVE-CLIMB-DOWN MOVE-CLIMB-UP MOVE-CORNER MOVE-DIAGONAL MOVE-DROP MOVE-GAP MOVE-JUMP MOVE-JUMP-CLIMB MOVE-OPEN MOVE-SWIM MOVE-WALK OPENABLE SLOW-EXTRA SNAP SQRT2 STEP UNLOADED WATER WHOLE FREE-FALL STALAGMITE-FACTOR STALAGMITE-FREE-FALL BOUNCE-S pad-cells]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(defn- run-move? [m] (or (== m MOVE-WALK) (== m MOVE-DIAGONAL)))

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

  ;; Remembered water may be gone since: masked (options.unknown), the drop adds the chance it changed of the dry fall to
  ;; the pool's floor (gone); else water the body has not seen lately is landed in only as far as ground would be.
  (dropIntoWater [s i x y z h region c x2 y2 z2 h0 slow-from ^boolean tight-src]
    (let [fall (- h0 (* y2 16))
          masked (>= (.-unk-id s) 0)
          reach (if (or masked ^boolean (.landSeen s x2 y2 z2 false)) (.-c-max-water-drop s) (js/Math.min (.-c-max-water-drop s) (.-max-drop s)))]
      (when-not (or (< (.swimAt s x2 y2 z2) 0) (> fall (* reach 16)) (not ^boolean (.fallStops s x2 y2 z2)))
        (let [sec (+ (* (.-c-walk-s s) (+ 1 (* SLOW-EXTRA slow-from))) (* 0.25 (js/Math.sqrt (/ fall 16))))
              risk (+ (.swimRisk s x2 y2 z2) (if masked (.drainedRisk s x2 y2 z2 h0) 0))]
          (if (or tight-src ^boolean (.isTight s x2 y2 z2))
            (.tightMove s i x y z h region c x2 y2 z2 0 MOVE-DROP sec risk 0 SNAP 0)
            (.edge s x2 y2 z2 0 MOVE-DROP i sec risk 0 0 0))))))

  ;; hp of risk a fall from h0 into the remembered water at x y z takes should it be gone (options.unknown.changed): its
  ;; chance times the dry fall's damage onto the floor under the water's column, or unk-death when that fall kills
  (drainedRisk [s x y z h0]
    (let [q (.changedAt s x y z)]
      (if-not (pos? q)
        0
        (let [bottom (loop [yb y] (if (and (> yb (- y 64)) (== (aget (.-tbl-kind s) (.stateAt s x (dec yb) z)) WATER)) (recur (dec yb)) yb))
              dmg (.dropDamage s 1 (- h0 (* bottom 16)))]
          (* q (if (>= dmg (.-lethal-hp s)) (.-unk-death s) dmg))))))

  ;; the water at x y z holds a body that falls into it: not falling water (a waterfall, which the body drops out of), and
  ;; held up by a floor or by water that is not falling (unseen or unloaded below: a pool's bottom, taken as held)
  (fallStops [s x y z]
    (let [below (.stateAt s x (dec y) z)]
      (and (not (== (aget (.-tbl-flowing s) (.stateAt s x y z)) 2))
           (or (== below UNLOADED) (== below (.-unk-id s)) (pos? (aget (.-tbl-top s) below))
               (and (== (aget (.-tbl-kind s) below) WATER) (not (== (aget (.-tbl-flowing s) below) 2)))))))

  ;; the hp a fall of fall16 (1/16 blocks) takes onto a block of landing factor f (landFactor): over FREE-FALL blocks, scaled
  ;; by f, the fall factor and the drop factor; a negative f is a bounce: no damage (see bounceS)
  (dropDamage [s f fall16]
    (let [over (- (/ fall16 16) (if (== f STALAGMITE-FACTOR) STALAGMITE-FREE-FALL FREE-FALL))]
      (if (or (<= over 0) (neg? f))
        0
        (* (.-c-drop-factor s) (.-fall-factor s) (js/Math.ceil (* over f))))))

  ;; the seconds a fall of fall16 onto a block of landing factor f takes to bounce away when f is negative, else 0
  (bounceS [s f fall16]
    (if (and (neg? f) (> fall16 (* FREE-FALL 16)))
      (* BOUNCE-S (js/Math.sqrt (/ fall16 16)))
      0))

  ;; the landing factor (options.landing, 1 when none) of `sup`, the support of a landing in the cell x y z (the cell's own
  ;; block, else the one under it) after a fall of fall16: a negative one (a bounce) only on a pad the bounce stays on
  ;; (bouncePad), else 1, the full fall
  (landFactor [s sup x y z fall16]
    (let [land (if (some? (.-land-factors s)) (.get ^js (.-land-factors s) sup) nil)]
      (cond
        (== (aget (.-tbl-farmland s) sup) 3) STALAGMITE-FACTOR
        (nil? land) 1
        (and (neg? land) (not ^boolean (.bouncePad s x (if (== (.stateAt s x y z) sup) y (dec y)) z fall16))) 1
        :else land)))

  ;; the body sees the block at x y z now (options.landingSeen; none: every block); wall? asks it as a wall of the pad,
  ;; which a hidden block may be taken for
  (landSeen [s x y z wall?]
    (or (nil? (.-land-seen s)) (true? (.call ^js (.-land-seen s) nil x y z wall?))))

  ;; a bounce off the block at x y z after a fall of fall16 stays on the pad (base/pad-cells): it and the 8 blocks round it
  ;; are bouncing blocks or (round it) walls up to the bounce's peak, and the body sees each
  (bouncePad [s x y z fall16]
    (some? (pad-cells (fn [bx by bz]
                        (let [id (.stateAt s bx by bz)
                              land (if (== id UNLOADED) nil (.get ^js (.-land-factors s) id))]
                          (and (some? land) (neg? land) ^boolean (.landSeen s bx by bz false))))
                      (fn [bx by bz] (.wallAt s bx by bz))
                      x y z (/ fall16 16))))

  ;; a seen block at x y z with a whole collision top
  (wallAt [s x y z]
    (let [id (.stateAt s x y z)]
      (and (not (== id UNLOADED)) (>= (aget (.-tbl-top s) id) WHOLE) ^boolean (.landSeen s x y z true))))

  ;; walk off an edge into the first standable cell below the neighbour column, or into water that holds a fall (fallStops) up to maxWaterDrop
  ;; (the fall is cancelled there). A tight cell at either end: the body falls straight down from the crossing point, which
  ;; the masks must leave free all the way (a climbable below is grabbed as it falls past, so it takes any position).
  ;; No fall goes into an unseen cell (options.unknown): it is the phantom floor of the landing above it.
  (expandDrop [s i x y z h region c x2 z2 h0 slow-from ^boolean tight-src]
    (loop [y2 (dec y)]
      (when (>= y2 (- y (.-c-max-water-drop s) 1))
        (let [id (.stateAt s x2 y2 z2)]
          (when-not (or (== id UNLOADED) (== id (.-unk-id s)) (== (aget (.-tbl-kind s) id) LAVA) ^boolean (.avoids s id x2 y2 z2))
            (if (== (aget (.-tbl-kind s) id) WATER)
              (.dropIntoWater s i x y z h region c x2 y2 z2 h0 slow-from tight-src)
              (let [h1 (.landing s x2 y2 z2)]
                (if (neg? h1)
                  (when-not (pos? (aget (.-tbl-top s) id)) (recur (dec y2)))
                  (let [sup (.-support s)
                        tight-drop (or tight-src ^boolean (.isTight s x2 y2 z2))
                        fall (- h0 (+ (* y2 16) h1))
                        f (.landFactor s sup x2 y2 z2 fall)
                        dmg (.dropDamage s f fall)]
                    (when-not (> fall (* (.-max-drop s) 16))
                      (let [sec (+ (* (.-c-walk-s s) (+ 1 (* SLOW-EXTRA (+ slow-from (.-enter-slow s)))))
                                   (* (.-c-drop-factor s) 0.25 (js/Math.sqrt (/ (js/Math.max 0 fall) 16)))
                                   (.bounceS s f fall)
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

  ;; an unseen cell (options.unknown) in the column x, z over the body span lo..hi (the cells `clear` reads): a block hidden
  ;; there would stop a jump
  (unseenSpan [s x z lo hi]
    (let [last-y (bit-shift-right (dec hi) 4)]
      (loop [y (bit-shift-right lo 4)]
        (cond
          (> y last-y) false
          ^boolean (.isUnknown s (.stateAt ^js (.-snapshot s) x y z)) true
          :else (recur (inc y))))))

  ;; the fall (1/16 blocks) from from16 down the column x z, from cell y, to the first block with a top (not a decaying one
  ;; when skip-decay: it goes under the body); 0 into water that holds it (fallStops); CLIMB-FALL-SCAN blocks past unloaded
  ;; land or an unseen cell (the depth behind a ledge is hidden by the ledge itself: a fall into the unknown may kill) and
  ;; past that many blocks of air
  (fallScan [s x y z from16 ^boolean skip-decay]
    (loop [y3 y]
      (if (< y3 (- y CLIMB-FALL-SCAN))
        (- from16 (* y3 16))
        (let [id (.stateAt s x y3 z)]
          (cond
            (or (== id UNLOADED) (== id (.-unk-id s))) (* CLIMB-FALL-SCAN 16)
            (and (== (aget (.-tbl-kind s) id) WATER) ^boolean (.fallStops s x y3 z)) 0
            (and (pos? (aget (.-tbl-top s) id)) (not (and skip-decay (== (aget (.-tbl-decays s) id) 1))))
            (js/Math.max 0 (- from16 (+ (* y3 16) (aget (.-tbl-top s) id))))
            :else (recur (dec y3)))))))

  (missFall [s x y z from16] (.fallScan s x y z from16 false))

  ;; a running jump over 2 or 3 cells along dx dz to lx ly lz (stand h1) lands up to a block past its aim with the run's
  ;; speed, so a landing one cell deep is overshot: true when the column past it has no wall to stop the body and a fall
  ;; there reaches options.health
  (gapOvershootLethal [s dx dz lx ly lz h1]
    (let [land16 (+ (* ly 16) h1)
          ox (+ lx dx)
          oz (+ lz dz)]
      (and ^boolean (.clear s ox oz land16 (+ land16 BODY))
           (>= (.dropDamage s 1 (.missFall s ox ly oz land16)) (.-lethal-hp s)))))

  ;; a body that falls short of a gap jump from h0 into the gap cell x y z dies: the fall down it (missFall) reaches
  ;; options.health. An unseen depth counts deep: the takeoff's own ledge hides the column under the gap.
  (gapShortLethal [s x y z h0]
    (>= (.dropDamage s 1 (.missFall s x y z h0)) (.-lethal-hp s)))

  (openableAt [s x y z]
    (or (== (aget (.-tbl-kind s) (.stateAt ^js (.-snapshot s) x y z)) OPENABLE)
        (== (aget (.-tbl-kind s) (.stateAt ^js (.-snapshot s) x (inc y) z)) OPENABLE)))

  ;; the closed openable blocks in the n cells a gap jump from x y z along dx dz crosses
  (gapDoors [s x y z dx dz n h]
    (let [out #js []]
      (dotimes [k n]
        (.forEach ^js (.closedIn s (+ x (* dx (inc k))) y (+ z (* dz (inc k))) h) (fn [b] (.push out b))))
      out))

  ;; sprint across 1..3 empty cells in a cardinal line, landing level or one lower; across 1 or 2, also up to one block
  ;; higher. A jump over 3 above a pit (pitBelow) has GAP-PIT-RISK: one that falls short traps the body, so a short way round
  ;; is taken instead. One over a hole whose fall kills (gapShortLethal) adds gap-death, the chance of a miss times the price
  ;; of a death: it is jumped only when no other way gets there. A running jump (over 2 or 3) whose overshoot past its
  ;; landing is lethal (gapOvershootLethal) is refused, as a lethal climb is.
  (expandGap [s i x y z c h0]
    (let [dx (aget (.-adx s) c)
          dz (aget (.-adz s) c)]
      (loop [n 1
             hole 0
             ;; some gap cell so far is over a pit (pitBelow): a jump over 3 then carries GAP-PIT-RISK
             pit false
             ;; some gap cell so far falls to a death (gapShortLethal)
             deadly false
             ;; the most damage a fall short into some gap cell so far takes
             miss 0
             ;; a jump up needs the higher arc over the start and every gap cell
             up-arc (and ^boolean (.clear s x z h0 (+ h0 ARC-UP)) (not ^boolean (.unseenSpan s x z h0 (+ h0 ARC-UP))))]
        (when (<= n 3)
          (let [gx (+ x (* dx n))
                gz (+ z (* dz n))]
            ;; (a phantom floor, options.unknown, is no landing: the jump may pass over it)
            ;; (an unseen cell in the arc's headroom ends the gap loop: a block hidden there would stop the jump over the
            ;; hole; an unseen floor under it is no matter)
            (when-not (or ^boolean (.unseenSpan s gx gz h0 (+ h0 ARC))
                          (and (> n 1) (or (and (>= (.landing s gx y gz) 0) (not ^boolean (.phantomAt s gx y gz)))
                                       (and (>= (.landing s gx (inc y) gz) 0) (not ^boolean (.phantomAt s gx (inc y) gz))))))
              (when ^boolean (.clear s gx gz h0 (+ h0 ARC))
                (let [up-arc (and up-arc ^boolean (.clear s gx gz h0 (+ h0 ARC-UP)) (not ^boolean (.unseenSpan s gx gz h0 (+ h0 ARC-UP))))
                      hole (js/Math.max hole (.holeRisk s gx y gz))
                      deadly (or deadly ^boolean (.gapShortLethal s gx y gz h0))
                      pit (or pit ^boolean (.pitBelow s gx y gz))
                      miss (js/Math.max miss (.dropDamage s 1 (.missFall s gx y gz h0)))
                      lx (+ gx dx)
                      lz (+ gz dz)
                      up (and (<= n 2) up-arc)
                      h1 (.gapLanding s lx y lz up)
                      ly (.-gap-y s)]
                  (when-not (or (neg? h1) ^boolean (.isTight s lx ly lz))
                    (let [delta (- (+ (* ly 16) h1) h0)
                          ;; (gap jumps are sprint-only, so sprint is constant in the shares)
                          share (* (aget (aget ^js (.-gap-share s) n) (if (> (.runUp s i x z dx dz) 1) 1 0)) (if (pos? delta) GAP-UP-MISS 1))
                          missed (if deadly 0 (* (.-gap-miss s) share miss))
                          risk (+ (if (and (== n 3) pit) (js/Math.max hole GAP-PIT-RISK) hole) (if deadly (* (.-gap-death s) share) missed))]
                      (when-not (or (< delta -16) (> delta (if up WHOLE 0))
                                    (and (> n 1) ^boolean (.gapOvershootLethal s dx dz lx ly lz h1))
                                    (and (some? ^js (.-limit-gap s))
                                         ;; (the goal flood expands cells nothing reached: i is -1, the takeoff a plain walk)
                                         (not (true? (^js (.-limit-gap s) x y z (- h0 (* y 16)) (if (neg? i) MOVE-WALK (aget (.-moves s) i)) lx ly lz h1)))
                                         (do (set! (.-limit-refused s) true) true)))
                        (set! (.-move-dmg s) (+ (.-enter-dmg s) missed))
                        ;; (the opening pass: the closed gates the jump crosses are opened first, like the one the body stands in)
                        (let [here (.-door-here s)]
                          (when ^boolean (.-open-mode s)
                            (set! (.-door-here s) (.concat ^js here (.gapDoors s x y z dx dz n (- h0 (* y 16))))))
                          (.edge s lx ly lz h1 MOVE-GAP i
                                 (+ (* (inc n) (.-c-sprint-s s)) GAP-S (if (pos? delta) GAP-UP-S 0) (.-enter-extra s))
                                 (+ (.-enter-risk s) risk) (.-enter-slow s) 0 0)
                          (set! (.-door-here s) here))
                        (set! (.-move-dmg s) 0))))
                  (recur (inc n) hole pit deadly miss up-arc)))))))))

  ;; the run-up cells a gap jump out of node i has along dx dz, the takeoff cell counted: 2 when the plan walks into it straight on
  (runUp [s i x z dx dz]
    (let [p (if (neg? i) -1 (aget (.-parents s) i))]
      (if (and (>= p 0) (run-move? (aget (.-moves s) i))
               (== (aget (.-xs s) p) (- x dx)) (== (aget (.-zs s) p) (- z dz)))
        2
        1)))

  ;; seconds a block of walking costs after node i: the executor sprints a run of three walk or diagonal steps, so a step
  ;; that follows two of them is priced at sprintS (the gait sets it equal to walkS when the body may not sprint)
  (runS [s i]
    (if (and (>= i 0) (run-move? (aget (.-moves s) i)))
      (let [p (aget (.-parents s) i)]
        (if (and (>= p 0) (run-move? (aget (.-moves s) p))) (.-c-sprint-s s) (.-c-walk-s s)))
      (.-c-walk-s s)))

  (expandCardinal [s x y z h slow-from i region c h0 ^boolean tight-src ^boolean climbing]
    (let [x2 (+ x (aget (.-adx s) c))
          z2 (+ z (aget (.-adz s) c))
          h1 (.neighbour s x2 z2 y h0)]
      (if (>= h1 0)
        (let [y2 (.-ty s)
              delta (- (+ (* y2 16) h1) h0)
              ;; climbing a stairs block in its direction is a walk, though the node above it is a whole block up
              climbs (and (== (aget (.-tbl-stair s) (.-support s)) (inc c)) (<= delta WHOLE))
              ;; (the body steps off a climbable without a jump: it is already rising)
              walks (or (<= delta STEP) climbs (and climbing (<= delta JUMP-UP)))
              walk (* (if walks (.runS s i) (.-c-walk-s s)) (+ 1 (* SLOW-EXTRA (+ slow-from (.-enter-slow s)))))]
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
              (set! (.-move-dmg s) 0)))
          ;; a level phantom floor (options.unknown) may be refused: the body may also jump over it
          (when (and (== y2 y) (not tight-src) (not ^boolean (.-open-mode s)) ^boolean (.phantomAt s x2 y z2)
                     ^boolean (.clear s x2 z2 h0 (+ h0 BODY)))
            (.expandGap s i x y z c h0)))
        (if (>= (.swimAt s x2 y z2) 0)
          ;; water ahead at our level: walk in and swim
          (let [risk (.swimRisk s x2 y z2)
                tight (or tight-src ^boolean (.tightAt s x2 y z2))
                base (.-c-swim-h s)
                extra (if (pos? (aget (.-tbl-flowing s) (.stateAt ^js (.-snapshot s) x2 y z2))) (.-c-current s) 0)
                sec (+ base extra)]
            (when ^boolean (.swimBegin s i true x2 y z2 base extra false)
              (if tight
                (.tightMove s i x y z h region c x2 y z2 0 MOVE-SWIM sec risk 0 SNAP SNAP)
                (.edge s x2 y z2 0 MOVE-SWIM i sec risk 0 0 0))
              (.swimEnd s)))
          ;; no ground ahead at our level: the body must at least fit in the column to leave the edge
          (when ^boolean (.clear s x2 z2 h0 (+ h0 BODY))
            (.expandDrop s i x y z h region c x2 z2 h0 slow-from tight-src)
            ;; no gap jumps out of a tight cell, but over a gate or door cell: the body has no other way across the hole
            (when (or (not tight-src) ^boolean (.openableAt s x2 y z2))
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
                (let [walk (+ (* (if (or jump (== slide 1)) (.-c-walk-s s) (.runS s i)) SQRT2 (+ 1 (* SLOW-EXTRA (+ slow-from (.-enter-slow s))))) (* slide CORNER-S))
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
      (do (.expandSwim s x y z i region)
          ;; an unseen cell read as water, in a shaft above a seen ladder: the way down is the ladder (swimming has no cell to enter)
          (when (and (not ^boolean (.-quiet s)) ^boolean (.isUnknown s (.stateAt ^js (.-snapshot s) x y z)) ^boolean (.shaftAbove s x y z))
            (.climbDown s i x y z h region)))
      (let [h0 (+ (* y 16) h)
            tight-src ^boolean (.tightAt s x y z)
            ;; (the same quiet test as for tight cells: no climbable within reach of the cell either)
            climbing (and (not ^boolean (.-quiet s)) ^boolean (.climbHere s x y z))]
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
        (let [sec (+ (.-c-climb-up s) (if ^boolean (.-enters-shut s) (.-c-open s) 0))
              fall (.climbFall s x y z h2)
              dmg (.dropDamage s 1 fall)]
          (when (or (>= (.-lethal-hp s) FULL-HEALTH) (< dmg (.-lethal-hp s)))
            (when ^boolean (.-enters-shut s) (set! (.-move-open s) (.push ^js (.-open-lists s) #js [#js {:x x :y (inc y) :z z}])))
            (.verticalMove s i x y z h region (inc y) h2 (if ^boolean (.-enters-shut s) MOVE-OPEN MOVE-CLIMB-UP) sec (+ (.-enter-risk s) (* CLIMB-FALL-P dmg)) (.-enter-slow s))
            (set! (.-move-open s) 0))))))

  ;; the fall (1/16 blocks) from the cell above (x, y, z) at height h2 down to the first solid ground below the climb, through
  ;; free and climbable cells; 0 when the water, unloaded land or no ground within CLIMB-FALL-SCAN lies below
  (climbFall [s x y z h2]
    (let [from16 (+ (* (inc y) 16) h2)]
      (loop [y3 y]
        (if (< y3 (- y CLIMB-FALL-SCAN))
          0
          (let [id (.stateAt s x y3 z)]
            (cond
              (or (== id UNLOADED) (== id (.-unk-id s)) (== (aget (.-tbl-kind s) id) WATER) (not (neg? (.swimAt s x y3 z)))) 0
              (and (pos? (aget (.-tbl-top s) id)) (zero? (aget (.-tbl-climb s) id))) (js/Math.max 0 (- from16 (+ (* y3 16) (aget (.-tbl-top s) id))))
              :else (recur (dec y3))))))))

  ;; a climbable or standable cell one block down; else, through free cells, a short fall onto the first climbable or floor
  (climbDown [s i x y z h region]
    (let [h2 (.enterCell s x (dec y) z)]
      (if (>= h2 0)
        (let [sec (+ (.-c-climb-down s) (if ^boolean (.-enters-shut s) (.-c-open s) 0))]
          (when ^boolean (.-enters-shut s) (set! (.-move-open s) (.push ^js (.-open-lists s) #js [#js {:x x :y (dec y) :z z}])))
          (.verticalMove s i x y z h region (dec y) h2 (if ^boolean (.-enters-shut s) MOVE-OPEN MOVE-CLIMB-DOWN) sec (.-enter-risk s) (.-enter-slow s))
          (set! (.-move-open s) 0))
        (let [free (.stateAt s x (dec y) z)]
          (when-not (or (== free UNLOADED) (== free (.-unk-id s)) (pos? (aget (.-tbl-top s) free)) (== (aget (.-tbl-kind s) free) WATER) (== (aget (.-tbl-kind s) free) LAVA)
                        ^boolean (.avoids s free x (dec y) z))
            (.fallThrough s i x y z h region (+ (* y 16) h)))))))

  (fallThrough [s i x y z h region from16]
    (loop [y3 (- y 2)]
      (when (>= y3 (- y (.-max-drop s) 1))
        (let [id (.stateAt s x y3 z)]
          (when-not (or (== id UNLOADED) (== id (.-unk-id s)) (== (aget (.-tbl-kind s) id) LAVA) (== (aget (.-tbl-kind s) id) WATER) ^boolean (.avoids s id x y3 z))
            (let [h3 (.landing s x y3 z)]
              (if (neg? h3)
                (when-not (pos? (aget (.-tbl-top s) id)) (recur (dec y3)))
                (let [sup (.-support s)
                      fall (- from16 (+ (* y3 16) h3))
                      f (.landFactor s sup x y3 z fall)
                      dmg (.dropDamage s f fall)]
                  (when-not (> fall (* (.-max-drop s) 16))
                    (set! (.-move-dmg s) (+ dmg (.-enter-dmg s)))
                    (.verticalMove s i x y z h region y3 h3 MOVE-DROP (+ (* (.-c-drop-factor s) 0.25 (js/Math.sqrt (/ fall 16))) (.bounceS s f fall))
                                   (+ (.-enter-risk s) dmg) (.-enter-slow s))
                    (set! (.-move-dmg s) 0))))))))))

  ;; from the floor, a jump puts the feet into a climbable one block up
  (jumpClimb [s i x y z h region]
    (let [id (.stateAt ^js (.-snapshot s) x (inc y) z)]
      (when-not (or (== id UNLOADED) (zero? (aget (.-tbl-climb s) id)) (== (aget (.-tbl-climb s) id) CLIMB-TRAP-SHUT))
        (let [h2 (.landing s x (inc y) z)]
          (when-not (neg? h2)
            (.verticalMove s i x y z h region (inc y) h2 MOVE-JUMP-CLIMB (.-c-jump-climb s) (.-enter-risk s) (.-enter-slow s))))))))
