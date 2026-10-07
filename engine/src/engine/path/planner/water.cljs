(ns engine.path.planner.water
  "Search methods: swimming, diving, currents and leaving the water."
  (:require [engine.path.planner.base :refer [BODY CORNER-S DAMAGE-STAND DAMAGE-TOUCH EXIT-SLACK HAZARD-AVOID JUMP-S JUMP-UP LAVA-ADJACENT MOVE-CORNER MOVE-EXIT MOVE-JUMP MOVE-SWIM MOVE-SWIM-DOWN MOVE-SWIM-UP MOVE-WALK NARROW NONE OPEN SNAP SQRT2 STEP UNLOADED WALK-S WATER WHOLE]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; ---- water ----
  (isWater [s x y z]
    (let [id (.stateAt s x y z)]
      (and (not (== id UNLOADED)) (== (aget (.-tbl-kind s) id) WATER))))

  ;; a body floating in the water cell (feet at its floor, h = 0): the cell over it is water or open and unhazardous
  (swimAt [s x y z]
    (let [id (.stateAt s x y z)]
      (if (or (== id UNLOADED) (not (== (aget (.-tbl-kind s) id) WATER)))
        -1
        (let [head (.stateAt s x (inc y) z)]
          (cond
            (== head UNLOADED) -1
            (== (aget (.-tbl-kind s) head) WATER) 0
            (and (== (aget (.-tbl-kind s) head) OPEN) (zero? (aget (.-tbl-top s) head)) (zero? (aget (.-tbl-hazard s) head))) 0
            :else -1)))))

  ;; head in water that is not a bubble column: the breath runs
  (submerged [s x y z]
    (let [head (.stateAt s x (inc y) z)]
      (and (not (== head UNLOADED)) (== (aget (.-tbl-kind s) head) WATER) (zero? (aget (.-tbl-bubble s) head)))))

  ;; Dominance: a submerged sideways move in open water is never better than swimming at the surface over it. A cell's column
  ;; is open when its water reaches plain air; then surfaceY is the top water cell, else NONE. Cached per cell.
  (surfaceScan [s x y z]
    (loop [y2 (inc y)]
      (let [id (.stateAt s x y2 z)]
        (cond
          (== id UNLOADED) NONE
          (== (aget (.-tbl-kind s) id) WATER) (recur (inc y2))
          (and (== (aget (.-tbl-kind s) id) OPEN) (zero? (aget (.-tbl-top s) id))) (dec y2)
          :else NONE))))

  (surfaceY [s x y z]
    (let [key (.keyOf s x y z 0)
          hit (.get ^js (.-surface-cache s) key)]
      (if (undefined? hit)
        (let [top2 (.surfaceScan s x y z)]
          (.set ^js (.-surface-cache s) key top2)
          top2)
        hit)))

  (openWater [s x y z]
    (not (== (.surfaceY s x y z) NONE)))

  (divesOpen [s x y z]
    (and ^boolean (.submerged s x y z) ^boolean (.openWater s x y z)))

  ;; a sideways swim move into (x, y, z) that dominance refuses: the body is at surface level `src-surface` (NONE: in a covered
  ;; passage, where the open water is the way on) and the target is submerged under the same surface
  (refuses [s x y z src-surface]
    (and (not (== src-surface NONE)) ^boolean (.submerged s x y z) (== (.surfaceY s x y z) src-surface)))

  ;; is something down the water column worth the dive: the goal, a bubble column, a bank to climb onto at that depth, or a
  ;; covered passage beside it
  (diveBeside [s x y2 z]
    (loop [c 0]
      (if (== c 4)
        false
        (let [x2 (+ x (aget (.-adx s) c))
              z2 (+ z (aget (.-adz s) c))]
          (if (if ^boolean (.isWater s x2 y2 z2)
                (and (>= (.swimAt s x2 y2 z2) 0) (not ^boolean (.divesOpen s x2 y2 z2)))
                (>= (.standH s x2 y2 z2) 0))
            true
            (recur (inc c)))))))

  (diveScan [s x y z]
    (loop [y2 y]
      (if (not ^boolean (.isWater s x y2 z))
        false
        (if (or ^boolean (.reached s x y2 z) (not (zero? (aget (.-tbl-bubble s) (.stateAt s x y2 z)))) ^boolean (.diveBeside s x y2 z))
          true
          (recur (dec y2))))))

  (worthDiving [s x y z]
    (let [key (.keyOf s x y z 0)
          hit (.get ^js (.-dive-cache s) key)]
      (if (undefined? hit)
        (let [found ^boolean (.diveScan s x y z)]
          (.set ^js (.-dive-cache s) key (if found 1 2))
          found)
        (== hit 1))))

  ;; swimming beside lava, or down onto magma
  (swimRisk [s x y z]
    (+ (if ^boolean (.lavaNear s x y z) LAVA-ADJACENT 0)
       (if (== (aget (.-tbl-hazard s) (.stateAt s x (dec y) z)) DAMAGE-STAND) 1 0)))

  ;; a stand height at a feet cell: on land, or floating in water (h = 0); -1 for neither
  (nodeH [s x y z]
    (let [h (.standH s x y z)]
      (if (>= h 0) h (.swimAt s x y z))))

  ;; One swim or exit edge. The caller calls swimBegin, makes the edge (`base` + `extra` seconds) if it says true,
  ;; then calls swimEnd. `base` seconds of swimming count for the air, `extra` seconds of current do not.
  ;; src-sub: the body starts the move with its head in water. target-water: it ends in a water cell.
  (swimBegin [s i ^boolean target-water x2 y2 z2 base extra ^boolean src-sub]
    (let [target-sub (and target-water ^boolean (.submerged s x2 y2 z2))
          use (+ (if (>= i 0) (aget (.-airs s) i) 0) base)]
      (if (and (or src-sub target-sub) (> use (.-c-air-limit s)))
        (do (set! (.-air-seen s) true)
            false)
        (do (set! (.-move-air s) (if target-sub use 0))
            (set! (.-move-peak s) (if (or src-sub target-sub) use 0))
            (set! (.-move-water s) (+ base extra))
            true))))

  (swimEnd [s]
    (set! (.-move-air s) 0)
    (set! (.-move-peak s) 0)
    (set! (.-move-water s) 0))

  (currentAt [s x y z]
    (if (== (aget (.-tbl-flowing s) (.stateAt s x y z)) 1) (.-c-current s) 0))

  ;; the water cell is 1 deep over a floor, with air over it: the body stands on the floor, it is not floating
  (standsInWater [s x y z]
    (if (not (zero? (aget (.-tbl-bubble s) (.stateAt s x y z))))
      false
      (let [head (.stateAt s x (inc y) z)
            below (.stateAt s x (dec y) z)]
        (if (or (== head UNLOADED) (== below UNLOADED) (== (aget (.-tbl-kind s) head) WATER))
          false
          (let [hz (aget (.-tbl-hazard s) below)]
            (and (>= (aget (.-tbl-floor s) below) WHOLE) (not (== (aget (.-tbl-kind s) below) NARROW)) (not (== hz HAZARD-AVOID)) (not (== hz DAMAGE-TOUCH))))))))

  ;; out of 1-deep water by the walking rules: a step of up to STEP is a walk, up to JUMP_UP a jump (the head cell is open)
  (wadeOut [s i x y z region c x2 z2 ^boolean tight-src]
    (let [h0 (* y 16)
          h1 (.neighbour s x2 z2 y h0)]
      (when (>= h1 0)
        (let [y2 (.-ty s)
              delta (- (+ (* y2 16) h1) h0)
              walks (<= delta STEP)]
          (when (or walks (and (<= delta JUMP-UP) ^boolean (.clear s x z (* (inc y) 16) (+ (* y2 16) h1 BODY))))
            (let [sec (+ (if walks WALK-S (+ WALK-S JUMP-S)) (.-enter-extra s))
                  move (if walks MOVE-WALK MOVE-JUMP)]
              (if (or tight-src ^boolean (.tightAt s x2 y2 z2))
                (.tightMove s i x y z 0 region c x2 y2 z2 h1 move sec (.-enter-risk s) (.-enter-slow s) SNAP SNAP)
                (.edge s x2 y2 z2 h1 move i sec (.-enter-risk s) (.-enter-slow s) 0 0))))))))

  ;; a lifting column (1) cannot be swum down, a dragging one (2) cannot be swum up
  (swimVertical [s x y z i region src-b ^boolean src-sub dy]
    (let [y2 (+ y dy)]
      (when (>= (.swimAt s x y2 z) 0)
        (let [tb (aget (.-tbl-bubble s) (.stateAt s x y2 z))]
          (when-not (if (== dy 1) (or (== src-b 2) (== tb 2)) (or (== src-b 1) (== tb 1)))
            (when-not (and (== dy -1) ^boolean (.divesOpen s x y2 z) (not ^boolean (.worthDiving s x y2 z)))
              (let [lift (or (== src-b 1) (== tb 1))
                    drag (or (== src-b 2) (== tb 2))
                    move (if (== dy 1) MOVE-SWIM-UP MOVE-SWIM-DOWN)
                    base (if (== dy 1)
                           (if lift (.-c-bubble-up s) (.-c-swim-up s))
                           (if drag (.-c-bubble-down s) (.-c-swim-down s)))
                    extra (.currentAt s x y2 z)
                    risk (.swimRisk s x y2 z)]
                (when ^boolean (.swimBegin s i true x y2 z base extra src-sub)
                  (.verticalMove s i x y z 0 region y2 0 move (+ base extra) risk 0)
                  (.swimEnd s)))))))))

  ;; a sideways swim move into the water cell beside (x, y, z); into a dragging column only when the goal is well below it
  (swimSideways [s i x y z region c x2 z2 ^boolean tight-src ^boolean src-sub]
    (when-not (and (== (aget (.-tbl-bubble s) (.stateAt s x2 y z2)) 2) (not (< (.-goal-y s) (- y 1))))
     (let [risk (.swimRisk s x2 y z2)
          tight (or tight-src ^boolean (.tightAt s x2 y z2))
          base (.-c-swim-h s)
          extra (.currentAt s x2 y z2)]
      (when ^boolean (.swimBegin s i true x2 y z2 base extra src-sub)
        (if tight
          (.tightMove s i x y z 0 region c x2 y z2 0 MOVE-SWIM (+ base extra) risk 0 SNAP SNAP)
          (.edge s x2 y z2 0 MOVE-SWIM i (+ base extra) risk 0 0 0))
        (.swimEnd s)))))

  ;; out of the water onto the bank cells beside it, from the same level up to last-ty
  (swimExit [s i x y z region c x2 z2 ^boolean tight-src ^boolean src-sub last-ty max-stand]
    (loop [y2 y]
      (when (<= y2 last-ty)
        (let [h1 (.landing s x2 y2 z2)]
          (when-not (or (neg? h1) (> (+ (* y2 16) h1) max-stand))
            (let [base (+ (if (== y2 y) (.-c-swim-h s) (.-c-exit s)) (.-enter-extra s))
                  risk (.-enter-risk s)
                  slow-to (.-enter-slow s)
                  tight (or tight-src ^boolean (.tightAt s x2 y2 z2))]
              (when ^boolean (.swimBegin s i false x2 y2 z2 base 0 src-sub)
                (if tight
                  (.tightMove s i x y z 0 region c x2 y2 z2 h1 MOVE-EXIT (+ base 0) risk slow-to SNAP SNAP)
                  (.edge s x2 y2 z2 h1 MOVE-EXIT i (+ base 0) risk slow-to 0 0))
                (.swimEnd s))))
          (recur (inc y2))))))

  ;; the diagonals of a swimming node, with the walking side rule: the body brushes both side cells
  (swimDiagonal [s i x y z c ^boolean src-sub src-surface]
    (let [dx (aget (.-adx s) c)
          dz (aget (.-adz s) c)
          x2 (+ x dx)
          z2 (+ z dz)]
      (when-not (or (< (.swimAt s x2 y z2) 0) ^boolean (.tightAt s x2 y z2) ^boolean (.refuses s x2 y z2 src-surface)
                    (and (== (aget (.-tbl-bubble s) (.stateAt s x2 y z2)) 2) (not (< (.-goal-y s) (- y 1)))))
        (let [lo (* y 16)
              hi (+ lo BODY)
              sa (.side s (+ x dx) z lo hi)
              sb (.side s x (+ z dz) lo hi)]
          (when-not (or (== sa 2) (== sb 2) (and (== sa 1) (== sb 1)))
            (let [slide (+ sa sb)
                  risk (.swimRisk s x2 y z2)
                  base (+ (* (.-c-swim-h s) SQRT2) (* slide CORNER-S))
                  extra (.currentAt s x2 y z2)]
              (when ^boolean (.swimBegin s i true x2 y z2 base extra src-sub)
                (.edge s x2 y z2 0 (if (zero? slide) MOVE-SWIM MOVE-CORNER) i (+ base extra) risk 0 slide 0)
                (.swimEnd s))))))))

  ;; a node floating in water: up, down, sideways and onto the bank; sideways moves and exits may cross tight cells (masks), a
  ;; diagonal never does
  (expandSwim [s x y z i region]
    (let [tight-src ^boolean (.tightAt s x y z)
          src-b (aget (.-tbl-bubble s) (.stateAt s x y z))
          src-sub ^boolean (.submerged s x y z)
          src-surface (if src-sub (.surfaceY s x y z) y)]
      (.swimVertical s x y z i region src-b src-sub 1)
      (.swimVertical s x y z i region src-b src-sub -1)
      ;; Out of the water onto a bank. A floating body gets out onto land whose stand height is at most the water's
      ;; top face + 1/16 (flush, or a 15/16 top), never onto land one higher. A body standing on a floor in water 1
      ;; deep is not floating: it walks and jumps out by the ordinary rules.
      (let [top-water ^boolean (.isWater s x (inc y) z)
            near-surface (or (not top-water) (not ^boolean (.isWater s x (+ y 2) z)))
            yt (if top-water (inc y) y)
            last-ty (if near-surface (js/Math.max y (inc yt)) y)
            max-stand (+ (* (inc yt) 16) EXIT-SLACK)
            wading ^boolean (.standsInWater s x y z)]
        (loop [c 0]
          (when (< c 4)
            (let [x2 (+ x (aget (.-adx s) c))
                  z2 (+ z (aget (.-adz s) c))]
              (cond
                (>= (.swimAt s x2 y z2) 0)
                (when-not ^boolean (.refuses s x2 y z2 src-surface)
                  (.swimSideways s i x y z region c x2 z2 tight-src src-sub))
                wading (.wadeOut s i x y z region c x2 z2 tight-src)
                :else (.swimExit s i x y z region c x2 z2 tight-src src-sub last-ty max-stand)))
            (recur (inc c)))))
      (when-not tight-src
        (loop [c 4]
          (when (< c 8)
            (.swimDiagonal s i x y z c src-sub src-surface)
            (recur (inc c))))))))
