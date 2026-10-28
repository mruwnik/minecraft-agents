(ns engine.path.planner.unknown
  "Search methods: cells the body has not seen (options.unknown, see engine.path.planner-tuned). Such a cell reads as
  void_air, air to the body. A stand on an unseen floor (a phantom floor, standPlain reads it as unk-floor-id) is allowed
  only where the walk can stop on the takeoff to check it first and can see it from there, and it is priced."
  (:require [engine.path.planner.base :refer [FREE-FALL MOVE-CLIMB-UP MOVE-CORNER MOVE-DIAGONAL MOVE-DROP MOVE-GAP MOVE-JUMP MOVE-OPEN MOVE-SWIM MOVE-WALK OPEN TABLE UNLOADED WATER WHOLE cell-key]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(def ^:const SEE-LIT-DEPTH 10) ; options.unknown.seeLitDepth default: the deepest drop a lit landing is seen over from the walk's worst peek stop (x .87, floor-sight-pin-test)
(def ^:const LIGHT-DEPTH 5) ; options.unknown.lightDepth default: the deepest drop a torch at the takeoff lights the landing of so it is seen

(extend-type Search
  Object

  ;; is the state id an unseen cell's?
  (isUnknown [s id] (== id (.-unk-id s)))

  ;; is the floor cell x y z one the walk looked for and could not see (options.unknown.missed)?
  (missedAt [s x y z]
    (and (some? ^js (.-missed s)) (true? (.has ^js (.-missed s) (cell-key x y z)))))

  ;; does the node at x y z stand on a phantom floor: its floor is unseen and it stands on its floor (not in water, which
  ;; holds the body up, not in a climbable or a shaft above one, not on its own cell's low block, not in a door's lower half, whose floor a door
  ;; implies)?
  (phantomAt [s x y z]
    (and (>= (.-unk-id s) 0)
         ^boolean (.isUnknown s (.stateAt ^js (.-snapshot s) x (dec y) z))
         (let [id (.stateAt s x y z)
               t (aget (.-tbl-top s) id)
               cl (aget (.-tbl-climb s) id)]
           (and (not (== (aget (.-tbl-kind s) id) WATER))
                (not (and (pos? t) (zero? (aget (.-tbl-base s) id)) (< t WHOLE)))
                (not (and (not (zero? cl)) ^boolean (.climbCell s cl id x y z)))
                (not ^boolean (.shaftAbove s x y z))
                (not (== (aget (.-tbl-door-half s) id) 1))))))

  ;; the unseen cells among the feet and head cells at x y z
  (unknownCells [s x y z]
    (+ (if ^boolean (.isUnknown s (.stateAt ^js (.-snapshot s) x y z)) 1 0)
       (if ^boolean (.isUnknown s (.stateAt ^js (.-snapshot s) x (inc y) z)) 1 0)))

  ;; the chance (options.unknown.changed) that the remembered cell at x y z changed since seen
  (changedAt [s x y z]
    (let [^js f (.-unk-changed s)]
      (if (nil? f) 0 (.call f nil x y z))))

  ;; the most chance that the remembered feet, head or floor cell at x y z changed since seen
  (changedNear [s x y z]
    (if (nil? (.-unk-changed s))
      0
      (js/Math.max (.changedAt s x y z) (.changedAt s x (inc y) z) (.changedAt s x (dec y) z))))

  ;; is the cell, its floor or its head cell unseen or likelier changed than not, or its floor missed? A proof (the goal
  ;; flood) must not rest on it.
  (unknownNear [s x y z]
    (and (>= (.-unk-id s) 0)
         (or ^boolean (.isUnknown s (.stateAt ^js (.-snapshot s) x y z))
             ^boolean (.isUnknown s (.stateAt ^js (.-snapshot s) x (dec y) z))
             ^boolean (.isUnknown s (.stateAt ^js (.-snapshot s) x (inc y) z))
             ^boolean (.missedAt s x (dec y) z)
             (>= (.changedNear s x y z) 0.5))))

  ;; did node p's way in end with the body bouncing on a pad (a drop whose landing factor is a bounce)?
  (bounceArrival [s p]
    (and (== (aget (.-moves s) p) MOVE-DROP) (some? (.-land-factors s)) (>= (aget (.-parents s) p) 0)
         (let [x (aget (.-xs s) p) y (aget (.-ys s) p) z (aget (.-zs s) p)
               own (.stateAt s x y z)
               sup (if (pos? (aget (.-tbl-top s) own)) own (.stateAt s x (dec y) z))
               fall (- (.stand16 s (aget (.-parents s) p)) (.stand16 s p))]
           (and (> fall (* FREE-FALL 16)) (neg? (.landFactor s sup x y z fall))))))

  ;; Can the walk stop on node p's stand before `move` takes it onto a phantom floor, to check the floor? The move is a walk,
  ;; diagonal, corner slide, jump or drop, p was reached by none of a gap jump, a climb (or trapdoor open), a swim, an exit or a
  ;; bounce, and p is no climbable with no floor under it (the start too: a body hanging there slides down while it looks).
  (gateCheckable [s move p]
    (and (or (== move MOVE-WALK) (== move MOVE-DIAGONAL) (== move MOVE-CORNER) (== move MOVE-JUMP) (== move MOVE-DROP))
         (let [m (aget (.-moves s) p)
               x (aget (.-xs s) p) y (aget (.-ys s) p) z (aget (.-zs s) p)
               below (.stateAt s x (dec y) z)]
           (and (not (== m MOVE-GAP)) (not (and (>= m MOVE-CLIMB-UP) (<= m MOVE-OPEN))) (< m MOVE-SWIM)
                (not ^boolean (.bounceArrival s p))
                (not (and ^boolean (.climbHere s x y z) (or (== below UNLOADED) (< (aget (.-tbl-floor s) below) WHOLE))))))))

  ;; 1 when the caller's test (options.unknown.seeLit) calls the landing x y z lit enough to see its floor from the peek,
  ;; else 0; cached as darkOf
  (seeLitOf [s x y z]
    (let [key (inc (.keyOf s x y z 0))
          slot (bit-and (.hashOf s x y z 0) (dec TABLE))]
      (if (== (aget (.-see-keys s) slot) key)
        (- (aget (.-see-flags s) slot) 1)
        (let [lit (if (== (.call ^js (.-see-lit s) nil x y z) 1) 1 0)]
          (aset (.-see-keys s) slot key)
          (aset (.-see-flags s) slot (inc lit))
          lit))))

  ;; a cell the body knows is air (seen, empty, no water)
  (knownAir [s x y z]
    (let [id (.stateAt ^js (.-snapshot s) x y z)]
      (and (not (== id UNLOADED)) (not ^boolean (.isUnknown s id)) (== (aget (.-tbl-kind s) id) OPEN)
           (zero? (aget (.-tbl-top s) id)) (zero? (aget ^js (.-wet ^js (.-table s)) id)))))

  ;; Can the floor of a drop of d from node p be seen once the body is at the peek? Within see-depth, or within
  ;; see-lit-depth when the landing's air is known and the caller calls it lit.
  (seeable [s x y z d]
    (or (<= d (.-see-depth s))
        (and (some? (.-see-lit s)) (<= d (.-see-lit-depth s)) ^boolean (.knownAir s x y z) (== (.seeLitOf s x y z) 1))))

  ;; Can a torch at node p's stand light a drop of d? Torches are on (light-cost), d is at most light-depth, p's feet cell
  ;; is known air on a torch base (options.unknown.torchBase), the caller allows it there (lightOk) and no torch failed there.
  (lightable [s p d]
    (and (>= (.-light-cost s) 0) (<= d (.-light-depth s)) (some? (.-torch-base s))
         (let [x (aget (.-xs s) p) y (aget (.-ys s) p) z (aget (.-zs s) p)
               floor (.stateAt ^js (.-snapshot s) x (dec y) z)]
           (and ^boolean (.knownAir s x y z)
                (not (== floor UNLOADED)) (== (aget ^js (.-torch-base s) floor) 1)
                (or (nil? (.-light-ok s)) (true? (.call ^js (.-light-ok s) nil x y z)))
                (not (and (some? ^js (.-no-light s)) (true? (.has ^js (.-no-light s) (cell-key x y z)))))))))

  ;; a phantom floor refused for want of sight: the refused stand nearest the goal is kept (result darkBest)
  (darkRefuse [s x y z]
    (set! (.-dark-refused s) true)
    (let [d (.distanceTo s x z)]
      (when (< d (.-dark-best-d s))
        (set! (.-dark-best-d s) d)
        (set! (.-dark-best-x s) x)
        (set! (.-dark-best-y s) y)
        (set! (.-dark-best-z s) z))))

  ;; True when the move from node p into x y z is refused for options.unknown: its stand is on a phantom floor that the walk
  ;; cannot stop to check first (unseen-refused), or was missed or cannot be seen, and is not lit (dark-refused). Else sets what it
  ;; costs (move-unk, seconds: cell times dsec when its feet, head or floor is unseen, else times their changedNear, and a peek over a drop or an aim at a
  ;; level or up one onto a phantom floor, and light-cost when a torch must light it) and move-light.
  (unseenRefuses [s x y z move p dsec]
    (set! (.-move-light s) 0)
    (if-not ^boolean (.phantomAt s x y z)
      (do (set! (.-move-unk s) (if (pos? (.unknownCells s x y z)) (* dsec (.-unk-cell s)) (* dsec (.-unk-cell s) (.changedNear s x y z))))
          false)
      (let [d (js/Math.max 0 (- (aget (.-ys s) p) y))
            priced (+ (* dsec (.-unk-cell s)) (if (pos? d) (.-peek-s s) (.-aim-s s)))]
        (cond
          (not ^boolean (.gateCheckable s move p)) (do (set! (.-unseen-refused s) true) true)
          (and (not ^boolean (.missedAt s x (dec y) z)) ^boolean (.seeable s x y z d)) (do (set! (.-move-unk s) priced) false)
          ^boolean (.lightable s p d) (do (set! (.-move-unk s) (+ priced (.-light-cost s)))
                                          (set! (.-move-light s) 1)
                                          false)
          :else (do (.darkRefuse s x y z) true))))))
