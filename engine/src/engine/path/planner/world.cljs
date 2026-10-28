(ns engine.path.planner.world
  "Search methods: the world as the body sees it: cell states, whether the body fits and stands in a cell, hazards
   beside it."
  (:require [engine.path.planner.base :refer [TABLE BODY CLIMB-FALL-SCAN CLIMB-INSIDE CLIMB-TRAP-SHUT DAMAGE-STAND DAMAGE-TOUCH FREE-FALL HAZARD-AVOID LADDER LAVA LAVA-ADJACENT NARROW OPENABLE PORTAL SLOW UNLOADED WATER WHOLE]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(def ^:const SHAFT-REACH 64) ; the most unseen cells a shaft is supposed to go on for above the climbable it was seen on

(extend-type Search
  Object

  ;; ---- the world, as the body sees it ----

  ;; What the body sees: in the opening pass a closed door, gate or trapdoor reads as open, so the move is judged with the
  ;; block as it will be once opened; `.stateAt snapshot` is the block as it stands.
  (stateAt [s x y z]
    (let [id (.stateAt ^js (.-snapshot s) x y z)]
      (if (and ^boolean (.-open-mode s) (pos? (aget (.-tbl-openable s) id)))
        (aget (.-tbl-open-state s) id)
        id)))

  ;; Does the climbable state `id` (climb = cl) at x,y,z make its cell one the body climbs in? An open trapdoor
  ;; counts over a ladder. Facing the ladder's way, it is climbed (vanilla, and the client with tools/patch-deps.mjs).
  ;; Facing another way, its panel leaves the ladder's top edge free to stand on and jump from (the step into it
  ;; aims at the ladder's wall, see hatchWall).
  (climbCell [s cl id x y z]
    (if (== cl CLIMB-INSIDE)
      true
      (let [below (.stateAt ^js (.-snapshot s) x (dec y) z)]
        (and (not (== below UNLOADED)) (== (aget (.-tbl-climb-name s) below) LADDER)))))

  ;; Does a section of the column's last SHAFT-REACH cells up to y hold any climbable state? Without one no shaft is above, and
  ;; the walk down the column need not read its cells (a walk over unseen land asks this of every cell).
  (climbableBelow [s x y z]
    (let [snap (.-snapshot s)
          sy1 (bit-shift-right (- y (.-min-y s)) 4)
          sx (bit-shift-right x 4)
          sz (bit-shift-right z 4)]
      (loop [sy (bit-shift-right (- (- y SHAFT-REACH) (.-min-y s)) 4)]
        (cond
          (> sy sy1) false
          (true? (.sectionHas snap (.-tbl-climb s) sx sy sz)) true
          :else (recur (inc sy))))))

  ;; An unseen cell (options.unknown) straight above a climbable, or above unseen cells that are above one: a shaft goes on
  ;; until seen otherwise, as unseen land is walked on, so a climb is planned up it and the climb shows the way.
  (shaftAbove [s x y z]
    (and (>= (.-unk-id s) 0)
         ^boolean (.climbableBelow s x y z)
         (loop [y2 y]
           (let [id (.stateAt ^js (.-snapshot s) x y2 z)]
             (cond
               (== id (.-unk-id s)) (and (> y2 (- y SHAFT-REACH)) (recur (dec y2)))
               (== id UNLOADED) false
               :else (and (< y2 y) (let [cl (aget (.-tbl-climb s) id)]
                                     (and (not (zero? cl)) (not (== cl CLIMB-TRAP-SHUT)) ^boolean (.climbCell s cl id x y2 z)))))))))

  (climbHere [s x y z]
    (let [id (.stateAt ^js (.-snapshot s) x y z)]
      (cond
        (== id UNLOADED) false
        (== id (.-unk-id s)) ^boolean (.shaftAbove s x y z)
        :else (let [cl (aget (.-tbl-climb s) id)]
                (and (not (zero? cl)) ^boolean (.climbCell s cl id x y z))))))

  ;; a closed wooden trapdoor above a ladder: entering the cell costs an OPEN
  (shutAt [s x y z]
    (let [id (.stateAt ^js (.-snapshot s) x y z)]
      (and (not (== id UNLOADED)) (== (aget (.-tbl-climb s) id) CLIMB-TRAP-SHUT) ^boolean (.climbCell s CLIMB-TRAP-SHUT id x y z))))

  (viewAt [s x y z]
    (let [id (.stateAt s x y z)]
      (if (and (== (aget (.-tbl-climb s) id) CLIMB-TRAP-SHUT) ^boolean (.climbCell s CLIMB-TRAP-SHUT id x y z))
        0
        id)))

  ;; the body already stands in its start cell: fire or lava flowing in there is no reason to refuse the plan out of it
  (atStart [s x y z]
    (and (== x (.-from-x s)) (== y (.-from-y s)) (== z (.-from-z s))))

  ;; lava, unless in the start cell
  (lavaIn [s kd x y z]
    (and (== kd LAVA) (not ^boolean (.atStart s x y z))))

  ;; a cell the body must not be in: an AVOID hazard (but the start cell), or a portal unless the cell (or the one under
  ;; it) is in the goal
  (avoids [s id x y z]
    (let [hz (aget (.-tbl-hazard s) id)]
      (or (and (== hz HAZARD-AVOID) (not ^boolean (.atStart s x y z)))
          (and (== hz PORTAL) (not ^boolean (.inGoal s x y z))))))

  ;; is the column at x,z free for a body spanning lo..hi (1/16 absolute)? Also false for fluid, NARROW, AVOID, unloaded.
  (clear [s x z lo hi]
    (let [last-y (bit-shift-right (dec hi) 4)]
      (loop [y (bit-shift-right lo 4)]
        (if (> y last-y)
          true
          (let [id (.stateAt s x y z)]
            (if (== id UNLOADED)
              false
              (let [t (aget (.-tbl-top s) id)
                    k (aget (.-tbl-kind s) id)]
                (if (or (and (pos? t) (> (+ (* y 16) t) lo) (< (+ (* y 16) (aget (.-tbl-base s) id)) hi))
                        (== k WATER) ^boolean (.lavaIn s k x y z) (== k NARROW) ^boolean (.avoids s id x y z))
                  false
                  (recur (inc y))))))))))

  ;; what a diagonal's side column holds for a body spanning lo..hi: 0 passable (fluid other than lava is fine, so is a
  ;; hole), 1 collision only (a corner to slide along), 2 lava, AVOID or unloaded: never brushed
  (side [s x z lo hi]
    (let [last-y (bit-shift-right (dec hi) 4)]
      (loop [y (bit-shift-right lo 4)
             blocked 0]
        (if (> y last-y)
          blocked
          (let [id (.stateAt s x y z)]
            (if (or (== id UNLOADED) (== (aget (.-tbl-kind s) id) LAVA) ^boolean (.avoids s id x y z))
              2
              (let [t (aget (.-tbl-top s) id)]
                (if (or (== (aget (.-tbl-kind s) id) NARROW)
                        (and (pos? t) (> (+ (* y 16) t) lo) (< (+ (* y 16) (aget (.-tbl-base s) id)) hi)))
                  (recur (inc y) 1)
                  (recur (inc y) blocked)))))))))

  ;; true when the column x,z, open at the body's level y, drops to farmland at least a cell down: a body brushing it on a
  ;; diagonal can slip in and trample it (a landing from over half a block up)
  (slipScan [s x z y]
    (when ^boolean (.farmlandNear s x (- y (.-max-drop s) 1) (dec y) z x z)
      (loop [y2 (dec y)]
        (when (>= y2 (- y (.-max-drop s) 1))
          (let [id (.stateAt s x y2 z)]
            (cond
              (== id UNLOADED) false
              (== (aget (.-tbl-farmland s) id) 1) (< y2 (dec y))
              (pos? (aget (.-tbl-top s) id)) false
              :else (recur (dec y2))))))))

  ;; does a section the block x0..x1, y0..y1, z0..z1 touches hold farmland or lava? (the usual case: no, answered without
  ;; reading cells)
  (farmlandNear [s x0 y0 y1 z0 x1 z1]
    (let [^js snap (.-snapshot s)
          tbl (.-tbl-farmland s)
          min-y (.-min-y s)
          sy1 (bit-shift-right (- y1 min-y) 4)
          sz1 (bit-shift-right z1 4)
          sx1 (bit-shift-right x1 4)
          sz0 (bit-shift-right z0 4)
          sx0 (bit-shift-right x0 4)]
      (loop [sy (bit-shift-right (- y0 min-y) 4)
             sz sz0
             sx sx0]
        (cond
          (> sy sy1) false
          (> sz sz1) (recur (inc sy) sz0 sx0)
          (> sx sx1) (recur sy (inc sz) sx0)
          (true? (.sectionHas snap tbl sx sy sz)) true
          :else (recur sy sz (inc sx))))))

  ;; slipScan, cached per column in a direct-mapped table (keys +1, the opening pass its own entries): the side columns of
  ;; neighbouring diagonals repeat, and a masked search reads each one through a drop of void
  (slipsOnFarmland [s x z y]
    (let [ks (.-slip-keys s)
          key (* (inc (.keyOf s x y z 0)) (if ^boolean (.-open-mode s) -1 1))
          slot (bit-and (.hashOf s x y z 0) (dec TABLE))]
      (if (== (aget ks slot) key)
        (== 1 (aget (.-slip-flags s) slot))
        (let [slips (true? (.slipScan s x z y))]
          (aset ks slot key)
          (aset (.-slip-flags s) slot (if slips 1 0))
          slips))))

  ;; DAMAGE_TOUCH cells (berry bush, wither rose, cactus) in column x,z over lo..hi (1/16 absolute): a diagonal brushes both
  ;; side columns, so each one hurts as one in the body's own column does (`fits`' touch)
  (sideTouch [s x z lo hi]
    (let [last-y (bit-shift-right (dec hi) 4)]
      (loop [y (bit-shift-right lo 4)
             touched 0]
        (if (> y last-y)
          touched
          (let [id (.stateAt s x y z)]
            (recur (inc y) (if (and (not (== id UNLOADED)) (== (aget (.-tbl-hazard s) id) DAMAGE-TOUCH)) (inc touched) touched)))))))

  ;; does the body fit in the column of feet cell y at x,z, its feet at absolute lo (1/16)? `id` stands for the feet cell
  ;; itself. Sets `touch`. Collision that leaves gaps is for the tight-cell mask to judge, not for refusing the cell.
  (fits [s x y z lo id]
    (let [hi (+ lo BODY)
          last-y (bit-shift-right (dec hi) 4)]
      (loop [k y
             touched 0]
        (if (> k last-y)
          (do (set! (.-touch s) touched)
              true)
          (let [cid (if (== k y) id (.stateAt s x k z))]
            (if (== cid UNLOADED)
              false
              (let [ct (aget (.-tbl-top s) cid)
                    kd (aget (.-tbl-kind s) cid)]
                (cond
                  (or (and (== kd WATER) (not (and (== cid (.-unk-id s)) ^boolean (.shaftAbove s x k z)))) ^boolean (.lavaIn s kd x k z) ^boolean (.avoids s cid x k z)) false
                  (and (zero? (aget (.-tbl-partial s) cid))
                       (or (and (pos? ct) (> (+ (* k 16) ct) lo) (< (+ (* k 16) (aget (.-tbl-base s) cid)) hi))
                           (== kd NARROW))) false
                  (== (aget (.-tbl-hazard s) cid) DAMAGE-TOUCH) (recur (inc k) (inc touched))
                  :else (recur (inc k) touched)))))))))

  ;; stand height at a feet cell, or -1
  (standH [s x y z]
    (let [raw (.stateAt s x y z)]
      (if (== raw UNLOADED)
        -1
        (let [cl (aget (.-tbl-climb s) raw)]
          (if (or (and (not (zero? cl)) ^boolean (.climbCell s cl raw x y z))
                  (and (== raw (.-unk-id s)) ^boolean (.shaftAbove s x y z)))
            (.standClimb s x y z raw cl)
            (.standPlain s x y z raw))))))

  ;; the body hangs in the climbable: no floor, feet at the cell's floor
  (standClimb [s x y z raw cl]
    (let [shut (== cl CLIMB-TRAP-SHUT)]
      (if (and shut (not ^boolean (.-allow-shut s)))
        -1
        (do (set! (.-support s) raw)
            (if ^boolean (.fits s x y z (* y 16) (if shut 0 raw)) 0 -1)))))

  (standPlain [s x y z id]
    (let [t (aget (.-tbl-top s) id)
          b (aget (.-tbl-base s) id)]
      (cond
        ;; a block, a stairs: not a place to stand in. A partial one (fence, bamboo, wall, gate) leaves room at the
        ;; cell's edge: the cell stands on the floor below and the mask decides where the body fits
        (and (>= t WHOLE) (zero? b) (zero? (aget (.-tbl-partial s) id))) -1

        (and (pos? t) (zero? b) (< t WHOLE))
        (let [hz (aget (.-tbl-hazard s) id)]
          (if (or (== (aget (.-tbl-kind s) id) NARROW) (== hz HAZARD-AVOID) (== hz DAMAGE-TOUCH))
            -1
            (do (set! (.-support s) id)
                (if ^boolean (.fits s x y z (+ (* y 16) t) id) t -1))))

        :else
        ;; (an unseen floor, options.unknown, reads as unk-floor-id: a phantom floor, judged in consider)
        (let [below (let [b (.stateAt s x (dec y) z)] (if (== b (.-unk-id s)) (.-unk-floor-id s) b))]
          (if (== below UNLOADED)
            -1
            (let [tb (aget (.-tbl-floor s) below)
                  hz (aget (.-tbl-hazard s) below)]
              ;; a lower top is that cell's own stand height, not ground for this one (an open door, gate or trapdoor is a
              ;; panel at the cell's edge: nothing to stand on)
              (if (or (< tb WHOLE) (== (aget (.-tbl-kind s) below) NARROW) (== hz HAZARD-AVOID) (== hz DAMAGE-TOUCH)
                      (and (== (aget (.-tbl-kind s) below) OPENABLE) (zero? (aget (.-tbl-openable s) below))))
                -1
                (do (set! (.-support s) below)
                    (if ^boolean (.fits s x y z (+ (* y 16) (- tb WHOLE)) id) (- tb WHOLE) -1)))))))))

  (lavaAt [s x y z]
    (let [id (.stateAt s x y z)]
      (and (not (== id UNLOADED)) (== (aget (.-tbl-kind s) id) LAVA))))

  ;; lava beside the feet or beside the floor under them
  (lavaNear [s x y z]
    (when ^boolean (.farmlandNear s (dec x) (dec y) y (dec z) (inc x) (inc z))
      (loop [c 0]
        (cond
          (== c 4) false
          (or ^boolean (.lavaAt s (+ x (aget (.-adx s) c)) y (+ z (aget (.-adz s) c)))
              ^boolean (.lavaAt s (+ x (aget (.-adx s) c)) (dec y) (+ z (aget (.-adz s) c)))) true
          :else (recur (inc c))))))

  ;; 1 when a fall through this column ends in lava or beyond a safe drop, else 0 (an unseen cell ends the look as unloaded land)
  (holeRisk [s x y z]
    (loop [k 1]
      (if (> k (inc FREE-FALL))
        1
        (let [id (.stateAt s x (- y k) z)]
          (cond
            (or (== id UNLOADED) (== id (.-unk-id s))) 0
            (== (aget (.-tbl-kind s) id) LAVA) 1
            (or (pos? (aget (.-tbl-top s) id)) (== (aget (.-tbl-kind s) id) WATER)) 0
            :else (recur (inc k)))))))

  ;; 1 when the side column x z of a corner slide whose body's feet are at lo (1/16 absolute) has no floor and lava or a
  ;; block to avoid or that hurts to stand on (fire, powder snow, cobweb, magma, a lit campfire) under the hole within a
  ;; safe drop, else 0. A floor (collision), water, unloaded land or an unseen cell ends the look.
  (slideHoleRisk [s x z lo]
    (let [top-y (bit-shift-right (dec lo) 4)]
      (loop [k 0]
        (if (> k FREE-FALL)
          0
          (let [id (.stateAt s x (- top-y k) z)]
            (cond
              (or (== id UNLOADED) (== id (.-unk-id s))) 0
              (or (== (aget (.-tbl-kind s) id) LAVA) (== (aget (.-tbl-hazard s) id) HAZARD-AVOID) (== (aget (.-tbl-hazard s) id) DAMAGE-STAND)) 1
              (or (pos? (aget (.-tbl-top s) id)) (== (aget (.-tbl-kind s) id) WATER)) 0
              :else (recur (inc k))))))))

  ;; A body that falls into the cell under a gap cell x y z cannot jump back out: the cell two below is open too (no
  ;; collision, not water, seen), so the floor is two or more blocks down
  (pitBelow [s x y z]
    (let [id (.stateAt s x (- y 2) z)]
      (and (not= id UNLOADED) (not= id (.-unk-id s)) (zero? (aget (.-tbl-top s) id)) (not= (aget (.-tbl-kind s) id) WATER))))

  ;; A body standing level with a magma bubble column (or at the surface cell beside it, when it stands on the bank above)
  ;; can slip into it. Costs risk. `quiet` says no section near the cell holds such a column.
  (besideMagma [s x y z]
    (if ^boolean (.-quiet s)
      0
      (loop [c 0]
        (if (== c 4)
          0
          (let [x2 (+ x (aget (.-adx s) c))
                z2 (+ z (aget (.-adz s) c))]
            (if (or (== (aget (.-tbl-bubble s) (.stateAt s x2 y z2)) 2) (== (aget (.-tbl-bubble s) (.stateAt s x2 (dec y) z2)) 2))
              (.-c-beside-magma s)
              (recur (inc c))))))))

  ;; the hp of the fall a body standing at feet height h in x y z takes when the leaves under it decay: down through them
  ;; and any decaying leaves below (fallScan)
  (decayDamage [s x y z h]
    (.dropDamage s 1 (.fallScan s x (dec y) z (+ (* y 16) h) true)))

  ;; the hp of the fall a body hanging in a lone climbable cell at x y z takes (fallScan): no player holds on without
  ;; sneaking, it slides out of the cell. A climbable below is a column, gone on down by climb moves: 0
  (hangDamage [s x y z]
    (if ^boolean (.climbHere s x (dec y) z)
      0
      (.dropDamage s 1 (.fallScan s x (dec y) z (* y 16) false))))

  ;; standH plus what arriving there costs (enter-risk, enter-slow, enter-extra); -1 when not standable. Leaves that decay
  ;; under the feet cost their whole fall as damage: no log holds them, they go at any random tick. A body falls once, so
  ;; only the first stand of a run of such leaves (src-decays: the cell the move leaves stands on them too) costs it. A hang
  ;; in a lone climbable cell costs the fall out of it (hangDamage).
  (landing [s x y z]
    (let [h (.standH s x y z)]
      (if (neg? h)
        -1
        (let [hz (aget (.-tbl-hazard s) (.-support s))
              leaf (== (aget (.-tbl-dripleaf s) (.-support s)) 1)
              decay (if (and (== (aget (.-tbl-decays s) (.-support s)) 1) (not ^boolean (.-src-decays s))) (.decayDamage s x y z h) 0)
              hang (if (and (pos? (aget (.-tbl-climb s) (.-support s))) (== (.-support s) (.stateAt s x y z))) (.hangDamage s x y z) 0)]
          (set! (.-enter-risk s) (+ (.-touch s) (if (== hz DAMAGE-STAND) 1 0) (if ^boolean (.lavaNear s x y z) LAVA-ADJACENT 0)
                              (if leaf (.-c-dripleaf-risk s) 0) (.besideMagma s x y z) decay hang))
          (set! (.-enter-dmg s) (+ (.-touch s) (if (== hz DAMAGE-STAND) 1 0) decay hang))
          (set! (.-enter-slow s) (if (== hz SLOW) 1 0))
          (set! (.-enter-extra s) (if leaf (.-c-dripleaf s) 0))
          h)))))
