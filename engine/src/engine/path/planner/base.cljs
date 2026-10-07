(ns engine.path.planner.base
  "The planner's constants (block table codes, move kinds, costs, body sizes, search limits) and small helpers.")

;; block table codes (engine.path.blocks) and the snapshot's unloaded marker
(def ^:const UNLOADED 0xFFFF)
(def ^:const OPEN 0)
(def ^:const WATER 2)
(def ^:const LAVA 3)
(def ^:const CLIMB 4)
(def ^:const OPENABLE 5)
(def ^:const NARROW 6)
(def ^:const HAZARD-AVOID 1)
(def ^:const DAMAGE-STAND 2)
(def ^:const DAMAGE-TOUCH 3)
(def ^:const SLOW 4)
(def ^:const PORTAL 5)
(def ^:const CLIMB-INSIDE 1)
(def ^:const CLIMB-TRAP-SHUT 3)
(def ^:const LADDER 1)
(def ^:const VINES 2)
(def ^:const SCAFFOLDING 3)
(def ^:const OPEN-REDSTONE 2)
(def ^:const KIND-DOOR 1)
(def ^:const KIND-GATE 2)
(def ^:const KIND-TRAPDOOR 3)
(def ^:const ACT-BUTTON 1)
(def ^:const ACT-LEVER 2)
(def ^:const ACT-PLATE 3)
;; by a ladder's facing (1 east, 2 west, 3 south, 4 north): the way to the wall it hangs on
(def wall-dx #js [0 -1 1 0 0])
(def wall-dz #js [0 0 0 -1 1])
(def ^:const GRID 17)
;; kinds of move a search for an alternative path may refuse (options.avoid.kinds, bits)
(def ^:const AVOID-CLIMB 1)
(def ^:const AVOID-WATER 2)
(def ^:const AVOID-OPEN 4)

(def ^:const MOVE-WALK 1)
(def ^:const MOVE-DIAGONAL 2)
(def ^:const MOVE-JUMP 3)
(def ^:const MOVE-DROP 4)
(def ^:const MOVE-GAP 5)
(def ^:const MOVE-CORNER 6)
(def ^:const MOVE-CLIMB-UP 7)
(def ^:const MOVE-CLIMB-DOWN 8)
(def ^:const MOVE-JUMP-CLIMB 9)
(def ^:const MOVE-OPEN 10)
(def ^:const MOVE-SWIM 11)
(def ^:const MOVE-SWIM-UP 12)
(def ^:const MOVE-SWIM-DOWN 13)
(def ^:const MOVE-EXIT 14)

(def MOVE
  "the move codes as the JS planner exports them"
  #js {:START 0 :WALK 1 :DIAGONAL 2 :JUMP 3 :DROP 4 :GAP 5 :CORNER 6 :CLIMB_UP 7 :CLIMB_DOWN 8 :JUMP_CLIMB 9 :OPEN 10
       :SWIM 11 :SWIM_UP 12 :SWIM_DOWN 13 :EXIT 14})

(def ^:const WALK-S 0.23164234422052352) ; seconds per block: 1 / 4.317
(def ^:const SPRINT-S 0.1781895937277263) ; 1 / 5.612
(def ^:const SNEAK-S 0.7722007722007722) ; 1 / 1.295 (a sneaking body: 30% of the walking speed)

(def DEFAULT-COSTS
  "every cost the policy might want to change, in seconds; options.costs overrides (walkS and sprintS: the seconds a block of walking and of a gap jump's run costs, the gait)"
  #js {:climbUp 0.43 :climbDown 0.33 :jumpClimb 0.5 :open 1.0 :openRedstone 1.5 :openLever 6 :openPlate 0 :besideMagmaColumn 1
       :swimH 0.5 :swimUp 0.3 :swimDown 0.35 :exit 0.6 :current 0.3 :bubbleUp 0.08 :bubbleDown 0.12
       :airSupply 15 :airLimit 12 :airDrain 1 :airGrace 0 :airUsed 0 :maxWaterDrop 64 :dripleaf 0.2 :dripleafRisk 0.5
       :dropFactor 1
       :walkS WALK-S :sprintS SPRINT-S})

(def ^:const BODY 29) ; 1.8 blocks in 1/16, rounded up
(def ^:const STEP 9) ; 0.6 blocks
(def ^:const JUMP-UP 20) ; 1.25 blocks
(def ^:const ARC 32) ; headroom over a gap: feet + 2
(def ^:const ARC-UP 40) ; headroom over a gap whose landing is one block higher: feet + 2.5
(def ^:const JUMP-S 0.35) ; a jump up costs this much more than the walk it replaces
(def ^:const GAP-S 0.5) ; a gap jump's run-up and landing, on top of the sprint over its length
(def ^:const GAP-UP-S 0.3) ; a gap jump landing one block higher costs this much more than a level one
(def ^:const GAP-PIT-RISK 0.5) ; hp of risk for a jump over 3 above a pit it cannot jump out of: a short jump traps the body
(def ^:const TIGHT-S 0.1) ; careful walking: each tight cell entered costs this much more than a plain step
;; in picking a crossing, a leg that is not straight through the cell's free mask counts as this many 1/16 of distance: a bent
;; leg (the path carries bends, planner.bends) is taken only where no straight crossing leads on
(def ^:const BENT-COST 100)
(def ^:const CORNER-S 0.15) ; a diagonal slid along a blocked corner: slower than a straight one
(def ^:const SLOW-EXTRA 0.75) ; walking time grows by this much of itself per slow end of a move
(def ^:const LAVA-ADJACENT 0.5) ; hp of risk for a step with lava beside the feet
;; hp of risk for a corner slide whose open side is a hole onto lava or fire: the slide carries the body wholly over that
;; hole and it dips in. Large, so a way round of up
;; to about a minute's walk wins; still allowed, so a body in a pocket whose only way out is such a slide gets out.
(def ^:const HAZARD-SLIDE-RISK 10)
(def ^:const FREE-FALL 3)
(def ^:const BOUNCE-S 1.4) ; seconds a body takes to settle on a bouncing block, per square root of the blocks it fell (10 blocks: about 4.4 s)
(def ^:const BOUNCE-MARGIN 0.5) ; blocks a wall round a slime pad must reach over the bounce's first peak

(defn bounce-flight-run
  "bounce-flight, simulated (no cache). Plain numbers (-1: not yet) keep the loop free of truthiness checks."
  [fall]
  (loop [n 0 y (max 0 fall) vy 0 contact -1 top 0 peak -1]
    (let [ground (<= (+ y vy) 0)
          y' (if ground 0 (+ y vy))
          v (* (- (if (and ground (neg? vy)) (- vy) vy) 0.08) 0.98)
          vy' (if (< (js/Math.abs v) 0.003) 0 v)
          contact' (if (>= contact 0) contact (if ground n -1))
          top' (if (>= contact' 0) (max top y') 0)
          peak' (if (>= peak 0) peak (if (and ground (>= contact 0) (pos? top)) top -1))]
      (if (or (> n 2000) (and (>= contact' 0) ground (<= (js/Math.abs vy') 0.4)))
        {:peak (if (>= peak' 0) peak' top') :settle (- n (if (>= contact' 0) contact' n))}
        (recur (inc n) y' vy' contact' top' peak')))))

(def ^:private bounce-flights (js/Map.))

(defn bounce-flight
  "A body's bounce on a slime block after a fall of `fall` blocks from rest, in vanilla physics (the fall reverses at
  contact, then each tick vy = (vy - 0.08) * 0.98, under 0.003 is 0): {:peak the first bounce's height in blocks, :settle
  the ticks from the first contact until it is on the block with |vy| at most 0.4}. A 10-block fall: 6.33, 90. Kept per fall."
  [fall]
  (let [kept (.get bounce-flights fall)]
    (if (some? kept)
      kept
      (let [flight (bounce-flight-run fall)]
        (.set bounce-flights fall flight)
        flight))))

(defn bounce-wall
  "The blocks a wall round a slime pad must reach over the pad's level to hold a bounce after a fall of `fall` blocks: the
  first peak plus BOUNCE-MARGIN."
  [fall]
  (js/Math.ceil (+ (:peak (bounce-flight fall)) BOUNCE-MARGIN)))

(def pad-ring (for [dx [-1 0 1] dz [-1 0 1] :when (not (and (zero? dx) (zero? dz)))] [dx dz]))

(defn pad-cells
  "The pad a bounce off the bouncing block at x y z after a fall of `fall` blocks stays on, as the [dx dz] offsets of its
  bouncing cells ([0 0], the landing, first): each of the 8 blocks round it bounces or is a wall up to bounce-wall over that
  level. nil when one is neither (the bounce may carry the body off) or x y z does not bounce. bounce? and wall? are fns
  [x y z] -> bool."
  [bounce? wall? x y z fall]
  (when ^boolean (bounce? x y z)
    (let [k (bounce-wall fall)]
      (reduce (fn [cells [dx dz]]
                (let [bx (+ x dx) bz (+ z dz)]
                  (cond
                    ^boolean (bounce? bx y bz) (conj cells [dx dz])
                    (every? #(wall? bx (+ y %) bz) (range 1 (inc k))) cells
                    :else (reduced nil))))
              [[0 0]] pad-ring))))
(def ^:const EXIT-SLACK 1) ; 1/16: a floating body exits onto land up to this over the water's top face
(def ^:const SQRT2 1.4142135623730951)
(def ^:const OCTILE-SLACK 1.0824) ; octile length of a vector of length r is at most this times r
(def ^:const SPAN 4096) ; nodes further than 2048 blocks from the start in x or z are not searched
(def ^:const HALF 2048)
(def ^:const MIN-CLOSER 2) ; an exhausted search is a partial result only when it got this many blocks closer
(def ^:const OPEN-REACH 2) ; a node this many columns or fewer from unloaded land stands at the loaded edge (oneWay.open)
(def ^:const FLOOD-GROWTH 4) ; a goal flood that ran out of budget runs again with this many times the budget,
(def ^:const FLOOD-SPACING 8) ; after this many times the expansions: the floods cost about half the search
(def ^:const WHOLE 16) ; a full block in 1/16
(def ^:const BODY-BLOCKS 1.8)
(def ^:const REGIONS 16) ; regions of one cell that can be nodes (4 bits of the key)
(def ^:const AIR-REFILL 4) ; seconds of air a second with the head out of water gives back (vanilla: +4 air a tick, -1 under water)
(def ^:const DROWN-HP 2) ; hp a second with no air left costs (vanilla: 2 every 20 ticks at 0 air)
(def ^:const LETHAL-S 1e6) ; seconds a lethal drowning costs, and each hp past it: any plan that does not drown wins
(def ^:const AIR-STEP 1) ; seconds of air that make an arrival at a node already reached worth a record of its own
(def ^:const DMG-STEP 0) ; hp of damage that make an arrival at a node already reached worth a record of its own (a finite budget: exact)
(def ^:const TABLE 8192) ; slots of the direct-mapped tight-cell caches
(def ^:const FLOOD-TABLE 65536) ; slots of the direct-mapped cache of the goal flood's stand heights
(def ^:const FLOOD-MEMO 40000) ; cells whose moves the flood keeps (flood-moves) before it starts the memo afresh
(def ^:const SNAP 6) ; a blocked boundary point takes the region of the nearest free position within this many 1/16
(def ^:const DROP-INSET 5) ; a body walking off a ledge falls once its 0.31 half-width clears it: 5/16 past the edge
(def ^:const NONE -1e9) ; surfaceY of a water column that does not reach open air

(def CENTRE "the representative point of an ordinary cell, in 1/16" #js {:px 8 :pz 8})
(def NO-MASKS #js [])
(def ATTACH "where a button's or lever's supporting block lies: index 1 +x, 2 -x, 3 +z, 4 -z, 5 +y, 6 -y"
  #js [#js [0 0 0] #js [1 0 0] #js [-1 0 0] #js [0 0 1] #js [0 0 -1] #js [0 1 0] #js [0 -1 0]])
(def CLIMB-NAMES #js [js/undefined "ladder" "vines" "scaffolding"])

(defn next-pow2 [n] (js/Math.pow 2 (js/Math.ceil (js/Math.log2 (js/Math.max 2 n)))))

(defn grown [^js array size]
  (let [^js bigger (js/Reflect.construct (.-constructor array) #js [size])]
    (.set bigger array)
    bigger))

(defn cell-key
  "one number for a cell, the key of options.avoid.cells: x and z within 2^20 of 0, y within 512"
  [x y z]
  (+ (* (+ (* (+ x 1048576) 2097152) (+ z 1048576)) 1024) (+ y 512)))

;; ---- block lists ----

(defn same? [^js a ^js b]
  (and (== (.-x a) (.-x b)) (== (.-y a) (.-y b)) (== (.-z a) (.-z b))))

(defn in-list? [^js b ^js list]
  (true? (.some list (fn [o] (same? o b)))))

;; the blocks of `blocks` that are not in `others`
(defn not-in [^js blocks ^js others]
  (.filter blocks (fn [b] (not ^boolean (in-list? b others)))))

(defn breathable?
  "Whether a head in the block of state id (table: the block table) breathes: open, a climbable or an openable block, or
  a bubble column (vanilla drains no air with the eyes in one); not a solid block, water or lava."
  [^js table id]
  (and (not (== id UNLOADED))
       (let [k (aget (.-kind table) id)]
         (or (== k OPEN) (== k CLIMB) (== k OPENABLE) (pos? (aget (.-bubble table) id))))))
