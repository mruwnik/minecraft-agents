(ns engine.path.planner-tuned
  "The path planner's search, in ClojureScript and written for speed: an A* over typed arrays of cells, moves and costs
   (climbing, water, doors, tight cells), returning a status, reason, expanded count, path, costs and summary.

   How it is kept fast: all search state is in the mutable fields of one Search object and every function of the search
   is a method of it, so the emitted JavaScript is property reads, typed-array indexing and direct method calls. Rules the
   hot code keeps: no persistent data, no seqs, no keywords; every test is a comparison or a ^boolean hinted call (so no
   truthiness check is emitted: the compiled file has one cljs.core.truth_, in count-steps); `let`, `do` and `loop` only in
   statement or tail position (in expression position they compile to a closure called on the spot); values a JS function
   returned on the side (support, touch, enter-risk, enter-slow, enter-extra, ty) are fields; where a closure would be
   passed (swimEdge's emit, the opening pass's edge) the call is split in a begin and an end, or `edge-mode` is switched.

   Called through interop, not written in the search: the snapshot (stateAt, sectionHas, hasColumn), the state table's arrays and the
   free-space masks of space.mjs (options.space: boxesNear, freeMask, labelRegions).

   Goal sets: query.goals, an array of goals ({kind x y z range} as query.goal), plans to the nearest of them by cost in
   one search: the goal test is the cell in any goal's area, the heuristic the least of the goals' (admissible and
   consistent as each is), and a found result's goal is the index of the goal reached. A goal set runs no goal flood and
   answers no goal-not-standable or goal-unloaded (they prove things of one goal); query.goal is then not read.

   Options: options.avoid {kinds, cells, factor}, which engine.path.alternatives sets to search for another path
   (see avoidCost), and options.limits {kinds, gap, corner}, what the walker can do: kinds (the same bits) are never planned,
   gap(x, y, z, h, move, lx, ly, lz, lh) -> false refuses a gap jump from the takeoff node (feet cell x,y,z, stand h in 1/16,
   reached by move) to the landing, and corner(x, y, z, h, lx, ly, lz, lh) -> false refuses a diagonal jump with one blocked
   side (a corner slide) from the takeoff node to the landing (engine.path.executor/planner-limits). Without them the search
   is unrestricted.")

(set! *warn-on-infer* true)

;; block table codes (blocks.mjs) and the snapshot's unloaded marker
(def ^:const UNLOADED 0xFFFF)
(def ^:const OPEN 0)
(def ^:const WATER 2)
(def ^:const LAVA 3)
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

(def DEFAULT-COSTS
  "every cost the policy might want to change, in seconds; options.costs overrides"
  #js {:climbUp 0.43 :climbDown 0.33 :jumpClimb 0.5 :open 1.0 :openRedstone 1.5 :openPlate 0 :besideMagmaColumn 1
       :swimH 0.5 :swimUp 0.3 :swimDown 0.35 :exit 0.6 :current 0.3 :bubbleUp 0.08 :bubbleDown 0.12
       :airSupply 15 :airLimit 12 :maxWaterDrop 64 :dripleaf 0.2 :dripleafRisk 0.5})

(def ^:const BODY 29) ; 1.8 blocks in 1/16, rounded up
(def ^:const STEP 9) ; 0.6 blocks
(def ^:const JUMP-UP 20) ; 1.25 blocks
(def ^:const ARC 32) ; headroom over a gap: feet + 2
(def ^:const ARC-UP 40) ; headroom over a gap whose landing is one block higher: feet + 2.5
(def ^:const WALK-S 0.23164234422052352) ; seconds per block: 1 / 4.317
(def ^:const SPRINT-S 0.1781895937277263) ; 1 / 5.612
(def ^:const JUMP-S 0.35) ; a jump up costs this much more than the walk it replaces
(def ^:const GAP-S 0.5) ; a gap jump's run-up and landing, on top of the sprint over its length
(def ^:const GAP-UP-S 0.3) ; a gap jump landing one block higher costs this much more than a level one
(def ^:const GAP-PIT-RISK 0.5) ; hp of risk for a jump over 3 above a pit it cannot jump out of: a short jump traps the body
(def ^:const TIGHT-S 0.1) ; careful walking: each tight cell entered costs this much more than a plain step
(def ^:const CORNER-S 0.15) ; a diagonal slid along a blocked corner: slower than a straight one
(def ^:const SLOW-EXTRA 0.75) ; walking time grows by this much of itself per slow end of a move
(def ^:const LAVA-ADJACENT 0.5) ; hp of risk for a step with lava beside the feet
(def ^:const FREE-FALL 3)
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
(def ^:const AIR-STEP 1) ; seconds of air that make an arrival at a node already reached worth a record of its own
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

(defn- next-pow2 [n] (js/Math.pow 2 (js/Math.ceil (js/Math.log2 (js/Math.max 2 n)))))

(defn- grown [^js array size]
  (let [^js bigger (js/Reflect.construct (.-constructor array) #js [size])]
    (.set bigger array)
    bigger))

(defn- fall-damage [fall16] (js/Math.max 0 (js/Math.ceil (- (/ fall16 16) FREE-FALL))))

(defn cell-key
  "one number for a cell, the key of options.avoid.cells: x and z within 2^20 of 0, y within 512"
  [x y z]
  (+ (* (+ (* (+ x 1048576) 2097152) (+ z 1048576)) 1024) (+ y 512)))

;; ---- results (cold: once per plan) ----

(defn- stand16 [^js step] (+ (* (.-y step) 16) (.-h step)))

(defn- count-steps [^js steps from pred]
  (loop [k from n 0]
    (if (< k (.-length steps))
      (recur (inc k) (if ^boolean (pred (aget steps k) (when (pos? k) (aget steps (dec k)))) (inc n) n))
      n)))

(defn- some-of [n one many] (when (pos? n) (if (== n 1) one (str n many))))

(defn- phrase [verb noun n] (when (pos? n) (str verb " " n " " noun (if (> n 1) "s" ""))))

(defn- move-of [^js step] (.-move step))

(defn- climbing-move? [m] (and (>= m MOVE-CLIMB-UP) (<= m MOVE-OPEN)))

(defn- swimming-move? [m] (>= m MOVE-SWIM))

(defn- distance [^js a ^js b] (js/Math.hypot (- (.-x a) (.-x b)) (- (.-z a) (.-z b))))

;; consecutive legs with one key are one run: "ladder up 9"; `key-of` gives a leg's key or nil for a leg that ends the run
(defn- run-strings [^js legs key-of]
  (let [keys #js []
        counts #js []]
    (loop [k 0 prev nil]
      (when (< k (.-length legs))
        (let [key (key-of (aget legs k))]
          (cond
            (nil? key) (recur (inc k) nil)
            (and (some? prev) (identical? prev key))
            (do (aset counts (dec (.-length counts)) (inc (aget counts (dec (.-length counts)))))
                (recur (inc k) key))
            :else (do (.push keys key)
                      (.push counts 1)
                      (recur (inc k) key))))))
    (.map keys (fn [key k] (str key " " (aget counts k))))))

(defn- legs-of [^js steps]
  (let [legs #js []]
    (loop [k 1]
      (when (< k (.-length steps))
        (.push legs #js [(aget steps k) (aget steps (dec k))])
        (recur (inc k))))
    legs))

(defn- depth [^js leg]
  (let [^js s (aget leg 0) ^js p (aget leg 1)]
    (js/Math.round (/ (- (stand16 p) (stand16 s)) 16))))

(defn- sum-hypot [^js legs]
  (.reduce legs (fn [sum ^js leg] (+ sum (distance (aget leg 0) (aget leg 1)))) 0))

(defn- max-of [^js xs] (.apply js/Math.max nil xs))

(defn- same? [^js a ^js b]
  (and (== (.-x a) (.-x b)) (== (.-y a) (.-y b)) (== (.-z a) (.-z b))))

(defn- in-list? [^js b ^js list]
  (true? (.some list (fn [o] (same? o b)))))

;; the blocks of `blocks` that are not in `others`
(defn- not-in [^js blocks ^js others]
  (.filter blocks (fn [b] (not ^boolean (in-list? b others)))))

;; ---- the search ----

(deftype Search
  [;; the world
   ^js snapshot ^js table ^js space
   tbl-top tbl-base tbl-kind tbl-hazard tbl-stair tbl-partial tbl-climb tbl-climb-name tbl-facing tbl-floor tbl-special
   tbl-flowing tbl-bubble tbl-magma tbl-dripleaf tbl-openable tbl-open-state tbl-open-kind tbl-door-half tbl-activator
   tbl-attach tbl-farmland min-y
   ;; the query
   from-x from-y from-z from-px from-pz goal-x goal-y goal-z goal-range slack ^boolean near ^boolean goal-unloaded
   ;; a goal set (query.goals; n-goals 0: the one goal above): each goal's cell, squared range, octile slack, and 1 for a
   ;; sphere (near) or 0 for an x-z disc
   n-goals ^js g-xs ^js g-ys ^js g-zs ^js g-r2 ^js g-slack ^js g-near
   ;; options
   max-nodes max-drop weight risk-weight ^:mutable goal-flood ^:mutable flood-after pre-flood frontier-reach
   ;; a search for an alternative path (options.avoid, see avoidCost): kinds of move refused, cells near earlier paths
   ^boolean avoiding avoid-kinds ^js avoid-cells avoid-factor
   ;; what the walker can do (options.limits): kinds of move never planned, a test of each gap jump and of each corner
   ;; slide jump (nil: every one)
   limit-kinds ^js limit-gap ^js limit-corner
   ;; costs (options.costs over DEFAULT-COSTS)
   c-climb-up c-climb-down c-jump-climb c-open c-open-redstone c-open-plate c-beside-magma c-swim-h c-swim-up c-swim-down
   c-exit c-current c-bubble-up c-bubble-down c-air-supply c-air-limit c-max-water-drop c-dripleaf c-dripleaf-risk
   ;; search box: start and goal, plus margins
   bx0 bx1 bz0 bz1 by0 by1
   ;; the 8 directions: 0-3 cardinal (east, west, south, north), 4-7 diagonal
   adx adz
   ;; node storage: open-addressing hash on packed coordinates, binary heap, all typed arrays
   ^:mutable cap ^:mutable slots ^:mutable hash-table ^:mutable node-keys
   ^:mutable xs ^:mutable ys ^:mutable zs ^:mutable hs ^:mutable moves ^:mutable slows ^:mutable corners
   ^:mutable shapes ; see packShape
   ^:mutable parents ^:mutable secs ^:mutable risks
   ^:mutable airs ; seconds of air used since the last breath
   ^:mutable peaks ; the most air used at any point of the path to the node
   ^:mutable wsecs ; seconds of the path spent swimming
   ^:mutable opens ; 1 + index in open-lists of what the move to the node opened, 0 for nothing
   ^:mutable gs ^:mutable fs
   ^:mutable heap-pos ; index in heap, -1 not yet in it, -2 once expanded
   ^:mutable heap ^:mutable n-nodes ^:mutable heap-n
   ;; what the last standH / landing / neighbour / gapLanding found, beside its return value
   ^:mutable support ; state id the last standH stood on
   ^:mutable touch ; DAMAGE_TOUCH cells inside the last standH body
   ^:mutable enter-risk ^:mutable enter-slow ^:mutable enter-extra ; set by landing
   ^:mutable ty ; y of the cell the last neighbour found
   ^:mutable gap-y ; y of the cell the last gapLanding found
   ^:mutable ^boolean quiet ; no special block near the cell being expanded: nothing it looks at is tight
   ^:mutable ^boolean open-mode ; the opening pass of an expansion: closed doors, gates and trapdoors read as open
   ^:mutable ^boolean allow-shut ; standH refuses a shut trapdoor's cell unless the caller pays for opening it
   ^:mutable ^boolean after-exit ; the node being expanded was reached by an EXIT
   ^:mutable ^boolean gap-seen ; a ladder was refused because the feet would leave it at a gap
   ^:mutable ^boolean air-seen ; a swim move was refused for lack of air
   ^:mutable ^boolean enters-shut ; the last enterCell was a shut trapdoor
   ;; what the swim move being made adds to the node, and what the move being made opens
   ^:mutable move-air ^:mutable move-peak ^:mutable move-water ^:mutable move-open
   ^js open-lists
   ^js activators
   ^:mutable ^js view ; what the free-space masks read: a closed wooden trapdoor over a ladder reads as air
   ;; where moves go: 0 into the search (or the flood's probe), 2 recording the edges the first pass finds, 3 the opening pass
   ^:mutable edge-mode ^:mutable ^js seen-edges ^:mutable ^js door-arrival ^:mutable ^js door-here
   ^:mutable ^boolean door-through ^:mutable open-x ^:mutable open-y ^:mutable open-z
   ;; tight cells: direct-mapped caches (keys stored +1 so zeroed memory reads as empty), masks per (cell, height)
   column-keys column-flags cell-keys cell-flags ^js mask-cache ^js tight-seen ^js pick ^js best-d ^js best-at
   ^js surface-cache ^js dive-cache
   ^:mutable masks ^:mutable tight-masks ^:mutable regions-seen ^:mutable mask-ms
   ;; the goal flood: moves go to the probe instead of the search while it runs; fr the probed cell's region (-1 any)
   ^:mutable ^boolean flooding ^:mutable fx ^:mutable fy ^:mutable fz ^:mutable fr ^:mutable ^boolean hit ^:mutable flooded ^:mutable pre-flooded
   ^:mutable ^boolean flood-pending ^:mutable ^boolean leaked
   ;; the moves out of each cell the flood has expanded (key -> array of the keys its moves enter, see floodNode), for the
   ;; whole search: the world does not change under it, so a cell is expanded once however many floods ask about it
   ^js flood-moves ^:mutable ^js flood-out
   ;; floodH of the cells the floods have looked at: a direct-mapped cache (key of region 0, stored +1 -> h; made by the
   ;; first flood), and the key the moves must enter to reach the flood's current node (see floodMovesOf)
   ^:mutable ^js flood-h-keys ^:mutable ^js flood-h-vals ^:mutable flood-target
   ;; progress
   ^:mutable ^boolean started ^:mutable ^boolean finished ^:mutable reason ^:mutable ^boolean over-budget
   ^:mutable ^boolean boxed ; some node was refused by the box
   ^:mutable goal-node ^:mutable best-node ; expanded node nearest the goal
   ^:mutable best-distance ^:mutable start-distance ^:mutable expanded ^:mutable t0 ^:mutable elapsed
   ^:mutable start-h ^:mutable start-slow
   ;; the returnable search (options.returnable): no step the body cannot undo is planned; the drops it has not yet probed
   ^boolean returnable ^js held ^:mutable ^boolean replaying
   ;; the late flood, one backward flood continued by each late flood and the flood at the end (lateFloodBegin): its seen
   ;; keys, queue (x y z region per node) and the head of the queue, the budget of the run in progress, whether a run is in
   ;; progress, whether it met the start (lf-seed-open: among the goal's own cells), whether the run is the one at the end
   ^:mutable ^js lf-seen ^:mutable ^js lf-queue ^:mutable lf-head ^:mutable lf-budget ^:mutable ^boolean lf-active
   ^:mutable ^boolean lf-open ^:mutable ^boolean lf-seed-open ^:mutable ^boolean lf-end
   ;; the walker's limits (options.limits) refused some move: a search without them could have gone further
   ^:mutable ^boolean limit-refused
   ;; edge capture (engine.path.regions, capture-search): capturing makes the goal-dependent rules a superset (every cell
   ;; reads as in the goal: a portal is standable, every dive is worth it); while capture-out is an array, each move's edge
   ;; is pushed there as x y z region cost (seconds + risk-weight * risk) instead of going to the search
   ^:mutable ^boolean capturing ^:mutable ^js capture-out]

  Object

  ;; ---- the world, as the body sees it ----

  ;; What the body sees: in the opening pass a closed door, gate or trapdoor reads as open, so the move is judged with the
  ;; block as it will be once opened; `.stateAt snapshot` is the block as it stands.
  (stateAt [s x y z]
    (let [id (.stateAt snapshot x y z)]
      (if (and open-mode (pos? (aget tbl-openable id)))
        (aget tbl-open-state id)
        id)))

  ;; does the climbable state `id` (climb = cl) at x,y,z make its cell one the body climbs in? An open trapdoor counts over
  ;; a ladder: vanilla climbs it when it faces the ladder's way (the client too, with tools/patch-deps.mjs), and over a
  ;; ladder of another facing its panel leaves the ladder's top edge free to stand on and jump from (the step into it is
  ;; aimed at the ladder's wall, see hatchWall)
  (climbCell [s cl id x y z]
    (if (== cl CLIMB-INSIDE)
      true
      (let [below (.stateAt snapshot x (dec y) z)]
        (and (not (== below UNLOADED)) (== (aget tbl-climb-name below) LADDER)))))

  (climbHere [s x y z]
    (let [id (.stateAt snapshot x y z)]
      (if (== id UNLOADED)
        false
        (let [cl (aget tbl-climb id)]
          (and (not (zero? cl)) ^boolean (.climbCell s cl id x y z))))))

  ;; a closed wooden trapdoor above a ladder: entering the cell costs an OPEN
  (shutAt [s x y z]
    (let [id (.stateAt snapshot x y z)]
      (and (not (== id UNLOADED)) (== (aget tbl-climb id) CLIMB-TRAP-SHUT) ^boolean (.climbCell s CLIMB-TRAP-SHUT id x y z))))

  (viewAt [s x y z]
    (let [id (.stateAt s x y z)]
      (if (and (== (aget tbl-climb id) CLIMB-TRAP-SHUT) ^boolean (.climbCell s CLIMB-TRAP-SHUT id x y z))
        0
        id)))

  ;; a cell the body must not be in: an AVOID hazard, or a portal unless the cell (or the one under it) is in the goal
  (avoids [s id x y z]
    (let [hz (aget tbl-hazard id)]
      (or (== hz HAZARD-AVOID)
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
              (let [t (aget tbl-top id)
                    k (aget tbl-kind id)]
                (if (or (and (pos? t) (> (+ (* y 16) t) lo) (< (+ (* y 16) (aget tbl-base id)) hi))
                        (== k WATER) (== k LAVA) (== k NARROW) ^boolean (.avoids s id x y z))
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
            (if (or (== id UNLOADED) (== (aget tbl-kind id) LAVA) ^boolean (.avoids s id x y z))
              2
              (let [t (aget tbl-top id)]
                (if (or (== (aget tbl-kind id) NARROW)
                        (and (pos? t) (> (+ (* y 16) t) lo) (< (+ (* y 16) (aget tbl-base id)) hi)))
                  (recur (inc y) 1)
                  (recur (inc y) blocked)))))))))

  ;; DAMAGE_TOUCH cells (berry bush, wither rose, cactus) in column x,z over lo..hi (1/16 absolute): a diagonal brushes both
  ;; side columns, so each one hurts as one in the body's own column does (`fits`' touch)
  (sideTouch [s x z lo hi]
    (let [last-y (bit-shift-right (dec hi) 4)]
      (loop [y (bit-shift-right lo 4)
             touched 0]
        (if (> y last-y)
          touched
          (let [id (.stateAt s x y z)]
            (recur (inc y) (if (and (not (== id UNLOADED)) (== (aget tbl-hazard id) DAMAGE-TOUCH)) (inc touched) touched)))))))

  ;; does the body fit in the column of feet cell y at x,z, its feet at absolute lo (1/16)? `id` stands for the feet cell
  ;; itself. Sets `touch`. Collision that leaves gaps is for the tight-cell mask to judge, not for refusing the cell.
  (fits [s x y z lo id]
    (let [hi (+ lo BODY)
          last-y (bit-shift-right (dec hi) 4)]
      (loop [k y
             touched 0]
        (if (> k last-y)
          (do (set! touch touched)
              true)
          (let [cid (if (== k y) id (.stateAt s x k z))]
            (if (== cid UNLOADED)
              false
              (let [ct (aget tbl-top cid)
                    kd (aget tbl-kind cid)]
                (cond
                  (or (== kd WATER) (== kd LAVA) ^boolean (.avoids s cid x k z)) false
                  (and (zero? (aget tbl-partial cid))
                       (or (and (pos? ct) (> (+ (* k 16) ct) lo) (< (+ (* k 16) (aget tbl-base cid)) hi))
                           (== kd NARROW))) false
                  (== (aget tbl-hazard cid) DAMAGE-TOUCH) (recur (inc k) (inc touched))
                  :else (recur (inc k) touched)))))))))

  ;; stand height at a feet cell, or -1
  (standH [s x y z]
    (let [raw (.stateAt s x y z)]
      (if (== raw UNLOADED)
        -1
        (let [cl (aget tbl-climb raw)]
          (if (and (not (zero? cl)) ^boolean (.climbCell s cl raw x y z))
            (.standClimb s x y z raw cl)
            (.standPlain s x y z raw))))))

  ;; the body hangs in the climbable: no floor, feet at the cell's floor
  (standClimb [s x y z raw cl]
    (let [shut (== cl CLIMB-TRAP-SHUT)]
      (if (and shut (not allow-shut))
        -1
        (do (set! support raw)
            (if ^boolean (.fits s x y z (* y 16) (if shut 0 raw)) 0 -1)))))

  (standPlain [s x y z id]
    (let [t (aget tbl-top id)
          b (aget tbl-base id)]
      (cond
        ;; a block, a stairs: not a place to stand in. A partial one (fence, bamboo, wall, gate) leaves room at the
        ;; cell's edge: the cell stands on the floor below and the mask decides where the body fits
        (and (>= t WHOLE) (zero? b) (zero? (aget tbl-partial id))) -1

        (and (pos? t) (zero? b) (< t WHOLE))
        (let [hz (aget tbl-hazard id)]
          (if (or (== (aget tbl-kind id) NARROW) (== hz HAZARD-AVOID) (== hz DAMAGE-TOUCH))
            -1
            (do (set! support id)
                (if ^boolean (.fits s x y z (+ (* y 16) t) id) t -1))))

        :else
        (let [below (.stateAt s x (dec y) z)]
          (if (== below UNLOADED)
            -1
            (let [tb (aget tbl-floor below)
                  hz (aget tbl-hazard below)]
              ;; a lower top is that cell's own stand height, not ground for this one (an open door, gate or trapdoor is a
              ;; panel at the cell's edge: nothing to stand on)
              (if (or (< tb WHOLE) (== (aget tbl-kind below) NARROW) (== hz HAZARD-AVOID) (== hz DAMAGE-TOUCH)
                      (and (== (aget tbl-kind below) OPENABLE) (zero? (aget tbl-openable below))))
                -1
                (do (set! support below)
                    (if ^boolean (.fits s x y z (+ (* y 16) (- tb WHOLE)) id) (- tb WHOLE) -1)))))))))

  (lavaAt [s x y z]
    (let [id (.stateAt s x y z)]
      (and (not (== id UNLOADED)) (== (aget tbl-kind id) LAVA))))

  ;; lava beside the feet or beside the floor under them
  (lavaNear [s x y z]
    (loop [c 0]
      (cond
        (== c 4) false
        (or ^boolean (.lavaAt s (+ x (aget adx c)) y (+ z (aget adz c)))
            ^boolean (.lavaAt s (+ x (aget adx c)) (dec y) (+ z (aget adz c)))) true
        :else (recur (inc c)))))

  ;; 1 when a fall through this column ends in lava or beyond a safe drop, else 0
  (holeRisk [s x y z]
    (loop [k 1]
      (if (> k (inc FREE-FALL))
        1
        (let [id (.stateAt s x (- y k) z)]
          (cond
            (== id UNLOADED) 0
            (== (aget tbl-kind id) LAVA) 1
            (or (pos? (aget tbl-top id)) (== (aget tbl-kind id) WATER)) 0
            :else (recur (inc k)))))))

  ;; A body that falls into the cell under a gap cell x y z cannot jump back out: the cell two below is open too (no
  ;; collision, not water), so the floor is two or more blocks down
  (pitBelow [s x y z]
    (let [id (.stateAt s x (- y 2) z)]
      (and (not= id UNLOADED) (zero? (aget tbl-top id)) (not= (aget tbl-kind id) WATER))))

  ;; A body standing level with a magma bubble column (or at the surface cell beside it, when it stands on the bank above)
  ;; can slip into it. Costs risk. `quiet` says no section near the cell holds such a column.
  (besideMagma [s x y z]
    (if quiet
      0
      (loop [c 0]
        (if (== c 4)
          0
          (let [x2 (+ x (aget adx c))
                z2 (+ z (aget adz c))]
            (if (or (== (aget tbl-bubble (.stateAt s x2 y z2)) 2) (== (aget tbl-bubble (.stateAt s x2 (dec y) z2)) 2))
              c-beside-magma
              (recur (inc c))))))))

  ;; standH plus what arriving there costs (enter-risk, enter-slow, enter-extra); -1 when not standable
  (landing [s x y z]
    (let [h (.standH s x y z)]
      (if (neg? h)
        -1
        (let [hz (aget tbl-hazard support)
              leaf (== (aget tbl-dripleaf support) 1)]
          (set! enter-risk (+ touch (if (== hz DAMAGE-STAND) 1 0) (if ^boolean (.lavaNear s x y z) LAVA-ADJACENT 0)
                              (if leaf c-dripleaf-risk 0) (.besideMagma s x y z)))
          (set! enter-slow (if (== hz SLOW) 1 0))
          (set! enter-extra (if leaf c-dripleaf 0))
          h))))

  ;; ---- goal ----

  (reached [s x y z]
    (cond
      capturing true
      (pos? n-goals)
      (>= (.goalAt s x y z) 0)
      :else
      (let [dx (- x goal-x)
            dz (- z goal-z)]
        (if near
          (<= (+ (* dx dx) (* (- y goal-y) (- y goal-y)) (* dz dz)) (* goal-range goal-range))
          (<= (+ (* dx dx) (* dz dz)) (* goal-range goal-range))))))

  (capturing! [s] (set! capturing true))

  ;; the moves out of node (x, y, z, region; h its stand height, region -1 for a cell the body fits only once something
  ;; is opened) as the search would make them from a node with no history (no air used, not slowed, not reached by an
  ;; exit: what history only takes away), pushed onto out as x y z region cost; for engine.path.regions
  (captureAt [s x y z h region ^js out]
    (set! capture-out out)
    (.expandAt s x y z h 0 -1 region)
    (set! capture-out nil)
    out)

  ;; the index of the first goal of the set whose area holds the cell, -1 for none
  (goalAt [s x y z]
    (loop [i 0]
      (if (< i n-goals)
        (let [dx (- x (aget g-xs i))
              dy (if (== 1 (aget g-near i)) (- y (aget g-ys i)) 0)
              dz (- z (aget g-zs i))]
          (if (<= (+ (* dx dx) (* dy dy) (* dz dz)) (aget g-r2 i))
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
    (if (pos? n-goals)
      (loop [i 0 best js/Infinity]
        (if (< i n-goals)
          (recur (inc i) (js/Math.min best (.octileTo s x z (aget g-xs i) (aget g-zs i))))
          best))
      (.octileTo s x z goal-x goal-z)))

  ;; a goal set's heuristic is the least of its goals' (each admissible and consistent, so their least is too)
  (heuristic [s x z]
    (if (pos? n-goals)
      (loop [i 0 best js/Infinity]
        (if (< i n-goals)
          (recur (inc i) (js/Math.min best (js/Math.max 0 (- (.octileTo s x z (aget g-xs i) (aget g-zs i)) (aget g-slack i)))))
          (* best WALK-S)))
      (* (js/Math.max 0 (- (.distanceTo s x z) slack)) WALK-S)))

  ;; ---- node storage ----

  (hashOf [s x y z region]
    (unsigned-bit-shift-right
     (bit-xor (js/Math.imul (+ (- x from-x) HALF) 73856093)
              (js/Math.imul (- y min-y) 19349663)
              (js/Math.imul (+ (- z from-z) HALF) 83492791)
              (js/Math.imul region 668265263))
     0))

  (keyOf [s x y z region]
    (+ (* (+ (* (+ (* (- y min-y) SPAN) (+ (- x from-x) HALF)) SPAN) (+ (- z from-z) HALF)) REGIONS) region))

  ;; the slot holding the node with this key, or the empty slot where it belongs
  (findSlot [s key start]
    (loop [slot start]
      (let [node (aget hash-table slot)]
        (if (or (== node -1) (== (aget node-keys node) key))
          slot
          (recur (bit-and (inc slot) (dec slots)))))))

  ;; the first empty slot from `start`, whatever keys sit before it
  (emptySlot [s start]
    (loop [slot start]
      (if (== (aget hash-table slot) -1)
        slot
        (recur (bit-and (inc slot) (dec slots))))))

  (grow [s]
    (set! cap (js/Math.min max-nodes (* cap 2)))
    (set! node-keys (grown node-keys cap))
    (set! xs (grown xs cap))
    (set! ys (grown ys cap))
    (set! zs (grown zs cap))
    (set! hs (grown hs cap))
    (set! moves (grown moves cap))
    (set! slows (grown slows cap))
    (set! corners (grown corners cap))
    (set! shapes (grown shapes cap))
    (set! parents (grown parents cap))
    (set! secs (grown secs cap))
    (set! risks (grown risks cap))
    (set! airs (grown airs cap))
    (set! peaks (grown peaks cap))
    (set! wsecs (grown wsecs cap))
    (set! opens (grown opens cap))
    (set! gs (grown gs cap))
    (set! fs (grown fs cap))
    (set! heap-pos (grown heap-pos cap))
    (set! heap (grown heap cap))
    (set! slots (next-pow2 (* cap 2)))
    (set! hash-table (.fill (js/Int32Array. slots) -1))
    ;; newest first: a rival record of a node (same key) sits ahead of the one it rivals in the probe, so lookups find it
    (loop [i (dec n-nodes)]
      (when (>= i 0)
        (let [x (aget xs i)
              y (aget ys i)
              z (aget zs i)]
          (aset hash-table (.emptySlot s (bit-and (.hashOf s x y z (bit-and (aget shapes i) 15)) (dec slots))) i))
        (recur (dec i)))))

  ;; the empty (or, for a rival record, the first record's) slot for a new key after making room
  (grownSlot [s x y z region key]
    (.grow s)
    (.findSlot s key (bit-and (.hashOf s x y z region) (dec slots))))

  (addNode [s x y z region key slot]
    (let [free (if (== n-nodes cap) (.grownSlot s x y z region key) slot)
          node n-nodes]
      (set! n-nodes (inc node))
      (aset hash-table free node)
      (aset node-keys node key)
      (aset xs node x)
      (aset ys node y)
      (aset zs node z)
      (aset heap-pos node -1)
      node))

  ;; best first by f, then deeper (larger g), then earlier discovered: ties never depend on memory layout
  (before [s a b]
    (or (< (aget fs a) (aget fs b))
        (and (== (aget fs a) (aget fs b))
             (or (> (aget gs a) (aget gs b))
                 (and (== (aget gs a) (aget gs b)) (< a b))))))

  (siftUp [s from node]
    (loop [i from]
      (if (pos? i)
        (let [p (bit-shift-right (dec i) 1)
              above (aget heap p)]
          (if ^boolean (.before s node above)
            (do (aset heap i above)
                (aset heap-pos above i)
                (recur p))
            (do (aset heap i node)
                (aset heap-pos node i))))
        (do (aset heap i node)
            (aset heap-pos node i)))))

  (siftDown [s from node]
    (loop [i from]
      (let [left (inc (* 2 i))]
        (if (>= left heap-n)
          (do (aset heap i node)
              (aset heap-pos node i))
          (let [c (if (and (< (inc left) heap-n) ^boolean (.before s (aget heap (inc left)) (aget heap left))) (inc left) left)
                child (aget heap c)]
            (if ^boolean (.before s child node)
              (do (aset heap i child)
                  (aset heap-pos child i)
                  (recur c))
              (do (aset heap i node)
                  (aset heap-pos node i))))))))

  (popMin [s]
    (let [node (aget heap 0)]
      (set! heap-n (dec heap-n))
      (when (pos? heap-n) (.siftDown s 0 (aget heap heap-n)))
      (aset heap-pos node -2)
      node))

  ;; set the node's way in and put it in its place in the heap
  (relax [s node x z h move parent-node sec risk g slow-to corner shape]
    (aset hs node h)
    (aset moves node move)
    (aset slows node slow-to)
    (aset corners node corner)
    (aset shapes node shape)
    (aset parents node parent-node)
    (aset secs node sec)
    (aset risks node risk)
    (aset airs node move-air)
    (aset peaks node (js/Math.max (aget peaks parent-node) move-peak))
    (aset wsecs node (+ (aget wsecs parent-node) move-water))
    (aset opens node move-open)
    (aset gs node g)
    (aset fs node (+ g (* weight (.heuristic s x z))))
    (if (== (aget heap-pos node) -1)
      (do (set! heap-n (inc heap-n))
          (.siftUp s (dec heap-n) node))
      (.siftUp s (aget heap-pos node) node)))

  ;; a record of its own for a node: unless the node budget is spent
  (insertNode [s x y z h move parent-node sec risk g slow-to corner shape region key slot]
    (if (== n-nodes max-nodes)
      (set! over-budget true)
      (.relax s (.addNode s x y z region key slot) x z h move parent-node sec risk g slow-to corner shape)))

  ;; relax the edge to a node: insert it, or lower its cost if this way is cheaper.
  ;; a tight cell's node also has its region, the region's point and the crossing the move came in by: `shape`
  ;; A node reached again with a very different air use gets a record of its own: the hash points at the newest.
  (consider [s x y z h move parent-node dsec drisk slow-to corner shape]
    (let [region (bit-and shape 15)
          rx (+ (- x from-x) HALF)
          rz (+ (- z from-z) HALF)]
      (cond
        (or (neg? rx) (>= rx SPAN) (neg? rz) (>= rz SPAN)) nil
        (or (< x bx0) (> x bx1) (< z bz0) (> z bz1) (< y by0) (> y by1)) (set! boxed true)
        ;; a gap jump or a drop never lands on farmland: a landing after a fall of over 0.5 blocks tramples it (a farmland node
        ;; is the farmland's own cell; a jump up one block falls about 0.3 from the top of its arc, so it may land there)
        (and (or (== move MOVE-GAP) (== move MOVE-DROP)) (== (aget tbl-farmland (.stateAt snapshot x y z)) 1)) nil
        (and returnable (not replaying) ^boolean (.holdsBack s x y z h move parent-node dsec drisk slow-to corner shape)) nil
        ^boolean (.refusedKind s limit-kinds x y z move) (do (set! limit-refused true) nil)
        :else
        (let [extra (if avoiding (.avoidCost s x y z move dsec drisk) 0)]
          (when-not (neg? extra)
            (let [key (.keyOf s x y z region)
                  slot (.findSlot s key (bit-and (.hashOf s x y z region) (dec slots)))
                  found (aget hash-table slot)
                  sec (+ (aget secs parent-node) dsec)
                  risk (+ (aget risks parent-node) drisk)
                  ;; an alternative's search orders by its penalised cost; secs and risks stay the true cost of the walk
                  g (if avoiding (+ (aget gs parent-node) dsec (* risk-weight drisk) extra) (+ sec (* risk-weight risk)))]
              (if (== found -1)
                (.insertNode s x y z h move parent-node sec risk g slow-to corner shape region key slot)
                (let [d-air (- move-air (aget airs found))
                      g-found (aget gs found)]
                  (cond
                    (and (< g g-found) (<= d-air AIR-STEP))
                    (when-not (== (aget heap-pos found) -2)
                      (.relax s found x z h move parent-node sec risk g slow-to corner shape))

                    (or (< g g-found) (< d-air (- AIR-STEP)))
                    (.insertNode s x y z h move parent-node sec risk g slow-to corner shape region key slot)

                    :else nil)))))))))

  (stand16 [s node] (+ (* (aget ys node) 16) (aget hs node)))

  ;; The returnable search plans no step the body cannot undo. True when the move is not considered now: a gap jump down is
  ;; refused, a drop of more than JUMP-UP too, any other drop is held back until the probe (canReturn) says the body can climb back
  ;; (the probe runs the moves of another cell, so it cannot run inside an expansion: see flushHeld).
  (holdsBack [s x y z h move parent-node dsec drisk slow-to corner shape]
    (cond
      (== move MOVE-GAP) (< (+ (* y 16) h) (.stand16 s parent-node))
      (== move MOVE-DROP) (do (when-not (> (- (.stand16 s parent-node) (+ (* y 16) h)) JUMP-UP)
                                (.push held #js [x y z h move parent-node dsec drisk slow-to corner shape move-open move-air move-peak move-water]))
                              true)
      :else false))

  ;; is entering x,y,z by `move` of one of `kinds` (bits: climbing, water, opening something)?
  (refusedKind [s kinds x y z move]
    (cond
      (zero? kinds) false
      (and (not (zero? (bit-and kinds AVOID-CLIMB))) (>= move MOVE-CLIMB-UP) (<= move MOVE-OPEN)) true
      (and (not (zero? (bit-and kinds AVOID-WATER))) (or (>= move MOVE-SWIM) ^boolean (.isWater s x y z))) true
      (and (not (zero? (bit-and kinds AVOID-OPEN))) (or (== move MOVE-OPEN) (pos? move-open))) true
      :else false))

  ;; what entering x,y,z by `move` adds to its cost in a search for an alternative path: -1 refuses a move of a kind avoided;
  ;; a cell within 1 block of an earlier path costs avoid-factor times its own cost more
  (avoidCost [s x y z move dsec drisk]
    (cond
      ^boolean (.refusedKind s avoid-kinds x y z move) -1
      (true? (.has avoid-cells (cell-key x y z))) (* avoid-factor (+ dsec (* risk-weight drisk)))
      :else 0))

  ;; where moves go: into the search, or the goal flood's probe
  (sink [s x y z h move parent-node dsec drisk slow-to corner shape]
    (cond
      (some? capture-out) (.push capture-out x y z (bit-and shape 15) (+ dsec (* risk-weight drisk)))
      (some? flood-out) (.push flood-out (.keyOf s x y z (bit-and shape 15)) (- -1 (.keyOf s x y z 0)))
      flooding
      (when (and (== x fx) (== y fy) (== z fz) (or (neg? fr) (== fr (bit-and shape 15)))) (set! hit true))
      :else
      (.consider s x y z h move parent-node dsec drisk slow-to corner shape)))

  ;; the edge of a move: straight to the sink, or through one of the passes over an expansion near a door (see expandAt)
  (edge [s x y z h move parent-node dsec drisk slow-to corner shape]
    (cond
      (== edge-mode 0) (.sink s x y z h move parent-node dsec drisk slow-to corner shape)
      (== edge-mode 2) (do (.add seen-edges (.keyOf s x y z (bit-and shape 15)))
                           (.sink s x y z h move parent-node dsec drisk slow-to corner shape))
      :else (.openingEdge s x y z h move parent-node dsec drisk slow-to corner shape)))

  ;; ---- tight cells: where in the cell the body fits ----

  (partialInColumn [s x y z]
    (loop [cy (dec y)]
      (if (> cy (+ y 2))
        0
        (let [id (.stateAt s x cy z)]
          (if (and (not (== id UNLOADED)) (not (zero? (aget tbl-partial id))))
            1
            (recur (inc cy)))))))

  ;; partial collision in rows y-1..y+2 of the column, cached (the opening pass sees other blocks: its own cache entries)
  (columnPartial [s x y z]
    (let [key (* (inc (.keyOf s x y z 0)) (if open-mode -1 1))
          slot (bit-and (.hashOf s x y z 0) (dec TABLE))]
      (if (== (aget column-keys slot) key)
        (aget column-flags slot)
        (let [flag (.partialInColumn s x y z)]
          (aset column-keys slot key)
          (aset column-flags slot flag)
          flag))))

  (partialAround [s x y z]
    (loop [cz (dec z)
           cx (dec x)]
      (cond
        (> cz (inc z)) 0
        (> cx (inc x)) (recur (inc cz) (dec x))
        (== (.columnPartial s cx y cz) 1) 1
        :else (recur cz (inc cx)))))

  ;; no section the block of cells x +-r, z +-r, rows y-below..y+above touches holds a partial block or a climbable (or a
  ;; magma column, or something openable): the usual case, answered without reading cells
  (sectionsClear [s x y z r below above]
    (let [sy1 (bit-shift-right (- (+ y above) min-y) 4)
          sz0 (bit-shift-right (- z r) 4)
          sz1 (bit-shift-right (+ z r) 4)
          sx0 (bit-shift-right (- x r) 4)
          sx1 (bit-shift-right (+ x r) 4)]
      (loop [sy (bit-shift-right (- (- y below) min-y) 4)
             sz sz0
             sx sx0]
        (cond
          (> sy sy1) true
          (> sz sz1) (recur (inc sy) sz0 sx0)
          (> sx sx1) (recur sy (inc sz) sx0)
          ;; (a ^boolean hint on a call into JS would keep its name out of the inferred externs)
          (true? (.sectionHas snapshot tbl-special sx sy sz)) false
          :else (recur sy sz (inc sx))))))

  ;; a partial block within one cell of the body, rows y-1..y+2: the cell is one node per free region
  (isTight [s x y z]
    (if ^boolean (.sectionsClear s x y z 1 1 2)
      false
      (let [key (* (inc (.keyOf s x y z 0)) (if open-mode -1 1))
            slot (bit-and (.hashOf s x y z 0) (dec TABLE))]
        (if (== (aget cell-keys slot) key)
          (== (aget cell-flags slot) 1)
          (let [flag (.partialAround s x y z)]
            (aset cell-keys slot key)
            (aset cell-flags slot flag)
            (== flag 1))))))

  (tightAt [s x y z]
    (and (not quiet) ^boolean (.isTight s x y z)))

  (makeShape [s x y z lo16 key]
    (let [t (js/performance.now)
          lo (/ lo16 16)
          mask (.freeMask space (.boxesNear space view table x y z lo (+ lo BODY-BLOCKS)) x z)
          ^js labelled (.labelRegions space mask)
          labels (.-labels labelled)
          ^js regs (.-regs labelled)
          shape #js {:mask mask :labels labels :regs regs :centre (aget labels (+ (* 8 GRID) 8))}]
      (.set mask-cache key shape)
      (set! mask-ms (+ mask-ms (- (js/performance.now) t)))
      (set! masks (inc masks))
      (when ^boolean (.isTight s x y z)
        (set! tight-masks (inc tight-masks))
        (set! regions-seen (+ regions-seen (.-length regs)))
        (.add tight-seen (.keyOf s x y z 0)))
      shape))

  ;; free-position mask of the cell for a body standing at lo16 (absolute 1/16), its labelled regions, and the region of
  ;; the cell centre. One per (cell, height) per search.
  (shapeOf [s x y z lo16]
    (let [key (* (+ (* (.keyOf s x y z 0) 128) (+ (- lo16 (* y 16)) 32)) (if open-mode -1 1))
          cached (.get mask-cache key)]
      (if (undefined? cached)
        (.makeShape s x y z lo16 key)
        cached)))

  (nearestRegion [s labels p snap]
    (let [pi (js-mod p GRID)
          pj (/ (- p pi) GRID)
          i0 (js/Math.max 0 (- pi snap))
          i1 (js/Math.min (dec GRID) (+ pi snap))
          j1 (js/Math.min (dec GRID) (+ pj snap))]
      (loop [j (js/Math.max 0 (- pj snap))
             i i0
             best -1
             best-dist js/Infinity]
        (cond
          (> j j1) best
          (> i i1) (recur (inc j) i0 best best-dist)
          :else
          (let [label (aget labels (+ (* j GRID) i))
                d (+ (* (- i pi) (- i pi)) (* (- j pj) (- j pj)))]
            (if (and (>= label 0) (< d best-dist))
              (recur j (inc i) label d)
              (recur j (inc i) best best-dist)))))))

  ;; The region of a boundary point in a cell's own mask. Where the two cells' heights differ the point can be blocked only
  ;; by the step itself (the body crosses at the higher level, then settles), so a blocked point takes the region of the
  ;; nearest free position within snap/16: a whole cell (GRID) for a body leaving a climbable by a rise, or falling onto one.
  (regionNear [s ^js shape p snap]
    (let [labels (.-labels shape)
          label (aget labels p)]
      (if (>= label 0)
        label
        (.nearestRegion s labels p snap))))

  ;; index in the 17x17 mask of boundary point t (0..16 along the shared edge) for the cell moved from (A) and to (B),
  ;; for the cardinal c: 0 east, 1 west, 2 south (+z), 3 north
  (indexA [s c t]
    (cond
      (== c 0) (+ (* t GRID) 16)
      (== c 1) (* t GRID)
      (== c 2) (+ (* 16 GRID) t)
      :else t))

  (indexB [s c t]
    (cond
      (== c 0) (* t GRID)
      (== c 1) (+ (* t GRID) 16)
      (== c 2) t
      :else (+ (* 16 GRID) t)))

  ;; index in B's mask of boundary point t moved d/16 into B, away from the shared edge
  (insetB [s c t d]
    (cond
      (== c 0) (+ (* t GRID) d)
      (== c 1) (+ (* t GRID) (- 16 d))
      (== c 2) (+ (* d GRID) t)
      :else (+ (* (- 16 d) GRID) t)))

  ;; What a node knows beyond its cell, in one Uint32 (0 for an ordinary cell reached from an ordinary one): bits 0-3
  ;; region, bit 4 set for a tight cell with its representative point px (5-9) and pz (10-14) in 1/16, bit 15 set with the
  ;; crossing point the move in came by, relative to the cell in 1/16: x (16-20), z (21-25).
  (packShape [s region ^boolean tight px pz ^boolean crossed cross-x cross-z]
    (bit-or region
            (if tight (bit-or 16 (bit-shift-left px 5) (bit-shift-left pz 10)) 0)
            (if crossed (bit-or (bit-shift-left 1 15) (bit-shift-left cross-x 16) (bit-shift-left cross-z 21)) 0)))

  ;; does a mask of `masks` leave boundary point p blocked?
  (fallBlocked [s ^js masks p]
    (loop [k 0]
      (if (< k (.-length masks))
        (if (zero? (aget (aget masks k) p))
          true
          (recur (inc k)))
        false)))

  ;; a drop falls straight down the neighbour column: every tight cell between must be free at the crossing point too
  (fallMasks [s x2 y y2 z2]
    (let [out #js []]
      (loop [k (inc y2)]
        (when (< k y)
          (when ^boolean (.isTight s x2 k z2)
            (.push out (.-mask ^js (.shapeOf s x2 k z2 (* k 16)))))
          (recur (inc k))))
      out))

  ;; The walker goes in a straight line from a cell's representative point to the crossing point of the next move, and from
  ;; a crossing point to the representative point it leads to, so a move is only as good as those lines. A fence post
  ;; beside a gap leaves its cell a U-shaped region (a strip each side of the line, joined along the gap) with its point on
  ;; one strip: a crossing on the other strip lies behind the post, and the body walks head-on into it and sticks (live,
  ;; ProbePen: pressed on the post at x .065, :stuck). A ring of free space round a bamboo stalk is the same: in
  ;; prismarine-physics the body stuck on a stalk on 7 of 8 replayed courses until these lines were checked. Is every
  ;; mask point the segment (ai, aj) - (bi, bj) passes (one per 1/16 along its longer axis, rounded) free? An end that is
  ;; itself blocked (a boundary point snapped to the nearest region, at a step or a climbable) leaves the leg unjudged.
  (lineFree [s ^js mask ai aj bi bj]
    (let [di (- bi ai)
          dj (- bj aj)
          n (js/Math.max (js/Math.abs di) (js/Math.abs dj))]
      (if (or (zero? (aget mask (+ (* aj GRID) ai))) (zero? (aget mask (+ (* bj GRID) bi))))
        true
        (loop [k 1]
          (cond
            (>= k n) true
            (not (zero? (aget mask (+ (* (js/Math.round (+ aj (/ (* dj k) n))) GRID) (js/Math.round (+ ai (/ (* di k) n)))))))
            (recur (inc k))
            :else false)))))

  ;; pick[rb]: for each region of B a boundary point free for both cells leads to from region `label` of A, the point
  ;; nearest the line between the two representative points; -1 where none does. A drop (inset DROP-INSET, else 0)
  ;; falls from the point `inset` into B, where the body has cleared the ledge it walked off: it must pass there at the
  ;; joint height, and the fall and the landing are judged there. The legs either side of the crossing must be straight
  ;; and free in their cell (lineFree), from A's point to the crossing and from where it enters B to B's point.
  (crossings [s c label ^js rep-a ^boolean tight-b ^js own-a ^js own-b ^js joint-a ^js joint-b snap-a snap-b ^js falls inset]
    (let [mask-a (.-mask joint-a)
          mask-b (.-mask joint-b)]
      (.fill pick -1)
      (loop [t 0]
        (when (<= t 16)
          (let [pa (.indexA s c t)
                pb (.indexB s c t)
                pf (if (zero? inset) pb (.insetB s c t inset))]
            (when (and (not (zero? (aget mask-a pa))) (not (zero? (aget mask-b pb))) (not (zero? (aget mask-b pf)))
                       (== (.regionNear s own-a pa snap-a) label)
                       (not ^boolean (.fallBlocked s falls pf))
                       ^boolean (.lineFree s (.-mask own-a) (.-px rep-a) (.-pz rep-a) (js-mod pa GRID) (js/Math.floor (/ pa GRID))))
              (let [lb (.regionNear s own-b pf snap-b)
                    rb (if tight-b lb (if (== lb (.-centre own-b)) 0 -1))]
                (when (and (>= rb 0) (< rb REGIONS))
                  (let [^js rep-b (if tight-b (aget (.-regs own-b) rb) CENTRE)
                        along (if (< c 2) (/ (+ (.-pz rep-a) (.-pz rep-b)) 2) (/ (+ (.-px rep-a) (.-px rep-b)) 2))
                        picked (aget pick rb)]
                    (when (and ^boolean (.lineFree s (.-mask own-b) (js-mod pf GRID) (js/Math.floor (/ pf GRID))
                                                   (.-px rep-b) (.-pz rep-b))
                               (or (== picked -1) (< (js/Math.abs (- t along)) (js/Math.abs (- picked along)))))
                      (aset pick rb t)))))))
          (recur (inc t))))))

  ;; one edge per picked region of B
  (tightEdges [s i c x2 y2 z2 h1 move sec drisk slow-to ^boolean tight-b ^js own-b]
    (loop [rb 0]
      (when (< rb REGIONS)
        (let [t (aget pick rb)]
          (when (>= t 0)
            (let [^js rep (if tight-b (aget (.-regs own-b) rb) CENTRE)
                  ;; the crossing, relative to B: on its west edge (0) for an eastward move, its east edge (16) for a
                  ;; westward one...
                  cross-x (cond (== c 0) 0 (== c 1) 16 :else t)
                  cross-z (cond (== c 2) 0 (== c 3) 16 :else t)]
              (.edge s x2 y2 z2 h1 move i sec drisk slow-to 0
                     (.packShape s rb tight-b (.-px rep) (.-pz rep) true cross-x cross-z)))))
        (recur (inc rb)))))

  ;; the cardinal move c from cell A (region `region`, or every region when -1) to cell B, either of them tight: one edge
  ;; per region of B that a boundary point free for both leads to. Costs are those of the plain move, plus TIGHT_S into a
  ;; tight cell.
  (tightMove [s i x y z h region c x2 y2 z2 h1 move dsec drisk slow-to snap-a snap-b]
    (let [lo-a (+ (* y 16) h)
          lo-b (+ (* y2 16) h1)
          joint (js/Math.max lo-a lo-b) ; the body straddles the boundary at the higher of the two heights
          tight-a ^boolean (.isTight s x y z)
          tight-b ^boolean (.isTight s x2 y2 z2)
          ^js own-a (.shapeOf s x y z lo-a)
          ^js own-b (.shapeOf s x2 y2 z2 lo-b)
          ^js joint-a (if (== lo-a joint) own-a (.shapeOf s x (js/Math.max y y2) z joint))
          ^js joint-b (if (== lo-b joint) own-b (.shapeOf s x2 (js/Math.max y y2) z2 joint))
          ^js falls (if (== move MOVE-DROP) (.fallMasks s x2 y y2 z2) NO-MASKS)
          ^js regs-a (.-regs own-a)
          last-region (cond (>= region 0) region tight-a (dec (.-length regs-a)) :else 0)
          sec (+ dsec (if tight-b TIGHT-S 0))]
      (loop [ra (if (neg? region) 0 region)]
        (when (and (<= ra last-region) (< ra REGIONS))
          (let [label (if tight-a ra (.-centre own-a))]
            (when (>= label 0)
              (.crossings s c label (if tight-a (aget regs-a ra) CENTRE) tight-b own-a own-b joint-a joint-b snap-a snap-b falls
                          (if (== move MOVE-DROP) DROP-INSET 0))
              (.tightEdges s i c x2 y2 z2 h1 move sec drisk slow-to tight-b own-b)))
          (recur (inc ra))))))

  ;; the edges of a vertical move from region ra of A: one per region of B a position fitting at both ends and every cell
  ;; between joins, the position nearest the middle of the two regions' points being the crossing
  (verticalPick [s ra label-a tight-a tight-b ^js own-a ^js own-b ^js between]
    (let [^js rep-a (if ^boolean tight-a (aget (.-regs own-a) ra) CENTRE)
          mask-a (.-mask own-a)
          labels-a (.-labels own-a)
          mask-b (.-mask own-b)
          labels-b (.-labels own-b)
          centre-b (.-centre own-b)]
      (.fill best-d js/Infinity)
      (loop [p 0]
        (when (< p (* GRID GRID))
          (when (and (not (zero? (aget mask-a p))) (== (aget labels-a p) label-a) (not (zero? (aget mask-b p)))
                     (not ^boolean (.fallBlocked s between p)))
            (let [lb (aget labels-b p)
                  rb (if ^boolean tight-b lb (if (== lb centre-b) 0 -1))]
              (when (and (>= rb 0) (< rb REGIONS))
                (let [^js rep-b (if ^boolean tight-b (aget (.-regs own-b) rb) CENTRE)
                      pi (js-mod p GRID)
                      pj (/ (- p pi) GRID)
                      dx (- pi (/ (+ (.-px rep-a) (.-px rep-b)) 2))
                      dz (- pj (/ (+ (.-pz rep-a) (.-pz rep-b)) 2))
                      d (+ (* dx dx) (* dz dz))]
                  (when (< d (aget best-d rb))
                    (aset best-d rb d)
                    (aset best-at rb p))))))
          (recur (inc p))))))

  (verticalEdges [s i x y2 z h2 move dsec drisk slow-to tight-b ^js own-b]
    (loop [rb 0]
      (when (< rb REGIONS)
        (when-not (== (aget best-d rb) js/Infinity)
          (let [^js rep (if ^boolean tight-b (aget (.-regs own-b) rb) CENTRE)
                p (aget best-at rb)
                pi (js-mod p GRID)]
            (.edge s x y2 z h2 move i dsec drisk slow-to 0
                   (.packShape s rb tight-b (.-px rep) (.-pz rep) true pi (/ (- p pi) GRID)))))
        (recur (inc rb)))))

  ;; the cells between two of one column, as masks for a body standing at each cell's floor
  (betweenMasks [s x y y2 z]
    (let [out #js []]
      (loop [k (inc (js/Math.min y y2))]
        (when (< k (js/Math.max y y2))
          (.push out (.-mask ^js (.shapeOf s x k z (* k 16))))
          (recur (inc k))))
      out))

  ;; A vertical move in one column from cell y (stand height h, region `region` or every one when -1) to cell y2: climbing, a
  ;; jump into a ladder, a fall. The body stays at one position (x + i/16, z + j/16) all the way, so it must fit there at the
  ;; start, the end and every cell between.
  (verticalMove [s i x y z h region y2 h2 move dsec drisk slow-to]
    (let [tight-a ^boolean (.isTight s x y z)
          tight-b ^boolean (.isTight s x y2 z)]
      (if (and (not tight-a) (not tight-b))
        (.edge s x y2 z h2 move i dsec drisk slow-to 0 0)
        (let [^js own-a (.shapeOf s x y z (+ (* y 16) h))
              ^js own-b (.shapeOf s x y2 z (+ (* y2 16) h2))
              between (.betweenMasks s x y y2 z)
              last-region (cond (>= region 0) region tight-a (dec (.-length (.-regs own-a))) :else 0)]
          (loop [ra (if (neg? region) 0 region)]
            (when (and (<= ra last-region) (< ra REGIONS))
              (let [label-a (if tight-a ra (.-centre own-a))]
                (when (>= label-a 0)
                  (.verticalPick s ra label-a tight-a tight-b own-a own-b between)
                  (.verticalEdges s i x y2 z h2 move dsec drisk slow-to tight-b own-b)))
              (recur (inc ra))))))))

  ;; ---- water ----

  (isWater [s x y z]
    (let [id (.stateAt s x y z)]
      (and (not (== id UNLOADED)) (== (aget tbl-kind id) WATER))))

  ;; a body floating in the water cell (feet at its floor, h = 0): the cell over it is water or open and unhazardous
  (swimAt [s x y z]
    (let [id (.stateAt s x y z)]
      (if (or (== id UNLOADED) (not (== (aget tbl-kind id) WATER)))
        -1
        (let [head (.stateAt s x (inc y) z)]
          (cond
            (== head UNLOADED) -1
            (== (aget tbl-kind head) WATER) 0
            (and (== (aget tbl-kind head) OPEN) (zero? (aget tbl-top head)) (zero? (aget tbl-hazard head))) 0
            :else -1)))))

  ;; head in water that is not a bubble column: the breath runs
  (submerged [s x y z]
    (let [head (.stateAt s x (inc y) z)]
      (and (not (== head UNLOADED)) (== (aget tbl-kind head) WATER) (zero? (aget tbl-bubble head)))))

  ;; Dominance: a submerged sideways move in open water is never better than swimming at the surface over it. A cell's column
  ;; is open when its water reaches plain air; then surfaceY is the top water cell, else NONE. Cached per cell.
  (surfaceScan [s x y z]
    (loop [y2 (inc y)]
      (let [id (.stateAt s x y2 z)]
        (cond
          (== id UNLOADED) NONE
          (== (aget tbl-kind id) WATER) (recur (inc y2))
          (and (== (aget tbl-kind id) OPEN) (zero? (aget tbl-top id))) (dec y2)
          :else NONE))))

  (surfaceY [s x y z]
    (let [key (.keyOf s x y z 0)
          hit (.get surface-cache key)]
      (if (undefined? hit)
        (let [top2 (.surfaceScan s x y z)]
          (.set surface-cache key top2)
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
        (let [x2 (+ x (aget adx c))
              z2 (+ z (aget adz c))]
          (if (if ^boolean (.isWater s x2 y2 z2)
                (and (>= (.swimAt s x2 y2 z2) 0) (not ^boolean (.divesOpen s x2 y2 z2)))
                (>= (.standH s x2 y2 z2) 0))
            true
            (recur (inc c)))))))

  (diveScan [s x y z]
    (loop [y2 y]
      (if (not ^boolean (.isWater s x y2 z))
        false
        (if (or ^boolean (.reached s x y2 z) (not (zero? (aget tbl-bubble (.stateAt s x y2 z)))) ^boolean (.diveBeside s x y2 z))
          true
          (recur (dec y2))))))

  (worthDiving [s x y z]
    (let [key (.keyOf s x y z 0)
          hit (.get dive-cache key)]
      (if (undefined? hit)
        (let [found ^boolean (.diveScan s x y z)]
          (.set dive-cache key (if found 1 2))
          found)
        (== hit 1))))

  ;; swimming beside lava, or down onto magma
  (swimRisk [s x y z]
    (+ (if ^boolean (.lavaNear s x y z) LAVA-ADJACENT 0)
       (if (== (aget tbl-hazard (.stateAt s x (dec y) z)) DAMAGE-STAND) 1 0)))

  ;; a stand height at a feet cell: on land, or floating in water (h = 0); -1 for neither
  (nodeH [s x y z]
    (let [h (.standH s x y z)]
      (if (>= h 0) h (.swimAt s x y z))))

  ;; One swim or exit edge: the caller calls swimBegin, makes the edge (`base` + `extra` seconds) if it says true, then calls
  ;; swimEnd. `base` seconds of swimming count for the air, `extra` seconds of current do not. src-sub: the body starts the
  ;; move with its head in water; target-water: it ends in a water cell.
  (swimBegin [s i ^boolean target-water x2 y2 z2 base extra ^boolean src-sub]
    (let [target-sub (and target-water ^boolean (.submerged s x2 y2 z2))
          use (+ (if (>= i 0) (aget airs i) 0) base)]
      (if (and (or src-sub target-sub) (> use c-air-limit))
        (do (set! air-seen true)
            false)
        (do (set! move-air (if target-sub use 0))
            (set! move-peak (if (or src-sub target-sub) use 0))
            (set! move-water (+ base extra))
            true))))

  (swimEnd [s]
    (set! move-air 0)
    (set! move-peak 0)
    (set! move-water 0))

  (currentAt [s x y z]
    (if (== (aget tbl-flowing (.stateAt s x y z)) 1) c-current 0))

  ;; the water cell is 1 deep over a floor, with air over it: the body stands on the floor, it is not floating
  (standsInWater [s x y z]
    (if (not (zero? (aget tbl-bubble (.stateAt s x y z))))
      false
      (let [head (.stateAt s x (inc y) z)
            below (.stateAt s x (dec y) z)]
        (if (or (== head UNLOADED) (== below UNLOADED) (== (aget tbl-kind head) WATER))
          false
          (let [hz (aget tbl-hazard below)]
            (and (>= (aget tbl-floor below) WHOLE) (not (== (aget tbl-kind below) NARROW)) (not (== hz HAZARD-AVOID)) (not (== hz DAMAGE-TOUCH))))))))

  ;; out of 1-deep water by the walking rules: a step of up to STEP is a walk, up to JUMP_UP a jump (the head cell is open)
  (wadeOut [s i x y z region c x2 z2 ^boolean tight-src]
    (let [h0 (* y 16)
          h1 (.neighbour s x2 z2 y h0)]
      (when (>= h1 0)
        (let [y2 ty
              delta (- (+ (* y2 16) h1) h0)
              walks (<= delta STEP)]
          (when (or walks (and (<= delta JUMP-UP) ^boolean (.clear s x z (* (inc y) 16) (+ (* y2 16) h1 BODY))))
            (let [sec (+ (if walks WALK-S (+ WALK-S JUMP-S)) enter-extra)
                  move (if walks MOVE-WALK MOVE-JUMP)]
              (if (or tight-src ^boolean (.tightAt s x2 y2 z2))
                (.tightMove s i x y z 0 region c x2 y2 z2 h1 move sec enter-risk enter-slow SNAP SNAP)
                (.edge s x2 y2 z2 h1 move i sec enter-risk enter-slow 0 0))))))))

  ;; a lifting column (1) cannot be swum down, a dragging one (2) cannot be swum up
  (swimVertical [s x y z i region src-b ^boolean src-sub dy]
    (let [y2 (+ y dy)]
      (when (>= (.swimAt s x y2 z) 0)
        (let [tb (aget tbl-bubble (.stateAt s x y2 z))]
          (when-not (if (== dy 1) (or (== src-b 2) (== tb 2)) (or (== src-b 1) (== tb 1)))
            (when-not (and (== dy -1) ^boolean (.divesOpen s x y2 z) (not ^boolean (.worthDiving s x y2 z)))
              (let [lift (or (== src-b 1) (== tb 1))
                    drag (or (== src-b 2) (== tb 2))
                    move (if (== dy 1) MOVE-SWIM-UP MOVE-SWIM-DOWN)
                    base (if (== dy 1)
                           (if lift c-bubble-up c-swim-up)
                           (if drag c-bubble-down c-swim-down))
                    extra (.currentAt s x y2 z)
                    risk (.swimRisk s x y2 z)]
                (when ^boolean (.swimBegin s i true x y2 z base extra src-sub)
                  (.verticalMove s i x y z 0 region y2 0 move (+ base extra) risk 0)
                  (.swimEnd s)))))))))

  ;; a sideways swim move into the water cell beside (x, y, z)
  (swimSideways [s i x y z region c x2 z2 ^boolean tight-src ^boolean src-sub]
    (let [risk (.swimRisk s x2 y z2)
          tight (or tight-src ^boolean (.tightAt s x2 y z2))
          base c-swim-h
          extra (.currentAt s x2 y z2)]
      (when ^boolean (.swimBegin s i true x2 y z2 base extra src-sub)
        (if tight
          (.tightMove s i x y z 0 region c x2 y z2 0 MOVE-SWIM (+ base extra) risk 0 SNAP SNAP)
          (.edge s x2 y z2 0 MOVE-SWIM i (+ base extra) risk 0 0 0))
        (.swimEnd s))))

  ;; out of the water onto the bank cells beside it, from the same level up to last-ty
  (swimExit [s i x y z region c x2 z2 ^boolean tight-src ^boolean src-sub last-ty max-stand]
    (loop [y2 y]
      (when (<= y2 last-ty)
        (let [h1 (.landing s x2 y2 z2)]
          (when-not (or (neg? h1) (> (+ (* y2 16) h1) max-stand))
            (let [base (+ (if (== y2 y) c-swim-h c-exit) enter-extra)
                  risk enter-risk
                  slow-to enter-slow
                  tight (or tight-src ^boolean (.tightAt s x2 y2 z2))]
              (when ^boolean (.swimBegin s i false x2 y2 z2 base 0 src-sub)
                (if tight
                  (.tightMove s i x y z 0 region c x2 y2 z2 h1 MOVE-EXIT (+ base 0) risk slow-to SNAP SNAP)
                  (.edge s x2 y2 z2 h1 MOVE-EXIT i (+ base 0) risk slow-to 0 0))
                (.swimEnd s))))
          (recur (inc y2))))))

  ;; the diagonals of a swimming node, with the walking side rule: the body brushes both side cells
  (swimDiagonal [s i x y z c ^boolean src-sub src-surface]
    (let [dx (aget adx c)
          dz (aget adz c)
          x2 (+ x dx)
          z2 (+ z dz)]
      (when-not (or (< (.swimAt s x2 y z2) 0) ^boolean (.tightAt s x2 y z2) ^boolean (.refuses s x2 y z2 src-surface))
        (let [lo (* y 16)
              hi (+ lo BODY)
              sa (.side s (+ x dx) z lo hi)
              sb (.side s x (+ z dz) lo hi)]
          (when-not (or (== sa 2) (== sb 2) (and (== sa 1) (== sb 1)))
            (let [slide (+ sa sb)
                  risk (.swimRisk s x2 y z2)
                  base (+ (* c-swim-h SQRT2) (* slide CORNER-S))
                  extra (.currentAt s x2 y z2)]
              (when ^boolean (.swimBegin s i true x2 y z2 base extra src-sub)
                (.edge s x2 y z2 0 (if (zero? slide) MOVE-SWIM MOVE-CORNER) i (+ base extra) risk 0 slide 0)
                (.swimEnd s))))))))

  ;; a node floating in water: up, down, sideways and onto the bank; sideways moves and exits may cross tight cells (masks), a
  ;; diagonal never does
  (expandSwim [s x y z i region]
    (let [tight-src ^boolean (.tightAt s x y z)
          src-b (aget tbl-bubble (.stateAt s x y z))
          src-sub ^boolean (.submerged s x y z)
          src-surface (if src-sub (.surfaceY s x y z) y)]
      (.swimVertical s x y z i region src-b src-sub 1)
      (.swimVertical s x y z i region src-b src-sub -1)
      ;; out of the water onto a bank: at the same level, or up to one cell over the top water cell of this column. Live
      ;; (26.1): a floating body gets out onto land whose stand height is at most the water's top face + 1/16 (flush, or a
      ;; 15/16 top), never onto land one higher. A body standing on a floor in water 1 deep is not floating: it walks and
      ;; jumps out by the ordinary rules.
      (let [top-water ^boolean (.isWater s x (inc y) z)
            near-surface (or (not top-water) (not ^boolean (.isWater s x (+ y 2) z)))
            yt (if top-water (inc y) y)
            last-ty (if near-surface (js/Math.max y (inc yt)) y)
            max-stand (+ (* (inc yt) 16) EXIT-SLACK)
            wading ^boolean (.standsInWater s x y z)]
        (loop [c 0]
          (when (< c 4)
            (let [x2 (+ x (aget adx c))
                  z2 (+ z (aget adz c))]
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
            (recur (inc c)))))))

  ;; ---- moves ----

  ;; the standable cell beside the feet: level, one up, or a step down of at most STEP; else -1 (a bigger fall is a drop).
  ;; `ty` is the y of the cell found.
  (neighbour [s x2 z2 y h0]
    (set! ty y)
    (let [level (.landing s x2 y z2)]
      (if (>= level 0)
        level
        (do
          (set! ty (inc y))
          (let [up (.landing s x2 (inc y) z2)]
            (if (>= up 0)
              up
              (do
                (set! ty (dec y))
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
    (and after-exit (== (aget tbl-bubble (.stateAt snapshot x y z)) 2) (not (< goal-y (- y 1)))))

  (dropIntoWater [s i x y z h region c x2 y2 z2 h0 slow-from ^boolean tight-src]
    (let [fall (- h0 (* y2 16))]
      (when-not (or (< (.swimAt s x2 y2 z2) 0) (> fall (* c-max-water-drop 16)))
        (let [sec (+ (* WALK-S (+ 1 (* SLOW-EXTRA slow-from))) (* 0.25 (js/Math.sqrt (/ fall 16))))]
          (if (or tight-src ^boolean (.isTight s x2 y2 z2))
            (.tightMove s i x y z h region c x2 y2 z2 0 MOVE-DROP sec (.swimRisk s x2 y2 z2) 0 SNAP 0)
            (.edge s x2 y2 z2 0 MOVE-DROP i sec (.swimRisk s x2 y2 z2) 0 0 0))))))

  ;; walk off an edge into the first standable cell below the neighbour column, or into water of any depth up to maxWaterDrop
  ;; (the fall is cancelled there). A tight cell at either end: the body falls straight down from the crossing point, which
  ;; the masks must leave free all the way (a climbable below is grabbed as it falls past, so it takes any position).
  (expandDrop [s i x y z h region c x2 z2 h0 slow-from ^boolean tight-src]
    (loop [y2 (dec y)]
      (when (>= y2 (- y c-max-water-drop 1))
        (let [id (.stateAt s x2 y2 z2)]
          (when-not (or (== id UNLOADED) (== (aget tbl-kind id) LAVA) ^boolean (.avoids s id x2 y2 z2))
            (if (== (aget tbl-kind id) WATER)
              (when-not ^boolean (.magmaTrap s x2 y2 z2)
                (.dropIntoWater s i x y z h region c x2 y2 z2 h0 slow-from tight-src))
              (let [h1 (.landing s x2 y2 z2)]
                (if (neg? h1)
                  (when-not (pos? (aget tbl-top id)) (recur (dec y2)))
                  (let [tight-drop (or tight-src ^boolean (.isTight s x2 y2 z2))
                        fall (- h0 (+ (* y2 16) h1))]
                    (when-not (> fall (* max-drop 16))
                      (let [sec (+ (* WALK-S (+ 1 (* SLOW-EXTRA (+ slow-from enter-slow))))
                                   (* 0.25 (js/Math.sqrt (/ (js/Math.max 0 fall) 16)))
                                   enter-extra)]
                        (if tight-drop
                          (.tightMove s i x y z h region c x2 y2 z2 h1 MOVE-DROP sec (+ enter-risk (fall-damage fall)) enter-slow
                                      SNAP (if ^boolean (.climbHere s x2 y2 z2) GRID 0))
                          (.edge s x2 y2 z2 h1 MOVE-DROP i sec (+ enter-risk (fall-damage fall)) enter-slow 0 0)))))))))))))

  ;; where a gap jump lands: level, else one up (when the higher arc is clear), else one down; `gap-y` is the cell's y
  (gapLanding [s lx y lz ^boolean up]
    (set! gap-y y)
    (let [h1 (.landing s lx y lz)]
      (cond
        (>= h1 0) h1
        up (.gapLandingUp s lx y lz)
        :else (.gapLandingDown s lx y lz))))

  (gapLandingUp [s lx y lz]
    (set! gap-y (inc y))
    (let [h1 (.landing s lx (inc y) lz)]
      (if (>= h1 0)
        h1
        (.gapLandingDown s lx y lz))))

  (gapLandingDown [s lx y lz]
    (set! gap-y (dec y))
    (.landing s lx (dec y) lz))

  ;; sprint across 1..3 empty cells in a cardinal line, landing level or one lower; across 1 or 2, also up to one block
  ;; higher. A jump over 3 above a pit (pitBelow) has GAP-PIT-RISK: one that falls short traps the body, so a short way round
  ;; is taken instead
  (expandGap [s i x y z c h0]
    (let [dx (aget adx c)
          dz (aget adz c)]
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
                      ly gap-y]
                  (when-not (or (neg? h1) ^boolean (.isTight s lx ly lz))
                    (let [delta (- (+ (* ly 16) h1) h0)]
                      (when-not (or (< delta -16) (> delta (if up WHOLE 0))
                                    (and (some? limit-gap)
                                         ;; (the goal flood expands cells nothing reached: i is -1, the takeoff a plain walk)
                                         (not (true? (limit-gap x y z (- h0 (* y 16)) (if (neg? i) MOVE-WALK (aget moves i)) lx ly lz h1)))
                                         (set! limit-refused true)))
                        (.edge s lx ly lz h1 MOVE-GAP i
                               (+ (* (inc n) SPRINT-S) GAP-S (if (pos? delta) GAP-UP-S 0) enter-extra)
                               (+ enter-risk risk) enter-slow 0 0))))
                  (recur (inc n) hole pit up-arc)))))))))

  (expandCardinal [s x y z h slow-from i region c h0 ^boolean tight-src ^boolean climbing]
    (let [x2 (+ x (aget adx c))
          z2 (+ z (aget adz c))
          h1 (.neighbour s x2 z2 y h0)]
      (if (>= h1 0)
        (let [y2 ty
              delta (- (+ (* y2 16) h1) h0)
              walk (* WALK-S (+ 1 (* SLOW-EXTRA (+ slow-from enter-slow))))
              ;; climbing a stairs block in its direction is a walk, though the node above it is a whole block up
              climbs (and (== (aget tbl-stair support) (inc c)) (<= delta WHOLE))
              ;; (the body steps off a climbable without a jump: it is already rising)
              walks (or (<= delta STEP) climbs (and climbing (<= delta JUMP-UP)))]
          ;; a jump needs headroom over the start column; a tight start's mask checks that itself
          (when (and (or walks (and (<= delta JUMP-UP) (or tight-src ^boolean (.clear s x z h0 (+ (* y2 16) h1 BODY)))))
                     ;; a walk up lifts the body into the slab above its old top: that must be free in the start column
                     (or (not walks) (<= delta 0) tight-src climbing ^boolean (.clear s x z (+ h0 BODY) (+ (* y2 16) h1 BODY))))
            (let [sec (+ (if walks walk (+ walk JUMP-S)) enter-extra)
                  move (if walks MOVE-WALK MOVE-JUMP)]
              (if (or tight-src ^boolean (.tightAt s x2 y2 z2))
                (.tightMove s i x y z h region c x2 y2 z2 h1 move sec enter-risk enter-slow
                            (if (and climbing (> delta STEP)) GRID SNAP) SNAP)
                (.edge s x2 y2 z2 h1 move i sec enter-risk enter-slow 0 0)))))
        (if (>= (.swimAt s x2 y z2) 0)
          ;; water ahead at our level: walk in and swim
          (when-not ^boolean (.magmaTrap s x2 y z2)
            (let [risk (.swimRisk s x2 y z2)
                  tight (or tight-src ^boolean (.tightAt s x2 y z2))
                  base c-swim-h
                  extra (if (== (aget tbl-flowing (.stateAt snapshot x2 y z2)) 1) c-current 0)]
              (when ^boolean (.swimBegin s i true x2 y z2 base extra false)
                (if tight
                  (.tightMove s i x y z h region c x2 y z2 0 MOVE-SWIM (+ base extra) risk 0 SNAP SNAP)
                  (.edge s x2 y z2 0 MOVE-SWIM i (+ base extra) risk 0 0 0))
                (.swimEnd s))))
          ;; no ground ahead at our level: the body must at least fit in the column to leave the edge
          (when ^boolean (.clear s x2 z2 h0 (+ h0 BODY))
            (.expandDrop s i x y z h region c x2 z2 h0 slow-from tight-src)
            ;; no gap jumps out of a tight cell, none over a door (in the opening pass)
            (when (and (not tight-src) (not open-mode))
              (.expandGap s i x y z c h0)))))))

  (expandDiagonal [s x y z slow-from i c h0]
    (let [dx (aget adx c)
          dz (aget adz c)
          x2 (+ x dx)
          z2 (+ z dz)
          h1 (.neighbour s x2 z2 y h0)
          y2 ty]
      (when (and (>= h1 0) (not ^boolean (.tightAt s x2 y2 z2)))
        (let [h2 (+ (* y2 16) h1)
              jump (> (- h2 h0) STEP)]
          (when (<= (- h2 h0) JUMP-UP)
            ;; the 0.62 wide body brushes both side cells near their shared corner, for its whole height: neither may hold
            ;; anything it must not touch (water, or a hole, is fine to pass). One side holding plain collision is a
            ;; corner slide: the body presses on it and slides over the other side cell (dipping there if it has no
            ;; floor, the step height lifts it out)
            (let [lo (js/Math.min h0 h2)
                  hi (+ (js/Math.max h0 h2) BODY)
                  sa (.side s x2 z lo hi)
                  sb (.side s x z2 lo hi)
                  slide (+ sa sb)] ; 1 when exactly one side is blocked
              (when (and (< slide 2)
                         ;; a step up lifts the body: its start column must have the room
                         (or (<= h2 h0) ^boolean (.clear s x z (if jump h0 (+ h0 BODY)) hi))
                         ;; (the walker's test of a jump that slides along a corner: limit-corner)
                         (or (not jump) (zero? slide) (nil? limit-corner)
                             (true? (limit-corner x y z (- h0 (* y 16)) x2 y2 z2 h1))
                             (do (set! limit-refused true) false)))
                (let [walk (+ (* WALK-S SQRT2 (+ 1 (* SLOW-EXTRA (+ slow-from enter-slow)))) (* slide CORNER-S))
                      brushed (+ (.sideTouch s x2 z lo hi) (.sideTouch s x z2 lo hi))]
                  (.edge s x2 y2 z2 h1
                         (cond jump MOVE-JUMP (== slide 1) MOVE-CORNER :else MOVE-DIAGONAL)
                         i (+ (if jump (+ walk JUMP-S) walk) enter-extra) (+ enter-risk brushed) enter-slow slide 0)))))))))

  ;; region -1: every region of a tight cell (the goal flood does not know which one it comes from)
  (expandMoves [s x y z h slow-from i region]
    (if (== (aget tbl-kind (.stateAt snapshot x y z)) WATER)
      (.expandSwim s x y z i region)
      (let [h0 (+ (* y 16) h)
            tight-src ^boolean (.tightAt s x y z)
            ;; (the same quiet test as for tight cells: no climbable within reach of the cell either)
            climbing (and (not quiet) ^boolean (.climbHere s x y z))]
        (set! after-exit (and (>= i 0) (== (aget moves i) MOVE-EXIT)))
        (if climbing
          (do (.climbUp s i x y z h region)
              (.climbDown s i x y z h region))
          (when-not quiet
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

  ;; ---- doors, gates and trapdoors ----

  (nearOpenable [s x y z]
    (loop [cz (dec z)
           cx (dec x)
           cy (dec y)]
      (cond
        (> cz (inc z)) false
        (> cx (inc x)) (recur (inc cz) (dec x) (dec y))
        (> cy (+ y 2)) (recur cz (inc cx) (dec y))
        (pos? (aget tbl-openable (.stateAt snapshot cx cy cz))) true
        :else (recur cz cx (inc cy)))))

  ;; the closed openable blocks in the body column of a node at (x, y, z) standing h/16 up, a door by its lower half:
  ;; [{ x, y, z, id }]
  (closedIn [s x y z h]
    (let [out #js []
          last-k (bit-shift-right (dec (+ (* y 16) h BODY)) 4)]
      (loop [k y]
        (when (<= k last-k)
          (let [id (.stateAt snapshot x k z)]
            (when (pos? (aget tbl-openable id))
              (let [lower (== (aget tbl-door-half id) 2)
                    by (if lower (dec k) k)]
                (when-not (true? (.some out (fn [^js b] (== (.-y b) by))))
                  (.push out #js {:x x :y by :z z :id (if lower (.stateAt snapshot x by z) id)})))))
          (recur (inc k))))
      out))

  ;; an activator for an iron door at `door`, for a body in the cell (sx, sy, sz) in front of it: a plate in that cell, or a
  ;; button or lever on the body's side of the door within 4 blocks, on a block next to the door's frame. nil when none.
  (findActivator [s ^js door sx sy sz side]
    (if (and (== (aget tbl-activator (.stateAt snapshot sx sy sz)) ACT-PLATE)
             (== (+ (js/Math.abs (- (.-x door) sx)) (js/Math.abs (- (.-z door) sz))) 1))
      #js {:via "plate" :at #js {:x sx :y sy :z sz}}
      (let [along-x (<= (aget tbl-facing (.-id door)) 2)]
        (loop [dz -4
               dy -2
               dx -4
               best nil
               best-dist js/Infinity]
          (cond
            (> dz 4) best
            (> dy 4) (recur (inc dz) -2 -4 best best-dist)
            (> dx 4) (recur dz (inc dy) -4 best best-dist)
            :else
            (let [d (+ (* dx dx) (* dy dy) (* dz dz))]
              (if (or (> d 16) (>= d best-dist))
                (recur dz dy (inc dx) best best-dist)
                (let [x (+ sx dx)
                      y (+ sy dy)
                      z (+ sz dz)
                      id (.stateAt snapshot x y z)
                      act (aget tbl-activator id)]
                  (if (and (or (== act ACT-BUTTON) (== act ACT-LEVER))
                           (== (js/Math.sign (if along-x (- x (.-x door)) (- z (.-z door)))) side))
                    (let [^js att (aget ATTACH (aget tbl-attach id))]
                      (if (> (js/Math.max (js/Math.abs (- (+ x (aget att 0)) (.-x door)))
                                          (js/Math.abs (- (+ y (aget att 1)) (.-y door)))
                                          (js/Math.abs (- (+ z (aget att 2)) (.-z door)))) 2)
                        (recur dz dy (inc dx) best best-dist)
                        (recur dz dy (inc dx)
                               #js {:via (if (== act ACT-BUTTON) "button" "lever") :at #js {:x x :y y :z z}}
                               d)))
                    (recur dz dy (inc dx) best best-dist))))))))))

  (activatorFor [s ^js door sx sy sz side]
    (let [key (str (.-x door) "," (.-y door) "," (.-z door) "|" sx "," sy "," sz "|" side)
          hit (.get activators key)]
      (if (undefined? hit)
        (let [found (.findActivator s door sx sy sz side)]
          (.set activators key found)
          found)
        hit)))

  ;; what a move from the cell (sx, sy, sz) into a node column holding the closed blocks `blocks` opens: { list, seconds },
  ;; or nil when an iron door among them has nothing to open it
  (opening [s sx sy sz dx dz ^js blocks]
    (let [list #js []]
      (loop [k 0
             seconds 0]
        (if (>= k (.-length blocks))
          #js {:list list :seconds seconds}
          (let [^js b (aget blocks k)
                bid (.-id b)
                iron (== (aget tbl-openable bid) OPEN-REDSTONE)
                along-x (<= (aget tbl-facing bid) 2)
                ;; the body's side of the door: where it is, or, standing in the door's cell, where it came from
                near-side (js/Math.sign (if along-x (- sx (.-x b)) (- sz (.-z b))))
                side (if (zero? near-side) (- (js/Math.sign (if along-x (- dx (.-x b)) (- dz (.-z b))))) near-side)
                found (if (or iron (== (aget tbl-activator (.stateAt snapshot sx sy sz)) ACT-PLATE))
                        (.activatorFor s b sx sy sz side)
                        nil)
                ^js act (if (or iron (and (some? found) (identical? (.-via ^js found) "plate"))) found nil)]
            (cond
              (and iron (nil? act)) nil
              (nil? act)
              (do (.push list #js {:x (.-x b) :y (.-y b) :z (.-z b)})
                  (recur (inc k) (+ seconds c-open)))
              :else
              (do (.push list #js {:x (.-x b) :y (.-y b) :z (.-z b) :via (.-via act) :at (.-at act)})
                  (recur (inc k) (+ seconds (if (identical? (.-via act) "plate") c-open-plate c-open-redstone))))))))))

  ;; the opening pass's edge: only what the first pass did not find, and that needs something opened
  (openingEdge [s x y z h move parent-node dsec drisk slow-to corner shape]
    (when-not (true? (.has seen-edges (.keyOf s x y z (bit-and shape 15))))
      (let [there (not-in (.closedIn s x y z h) door-here)
            blocks (not-in (.concat door-here there) door-arrival)]
        (when-not (and (zero? (.-length blocks)) (not door-through))
          (let [^js opened (.opening s open-x open-y open-z x z blocks)]
            (when (some? opened)
              (set! move-open (if (pos? (.-length (.-list opened))) (.push open-lists (.-list opened)) 0))
              (.sink s x y z h move parent-node (+ dsec (.-seconds opened)) drisk slow-to corner shape)
              (set! move-open 0)))))))

  ;; Every expansion near a closed door, gate or trapdoor runs twice: once with the world as it stands, then in the opening
  ;; pass with those blocks open. An edge only the second pass finds is a move that needs something opened: it costs
  ;; costs.open per block opened by hand and records step.opens.
  (expandOpening [s x y z h slow-from i region]
    (set! open-x x)
    (set! open-y y)
    (set! open-z z)
    (set! seen-edges (js/Set.))
    (set! edge-mode 2)
    (.expandMoves s x y z h slow-from i region)
    ;; what the move into the node here opened is open still; a body standing in a gate's cell without having opened it is not
    (set! door-arrival (if (and (>= i 0) (pos? (aget opens i))) (aget open-lists (dec (aget opens i))) #js []))
    (set! door-here (.closedIn s x y z h))
    (set! door-through (true? (.some door-here (fn [b] (in-list? b door-arrival)))))
    (set! edge-mode 3)
    (set! open-mode true)
    (.expandMoves s x y z h slow-from i region)
    (set! open-mode false)
    (set! edge-mode 0))

  (expandAt [s x y z h slow-from i region]
    ;; when no section near the cell holds a partial block or a climbable, no cell this expansion looks at is tight
    (set! quiet (.sectionsClear s x y z 2 2 3))
    (if (or quiet (not ^boolean (.nearOpenable s x y z)))
      (.expandMoves s x y z h slow-from i region)
      (.expandOpening s x y z h slow-from i region)))

  ;; ---- climbing ----

  ;; the cell (x, y2, z) as the next node of a vertical move: standH with a shut trapdoor allowed (its cost added), or -1
  (enterCell [s x y2 z]
    (let [was allow-shut]
      (set! allow-shut true)
      (set! enters-shut ^boolean (.shutAt s x y2 z))
      (let [h2 (.landing s x y2 z)]
        (set! allow-shut was)
        h2)))

  (freeCell [s id]
    (and (not (== id UNLOADED)) (zero? (aget tbl-top id)) (not (== (aget tbl-kind id) WATER)) (not (== (aget tbl-kind id) LAVA))))

  ;; a climbable one block up (or the deck of scaffolding, or a trapdoor to open); a gap above refuses the climb
  (climbUp [s i x y z h region]
    (let [h2 (.enterCell s x (inc y) z)]
      (if (neg? h2)
        (loop [k 2]
          (when (and (<= k 3) (not gap-seen))
            (set! gap-seen (and ^boolean (.climbHere s x (+ y k) z) ^boolean (.freeCell s (.stateAt s x (inc y) z))
                                (or (== k 2) ^boolean (.freeCell s (.stateAt s x (+ y 2) z)))))
            (recur (inc k))))
        (let [sec (+ c-climb-up (if enters-shut c-open 0))]
          (when enters-shut (set! move-open (.push open-lists #js [#js {:x x :y (inc y) :z z}])))
          (.verticalMove s i x y z h region (inc y) h2 (if enters-shut MOVE-OPEN MOVE-CLIMB-UP) sec enter-risk enter-slow)
          (set! move-open 0)))))

  ;; a climbable or standable cell one block down; else, through free cells, a short fall onto the first climbable or floor
  (climbDown [s i x y z h region]
    (let [h2 (.enterCell s x (dec y) z)]
      (if (>= h2 0)
        (let [sec (+ c-climb-down (if enters-shut c-open 0))]
          (when enters-shut (set! move-open (.push open-lists #js [#js {:x x :y (dec y) :z z}])))
          (.verticalMove s i x y z h region (dec y) h2 (if enters-shut MOVE-OPEN MOVE-CLIMB-DOWN) sec enter-risk enter-slow)
          (set! move-open 0))
        (let [free (.stateAt s x (dec y) z)]
          (when-not (or (== free UNLOADED) (pos? (aget tbl-top free)) (== (aget tbl-kind free) WATER) (== (aget tbl-kind free) LAVA)
                        ^boolean (.avoids s free x (dec y) z))
            (.fallThrough s i x y z h region (+ (* y 16) h)))))))

  (fallThrough [s i x y z h region from16]
    (loop [y3 (- y 2)]
      (when (>= y3 (- y max-drop 1))
        (let [id (.stateAt s x y3 z)]
          (when-not (or (== id UNLOADED) (== (aget tbl-kind id) LAVA) (== (aget tbl-kind id) WATER) ^boolean (.avoids s id x y3 z))
            (let [h3 (.landing s x y3 z)]
              (if (neg? h3)
                (when-not (pos? (aget tbl-top id)) (recur (dec y3)))
                (let [fall (- from16 (+ (* y3 16) h3))]
                  (when-not (> fall (* max-drop 16))
                    (.verticalMove s i x y z h region y3 h3 MOVE-DROP (* 0.25 (js/Math.sqrt (/ fall 16)))
                                   (+ enter-risk (fall-damage fall)) enter-slow))))))))))

  ;; from the floor, a jump puts the feet into a climbable one block up
  (jumpClimb [s i x y z h region]
    (let [id (.stateAt snapshot x (inc y) z)]
      (when-not (or (== id UNLOADED) (zero? (aget tbl-climb id)) (== (aget tbl-climb id) CLIMB-TRAP-SHUT))
        (let [h2 (.landing s x (inc y) z)]
          (when-not (neg? h2)
            (.verticalMove s i x y z h region (inc y) h2 MOVE-JUMP-CLIMB c-jump-climb enter-risk enter-slow))))))

  ;; ---- the goal flood ----

  (inSpan [s x z]
    (let [rx (+ (- x from-x) HALF)
          rz (+ (- z from-z) HALF)]
      (and (>= rx 0) (< rx SPAN) (>= rz 0) (< rz SPAN))))

  (goalNotStandable [s]
    (if (or (not near) goal-unloaded)
      false
      (let [r (js/Math.ceil goal-range)]
        (loop [dx (- r)
               dy (- r)
               dz (- r)]
          (cond
            (> dx r) true
            (> dy r) (recur (inc dx) (- r) (- r))
            (> dz r) (recur dx (inc dy) (- r))
            (and (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* goal-range goal-range))
                 (>= (.nodeH s (+ goal-x dx) (+ goal-y dy) (+ goal-z dz)) 0)) false
            :else (recur dx dy (inc dz)))))))

  ;; the standable cells of the goal start the flood
  (floodSeeds [s ^js seen ^js queue]
    (let [r (js/Math.ceil goal-range)]
      (loop [dx (- r)
             dy (- r)
             dz (- r)]
        (cond
          (> dx r) nil
          (> dy r) (recur (inc dx) (- r) (- r))
          (> dz r) (recur dx (inc dy) (- r))
          :else
          (let [x (+ goal-x dx)
                y (+ goal-y dy)
                z (+ goal-z dz)]
            (when (and (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* goal-range goal-range))
                       ^boolean (.inSpan s x z)
                       (>= (.nodeH s x y z) 0))
              (dotimes [r (.floodRegions s x y z (.nodeH s x y z))]
                (.add seen (.keyOf s x y z r))
                (.push queue x y z r))
              (when ^boolean (.isWater s x y z) (set! leaked true)))
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
        (and (== (.stateAt snapshot x y z) UNLOADED)
             (false? (.hasColumn snapshot (bit-shift-right x 4) (bit-shift-right z 4))))))

  ;; the cell's stand height as the search can reach it: as it stands, or with a closed door, gate or trapdoor in its column
  ;; read as open (the opening pass makes nodes the body only fits in once something is opened)
  (floodH [s x y z]
    (when (nil? flood-h-keys)
      (set! flood-h-keys (js/Float64Array. FLOOD-TABLE))
      (set! flood-h-vals (js/Int16Array. FLOOD-TABLE)))
    (let [key (inc (.keyOf s x y z 0))
          slot (bit-and (.hashOf s x y z 0) (dec FLOOD-TABLE))]
      (if (== (aget flood-h-keys slot) key)
        (aget flood-h-vals slot)
        (let [h (.floodHeight s x y z)]
          (aset flood-h-keys slot key)
          (aset flood-h-vals slot h)
          h))))

  (floodHeight [s x y z]
    (let [h (.nodeH s x y z)]
      (if (or (>= h 0)
              (not (or (pos? (aget tbl-openable (.stateAt snapshot x (dec y) z)))
                       (pos? (aget tbl-openable (.stateAt snapshot x y z)))
                       (pos? (aget tbl-openable (.stateAt snapshot x (inc y) z))))))
        h
        (do (set! open-mode true)
            (let [opened (.nodeH s x y z)]
              (set! open-mode false)
              opened)))))

  ;; can a gap jump in direction c from a takeoff dy above the flood's current cell come over the column beside it (free at
  ;; body height over the takeoff)?
  (jumpOver [s c dy]
    (let [lo (* (+ fy dy) 16)]
      (.clear s (+ fx (aget adx c)) (+ fz (aget adz c)) lo (+ lo BODY))))

  ;; adds the cell when it is standable and a forward move of it reaches the flood's current cell; true when it is the
  ;; start. A cell the flood cannot see is a way in it does not know, and marks the flood leaked: always beside the current
  ;; cell (jump -1), and from a gap jump's takeoff in direction jump only when the jump can come over the column between.
  (floodVisit [s ^js seen ^js queue start-key x y z jump]
    (if ^boolean (.unseen s x y z)
      (do (when (or (neg? jump) ^boolean (.jumpOver s jump (- y fy))) (set! leaked true))
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
          known (.get flood-moves memo-key)]
      (if (some? known)
        known
        (let [out #js []]
          (set! flood-out out)
          (.expandAt s x y z h 0 -1 r)
          (set! flood-out nil)
          (when (>= (.-size flood-moves) FLOOD-MEMO) (.clear flood-moves)) ; bounded memory; a flood's front is what asks again
          (.set flood-moves memo-key out)
          out))))

  ;; adds region r of the cell (-1: every region, as one node) when a forward move out of it reaches the flood's current
  ;; node; true when it is the start
  (floodNode [s ^js seen ^js queue start-key x y z h r]
    (let [key (.keyOf s x y z (js/Math.max r 0))]
      (if (true? (.has seen key))
        false
        (do
          (set! hit (.includes (.floodMovesOf s x y z h r key) flood-target))
          (if hit
            (do (.add seen key)
                (.push queue x y z r)
                (when ^boolean (.isWater s x y z) (set! leaked true))
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
        ^boolean (.floodVisit s seen queue start-key (+ fx (* (aget adx c) n)) (+ fy dy) (+ fz (* (aget adz c) n)) c) true
        :else (recur c n (inc dy)))))

  ;; the predecessors of the flood's current cell; true when the start is among them
  (floodAround [s seen queue start-key]
    (loop [c 0
           dy -1]
      (cond
        (== c 8) (.floodAhead s seen queue start-key)
        (> dy (inc max-drop)) (recur (inc c) -1)
        ^boolean (.floodVisit s seen queue start-key (+ fx (aget adx c)) (+ fy dy) (+ fz (aget adz c)) -1) true
        :else (recur c (inc dy)))))

  ;; climbs and falls in the column
  (floodColumn [s seen queue start-key]
    (loop [dy -1]
      (cond
        (> dy (inc max-drop)) false
        (zero? dy) (recur (inc dy))
        ^boolean (.floodVisit s seen queue start-key fx (+ fy dy) fz -1) true
        :else (recur (inc dy)))))

  (floodRun [s ^js seen ^js queue start-key budget]
    (loop [head 0]
      (if (and (< head (.-length queue)) (<= (.-size seen) budget))
        (do
          (set! fx (aget queue head))
          (set! fy (aget queue (+ head 1)))
          (set! fz (aget queue (+ head 2)))
          (set! fr (aget queue (+ head 3)))
          (set! flood-target (if (neg? fr) (- -1 (.keyOf s fx fy fz 0)) (.keyOf s fx fy fz fr)))
          (if (or ^boolean (.floodColumn s seen queue start-key) ^boolean (.floodAround s seen queue start-key))
            true
            (recur (+ head 4))))
        false)))

  ;; Backward flood from the standable goal cells over predecessors: nodes n (a cell and, in a tight cell, a region; the
  ;; queue holds x y z region) with a forward move n -> c, found by running n's own moves (so it can never disagree with
  ;; the search). True when it exhausts within budget nodes without
  ;; meeting the start: then nothing reaches the goal. Slow, but bounded by the budget; false on budget or when the start
  ;; is met, or when it leaks: it meets water (a drop into water starts further up than the flood looks), or an unloaded
  ;; or out-of-span cell (what lies there is unknown).
  (goalEnclosed [s budget sealed]
    (let [start-key (aget node-keys 0) ; the start node's key, its region in a tight cell (begin)
          seen (js/Set.)
          queue #js []]
      (set! leaked false)
      (.floodSeeds s seen queue)
      (if (true? (.has seen start-key))
        false
        (do
          (set! flooding true)
          (set! allow-shut true) ; a shut trapdoor is a way through, only dearer: the flood must not call its far side enclosed
          (let [open ^boolean (.floodRun s seen queue start-key budget)
                enclosed (and (not open) (not leaked) (<= (.-size seen) budget))
                leaks (and enclosed ^boolean sealed ^boolean (.floodLeaks s queue))]
            (set! flooding false)
            (set! allow-shut false)
            (set! fr -1)
            (set! flooded (.-size seen))
            (and enclosed (not leaks)))))))

  ;; a body-high free column beside the cell with nothing to stand on within a drop below it: a cliff or a gap, an edge the body
  ;; can walk off to nowhere (a wall top above the floor is no cliff: its drop lands)
  (cliffBeside [s x y z h]
    (loop [c 0]
      (if (== c 4)
        false
        (let [x2 (+ x (aget adx c))
              z2 (+ z (aget adz c))]
          (if (and ^boolean (.clear s x2 z2 (+ (* y 16) h) (+ (* y 16) h BODY))
                   (loop [dy (- (inc max-drop))]
                     (cond
                       (> dy 1) true
                       (>= (.nodeH s x2 (+ y dy) z2) 0) false
                       :else (recur (inc dy)))))
            true
            (recur (inc c)))))))

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
    (let [budget (js/Math.min pre-flood goal-flood)]
      (if (or (not near) (<= budget 0) goal-unloaded)
        false
        (let [enclosed ^boolean (.goalEnclosed s budget true)]
          (set! pre-flooded flooded)
          (set! flooded 0)
          enclosed))))

  ;; ---- the search ----

  (finish [s why]
    (set! finished true)
    (set! reason why)
    (set! elapsed (- (js/performance.now) t0)))

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
    (if-not ^boolean (.isTight s from-x from-y from-z)
      0
      (let [^js shape (.shapeOf s from-x from-y from-z (+ (* from-y 16) start-h))
            labels (.-labels shape)
            best (.nearestFree s (.-mask shape) (* (- from-px from-x) 16) (* (- from-pz from-z) 16))]
        (if (or (neg? best) (>= (aget labels best) REGIONS))
          -1
          (let [region (aget labels best)
                ^js rep (aget (.-regs shape) region)]
            (.packShape s region true (.-px rep) (.-pz rep) false 0 0))))))

  (init [s]
    (set! view (js-obj "stateAt" (fn [x y z] (.viewAt s x y z))))
    (set! start-h (.nodeH s from-x from-y from-z))
    ;; (before goalNotStandable reuses standH)
    (set! start-slow (if (and (== (aget tbl-hazard support) SLOW) (not ^boolean (.isWater s from-x from-y from-z))) 1 0)))

  (begin [s]
    (set! started true)
    (set! t0 (js/performance.now))
    (cond
      (neg? start-h) (.finish s "start-not-standable")
      ^boolean (.goalNotStandable s) (.finish s "goal-not-standable")
      :else
      (do
        (set! start-distance (.distanceTo s from-x from-z))
        (let [shape (.startShape s)
              region (bit-and shape 15)]
          (if (neg? shape)
            (.finish s "start-not-standable")
            (do
              ;; the start is node 0
              (aset shapes 0 shape)
              (aset node-keys 0 (.keyOf s from-x from-y from-z region))
              (aset xs 0 from-x)
              (aset ys 0 from-y)
              (aset zs 0 from-z)
              (aset hs 0 start-h)
              (aset parents 0 -1)
              (aset gs 0 0)
              (aset fs 0 (.heuristic s from-x from-z))
              (aset hash-table (bit-and (.hashOf s from-x from-y from-z region) (dec slots)) 0)
              (aset slows 0 start-slow)
              (set! n-nodes 1)
              (set! heap-n 1)
              (aset heap 0 0)
              (aset heap-pos 0 0)))))))

  ;; the body died idling on magma: a route never ends on a magma block or in a magma column, so such a node is passed
  ;; through (it is searched on from) but is neither the goal nor the best partial end
  (endsOnMagma [s x y z h]
    (or (== (aget tbl-bubble (.stateAt snapshot x y z)) 2)
        (and (zero? h) (== (aget tbl-magma (.stateAt s x (dec y) z)) 1))))

  ;; pop the best open node and expand it
  (expandNext [s]
    (if (zero? heap-n)
      (when-not ^boolean (.floodAtEnd s)
        (.finish s (if boxed "box" "exhausted")))
      (let [i (.popMin s)
            x (aget xs i)
            y (aget ys i)
            z (aget zs i)
            deadly ^boolean (.endsOnMagma s x y z (aget hs i))]
        (set! expanded (inc expanded))
        (if (and (not deadly) ^boolean (.reached s x y z))
          (do (set! goal-node i)
              (.finish s nil))
          (let [d (.distanceTo s x z)]
            (when (and (not deadly) (< d best-distance))
              (set! best-distance d)
              (set! best-node i))
            (.expandAt s x y z (aget hs i) (aget slows i) i (bit-and (aget shapes i) 15))
            (when (pos? (.-length held)) (.flushHeld s))
            (when over-budget (.finish s "budget")))))))

  ;; ---- steps the body cannot undo ----

  ;; Can the planner's own moves take the body from the lower cell back to the upper one? Runs the moves out of the lower cell
  ;; and looks for the one that enters the upper cell (the goal flood's probe, for one cell).
  (canReturn [s lx ly lz lh ux uy uz]
    (set! flooding true)
    (set! fr -1)
    (set! fx ux)
    (set! fy uy)
    (set! fz uz)
    (set! hit false)
    (.expandAt s lx ly lz lh 0 -1 -1)
    (set! flooding false)
    hit)

  ;; A one-way step cannot be undone with the body's own moves: a gap jump down, a drop of more than JUMP-UP, or a drop the planner
  ;; has no step-up move back from (a one-way move).
  (isOneWay [s node]
    (let [p (aget parents node)
          m (aget moves node)]
      (cond
        (== m MOVE-GAP) (< (.stand16 s node) (.stand16 s p))
        (not (== m MOVE-DROP)) false
        :else (or (> (- (.stand16 s p) (.stand16 s node)) JUMP-UP)
                  (not ^boolean (.canReturn s (aget xs node) (aget ys node) (aget zs node) (aget hs node) (aget xs p) (aget ys p) (aget zs p)))))))

  ;; the first one-way step on the way to the node (nearest the start), or -1
  (firstOneWay [s node]
    (loop [i node
           first -1]
      (if (== (aget parents i) -1)
        first
        (recur (aget parents i) (if ^boolean (.isOneWay s i) i first)))))

  (flushHeld [s]
    (set! replaying true)
    (let [held-now (.splice held 0)]
      (dotimes [k (.-length held-now)]
        (let [^js d (aget held-now k)
              x (aget d 0) y (aget d 1) z (aget d 2) p (aget d 5)]
          (when ^boolean (.canReturn s x y z (aget d 3) (aget xs p) (aget ys p) (aget zs p))
            (set! move-open (aget d 11))
            (set! move-air (aget d 12))
            (set! move-peak (aget d 13))
            (set! move-water (aget d 14))
            (.consider s x y z (aget d 3) (aget d 4) p (aget d 6) (aget d 7) (aget d 8) (aget d 9) (aget d 10))))))
    (set! replaying false)
    (set! move-open 0)
    (set! move-air 0)
    (set! move-peak 0)
    (set! move-water 0))

  ;; after a late flood with budget: one that ran out of it (and neither leaked nor met the start) is due again after
  ;; FLOOD-SPACING times the expansions with FLOOD-GROWTH times the budget (at most max-nodes), so its cost stays a share of
  ;; the search's and a large walled-in region is still proved (live: a sealed platform whose flood needed ~12000 nodes
  ;; searched the whole wide box, 50-190 s a give-up); any other is the last
  (growFlood [s budget]
    (if (and (> flooded budget) (not leaked) (< budget max-nodes))
      (do (set! goal-flood (js/Math.min max-nodes (* FLOOD-GROWTH budget)))
          (set! flood-after (* FLOOD-SPACING flood-after)))
      (set! flood-pending false)))

  ;; a search that ran out of nodes to expand with its late flood still due floods once more, so a walled-in goal is named
  ;; so (live: a gateless pen on a platform whose search ran out before the flood ended :one-way at the platform's edge):
  ;; with the flood budget when no late flood ran yet, else with no more than the search expanded. Not when a ladder was
  ;; turned away at a gap or a swim for air: those reasons say more. True when that flood is begun (step runs it and
  ;; finishes the search: goal-enclosed, box or exhausted).
  (floodAtEnd [s]
    (if (or (not flood-pending) goal-unloaded gap-seen air-seen)
      false
      (do (.lateFloodBegin s (if (pos? flooded) (js/Math.min goal-flood expanded) goal-flood) true)
          true)))

  ;; ---- the late flood ----
  ;; The late floods and the flood at the end of one search are one backward flood (goalEnclosed's, without the cliff
  ;; test), continued: a run with a bigger budget goes on from where the last one stopped. It is breadth-first from the same
  ;; seeds over the same moves, so a run floods what a fresh flood with its budget would, in the same order, and answers
  ;; the same; the cells flooded before are not flooded again, and a run can stop and go on later (step's slices).

  (lateFloodBegin [s budget end]
    (set! lf-budget budget)
    (set! lf-end end)
    (set! lf-active true)
    (when (nil? lf-seen)
      (set! lf-seen (js/Set.))
      (set! lf-queue #js [])
      (set! leaked false)
      (.floodSeeds s lf-seen lf-queue)
      (when (true? (.has lf-seen (aget node-keys 0)))
        (set! lf-open true)
        (set! lf-seed-open true))))

  ;; floods at most about allowance more cells of the run in progress; true when the run is over (the start met, the
  ;; flood exhausted, or past its budget), false when it stopped for the allowance
  (lateFloodRun [s allowance]
    (if lf-open
      true
      (let [start-key (aget node-keys 0)
            limit (+ (.-size lf-seen) allowance)]
        (set! flooding true)
        (set! allow-shut true) ; a shut trapdoor is a way through, only dearer: the flood must not call its far side enclosed
        (let [state (loop []
                      (cond
                        (not (and (< lf-head (.-length lf-queue)) (<= (.-size lf-seen) lf-budget))) 0
                        (> (.-size lf-seen) limit) 1
                        :else
                        (do
                          (set! fx (aget lf-queue lf-head))
                          (set! fy (aget lf-queue (+ lf-head 1)))
                          (set! fz (aget lf-queue (+ lf-head 2)))
                          (set! fr (aget lf-queue (+ lf-head 3)))
                          (set! flood-target (if (neg? fr) (- -1 (.keyOf s fx fy fz 0)) (.keyOf s fx fy fz fr)))
                          (if (or ^boolean (.floodColumn s lf-seen lf-queue start-key) ^boolean (.floodAround s lf-seen lf-queue start-key))
                            2
                            (do (set! lf-head (+ lf-head 4))
                                (recur))))))]
          (set! flooding false)
          (set! allow-shut false)
          (set! fr -1)
          (when (== state 2) (set! lf-open true))
          (not (== state 1))))))

  ;; the run that is over: true when the goal is walled in (the flood exhausted within its budget, neither meeting the start
  ;; nor leaking); flooded is the flood's size (left alone when the start is one of the goal's own cells, as goalEnclosed)
  (lateFloodEnd [s]
    (set! lf-active false)
    (when-not lf-seed-open (set! flooded (.-size lf-seen)))
    (and (not lf-open) (not leaked) (<= (.-size lf-seen) lf-budget)))

  ;; the goal flood costs ~30 ms, so easy queries must never see it: it runs after flood-after forward expansions. A slice
  ;; of max-expansions counts each newly flooded cell as one.
  (step [s max-expansions]
    (when-not started
      (.begin s)
      (when (and (not finished) ^boolean (.goalEnclosedEarly s))
        (.finish s "goal-enclosed")))
    (loop [n 0]
      (when (and (< n max-expansions) (not finished))
        (cond
          lf-active
          (let [size0 (.-size lf-seen)
                over ^boolean (.lateFloodRun s (- max-expansions n))
                used (inc (- (.-size lf-seen) size0))]
            (cond
              (not over) (recur (+ n used))
              lf-end (let [enclosed ^boolean (.lateFloodEnd s)]
                       (set! flood-pending false)
                       (set! expanded (+ expanded flooded))
                       (.finish s (cond enclosed "goal-enclosed" boxed "box" :else "exhausted")))
              :else (let [enclosed ^boolean (.lateFloodEnd s)]
                      (.growFlood s lf-budget)
                      (set! expanded (+ expanded flooded))
                      (if enclosed
                        (.finish s "goal-enclosed")
                        (do (.expandNext s)
                            (recur (+ n used)))))))

          (and flood-pending (>= expanded flood-after) (not goal-unloaded))
          (do (.lateFloodBegin s goal-flood false)
              (recur n))

          :else
          (do (.expandNext s)
              (recur (inc n))))))
    finished)

  ;; ---- results ----

  (stepsTo [s node]
    (let [out #js []]
      (loop [i node]
        (when-not (== i -1)
          (let [shape (aget shapes i)
                tight (== (bit-and (bit-shift-right shape 4) 1) 1)
                x (aget xs i)
                z (aget zs i)
                step #js {:x x :y (aget ys i) :z z :h (aget hs i) :move (aget moves i) :corner (== (aget corners i) 1)
                          :px (+ x (if tight (/ (bit-and (bit-shift-right shape 5) 31) 16) 0.5))
                          :pz (+ z (if tight (/ (bit-and (bit-shift-right shape 10) 31) 16) 0.5))}]
            (when ^boolean (.isWater s x (aget ys i) z) (unchecked-set step "swim" true))
            (when (pos? (aget opens i)) (unchecked-set step "opens" (aget open-lists (dec (aget opens i)))))
            (let [p (aget parents i)
                  wall (.hatchWall s x (aget ys i) z)]
              (when (and (pos? wall) (>= p 0) (< (aget ys p) (aget ys i)))
                (unchecked-set step "hatch" true)
                (unchecked-set step "px" (+ x 0.5 (* 0.3 (aget wall-dx wall))))
                (unchecked-set step "pz" (+ z 0.5 (* 0.3 (aget wall-dz wall))))))
            (when (== (bit-and (bit-shift-right shape 15) 1) 1)
              (unchecked-set step "cx" (+ x (/ (bit-and (bit-shift-right shape 16) 31) 16)))
              (unchecked-set step "cz" (+ z (/ (bit-and (bit-shift-right shape 21) 31) 16))))
            (.push out step))
          (recur (aget parents i))))
      (.reverse out)))

  ;; the facing (1 east, 2 west, 3 south, 4 north) of the ladder under a trapdoor at x,y,z, or 0: a climb up into that cell
  ;; is aimed at the ladder's wall, where the body stands on the ladder's top edge when the trapdoor's panel leaves it free
  (hatchWall [s x y z]
    (let [id (.stateAt snapshot x y z)
          below (.stateAt snapshot x (dec y) z)]
      (if (or (== id UNLOADED) (== below UNLOADED) (not (== (aget tbl-open-kind id) KIND-TRAPDOOR))
              (not (== (aget tbl-climb-name below) LADDER)))
        0
        (aget tbl-facing below))))

  ;; "ladder up 9": consecutive climbing legs on one kind of climbable in one direction are one run
  (climbRuns [s ^js legs]
    (run-strings legs
                 (fn [^js leg]
                   (let [^js st (aget leg 0) ^js p (aget leg 1)]
                     (when (climbing-move? (.-move st))
                       (let [named (aget tbl-climb-name (.stateAt snapshot (.-x st) (.-y st) (.-z st)))
                             named (if (zero? named) (aget tbl-climb-name (.stateAt snapshot (.-x p) (.-y p) (.-z p))) named)]
                         (str (js/String (aget CLIMB-NAMES named)) " " (if (> (.-y st) (.-y p)) "up" "down"))))))))

  ;; "water column up 20", "bubble lift up 18", "magma column down 15": consecutive vertical swim legs of one kind and way
  (swimRuns [s ^js legs]
    (run-strings legs
                 (fn [^js leg]
                   (let [^js st (aget leg 0) ^js p (aget leg 1)
                         m (.-move st)]
                     (when (or (== m MOVE-SWIM-UP) (== m MOVE-SWIM-DOWN))
                       (let [bubbles (js/Math.max (aget tbl-bubble (.stateAt snapshot (.-x st) (.-y st) (.-z st)))
                                                  (aget tbl-bubble (.stateAt snapshot (.-x p) (.-y p) (.-z p))))]
                         (if (== m MOVE-SWIM-UP)
                           (if (== bubbles 1) "bubble lift up" "water column up")
                           (if (== bubbles 2) "magma column down" "water column down"))))))))

  ;; "opens 2 doors", "opens 1 gate", "presses 1 button", "steps on 1 plate": what the path opens, by what opens it
  (openRuns [s ^js steps]
    (let [all #js []]
      (loop [k 0]
        (when (< k (.-length steps))
          (let [opens-here (unchecked-get (aget steps k) "opens")]
            (when (some? opens-here) (.apply (.-push all) all opens-here)))
          (recur (inc k))))
      (let [by-hand (fn [kind-code noun]
                      (phrase "opens" noun
                              (.-length (.filter all (fn [^js o]
                                                       (and (undefined? (unchecked-get o "via"))
                                                            (== (aget tbl-open-kind (.stateAt snapshot (.-x o) (.-y o) (.-z o))) kind-code)))))))
            by-via (fn [via verb noun]
                     (phrase verb noun (.-length (.filter all (fn [^js o] (identical? (unchecked-get o "via") via))))))]
        (.filter #js [(by-hand KIND-DOOR "door") (by-hand KIND-GATE "gate") (by-hand KIND-TRAPDOOR "trapdoor")
                      (by-via "button" "presses" "button") (by-via "lever" "pulls" "lever") (by-via "plate" "steps on" "plate")]
                 some?))))

  (summarize [s ^js steps node]
    (let [legs (legs-of steps)
          blocks (js/Math.round (sum-hypot legs))
          rise? (fn [^js leg]
                  (let [^js st (aget leg 0) ^js p (aget leg 1)
                        m (.-move st)]
                    (and (not (climbing-move? m)) (not (swimming-move? m)) (not (== m MOVE-DROP)) (not (== m MOVE-GAP))
                         (> (stand16 st) (stand16 p)))))
          ups (.-length (.filter legs rise?))
          deep? (fn [d] (>= d 2))
          falls (.filter (.map (.filter legs (fn [^js leg] (let [^js st (aget leg 0)] (and (== (.-move st) MOVE-DROP) (not ^boolean (.isWater s (.-x st) (.-y st) (.-z st))))))) depth) deep?)
          splashes (.filter (.map (.filter legs (fn [^js leg] (let [^js st (aget leg 0)] (and (== (.-move st) MOVE-DROP) ^boolean (.isWater s (.-x st) (.-y st) (.-z st)))))) depth) deep?)
          swum (js/Math.round (sum-hypot (.filter legs (fn [^js leg] (let [^js st (aget leg 0) m (.-move st)]
                                                                       (or (== m MOVE-SWIM) (and (== m MOVE-CORNER) (true? (unchecked-get st "swim")))))))))
          lowest (- c-air-supply (aget peaks node))
          gaps (.-length (.filter legs (fn [^js leg] (== (.-move ^js (aget leg 0)) MOVE-GAP))))
          slides (count-steps steps 0 (fn [^js st _] (true? (.-corner st))))
          gap-ups (.-length (.filter legs (fn [^js leg] (let [^js st (aget leg 0) ^js p (aget leg 1)] (and (== (.-move st) MOVE-GAP) (> (stand16 st) (stand16 p)))))))
          lava (loop [k 0] (cond (== k (.-length steps)) false
                                 (let [^js st (aget steps k)] ^boolean (.lavaNear s (.-x st) (.-y st) (.-z st))) true
                                 :else (recur (inc k))))
          parts #js [(str blocks " blocks")
                     (when (pos? swum) (str "swims " swum))]]
      (.apply (.-push parts) parts (.swimRuns s legs))
      (.apply (.-push parts) parts (.climbRuns s legs))
      (.apply (.-push parts) parts (.openRuns s steps))
      (.push parts (some-of ups "1 step up" " steps up"))
      (.push parts (when (== (.-length falls) 1) (str "1 drop of " (aget falls 0))))
      (.push parts (when (> (.-length falls) 1) (str (.-length falls) " drops, deepest " (max-of falls))))
      (.push parts (when (== (.-length splashes) 1) (str "drops " (aget splashes 0) " into water")))
      (.push parts (when (> (.-length splashes) 1) (str (.-length splashes) " drops into water, deepest " (max-of splashes))))
      (.push parts (some-of gaps "1 gap jump" " gap jumps"))
      (.push parts (some-of slides "1 corner slide" " corner slides"))
      (.push parts (some-of gap-ups "1 jump up over a gap" " jumps up over gaps"))
      (.push parts (when lava "passes 1 cell from lava"))
      (.push parts (when (pos? (aget peaks node)) (str "lowest air " (js/Math.round lowest) " s")))
      (.join (.filter parts some?) ", ")))

  (pathTo [s node]
    (let [steps (.stepsTo s node)
          n (.-length steps)
          cost #js {:seconds (aget secs node)
                    :risk (aget risks node)
                    :maxDrop (loop [k 1 best 0]
                               (if (< k n)
                                 (let [^js st (aget steps k) ^js p (aget steps (dec k))]
                                   (recur (inc k) (js/Math.max best (if (and (== (.-move st) MOVE-DROP) (not (true? (unchecked-get st "swim"))))
                                                                      (/ (- (stand16 p) (stand16 st)) 16) 0))))
                                 best))
                    :jumps (count-steps steps 0 (fn [^js st _] (let [m (.-move st)] (or (== m MOVE-JUMP) (== m MOVE-GAP) (== m MOVE-JUMP-CLIMB)))))
                    :climbed (count-steps steps 0 (fn [^js st _] (climbing-move? (.-move st))))
                    :opens (loop [k 0 total 0]
                             (if (< k n)
                               (let [opened (unchecked-get (aget steps k) "opens")]
                                 (recur (inc k) (+ total (if (some? opened) (.-length opened) 0))))
                               total))
                    :unknown 0
                    :waterSeconds (aget wsecs node)
                    :airMin (- c-air-supply (aget peaks node))
                    :waterDrop (loop [k 1 best 0]
                                 (if (< k n)
                                   (let [^js st (aget steps k) ^js p (aget steps (dec k))]
                                     (recur (inc k) (js/Math.max best (if (and (== (.-move st) MOVE-DROP) (true? (unchecked-get st "swim")))
                                                                        (/ (- (stand16 p) (stand16 st)) 16) 0))))
                                   best))}]
      #js {:steps steps :cost cost :summary (.summarize s steps node)}))

  (outcome [s status why path one-way]
    #js {:status status
         :reason why
         :ms elapsed
         :expanded expanded
         :stats #js {:masks masks :tightMasks tight-masks :tightCells (.-size tight-seen) :regions regions-seen :maskMs mask-ms :flooded flooded :preFlooded pre-flooded}
         :path path
         :oneWay one-way
         :frontier (when-not (identical? status "found") (.frontierOf s))
         :limited limit-refused})

  ;; ends the search if it is not over; an exhausted search that turned a ladder away at a gap, or a swim move for lack of air,
  ;; says so
  (settle [s]
    (when-not finished (.finish s "budget"))
    (cond
      (and (identical? reason "exhausted") gap-seen) (set! reason "ladder-gap")
      (and (identical? reason "exhausted") air-seen) (set! reason "air")))

  ;; the first one-way step on the way to the node nearest the goal, -1 when there is none or the result has no partial end
  (oneWayNode [s]
    (if (or (nil? reason) (identical? reason "start-not-standable") (identical? reason "goal-not-standable") returnable (== best-node -1))
      -1
      (.firstOneWay s best-node)))

  ;; Does the node stand within OPEN-REACH columns of unloaded land (at its feet height)? Then the land it is on runs on past what
  ;; is loaded; a node with loaded land all round it is a dead end as far as the world is known.
  (atLoadedEdge [s node]
    (let [x (aget xs node) y (aget ys node) z (aget zs node)]
      (loop [dx (- OPEN-REACH) dz (- OPEN-REACH)]
        (cond
          (> dx OPEN-REACH) false
          (> dz OPEN-REACH) (recur (inc dx) (- OPEN-REACH))
          (== (.stateAt snapshot (+ x dx) y (+ z dz)) UNLOADED) true
          :else (recur dx (inc dz))))))

  (nearest [s]
    #js {:path (if (== best-node -1) nil (.pathTo s best-node)) :distance best-distance})

  ;; Where an unfinished search has got to: the path to its expanded node nearest the goal, cut before the first step on the
  ;; way the body cannot undo (so the body can always come back, and whatever the search could reach it still can), with
  ;; that end's distance to the goal and the start's: {path distance startDistance oneWay}; path nil (distance the start's)
  ;; when the cut leaves no step or its end stands on magma. oneWay, when the way to the nearest node holds such a step, is
  ;; that step and the uncut path as result's oneWay: {move x y z distance path open}, open when the nearest node stands at
  ;; the loaded edge (atLoadedEdge, not on magma). nil when nothing was expanded but the start, or there is neither.
  (progress [s]
    (if (or (not started) (<= best-node 0))
      nil
      (let [ow (.firstOneWay s best-node)
            end (if (neg? ow) best-node (aget parents ow))
            cut (when-not (or (<= end 0) ^boolean (.endsOnMagma s (aget xs end) (aget ys end) (aget zs end) (aget hs end)))
                  end)
            one-way (when-not (neg? ow)
                      #js {:move (aget moves ow) :x (aget xs ow) :y (aget ys ow) :z (aget zs ow)
                           :distance best-distance :path (.pathTo s best-node)
                           :open (and ^boolean (.atLoadedEdge s best-node)
                                      (not ^boolean (.endsOnMagma s (aget xs best-node) (aget ys best-node) (aget zs best-node) (aget hs best-node))))})]
        (when (or (some? cut) (some? one-way))
          #js {:path (when (some? cut) (.pathTo s cut))
               :distance (if (some? cut) (.distanceTo s (aget xs cut) (aget zs cut)) start-distance)
               :startDistance start-distance
               :oneWay one-way}))))

  ;; The frontier node: of the nodes standing at the loaded edge (atLoadedEdge) within frontier-reach blocks of the goal
  ;; (along x and along z), the one with the least cost to it plus the heuristic on to the goal, -1 when none: where the
  ;; searched land runs on into land not loaded, so a way may go on there. Only columns within OPEN-REACH of a chunk's
  ;; side can be at the edge.
  (frontierNode [s]
    (let [lo OPEN-REACH
          hi (- 16 OPEN-REACH)]
      (loop [i 0
             best -1
             best-f js/Infinity]
        (if (< i n-nodes)
          (let [x (aget xs i) y (aget ys i) z (aget zs i)
                mx (bit-and x 15) mz (bit-and z 15)]
            (if (and (or (< mx lo) (>= mx hi) (< mz lo) (>= mz hi))
                     (<= (js/Math.max (js/Math.abs (- x goal-x)) (js/Math.abs (- z goal-z))) frontier-reach)
                     (not ^boolean (.endsOnMagma s x y z (aget hs i)))
                     ^boolean (.atLoadedEdge s i))
              (let [f (+ (aget gs i) (.heuristic s x z))]
                (if (< f best-f) (recur (inc i) i f) (recur (inc i) best best-f)))
              (recur (inc i) best best-f)))
          best))))

  ;; the result's frontier: {x y z path} of frontierNode for a search that ran out of land to search (exhausted, its box,
  ;; a ladder at a gap, air), nil otherwise or when no node stands at the loaded edge
  (frontierOf [s]
    (when (or (identical? reason "exhausted") (identical? reason "box") (identical? reason "ladder-gap") (identical? reason "air"))
      (let [node (.frontierNode s)]
        (when-not (neg? node)
          #js {:x (aget xs node) :y (aget ys node) :z (aget zs node) :path (.pathTo s node)}))))

  ;; the result; one-way-node is the first one-way step on the way to the nearest node (-1: none), clean-end the nearest node of
  ;; the returnable search then run ({path distance})
  (resultFrom [s one-way-node ^js clean-end]
    (cond
      (or (identical? reason "start-not-standable") (identical? reason "goal-not-standable")) (.outcome s "none" reason nil nil)
      (nil? reason) (let [^js r (.outcome s "found" reason (.pathTo s goal-node) nil)]
                      ;; a goal set's result names the goal reached (the first whose area holds the end)
                      (when (pos? n-goals)
                        (set! (.-goal r) (.goalAt s (aget xs goal-node) (aget ys goal-node) (aget zs goal-node))))
                      r)
      :else
      (let [clean (not (neg? one-way-node))
            end-path (cond clean (.-path clean-end) (== best-node -1) nil :else (.pathTo s best-node))
            end-distance (cond clean (.-distance clean-end) (== best-node -1) js/Infinity :else best-distance)
            one-way (when (and clean (< best-distance end-distance))
                      #js {:move (aget moves one-way-node) :x (aget xs one-way-node) :y (aget ys one-way-node) :z (aget zs one-way-node)
                           :distance best-distance :path (.pathTo s best-node) :open (.atLoadedEdge s best-node)})]
        (cond
          (identical? reason "budget") (.outcome s "partial" "budget" end-path one-way)
          goal-unloaded (.outcome s "partial" "goal-unloaded" end-path one-way)
          (and (some? end-path) (>= (- start-distance end-distance) MIN-CLOSER)) (.outcome s "partial" reason end-path one-way)
          :else (.outcome s "none" reason nil one-way))))))

(defn- option [^js options k default]
  (let [v (unchecked-get options k)]
    (if (undefined? v) default v)))

(defn- or-else [v default] (if (some? v) v default))

(defn- goal-bounds
  "#js [x0 x1 z0 z1 y0 y1]: the extent of the start and the goals (y: the start's and the sphere goals')."
  [^js from ^js goals]
  (let [b #js [(.-x from) (.-x from) (.-z from) (.-z from) (.-y from) (.-y from)]]
    (doseq [^js g (array-seq goals)]
      (aset b 0 (js/Math.min (aget b 0) (.-x g)))
      (aset b 1 (js/Math.max (aget b 1) (.-x g)))
      (aset b 2 (js/Math.min (aget b 2) (.-z g)))
      (aset b 3 (js/Math.max (aget b 3) (.-z g)))
      (when (identical? (.-kind g) "near")
        (aset b 4 (js/Math.min (aget b 4) (.-y g)))
        (aset b 5 (js/Math.max (aget b 5) (.-y g)))))
    b))

(defn- goal-array [ctor ^js goals f] (new ctor (.map goals f)))

(defn- new-search ^Search [^js snapshot ^js query ^js options]
  (let [^js table (.-table options)
        ^js from (.-from query)
        ^js set-goals (.-goals query)
        n-goals (if (some? set-goals) (.-length set-goals) 0)
        multi (pos? n-goals)
        ^js goal (if multi (aget set-goals 0) (.-goal query))
        ^js bounds (goal-bounds from (if multi set-goals #js [goal]))
        ^js costs (js/Object.assign #js {} DEFAULT-COSTS (.-costs options))
        max-nodes (option options "maxNodes" 200000)
        ^js avoid (.-avoid options)
        ^js limits (.-limits options)
        margin (option options "margin" 64)
        y-margin (option options "yMargin" 48)
        goal-range (or-else (.-range goal) 0)
        ;; a goal set has no goal flood and no goal-not-standable or goal-unloaded answers (near false): those prove
        ;; things of one goal
        near (and (not multi) (identical? (.-kind goal) "near"))
        goal-unloaded (cond
                        multi false
                        near (== (.stateAt snapshot (.-x goal) (.-y goal) (.-z goal)) UNLOADED)
                        :else (false? (.hasColumn snapshot (bit-shift-right (.-x goal) 4) (bit-shift-right (.-z goal) 4))))
        y-low (aget bounds 4)
        y-high (aget bounds 5)
        cap (js/Math.min max-nodes 1024)
        slots (next-pow2 (* cap 2))]
    (->Search
     ;; the world
     snapshot table (.-space options)
     (.-top table) (.-base table) (.-kind table) (.-hazard table) (.-stairUp table) (.-partial table) (.-climb table)
     (.-climbName table) (.-facing table) (.-floor table) (.-special table)
     (.-flowing table) (.-bubble table) (.-magma table) (.-dripleaf table) (.-openable table) (.-openState table)
     (.-openKind table) (.-doorHalf table) (.-activator table)
     (.-attach table) (.-farmland table) (.-minY snapshot)
     ;; the query
     (.-x from) (.-y from) (.-z from) (or-else (.-px from) (+ (.-x from) 0.5)) (or-else (.-pz from) (+ (.-z from) 0.5))
     (.-x goal) (.-y goal) (.-z goal) goal-range (* OCTILE-SLACK goal-range) near goal-unloaded
     ;; the goal set
     n-goals
     (when multi (goal-array js/Int32Array set-goals (fn [^js g] (.-x g))))
     (when multi (goal-array js/Int32Array set-goals (fn [^js g] (or-else (.-y g) 0))))
     (when multi (goal-array js/Int32Array set-goals (fn [^js g] (.-z g))))
     (when multi (goal-array js/Float64Array set-goals (fn [^js g] (let [r (or-else (.-range g) 0)] (* r r)))))
     (when multi (goal-array js/Float64Array set-goals (fn [^js g] (* OCTILE-SLACK (or-else (.-range g) 0)))))
     (when multi (goal-array js/Uint8Array set-goals (fn [^js g] (if (identical? (.-kind g) "near") 1 0))))
     ;; options
     max-nodes (option options "maxDrop" 3) (option options "weight" 1) (option options "riskWeight" 2)
     (option options "goalFlood" 4000) (option options "floodAfter" 3000) (option options "preFlood" 24)
     (option options "frontierReach" 256)
     ;; avoid
     (some? avoid) (if (some? avoid) (.-kinds avoid) 0) (if (some? avoid) (.-cells avoid) nil) (if (some? avoid) (.-factor avoid) 0)
     ;; limits
     (if (some? limits) (or-else (.-kinds limits) 0) 0) (if (some? limits) (.-gap limits) nil) (if (some? limits) (.-corner limits) nil)
     ;; costs
     (unchecked-get costs "climbUp") (unchecked-get costs "climbDown") (unchecked-get costs "jumpClimb") (unchecked-get costs "open")
     (unchecked-get costs "openRedstone") (unchecked-get costs "openPlate") (unchecked-get costs "besideMagmaColumn")
     (unchecked-get costs "swimH") (unchecked-get costs "swimUp") (unchecked-get costs "swimDown")
     (unchecked-get costs "exit") (unchecked-get costs "current") (unchecked-get costs "bubbleUp") (unchecked-get costs "bubbleDown")
     (unchecked-get costs "airSupply") (unchecked-get costs "airLimit") (unchecked-get costs "maxWaterDrop")
     (unchecked-get costs "dripleaf") (unchecked-get costs "dripleafRisk")
     ;; search box
     (- (aget bounds 0) margin) (+ (aget bounds 1) margin)
     (- (aget bounds 2) margin) (+ (aget bounds 3) margin)
     (- y-low y-margin) (+ y-high y-margin)
     ;; directions
     (js/Int8Array. #js [1 -1 0 0 1 1 -1 -1]) (js/Int8Array. #js [0 0 1 -1 1 -1 1 -1])
     ;; node storage: cap slots hash-table node-keys xs ys zs hs moves slows corners shapes parents secs risks airs peaks wsecs opens gs fs heap-pos heap n-nodes heap-n
     cap slots (.fill (js/Int32Array. slots) -1) (js/Float64Array. cap)
     (js/Int32Array. cap) (js/Int32Array. cap) (js/Int32Array. cap) (js/Uint8Array. cap) (js/Uint8Array. cap) (js/Uint8Array. cap) (js/Uint8Array. cap)
     (js/Uint32Array. cap)
     (js/Int32Array. cap) (js/Float64Array. cap) (js/Float64Array. cap)
     (js/Float64Array. cap) (js/Float64Array. cap) (js/Float64Array. cap) (js/Int32Array. cap)
     (js/Float64Array. cap) (js/Float64Array. cap)
     (js/Int32Array. cap)
     (js/Int32Array. cap) 0 0
     ;; support touch enter-risk enter-slow enter-extra ty gap-y quiet open-mode allow-shut after-exit gap-seen air-seen enters-shut
     0 0 0 0 0 0 0 false false false false false false false
     ;; move-air move-peak move-water move-open open-lists activators view
     0 0 0 0 #js [] (js/Map.) nil
     ;; edge-mode seen-edges door-arrival door-here door-through open-x open-y open-z
     0 nil nil nil false 0 0 0
     ;; tight cells
     (js/Float64Array. TABLE) (js/Uint8Array. TABLE) (js/Float64Array. TABLE) (js/Uint8Array. TABLE) (js/Map.) (js/Set.) (js/Int8Array. REGIONS)
     (js/Float64Array. REGIONS) (js/Int16Array. REGIONS) (js/Map.) (js/Map.)
     0 0 0 0
     ;; the goal flood: flooding fx fy fz fr hit flooded pre-flooded flood-pending leaked
     false 0 0 0 -1 false 0 0
     (and near (pos? (option options "goalFlood" 4000))) false
     ;; flood-moves flood-out flood-h-keys flood-h-vals flood-target
     (js/Map.) nil nil nil 0
     ;; progress: started finished reason over-budget boxed goal-node best-node
     false false nil false false -1 -1
     ;; best-distance start-distance expanded t0 elapsed start-h start-slow
     js/Infinity 0 0 (js/performance.now) 0 -1 0
     ;; returnable held replaying
     (true? (option options "returnable" false)) #js [] false
     ;; lf-seen lf-queue lf-head lf-budget lf-active lf-open lf-seed-open lf-end limit-refused
     nil nil 0 0 false false false false false
     ;; capturing capture-out
     false nil)))

(defn- clean-options
  "The options of the returnable search behind a one-way step of search: options.returnable, no goal flood, and no more
  nodes than the search itself made. Its nearest node is only where a partial plan ends; with no flood to prove a walled-in
  goal it would otherwise search all the box holds (live: 2 s over the wide box after a 300 ms search)."
  [^Search search options]
  (js/Object.assign #js {} options #js {:returnable true :goalFlood 0
                                        :maxNodes (js/Math.min (option options "maxNodes" 200000) (js/Math.max 1 (.-n-nodes search)))}))

(defn- result-of
  "The result of a finished search; when the path to its nearest node holds a one-way step, a second search with options.returnable
  (and no goal flood, clean-options) supplies the partial end."
  [^Search search snapshot query options]
  (.settle search)
  (let [node (.oneWayNode search)]
    (if (neg? node)
      (.resultFrom search -1 nil)
      (let [^Search clean (new-search snapshot query (clean-options search options))]
        (.init clean)
        (.step clean js/Infinity)
        (.resultFrom search node (.nearest clean))))))

(defn capture-search
  "A Search for engine.path.regions: no query of its own (its start and goal are `centre`), capturing (see captureAt).
  Its key and caches are relative to centre (keyOf packs x and z within 2048 of it) and assume the world does not change:
  the caller makes a new one far from centre and after any block change. options as plan's (table, space)."
  ^Search [snapshot ^js centre options]
  (let [^Search search (new-search snapshot
                                   #js {:from centre :goal #js {:kind "near" :x (.-x centre) :y (.-y centre) :z (.-z centre) :range 1}}
                                   (js/Object.assign #js {} options #js {:goalFlood 0 :maxNodes 16}))]
    (.init search)
    (.capturing! search)
    search))

(defn create-search
  "{step, result, nearest} as create-search returns them. options.table and options.space are required."
  [snapshot query options]
  (let [search (new-search snapshot query options)]
    (.init search)
    #js {:step (fn [max-expansions] (.step search max-expansions))
         :result (fn [] (result-of search snapshot query options))
         :nearest (fn [] (.nearest search))}))

(defn plan [snapshot query options]
  (let [search (new-search snapshot query options)]
    (.init search)
    (.step search js/Infinity)
    (result-of search snapshot query options)))

(defn create-plan
  "plan in slices, for a caller that yields to the event loop between them: {step, result, progress}. step(n) runs at most
  about n expansions (each newly flooded cell of the goal flood counts as one) and is true once the plan is ready; result()
  is then what plan answers. The returnable search behind a one-way step (result-of) runs in the same slices. progress()
  is where an unfinished search has got to (Search.progress), nil once the search itself is over."
  [snapshot query options]
  (let [search (new-search snapshot query options)
        clean (volatile! nil)
        node (volatile! -1)
        ready (volatile! false)]
    (.init search)
    #js {:step (fn [max-expansions]
                 (cond
                   @ready true
                   (some? @clean) (vreset! ready ^boolean (.step ^Search @clean max-expansions))
                   (not ^boolean (.step search max-expansions)) false
                   :else (do (.settle search)
                             (vreset! node (.oneWayNode search))
                             (if (neg? @node)
                               (vreset! ready true)
                               (let [^Search c (new-search snapshot query (clean-options search options))]
                                 (.init c)
                                 (vreset! clean c)
                                 false)))))
         :result (fn [] (if (neg? @node)
                          (.resultFrom search -1 nil)
                          (.resultFrom search @node (.nearest ^Search @clean))))
         ;; while the search itself is not over: where it has got to (Search.progress), else nil
         :progress (fn [] (when-not (.-finished search) (.progress search)))}))
