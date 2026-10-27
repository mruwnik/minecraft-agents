(ns engine.path.planner-tuned
  "The path planner's search: an A* over typed arrays of cells, moves and costs (climbing, water, doors, tight
   cells). It returns a status, reason, expanded count, path, costs and summary. Written for speed.

   How it stays fast:
   - All search state is in the mutable fields of one Search object (engine.path.planner.search), and every function of
     the search is a method of it (added by part in the engine.path.planner.* namespaces). The emitted JavaScript is property reads, typed-array indexing and direct method calls.
   - No persistent data, no seqs, no keywords in the hot code. Every test is a comparison or a ^boolean hinted call,
     so no truthiness check is emitted (the compiled planner has one cljs.core.truth_, in count-steps).
   - `let`, `do` and `loop` only in statement or tail position: in expression position they compile to a closure
     called on the spot.
   - Values that a JS function returns on the side (support, touch, enter-risk, enter-slow, enter-extra, ty) are
     fields. Where a closure would be passed (swimEdge's emit, the opening pass's edge) the call is split in a begin
     and an end, or `edge-mode` is switched.

   Called through interop, not written in the search: the snapshot (stateAt, sectionHas, hasColumn), the state table's
   arrays, and the free-space masks of engine.path.space (options.space: boxesNear, freeMask, labelRegions).

   Goal sets: query.goals is an array of goals ({kind x y z range}, like query.goal). One search plans to the nearest
   of them by cost. The goal test is the cell in any goal's area, and the heuristic is the least of the goals'
   (admissible and consistent, as each is). A found result's goal is the index of the goal reached. A goal set runs no
   goal flood and answers no goal-not-standable or goal-unloaded, since those prove things about one goal. query.goal
   is not read then.

   Options:
   - options.goalSeen (fn x y z): a cell in the goal's area is the goal only when it answers true (go-to's :see: the
     eye there sees the goal block). The goal flood and goal-not-standable still judge the whole area.
   - options.avoid {kinds, cells, factor}: a search for another path (see avoidCost).
   - options.limits {kinds, gap, corner}: what the walker can do. Without it the search is unrestricted.
       kinds: bits of the move kinds never planned.
       gap(x, y, z, h, move, lx, ly, lz, lh): false refuses a gap jump from the takeoff node (feet cell x,y,z, stand h
         in 1/16, reached by move) to the landing.
       corner(x, y, z, h, lx, ly, lz, lh): false refuses a diagonal jump with one blocked side (a corner slide).
     engine.path.executor/planner-limits builds them.
   - options.knownCells: a Set of the cells (knownKey) earlier searches toward the same goal knew to their end. The
     frontier avoids them while another edge is loaded (frontierNode, result frontier.known), and a search that ends
     exhausted puts the cells it adds in the result's known (a Set; nil without knownCells), and the cells of its
     nodes at the loaded edge that are not known in the result's edges. jobs.lib.walk keeps both for go-to.
   - options.knownEdges: a Set of the cells (knownKey) earlier searches found at the loaded edge that no search has
     known to its end since. When it is empty, a frontier in known land is never taken: every edge was searched past,
     and the result says so as searchedOut true (the way on, if any, is not in the land this goal's searches can reach).
   - options.dangers: known hostiles, an array of {x y z close radius rate block} (at most 8): a move adds to its risk rate
     (hp a second) times its seconds within close blocks of a danger's point, falling linearly to 0 at radius; all the
     dangers together add at most options.dangerCap (4) a second (see dangerRisk), each hp priced at options.damageWeight. A danger with block true (a seen mob) also fills its cell: no plan enters it. jobs.lib.threats builds them.
   - options.dark {at, factor, water}: a cell that at(x, y, z) calls dark (returns 1) costs factor times its own seconds
     more, one it gives class 2 or more water[class - 2] times, in g and in cost.darkSeconds (see darkOf; jobs.lib.walk.world builds at).
   - options.tolls {cells}: cells is a Map of cell-key (planner/cell-key) to a factor: entering such a cell costs factor
     times its own seconds more, in g and in cost.darkSeconds (the caller's price of a cell; no zone knowledge here); Infinity: no plan enters it.
     tolls.edges (optional): a Map of a cell's cell-key to a flat array of [move, to cell-key, seconds]: that move out of the
     cell into the to cell costs those seconds more, the same way; a to cell-key of -1 prices every move of that kind out of
     the cell that has no entry of its own (a step the walker failed at, jobs.lib.walk.edges).
   - options.damageBudget (Infinity): the most hp of certain damage a path may take (falls over 3 blocks, plants that hurt on
     touch); a move over it is refused, and a search that finds nothing for that reason says damageRefused true. options.maxDrop
     is still the length of the longest fall. options.damageWeight (riskWeight): seconds an hp of that damage costs.
     options.fallFactor (1) scales a fall's damage (feather falling, protection); options.landing, a Map of state id to the
     factor of the fall damage onto that block (hay 0.2), a negative one: a bounce (no damage, BOUNCE-S settle seconds) when the
     8 blocks round the landing's are bouncing ones or a wall (else the full fall) and options.landingSeen (fn x y z, nil: all)
     says the body sees each now. A drop onto a stalagmite (pointed dripstone pointing up) takes (fall - 1) * 2 hp, so a
     fall of over 1 block hurts. result.cost.damage is the path's damage, a step's damage its own.
   - A climb up prices the fall it risks, a climb cell's height over the solid ground below it (through free and climbable cells;
     none over water or unloaded land): CLIMB-FALL-P of that fall's hp as risk, and a cell whose fall would reach options.health is
     refused (not at options.health 20, the default: a body at full health climbs any shaft).
   - options.costs.dropFactor (1): scales the fall seconds and fall damage of every drop on land (0: free); options.maxDrop
     (3) refuses a drop of more than that many blocks (1: none of 2 or 3). go-to's :drop-cost sets them.
   - options.costs.swimSubH (0.26, at most swimH): seconds a sideways swim cell takes with the head under water, the walker sprint-swimming ~3.9 b/s (gait :walk and :sneak price it at swimH; Depth Strider scales it).
   - options.costs.airUsed (0): seconds of air the body has used at the start (a plan made mid-dive); go-to's body-policy
     sets it from the oxygen when the head is under water. Such a plan to a goal whose head cell breathes, with a breathing
     head cell seen within the goal's range (at most TO-AIR-REACH blocks; options.airSeen, fn x y z: the body has seen or felt
     that block, nil: all) (to-air?), is never refused for air and ends with the head in such a cell: each second past the supply drowns DROWN-HP hp (result cost.drown; not
     damage, so no damageBudget), the first k hp costing options.drownPrices[k] seconds (nil: damageWeight each); drowning
     with the plan's certain damage to options.health (20) is lethal, each air-out (under since the last breath) counting its 2 hp hits as ceil(seconds + costs.drownSlack (1)): refused (reason air-lethal) unless options.costs.lethalAir is 1 (go-to :costs :lethal-air, breathe's), then it costs LETHAL-S (times the dearest second the caller's prices allow) more, so a lethal plan comes back only when no other does. Any
     other plan swims no further than airLimit from airUsed.
   - options.unknown (nil: off, every loaded cell as it is) {id floorId cell seeDepth seeLit seeLitDepth lightDepth peekS aimS lightCost lightOk
     torchBase missed noLight changed deathHp}: a cell of state id (void_air) is one the body has not seen, air to the body (but never breath: a swim in water under an unseen head cell drains the air like a dive). A stand on such a
     floor is a phantom floor (read as floorId, stone; not under water): allowed only after a move the walk can stop before
     (walk, diagonal, corner, jump, drop; not after a gap, climb, trapdoor open, swim, exit or bounce: else result unseenRefused; a
     gap may jump over it, not through an unseen feet or head cell) and only where it can be seen: a drop of d (level and
     up: 0) within seeDepth, or within seeLitDepth (10) over known air that seeLit(x, y, z) calls lit (1), or
     lit by a torch (lightCost seconds, the step's light true) from a takeoff whose feet are known air on a torchBase floor
     (Uint8Array by state id), d at most lightDepth (5), lightOk(x, y, z) not false and not in noLight; else result darkRefused and
     darkBest {x y z h}. A floor in missed is never seen, only lit. The floor under a door's lower half is implied. A move with an
     unseen feet, head or floor cell costs cell times its seconds, and peekS a drop onto a phantom floor, aimS a level or up step
     (in cost.darkSeconds; jobs.lib.cost/unknown-prices). No fall goes into an unseen cell, risk looks stop there, and no
     goal-enclosed, goal-not-standable or start-enclosed proof rests on one. missed and noLight are Sets of cell-key.
     changed(x, y, z) (nil: 0 everywhere) is the chance a remembered cell changed since it was seen (unseen at that chance):
     a move costs cell times its seconds times the most of its feet, head and floor cells', and a drop into remembered water
     adds that chance of the dry fall to the pool's floor as risk (its damage, or deathHp when it kills; default lethal-s
     over riskWeight) in place of landingSeen's cut. No proof rests on a cell at 1/2 or more.
   - options.stopAtEdge: with the goal unloaded, the search names the first node it expands at the loaded edge (edgeStop)
     as its frontier and ends once it has expanded as many nodes again (or run out, or the caller ends it: endSweep), not
     after searching all loaded land; what only those nodes refused is not in the result. go-to's budgeted searches set it
     (walk.search/new-search)."
  (:require [engine.constants :as constants]
            [engine.path.planner.base :as base :refer [OCTILE-SLACK REGIONS TABLE WHOLE next-pow2]]
            [engine.path.planner.search :refer [Search ->Search]]
            [engine.path.planner.world]
            [engine.path.planner.nodes]
            [engine.path.planner.tight]
            [engine.path.planner.bends]
            [engine.path.planner.water]
            [engine.path.planner.moves]
            [engine.path.planner.doors]
            [engine.path.planner.flood]
            [engine.path.planner.run]
            [engine.path.planner.results]
            [engine.path.planner.danger]
            [engine.path.planner.dark]
            [engine.path.planner.unknown :as unk-defaults]))

(set! *warn-on-infer* true)

;; the planner's public constants
(def UNLOADED base/UNLOADED)
(def AVOID-CLIMB base/AVOID-CLIMB)
(def AVOID-WATER base/AVOID-WATER)
(def AVOID-OPEN base/AVOID-OPEN)
(def MOVE base/MOVE)
(def DEFAULT-COSTS base/DEFAULT-COSTS)
(def cell-key base/cell-key)

(defn- option [^js options k default]
  (let [v (unchecked-get options k)]
    (if (undefined? v) default v)))

(defn- or-else [v default] (if (some? v) v default))

(def ^:const KEY-REACH 1000) ; knownKey holds cells within 1024 of the goal along x and z

(defn- reach-of
  "How far from the goal (along x and z) a frontier node may lie: options.frontierReach (256) past the start's own
  distance, so the land round the body always counts (a way down far behind it on a walkway), at most KEY-REACH."
  [^js options ^js from ^js goal]
  (js/Math.min KEY-REACH (+ (option options "frontierReach" 256)
                            (js/Math.max (js/Math.abs (- (.-x from) (.-x goal))) (js/Math.abs (- (.-z from) (.-z goal)))))))

(defn- ground-y
  "The y above the highest block of the x z column that is not open (the feet of a body on top of it); nil when the
  column is unloaded or all open."
  [^js snapshot ^js table x z]
  (let [kinds (.-kind table)
        bottom (.-minY snapshot)]
    (loop [y (+ bottom (or-else (.-height snapshot) 384) -1)]
      (when (>= y bottom)
        (let [id (.stateAt snapshot x y z)]
          (cond
            (== id UNLOADED) nil
            (== (aget kinds id) base/OPEN) (recur (dec y))
            :else (inc y)))))))

(def ^:const GROUND-SAMPLES 32) ; column goals whose ground goal-bounds reads: an even sample of a big set (targets/max-targets)

(defn- goal-bounds
  "#js [x0 x1 z0 z1 y0 y1]: the extent of the start and the goals (y: the start's, the sphere goals' and the ground of
  each column goal: a column goal whose ground lies outside the start's y margin would leave no goal cell in the box,
  and the search would drain the box before it answered). The ground is read for at most GROUND-SAMPLES column goals
  (an even sample), so a goal set of hundreds of columns costs a bounded number of reads; plan and create-plan search
  again over every column when a sampled search ends \"box\"."
  [^js snapshot ^js table ^js from ^js goals samples]
  (let [b #js [(.-x from) (.-x from) (.-z from) (.-z from) (.-y from) (.-y from)]
        stride (js/Math.max 1 (js/Math.ceil (/ (.-length goals) samples)))]
    (doseq [[i ^js g] (map-indexed vector (array-seq goals))]
      (aset b 0 (js/Math.min (aget b 0) (.-x g)))
      (aset b 1 (js/Math.max (aget b 1) (.-x g)))
      (aset b 2 (js/Math.min (aget b 2) (.-z g)))
      (aset b 3 (js/Math.max (aget b 3) (.-z g)))
      (let [y (cond
                (identical? (.-kind g) "near") (.-y g)
                (zero? (mod i stride)) (ground-y snapshot table (.-x g) (.-z g)))]
        (when (some? y)
          (aset b 4 (js/Math.min (aget b 4) y))
          (aset b 5 (js/Math.max (aget b 5) y)))))
    b))

(defn- goal-array [ctor ^js goals f] (new ctor (.map goals f)))

(def ^:const DANGER-STRIDE 7)
(def ^:const MAX-DANGERS 8)

(defn- danger-array
  "options.dangers ({x y z close radius rate}, at most MAX-DANGERS used) as a Float64Array of DANGER-STRIDE numbers per
  danger, each rate times scale (the damage price of the walk over the risk price, once per plan); nil for none."
  [^js ds scale]
  (when (and (some? ds) (pos? (.-length ds)))
    (let [n (js/Math.min MAX-DANGERS (.-length ds))
          a (js/Float64Array. (* n DANGER-STRIDE))]
      (dotimes [i n]
        (let [^js d (aget ds i)
              o (* i DANGER-STRIDE)]
          (aset a o (.-x d)) (aset a (+ o 1) (.-y d)) (aset a (+ o 2) (.-z d))
          (aset a (+ o 3) (.-close d)) (aset a (+ o 4) (.-radius d)) (aset a (+ o 5) (* scale (.-rate d)))
          (aset a (+ o 6) (if (true? (.-block d)) 1 0))))
      a)))

(defn- danger-box
  "#js [x0 x1 y0 y1 z0 z1]: the box round every danger's radius (an empty box for none)."
  [^js a]
  (let [b #js [js/Infinity js/-Infinity js/Infinity js/-Infinity js/Infinity js/-Infinity]]
    (when (some? a)
      (dotimes [i (/ (.-length a) DANGER-STRIDE)]
        (let [o (* i DANGER-STRIDE)
              r (aget a (+ o 4))]
          (aset b 0 (js/Math.min (aget b 0) (- (aget a o) r)))
          (aset b 1 (js/Math.max (aget b 1) (+ (aget a o) r)))
          (aset b 2 (js/Math.min (aget b 2) (- (aget a (+ o 1)) r)))
          (aset b 3 (js/Math.max (aget b 3) (+ (aget a (+ o 1)) r)))
          (aset b 4 (js/Math.min (aget b 4) (- (aget a (+ o 2)) r)))
          (aset b 5 (js/Math.max (aget b 5) (+ (aget a (+ o 2)) r))))))
    b))

;; The air a swim may use up to: airLimit, plus the free grace seconds' worth (airGrace * airDrain). A plan made mid-dive
;; counts it from the air used (airUsed): past it, no swim with the head under water (unless to-air?).
;; Drowning starts past the supply plus the same grace (c-air-drown).
(defn- air-grace [^js costs] (* (unchecked-get costs "airGrace") (unchecked-get costs "airDrain")))

(defn- air-limit [^js costs]
  (+ (unchecked-get costs "airLimit") (air-grace costs)))

(def ^:const TO-AIR-REACH "The most blocks of a goal's range to-air? looks through for seen air." 3)

(defn- to-air?
  "Whether a plan started with air used (costs.airUsed above 0) goes to air: its one near goal's head cell breathes
  (base/breathable?) and the body has seen or felt the head of a cell within the goal's range (at most TO-AIR-REACH)
  that breathes (options.airSeen, nil: every cell; a pocket seen edge-on through water shows a cell or two). No swim of
  such a plan is refused for air: it drowns, priced (drownCost), and it ends with the head in such a cell (expandNext)."
  [^js snapshot ^js query ^js options]
  (let [^js goal (.-goal query)
        ^js goals (.-goals query)
        ^js seen (.-airSeen options)
        ^js table (.-table options)]
    (and (pos? (or-else (some-> (.-costs options) (unchecked-get "airUsed")) 0))
         (or (nil? goals) (zero? (.-length goals)))
         (some? goal) (identical? (.-kind goal) "near")
         (base/breathable? table (.stateAt snapshot (.-x goal) (inc (.-y goal)) (.-z goal)))
         (or (nil? seen)
             (let [goal-range (or-else (.-range goal) 0)
                   r (js/Math.min TO-AIR-REACH (js/Math.floor goal-range))]
               (some (fn [[dx dy dz]]
                       (let [x (+ (.-x goal) dx) y (+ (.-y goal) dy 1) z (+ (.-z goal) dz)]
                         (and (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* goal-range goal-range))
                              (base/breathable? table (.stateAt snapshot x y z))
                              (true? (seen x y z)))))
                     (for [dx (range (- r) (inc r)) dy (range (- r) (inc r)) dz (range (- r) (inc r))] [dx dy dz])))))))

;; the seconds a lethal plan costs (Search lethal-s): LETHAL-S times the dearest second the caller's prices allow (a walk
;; at the danger cap through its dearest toll, in the dark, avoided, an hp of risk and of damage and a second of drowning
;; at the dearest drownPrices hp), so no toll, danger rate or weight makes a plan that does not kill cost more than one
;; that does. A price that is not finite (an Infinity toll, NaN) counts 0: lethal-s stays finite (Infinity * 0 is NaN).
(defn lethal-seconds [risk-weight damage-weight danger-cap dark-factor ^js tolls avoid-factor unknown-factor ^js drown-prices]
  (let [fin (fn [x] (if ^boolean (js/isFinite x) x 0))
        toll (if (some? tolls) (reduce max 0 (map fin (es6-iterator-seq (.values tolls)))) 0)
        drown-hp (if (some? drown-prices)
                   (reduce max (fin (aget drown-prices 0)) (map #(fin (- %2 %1)) drown-prices (rest drown-prices)))
                   0)]
    (* base/LETHAL-S (+ 1 (fin avoid-factor))
       (+ 1 toll (fin dark-factor) (fin unknown-factor) (fin (* risk-weight danger-cap)) (fin risk-weight) (fin damage-weight)
          (* base/DROWN-HP drown-hp)))))

(defn- first-id [^js table name] (aget (.get (.-nameIds table) name) 0))

(defn- unknown-factor
  "The most options.unknown adds to a second of walking (for lethal-seconds): an unseen move, and the dearest of a peek, an
  aim and a torch over the shortest move (a sprinted block); 0 when off."
  [^js unknown ^js costs]
  (if (nil? unknown)
    0
    (+ (or-else (.-cell unknown) 0)
       (/ (+ (js/Math.max (or-else (.-peekS unknown) 0) (or-else (.-aimS unknown) 0)) (js/Math.max 0 (or-else (.-lightCost unknown) 0)))
          (js/Math.min (unchecked-get costs "walkS") (unchecked-get costs "sprintS"))))))

(defn- search-from ^Search [^js snapshot ^js query ^js options]
  (let [^js table (.-table options)
        ^js from (.-from query)
        ^js set-goals (.-goals query)
        n-goals (if (some? set-goals) (.-length set-goals) 0)
        multi (pos? n-goals)
        ^js goal (if multi (aget set-goals 0) (.-goal query))
        ^js bounds (goal-bounds snapshot table from (if multi set-goals #js [goal]) (option options "groundSamples" GROUND-SAMPLES))
        ^js costs (js/Object.assign #js {} DEFAULT-COSTS (.-costs options))
        max-nodes (option options "maxNodes" 200000)
        ^js avoid (.-avoid options)
        ^js limits (.-limits options)
        margin (option options "margin" 64)
        y-margin (option options "yMargin" 48)
        y-below (option options "yBelow" y-margin)
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
        slots (next-pow2 (* cap 2))
        risk-scale (let [rw (option options "riskWeight" 2)]
                     (if (zero? rw) 1 (/ (option options "damageWeight" rw) rw)))
        ^js dangers (danger-array (.-dangers options) risk-scale)
        n-dangers (if (some? dangers) (/ (.-length dangers) DANGER-STRIDE) 0)
        ^js dbox (danger-box dangers)
        ^js dark (.-dark options)
        ^js tolls (.-tolls options)
        dark-at (when (some? dark) (.-at dark))
        ^js dark-shares (when (some? dark) (.concat #js [0 (or-else (.-factor dark) 1)] (or-else (.-water dark) #js [])))
        ^js unknown (.-unknown options)
        unk (some? unknown)
        see-lit (when unk (.-seeLit unknown))
        rw (option options "riskWeight" 2)
        lethal-s (lethal-seconds rw (option options "damageWeight" rw) (* risk-scale (option options "dangerCap" 4)) (if (some? dark-shares) (apply max (array-seq dark-shares)) 0)
                                 (when (some? tolls) (.-cells tolls)) (if (some? avoid) (or-else (.-factor avoid) 0) 0)
                                 (unknown-factor unknown costs) (.-drownPrices options))]
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
     (option options "goalFlood" 4000) (option options "floodAfter" 3000) (option options "preFlood" 256)
     (reach-of options from goal)
     ;; avoid
     (some? avoid) (if (some? avoid) (.-kinds avoid) 0) (if (some? avoid) (.-cells avoid) nil) (if (some? avoid) (.-factor avoid) 0)
     ;; limits
     (if (some? limits) (or-else (.-kinds limits) 0) 0) (if (some? limits) (.-gap limits) nil) (if (some? limits) (.-corner limits) nil)
     ;; costs
     (unchecked-get costs "climbUp") (unchecked-get costs "climbDown") (unchecked-get costs "jumpClimb") (unchecked-get costs "open")
     (unchecked-get costs "openRedstone") (unchecked-get costs "openLever") (unchecked-get costs "openPlate") (unchecked-get costs "besideMagmaColumn")
     (unchecked-get costs "swimH") (js/Math.min (unchecked-get costs "swimSubH") (unchecked-get costs "swimH")) (unchecked-get costs "swimUp") (unchecked-get costs "swimDown")
     (unchecked-get costs "exit") (unchecked-get costs "current") (unchecked-get costs "bubbleUp") (unchecked-get costs "bubbleDown")
     (unchecked-get costs "airSupply") (air-limit costs) (+ (unchecked-get costs "airSupply") (air-grace costs)) (unchecked-get costs "airDrain") (unchecked-get costs "maxWaterDrop")
     (unchecked-get costs "dripleaf") (unchecked-get costs "dripleafRisk") (to-air? snapshot query options) (unchecked-get costs "dropFactor")
     (unchecked-get costs "walkS") (unchecked-get costs "sprintS")
     ;; search box
     (- (aget bounds 0) margin) (+ (aget bounds 1) margin)
     (- (aget bounds 2) margin) (+ (aget bounds 3) margin)
     (- y-low y-below) (+ y-high y-margin)
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
     ;; support touch enter-risk enter-slow enter-extra ty gap-y quiet open-mode allow-shut gap-seen air-seen enters-shut
     0 0 0 0 0 0 0 false false false false false false
     ;; move-air move-peak move-water move-drown move-open open-lists activators view
     0 0 0 0 0 #js [] (js/Map.) nil
     ;; edge-mode seen-edges door-arrival door-here door-through open-x open-y open-z
     0 nil nil nil false 0 0 0
     ;; tight cells
     (js/Float64Array. TABLE) (js/Uint8Array. TABLE) (js/Float64Array. TABLE) (js/Uint8Array. TABLE) (js/Map.) (js/Set.) (js/Int8Array. REGIONS)
     (js/Float64Array. REGIONS) (js/Int16Array. REGIONS) nil nil
     0 0 0 0
     ;; the goal flood: flooding fx fy fz fr hit flooded pre-flooded flood-pending leaked
     false 0 0 0 -1 false 0 0
     (and near (pos? (option options "goalFlood" 4000))) false
     ;; flood-memo flood-base lf-imported verifying
     (when (and near (pos? (option options "goalFlood" 4000))) (.-goalFloodMemo options)) 0 0 false
     ;; flood-moves flood-out flood-h-keys flood-h-vals flood-target
     (js/Map.) nil nil nil 0
     ;; progress: started finished reason over-budget boxed goal-node best-node
     false false nil false false -1 -1
     ;; best-distance start-distance expanded t0 elapsed start-h start-slow
     js/Infinity 0 0 (js/performance.now) 0 -1 0
     ;; returnable held replaying
     (true? (option options "returnable" false)) #js [] false
     ;; lf-seen lf-queue lf-head lf-budget lf-active lf-open lf-seed-open lf-end cut-off limit-refused
     nil nil 0 0 false false false false false false
     ;; known-cells known-new known-edges edges-new
     (.-knownCells options) nil (.-knownEdges options) nil
     ;; searched-out
     false
     ;; stop-at-edge edge-node sweep-end sweep-flags
     (and goal-unloaded (true? (option options "stopAtEdge" false))) -1 0 0
     ;; dangers n-dangers danger-cap dbx0 dbx1 dby0 dby1 dbz0 dbz1
     dangers n-dangers (* risk-scale (option options "dangerCap" 4))
     (aget dbox 0) (aget dbox 1) (aget dbox 2) (aget dbox 3) (aget dbox 4) (aget dbox 5)
     ;; dark-at dark-shares dark-keys dark-flags darks
     dark-at dark-shares
     (when (some? dark-at) (js/Float64Array. TABLE)) (when (some? dark-at) (js/Uint8Array. TABLE)) (js/Float64Array. cap)
     ;; tolls edge-tolls
     (when (some? tolls) (.-cells tolls)) (when (some? tolls) (.-edges tolls))
     ;; damage-budget damage-weight fall-factor land-factors land-seen
     (option options "damageBudget" js/Infinity) (option options "damageWeight" (option options "riskWeight" 2)) (option options "fallFactor" 1)
     (.-landing options) (.-landingSeen options)
     ;; dmgs move-dmg enter-dmg cur-dmg damage-refused
     (js/Float64Array. cap) 0 0 0 false
     ;; drowns cur-drown drown-prices lethal-hp air-known
     (js/Float64Array. cap) 0 (.-drownPrices options) (option options "health" 20) (.-airSeen options) (unchecked-get costs "drownSlack")
     (not (pos? (or-else (unchecked-get costs "lethalAir") 0))) false
     ;; drown-runs drown-closed move-run move-closed lethal-s
     (js/Float64Array. cap) (js/Float64Array. cap) 0 0 lethal-s
     ;; unk-id unk-floor-id unk-cell see-depth see-lit see-lit-depth light-depth peek-s aim-s light-cost light-ok torch-base missed no-light
     (if unk (or-else (.-id unknown) (first-id table "void_air")) -1)
     (if unk (or-else (.-floorId unknown) (first-id table "stone")) 0)
     (if unk (or-else (.-cell unknown) 0) 0)
     (if unk (or-else (.-seeDepth unknown) 0) 0) see-lit
     (if unk (or-else (.-seeLitDepth unknown) unk-defaults/SEE-LIT-DEPTH) 0) (if unk (or-else (.-lightDepth unknown) unk-defaults/LIGHT-DEPTH) 0)
     (if unk (or-else (.-peekS unknown) 0) 0) (if unk (or-else (.-aimS unknown) 0) 0)
     (if unk (or-else (.-lightCost unknown) -1) -1) (when unk (.-lightOk unknown)) (when unk (.-torchBase unknown))
     (when unk (.-missed unknown)) (when unk (.-noLight unknown))
     ;; see-keys see-flags
     (when (some? see-lit) (js/Float64Array. TABLE)) (when (some? see-lit) (js/Uint8Array. TABLE))
     ;; slip-keys slip-flags
     (js/Float64Array. TABLE) (js/Uint8Array. TABLE)
     ;; move-unk move-light lights dark-refused unseen-refused dark-best-x dark-best-y dark-best-z dark-best-d
     0 0 (js/Uint8Array. cap) false false 0 0 0 js/Infinity
     ;; rivals
     (js/Int32Array. cap)
     ;; goal-seen
     (.-goalSeen options)
     (unchecked-get costs "descentS")
     (if (pos? rw) (/ (* (unchecked-get costs "gapMiss") lethal-s) rw) 0)
     ;; unk-changed unk-death
     (when unk (.-changed unknown))
     (if (and unk (some? (.-deathHp unknown))) (.-deathHp unknown) (if (pos? rw) (/ lethal-s rw) 0)))))

;; the body's box reaches this far from its centre in x and z: the client physics' width, which decides what holds it up
(def HITBOX-HALF constants/body-half-width)

(defn- overlap [lo hi c] (- (js/Math.min hi (inc c)) (js/Math.max lo c)))

(defn- hitbox-cells
  "The cells [x z] other than (fx fz) that the body's hitbox at px pz overlaps, most overlap first, then lower x, then
  lower z."
  [fx fz px pz]
  (let [x0 (- px HITBOX-HALF) x1 (+ px HITBOX-HALF) z0 (- pz HITBOX-HALF) z1 (+ pz HITBOX-HALF)]
    (->> (for [x (range (js/Math.floor x0) (inc (js/Math.floor x1)))
               z (range (js/Math.floor z0) (inc (js/Math.floor z1)))
               :let [area (* (overlap x0 x1 x) (overlap z0 z1 z))]
               :when (and (pos? area) (not (and (== x fx) (== z fz))))]
           [area x z])
         (sort-by (fn [[area x z]] [(- area) x z]))
         (map (fn [[_ x z]] [x z])))))

(defn- start-query
  "query with its start cell moved for a body on the edge of a block: when from (the floored body position) is no
  place to stand but from.px/pz is given, the first cell the body's box (HITBOX-HALF) overlaps (hitbox-cells) that stands with
  the feet at from.py (when given, within 1/16); failing that, a cell the box overlaps whose stand is up to half a block
  under the feet (the low half of a stairs block: the body steps down off it). A body whose feet are above the floored
  cell's floor (a stair, whose stand the search puts in the cell above it) otherwise starts from the cell above when that stands;
  one whose feet are up to 1/16 over the stand of the cell below (a short floor) starts from the cell below. Unchanged when the floored
  cell stands or no cell does.
  search: a search of query, initialised."
  [^Search search ^js query]
  (let [^js from (.-from query)
        px (.-px from) pz (.-pz from) py (.-py from)
        fx (.-x from) fy (.-y from) fz (.-z from)
        edge (when (and (some? px) (some? pz))
               (first (filter (fn [[x z]]
                                (let [h (.nodeH search x fy z)]
                                  (and (>= h 0) (or (nil? py) (<= (js/Math.abs (- (+ fy (/ h WHOLE)) py)) (/ 1 WHOLE))))))
                              (hitbox-cells fx fz px pz))))
        ;; a body on the low half of a stairs block (its box reaches the neighbour cell) steps down off it
        step-off (when (and (some? px) (some? pz) (some? py))
                   (first (filter (fn [[x z]]
                                    (let [h (.nodeH search x fy z)
                                          d (- py (+ fy (/ h WHOLE)))]
                                      (and (>= h 0) (<= (- (/ 1 WHOLE)) d 0.5))))
                                  (hitbox-cells fx fz px pz))))
        above? (and (some? py) (> py (+ fy (/ 1 WHOLE))) (>= (.nodeH search fx (inc fy) fz) 0))
        ;; feet a hair above a short floor (farmland, a path: 15/16) that a neighbouring full block level with the cell's
        ;; floor held up, so the floored cell has nothing under it but the cell below stands
        below? (and (some? py) (let [h (.nodeH search fx (dec fy) fz)]
                                 (and (>= h 0) (<= 0 (- py (+ (dec fy) (/ h WHOLE))) (/ 1 WHOLE)))))]
    (cond
      (>= (.-start-h search) 0) query
      (some? edge) (js/Object.assign #js {} query #js {:from (js/Object.assign #js {} from #js {:x (first edge) :z (second edge)})})
      (some? step-off) (js/Object.assign #js {} query #js {:from (js/Object.assign #js {} from #js {:x (first step-off) :z (second step-off)})})
      above? (js/Object.assign #js {} query #js {:from (js/Object.assign #js {} from #js {:y (inc fy)})})
      below? (js/Object.assign #js {} query #js {:from (js/Object.assign #js {} from #js {:y (dec fy)})})
      :else query)))

(defn- with-start-air
  "search with its start node (node 0) having used options.costs.airUsed seconds of air (a plan made mid-dive)."
  ^Search [^Search search ^js options]
  (let [used (or-else (some-> (.-costs options) (unchecked-get "airUsed")) 0)]
    (aset (.-airs search) 0 used)
    (aset (.-peaks search) 0 used)
    search))

(defn- new-search
  "The search of query, its start moved off the edge of a block (start-query); initialised (init again is harmless)."
  ^Search [^js snapshot ^js query ^js options]
  (let [^Search search (search-from snapshot query options)]
    (.init search)
    (let [q (start-query search query)]
      (with-start-air (if (identical? q query) search (search-from snapshot q options)) options))))

(defn- clean-options
  "The options of the returnable search behind a one-way step of search: options.returnable, no goal flood, no stop at
  the loaded edge (stopAtEdge), and no more nodes than the search itself made. It only finds where a partial plan ends. With no flood to prove a walled-in
  goal it would otherwise search the whole box."
  [^Search search options]
  (js/Object.assign #js {} options #js {:returnable true :goalFlood 0 :stopAtEdge false
                                        :maxNodes (js/Math.min (option options "maxNodes" 200000) (js/Math.max 1 (.-n-nodes search)))}))

(defn- result-of
  "The result of a finished search. When the path to its nearest node holds a one-way step, a second search
  (clean-options) supplies the partial end."
  [^Search search snapshot query options]
  (.settle search)
  (let [node (.oneWayNode search)]
    (if (neg? node)
      (.resultFrom search -1 nil)
      (let [^Search clean (new-search snapshot query (clean-options search options))]
        (.init clean)
        (.step clean js/Infinity)
        (.resultFrom search node (.nearest clean))))))

(defn create-search
  "A search to run in steps: #js {step(n) result() nearest()}. options.table and options.space are required."
  [snapshot query options]
  (let [search (new-search snapshot query options)]
    (.init search)
    #js {:step (fn [max-expansions] (.step search max-expansions))
         :result (fn [] (result-of search snapshot query options))
         :nearest (fn [] (.nearest search))}))

(defn- ^boolean sampled-box?
  "True when result r ended on the search box while goal-bounds read only a sample of the query's column goals."
  [^js r ^js query options]
  (and (identical? (.-reason r) "box")
       (some? (.-goals query))
       (> (.-length (.-goals query)) (option options "groundSamples" GROUND-SAMPLES))))

(defn- exact-options [options] (js/Object.assign #js {} options #js {:groundSamples js/Infinity}))

(defn- plan-once [snapshot query options]
  (let [search (new-search snapshot query options)]
    (.init search)
    (.step search js/Infinity)
    (result-of search snapshot query options)))

(defn plan [snapshot query options]
  (let [r (plan-once snapshot query options)]
    (if (sampled-box? r query options)
      (plan-once snapshot query (exact-options options))
      r)))

(defn- create-plan-once
  "plan in slices, for a caller that yields to the event loop between them: #js {step result progress}.
  step(n) runs at most about n expansions (each newly flooded cell of the goal flood counts as one). It is true once
  the plan is ready, and result() is then what plan answers. The returnable search behind a one-way step (result-of)
  runs in the same slices. progress() is where an unfinished search has got to (Search.progress), nil once the
  search itself is over. endSweep() ends a search sweeping after its edge stop (Search.endSweep): true when it did."
  [snapshot query options]
  (let [search (new-search snapshot query options)
        clean (volatile! nil)
        node (volatile! -1)
        ready (volatile! false)]
    (.init search)
    #js {:step (fn [max-expansions]
                 (cond
                   ^boolean @ready true
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
         ;; ends a search sweeping after its edge stop now (Search.endSweep), for a caller out of budget: true when it did
         :endSweep (fn [] (if (some? @clean) (.endSweep ^Search @clean) (.endSweep search)))
         ;; while the search itself is not over: where it has got to (Search.progress), else nil
         :progress (fn [] (when-not ^boolean (.-finished search) (.progress search)))}))

(defn create-plan
  "plan in slices (create-plan-once); a sampled search that ends on the box (sampled-box?) restarts over every column."
  [snapshot query options]
  (let [inner (volatile! (create-plan-once snapshot query options))
        exact (volatile! false)]
    #js {:step (fn [max-expansions]
                 (let [^js p @inner
                       ^boolean done (.step p max-expansions)]
                   (if (and done (not ^boolean @exact) (sampled-box? (.result p) query options))
                     (do (vreset! exact true)
                         (vreset! inner (create-plan-once snapshot query (exact-options options)))
                         false)
                     done)))
         :result (fn [] (.result ^js @inner))
         :progress (fn [] (.progress ^js @inner))
         :endSweep (fn [] (.endSweep ^js @inner))}))
