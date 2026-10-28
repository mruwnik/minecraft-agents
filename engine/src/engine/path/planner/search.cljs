(ns engine.path.planner.search
  "The planner's search state: the fields of one Search object (engine.path.planner-tuned builds it). Its methods
   are added by part: engine.path.planner.world, nodes, tight, water, moves, doors, flood, run and results.")

(set! *warn-on-infer* true)

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
   c-climb-up c-climb-down c-jump-climb c-open c-open-redstone c-open-lever c-open-plate c-beside-magma c-swim-h c-swim-sub-h c-swim-up c-swim-down
   c-exit c-current c-bubble-up c-bubble-down c-air-supply c-air-limit c-air-drown c-air-drain c-max-water-drop c-dripleaf c-dripleaf-risk
   ^boolean to-air ; a plan started under water to a goal with its head in air: no swim is refused for air, it drowns (drowns)
   c-drop-factor ; a drop's fall seconds and fall damage are scaled by it (0: free)
   c-walk-s c-sprint-s ; seconds per block walked, and per block of a gap jump (the gait: walking, sprinting, sneaking)
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
   ^:mutable ^boolean gap-seen ; a ladder was refused because the feet would leave it at a gap
   ^:mutable ^boolean air-seen ; a move with the head in water was refused for lack of air
   ^:mutable ^boolean enters-shut ; the last enterCell was a shut trapdoor
   ;; what the swim move being made adds to the node (move-drown: the hp its drowning seconds cost, see airStep), and
   ;; what the move being made opens
   ^:mutable move-air ^:mutable move-peak ^:mutable move-water ^:mutable move-drown ^:mutable move-open
   ^js open-lists
   ^js activators
   ^:mutable ^js view ; what the free-space masks read: a closed wooden trapdoor over a ladder reads as air
   ;; where moves go: 0 into the search (or the flood's probe), 2 recording the edges the first pass finds, 3 the opening pass
   ^:mutable edge-mode ^:mutable ^js seen-edges ^:mutable ^js door-arrival ^:mutable ^js door-here
   ^:mutable ^boolean door-through ^:mutable open-x ^:mutable open-y ^:mutable open-z
   ;; tight cells: direct-mapped caches (keys stored +1 so zeroed memory reads as empty), masks per (cell, height)
   column-keys column-flags cell-keys cell-flags ^js mask-cache ^js tight-seen ^js pick ^js best-d ^js best-at
   ^:mutable ^js surface-cache ^:mutable ^js dive-cache
   ^:mutable masks ^:mutable tight-masks ^:mutable regions-seen ^:mutable mask-ms
   ;; the goal flood: moves go to the probe instead of the search while it runs; fr the probed cell's region (-1 any)
   ^:mutable ^boolean flooding ^:mutable fx ^:mutable fy ^:mutable fz ^:mutable fr ^:mutable ^boolean hit ^:mutable flooded ^:mutable pre-flooded
   ^:mutable ^boolean flood-pending ^:mutable ^boolean leaked
   ;; the remembered aging cells the flood's proof rests on (cell-key) and the chance none of them changed (restOn)
   ^:mutable ^js rest-cells ^:mutable rest-p
   ;; the late flood kept over the searches of one goal (options.goalFloodMemo, see keepFlood; nil: none), the expansions
   ;; the earlier searches of that goal made (a late flood is due by all of them), the cells taken over from them, and
   ;; whether a fresh flood is checking their enclosed answer (progress is nil meanwhile, so the caller does not walk)
   ^js flood-memo ^:mutable flood-base ^:mutable lf-imported ^:mutable ^boolean verifying
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
   ^:mutable ^boolean cut-off ; the flood proved the goal unreachable with a cliff edge in its region or an edge to drop off at its level: cut off, not walled in
   ;; the walker's limits (options.limits) refused some move: a search without them could have gone further
   ^:mutable ^boolean limit-refused
   ;; land a caller's earlier searches toward the same goal searched to the end (options.knownCells, a Set of knownKey,
   ;; nil for none; see frontierNode), and the cells this search adds to it once it ends exhausted (a Set, see frontierOf)
   ^js known-cells ^:mutable ^js known-new
   ;; options.knownEdges (nil for none), and the cells of this search's edge nodes not in known-cells (an array while
   ;; known-new is a Set)
   ^js known-edges ^:mutable ^js edges-new
   ;; whether frontierNode refused a frontier in known land because no edge is left open (result searchedOut)
   ^:mutable ^boolean searched-out
   ;; options.stopAtEdge with the goal unloaded: the first node the search expands at the loaded edge (edgeStop) is
   ;; edge-node (-1: none yet), its frontier; it ends once it has expanded sweep-end nodes (see expandNext) with the
   ;; refusals it had met at edge-node (sweep-flags, see refusalFlags)
   ^boolean stop-at-edge ^:mutable edge-node ^:mutable sweep-end ^:mutable sweep-flags
   ;; known dangers (options.dangers, see dangerRisk): x y z close radius rate each (nil: none), their count, the most risk
   ;; a second of walking takes from all of them (options.dangerCap), and the box round every danger's radius
   ^js dangers n-dangers danger-cap dbx0 dbx1 dby0 dby1 dbz0 dbz1
   ;; options.dark (see darkOf): the test of a cell (nil: none), the extra cost of a cell of each class the test gives as
   ;; a share of its own seconds (0 lit, 1 dark, then the water classes), a direct-mapped cache of the test (keys
   ;; stored +1, flags the class +1) and the seconds of dark cells on the path to each node
   ^js dark-at ^js dark-shares ^js dark-keys ^js dark-flags ^:mutable darks
   ;; options.tolls.cells (nil: none): a Map of cell-key to the factor of the cell's own seconds it costs more (see tollOf)
   ^js tolls
   ;; options.tolls.edges (nil: none): a Map of the from cell's cell-key to a flat array of [move, to cell-key or -1 (any
   ;; to cell), seconds] a move out of it costs more (see edgeToll)
   ^js edge-tolls
   ;; the damage budget (options.damageBudget hp, nil-free: Infinity for none), the price of an hp of certain damage in seconds
   ;; (options.damageWeight), the factor of a fall's damage (options.fallFactor) and options.landing (Map of state id to the factor
   ;; of the fall damage onto that block, negative: a bounce, no damage but settle seconds; nil: 1 for every block), and
   ;; options.landingSeen (fn x y z: the body sees that block now; nil: every block counts as seen, see bouncePad)
   damage-budget damage-weight fall-factor ^js land-factors ^js land-seen
   ;; certain damage so far to each node; what the move being made adds (set around its edge, 0 otherwise); what the last landing
   ;; adds (plants touched, a hurting floor); the total of the node being recorded; a move was refused for the budget
   ^:mutable dmgs ^:mutable move-dmg ^:mutable enter-dmg ^:mutable cur-dmg ^:mutable ^boolean damage-refused
   ;; drowning (to-air only): its hp so far to each node, the hp of the node being recorded, options.drownPrices (the seconds
   ;; the first k hp cost, nil: damage-weight each) and options.health (drowning to it is lethal, see drownCost), and
   ;; options.airSeen (fn x y z: the body has seen or felt that block; nil: every block, see breathes); costs.drownSlack
   ;; (seconds of margin in the count of drowning hits, drowned-hp)
   ^:mutable drowns ^:mutable cur-drown ^js drown-prices lethal-hp ^js air-known drown-slack
   ;; costs.lethalAir off (the default): a swim whose drowning reaches lethal-hp is refused (lethal-seen, reason "air-lethal")
   ^boolean no-lethal ^:mutable ^boolean lethal-seen
   ;; each node's drowning hp in its open air-out (the head under since the last breath) and the counted hp (drowned-hp) of
   ;; the air-outs closed before it; what the move being made leaves (set by airStep); the seconds a lethal plan costs
   ;; (LETHAL-S times the dearest second the caller's prices allow, see drownCost)
   ^:mutable drown-runs ^:mutable drown-closed ^:mutable move-run ^:mutable move-closed lethal-s
   ;; options.unknown (unk-id -1: off, see unseenRefuses): the state id of a cell the body has not seen (void_air: air to
   ;; the body), the id a phantom floor (an unseen floor stood on) reads as, the prices (a share of the seconds of a move
   ;; with an unseen feet, head or floor cell; the seconds of a peek over a drop onto a phantom floor and of an aim at a
   ;; level or up one), how deep a drop the walk can see from its peek (see-depth) and the caller's test of a lit landing
   ;; (see-lit, fn x y z -> 1 or 0, nil: none; cached in see-keys and see-flags as darkOf) seen to a drop of see-lit-depth, how deep a drop a torch at the takeoff lights (light-depth), a torch's price (light-cost, -1:
   ;; no torches), its test of a takeoff (light-ok, fn x y z, nil: every one), the floors a torch stands on (torch-base, by
   ;; state id), and the floor cells the walk could not see and the takeoffs where a torch failed (Sets of cell-key, nil: none)
   unk-id unk-floor-id unk-cell see-depth ^js see-lit see-lit-depth light-depth peek-s aim-s light-cost ^js light-ok ^js torch-base
   ^js missed ^js no-light ^js see-keys ^js see-flags ^js slip-keys ^js slip-flags
   ;; what the move being made adds (set by unseenRefuses): seconds, and 1 when it lights its landing; each node's light flag;
   ;; a phantom floor was refused for want of sight (dark-refused; the refused stand nearest the goal, dark-best-*) or because
   ;; the walk could not stop to check it first (unseen-refused)
   ^:mutable move-unk ^:mutable move-light ^:mutable lights ^:mutable ^boolean dark-refused ^:mutable ^boolean unseen-refused
   ^:mutable dark-best-x ^:mutable dark-best-y ^:mutable dark-best-z ^:mutable dark-best-d
   ;; each node's older record of the same key (a rival, see consider) plus 1, 0 for none: the records of one cell, newest first
   ^:mutable rivals
   ;; options.goalSeen (fn x y z; nil: every cell): a cell in the goal's area is reached only when it answers true
   ^js goal-seen
   ;; costs.descentS: seconds a block of descent adds to a move's cost
   c-descent
   ;; hp of risk a gap jump that falls short to its death adds: costs.gapMiss (the chance) times lethal-s, over riskWeight
   gap-death
   ;; options.unknown.changed (fn x y z: the chance a remembered cell changed since it was seen; nil: none did) and the hp
   ;; of risk a death weighs (options.unknown.deathHp, else lethal-s over riskWeight)
   ^js unk-changed unk-death])
