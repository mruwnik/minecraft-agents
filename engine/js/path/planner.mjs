// Pure A* over a snapshot of block state ids. A node is a feet cell; its stand height h (1/16 block above the cell's
// floor) comes from the collision under or in it, so slabs, carpets and snow are half steps, not cell-sized ones.
//
// Standable, as in vanilla with its 0.6 step height: the cell's own collision tops at 1..15/16 from the floor (bed, snow
// layers, slab, chest, soul sand, farmland...): then h = top and the node is that cell; or the cell is free and the one below
// tops at 16/16 (h = 0) or above (a closed gate: h = top - 16). Fences, walls, panes, bars and bamboo (NARROW) are never a
// floor and never walked through. Between nodes a rise of up to STEP is a walk, up to JUMP_UP a jump. A bottom straight stairs
// block entered in its climbing direction is a walk up its whole block, as vanilla's step-up makes it. The body's column,
// 29/16 tall from the stand height, must have no collision, no fluid and no AVOID hazard. A portal (nether, end, gateway) is
// never entered (feet or head, or crossed in a gap jump) unless the cell is within the goal: the goal is the portal.
//
// Climbing: a feet cell holding a climbable (ladder, vines, scaffolding; an open trapdoor directly above a ladder of its facing;
// a closed wooden trapdoor above a ladder, which costs an OPEN to enter) is a climb node: standable with no floor, h = 0.
// Moves between climb cells are vertical (CLIMB_UP / CLIMB_DOWN, OPEN into a closed trapdoor), a standable cell under a
// climbable is left by a JUMP_CLIMB, and the top of a ladder is left sideways like any cell. Up through a gap in a ladder is
// refused (the feet leave the ladder), down through one is a short fall caught below. Every cost the policy might want to
// change is in DEFAULT_COSTS, overridable by options.costs.
//
// Doors, gates, trapdoors (see expandAt): where a move needs a closed hand-openable one (wood, copper) open, it is found in a second
// pass over the expansion with those blocks open, costs.open each, and the step says what it opens (step.opens). An iron door only
// when a button or lever lies within reach on the body's side, or a plate in the cell in front: costs.openRedstone.
//
// Water (vanilla 26.1, as modelled here): a feet cell holding water, a bubble column or a no-collision waterlogged block (kelp,
// seagrass) is a SWIM node, h = 0, no floor needed; its head cell (feet + 1) must be water or open air. Moves: SWIM sideways
// (cardinal; a diagonal costs sqrt 2 and follows the walking side rule), SWIM_UP and SWIM_DOWN a cell, EXIT onto a bank. A floating
// body gets out only onto land whose stand height is at most the top water cell + 1 + 1/16 (flush with the surface, costs.exit):
// measured live, land one higher is out of reach (0/5). A body standing on a floor in water 1 deep is not floating: it walks and
// jumps out by the ordinary rules. A route never ends on a magma block or in a magma bubble column, a standing node beside a magma
// column costs costs.besideMagmaColumn risk, and the step after leaving a bubble column sideways is not into a magma column unless
// the goal is below it. Water that is not a source (level != 0, a waterfall included) adds costs.current per block entered; a bubble
// column lifts (drag=false, soul sand: up at costs.bubbleUp, down refused) or drags (drag=true, magma: down at
// costs.bubbleDown, up refused). A body breathes 15 s: seconds with the head in water (not in a bubble column) accumulate
// along the path in `airs` (the current's extra is a penalty, not time, so it does not count), a head out of water or in a
// bubble column refills at once, and a move that would pass costs.airLimit is refused (reason 'air' when that ends the search).
// The cost therefore depends on the path, not only the node: a node already reached keeps its record, and an arrival with at
// least AIR_STEP seconds less air used (or a cheaper one with that much more air used) is added as a rival record that
// replaces the first in the hash, so no record is ever overwritten under the nodes that grew from it. A drop of up to
// costs.maxWaterDrop into water through a clear column is a DROP with no fall damage (vanilla resets the fall in water, even one
// block deep); a drop onto land is still limited by maxDrop. Gap jumps never land in water or cross it. Swimming through or
// into a tight cell (a lily pad over the water, say) goes by masks like walking, except a diagonal, which never touches one.
// A big dripleaf leaf holds the body (it tilts after about a second): costs.dripleaf extra seconds and costs.dripleafRisk hp of
// risk for each leaf entered, the chance of the fall.
//
// Tight cells: where a block that leaves part of its cell empty (fence, bamboo, cocoa, wall, ladder...: `partial`) lies within
// one cell of the body, rows y-1..y+2, whether the 0.62 wide body fits depends on where in the cell it stands. There the
// cell is not one node but one per region of the free-position mask (space.mjs: 17x17 positions at 1/16, 4-connected
// regions), node key (x, y, z, region), position the region's point nearest the cell centre. A cardinal move joins region
// RA of cell A to RB of cell B when a point of their shared boundary is free in both masks (body at the higher of the two
// stand heights) and lies in RA and in RB; that point is the crossing. Diagonals touching a tight cell are refused (the
// cardinal chain covers them). A drop out of or into a tight cell falls straight down from the crossing point: it must be free in
// the takeoff mask, in the neighbour column at takeoff height and in the landing cell's mask, and the landing node is that
// point's region. Gap jumps out of or into a tight cell are refused. A search box (margin, yMargin) bounds
// the nodes, and a backward flood from the goal reports a goal nothing can reach: a small one (preFlood cells) before the first expansion,
// the full one (goalFlood cells) once the search has spent floodAfter expansions
// (it declines to when it meets water: a drop into water starts further up than the flood looks).
import { UNLOADED } from './snapshot.mjs'
import { defaultStateTable, OPEN, OPENABLE, WATER, LAVA, NARROW, HAZARD_AVOID, DAMAGE_STAND, DAMAGE_TOUCH, SLOW, PORTAL, CLIMB_INSIDE, CLIMB_TRAP_SHUT, LADDER, VINES, SCAFFOLDING, OPEN_REDSTONE, KIND_DOOR, KIND_GATE, ACT_BUTTON, ACT_LEVER, ACT_PLATE } from './blocks.mjs'
import { boxesNear, freeMask, labelRegions, GRID } from './space.mjs'

export const MOVE = { START: 0, WALK: 1, DIAGONAL: 2, JUMP: 3, DROP: 4, GAP: 5, CORNER: 6, CLIMB_UP: 7, CLIMB_DOWN: 8, JUMP_CLIMB: 9, OPEN: 10, SWIM: 11, SWIM_UP: 12, SWIM_DOWN: 13, EXIT: 14 }

// seconds for the moves climbing adds: per block climbed up and down, a jump from the floor into a ladder one block up, and
// opening a door, gate or trapdoor by hand (openRedstone: an iron door by a button or lever; openPlate: by a plate, which the body
// steps on anyway) and besideMagmaColumn, the risk of standing beside a magma bubble column; water, in seconds: swimming a block
// sideways, up, down, getting out onto a bank flush with the surface, the extra for a block of flowing water,
// a block in a bubble column up (soul sand) and down (magma), the breath (supply and the margin kept under it), the highest
// drop into water and, for a big dripleaf leaf, the extra seconds and the hp of risk
export const DEFAULT_COSTS = {
  climbUp: 0.43, climbDown: 0.33, jumpClimb: 0.5, open: 1.0, openRedstone: 1.5, openPlate: 0, besideMagmaColumn: 1,
  swimH: 0.5, swimUp: 0.3, swimDown: 0.35, exit: 0.6, current: 0.3, bubbleUp: 0.08, bubbleDown: 0.12,
  airSupply: 15, airLimit: 12, maxWaterDrop: 64, dripleaf: 0.2, dripleafRisk: 0.5
}

const BODY = 29 // 1.8 blocks in 1/16, rounded up
const STEP = 9 // 0.6 blocks
const JUMP_UP = 20 // 1.25 blocks
const ARC = 32 // headroom over a gap: feet + 2
const ARC_UP = 40 // headroom over a gap whose landing is one block higher: feet + 2.5
const WALK_S = 1 / 4.317 // seconds per block
const SPRINT_S = 1 / 5.612
const JUMP_S = 0.35 // a jump up costs this much more than the walk it replaces
const GAP_S = 0.5 // a gap jump's run-up and landing, on top of the sprint over its length
const GAP_UP_S = 0.3 // a gap jump landing one block higher costs this much more than a level one
const TIGHT_S = 0.1 // careful walking: each tight cell entered costs this much more than a plain step
const CORNER_S = 0.15 // a diagonal slid along a blocked corner: slower than a straight one
const SLOW_EXTRA = 0.75 // walking time grows by this much of itself per slow end of a move: both ends soul sand is x2.5
const LAVA_ADJACENT = 0.5 // hp of risk for a step with lava beside the feet
const FREE_FALL = 3
const EXIT_SLACK = 1 // 1/16: a floating body exits onto land up to this over the water's top face
const SQRT2 = Math.SQRT2
const OCTILE_SLACK = 1.0824 // octile length of a vector of length r is at most this times r: keeps the range term admissible
const SPAN = 4096 // nodes further than 2048 blocks from the start in x or z are not searched
const HALF = 2048
const MIN_CLOSER = 2 // an exhausted search is a partial result only when it got this many blocks closer
const WHOLE = 16 // a full block in 1/16
const BODY_BLOCKS = 1.8
const REGIONS = 16 // regions of one cell that can be nodes (4 bits of the key); a cell never has this many in practice
const AIR_STEP = 1 // seconds of air that make an arrival at a node already reached worth a record of its own
const CENTRE = { px: 8, pz: 8 } // the representative point of an ordinary cell, in 1/16

// What a node knows beyond its cell, in one Uint32 (0 for an ordinary cell reached from an ordinary one): bits 0-3 region,
// bit 4 set for a tight cell with its representative point px (5-9) and pz (10-14) in 1/16, bit 15 set with the crossing
// point the move in came by, relative to the cell in 1/16: x (16-20), z (21-25).
const packShape = (region, tight, px, pz, crossed, crossX, crossZ) =>
  region | (tight ? 16 | px << 5 | pz << 10 : 0) | (crossed ? 1 << 15 | crossX << 16 | crossZ << 21 : 0)

const CLIMBS = new Set([MOVE.CLIMB_UP, MOVE.CLIMB_DOWN, MOVE.JUMP_CLIMB, MOVE.OPEN]) // moves that climb a block

const CARDINAL = [[1, 0], [-1, 0], [0, 1], [0, -1]]
const DIAGONAL = [[1, 1], [1, -1], [-1, 1], [-1, -1]]
const AROUND = [...CARDINAL, ...DIAGONAL]

const nextPow2 = n => 2 ** Math.ceil(Math.log2(Math.max(2, n)))
const fallDamage = fall16 => Math.max(0, Math.ceil(fall16 / 16 - FREE_FALL))

export function createSearch (snapshot, query, options = {}) {
  const {
    maxNodes = 200000, maxDrop = 3, weight = 1, riskWeight = 2,
    table = defaultStateTable(),
    goalFlood = 4000, floodAfter = 3000, preFlood = 24, margin = 64, yMargin = 48,
    returnable = false // plan no step the body cannot undo (see isOneWay): the search a partial end is taken from
  } = options
  const costs = { ...DEFAULT_COSTS, ...options.costs }
  const { top, base, kind, hazard, stairUp, partial, climb, climbName, facing, floor, special, flowing, bubble, magma, dripleaf, farmland, openable, openState, openKind, doorHalf, activator, attach } = table
  const { minY } = snapshot
  const rawAt = snapshot.stateAt
  // What the body sees: in the opening pass of an expansion (see expandAt) a closed door, gate or trapdoor reads as open, so the
  // move is judged with the block as it will be once opened; rawAt is the block as it stands.
  let openMode = false
  const stateAt = (x, y, z) => {
    const id = rawAt(x, y, z)
    return openMode && openable[id] > 0 ? openState[id] : id
  }
  const { from, goal } = query
  const goalRange = goal.range ?? 0
  const slack = OCTILE_SLACK * goalRange

  // ---- the world, as the body sees it ----

  // does the climbable state `id` (climb[id] = cl) at x,y,z make its cell one the body climbs in?
  const climbCell = (cl, id, x, y, z) => {
    if (cl === CLIMB_INSIDE) return true
    const below = rawAt(x, y - 1, z)
    if (below === UNLOADED || climbName[below] !== LADDER) return false
    // measured live (26.1, once, 2026-10-03: may be a test-geometry artifact): over a ladder, a trapdoor is passable for the
    // climb only when its facing differs from the ladder's; one facing the same way stopped the body, open or not
    return facing[id] !== facing[below]
  }
  const climbHere = (x, y, z) => {
    const id = rawAt(x, y, z)
    if (id === UNLOADED) return false
    const cl = climb[id]
    return cl !== 0 && climbCell(cl, id, x, y, z)
  }
  // a closed wooden trapdoor above a ladder: entering the cell costs an OPEN
  const shutAt = (x, y, z) => {
    const id = rawAt(x, y, z)
    return id !== UNLOADED && climb[id] === CLIMB_TRAP_SHUT && climbCell(climb[id], id, x, y, z)
  }
  // what the free-space masks read: a closed wooden trapdoor over a ladder is one the climb opens, so there it reads as air
  const view = { stateAt: (x, y, z) => {
    const id = stateAt(x, y, z)
    return climb[id] === CLIMB_TRAP_SHUT && climbCell(CLIMB_TRAP_SHUT, id, x, y, z) ? 0 : id
  } }
  let allowShut = false // standH refuses a shut trapdoor's cell unless the caller pays for opening it

  // a cell the body must not be in: an AVOID hazard, or a portal unless the cell (or the one under it, the head cell) is in the goal
  const avoids = (id, x, y, z) => {
    const hz = hazard[id]
    return hz === HAZARD_AVOID || hz === PORTAL && !inGoal(x, y, z)
  }

  // is the column at x,z free for a body spanning lo..hi (1/16 absolute)? Also false for fluid, NARROW, AVOID, unloaded.
  const clear = (x, z, lo, hi) => {
    const last = (hi - 1) >> 4
    for (let y = lo >> 4; y <= last; y++) {
      const id = stateAt(x, y, z)
      if (id === UNLOADED) return false
      const t = top[id]
      if (t > 0 && y * 16 + t > lo && y * 16 + base[id] < hi) return false
      const k = kind[id]
      if (k === WATER || k === LAVA || k === NARROW || avoids(id, x, y, z)) return false
    }
    return true
  }

  // what a diagonal's side column holds for a body spanning lo..hi: 0 passable (fluid other than lava is fine, so is a hole),
  // 1 collision only (a corner to slide along), 2 lava, AVOID or unloaded: never brushed
  const side = (x, z, lo, hi) => {
    const last = (hi - 1) >> 4
    let blocked = 0
    for (let y = lo >> 4; y <= last; y++) {
      const id = stateAt(x, y, z)
      if (id === UNLOADED || kind[id] === LAVA || avoids(id, x, y, z)) return 2
      const t = top[id]
      if (kind[id] === NARROW || (t > 0 && y * 16 + t > lo && y * 16 + base[id] < hi)) blocked = 1
    }
    return blocked
  }

  let support = 0 // state id the last standH stood on
  let touch = 0 // DAMAGE_TOUCH cells inside the last standH body

  // does the body fit in the column of feet cell y at x,z, its feet at absolute lo (1/16)? `id` stands for the feet cell itself.
  // Sets `touch`. Collision that leaves gaps is for the tight-cell mask to judge, not for refusing the cell.
  const fits = (x, y, z, lo, id) => {
    const hi = lo + BODY
    const last = (hi - 1) >> 4
    let touched = 0
    for (let k = y; k <= last; k++) {
      const cid = k === y ? id : stateAt(x, k, z)
      if (cid === UNLOADED) return false
      const ct = top[cid]
      const kd = kind[cid]
      if (kd === WATER || kd === LAVA || avoids(cid, x, k, z)) return false
      if (!partial[cid] && (ct > 0 && k * 16 + ct > lo && k * 16 + base[cid] < hi || kd === NARROW)) return false
      if (hazard[cid] === DAMAGE_TOUCH) touched++
    }
    touch = touched
    return true
  }

  // stand height at a feet cell, or -1
  const standH = (x, y, z) => {
    const raw = stateAt(x, y, z)
    if (raw === UNLOADED) return -1
    const cl = climb[raw]
    if (cl !== 0 && climbCell(cl, raw, x, y, z)) {
      // the body hangs in the climbable: no floor, feet at the cell's floor
      const shut = cl === CLIMB_TRAP_SHUT
      if (shut && !allowShut) return -1
      support = raw
      return fits(x, y, z, y * 16, shut ? 0 : raw) ? 0 : -1
    }
    const id = raw
    const t = top[id]
    let h
    // a block, a stairs: not a place to stand in. A partial one (fence, bamboo, wall, gate) leaves room at the cell's
    // edge: the cell stands on the floor below and the mask decides where the body fits
    if (t >= WHOLE && base[id] === 0 && !partial[id]) return -1
    if (t > 0 && base[id] === 0 && t < WHOLE) {
      const hz = hazard[id]
      if (kind[id] === NARROW || hz === HAZARD_AVOID || hz === DAMAGE_TOUCH) return -1
      h = t
      support = id
    } else {
      const below = stateAt(x, y - 1, z)
      if (below === UNLOADED) return -1
      const tb = floor[below]
      const hz = hazard[below]
      // a lower top is that cell's own stand height, not ground for this one
      // (an open door, gate or trapdoor is a panel at the cell's edge: nothing to stand on)
      if (tb < WHOLE || kind[below] === NARROW || hz === HAZARD_AVOID || hz === DAMAGE_TOUCH || kind[below] === OPENABLE && openable[below] === 0) return -1
      h = tb - WHOLE
      support = below
    }
    return fits(x, y, z, y * 16 + h, id) ? h : -1
  }

  const lavaAt = (x, y, z) => {
    const id = stateAt(x, y, z)
    return id !== UNLOADED && kind[id] === LAVA
  }
  // lava beside the feet or beside the floor under them
  const lavaNear = (x, y, z) => {
    for (let c = 0; c < 4; c++) {
      const [dx, dz] = CARDINAL[c]
      if (lavaAt(x + dx, y, z + dz) || lavaAt(x + dx, y - 1, z + dz)) return true
    }
    return false
  }

  // 1 when a fall through this column ends in lava or beyond a safe drop, else 0
  const holeRisk = (x, y, z) => {
    for (let k = 1; k <= FREE_FALL + 1; k++) {
      const id = stateAt(x, y - k, z)
      if (id === UNLOADED) return 0
      if (kind[id] === LAVA) return 1
      if (top[id] > 0 || kind[id] === WATER) return 0
    }
    return 1
  }

  let enterRisk = 0 // set by landing()
  let enterSlow = 0
  let enterExtra = 0 // seconds a big dripleaf leaf adds

  // A body standing level with a magma bubble column (or at the surface cell beside it, when it stands on the bank above) can slip
  // into it: 1 of 3 live exits from a soul-sand column did. Costs risk. `quiet` says no section near the cell holds such a column.
  const besideMagma = (x, y, z) => {
    if (quiet) return 0
    for (let c = 0; c < 4; c++) {
      const x2 = x + CARDINAL[c][0]
      const z2 = z + CARDINAL[c][1]
      if (bubble[stateAt(x2, y, z2)] === 2 || bubble[stateAt(x2, y - 1, z2)] === 2) return costs.besideMagmaColumn
    }
    return 0
  }

  // standH plus what arriving there costs; -1 when not standable
  const landing = (x, y, z) => {
    const h = standH(x, y, z)
    if (h < 0) return -1
    const hz = hazard[support]
    const leaf = dripleaf[support] === 1
    enterRisk = touch + (hz === DAMAGE_STAND ? 1 : 0) + (lavaNear(x, y, z) ? LAVA_ADJACENT : 0) + (leaf ? costs.dripleafRisk : 0) + besideMagma(x, y, z)
    enterSlow = hz === SLOW ? 1 : 0
    enterExtra = leaf ? costs.dripleaf : 0
    return h
  }

  // ---- goal ----

  const near = goal.kind === 'near'
  const reached = near
    ? (x, y, z) => (x - goal.x) ** 2 + (y - goal.y) ** 2 + (z - goal.z) ** 2 <= goalRange * goalRange
    : (x, y, z) => (x - goal.x) ** 2 + (z - goal.z) ** 2 <= goalRange * goalRange
  // the cell, or the one under it (a head cell), is within the goal: where a portal may be entered
  const inGoal = (x, y, z) => reached(x, y, z) || reached(x, y - 1, z)
  const distanceTo = (x, z) => {
    const a = Math.abs(x - goal.x)
    const b = Math.abs(z - goal.z)
    return Math.max(a, b) + (SQRT2 - 1) * Math.min(a, b)
  }
  const heuristic = (x, z) => Math.max(0, distanceTo(x, z) - slack) * WALK_S
  const goalUnloaded = near ? stateAt(goal.x, goal.y, goal.z) === UNLOADED : !snapshot.hasColumn(goal.x >> 4, goal.z >> 4)

  // ---- search box: start and goal, plus margins ----

  const bx0 = Math.min(from.x, goal.x) - margin
  const bx1 = Math.max(from.x, goal.x) + margin
  const bz0 = Math.min(from.z, goal.z) - margin
  const bz1 = Math.max(from.z, goal.z) + margin
  const by0 = (near ? Math.min(from.y, goal.y) : from.y) - yMargin
  const by1 = (near ? Math.max(from.y, goal.y) : from.y) + yMargin
  let boxed = false // some node was refused by the box

  // ---- node storage: open-addressing hash on packed coordinates, binary heap, all typed arrays ----

  let cap = Math.min(maxNodes, 1024)
  let slots = nextPow2(cap * 2)
  let hashTable = new Int32Array(slots).fill(-1)
  let keys = new Float64Array(cap)
  let xs = new Int32Array(cap)
  let ys = new Int32Array(cap)
  let zs = new Int32Array(cap)
  let hs = new Uint8Array(cap)
  let moves = new Uint8Array(cap)
  let slow = new Uint8Array(cap)
  let corners = new Uint8Array(cap)
  let shapes = new Uint32Array(cap) // see packShape
  let parent = new Int32Array(cap)
  let secs = new Float64Array(cap)
  let risks = new Float64Array(cap)
  let airs = new Float64Array(cap) // seconds of air used since the last breath
  let peaks = new Float64Array(cap) // the most air used at any point of the path to the node
  let wsecs = new Float64Array(cap) // seconds of the path spent swimming
  let opens = new Int32Array(cap) // 1 + index in openLists of what the move to the node opened, 0 for nothing
  let gs = new Float64Array(cap)
  let fs = new Float64Array(cap)
  let heapPos = new Int32Array(cap) // index in heap, -2 once expanded
  let heap = new Int32Array(cap)
  let count = 0
  let heapN = 0

  const grown = (array, size) => {
    const bigger = new array.constructor(size)
    bigger.set(array)
    return bigger
  }
  const hashOf = (x, y, z, region) => (Math.imul(x - from.x + HALF, 73856093) ^ Math.imul(y - minY, 19349663) ^ Math.imul(z - from.z + HALF, 83492791) ^ Math.imul(region, 668265263)) >>> 0
  const keyOf = (x, y, z, region = 0) => (((y - minY) * SPAN + (x - from.x + HALF)) * SPAN + (z - from.z + HALF)) * REGIONS + region

  const grow = () => {
    cap = Math.min(maxNodes, cap * 2)
    ;[keys, xs, ys, zs, hs, moves, slow, corners, shapes, parent, secs, risks, airs, peaks, wsecs, opens, gs, fs, heapPos, heap] =
      [keys, xs, ys, zs, hs, moves, slow, corners, shapes, parent, secs, risks, airs, peaks, wsecs, opens, gs, fs, heapPos, heap].map(a => grown(a, cap))
    slots = nextPow2(cap * 2)
    hashTable = new Int32Array(slots).fill(-1)
    // newest first: a rival record of a node (same key) sits ahead of the one it rivals in the probe, so lookups find it
    for (let i = count - 1; i >= 0; i--) {
      let s = hashOf(xs[i], ys[i], zs[i], shapes[i] & 15) & (slots - 1)
      while (hashTable[s] !== -1) s = (s + 1) & (slots - 1)
      hashTable[s] = i
    }
  }

  // best first by f, then deeper (larger g), then earlier discovered: ties never depend on memory layout
  const before = (a, b) => fs[a] < fs[b] || (fs[a] === fs[b] && (gs[a] > gs[b] || (gs[a] === gs[b] && a < b)))

  const siftUp = (i, node) => {
    while (i > 0) {
      const p = (i - 1) >> 1
      if (!before(node, heap[p])) break
      heap[i] = heap[p]
      heapPos[heap[i]] = i
      i = p
    }
    heap[i] = node
    heapPos[node] = i
  }
  const siftDown = (i, node) => {
    for (;;) {
      let c = 2 * i + 1
      if (c >= heapN) break
      if (c + 1 < heapN && before(heap[c + 1], heap[c])) c++
      if (!before(heap[c], node)) break
      heap[i] = heap[c]
      heapPos[heap[i]] = i
      i = c
    }
    heap[i] = node
    heapPos[node] = i
  }
  const pop = () => {
    const first = heap[0]
    heapN--
    if (heapN > 0) siftDown(0, heap[heapN])
    heapPos[first] = -2
    return first
  }

  let overBudget = false

  // What the swim move being made adds to the node: the air used by the end of it, the most air used while it lasted, the
  // seconds in water. Set around the edge call by swimEdge (0 for every other move).
  let moveAir = 0
  let movePeak = 0
  let moveWater = 0
  let moveOpen = 0 // 1 + index in openLists of what the move being made opens (see expandAt), 0 for nothing
  const openLists = []

  // The returnable search holds back every drop of at most JUMP_UP until the probe (canReturn) has said the body can climb back:
  // the probe runs the moves of another cell, so it cannot run inside an expansion (see flushHeld, which `step` calls after each).
  const stand16 = node => ys[node] * 16 + hs[node]
  const held = []
  let replaying = false

  // relax the edge to a node: insert it, or lower its cost if this way is cheaper
  // a tight cell's node also has its region, the region's point and the crossing the move came in by: `shape`
  // A node reached again with a very different air use gets a record of its own (see the header): the hash points at the newest.
  const consider = (x, y, z, h, move, parentNode, dsec, drisk, slowTo, corner = 0, shape = 0) => {
    const region = shape & 15
    const rx = x - from.x + HALF
    const rz = z - from.z + HALF
    if (rx < 0 || rx >= SPAN || rz < 0 || rz >= SPAN) return
    if (x < bx0 || x > bx1 || z < bz0 || z > bz1 || y < by0 || y > by1) { boxed = true; return }
    // a gap jump or a drop never lands on farmland: a landing after a fall of over 0.5 blocks tramples it (a farmland node is the
    // farmland's own cell; a jump up one block falls about 0.3 from the top of its arc, so it may land there)
    if ((move === MOVE.GAP || move === MOVE.DROP) && farmland[rawAt(x, y, z)] === 1) return
    if (returnable && !replaying) {
      if (move === MOVE.GAP && y * 16 + h < stand16(parentNode)) return
      if (move === MOVE.DROP) {
        if (stand16(parentNode) - (y * 16 + h) > JUMP_UP) return
        held.push([x, y, z, h, move, parentNode, dsec, drisk, slowTo, corner, shape, moveOpen, moveAir, movePeak, moveWater])
        return
      }
    }
    const key = keyOf(x, y, z, region)
    let s = hashOf(x, y, z, region) & (slots - 1)
    let found = -1
    while (hashTable[s] !== -1) {
      if (keys[hashTable[s]] === key) { found = hashTable[s]; break }
      s = (s + 1) & (slots - 1)
    }
    const sec = secs[parentNode] + dsec
    const risk = risks[parentNode] + drisk
    const g = sec + riskWeight * risk
    let node = found
    if (found !== -1) {
      const dAir = moveAir - airs[found]
      if (g < gs[found] && dAir <= AIR_STEP) {
        if (heapPos[found] === -2) return
      } else if (g < gs[found] || dAir < -AIR_STEP) {
        node = -1
      } else return
    }
    if (node === -1) {
      if (count === maxNodes) { overBudget = true; return }
      if (count === cap) {
        grow()
        s = hashOf(x, y, z, region) & (slots - 1)
        while (hashTable[s] !== -1 && (found === -1 || keys[hashTable[s]] !== key)) s = (s + 1) & (slots - 1)
      }
      node = count++
      hashTable[s] = node
      keys[node] = key
      xs[node] = x
      ys[node] = y
      zs[node] = z
      heapPos[node] = -1
    }
    hs[node] = h
    moves[node] = move
    slow[node] = slowTo
    corners[node] = corner
    shapes[node] = shape
    parent[node] = parentNode
    secs[node] = sec
    risks[node] = risk
    airs[node] = moveAir
    peaks[node] = Math.max(peaks[parentNode], movePeak)
    wsecs[node] = wsecs[parentNode] + moveWater
    opens[node] = moveOpen
    gs[node] = g
    fs[node] = g + weight * heuristic(x, z)
    if (heapPos[node] === -1) {
      heapN++
      siftUp(heapN - 1, node)
      return
    }
    siftUp(heapPos[node], node)
  }

  // ---- tight cells: where in the cell the body fits ----

  // Tightness is asked about every neighbour of every expansion, so it is cached in direct-mapped tables (a collision just
  // recomputes): per column, "partial collision in rows y-1..y+2", and per cell, "in any of the 9 columns around". Keys are
  // stored +1 so that zeroed memory reads as empty.
  const TABLE = 1 << 13
  const columnKeys = new Float64Array(TABLE)
  const columnFlags = new Uint8Array(TABLE)
  const cellKeys = new Float64Array(TABLE)
  const cellFlags = new Uint8Array(TABLE)
  const columnPartial = (x, y, z) => {
    const key = (keyOf(x, y, z) + 1) * (openMode ? -1 : 1) // (the opening pass sees other blocks: its own cache entries)
    const slot = hashOf(x, y, z, 0) & (TABLE - 1)
    if (columnKeys[slot] === key) return columnFlags[slot]
    let flag = 0
    for (let cy = y - 1; cy <= y + 2 && flag === 0; cy++) {
      const id = stateAt(x, cy, z)
      if (id !== UNLOADED && partial[id]) flag = 1
    }
    columnKeys[slot] = key
    columnFlags[slot] = flag
    return flag
  }
  // no section the block of cells x +-r, z +-r, rows y-below..y+above touches holds a partial block or a climbable: the usual case, answered
  // without reading cells
  const sectionsClear = (x, y, z, r, below, above) => {
    const sy1 = (y + above - minY) >> 4
    for (let sy = (y - below - minY) >> 4; sy <= sy1; sy++) {
      for (let sz = (z - r) >> 4; sz <= (z + r) >> 4; sz++) {
        for (let sx = (x - r) >> 4; sx <= (x + r) >> 4; sx++) if (snapshot.sectionHas(special, sx, sy, sz)) return false
      }
    }
    return true
  }
  const isTight = (x, y, z) => {
    if (sectionsClear(x, y, z, 1, 1, 2)) return false
    const key = (keyOf(x, y, z) + 1) * (openMode ? -1 : 1)
    const slot = hashOf(x, y, z, 0) & (TABLE - 1)
    if (cellKeys[slot] === key) return cellFlags[slot] === 1
    let flag = 0
    for (let cz = z - 1; cz <= z + 1 && flag === 0; cz++) {
      for (let cx = x - 1; cx <= x + 1 && flag === 0; cx++) flag = columnPartial(cx, y, cz)
    }
    cellKeys[slot] = key
    cellFlags[slot] = flag
    return flag === 1
  }

  // free-position mask of the cell for a body standing at lo16 (absolute 1/16), its labelled regions, and the region of the
  // cell centre. One per (cell, height) per search.
  const maskCache = new Map()
  const tightSeen = new Set()
  const stats = { masks: 0, tightMasks: 0, tightCells: 0, regions: 0, maskMs: 0, flooded: 0 }
  const shapeOf = (x, y, z, lo16) => {
    const key = (keyOf(x, y, z) * 128 + (lo16 - y * 16 + 32)) * (openMode ? -1 : 1)
    let shape = maskCache.get(key)
    if (shape !== undefined) return shape
    const t = performance.now()
    const lo = lo16 / 16
    const mask = freeMask(boxesNear(view, table, x, y, z, lo, lo + BODY_BLOCKS), x, z)
    const { labels, regs } = labelRegions(mask)
    shape = { mask, labels, regs, centre: labels[8 * GRID + 8] }
    maskCache.set(key, shape)
    stats.maskMs += performance.now() - t
    stats.masks++
    if (!isTight(x, y, z)) return shape
    stats.tightMasks++
    stats.regions += regs.length
    tightSeen.add(keyOf(x, y, z))
    return shape
  }

  // The region of a boundary point in a cell's own mask. Where the two cells' heights differ the point can be blocked only by
  // the step itself (the body crosses at the higher level, then settles), so a blocked point takes the region of the nearest
  // free position within SNAP/16: a whole cell (GRID) for a body leaving a climbable by a rise, or falling onto one.
  const SNAP = 6
  const regionNear = (shape, p, snap = SNAP) => {
    if (shape.labels[p] >= 0) return shape.labels[p]
    const pi = p % GRID
    const pj = (p - pi) / GRID
    let best = -1
    let bestD = Infinity
    for (let j = Math.max(0, pj - snap); j <= Math.min(GRID - 1, pj + snap); j++) {
      for (let i = Math.max(0, pi - snap); i <= Math.min(GRID - 1, pi + snap); i++) {
        const label = shape.labels[j * GRID + i]
        const d = (i - pi) ** 2 + (j - pj) ** 2
        if (label >= 0 && d < bestD) { best = label; bestD = d }
      }
    }
    return best
  }

  // index in the 17x17 mask of boundary point t (0..16 along the shared edge) for the cell moved from (A) and to (B),
  // for the cardinal c: 0 east, 1 west, 2 south (+z), 3 north
  const indexA = (c, t) => c === 0 ? t * GRID + 16 : c === 1 ? t * GRID : c === 2 ? 16 * GRID + t : t
  const indexB = (c, t) => c === 0 ? t * GRID : c === 1 ? t * GRID + 16 : c === 2 ? t : 16 * GRID + t
  const pick = new Int8Array(REGIONS)

  // the cardinal move c from cell A (region `region`, or every region when -1) to cell B, either of them tight: one edge per
  // region of B that a boundary point free for both leads to. Costs are those of the plain move, plus TIGHT_S into a tight cell.
  const tightMove = (i, x, y, z, h, region, c, x2, y2, z2, h1, move, dsec, drisk, slowTo, snapA = SNAP, snapB = SNAP) => {
    const loA = y * 16 + h
    const loB = y2 * 16 + h1
    const top = Math.max(loA, loB) // the body straddles the boundary at the higher of the two heights
    const tightA = isTight(x, y, z)
    const tightB = isTight(x2, y2, z2)
    const ownA = shapeOf(x, y, z, loA)
    const ownB = shapeOf(x2, y2, z2, loB)
    const jointA = loA === top ? ownA : shapeOf(x, Math.max(y, y2), z, top)
    const jointB = loB === top ? ownB : shapeOf(x2, Math.max(y, y2), z2, top)
    // a drop falls straight down the neighbour column: every tight cell between must be free at the crossing point too
    const fallMasks = []
    if (move === MOVE.DROP) for (let k = y2 + 1; k < y; k++) if (isTight(x2, k, z2)) fallMasks.push(shapeOf(x2, k, z2, k * 16).mask)
    const first = region < 0 ? 0 : region
    const last = region < 0 ? (tightA ? ownA.regs.length - 1 : 0) : region
    const sec = dsec + (tightB ? TIGHT_S : 0)
    for (let ra = first; ra <= last && ra < REGIONS; ra++) {
      const label = tightA ? ra : ownA.centre
      if (label < 0) continue
      const repA = tightA ? ownA.regs[ra] : CENTRE
      pick.fill(-1)
      for (let t = 0; t <= 16; t++) {
        const pa = indexA(c, t)
        const pb = indexB(c, t)
        if (!jointA.mask[pa] || !jointB.mask[pb] || regionNear(ownA, pa, snapA) !== label || fallMasks.some(m => !m[pb])) continue
        const lb = regionNear(ownB, pb, snapB)
        const rb = tightB ? lb : lb === ownB.centre ? 0 : -1
        if (rb < 0 || rb >= REGIONS) continue
        const repB = tightB ? ownB.regs[rb] : CENTRE
        const along = c < 2 ? (repA.pz + repB.pz) / 2 : (repA.px + repB.px) / 2
        if (pick[rb] === -1 || Math.abs(t - along) < Math.abs(pick[rb] - along)) pick[rb] = t
      }
      for (let rb = 0; rb < REGIONS; rb++) {
        const t = pick[rb]
        if (t < 0) continue
        const rep = tightB ? ownB.regs[rb] : CENTRE
        // the crossing, relative to B: on its west edge (0) for an eastward move, its east edge (16) for a westward one...
        const crossX = c === 0 ? 0 : c === 1 ? 16 : t
        const crossZ = c === 2 ? 0 : c === 3 ? 16 : t
        edge(x2, y2, z2, h1, move, i, sec, drisk, slowTo, 0, packShape(rb, tightB, rep.px, rep.pz, true, crossX, crossZ))
      }
    }
  }

  // A vertical move in one column from cell y (stand height h, region `region` or every one when -1) to cell y2: climbing, a jump
  // into a ladder, a fall. The body stays at one position (x + i/16, z + j/16) all the way, so it must fit there at the
  // start, the end and every cell between: one edge per pair of regions that such a position joins, the position nearest
  // the middle of the two regions' points being the crossing.
  const bestD = new Float64Array(REGIONS)
  const bestAt = new Int16Array(REGIONS)
  const verticalMove = (i, x, y, z, h, region, y2, h2, move, dsec, drisk, slowTo) => {
    const tightA = isTight(x, y, z)
    const tightB = isTight(x, y2, z)
    if (!tightA && !tightB) return edge(x, y2, z, h2, move, i, dsec, drisk, slowTo)
    const ownA = shapeOf(x, y, z, y * 16 + h)
    const ownB = shapeOf(x, y2, z, y2 * 16 + h2)
    const between = []
    for (let k = Math.min(y, y2) + 1; k < Math.max(y, y2); k++) between.push(shapeOf(x, k, z, k * 16).mask)
    const first = region < 0 ? 0 : region
    const last = region < 0 ? (tightA ? ownA.regs.length - 1 : 0) : region
    for (let ra = first; ra <= last && ra < REGIONS; ra++) {
      const labelA = tightA ? ra : ownA.centre
      if (labelA < 0) continue
      const repA = tightA ? ownA.regs[ra] : CENTRE
      bestD.fill(Infinity)
      for (let p = 0; p < GRID * GRID; p++) {
        if (!ownA.mask[p] || ownA.labels[p] !== labelA || !ownB.mask[p] || (between.length > 0 && between.some(m => !m[p]))) continue
        const lb = ownB.labels[p]
        const rb = tightB ? lb : lb === ownB.centre ? 0 : -1
        if (rb < 0 || rb >= REGIONS) continue
        const repB = tightB ? ownB.regs[rb] : CENTRE
        const pi = p % GRID
        const pj = (p - pi) / GRID
        const d = (pi - (repA.px + repB.px) / 2) ** 2 + (pj - (repA.pz + repB.pz) / 2) ** 2
        if (d < bestD[rb]) { bestD[rb] = d; bestAt[rb] = p }
      }
      for (let rb = 0; rb < REGIONS; rb++) {
        if (bestD[rb] === Infinity) continue
        const rep = tightB ? ownB.regs[rb] : CENTRE
        const p = bestAt[rb]
        const pi = p % GRID
        edge(x, y2, z, h2, move, i, dsec, drisk, slowTo, 0, packShape(rb, tightB, rep.px, rep.pz, true, pi, (p - pi) / GRID))
      }
    }
  }

  // ---- water ----

  const isWater = (x, y, z) => {
    const id = stateAt(x, y, z)
    return id !== UNLOADED && kind[id] === WATER
  }
  // a body floating in the water cell (feet at its floor, h = 0): the cell over it is water or open and unhazardous
  const swimAt = (x, y, z) => {
    const id = stateAt(x, y, z)
    if (id === UNLOADED || kind[id] !== WATER) return -1
    const head = stateAt(x, y + 1, z)
    if (head === UNLOADED) return -1
    if (kind[head] === WATER) return 0
    return kind[head] === OPEN && top[head] === 0 && hazard[head] === 0 ? 0 : -1
  }
  // head in water that is not a bubble column: the breath runs
  const submerged = (x, y, z) => {
    const head = stateAt(x, y + 1, z)
    return head !== UNLOADED && kind[head] === WATER && bubble[head] === 0
  }
  // Dominance: a submerged sideways move in open water is never better than swimming at the surface over it, so the lake's
  // volume is not searched. A cell's column is open when its water reaches plain air (no collision); then surfaceY is the top
  // water cell. Ending at a ceiling, a collision block or an unloaded cell is a passage under something (NONE), where diving is
  // the only way. The surface only stands in for the cell when both ends have the same one: a waterfall's foot, whose column
  // rises out of a shallow pool, is entered sideways. Cached per cell.
  const NONE = -1e9
  const surfaceCache = new Map()
  const surfaceY = (x, y, z) => {
    const key = keyOf(x, y, z)
    const hit = surfaceCache.get(key)
    if (hit !== undefined) return hit
    let y2 = y + 1
    let id = stateAt(x, y2, z)
    while (id !== UNLOADED && kind[id] === WATER) id = stateAt(x, ++y2, z)
    const top2 = id !== UNLOADED && kind[id] === OPEN && top[id] === 0 ? y2 - 1 : NONE
    surfaceCache.set(key, top2)
    return top2
  }
  const openWater = (x, y, z) => surfaceY(x, y, z) !== NONE
  const divesOpen = (x, y, z) => submerged(x, y, z) && openWater(x, y, z)
  // a sideways swim move into (x, y, z) that dominance refuses: the body is at surface level `srcSurface` (NONE: in a covered
  // passage, where the open water is the way on) and the target is submerged under the same surface
  const refuses = (x, y, z, srcSurface) => srcSurface !== NONE && submerged(x, y, z) && surfaceY(x, y, z) === srcSurface
  // Swimming down from the surface into open water leads nowhere but more cells of the same column, unless something down it is
  // worth the dive: the goal, a bubble column, a bank to climb onto at that depth, or a covered passage beside it. Cached per cell
  // like openWater.
  const diveCache = new Map()
  const worthDiving = (x, y, z) => {
    const key = keyOf(x, y, z)
    const hit = diveCache.get(key)
    if (hit !== undefined) return hit === 1
    let found = false
    for (let y2 = y; !found && isWater(x, y2, z); y2--) {
      found = reached(x, y2, z) || bubble[stateAt(x, y2, z)] !== 0
      for (let c = 0; !found && c < 4; c++) {
        const x2 = x + CARDINAL[c][0]
        const z2 = z + CARDINAL[c][1]
        found = isWater(x2, y2, z2) ? swimAt(x2, y2, z2) >= 0 && !divesOpen(x2, y2, z2) : standH(x2, y2, z2) >= 0
      }
    }
    diveCache.set(key, found ? 1 : 2)
    return found
  }
  // swimming beside lava, or down onto magma
  const swimRisk = (x, y, z) => (lavaNear(x, y, z) ? LAVA_ADJACENT : 0) + (hazard[stateAt(x, y - 1, z)] === DAMAGE_STAND ? 1 : 0)
  // a stand height at a feet cell: on land, or floating in water (h = 0); -1 for neither
  const nodeH = (x, y, z) => {
    const h = standH(x, y, z)
    return h >= 0 ? h : swimAt(x, y, z)
  }

  let airSeen = false // a swim move was refused for lack of air

  // one swim or exit edge, made by `emit(dsec)` (a plain edge, a tight-cell move or a vertical one): `base` seconds of swimming
  // (they count for the air), `extra` seconds of current (a cost, not breath). srcSub: the body starts the move with its head in
  // water; targetWater: it ends in a water cell.
  const swimEdge = (i, targetWater, x2, y2, z2, base, extra, srcSub, emit) => {
    const targetSub = targetWater && submerged(x2, y2, z2)
    const use = (i >= 0 ? airs[i] : 0) + base
    if ((srcSub || targetSub) && use > costs.airLimit) { airSeen = true; return }
    moveAir = targetSub ? use : 0
    movePeak = srcSub || targetSub ? use : 0
    moveWater = base + extra
    emit(base + extra)
    moveAir = movePeak = moveWater = 0
  }

  // the water cell is 1 deep over a floor, with air over it: the body stands on the floor, it is not floating
  const standsInWater = (x, y, z) => {
    if (bubble[stateAt(x, y, z)] !== 0) return false
    const head = stateAt(x, y + 1, z)
    const below = stateAt(x, y - 1, z)
    if (head === UNLOADED || below === UNLOADED || kind[head] === WATER) return false
    const hz = hazard[below]
    return floor[below] >= WHOLE && kind[below] !== NARROW && hz !== HAZARD_AVOID && hz !== DAMAGE_TOUCH
  }

  // out of 1-deep water by the walking rules: a step of up to STEP is a walk, up to JUMP_UP a jump (the head cell is open)
  const wadeOut = (i, x, y, z, region, c, x2, z2, tightSrc) => {
    const h0 = y * 16
    const h1 = neighbour(x2, z2, y, h0)
    if (h1 < 0) return
    const delta = ty * 16 + h1 - h0
    const walks = delta <= STEP
    if (!walks && (delta > JUMP_UP || !clear(x, z, (y + 1) * 16, ty * 16 + h1 + BODY))) return
    const sec = (walks ? WALK_S : WALK_S + JUMP_S) + enterExtra
    const move = walks ? MOVE.WALK : MOVE.JUMP
    if (tightSrc || tightAt(x2, ty, z2)) tightMove(i, x, y, z, 0, region, c, x2, ty, z2, h1, move, sec, enterRisk, enterSlow)
    else edge(x2, ty, z2, h1, move, i, sec, enterRisk, enterSlow)
  }

  // a node floating in water: up, down, sideways and onto the bank; sideways moves and exits may cross tight cells (masks), a
  // diagonal never does
  const expandSwim = (x, y, z, i, region) => {
    const tightSrc = tightAt(x, y, z)
    const srcB = bubble[stateAt(x, y, z)]
    const srcSub = submerged(x, y, z)
    const srcSurface = srcSub ? surfaceY(x, y, z) : y
    const currentAt = (cx, cy, cz) => flowing[stateAt(cx, cy, cz)] === 1 ? costs.current : 0

    // a lifting column (1) cannot be swum down, a dragging one (2) cannot be swum up
    for (const dy of [1, -1]) {
      const y2 = y + dy
      if (swimAt(x, y2, z) < 0) continue
      const tb = bubble[stateAt(x, y2, z)]
      if (dy === 1 ? srcB === 2 || tb === 2 : srcB === 1 || tb === 1) continue
      if (dy === -1 && divesOpen(x, y2, z) && !worthDiving(x, y2, z)) continue
      const lift = srcB === 1 || tb === 1
      const drag = srcB === 2 || tb === 2
      const move = dy === 1 ? MOVE.SWIM_UP : MOVE.SWIM_DOWN
      const base = dy === 1 ? (lift ? costs.bubbleUp : costs.swimUp) : (drag ? costs.bubbleDown : costs.swimDown)
      const risk = swimRisk(x, y2, z)
      swimEdge(i, true, x, y2, z, base, currentAt(x, y2, z), srcSub, dsec => verticalMove(i, x, y, z, 0, region, y2, 0, move, dsec, risk, 0))
    }

    // out of the water onto a bank: at the same level, or up to exitRise cells over the top water cell of this column
    // Live (26.1): a floating body gets out onto land whose stand height is at most the water's top face + 1/16 (flush, or a
    // 15/16 top), never onto land one higher (0/5, peak 0.42-0.63 short). A body standing on a floor in water 1 deep is not
    // floating: it walks and jumps out by the ordinary rules.
    const topWater = isWater(x, y + 1, z)
    const nearSurface = !topWater || !isWater(x, y + 2, z)
    const yt = topWater ? y + 1 : y
    const lastTy = nearSurface ? Math.max(y, yt + 1) : y
    const maxStand = (yt + 1) * 16 + EXIT_SLACK
    const wading = standsInWater(x, y, z)
    for (let c = 0; c < 4; c++) {
      const x2 = x + CARDINAL[c][0]
      const z2 = z + CARDINAL[c][1]
      if (swimAt(x2, y, z2) >= 0) {
        if (refuses(x2, y, z2, srcSurface)) continue
        const risk = swimRisk(x2, y, z2)
        const tight = tightSrc || tightAt(x2, y, z2)
        swimEdge(i, true, x2, y, z2, costs.swimH, currentAt(x2, y, z2), srcSub, dsec => tight
          ? tightMove(i, x, y, z, 0, region, c, x2, y, z2, 0, MOVE.SWIM, dsec, risk, 0)
          : edge(x2, y, z2, 0, MOVE.SWIM, i, dsec, risk, 0))
        continue
      }
      if (wading) {
        wadeOut(i, x, y, z, region, c, x2, z2, tightSrc)
        continue
      }
      for (let ty = y; ty <= lastTy; ty++) {
        const h1 = landing(x2, ty, z2)
        if (h1 < 0 || ty * 16 + h1 > maxStand) continue
        const base = (ty === y ? costs.swimH : costs.exit) + enterExtra
        const risk = enterRisk
        const slowTo = enterSlow
        const tight = tightSrc || tightAt(x2, ty, z2)
        swimEdge(i, false, x2, ty, z2, base, 0, srcSub, dsec => tight
          ? tightMove(i, x, y, z, 0, region, c, x2, ty, z2, h1, MOVE.EXIT, dsec, risk, slowTo)
          : edge(x2, ty, z2, h1, MOVE.EXIT, i, dsec, risk, slowTo))
      }
    }
    if (tightSrc) return

    // diagonals, with the walking side rule: the body brushes both side cells
    for (let c = 0; c < 4; c++) {
      const dx = DIAGONAL[c][0]
      const dz = DIAGONAL[c][1]
      const x2 = x + dx
      const z2 = z + dz
      if (swimAt(x2, y, z2) < 0 || tightAt(x2, y, z2) || refuses(x2, y, z2, srcSurface)) continue
      const lo = y * 16
      const hi = lo + BODY
      const sa = side(x + dx, z, lo, hi)
      const sb = side(x, z + dz, lo, hi)
      if (sa === 2 || sb === 2 || (sa === 1 && sb === 1)) continue
      const slide = sa + sb
      const risk = swimRisk(x2, y, z2)
      swimEdge(i, true, x2, y, z2, costs.swimH * SQRT2 + slide * CORNER_S, currentAt(x2, y, z2), srcSub, dsec => edge(x2, y, z2, 0, slide ? MOVE.CORNER : MOVE.SWIM, i, dsec, risk, 0, slide))
    }
  }

  // ---- moves ----

  let edge = consider // where moves go: into the search, or the goal flood's probe

  let ty = 0 // y of the cell the last neighbour() found
  // the standable cell beside the feet: level, one up, or a step down of at most STEP; else -1 (a bigger fall is a drop)
  const neighbour = (x2, z2, y, h0) => {
    ty = y
    let h1 = landing(x2, y, z2)
    if (h1 >= 0) return h1
    ty = y + 1
    h1 = landing(x2, ty, z2)
    if (h1 >= 0) return h1
    ty = y - 1
    h1 = landing(x2, ty, z2)
    if (h1 < 0 || ty * 16 + h1 - h0 < -STEP) return -1
    // the body also leaves the higher level through this column (a tight cell's mask checks that itself)
    return tightAt(x2, ty, z2) || clear(x2, z2, ty * 16 + h1, h0 + BODY) ? h1 : -1
  }

  const expand = i => expandAt(xs[i], ys[i], zs[i], hs[i], slow[i], i, shapes[i] & 15)

  // The step after leaving a soul-sand column sideways must not be into a magma column cell, unless the path is going down it
  // (the goal well below the cell): the body slips in. Set per expansion, for the sideways entries from land.
  let afterExit = false
  const magmaTrap = (x, y, z) => afterExit && bubble[rawAt(x, y, z)] === 2 && !(goal.y < y - 1)

  let quiet = false
  const tightAt = (x, y, z) => !quiet && isTight(x, y, z)

  // region -1: every region of a tight cell (the goal flood does not know which one it comes from)
  const expandMoves = (x, y, z, h, slowFrom, i, region) => {
    if (kind[rawAt(x, y, z)] === WATER) return expandSwim(x, y, z, i, region)
    afterExit = i >= 0 && moves[i] === MOVE.EXIT
    const h0 = y * 16 + h
    const tightSrc = tightAt(x, y, z)
    // (the same quiet test as for tight cells: no climbable within reach of the cell either)
    const climbing = !quiet && climbHere(x, y, z)
    if (climbing) {
      climbUp(i, x, y, z, h, region)
      climbDown(i, x, y, z, h, region)
    } else if (!quiet) {
      jumpClimb(i, x, y, z, h, region)
      if (climbHere(x, y - 1, z)) climbDown(i, x, y, z, h, region) // standing on scaffolding: sneak down into it
    }

    for (let c = 0; c < 4; c++) {
      const x2 = x + CARDINAL[c][0]
      const z2 = z + CARDINAL[c][1]
      const h1 = neighbour(x2, z2, y, h0)
      if (h1 >= 0) {
        const delta = ty * 16 + h1 - h0
        const walk = WALK_S * (1 + SLOW_EXTRA * (slowFrom + enterSlow))
        // climbing a stairs block in its direction is a walk, though the node above it is a whole block up
        const climbs = stairUp[support] === c + 1 && delta <= WHOLE
        // (the body steps off a climbable without a jump: it is already rising)
        const walks = delta <= STEP || climbs || (climbing && delta <= JUMP_UP)
        // a jump needs headroom over the start column; a tight start's mask checks that itself
        if (!walks && !(delta <= JUMP_UP && (tightSrc || clear(x, z, h0, ty * 16 + h1 + BODY)))) continue
        // a walk up (a stairs, a slab, a deep snow layer) lifts the body into the slab above its old top: that must be free in the start column
        if (walks && delta > 0 && !tightSrc && !climbing && !clear(x, z, h0 + BODY, ty * 16 + h1 + BODY)) continue
        const sec = (walks ? walk : walk + JUMP_S) + enterExtra
        const move = walks ? MOVE.WALK : MOVE.JUMP
        if (tightSrc || tightAt(x2, ty, z2)) tightMove(i, x, y, z, h, region, c, x2, ty, z2, h1, move, sec, enterRisk, enterSlow, climbing && delta > STEP ? GRID : SNAP)
        else edge(x2, ty, z2, h1, move, i, sec, enterRisk, enterSlow)
        continue
      }
      // water ahead at our level: walk in and swim
      if (swimAt(x2, y, z2) >= 0) {
        if (magmaTrap(x2, y, z2)) continue
        const risk = swimRisk(x2, y, z2)
        const tight = tightSrc || tightAt(x2, y, z2)
        swimEdge(i, true, x2, y, z2, costs.swimH, flowing[rawAt(x2, y, z2)] === 1 ? costs.current : 0, false, dsec => tight
          ? tightMove(i, x, y, z, h, region, c, x2, y, z2, 0, MOVE.SWIM, dsec, risk, 0)
          : edge(x2, y, z2, 0, MOVE.SWIM, i, dsec, risk, 0))
        continue
      }
      // no ground ahead at our level: the body must at least fit in the column to leave the edge
      if (!clear(x2, z2, h0, h0 + BODY)) continue
      expandDrop(i, x, y, z, h, region, c, x2, z2, h0, slowFrom, tightSrc)
      if (!tightSrc && !openMode) expandGap(i, x, y, z, c, h0) // no gap jumps out of a tight cell, none over a door (in the opening pass)
    }

    if (tightSrc) return
    for (let c = 0; c < 4; c++) {
      const dx = DIAGONAL[c][0]
      const dz = DIAGONAL[c][1]
      const x2 = x + dx
      const z2 = z + dz
      const h1 = neighbour(x2, z2, y, h0)
      if (h1 < 0) continue
      const y2 = ty
      if (tightAt(x2, y2, z2)) continue
      const h2 = y2 * 16 + h1
      const jump = h2 - h0 > STEP
      if (h2 - h0 > JUMP_UP) continue
      // the 0.62 wide body brushes both side cells near their shared corner, for its whole height: neither may hold anything
      // it must not touch (water, or a hole, is fine to pass). One side holding plain collision is a corner slide: the body
      // presses on it and slides over the other side cell (dipping there if it has no floor, the step height lifts it out)
      const lo = Math.min(h0, h2)
      const hi = Math.max(h0, h2) + BODY
      const sa = side(x + dx, z, lo, hi)
      const sb = side(x, z + dz, lo, hi)
      if (sa === 2 || sb === 2 || (sa === 1 && sb === 1)) continue
      if (h2 > h0 && !clear(x, z, jump ? h0 : h0 + BODY, hi)) continue // a step up lifts the body: its start column must have the room
      const slide = sa + sb // 1 when exactly one side is blocked
      const walk = WALK_S * SQRT2 * (1 + SLOW_EXTRA * (slowFrom + enterSlow)) + slide * CORNER_S
      const code = jump ? MOVE.JUMP : slide ? MOVE.CORNER : MOVE.DIAGONAL
      edge(x2, y2, z2, h1, code, i, (jump ? walk + JUMP_S : walk) + enterExtra, enterRisk, enterSlow, slide)
    }
  }

  // ---- doors, gates and trapdoors ----

  // Every expansion near a closed door, gate or trapdoor runs twice: once with the world as it stands, then in the opening pass
  // with those blocks open. An edge only the second pass finds is a move that needs something opened: it costs costs.open per
  // block opened by hand (costs.openRedstone for an iron door's button or lever, costs.openPlate for a plate, which the body
  // steps on anyway) and records step.opens. A body that stands in a door's cell has already opened it: the edges out of
  // there come from the second pass too, for nothing. There is no timing model: a door closes again about 1 s after a plate
  // is left (stone plate), 1 s after a stone button was pressed (1.5 s for wood); the executor deals with that.
  const nearOpenable = (x, y, z) => {
    for (let cz = z - 1; cz <= z + 1; cz++) {
      for (let cx = x - 1; cx <= x + 1; cx++) {
        for (let cy = y - 1; cy <= y + 2; cy++) if (openable[rawAt(cx, cy, cz)] > 0) return true
      }
    }
    return false
  }

  // the closed openable blocks in the body column of a node at (x, y, z) standing h/16 up, a door by its lower half: [{ x, y, z, id }]
  const closedIn = (x, y, z, h) => {
    const out = []
    const last = (y * 16 + h + BODY - 1) >> 4
    for (let k = y; k <= last; k++) {
      const id = rawAt(x, k, z)
      if (!(openable[id] > 0)) continue
      const by = doorHalf[id] === 2 ? k - 1 : k
      if (!out.some(b => b.y === by)) out.push({ x, y: by, z, id: doorHalf[id] === 2 ? rawAt(x, by, z) : id })
    }
    return out
  }

  // an activator for an iron door at `door`, for a body in the cell (sx, sy, sz) in front of it: a plate in that cell, or a button
  // or lever on the body's side of the door within 4 blocks, on a block next to the door's frame. null when there is none.
  const activators = new Map()
  const activatorFor = (door, sx, sy, sz, side) => {
    const key = `${door.x},${door.y},${door.z}|${sx},${sy},${sz}|${side}`
    const hit = activators.get(key)
    if (hit !== undefined) return hit
    const found = findActivator(door, sx, sy, sz, side)
    activators.set(key, found)
    return found
  }
  const ATTACH = [null, [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1], [0, 1, 0], [0, -1, 0]]
  const findActivator = (door, sx, sy, sz, side) => {
    if (activator[rawAt(sx, sy, sz)] === ACT_PLATE && Math.abs(door.x - sx) + Math.abs(door.z - sz) === 1) return { via: 'plate', at: { x: sx, y: sy, z: sz } }
    const alongX = facing[door.id] <= 2
    let best = null
    let bestD = Infinity
    for (let dz = -4; dz <= 4; dz++) {
      for (let dy = -2; dy <= 4; dy++) {
        for (let dx = -4; dx <= 4; dx++) {
          const d = dx * dx + dy * dy + dz * dz
          if (d > 16 || d >= bestD) continue
          const x = sx + dx
          const y = sy + dy
          const z = sz + dz
          const id = rawAt(x, y, z)
          const act = activator[id]
          if (act !== ACT_BUTTON && act !== ACT_LEVER) continue
          if (Math.sign(alongX ? x - door.x : z - door.z) !== side) continue
          const [ax, ay, az] = ATTACH[attach[id]] ?? [0, 0, 0]
          if (Math.max(Math.abs(x + ax - door.x), Math.abs(y + ay - door.y), Math.abs(z + az - door.z)) > 2) continue
          best = { via: act === ACT_BUTTON ? 'button' : 'lever', at: { x, y, z } }
          bestD = d
        }
      }
    }
    return best
  }

  // what a move from the cell (sx, sy, sz) into a node column holding the closed blocks `blocks` opens: { list, seconds }, or null
  // when an iron door among them has nothing to open it
  const opening = (sx, sy, sz, dx, dz, blocks) => {
    const list = []
    let seconds = 0
    for (const b of blocks) {
      const iron = openable[b.id] === OPEN_REDSTONE
      // the body's side of the door: where it is, or, standing in the door's cell, where it came from (opposite its way on)
      const alongX = facing[b.id] <= 2
      const side = Math.sign(alongX ? sx - b.x : sz - b.z) || -Math.sign(alongX ? dx - b.x : dz - b.z)
      const found = iron || activator[rawAt(sx, sy, sz)] === ACT_PLATE ? activatorFor(b, sx, sy, sz, side) : null
      const act = iron || found?.via === 'plate' ? found : null // a wooden door is opened by hand unless a plate does it
      if (iron && act === null) return null
      if (act === null) {
        list.push({ x: b.x, y: b.y, z: b.z })
        seconds += costs.open
        continue
      }
      list.push({ x: b.x, y: b.y, z: b.z, via: act.via, at: act.at })
      seconds += act.via === 'plate' ? costs.openPlate : costs.openRedstone
    }
    return { list, seconds }
  }

  const expandAt = (x, y, z, h, slowFrom, i, region = -1) => {
    // when no section near the cell holds a partial block or a climbable, no cell this expansion looks at is tight
    quiet = sectionsClear(x, y, z, 2, 2, 3)
    if (quiet || !nearOpenable(x, y, z)) return expandMoves(x, y, z, h, slowFrom, i, region)
    const sink = edge
    const seen = new Set()
    edge = (x2, y2, z2, h2, move, parent, dsec, drisk, slowTo, corner = 0, shape = 0) => {
      seen.add(keyOf(x2, y2, z2, shape & 15))
      sink(x2, y2, z2, h2, move, parent, dsec, drisk, slowTo, corner, shape)
    }
    expandMoves(x, y, z, h, slowFrom, i, region)
    // what the move into the node here opened is open still; a body standing in a gate's cell without having opened it is not
    const arrival = i >= 0 && opens[i] > 0 ? openLists[opens[i] - 1] : []
    const same = (a, b) => a.x === b.x && a.y === b.y && a.z === b.z
    const here = closedIn(x, y, z, h)
    const through = here.some(b => arrival.some(o => same(o, b)))
    edge = (x2, y2, z2, h2, move, parent, dsec, drisk, slowTo, corner = 0, shape = 0) => {
      if (seen.has(keyOf(x2, y2, z2, shape & 15))) return
      const involved = [...here, ...closedIn(x2, y2, z2, h2).filter(b => !here.some(o => same(o, b)))]
      const blocks = involved.filter(b => !arrival.some(o => same(o, b)))
      if (blocks.length === 0 && !through) return // only the open world allows it, and nothing was opened: not a move
      const opened = opening(x, y, z, x2, z2, blocks)
      if (opened === null) return
      moveOpen = opened.list.length > 0 ? openLists.push(opened.list) : 0
      sink(x2, y2, z2, h2, move, parent, dsec + opened.seconds, drisk, slowTo, corner, shape)
      moveOpen = 0
    }
    openMode = true
    expandMoves(x, y, z, h, slowFrom, i, region)
    openMode = false
    edge = sink
  }

  // ---- climbing ----

  let gapSeen = false // a ladder was refused because the feet would leave it at a gap

  // the cell (x, y2, z) as the next node of a vertical move: standH with a shut trapdoor allowed (its cost added), or -1
  let entersShut = false
  const enterCell = (x, y2, z) => {
    const was = allowShut
    allowShut = true
    entersShut = shutAt(x, y2, z)
    const h2 = landing(x, y2, z)
    allowShut = was
    return h2
  }

  // a climbable one block up (or the deck of scaffolding, or a trapdoor to open); a gap above refuses the climb
  const climbUp = (i, x, y, z, h, region) => {
    const h2 = enterCell(x, y + 1, z)
    if (h2 < 0) {
      for (let k = 2; k <= 3 && !gapSeen; k++) {
        const free = id => id !== UNLOADED && top[id] === 0 && kind[id] !== WATER && kind[id] !== LAVA
        gapSeen = climbHere(x, y + k, z) && free(stateAt(x, y + 1, z)) && (k === 2 || free(stateAt(x, y + 2, z)))
      }
      return
    }
    const sec = costs.climbUp + (entersShut ? costs.open : 0)
    if (entersShut) moveOpen = openLists.push([{ x, y: y + 1, z }])
    verticalMove(i, x, y, z, h, region, y + 1, h2, entersShut ? MOVE.OPEN : MOVE.CLIMB_UP, sec, enterRisk, enterSlow)
    moveOpen = 0
  }

  // a climbable or standable cell one block down; else, through free cells, a short fall onto the first climbable or floor
  const climbDown = (i, x, y, z, h, region) => {
    const h2 = enterCell(x, y - 1, z)
    if (h2 >= 0) {
      const sec = costs.climbDown + (entersShut ? costs.open : 0)
      if (entersShut) moveOpen = openLists.push([{ x, y: y - 1, z }])
      verticalMove(i, x, y, z, h, region, y - 1, h2, entersShut ? MOVE.OPEN : MOVE.CLIMB_DOWN, sec, enterRisk, enterSlow)
      moveOpen = 0
      return
    }
    const free = stateAt(x, y - 1, z)
    if (free === UNLOADED || top[free] > 0 || kind[free] === WATER || kind[free] === LAVA || avoids(free, x, y - 1, z)) return
    const from16 = y * 16 + h
    for (let y3 = y - 2; y3 >= y - maxDrop - 1; y3--) {
      const id = stateAt(x, y3, z)
      if (id === UNLOADED || kind[id] === LAVA || kind[id] === WATER || avoids(id, x, y3, z)) return
      const h3 = landing(x, y3, z)
      if (h3 < 0) {
        if (top[id] > 0) return
        continue
      }
      const fall = from16 - (y3 * 16 + h3)
      if (fall > maxDrop * 16) return
      verticalMove(i, x, y, z, h, region, y3, h3, MOVE.DROP, 0.25 * Math.sqrt(fall / 16), enterRisk + fallDamage(fall), enterSlow)
      return
    }
  }

  // from the floor, a jump puts the feet into a climbable one block up
  const jumpClimb = (i, x, y, z, h, region) => {
    const id = rawAt(x, y + 1, z)
    if (id === UNLOADED || climb[id] === 0 || climb[id] === CLIMB_TRAP_SHUT) return
    const h2 = landing(x, y + 1, z)
    if (h2 < 0) return
    verticalMove(i, x, y, z, h, region, y + 1, h2, MOVE.JUMP_CLIMB, costs.jumpClimb, enterRisk, enterSlow)
  }

  // walk off an edge into the first standable cell below the neighbour column, or into water of any depth up to maxWaterDrop
  // (the fall is cancelled there). A tight cell at either end: the body falls straight down from the crossing point, which the
  // masks must leave free all the way (a climbable below is grabbed as it falls past, so it takes any position of its cell).
  const expandDrop = (i, x, y, z, h, region, c, x2, z2, h0, slowFrom, tightSrc) => {
    for (let y2 = y - 1; y2 >= y - costs.maxWaterDrop - 1; y2--) {
      const id = stateAt(x2, y2, z2)
      if (id === UNLOADED || kind[id] === LAVA || avoids(id, x2, y2, z2)) return
      if (kind[id] === WATER) return magmaTrap(x2, y2, z2) ? undefined : dropIntoWater(i, x, y, z, h, region, c, x2, y2, z2, h0, slowFrom, tightSrc)
      const h1 = landing(x2, y2, z2)
      if (h1 < 0) {
        if (top[id] > 0) return
        continue
      }
      const tightDrop = tightSrc || isTight(x2, y2, z2)
      const fall = h0 - (y2 * 16 + h1)
      if (fall > maxDrop * 16) return
      const sec = WALK_S * (1 + SLOW_EXTRA * (slowFrom + enterSlow)) + 0.25 * Math.sqrt(Math.max(0, fall) / 16) + enterExtra
      if (tightDrop) tightMove(i, x, y, z, h, region, c, x2, y2, z2, h1, MOVE.DROP, sec, enterRisk + fallDamage(fall), enterSlow, SNAP, climbHere(x2, y2, z2) ? GRID : 0)
      else edge(x2, y2, z2, h1, MOVE.DROP, i, sec, enterRisk + fallDamage(fall), enterSlow)
      return
    }
  }

  const dropIntoWater = (i, x, y, z, h, region, c, x2, y2, z2, h0, slowFrom, tightSrc) => {
    const fall = h0 - y2 * 16
    if (swimAt(x2, y2, z2) < 0 || fall > costs.maxWaterDrop * 16) return
    const sec = WALK_S * (1 + SLOW_EXTRA * slowFrom) + 0.25 * Math.sqrt(fall / 16)
    if (tightSrc || isTight(x2, y2, z2)) tightMove(i, x, y, z, h, region, c, x2, y2, z2, 0, MOVE.DROP, sec, swimRisk(x2, y2, z2), 0, SNAP, 0)
    else edge(x2, y2, z2, 0, MOVE.DROP, i, sec, swimRisk(x2, y2, z2), 0)
  }

  // sprint across 1..3 empty cells in a cardinal line, landing level or one lower; across 1 or 2, also up to one block higher
  const expandGap = (i, x, y, z, c, h0) => {
    const dx = CARDINAL[c][0]
    const dz = CARDINAL[c][1]
    let hole = 0
    let upArc = clear(x, z, h0, h0 + ARC_UP) // a jump up needs the higher arc over the start and every gap cell
    for (let n = 1; n <= 3; n++) {
      const gx = x + dx * n
      const gz = z + dz * n
      if (n > 1 && (landing(gx, y, gz) >= 0 || landing(gx, y + 1, gz) >= 0)) return
      if (!clear(gx, gz, h0, h0 + ARC)) return
      upArc &&= clear(gx, gz, h0, h0 + ARC_UP)
      hole = Math.max(hole, holeRisk(gx, y, gz))
      const lx = gx + dx
      const lz = gz + dz
      let ly = y
      let h1 = landing(lx, y, lz)
      if (h1 < 0 && n <= 2 && upArc) {
        ly = y + 1
        h1 = landing(lx, ly, lz)
      }
      if (h1 < 0) {
        ly = y - 1
        h1 = landing(lx, ly, lz)
      }
      if (h1 < 0 || isTight(lx, ly, lz)) continue
      const delta = ly * 16 + h1 - h0
      if (delta < -16 || delta > (n <= 2 && upArc ? WHOLE : 0)) continue
      edge(lx, ly, lz, h1, MOVE.GAP, i, (n + 1) * SPRINT_S + GAP_S + (delta > 0 ? GAP_UP_S : 0) + enterExtra, enterRisk + hole, enterSlow)
    }
  }

  // ---- the search ----

  let finished = false
  let reason = null
  let goalNode = -1
  let best = -1 // expanded node nearest the goal
  let bestDistance = Infinity
  let startDistance = 0
  let expanded = 0
  let started = false
  let t0 = performance.now()
  let elapsed = 0

  const startH = nodeH(from.x, from.y, from.z)
  const startSlow = hazard[support] === SLOW && !isWater(from.x, from.y, from.z) ? 1 : 0 // before goalNotStandable reuses standH
  const goalNotStandable = () => {
    if (!near || goalUnloaded) return false
    const r = Math.ceil(goalRange)
    for (let dx = -r; dx <= r; dx++) {
      for (let dy = -r; dy <= r; dy++) {
        for (let dz = -r; dz <= r; dz++) {
          if (dx * dx + dy * dy + dz * dz > goalRange * goalRange) continue
          if (nodeH(goal.x + dx, goal.y + dy, goal.z + dz) >= 0) return false
        }
      }
    }
    return true
  }

  // Backward flood from the standable goal cells over predecessors: cells n with a forward move n -> c, found by running n's
  // own moves (so it can never disagree with the search). True when it exhausts within `budget` nodes without meeting the
  // start: then nothing reaches the goal. Slow, but bounded by the budget; false on budget or when the start is met.
  let flooded = 0
  let preFlooded = 0
  // With `sealed`, an enclosed region also counts only when it has no cliff edge (the early pass).
  const goalEnclosed = (budget, sealed = false) => {
    const inSpan = (x, z) => x - from.x + HALF >= 0 && x - from.x + HALF < SPAN && z - from.z + HALF >= 0 && z - from.z + HALF < SPAN
    const startKey = keyOf(from.x, from.y, from.z)
    const seen = new Set()
    const queue = []
    let fx = 0
    let fy = 0
    let fz = 0
    let hit = false
    let sawWater = false // a water cell in the flood: drops into water start further up than the flood looks
    const probe = (x, y, z) => { if (x === fx && y === fy && z === fz) hit = true }
    // adds n when it is standable and a forward move reaches the flood's current cell; true when n is the start
    const visit = (x, y, z) => {
      if (!inSpan(x, z)) return false
      const key = keyOf(x, y, z)
      if (seen.has(key)) return false
      const h = nodeH(x, y, z)
      if (h < 0) return false
      hit = false
      expandAt(x, y, z, h, 0, -1, -1)
      if (!hit) return false
      seen.add(key)
      queue.push([x, y, z])
      sawWater ||= isWater(x, y, z)
      return key === startKey
    }
    const r = Math.ceil(goalRange)
    const seed = []
    for (let dx = -r; dx <= r; dx++) {
      for (let dy = -r; dy <= r; dy++) {
        for (let dz = -r; dz <= r; dz++) {
          if (dx * dx + dy * dy + dz * dz > goalRange * goalRange) continue
          const x = goal.x + dx
          const y = goal.y + dy
          const z = goal.z + dz
          if (!inSpan(x, z) || nodeH(x, y, z) < 0) continue
          seen.add(keyOf(x, y, z))
          queue.push([x, y, z])
          seed.push(keyOf(x, y, z))
          sawWater ||= isWater(x, y, z)
        }
      }
    }
    if (seed.includes(startKey)) return false
    edge = probe
    allowShut = true // a shut trapdoor is a way through, only dearer: the flood must not call its far side enclosed
    let open = false
    for (let head = 0; head < queue.length && !open && seen.size <= budget; head++) {
      ;[fx, fy, fz] = queue[head]
      for (let dy = -1; dy <= maxDrop + 1; dy++) if (dy !== 0) open ||= visit(fx, fy + dy, fz) // climbs and falls in the column
      for (const [dx, dz] of AROUND) {
        for (let dy = -1; dy <= maxDrop + 1; dy++) open ||= visit(fx + dx, fy + dy, fz + dz)
      }
      for (const [dx, dz] of CARDINAL) {
        for (let n = 2; n <= 4; n++) {
          for (let dy = 0; dy <= 1; dy++) open ||= visit(fx + dx * n, fy + dy, fz + dz * n)
        }
      }
    }
    const enclosed = !open && !sawWater && seen.size <= budget
    const leaks = enclosed && sealed && leaksOut(queue)
    edge = consider
    allowShut = false
    flooded = seen.size
    return enclosed && !leaks
  }

  // a body-high free column beside the cell with nothing to stand on within a drop below it: a cliff or a gap, an edge the body
  // can walk off to nowhere (a wall top above the floor is no cliff: its drop lands)
  const cliffBeside = (x, y, z, h) => CARDINAL.some(([dx, dz]) => {
    if (!clear(x + dx, z + dz, y * 16 + h, y * 16 + h + BODY)) return false
    for (let dy = -maxDrop - 1; dy <= 1; dy++) if (nodeH(x + dx, y + dy, z + dz) >= 0) return false
    return true
  })

  // true when the flooded region has a cliff edge: the goal sits on an island or above a drop, not in a pocket. (Not "a move lands
  // outside": a jump over a one block wall does, and a goal walled in on all sides is still enclosed then.)
  const leaksOut = queue => queue.some(([x, y, z]) => cliffBeside(x, y, z, nodeH(x, y, z)))

  // the early pass of the flood, before the first expansion: only a small enclosed goal is caught, so it stays cheap, and only a
  // sealed one (no cliff edge beside the flooded cells: a goal on an island or above a drop is left to the search). It leaves
  // `flooded` and `expanded` alone (the late flood owns them) and reports its size as stats.preFlooded.
  const goalEnclosedEarly = () => {
    const budget = Math.min(preFlood, goalFlood)
    if (!near || budget <= 0 || goalUnloaded) return false
    const enclosed = goalEnclosed(budget, true)
    preFlooded = flooded
    flooded = 0
    return enclosed
  }

  const finish = why => {
    finished = true
    reason = why
    elapsed = performance.now() - t0
  }

  const regionAtStart = () => {
    if (!isTight(from.x, from.y, from.z)) return { region: 0, shape: 0 }
    const { mask, labels, regs } = shapeOf(from.x, from.y, from.z, from.y * 16 + startH)
    const wantX = ((from.px ?? from.x + 0.5) - from.x) * 16
    const wantZ = ((from.pz ?? from.z + 0.5) - from.z) * 16
    let best = -1
    let bestD = Infinity
    for (let k = 0; k < mask.length; k++) {
      if (!mask[k]) continue
      const d = (k % GRID - wantX) ** 2 + (Math.floor(k / GRID) - wantZ) ** 2
      if (d < bestD) { best = k; bestD = d }
    }
    if (best < 0 || labels[best] >= REGIONS) return null
    const region = labels[best]
    return { region, shape: packShape(region, true, regs[region].px, regs[region].pz, false, 0, 0) }
  }

  const begin = () => {
    started = true
    t0 = performance.now()
    if (startH < 0) return finish('start-not-standable')
    if (goalNotStandable()) return finish('goal-not-standable')
    startDistance = distanceTo(from.x, from.z)
    // the start is node 0; in a tight cell its region is the one holding the free position nearest where the body is
    const startRegion = regionAtStart()
    if (startRegion === null) return finish('start-not-standable')
    shapes[0] = startRegion.shape
    keys[0] = keyOf(from.x, from.y, from.z, startRegion.region)
    xs[0] = from.x
    ys[0] = from.y
    zs[0] = from.z
    hs[0] = startH
    parent[0] = -1
    gs[0] = 0
    fs[0] = heuristic(from.x, from.z)
    hashTable[hashOf(from.x, from.y, from.z, startRegion.region) & (slots - 1)] = 0
    slow[0] = startSlow
    count = 1
    heapN = 1
    heap[0] = 0
    heapPos[0] = 0
  }

  // the goal flood costs ~30 ms, so easy queries must never see it: it runs once, after floodAfter forward expansions
  let floodPending = near && goalFlood > 0

  const endsOnMagma = (x, y, z, h) => bubble[rawAt(x, y, z)] === 2 || h === 0 && magma[stateAt(x, y - 1, z)] === 1

  // ---- steps the body cannot undo ----

  // Can the planner's own moves take the body from the lower cell back to the upper one? Runs the moves out of the lower cell
  // and looks for the one that enters the upper cell (what the goal flood's probe does for its cells).
  const canReturn = (lx, ly, lz, lh, ux, uy, uz) => {
    let hit = false
    edge = (x, y, z) => { if (x === ux && y === uy && z === uz) hit = true }
    expandAt(lx, ly, lz, lh, 0, -1, -1)
    edge = consider
    return hit
  }

  // A one-way step cannot be undone with the body's own moves: a gap jump down (the way back is a gap jump up, which the walker
  // refuses), a drop of more than JUMP_UP, or a drop the planner has no step-up move back from. A drop of exactly 1 is returnable
  // exactly when the planner's step up is legal from the lower cell. Everything else (walk, diagonal, corner, jump, climbs, swims,
  // exit) has a reverse.
  const isOneWay = node => {
    const p = parent[node]
    if (moves[node] === MOVE.GAP) return stand16(node) < stand16(p)
    if (moves[node] !== MOVE.DROP) return false
    return stand16(p) - stand16(node) > JUMP_UP || !canReturn(xs[node], ys[node], zs[node], hs[node], xs[p], ys[p], zs[p])
  }

  // the first one-way step on the way to the node (nearest the start), or -1
  const firstOneWay = node => {
    let first = -1
    for (let i = node; parent[i] !== -1; i = parent[i]) if (isOneWay(i)) first = i
    return first
  }

  const flushHeld = () => {
    replaying = true
    for (const [x, y, z, h, move, p, dsec, drisk, slowTo, corner, shape, open, air, peak, water] of held.splice(0)) {
      if (!canReturn(x, y, z, h, xs[p], ys[p], zs[p])) continue
      moveOpen = open
      moveAir = air
      movePeak = peak
      moveWater = water
      consider(x, y, z, h, move, p, dsec, drisk, slowTo, corner, shape)
    }
    replaying = false
    moveOpen = moveAir = movePeak = moveWater = 0
  }

  const step = maxExpansions => {
    if (!started) {
      begin()
      if (!finished && goalEnclosedEarly()) finish('goal-enclosed')
    }
    for (let n = 0; n < maxExpansions && !finished; n++) {
      if (floodPending && expanded >= floodAfter && !goalUnloaded) {
        floodPending = false
        const enclosed = goalEnclosed(goalFlood)
        expanded += flooded
        n += flooded // the flood counts toward the slice
        if (enclosed) { finish('goal-enclosed'); break }
      }
      if (heapN === 0) { finish(boxed ? 'box' : 'exhausted'); break }
      const i = pop()
      expanded++
      // the body died idling on magma: a route never ends on a magma block or in a magma column, so such a node is passed
      // through (it is searched on from) but is neither the goal nor the best partial end
      const deadly = endsOnMagma(xs[i], ys[i], zs[i], hs[i])
      if (!deadly && reached(xs[i], ys[i], zs[i])) {
        goalNode = i
        finish(null)
        break
      }
      const d = distanceTo(xs[i], zs[i])
      if (!deadly && d < bestDistance) { bestDistance = d; best = i }
      expand(i)
      if (held.length > 0) flushHeld()
      if (overBudget) finish('budget')
    }
    return finished
  }

  // ---- results ----

  const stepsTo = node => {
    const out = []
    for (let i = node; i !== -1; i = parent[i]) {
      const shape = shapes[i]
      const tight = (shape >> 4 & 1) === 1
      const step = {
        x: xs[i], y: ys[i], z: zs[i], h: hs[i], move: moves[i], corner: corners[i] === 1,
        px: xs[i] + (tight ? (shape >> 5 & 31) / 16 : 0.5), pz: zs[i] + (tight ? (shape >> 10 & 31) / 16 : 0.5)
      }
      if (isWater(xs[i], ys[i], zs[i])) step.swim = true
      if (opens[i] > 0) step.opens = openLists[opens[i] - 1]
      if (shape >> 15 & 1) { step.cx = xs[i] + (shape >> 16 & 31) / 16; step.cz = zs[i] + (shape >> 21 & 31) / 16 }
      out.push(step)
    }
    return out.reverse()
  }

  // "ladder up 9": consecutive climbing legs on one kind of climbable in one direction are one run
  const CLIMB_NAMES = { [LADDER]: 'ladder', [VINES]: 'vines', [SCAFFOLDING]: 'scaffolding' }
  const climbedOn = (s, p) => CLIMB_NAMES[climbName[rawAt(s.x, s.y, s.z)] || climbName[rawAt(p.x, p.y, p.z)]]
  const climbRuns = legs => {
    const runs = []
    let prev = null
    for (const { s, p } of legs) {
      if (!CLIMBS.has(s.move)) { prev = null; continue }
      const key = `${climbedOn(s, p)} ${s.y > p.y ? 'up' : 'down'}`
      if (prev?.key === key) prev.n++
      else runs.push(prev = { key, n: 1 })
    }
    return runs.map(({ key, n }) => `${key} ${n}`)
  }

  // "water column up 20", "bubble lift up 18", "magma column down 15": consecutive vertical swim legs of one kind in one direction
  const swimRuns = legs => {
    const runs = []
    let prev = null
    for (const { s, p } of legs) {
      if (s.move !== MOVE.SWIM_UP && s.move !== MOVE.SWIM_DOWN) { prev = null; continue }
      const bubbles = Math.max(bubble[rawAt(s.x, s.y, s.z)], bubble[rawAt(p.x, p.y, p.z)])
      const key = s.move === MOVE.SWIM_UP ? (bubbles === 1 ? 'bubble lift up' : 'water column up') : (bubbles === 2 ? 'magma column down' : 'water column down')
      if (prev?.key === key) prev.n++
      else runs.push(prev = { key, n: 1 })
    }
    return runs.map(({ key, n }) => `${key} ${n}`)
  }
  const isSwim = move => move === MOVE.SWIM || move === MOVE.SWIM_UP || move === MOVE.SWIM_DOWN || move === MOVE.EXIT
  const intoWater = ({ s }) => s.move === MOVE.DROP && isWater(s.x, s.y, s.z)

  // "opens 2 doors", "opens 1 gate", "presses 1 button", "steps on 1 plate": what the path opens, by what opens it
  const OPENED = [[KIND_DOOR, 'door'], [KIND_GATE, 'gate'], [3, 'trapdoor']]
  const VIA = { button: ['presses', 'button'], lever: ['pulls', 'lever'], plate: ['steps on', 'plate'] }
  const openRuns = steps => {
    const all = steps.flatMap(s => s.opens ?? [])
    const phrase = (verb, noun, n) => n > 0 && `${verb} ${n} ${noun}${n > 1 ? 's' : ''}`
    const byHand = ([kindCode, noun]) => phrase('opens', noun, all.filter(o => o.via === undefined && openKind[rawAt(o.x, o.y, o.z)] === kindCode).length)
    const byVia = ([via, [verb, noun]]) => phrase(verb, noun, all.filter(o => o.via === via).length)
    return [...OPENED.map(byHand), ...Object.entries(VIA).map(byVia)].filter(Boolean)
  }

  const summarize = (steps, node) => {
    const legs = steps.slice(1).map((s, k) => ({ s, p: steps[k] }))
    const blocks = Math.round(legs.reduce((sum, { s, p }) => sum + Math.hypot(s.x - p.x, s.z - p.z), 0))
    const rise = ({ s, p }) => !CLIMBS.has(s.move) && !isSwim(s.move) && s.move !== MOVE.DROP && s.move !== MOVE.GAP && s.y * 16 + s.h > p.y * 16 + p.h
    const ups = legs.filter(rise).length
    const depth = ({ s, p }) => Math.round((p.y * 16 + p.h - s.y * 16 - s.h) / 16)
    const falls = legs.filter(({ s }) => s.move === MOVE.DROP && !isWater(s.x, s.y, s.z)).map(depth).filter(f => f >= 2)
    const splashes = legs.filter(intoWater).map(depth).filter(f => f >= 2)
    const swum = Math.round(legs.filter(({ s }) => s.move === MOVE.SWIM || s.move === MOVE.CORNER && s.swim).reduce((sum, { s, p }) => sum + Math.hypot(s.x - p.x, s.z - p.z), 0))
    const lowest = costs.airSupply - peaks[node]
    const gaps = legs.filter(({ s }) => s.move === MOVE.GAP).length
    const slides = steps.filter(s => s.corner).length
    const gapUps = legs.filter(({ s, p }) => s.move === MOVE.GAP && s.y * 16 + s.h > p.y * 16 + p.h).length
    const lava = steps.some(s => lavaNear(s.x, s.y, s.z))
    return [
      `${blocks} blocks`,
      swum > 0 && `swims ${swum}`,
      ...swimRuns(legs),
      ...climbRuns(legs),
      ...openRuns(steps),
      ups > 0 && (ups === 1 ? '1 step up' : `${ups} steps up`),
      falls.length === 1 && `1 drop of ${falls[0]}`,
      falls.length > 1 && `${falls.length} drops, deepest ${Math.max(...falls)}`,
      splashes.length === 1 && `drops ${splashes[0]} into water`,
      splashes.length > 1 && `${splashes.length} drops into water, deepest ${Math.max(...splashes)}`,
      gaps > 0 && (gaps === 1 ? '1 gap jump' : `${gaps} gap jumps`),
      slides > 0 && (slides === 1 ? '1 corner slide' : `${slides} corner slides`),
      gapUps > 0 && (gapUps === 1 ? '1 jump up over a gap' : `${gapUps} jumps up over gaps`),
      lava && 'passes 1 cell from lava',
      peaks[node] > 0 && `lowest air ${Math.round(lowest)} s`
    ].filter(Boolean).join(', ')
  }

  const pathTo = node => {
    const steps = stepsTo(node)
    const dropOf = (s, k) => (steps[k].y * 16 + steps[k].h - s.y * 16 - s.h) / 16
    const drops = steps.slice(1).map((s, k) => s.move === MOVE.DROP && !s.swim ? dropOf(s, k) : 0)
    const splashes = steps.slice(1).map((s, k) => s.move === MOVE.DROP && s.swim ? dropOf(s, k) : 0)
    const jumps = steps.filter(s => s.move === MOVE.JUMP || s.move === MOVE.GAP || s.move === MOVE.JUMP_CLIMB).length
    const climbed = steps.filter(s => CLIMBS.has(s.move)).length
    const openedCount = steps.reduce((n, s) => n + (s.opens?.length ?? 0), 0)
    const cost = {
      seconds: secs[node], risk: risks[node], maxDrop: Math.max(0, ...drops), jumps, climbed, opens: openedCount, unknown: 0,
      waterSeconds: wsecs[node], airMin: costs.airSupply - peaks[node], waterDrop: Math.max(0, ...splashes)
    }
    return { steps, cost, summary: summarize(steps, node) }
  }

  // The end of a partial plan: the node nearest the goal that the body reached, and can come back from, without a one-way step.
  // When the path to the nearest node of all holds a one-way step, a second search that plans none finds that node; oneWay then
  // says what lies behind the step (its move, the cell it enters, how near the goal the node behind it is), or null when nothing
  // nearer does. { path, distance, oneWay }
  const partialEnd = () => {
    if (best === -1) return { path: null, distance: Infinity, oneWay: null }
    const step = returnable ? -1 : firstOneWay(best)
    if (step === -1) return { path: pathTo(best), distance: bestDistance, oneWay: null }
    const clean = createSearch(snapshot, query, { ...options, returnable: true, goalFlood: 0 })
    clean.step(Infinity)
    const end = clean.nearest()
    const oneWay = bestDistance < end.distance ? { move: moves[step], x: xs[step], y: ys[step], z: zs[step], distance: bestDistance } : null
    return { path: end.path, distance: end.distance, oneWay }
  }

  const nearest = () => ({ path: best === -1 ? null : pathTo(best), distance: bestDistance })

  const result = () => {
    if (!finished) finish('budget')
    // an exhausted search that turned a ladder away at a gap says so
    if (reason === 'exhausted' && gapSeen) reason = 'ladder-gap'
    else if (reason === 'exhausted' && airSeen) reason = 'air'
    const done = (status, path, oneWay = null, why = reason) => {
      stats.flooded = flooded
      stats.preFlooded = preFlooded
      stats.tightCells = tightSeen.size
      return { status, reason: why, ms: elapsed, expanded, stats, path, oneWay }
    }
    if (reason === 'start-not-standable' || reason === 'goal-not-standable') return done('none', null)
    if (reason === null) return done('found', pathTo(goalNode))
    const end = partialEnd()
    if (reason === 'budget') return done('partial', end.path, end.oneWay)
    if (goalUnloaded) return done('partial', end.path, end.oneWay, 'goal-unloaded')
    if (end.path !== null && startDistance - end.distance >= MIN_CLOSER) return done('partial', end.path, end.oneWay)
    return done('none', null, end.oneWay)
  }

  return { step, result, nearest }
}

export function plan (snapshot, query, options) {
  const search = createSearch(snapshot, query, options)
  search.step(Infinity)
  return search.result()
}
