// Pure A* over a snapshot of block state ids. A node is a feet cell; its stand height h (1/16 block above the cell's
// floor) comes from the collision under or in it, so slabs, carpets and snow are half steps, not cell-sized ones.
//
// Standable, as in vanilla with its 0.6 step height: the cell's own collision tops at 1..15/16 from the floor (bed, snow
// layers, slab, chest, soul sand, farmland...): then h = top and the node is that cell; or the cell is free and the one below
// tops at 16/16 (h = 0) or above (a closed gate: h = top - 16). Fences, walls, panes, bars and bamboo (NARROW) are never a
// floor and never walked through. Between nodes a rise of up to STEP is a walk, up to JUMP_UP a jump. A bottom straight stairs
// block entered in its climbing direction is a walk up its whole block, as vanilla's step-up makes it. The body's column,
// 29/16 tall from the stand height, must have no collision, no fluid, no NARROW and no AVOID hazard. Water is not entered at
// all in stage 1 (drops into it are refused too), but a diagonal may brush past it. A search box (margin, yMargin) bounds
// the nodes, and a backward flood from the goal, run only once the search has spent floodAfter expansions, reports a goal nothing can reach.
import { UNLOADED } from './snapshot.mjs'
import { defaultStateTable, WATER, LAVA, NARROW, HAZARD_AVOID, DAMAGE_STAND, DAMAGE_TOUCH, SLOW } from './blocks.mjs'

export const MOVE = { START: 0, WALK: 1, DIAGONAL: 2, JUMP: 3, DROP: 4, GAP: 5, CORNER: 6 }

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
const CORNER_S = 0.15 // a diagonal slid along a blocked corner: slower than a straight one
const SLOW_EXTRA = 0.75 // walking time grows by this much of itself per slow end of a move: both ends soul sand is x2.5
const LAVA_ADJACENT = 0.5 // hp of risk for a step with lava beside the feet
const FREE_FALL = 3
const SQRT2 = Math.SQRT2
const OCTILE_SLACK = 1.0824 // octile length of a vector of length r is at most this times r: keeps the range term admissible
const SPAN = 4096 // nodes further than 2048 blocks from the start in x or z are not searched
const HALF = 2048
const MIN_CLOSER = 2 // an exhausted search is a partial result only when it got this many blocks closer
const WHOLE = 16 // a full block in 1/16

const CARDINAL = [[1, 0], [-1, 0], [0, 1], [0, -1]]
const DIAGONAL = [[1, 1], [1, -1], [-1, 1], [-1, -1]]
const AROUND = [...CARDINAL, ...DIAGONAL]

const nextPow2 = n => 2 ** Math.ceil(Math.log2(Math.max(2, n)))
const fallDamage = fall16 => Math.max(0, Math.ceil(fall16 / 16 - FREE_FALL))

export function createSearch (snapshot, query, options = {}) {
  const {
    maxNodes = 200000, maxDrop = 3, weight = 1, riskWeight = 2,
    table = defaultStateTable(),
    goalFlood = 4000, floodAfter = 3000, margin = 64, yMargin = 48
  } = options
  const { top, base, kind, hazard, stairUp } = table
  const { stateAt, minY } = snapshot
  const { from, goal } = query
  const goalRange = goal.range ?? 0
  const slack = OCTILE_SLACK * goalRange

  // ---- the world, as the body sees it ----

  // is the column at x,z free for a body spanning lo..hi (1/16 absolute)? Also false for fluid, NARROW, AVOID, unloaded.
  const clear = (x, z, lo, hi) => {
    const last = (hi - 1) >> 4
    for (let y = lo >> 4; y <= last; y++) {
      const id = stateAt(x, y, z)
      if (id === UNLOADED) return false
      const t = top[id]
      if (t > 0 && y * 16 + t > lo && y * 16 + base[id] < hi) return false
      const k = kind[id]
      if (k === WATER || k === LAVA || k === NARROW || hazard[id] === HAZARD_AVOID) return false
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
      if (id === UNLOADED || kind[id] === LAVA || hazard[id] === HAZARD_AVOID) return 2
      const t = top[id]
      if (kind[id] === NARROW || (t > 0 && y * 16 + t > lo && y * 16 + base[id] < hi)) blocked = 1
    }
    return blocked
  }

  let support = 0 // state id the last standH stood on
  let touch = 0 // DAMAGE_TOUCH cells inside the last standH body

  // stand height at a feet cell, or -1
  const standH = (x, y, z) => {
    const id = stateAt(x, y, z)
    if (id === UNLOADED) return -1
    const t = top[id]
    let h
    if (t >= WHOLE && base[id] === 0) return -1 // a block, a stairs, a closed gate: not a place to stand in
    if (t > 0 && base[id] === 0) {
      const hz = hazard[id]
      if (kind[id] === NARROW || hz === HAZARD_AVOID || hz === DAMAGE_TOUCH) return -1
      h = t
      support = id
    } else {
      const below = stateAt(x, y - 1, z)
      if (below === UNLOADED) return -1
      const tb = top[below]
      const hz = hazard[below]
      // a lower top is that cell's own stand height, not ground for this one
      if (tb < WHOLE || kind[below] === NARROW || hz === HAZARD_AVOID || hz === DAMAGE_TOUCH) return -1
      h = tb - WHOLE
      support = below
    }
    const lo = y * 16 + h
    const hi = lo + BODY
    const last = (hi - 1) >> 4
    let touched = 0
    for (let k = y; k <= last; k++) {
      const cid = k === y ? id : stateAt(x, k, z)
      if (cid === UNLOADED) return -1
      const ct = top[cid]
      if (ct > 0 && k * 16 + ct > lo && k * 16 + base[cid] < hi) return -1
      const kd = kind[cid]
      if (kd === WATER || kd === LAVA || kd === NARROW || hazard[cid] === HAZARD_AVOID) return -1
      if (hazard[cid] === DAMAGE_TOUCH) touched++
    }
    touch = touched
    return h
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

  // standH plus what arriving there costs; -1 when not standable
  const landing = (x, y, z) => {
    const h = standH(x, y, z)
    if (h < 0) return -1
    const hz = hazard[support]
    enterRisk = touch + (hz === DAMAGE_STAND ? 1 : 0) + (lavaNear(x, y, z) ? LAVA_ADJACENT : 0)
    enterSlow = hz === SLOW ? 1 : 0
    return h
  }

  // ---- goal ----

  const near = goal.kind === 'near'
  const reached = near
    ? (x, y, z) => (x - goal.x) ** 2 + (y - goal.y) ** 2 + (z - goal.z) ** 2 <= goalRange * goalRange
    : (x, y, z) => (x - goal.x) ** 2 + (z - goal.z) ** 2 <= goalRange * goalRange
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
  let parent = new Int32Array(cap)
  let secs = new Float64Array(cap)
  let risks = new Float64Array(cap)
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
  const hashOf = (x, y, z) => (Math.imul(x - from.x + HALF, 73856093) ^ Math.imul(y - minY, 19349663) ^ Math.imul(z - from.z + HALF, 83492791)) >>> 0
  const keyOf = (x, y, z) => ((y - minY) * SPAN + (x - from.x + HALF)) * SPAN + (z - from.z + HALF)

  const grow = () => {
    cap = Math.min(maxNodes, cap * 2)
    ;[keys, xs, ys, zs, hs, moves, slow, corners, parent, secs, risks, gs, fs, heapPos, heap] =
      [keys, xs, ys, zs, hs, moves, slow, corners, parent, secs, risks, gs, fs, heapPos, heap].map(a => grown(a, cap))
    slots = nextPow2(cap * 2)
    hashTable = new Int32Array(slots).fill(-1)
    for (let i = 0; i < count; i++) {
      let s = hashOf(xs[i], ys[i], zs[i]) & (slots - 1)
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

  // relax the edge to a node: insert it, or lower its cost if this way is cheaper
  const consider = (x, y, z, h, move, parentNode, dsec, drisk, slowTo, corner = 0) => {
    const rx = x - from.x + HALF
    const rz = z - from.z + HALF
    if (rx < 0 || rx >= SPAN || rz < 0 || rz >= SPAN) return
    if (x < bx0 || x > bx1 || z < bz0 || z > bz1 || y < by0 || y > by1) { boxed = true; return }
    const key = keyOf(x, y, z)
    let s = hashOf(x, y, z) & (slots - 1)
    let found = -1
    while (hashTable[s] !== -1) {
      if (keys[hashTable[s]] === key) { found = hashTable[s]; break }
      s = (s + 1) & (slots - 1)
    }
    const sec = secs[parentNode] + dsec
    const risk = risks[parentNode] + drisk
    const g = sec + riskWeight * risk
    let node = found
    if (node === -1) {
      if (count === maxNodes) { overBudget = true; return }
      if (count === cap) {
        grow()
        s = hashOf(x, y, z) & (slots - 1)
        while (hashTable[s] !== -1) s = (s + 1) & (slots - 1)
      }
      node = count++
      hashTable[s] = node
      keys[node] = key
      xs[node] = x
      ys[node] = y
      zs[node] = z
      heapPos[node] = -1
    } else if (heapPos[node] === -2 || g >= gs[node]) return
    hs[node] = h
    moves[node] = move
    slow[node] = slowTo
    corners[node] = corner
    parent[node] = parentNode
    secs[node] = sec
    risks[node] = risk
    gs[node] = g
    fs[node] = g + weight * heuristic(x, z)
    if (heapPos[node] === -1) {
      heapN++
      siftUp(heapN - 1, node)
      return
    }
    siftUp(heapPos[node], node)
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
    // the body also leaves the higher level through this column
    return clear(x2, z2, ty * 16 + h1, h0 + BODY) ? h1 : -1
  }

  const expand = i => expandAt(xs[i], ys[i], zs[i], hs[i], slow[i], i)

  const expandAt = (x, y, z, h, slowFrom, i) => {
    const h0 = y * 16 + h

    for (let c = 0; c < 4; c++) {
      const x2 = x + CARDINAL[c][0]
      const z2 = z + CARDINAL[c][1]
      const h1 = neighbour(x2, z2, y, h0)
      if (h1 >= 0) {
        const delta = ty * 16 + h1 - h0
        const walk = WALK_S * (1 + SLOW_EXTRA * (slowFrom + enterSlow))
        // climbing a stairs block in its direction is a walk, though the node above it is a whole block up
        const climbs = stairUp[support] === c + 1 && delta <= WHOLE
        if (delta <= STEP || climbs) edge(x2, ty, z2, h1, MOVE.WALK, i, walk, enterRisk, enterSlow)
        else if (delta <= JUMP_UP && clear(x, z, h0, ty * 16 + h1 + BODY)) edge(x2, ty, z2, h1, MOVE.JUMP, i, walk + JUMP_S, enterRisk, enterSlow)
        continue
      }
      // no ground ahead at our level: the body must at least fit in the column to leave the edge
      if (!clear(x2, z2, h0, h0 + BODY)) continue
      expandDrop(i, x, y, z, x2, z2, h0, slowFrom)
      expandGap(i, x, y, z, c, h0)
    }

    for (let c = 0; c < 4; c++) {
      const dx = DIAGONAL[c][0]
      const dz = DIAGONAL[c][1]
      const x2 = x + dx
      const z2 = z + dz
      const h1 = neighbour(x2, z2, y, h0)
      if (h1 < 0) continue
      const y2 = ty
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
      if (jump && !clear(x, z, h0, hi)) continue
      const slide = sa + sb // 1 when exactly one side is blocked
      const walk = WALK_S * SQRT2 * (1 + SLOW_EXTRA * (slowFrom + enterSlow)) + slide * CORNER_S
      const code = jump ? MOVE.JUMP : slide ? MOVE.CORNER : MOVE.DIAGONAL
      edge(x2, y2, z2, h1, code, i, jump ? walk + JUMP_S : walk, enterRisk, enterSlow, slide)
    }
  }

  // walk off an edge into the first standable cell below the neighbour column
  const expandDrop = (i, x, y, z, x2, z2, h0, slowFrom) => {
    for (let y2 = y - 1; y2 >= y - maxDrop - 1; y2--) {
      const id = stateAt(x2, y2, z2)
      if (id === UNLOADED || kind[id] === LAVA || kind[id] === WATER || hazard[id] === HAZARD_AVOID) return
      const h1 = landing(x2, y2, z2)
      if (h1 < 0) {
        if (top[id] > 0) return
        continue
      }
      const fall = h0 - (y2 * 16 + h1)
      if (fall > maxDrop * 16) return
      const sec = WALK_S * (1 + SLOW_EXTRA * (slowFrom + enterSlow)) + 0.25 * Math.sqrt(Math.max(0, fall) / 16)
      edge(x2, y2, z2, h1, MOVE.DROP, i, sec, enterRisk + fallDamage(fall), enterSlow)
      return
    }
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
      if (h1 < 0) continue
      const delta = ly * 16 + h1 - h0
      if (delta < -16 || delta > (n <= 2 && upArc ? WHOLE : 0)) continue
      edge(lx, ly, lz, h1, MOVE.GAP, i, (n + 1) * SPRINT_S + GAP_S + (delta > 0 ? GAP_UP_S : 0), enterRisk + hole, enterSlow)
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

  const startH = standH(from.x, from.y, from.z)
  const startSlow = hazard[support] === SLOW ? 1 : 0 // before goalNotStandable reuses standH
  const goalNotStandable = () => {
    if (!near || goalUnloaded) return false
    const r = Math.ceil(goalRange)
    for (let dx = -r; dx <= r; dx++) {
      for (let dy = -r; dy <= r; dy++) {
        for (let dz = -r; dz <= r; dz++) {
          if (dx * dx + dy * dy + dz * dz > goalRange * goalRange) continue
          if (standH(goal.x + dx, goal.y + dy, goal.z + dz) >= 0) return false
        }
      }
    }
    return true
  }

  // Backward flood from the standable goal cells over predecessors: cells n with a forward move n -> c, found by running n's
  // own moves (so it can never disagree with the search). True when it exhausts within goalFlood nodes without meeting the
  // start: then nothing reaches the goal. Slow, but bounded by the budget; false on budget or when the start is met.
  let flooded = 0
  const goalEnclosed = () => {
    const inSpan = (x, z) => x - from.x + HALF >= 0 && x - from.x + HALF < SPAN && z - from.z + HALF >= 0 && z - from.z + HALF < SPAN
    const startKey = keyOf(from.x, from.y, from.z)
    const seen = new Set()
    const queue = []
    let fx = 0
    let fy = 0
    let fz = 0
    let hit = false
    const probe = (x, y, z) => { if (x === fx && y === fy && z === fz) hit = true }
    // adds n when it is standable and a forward move reaches the flood's current cell; true when n is the start
    const visit = (x, y, z) => {
      if (!inSpan(x, z)) return false
      const key = keyOf(x, y, z)
      if (seen.has(key)) return false
      const h = standH(x, y, z)
      if (h < 0) return false
      hit = false
      expandAt(x, y, z, h, 0, -1)
      if (!hit) return false
      seen.add(key)
      queue.push([x, y, z])
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
          if (!inSpan(x, z) || standH(x, y, z) < 0) continue
          seen.add(keyOf(x, y, z))
          queue.push([x, y, z])
          seed.push(keyOf(x, y, z))
        }
      }
    }
    if (seed.includes(startKey)) return false
    edge = probe
    let open = false
    for (let head = 0; head < queue.length && !open && seen.size <= goalFlood; head++) {
      ;[fx, fy, fz] = queue[head]
      for (const [dx, dz] of AROUND) {
        for (let dy = -1; dy <= maxDrop + 1; dy++) open ||= visit(fx + dx, fy + dy, fz + dz)
      }
      for (const [dx, dz] of CARDINAL) {
        for (let n = 2; n <= 4; n++) {
          for (let dy = 0; dy <= 1; dy++) open ||= visit(fx + dx * n, fy + dy, fz + dz * n)
        }
      }
    }
    edge = consider
    flooded = seen.size
    return !open && seen.size <= goalFlood
  }

  const finish = why => {
    finished = true
    reason = why
    elapsed = performance.now() - t0
  }

  const begin = () => {
    started = true
    t0 = performance.now()
    if (startH < 0) return finish('start-not-standable')
    if (goalNotStandable()) return finish('goal-not-standable')
    startDistance = distanceTo(from.x, from.z)
    // the start is node 0
    keys[0] = keyOf(from.x, from.y, from.z)
    xs[0] = from.x
    ys[0] = from.y
    zs[0] = from.z
    hs[0] = startH
    parent[0] = -1
    gs[0] = 0
    fs[0] = heuristic(from.x, from.z)
    hashTable[hashOf(from.x, from.y, from.z) & (slots - 1)] = 0
    slow[0] = startSlow
    count = 1
    heapN = 1
    heap[0] = 0
    heapPos[0] = 0
  }

  // the goal flood costs ~30 ms, so easy queries must never see it: it runs once, after floodAfter forward expansions
  let floodPending = near && goalFlood > 0

  const step = maxExpansions => {
    if (!started) begin()
    for (let n = 0; n < maxExpansions && !finished; n++) {
      if (floodPending && expanded >= floodAfter && !goalUnloaded) {
        floodPending = false
        const enclosed = goalEnclosed()
        expanded += flooded
        n += flooded // the flood counts toward the slice
        if (enclosed) { finish('goal-enclosed'); break }
      }
      if (heapN === 0) { finish(boxed ? 'box' : 'exhausted'); break }
      const i = pop()
      expanded++
      if (reached(xs[i], ys[i], zs[i])) {
        goalNode = i
        finish(null)
        break
      }
      const d = distanceTo(xs[i], zs[i])
      if (d < bestDistance) { bestDistance = d; best = i }
      expand(i)
      if (overBudget) finish('budget')
    }
    return finished
  }

  // ---- results ----

  const stepsTo = node => {
    const out = []
    for (let i = node; i !== -1; i = parent[i]) out.push({ x: xs[i], y: ys[i], z: zs[i], h: hs[i], move: moves[i], corner: corners[i] === 1 })
    return out.reverse()
  }

  const summarize = steps => {
    const legs = steps.slice(1).map((s, k) => ({ s, p: steps[k] }))
    const blocks = Math.round(legs.reduce((sum, { s, p }) => sum + Math.hypot(s.x - p.x, s.z - p.z), 0))
    const rise = ({ s, p }) => s.move !== MOVE.DROP && s.move !== MOVE.GAP && s.y * 16 + s.h > p.y * 16 + p.h
    const ups = legs.filter(rise).length
    const falls = legs.filter(({ s }) => s.move === MOVE.DROP).map(({ s, p }) => Math.round((p.y * 16 + p.h - s.y * 16 - s.h) / 16)).filter(f => f >= 2)
    const gaps = legs.filter(({ s }) => s.move === MOVE.GAP).length
    const slides = steps.filter(s => s.corner).length
    const gapUps = legs.filter(({ s, p }) => s.move === MOVE.GAP && s.y * 16 + s.h > p.y * 16 + p.h).length
    const lava = steps.some(s => lavaNear(s.x, s.y, s.z))
    return [
      `${blocks} blocks`,
      ups > 0 && (ups === 1 ? '1 step up' : `${ups} steps up`),
      falls.length === 1 && `1 drop of ${falls[0]}`,
      falls.length > 1 && `${falls.length} drops, deepest ${Math.max(...falls)}`,
      gaps > 0 && (gaps === 1 ? '1 gap jump' : `${gaps} gap jumps`),
      slides > 0 && (slides === 1 ? '1 corner slide' : `${slides} corner slides`),
      gapUps > 0 && (gapUps === 1 ? '1 jump up over a gap' : `${gapUps} jumps up over gaps`),
      lava && 'passes 1 cell from lava'
    ].filter(Boolean).join(', ')
  }

  const pathTo = node => {
    const steps = stepsTo(node)
    const drops = steps.slice(1).map((s, k) => s.move === MOVE.DROP ? (steps[k].y * 16 + steps[k].h - s.y * 16 - s.h) / 16 : 0)
    const jumps = steps.filter(s => s.move === MOVE.JUMP || s.move === MOVE.GAP).length
    const cost = { seconds: secs[node], risk: risks[node], maxDrop: Math.max(0, ...drops), jumps, unknown: 0 }
    return { steps, cost, summary: summarize(steps) }
  }

  const result = () => {
    if (!finished) finish('budget')
    const base = { ms: elapsed, expanded }
    if (reason === 'start-not-standable' || reason === 'goal-not-standable') return { status: 'none', reason, ...base, path: null }
    if (reason === null) return { status: 'found', reason, ...base, path: pathTo(goalNode) }
    const partial = (why) => ({ status: 'partial', reason: why, ...base, path: best === -1 ? null : pathTo(best) })
    if (reason === 'budget') return partial('budget')
    if (goalUnloaded) return partial('goal-unloaded')
    if (best !== -1 && startDistance - bestDistance >= MIN_CLOSER) return partial(reason)
    return { status: 'none', reason, ...base, path: null }
  }

  return { step, result }
}

export function plan (snapshot, query, options) {
  const search = createSearch(snapshot, query, options)
  search.step(Infinity)
  return search.result()
}
