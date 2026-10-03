// Pure A* over a snapshot of block state ids. A node is a feet cell; its stand height h (1/16 block above the cell's
// floor) comes from the collision under or in it, so slabs, carpets and snow are half steps, not cell-sized ones.
//
// Standable, kept conservative: the cell's own collision is low (top <= 8/16: slab, carpet, snow, campfire), then h = top;
// or the cell is free and the one below tops at 14..16/16 (full-ish blocks, soul sand and farmland count as h = 0) or above
// 16 (a closed gate: h = top - 16). Collision tops of 9..13 (chests, enchanting tables) are neither: not standable on, not
// walkable through. Fences, walls, panes, bars and bamboo (NARROW) are never a floor and never walked through. Stairs have
// a full-height top in the table, so they take a jump. The body's column, 29/16 tall from the stand height, must have no
// collision, no fluid, no NARROW and no AVOID hazard. Water is not entered at all in stage 1 (drops into it are refused too).
import { UNLOADED } from './snapshot.mjs'
import { defaultStateTable, WATER, LAVA, NARROW, HAZARD_AVOID, DAMAGE_STAND, DAMAGE_TOUCH, SLOW } from './blocks.mjs'

export const MOVE = { START: 0, WALK: 1, DIAGONAL: 2, JUMP: 3, DROP: 4, GAP: 5 }

const BODY = 29 // 1.8 blocks in 1/16, rounded up
const STEP = 9 // 0.6 blocks
const JUMP_UP = 20 // 1.25 blocks
const ARC = 32 // headroom over a gap: feet + 2
const WALK_S = 1 / 4.317 // seconds per block
const SPRINT_S = 1 / 5.612
const JUMP_S = 0.35 // a jump up costs this much more than the walk it replaces
const GAP_S = 0.5 // a gap jump's run-up and landing, on top of the sprint over its length
const SLOW_EXTRA = 0.75 // walking time grows by this much of itself per slow end of a move: both ends soul sand is x2.5
const LAVA_ADJACENT = 0.5 // hp of risk for a step with lava beside the feet
const FREE_FALL = 3
const SQRT2 = Math.SQRT2
const OCTILE_SLACK = 1.0824 // octile length of a vector of length r is at most this times r: keeps the range term admissible
const SPAN = 4096 // nodes further than 2048 blocks from the start in x or z are not searched
const HALF = 2048
const MIN_CLOSER = 2 // an exhausted search is a partial result only when it got this many blocks closer

const CARDINAL = [[1, 0], [-1, 0], [0, 1], [0, -1]]
const DIAGONAL = [[1, 1], [1, -1], [-1, 1], [-1, -1]]

const nextPow2 = n => 2 ** Math.ceil(Math.log2(Math.max(2, n)))
const fallDamage = fall16 => Math.max(0, Math.ceil(fall16 / 16 - FREE_FALL))

export function createSearch (snapshot, query, options = {}) {
  const {
    maxNodes = 200000, maxDrop = 3, weight = 1, riskWeight = 2,
    table = defaultStateTable(),
    strictCorners = true
  } = options
  const { top, base, kind, hazard } = table
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

  let support = 0 // state id the last standH stood on
  let touch = 0 // DAMAGE_TOUCH cells inside the last standH body

  // stand height at a feet cell, or -1
  const standH = (x, y, z) => {
    const id = stateAt(x, y, z)
    if (id === UNLOADED) return -1
    const t = top[id]
    let h
    if (t === 0) {
      const below = stateAt(x, y - 1, z)
      if (below === UNLOADED) return -1
      const tb = top[below]
      const hz = hazard[below]
      if (tb < 14 || kind[below] === NARROW || hz === HAZARD_AVOID || hz === DAMAGE_TOUCH) return -1
      h = tb > 16 ? tb - 16 : 0
      support = below
    } else {
      const hz = hazard[id]
      if (t > 8 || kind[id] === NARROW || hz === HAZARD_AVOID || hz === DAMAGE_TOUCH) return -1
      h = t
      support = id
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
    ;[keys, xs, ys, zs, hs, moves, slow, parent, secs, risks, gs, fs, heapPos, heap] =
      [keys, xs, ys, zs, hs, moves, slow, parent, secs, risks, gs, fs, heapPos, heap].map(a => grown(a, cap))
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
  const consider = (x, y, z, h, move, parentNode, dsec, drisk, slowTo) => {
    const rx = x - from.x + HALF
    const rz = z - from.z + HALF
    if (rx < 0 || rx >= SPAN || rz < 0 || rz >= SPAN) return
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

  const expand = i => {
    const x = xs[i]
    const y = ys[i]
    const z = zs[i]
    const h0 = y * 16 + hs[i]
    const slowFrom = slow[i]

    for (let c = 0; c < 4; c++) {
      const x2 = x + CARDINAL[c][0]
      const z2 = z + CARDINAL[c][1]
      let y2 = y
      let h1 = landing(x2, y, z2)
      if (h1 < 0) {
        y2 = y + 1
        h1 = landing(x2, y2, z2)
      }
      if (h1 >= 0) {
        const delta = y2 * 16 + h1 - h0
        const walk = WALK_S * (1 + SLOW_EXTRA * (slowFrom + enterSlow))
        if (delta <= STEP) consider(x2, y2, z2, h1, MOVE.WALK, i, walk, enterRisk, enterSlow)
        else if (delta <= JUMP_UP && clear(x, z, h0, y2 * 16 + h1 + BODY)) consider(x2, y2, z2, h1, MOVE.JUMP, i, walk + JUMP_S, enterRisk, enterSlow)
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
      let y2 = y
      let h1 = landing(x2, y, z2)
      if (h1 < 0) {
        y2 = y + 1
        h1 = landing(x2, y2, z2)
      }
      if (h1 < 0) continue
      const h2 = y2 * 16 + h1
      const jump = h2 - h0 > STEP
      if (h2 - h0 > JUMP_UP) continue
      // both side columns clear over the body's whole travel: no corner cutting. A diagonal jump also needs headroom here
      const lo = Math.min(h0, h2)
      const hi = Math.max(h0, h2) + BODY
      const sideA = clear(x + dx, z, lo, hi)
      const sideB = clear(x, z + dz, lo, hi)
      if (strictCorners ? !(sideA && sideB) : !(sideA || sideB)) continue
      if (jump && !clear(x, z, h0, hi)) continue
      const walk = WALK_S * SQRT2 * (1 + SLOW_EXTRA * (slowFrom + enterSlow))
      consider(x2, y2, z2, h1, jump ? MOVE.JUMP : MOVE.DIAGONAL, i, jump ? walk + JUMP_S : walk, enterRisk, enterSlow)
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
      consider(x2, y2, z2, h1, MOVE.DROP, i, sec, enterRisk + fallDamage(fall), enterSlow)
      return
    }
  }

  // sprint across 1..3 empty cells in a cardinal line, landing level or one lower
  const expandGap = (i, x, y, z, c, h0) => {
    const dx = CARDINAL[c][0]
    const dz = CARDINAL[c][1]
    let hole = 0
    for (let n = 1; n <= 3; n++) {
      const gx = x + dx * n
      const gz = z + dz * n
      if (n > 1 && (landing(gx, y, gz) >= 0 || landing(gx, y + 1, gz) >= 0)) return
      if (!clear(gx, gz, h0, h0 + ARC)) return
      hole = Math.max(hole, holeRisk(gx, y, gz))
      const lx = gx + dx
      const lz = gz + dz
      let ly = y
      let h1 = landing(lx, y, lz)
      if (h1 < 0) {
        ly = y - 1
        h1 = landing(lx, ly, lz)
      }
      if (h1 < 0) continue
      const delta = ly * 16 + h1 - h0
      if (delta > 0 || delta < -16) continue
      consider(lx, ly, lz, h1, MOVE.GAP, i, (n + 1) * SPRINT_S + GAP_S, enterRisk + hole, enterSlow)
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

  const step = maxExpansions => {
    if (!started) begin()
    for (let n = 0; n < maxExpansions && !finished; n++) {
      if (heapN === 0) { finish('exhausted'); break }
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
    for (let i = node; i !== -1; i = parent[i]) out.push({ x: xs[i], y: ys[i], z: zs[i], h: hs[i], move: moves[i] })
    return out.reverse()
  }

  const summarize = steps => {
    const legs = steps.slice(1).map((s, k) => ({ s, p: steps[k] }))
    const blocks = Math.round(legs.reduce((sum, { s, p }) => sum + Math.hypot(s.x - p.x, s.z - p.z), 0))
    const rise = ({ s, p }) => s.move !== MOVE.DROP && s.move !== MOVE.GAP && s.y * 16 + s.h > p.y * 16 + p.h
    const ups = legs.filter(rise).length
    const falls = legs.filter(({ s }) => s.move === MOVE.DROP).map(({ s, p }) => Math.round((p.y * 16 + p.h - s.y * 16 - s.h) / 16)).filter(f => f >= 2)
    const gaps = legs.filter(({ s }) => s.move === MOVE.GAP).length
    const lava = steps.some(s => lavaNear(s.x, s.y, s.z))
    return [
      `${blocks} blocks`,
      ups > 0 && (ups === 1 ? '1 step up' : `${ups} steps up`),
      falls.length === 1 && `1 drop of ${falls[0]}`,
      falls.length > 1 && `${falls.length} drops, deepest ${Math.max(...falls)}`,
      gaps > 0 && (gaps === 1 ? '1 gap jump' : `${gaps} gap jumps`),
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
    if (best !== -1 && startDistance - bestDistance >= MIN_CLOSER) return partial('exhausted')
    return { status: 'none', reason, ...base, path: null }
  }

  return { step, result }
}

export function plan (snapshot, query, options) {
  const search = createSearch(snapshot, query, options)
  search.step(Infinity)
  return search.result()
}
