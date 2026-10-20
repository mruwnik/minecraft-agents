// Why JavaScript: tests sight-kernel.mjs against a brute-force line-of-sight oracle over random worlds.
// What the kernel records must be what an eye can see: a ray records exactly the cells of its straight line up to the
// first blocking cell (never through a wall), and every cell with a clear line well inside range is recorded by a
// dense spread of rays (never less than a player).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { castRays, lazySections, MARKS_LENGTH } from './sight-kernel.mjs'

const rng = seed => () => { seed = (seed + 0x6D2B79F5) | 0; let t = Math.imul(seed ^ (seed >>> 15), 1 | seed); t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t; return ((t ^ (t >>> 14)) >>> 0) / 4294967296 }
const hash = (x, y, z, k) => { let h = Math.imul(x | 0, 73856093) ^ Math.imul(y | 0, 19349663) ^ Math.imul(z | 0, 83492791) ^ Math.imul(k, 2654435761); h ^= h >>> 13; h = Math.imul(h, 0x5bd1e995); return (h ^ (h >>> 15)) >>> 0 }

// ids: 0 air, 1 stone, 2 slab (blocks sight), 3 glass (clear); -1 unloaded
const sight = Uint8Array.from([0, 1, 1, 0])
const blocksId = id => id < 0 || sight[id] === 1

function makeWorld (seed, { density, dark, hole }) {
  const at = (x, y, z) => {
    if (hole && x > 8) return -1
    if (y < 56) return 1
    const h = hash(x, y, z, seed) % 1000 / 1000
    if (h < density) return 1
    if (h < density * 1.4) return 2
    return h < density * 1.6 ? 3 : 0
  }
  const lightAt = dark
    ? (x, y, z) => { const h = hash(x >> 1, y >> 1, z >> 1, seed + 7); return (h % 3 === 0 ? 0xA0 : 0) | (h % 5 === 0 ? 0x0C : 0) }
    : () => 0xF0
  return { at, lightAt, dark }
}

// the cells the kernel records from `eye` with unit directions spread over the whole sphere (Fibonacci lattice)
function recordedFrom (world, eye, radius, rays, given, shared = { marks: new Map(), sections: lazySections() }) {
  const { marks } = shared
  const seen = new Set()
  const ctx = {
    raw: { stateAt: world.at, lightAt: world.lightAt },
    sections: shared.sections,
    sight,
    visible: Uint8Array.from({ length: 256 }, (_, l) => Math.max(l >> 4, l & 15) >= 3 ? 1 : 0),
    radius,
    near: 0,
    marksOf: (cx, cy, cz) => {
      const k = `${cx},${cy},${cz}`
      if (!marks.has(k)) marks.set(k, new Uint8Array(MARKS_LENGTH))
      return marks.get(k)
    },
    record: (x, y, z) => seen.add(`${x},${y},${z}`),
    markKind: id => (sight[id] !== 1 ? 1 : 2),
    thin: () => true
  }
  const dirs = given ?? new Float64Array(3 * rays)
  const golden = Math.PI * (3 - Math.sqrt(5))
  for (let i = 0; i < (given ? 0 : rays); i++) {
    const y = 1 - (2 * i + 1) / rays
    const r = Math.sqrt(1 - y * y)
    dirs.set([Math.cos(golden * i) * r, y, Math.sin(golden * i) * r], 3 * i)
  }
  castRays(ctx, ...eye, dirs, rays)
  return seen
}

// Whether the straight segment from the eye to p meets no blocking or unloaded cell other than the eye's cell and p's cell.
function lineClear (world, eye, p) {
  const eyeCell = eye.map(Math.floor)
  const pCell = p.map(Math.floor)
  const d = Math.hypot(p[0] - eye[0], p[1] - eye[1], p[2] - eye[2])
  const n = Math.max(1, Math.ceil(d / 0.02))
  for (let s = 1; s < n; s++) {
    const c = [0, 1, 2].map(k => Math.floor(eye[k] + (p[k] - eye[k]) * s / n))
    if (c.every((v, k) => v === eyeCell[k]) || c.every((v, k) => v === pCell[k])) continue
    if (blocksId(world.at(...c))) return false
  }
  return true
}

// whether the centre line, and the lines to six points a little off the centre, are all clear: no corner is grazed
const robustlySeen = (world, eye, [x, y, z]) =>
  [[0, 0, 0], [0.2, 0, 0], [-0.2, 0, 0], [0, 0.2, 0], [0, -0.2, 0], [0, 0, 0.2], [0, 0, -0.2]]
    .every(([a, b, c]) => lineClear(world, eye, [x + 0.5 + a, y + 0.5 + b, z + 0.5 + c]))

const RADIUS = 10
const RAYS = 40000

function setUp (seed) {
  const r = rng(seed)
  const world = makeWorld(seed, { density: [0.02, 0.1, 0.3][seed % 3], dark: seed % 4 === 0, hole: seed % 5 === 0 })
  const eye = [(r() - 0.5) * 12, 57 + r() * 8, (r() - 0.5) * 12]
  while (blocksId(world.at(...eye.map(Math.floor)))) eye[1]++
  return { world, eye }
}

const cellsAround = eye => {
  const [ex, ey, ez] = eye.map(Math.floor)
  const out = []
  for (let x = ex - RADIUS; x <= ex + RADIUS; x++) for (let y = ey - RADIUS; y <= ey + RADIUS; y++) for (let z = ez - RADIUS; z <= ez + RADIUS; z++) out.push([x, y, z])
  return out
}

// The cells a unit direction d from the eye passes through, in order, each with the distance at which it is entered,
// found by walking the line in tiny steps; null when a cell is crossed over less than 0.01 (too close to a corner for
// the walk to be a reference, or two coordinates change at once).
function lineCells (world, eye, d) {
  const step = 0.0005
  const out = []
  let last = ''
  let entered = 0
  for (let s = 0; s <= RADIUS + 0.02; s += step) {
    const c = [0, 1, 2].map(k => Math.floor(eye[k] + d[k] * s))
    const key = c.join(',')
    if (key === last) continue
    if (out.length && (s - entered < 0.01 || c.filter((v, k) => v !== out[out.length - 1].cell[k]).length > 1)) return null
    out.push({ cell: c, entered: s })
    last = key
    entered = s
    if (blocksId(world.at(...c))) break
  }
  return out
}

const randomDir = r => {
  const v = [r() - 0.5, r() - 0.5, r() - 0.5]
  const len = Math.hypot(...v)
  return v.map(c => c / len)
}

for (let seed = 1; seed <= 12; seed++) {
  test(`a ray records the cells of its line up to the first block, none beyond, also with the marks of earlier rays (seed ${seed})`, () => {
    const r = rng(seed * 101)
    const { world, eye } = setUp(seed)
    const lines = Array.from({ length: 600 }, () => randomDir(r)).map(d => [d, lineCells(world, eye, d)]).filter(([, cells]) => cells !== null)
    const shared = { marks: new Map(), sections: lazySections() }
    const wrong = lines.flatMap(([d, cells]) => {
      const seen = recordedFrom(world, eye, RADIUS, 1, Float64Array.from(d))
      const again = recordedFrom(world, eye, RADIUS, 1, Float64Array.from(d), shared)
      const required = cells.filter(c => c.entered <= RADIUS - 0.01 && world.at(...c.cell) >= 0).map(c => c.cell.join(','))
      const allowed = new Set(cells.filter(c => c.entered <= RADIUS + 0.01).map(c => c.cell.join(',')))
      const missing = !world.dark ? required.filter(k => !seen.has(k)) : []
      const extra = [...seen, ...again].filter(k => !allowed.has(k))
      return missing.length || extra.length ? [{ d, missing, extra }] : []
    })
    assert.deepEqual(wrong, [])
    assert.ok(lines.length > 300)
  })
}

// Lit worlds only: a dark cell is rightly left out.
for (const seed of [1, 2, 3, 5, 6, 7, 9, 10, 11]) {
  test(`every lit cell within range with a robust clear line is recorded (seed ${seed})`, () => {
    const { world, eye } = setUp(seed)
    const seen = recordedFrom(world, eye, RADIUS, RAYS)
    const loaded = cellsAround(eye).filter(c => Math.hypot(c[0] + 0.5 - eye[0], c[1] + 0.5 - eye[1], c[2] + 0.5 - eye[2]) <= RADIUS - 1 && world.at(...c) >= 0)
    const clear = loaded.filter(c => robustlySeen(world, eye, c))
    const missed = clear.filter(c => !seen.has(c.join(',')))
    assert.deepEqual(missed, [])
    assert.ok(clear.length > 50)
  })
}
