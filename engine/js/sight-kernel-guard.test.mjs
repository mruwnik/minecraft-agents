// Why JavaScript: tests sight-kernel.mjs against sight-kernel-ref.mjs, the kernel as it was before its speed-up.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import * as live from './sight-kernel.mjs'
import * as ref from './sight-kernel-ref.mjs'

const rng = seed => () => { seed = (seed + 0x6D2B79F5) | 0; let t = Math.imul(seed ^ (seed >>> 15), 1 | seed); t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t; return ((t ^ (t >>> 14)) >>> 0) / 4294967296 }
const hash = (x, y, z, k) => { let h = Math.imul(x | 0, 73856093) ^ Math.imul(y | 0, 19349663) ^ Math.imul(z | 0, 83492791) ^ Math.imul(k, 2654435761); h ^= h >>> 13; h = Math.imul(h, 0x5bd1e995); return (h ^ (h >>> 15)) >>> 0 }

// ids: 0 air, 1 stone, 2 slab (blocks sight, thin), 3 glass (clear), 4 leaves-like (clear); -1 unloaded
function makeWorld (seed, { density, dark, hole }) {
  const at = (x, y, z) => {
    if (hole && x > 24) return -1
    if (y < 56) return 1
    const h = hash(x, y, z, seed) % 1000 / 1000
    if (h < density) return 1
    if (h < density * 1.4) return 2
    if (h < density * 1.6) return 3
    return h > 0.96 ? 4 : 0
  }
  const lightAt = (x, y, z) => {
    const h = hash(x >> 1, y >> 1, z >> 1, seed + 7)
    return dark ? ((h % 3 === 0 ? 0xA0 : 0) | (h % 5 === 0 ? 0x0C : 0) | (h % 7 === 0 ? 0x30 : 0)) : (h % 11 === 0 ? 0x05 : 0xF0 | (h % 4))
  }
  return { at, lightAt }
}

const sight = Uint8Array.from([0, 1, 1, 0, 0])
const visible = Uint8Array.from({ length: 256 }, (_, l) => Math.max(l >> 4, l & 15) >= 3 ? 1 : 0)

// A cast context over `kernel`: lazy sections (the raw world has none) or sections built from the world's own arrays.
function rig (kernel, world, { eager, legacy, radius, near }) {
  let marks = new Map()
  const records = []
  const raw = { stateAt: world.at, lightAt: world.lightAt }
  const makeSections = () => eager
    ? (() => {
        const states = new Map(); const lights = new Map()
        return {
          sectionRange: () => ({ minY: 0, sections: 16 }),
          sectionStates: (cx, s, cz) => {
            const key = `${cx},${s},${cz}`
            if (!states.has(key)) states.set(key, Uint16Array.from({ length: 4096 }, (_, i) => { const id = world.at((cx << 4) | (i & 15), (s << 4) | (i >> 8), (cz << 4) | ((i >> 4) & 15)); return id < 0 ? 0xFFFF : id }))
            return states.get(key)
          },
          sectionLight: (cx, s, cz) => {
            const key = `${cx},${s},${cz}`
            if (!lights.has(key)) {
              const sky = new Uint8Array(4096); const block = new Uint8Array(4096)
              for (let i = 0; i < 4096; i++) { const v = world.lightAt((cx << 4) | (i & 15), (s << 4) | (i >> 8), (cz << 4) | ((i >> 4) & 15)); sky[i] = v >> 4; block[i] = v & 15 }
              lights.set(key, { sky, block })
            }
            return lights.get(key)
          }
        }
      })()
    : kernel.lazySections()
  const ctx = {
    raw, sections: makeSections(), sight, visible, base: 0, radius, near,
    marksOf: (cx, cy, cz) => {
      const key = `${cx},${cy},${cz}`
      if (!marks.has(key)) marks.set(key, legacy ? new Uint32Array(4096) : new Uint8Array(kernel.MARKS_LENGTH))
      return marks.get(key)
    },
    record: (x, y, z, id) => records.push([x, y, z, id]),
    markKind: id => (sight[id] !== 1 ? 1 : id === 2 ? 3 : 2),
    thin: (id, x, y, z, ox, oy, oz, ex, ey, ez) => (hash(x, y, z, 3) + Math.floor(ox * 7) + Math.floor(ex)) % 2 === 0
  }
  // a new mark stamp: nothing recorded, section arrays read again
  const fresh = () => { marks = new Map(); ctx.sections = makeSections(); ctx.base += 4; records.length = 0 }
  return { ctx, records, fresh }
}

function randomDirs (r, n) {
  const out = new Float64Array(3 * n)
  for (let i = 0; i < n; i++) {
    const pick = r()
    let v
    if (pick < 0.04) v = [r() < 0.5 ? 1 : -1, 0, 0]
    else if (pick < 0.08) v = [0, r() < 0.5 ? 1 : -1, 0]
    else if (pick < 0.12) v = [0, 0, r() < 0.5 ? 1 : -1]
    else if (pick < 0.16) v = [r() < 0.5 ? 1 : -1, r() < 0.5 ? 1 : -1, 0]
    else if (pick < 0.2) v = [1, 1, 1].map(c => c * (r() < 0.5 ? 1 : -1))
    else if (pick < 0.22) v = [NaN, 0, 0]
    else v = [r() - 0.5, (r() - 0.5) * 0.8, r() - 0.5]
    const len = Math.hypot(...v) || 1
    out.set(v.map(c => c / len), 3 * i)
  }
  return out
}

for (let seed = 1; seed <= 60; seed++) {
  test(`live kernel casts like the reference over a random world and eye (seed ${seed})`, () => {
    const r = rng(seed)
    const world = makeWorld(seed, { density: [0.02, 0.1, 0.3][seed % 3], dark: seed % 4 === 0, hole: seed % 5 === 0 })
    const radius = [8, 20, 48][seed % 3]
    const near = seed % 2 ? 0 : 3.5
    const eager = seed % 6 < 3
    const a = rig(live, world, { eager, legacy: false, radius, near })
    const b = rig(ref, world, { eager, legacy: true, radius, near })
    for (let pass = 0; pass < 4; pass++) {
      if (pass === 2) { a.fresh(); b.fresh() }
      const eye = [(r() - 0.5) * 30, 56 + r() * 12, (r() - 0.5) * 30].map(c => (pass % 2 ? Math.floor(c) + 0.5 : c))
      const dirs = randomDirs(r, 150)
      const ta = live.castRays(a.ctx, ...eye, dirs, 150)
      const tb = ref.castRays(b.ctx, ...eye, dirs, 150)
      assert.equal(ta, tb, 'cells entered')
      assert.deepEqual(a.records, b.records, 'cells recorded, in order')
    }
  })
}
