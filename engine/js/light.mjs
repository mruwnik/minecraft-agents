// Pure Minecraft light propagation. Region recompute: the one-cell shell of a
// box keeps its values and acts as fixed sources; the interior is rebuilt from
// emitters plus the shell using vanilla flood-fill rules. No bot access.
import { createRequire } from 'node:module'

const require = createRequire(import.meta.url)
const blockFactory = require('prismarine-block')

const COPPER_BULB_LEVEL = { copper_bulb: 15, exposed_copper_bulb: 12, weathered_copper_bulb: 8, oxidized_copper_bulb: 4 }
const ANCHOR_LEVEL = [0, 3, 7, 11, 15]
const lit = (on) => (props) => (props.lit === true ? on : 0)

// name -> (props, base) => emitted light. Anything not here uses base,
// with `lit === false` forcing 0 (applied before the overrides).
const OVERRIDES = {
  redstone_lamp: lit(15),
  furnace: lit(13),
  blast_furnace: lit(13),
  smoker: lit(13),
  redstone_ore: lit(9),
  deepslate_redstone_ore: lit(9),
  sea_pickle: (p) => (p.waterlogged === true ? 3 * (Number(p.pickles) + 1) : 0),
  light: (p) => Number(p.level ?? 15),
  respawn_anchor: (p) => ANCHOR_LEVEL[Number(p.charges)] ?? 0,
  cave_vines: (p) => (p.berries === true ? 14 : 0),
  cave_vines_plant: (p) => (p.berries === true ? 14 : 0),
  glow_lichen: () => 7,
  sculk_catalyst: () => 6
}

export function emitOf (name, props, base) {
  const p = props ?? {}
  if (p.lit === false) return 0
  const special = OVERRIDES[name]
  if (special) return special(p, base)
  if (/(^|_)candle$/.test(name)) return p.lit === true ? 3 * Number(p.candles ?? 1) : 0
  if (/(^|_)candle_cake$/.test(name)) return p.lit === true ? 3 : 0
  const bulb = name.replace(/^waxed_/, '')
  if (bulb in COPPER_BULB_LEVEL) return p.lit === true ? COPPER_BULB_LEVEL[bulb] : 0
  return base
}

const EPS = 1e-6

// Face planes: [axis, planeValue, uAxis, vAxis] for directions 0..5.
const FACE = [[1, 0, 0, 2], [1, 1, 0, 2], [2, 0, 0, 1], [2, 1, 0, 1], [0, 0, 1, 2], [0, 1, 1, 2]]

function coversFace (boxes, d) {
  const [axis, plane, ua, va] = FACE[d]
  const touching = boxes.filter((b) => (plane === 0 ? b[axis] <= EPS : b[axis + 3] >= 1 - EPS))
  if (touching.length === 0) return false
  const cuts = (a) => [...new Set([0, 1, ...touching.flatMap((b) => [b[a], b[a + 3]])])].sort((x, y) => x - y)
  const us = cuts(ua)
  const vs = cuts(va)
  return us.slice(1).every((u1, i) => vs.slice(1).every((v1, j) => {
    const cu = (us[i] + u1) / 2
    const cv = (vs[j] + v1) / 2
    return touching.some((b) => b[ua] <= cu && cu <= b[ua + 3] && b[va] <= cv && cv <= b[va + 3])
  }))
}

const isFullCube = (shapes) => shapes.length === 1 && shapes[0].slice(0, 3).every((v) => v <= EPS) && shapes[0].slice(3).every((v) => v >= 1 - EPS)

function facesOf (shapes) {
  if (!shapes || shapes.length === 0 || isFullCube(shapes)) return 0
  return [0, 1, 2, 3, 4, 5].reduce((m, d) => (coversFace(shapes, d) ? m | (1 << d) : m), 0)
}

export function lightTable (registry) {
  const Block = blockFactory(registry)
  // most states share a few dozen distinct collision shapes: compute each face mask once per shape
  const facesByShape = new Map()
  const facesCached = (shapes) => {
    const key = shapes.map((b) => b.join(',')).join(';')
    let mask = facesByShape.get(key)
    if (mask === undefined) facesByShape.set(key, mask = facesOf(shapes))
    return mask
  }
  const count = Math.max(...registry.blocksArray.map((b) => b.maxStateId)) + 1
  const emit = new Uint8Array(count)
  const filter = new Uint8Array(count)
  const faces = new Uint8Array(count)
  for (const block of registry.blocksArray) {
    const stateful = (block.states?.length ?? 0) > 0
    for (let id = block.minStateId; id <= block.maxStateId; id++) {
      const needShapes = block.filterLight < 15
      const inst = stateful || needShapes ? Block.fromStateId(id, 0) : null
      const props = stateful ? inst.getProperties() : {}
      emit[id] = emitOf(block.name, props, block.emitLight)
      filter[id] = props.waterlogged === true ? Math.max(block.filterLight, 1) : block.filterLight
      if (needShapes) faces[id] = facesCached(inst.shapes)
    }
  }
  return { emit, filter, faces }
}

export const boxIndex = (size, x, y, z) => (y * size[2] + z) * size[0] + x

// Bucket queues reused across calls: one Int32Array per level, sized to the box.
let buckets = null
let bucketCap = 0
const counts = new Int32Array(16)
const DX = [0, 0, 0, 0, -1, 1]
const DY = [-1, 1, 0, 0, 0, 0]
const DZ = [0, 0, -1, 1, 0, 0]

function ensureBuckets (cells) {
  if (buckets && bucketCap >= cells) return
  bucketCap = cells
  buckets = Array.from({ length: 16 }, () => new Int32Array(cells))
}

function flood (levels, filter, faces, states, size, isSky) {
  const [sx, sy, sz] = size
  const S = sx * sz
  const strides = [-S, S, -sx, sx, -1, 1]
  for (let L = 15; L >= 1; L--) {
    const q = buckets[L]
    // the bucket can grow while processed (sky 15 straight down stays 15)
    for (let h = 0; h < counts[L]; h++) {
      const a = q[h]
      if (levels[a] !== L) continue
      const x = a % sx
      const z = ((a / sx) | 0) % sz
      const y = (a / S) | 0
      const fa = faces[states[a]]
      for (let d = 0; d < 6; d++) {
        const bx = x + DX[d]
        const by = y + DY[d]
        const bz = z + DZ[d]
        if (bx < 1 || bx > sx - 2 || by < 1 || by > sy - 2 || bz < 1 || bz > sz - 2) continue
        const b = a + strides[d]
        const sb = states[b]
        if (((fa >> d) & 1) || ((faces[sb] >> (d ^ 1)) & 1)) continue
        const f = filter[sb]
        const nl = isSky && d === 0 && L === 15 && f === 0 ? 15 : L - (f > 1 ? f : 1)
        if (nl <= levels[b]) continue
        levels[b] = nl
        buckets[nl][counts[nl]++] = b
      }
    }
  }
}

export function relightBox ({ table, states, sky, block, size }) {
  const [sx, sy, sz] = size
  const cells = sx * sy * sz
  ensureBuckets(cells)
  const { emit, filter, faces } = table
  const outSky = new Uint8Array(cells)
  const outBlock = new Uint8Array(cells)
  counts.fill(0)
  for (let y = 0, i = 0; y < sy; y++) {
    for (let z = 0; z < sz; z++) {
      const edgeYZ = y === 0 || y === sy - 1 || z === 0 || z === sz - 1
      for (let x = 0; x < sx; x++, i++) {
        if (edgeYZ || x === 0 || x === sx - 1) {
          outBlock[i] = block[i]
          outSky[i] = sky[i]
        } else {
          outBlock[i] = emit[states[i]]
        }
      }
    }
  }
  // bucket seeding pass, separate per light kind
  const seedAll = (out, interiorEmit) => {
    counts.fill(0)
    for (let y = 0, i = 0; y < sy; y++) {
      for (let z = 0; z < sz; z++) {
        const edgeYZ = y === 0 || y === sy - 1 || z === 0 || z === sz - 1
        for (let x = 0; x < sx; x++, i++) {
          const shell = edgeYZ || x === 0 || x === sx - 1
          if (!shell && !interiorEmit) continue
          const v = out[i]
          if (v > 0) buckets[v][counts[v]++] = i
        }
      }
    }
  }
  seedAll(outBlock, true)
  flood(outBlock, filter, faces, states, size, false)
  seedAll(outSky, false)
  flood(outSky, filter, faces, states, size, true)
  return { sky: outSky, block: outBlock }
}
