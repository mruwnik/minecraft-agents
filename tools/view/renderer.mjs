// Why JavaScript: graphics/performance; software raycaster over chunk data.
// Eyes for the bot: a small software raycaster over the chunk data mineflayer already holds.
// Everything here is pure (no bot, no disk) so it can be tested without a server; the body feeds it the world.
import zlib from 'node:zlib'
import { canSee, placeLabels } from './web/mobs.mjs'
import { mobFor, mobPaint } from './mob-draw.mjs'
import { mobImage } from './mob-textures.mjs'
import { drawLabels } from './labels.mjs'
import { lightColor, skyDarken } from './web/shading.mjs'

// ---------------------------------------------------------------- png
const PNG_MAGIC = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10])
const CRC_TABLE = Array.from({ length: 256 }, (_, n) => {
  let c = n
  for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1
  return c >>> 0
})
const crc32 = buf => {
  let c = 0xffffffff
  for (const b of buf) c = CRC_TABLE[(c ^ b) & 0xff] ^ (c >>> 8)
  return (c ^ 0xffffffff) >>> 0
}
const pngChunk = (type, data) => {
  const body = Buffer.concat([Buffer.from(type, 'ascii'), data])
  const out = Buffer.alloc(body.length + 8)
  out.writeUInt32BE(data.length, 0)
  body.copy(out, 4)
  out.writeUInt32BE(crc32(body), body.length + 4)
  return out
}

export function encodePng (width, height, rgba) {
  const header = Buffer.alloc(13)
  header.writeUInt32BE(width, 0)
  header.writeUInt32BE(height, 4)
  header.set([8, 6, 0, 0, 0], 8)
  const stride = width * 4
  const raw = Buffer.alloc((stride + 1) * height)
  for (let y = 0; y < height; y++) raw.set(rgba.subarray(y * stride, (y + 1) * stride), y * (stride + 1) + 1)
  // the fastest level: a frame is 9 KB bigger and 2.5 ms sooner, and the stream sends ten a second
  return Buffer.concat([PNG_MAGIC, pngChunk('IHDR', header), pngChunk('IDAT', zlib.deflateSync(raw, { level: 1 })), pngChunk('IEND', Buffer.alloc(0))])
}

const CHANNELS = { 0: 1, 2: 3, 3: 1, 4: 2, 6: 4 }
const paeth = (a, b, c) => {
  const p = a + b - c
  const [pa, pb, pc] = [Math.abs(p - a), Math.abs(p - b), Math.abs(p - c)]
  return pa <= pb && pa <= pc ? a : pb <= pc ? b : c
}

function unfilter (data, rowBytes, bpp, height) {
  const out = new Uint8Array(rowBytes * height)
  for (let y = 0; y < height; y++) {
    const filter = data[y * (rowBytes + 1)]
    for (let i = 0; i < rowBytes; i++) {
      const x = data[y * (rowBytes + 1) + 1 + i]
      const a = i >= bpp ? out[y * rowBytes + i - bpp] : 0
      const b = y > 0 ? out[(y - 1) * rowBytes + i] : 0
      const c = i >= bpp && y > 0 ? out[(y - 1) * rowBytes + i - bpp] : 0
      const predicted = [0, a, b, (a + b) >> 1, paeth(a, b, c)][filter]
      out[y * rowBytes + i] = (x + predicted) & 0xff
    }
  }
  return out
}

// Handles what Mojang ships: 8-bit grey/rgb/rgba and 1/2/4/8-bit palettes, non-interlaced.
export function decodePng (file) {
  if (!file.subarray(0, 8).equals(PNG_MAGIC)) throw new Error('not a png')
  const chunks = {}
  const idat = []
  for (let at = 8; at < file.length;) {
    const length = file.readUInt32BE(at)
    const type = file.toString('ascii', at + 4, at + 8)
    const data = file.subarray(at + 8, at + 8 + length)
    if (type === 'IDAT') idat.push(data)
    else chunks[type] = data
    at += length + 12
  }
  const width = chunks.IHDR.readUInt32BE(0)
  const height = chunks.IHDR.readUInt32BE(4)
  const [depth, colorType, , , interlace] = chunks.IHDR.subarray(8)
  if (interlace || depth > 8) throw new Error(`unsupported png (depth ${depth}, interlace ${interlace})`)
  const channels = CHANNELS[colorType]
  const rowBytes = Math.ceil(width * channels * depth / 8)
  const pixels = unfilter(zlib.inflateSync(Buffer.concat(idat)), rowBytes, Math.max(1, channels * depth >> 3), height)
  const sample = (x, y, ch) => {
    if (depth === 8) return pixels[y * rowBytes + x * channels + ch]
    const bit = x * depth
    return (pixels[y * rowBytes + (bit >> 3)] >> (8 - depth - (bit & 7))) & ((1 << depth) - 1)
  }
  const greyScale = 255 / ((1 << depth) - 1)
  const rgba = new Uint8Array(width * height * 4)
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const s = ch => sample(x, y, ch)
      const px = colorType === 3
        ? [...chunks.PLTE.subarray(s(0) * 3, s(0) * 3 + 3), chunks.tRNS?.[s(0)] ?? 255]
        : colorType === 0 ? [s(0) * greyScale, s(0) * greyScale, s(0) * greyScale, 255]
          : colorType === 4 ? [s(0), s(0), s(0), s(1)]
            : colorType === 2 ? [s(0), s(1), s(2), 255]
              : [s(0), s(1), s(2), s(3)]
      rgba.set(px, (y * width + x) * 4)
    }
  }
  return { width, height, rgba }
}

// ---------------------------------------------------------------- textures
const SHAPED = /_(stairs|slab|wall|fence_gate|fence|pressure_plate|button|trapdoor|carpet|pane)$/
const FACE_SUFFIXES = { top: ['_top', ''], bottom: ['_bottom', '_top', ''], side: ['_side', '', '_front'], cross: ['', '_side'] }
// blocks whose picture is filed under another name
const DRAWN_AS = {
  snow_block: 'snow', magma_block: 'magma', bamboo: 'bamboo_stalk', bamboo_sapling: 'bamboo_stage0', dried_kelp_block: 'dried_kelp',
  ender_chest: 'obsidian', chest: 'oak_planks', trapped_chest: 'oak_planks', redstone_wire: 'redstone_dust_line0', fire: 'fire_0',
  soul_fire: 'soul_fire_0', frosted_ice: 'frosted_ice_0', petrified_oak_slab: 'oak_planks', light_weighted_pressure_plate: 'gold_block',
  heavy_weighted_pressure_plate: 'iron_block', piston_head: 'piston', sticky_piston: 'piston', moving_piston: 'piston',
  campfire: 'campfire_log', soul_campfire: 'soul_campfire_log'
}
// chests are entity-rendered: their real picture is an atlas under entity/chest/, not a plain texture under
// block/, so DRAWN_AS above only gives textureCandidates something plausible for the dashboard's inventory icon.
// The world view (render(), below) uses this dedicated colour instead, so a chest is not just a plank cube.
const BLOCK_COLORS = { chest: [162, 112, 63], trapped_chest: [138, 56, 43], ender_chest: [35, 48, 46] }
export const colorOf = block => BLOCK_COLORS[block]
// a wrapper, treatment or variant of a block that shares its picture; tried after the full name, so smooth_stone keeps its own
const plainName = name => name
  .replace(/^(waxed|infested|potted|smooth)_/, '')
  .replace(/^(water|lava|powder_snow)_cauldron$/, 'cauldron')
  .replace(/^(\w+_)?candle_cake$/, 'cake')
  .replace(/_wood$/, '_log')
  .replace(/_hyphae$/, '_stem')

function candidatesFor (name, face, props) {
  const half = props.half === 'upper' ? '_top' : '_bottom'
  const base = name.replace(SHAPED, '')
  const faced = n => FACE_SUFFIXES[face].map(s => `${n}${s}`)
  return [
    ...(props.age !== undefined ? [`${name}_stage${props.age}`] : []),
    ...(props.half === 'upper' || props.half === 'lower' ? [`${name}${half}`] : []),
    ...faced(name),
    ...(base === name ? [] : [...faced(base), `${base}_planks`, `${base}s`, `${base}_wool`, ...faced(`${base}_block`)])
  ]
}

// Texture file names (without .png) worth trying for a block face, best first. The caller picks the first that exists.
export function textureCandidates (block, face, props = {}) {
  if (block === 'water' || block === 'lava') return [`${block}_still`]
  if (block.endsWith('_bed')) return [block.replace(/_bed$/, '_wool')]
  if (block === 'grass_block' && face === 'bottom') return ['dirt']
  const name = DRAWN_AS[block] ?? block.replace('wall_', '')
  return [...new Set([name, plainName(name)].flatMap(n => candidatesFor(n, face, props)))]
}

// grass and leaves ship grey: the game colours them by biome, and this is a temperate one
const GRASS = [124, 189, 107]
const FOLIAGE = [89, 174, 48]
const TINTS = [
  [/^(grass_block_top|short_grass|tall_grass_(top|bottom)|fern|large_fern_(top|bottom))$/, GRASS],
  [/^birch_leaves$/, [128, 167, 85]],
  [/^spruce_leaves$/, [97, 153, 97]],
  [/^(oak|jungle|acacia|dark_oak|mangrove)_leaves$|^vine$|^lily_pad$/, FOLIAGE],
  [/^water_still$/, [63, 118, 228]]
]
export const tintOf = texture => TINTS.find(([re]) => re.test(texture))?.[1]

// ---------------------------------------------------------------- inventory icons
// A block item as the inventory screen draws it: a cube seen from above at 2:1, its front on the left, lit from the top.
// Each face is a parallelogram origin + s*A + t*B over the texture's (s, t); a pixel is mapped back to the one it lies in.
const ICON_FACES = (h, q) => [
  { origin: [h, 0], a: [h, q], b: [-h, q], light: 1 },
  { origin: [0, q], a: [h, q], b: [0, h], light: 0.8 },
  { origin: [h, h], a: [h, -q], b: [0, h], light: 0.6 }
]
export function blockIcon (top, left, right, size = 32) {
  const faces = ICON_FACES(size / 2, size / 4).map((face, i) => ({ ...face, image: [top, left, right][i] }))
  const rgba = new Uint8Array(size * size * 4)
  for (let y = 0; y < size; y++) {
    for (let x = 0; x < size; x++) {
      for (const { origin, a, b, light, image } of faces) {
        const [dx, dy] = [x + 0.5 - origin[0], y + 0.5 - origin[1]]
        const det = a[0] * b[1] - a[1] * b[0]
        const [s, t] = [(dx * b[1] - dy * b[0]) / det, (a[0] * dy - a[1] * dx) / det]
        if (s < 0 || s >= 1 || t < 0 || t >= 1) continue
        // square: an animated texture is its frames stacked, and only the first is wanted
        const at = (Math.floor(t * image.width) * image.width + Math.floor(s * image.width)) * 4
        rgba.set([0, 1, 2].map(i => Math.round(image.rgba[at + i] * light)), (y * size + x) * 4)
        rgba[(y * size + x) * 4 + 3] = image.rgba[at + 3]
        break
      }
    }
  }
  return { width: size, height: size, rgba }
}

// ---------------------------------------------------------------- world grid
// shared memory (blocks and light), so the render worker reads the very cells block updates are written into, without a copy per look
export const OPEN_SKY = 15 << 4 // a grid light byte, `sky << 4 | block`: open sky, no block light
export function makeGrid (origin, size) {
  const data = new Uint16Array(new SharedArrayBuffer(size.x * size.y * size.z * 2))
  // per cell `sky << 4 | block`; cells no dumped light covers count as open sky
  const light = new Uint8Array(new SharedArrayBuffer(size.x * size.y * size.z)).fill(OPEN_SKY)
  const index = (x, y, z) => {
    const lx = x - origin.x
    const ly = y - origin.y
    const lz = z - origin.z
    if (lx < 0 || ly < 0 || lz < 0 || lx >= size.x || ly >= size.y || lz >= size.z) return -1
    return (ly * size.z + lz) * size.x + lx
  }
  return {
    origin,
    size,
    data,
    light,
    set: (x, y, z, id) => { const i = index(x, y, z); if (i >= 0) data[i] = id }
  }
}

// ---------------------------------------------------------------- rays
const ENTRY_FACE = { x: ['east', 'west'], y: ['top', 'bottom'], z: ['south', 'north'] } // [moving negative, moving positive]

// Nearest intersection of a ray with an axis-aligned box, into `out` ({t, face}); false when it misses. Scalars only:
// a frame asks this a few hundred thousand times and each returned object was a share of the frame.
function rayBox (o, d, x1, y1, z1, x2, y2, z2, out) {
  let tNear = -Infinity
  let tFar = Infinity
  let axis = 'x'
  if (d.x === 0) { if (o.x < x1 || o.x > x2) return false } else {
    const t1 = (x1 - o.x) / d.x
    const t2 = (x2 - o.x) / d.x
    if (Math.min(t1, t2) > tNear) { tNear = Math.min(t1, t2); axis = 'x' }
    tFar = Math.min(tFar, Math.max(t1, t2))
  }
  if (d.y === 0) { if (o.y < y1 || o.y > y2) return false } else {
    const t1 = (y1 - o.y) / d.y
    const t2 = (y2 - o.y) / d.y
    if (Math.min(t1, t2) > tNear) { tNear = Math.min(t1, t2); axis = 'y' }
    tFar = Math.min(tFar, Math.max(t1, t2))
  }
  if (d.z === 0) { if (o.z < z1 || o.z > z2) return false } else {
    const t1 = (z1 - o.z) / d.z
    const t2 = (z2 - o.z) / d.z
    if (Math.min(t1, t2) > tNear) { tNear = Math.min(t1, t2); axis = 'z' }
    tFar = Math.min(tFar, Math.max(t1, t2))
  }
  if (tNear > tFar || tNear < 0) return false
  out.t = tNear
  out.face = ENTRY_FACE[axis][d[axis] > 0 ? 1 : 0]
  return true
}

// where a ray meets one of the two diagonal quads plants are drawn on (a*px + b*pz = c), or -1
const crossPlane = (o, d, x, y, z, a, b, c) => {
  const denom = a * d.x + b * d.z
  if (denom === 0) return -1
  const t = (c - a * o.x - b * o.z) / denom
  const lx = o.x + d.x * t - x
  const ly = o.y + d.y * t - y
  const lz = o.z + d.z * t - z
  return t >= 0 && lx >= 0 && lx <= 1 && ly >= 0 && ly <= 1 && lz >= 0 && lz <= 1 ? t : -1
}

const clamp01 = v => Math.min(1, Math.max(0, v))

// a hit, with the texel it lands on; `image` is filled in by the renderer's accept
const hitAt = (x, y, z, id, block, face, o, d, t) => {
  const lx = o.x + d.x * t - x
  const ly = o.y + d.y * t - y
  const lz = o.z + d.z * t - z
  const top = face === 'top' || face === 'bottom'
  return { x, y, z, id, block, t, face, u: face === 'east' || face === 'west' ? lz : lx, v: top ? lz : 1 - ly, image: null }
}

const boxHit = { t: 0, face: '' }
// accept() judges each hit on its own, so the nearest it takes is the nearest of those it takes
const nearestBoxHit = (block, x, y, z, id, o, d, maxDist, accept) => {
  let best = null
  for (const b of block.boxes) {
    if (!rayBox(o, d, x + clamp01(b[0]), y + clamp01(b[1]), z + clamp01(b[2]), x + clamp01(b[3]), y + clamp01(b[4]), z + clamp01(b[5]), boxHit)) continue
    if (boxHit.t > maxDist || (best && boxHit.t >= best.t)) continue
    const hit = hitAt(x, y, z, id, block, boxHit.face, o, d, boxHit.t)
    if (accept(hit)) best = hit
  }
  return best
}

const nearestCrossHit = (block, x, y, z, id, o, d, maxDist, accept) => {
  let best = null
  for (const t of [crossPlane(o, d, x, y, z, 1, -1, x - z), crossPlane(o, d, x, y, z, 1, 1, x + z + 1)]) {
    if (t < 0 || t > maxDist || (best && t >= best.t)) continue
    const hit = hitAt(x, y, z, id, block, 'cross', o, d, t)
    if (accept(hit)) best = hit
  }
  return best
}

// Walk the voxels along a ray (Amanatides & Woo) until something solid is hit. `info(stateId)` describes a block:
// null for air, {kind:'cube'}, {kind:'boxes', boxes} or {kind:'cross'}. `accept(hit)` can reject see-through texels.
// Every pixel walks a hundred-odd cells, so the walk keeps to scalars: an array or two per step was most of a frame.
export function castRay (grid, info, o, d, maxDist, accept = () => true) {
  const { data, origin, size } = grid
  // nothing stands above the grid's top, so a ray past it and not going down has nothing left to hit
  const top = grid.top ?? Infinity
  let x = Math.floor(o.x)
  let y = Math.floor(o.y)
  let z = Math.floor(o.z)
  const lx = x - origin.x
  const ly = y - origin.y
  const lz = z - origin.z
  if (lx < 0 || ly < 0 || lz < 0 || lx >= size.x || ly >= size.y || lz >= size.z) throw new Error('the eye is outside the grid')
  const sx = Math.sign(d.x)
  const sy = Math.sign(d.y)
  const sz = Math.sign(d.z)
  const dx = d.x === 0 ? Infinity : Math.abs(1 / d.x)
  const dy = d.y === 0 ? Infinity : Math.abs(1 / d.y)
  const dz = d.z === 0 ? Infinity : Math.abs(1 / d.z)
  let nx = d.x === 0 ? Infinity : ((d.x > 0 ? x + 1 : x) - o.x) / d.x
  let ny = d.y === 0 ? Infinity : ((d.y > 0 ? y + 1 : y) - o.y) / d.y
  let nz = d.z === 0 ? Infinity : ((d.z > 0 ? z + 1 : z) - o.z) / d.z
  // the steps left on each axis before the grid's far side, counted rather than timed: a time to the edge rounds
  // differently from the summed steps, and one cell too many reads the next row
  let leftX = sx > 0 ? size.x - 1 - lx : lx
  let leftY = sy > 0 ? size.y - 1 - ly : ly
  let leftZ = sz > 0 ? size.z - 1 - lz : lz
  const stepY = sy * size.x * size.z
  const stepZ = sz * size.x
  let i = (ly * size.z + lz) * size.x + lx
  let t = 0
  let entered = null
  while (t <= maxDist) {
    if (y > top && sy >= 0) return null
    const id = data[i]
    const block = id ? info(id) : null
    if (block) {
      const hit = block.kind === 'cube'
        ? (entered ? hitAt(x, y, z, id, block, entered, o, d, t) : null)
        : block.kind === 'cross'
          ? nearestCrossHit(block, x, y, z, id, o, d, maxDist, accept)
          : nearestBoxHit(block, x, y, z, id, o, d, maxDist, accept)
      if (hit && (block.kind !== 'cube' || accept(hit))) return hit
    }
    if (nx <= ny && nx <= nz) {
      if (leftX-- === 0) return null
      t = nx
      nx += dx
      x += sx
      i += sx
      entered = sx > 0 ? 'west' : 'east'
    } else if (ny <= nz) {
      if (leftY-- === 0) return null
      t = ny
      ny += dy
      y += sy
      i += stepY
      entered = sy > 0 ? 'bottom' : 'top'
    } else {
      if (leftZ-- === 0) return null
      t = nz
      nz += dz
      z += sz
      i += stepZ
      entered = sz > 0 ? 'north' : 'south'
    }
  }
  return null
}

// ---------------------------------------------------------------- camera
// mineflayer's convention: yaw 0 faces north (-z) and grows turning left; pitch > 0 looks up.
export const directionFor = (yaw, pitch) => ({
  x: -Math.sin(yaw) * Math.cos(pitch),
  y: Math.sin(pitch),
  z: -Math.cos(yaw) * Math.cos(pitch)
})
const cross = (a, b) => ({ x: a.y * b.z - a.z * b.y, y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x })

// ray(px, py, out) writes a pixel's unit direction into `out`. project(p) is the pixel a point lands on, or null behind
// the eye; a panorama has none, as its projection is not linear and a box's corners do not bound its picture
function cameraFor ({ panorama, yaw = 0, pitch = 0, fov = 90, width, height }) {
  if (panorama) {
    // 360 degrees around, north in the middle, square pixels at the horizon
    const vfov = 2 * Math.PI * height / width
    return {
      ray: (px, py, out) => {
        const d = directionFor((0.5 - (px + 0.5) / width) * 2 * Math.PI, (0.5 - (py + 0.5) / height) * vfov)
        out.x = d.x
        out.y = d.y
        out.z = d.z
      },
      project: null
    }
  }
  const forward = directionFor(yaw, pitch)
  const right = directionFor(yaw - Math.PI / 2, 0)
  const up = cross(right, forward)
  const half = Math.tan(fov * Math.PI / 360)
  return {
    ray: (px, py, out) => {
      const sx = ((px + 0.5) / width * 2 - 1) * half
      const sy = (1 - (py + 0.5) / height * 2) * half * height / width
      const x = forward.x + right.x * sx + up.x * sy
      const y = forward.y + right.y * sx + up.y * sy
      const z = forward.z + right.z * sx + up.z * sy
      const len = Math.hypot(x, y, z)
      out.x = x / len
      out.y = y / len
      out.z = z / len
    },
    project: p => {
      const f = p.x * forward.x + p.y * forward.y + p.z * forward.z
      if (f <= 1e-9) return null
      const sx = (p.x * right.x + p.y * right.y + p.z * right.z) / f
      const sy = (p.x * up.x + p.y * up.y + p.z * up.z) / f
      return { px: (sx / half + 1) / 2 * width - 0.5, py: (1 - sy / (half * height / width)) / 2 * height - 0.5 }
    }
  }
}

// ---------------------------------------------------------------- render
const FACE_SHADE = { top: 1, bottom: 0.5, north: 0.8, south: 0.8, east: 0.62, west: 0.62, cross: 0.95 }
const hashColor = name => {
  let h = 0
  for (const c of name) h = (h * 31 + c.charCodeAt(0)) >>> 0
  return [90 + h % 130, 90 + (h >> 8) % 130, 90 + (h >> 16) % 130]
}
// ---------------------------------------------------------------- light
// the light byte of a cell (`sky << 4 | block`), open sky outside the grid
const lightByte = (grid, x, y, z) => {
  const { origin, size } = grid
  const lx = x - origin.x
  const ly = y - origin.y
  const lz = z - origin.z
  if (lx < 0 || ly < 0 || lz < 0 || lx >= size.x || ly >= size.y || lz >= size.z) return OPEN_SKY
  return grid.light[(ly * size.z + lz) * size.x + lx]
}
const FACE_NORMAL = { top: [0, 1, 0], bottom: [0, -1, 0], east: [1, 0, 0], west: [-1, 0, 0], south: [0, 0, 1], north: [0, 0, -1] }
const brighter = (a, b) => (Math.max(a >> 4, b >> 4) << 4) | Math.max(a & 15, b & 15)
// The light a hit face shows: the brighter of its own cell and the cell across the face (an opaque block stores 0, so
// its face takes the air's light; a slab, torch or plant keeps its own).
export const hitLight = (grid, hit) => {
  const own = lightByte(grid, hit.x, hit.y, hit.z)
  const n = FACE_NORMAL[hit.face]
  return n ? brighter(own, lightByte(grid, hit.x + n[0], hit.y + n[1], hit.z + n[2])) : own
}
// the light at an entity: the cell around the middle of its height
export const entityLight = (grid, e) => lightByte(grid, Math.floor(e.x), Math.floor(e.y + e.height / 2), Math.floor(e.z))

// vanilla's lightmap (default brightness) for a time of day and rain: 256 [r, g, b] multipliers indexed by a light byte
export const lightTable = (timeOfDay, rain = 0) => {
  const darken = skyDarken(timeOfDay ?? 6000, rain ?? 0)
  return Array.from({ length: 256 }, (_, i) => lightColor(i >> 4, i & 15, darken))
}
// {sky, block, seeing} of a light byte; seeing is the lightmap's brightest channel, 0.1 (no light) .. 1
export const lightReport = (table, byte) => ({ sky: byte >> 4, block: byte & 15, seeing: Math.round(Math.max(...table[byte]) * 1000) / 1000 })

// the mobs a body could see, as entities: lit enough or close (web/mobs.mjs canSee); the others get no name label
export const visibleMobs = (mobs, table, eye) => mobs
  .filter(m => canSee(lightReport(table, m.light).seeing, Math.hypot(m.e.x - eye.x, m.e.y + m.e.height / 2 - eye.y, m.e.z - eye.z)))
  .map(m => m.e)

// brightness of the sky colour, 0.3 (midnight) .. 1 (day); timeOfDay 0 is sunrise, 6000 noon, 18000 midnight
const daylight = time => 0.3 + 0.7 * clamp01(0.5 + 1.6 * Math.sin(((time ?? 6000) % 24000) / 24000 * 2 * Math.PI))
const mix = (a, b, k) => a + (b - a) * k

function texel (image, u, v) {
  const size = image.width // animated textures are vertical strips; use the first frame
  const tx = Math.min(size - 1, Math.floor(clamp01(u) * size))
  const ty = Math.min(size - 1, Math.floor(clamp01(v) * size))
  return (ty * image.width + tx) * 4
}

// a view is 'blocked' when most of it is a block face closer than this (standing in a hole, nose against a wall)
const NEAR = 2

// the pixels an entity's box can cover: its corners projected, a pixel wider for rounding; every pixel when the camera
// cannot project it or the box straddles the eye. null when the whole box is behind the eye, which no ray reaches
const PAD = 1
const screenRect = (project, eye, box, width, height) => {
  if (!project) return [0, 0, width - 1, height - 1]
  let x1 = Infinity
  let y1 = Infinity
  let x2 = -Infinity
  let y2 = -Infinity
  let behind = 0
  for (let i = 0; i < 8; i++) {
    const p = project({ x: box[i & 1 ? 3 : 0] - eye.x, y: box[i & 2 ? 4 : 1] - eye.y, z: box[i & 4 ? 5 : 2] - eye.z })
    if (!p) { behind++; continue }
    x1 = Math.min(x1, p.px)
    y1 = Math.min(y1, p.py)
    x2 = Math.max(x2, p.px)
    y2 = Math.max(y2, p.py)
  }
  if (behind === 8) return null
  if (behind) return [0, 0, width - 1, height - 1]
  return [Math.max(0, Math.floor(x1) - PAD), Math.max(0, Math.floor(y1) - PAD), Math.min(width - 1, Math.ceil(x2) + PAD), Math.min(height - 1, Math.ceil(y2) + PAD)]
}

// Draw the world. `near` is the fraction of the picture closer than NEAR. `texture(blockName, face, props)` returns {width,height,rgba,tint?} or null; entities are
// {name, kind?, x, y, z, width, height, yaw?}, drawn as their family's parts. Returns {width,height,rgba,seen} where `seen` lists the entities
// that actually ended up on screen (not hidden behind blocks) with the pixel they are centred on and the box of pixels
// they cover.
export function render ({ grid, info, texture, eye, entities = [], timeOfDay, rain = 0, width, height, maxDist = 64, mobPictures = mobImage, ...camera }) {
  const cam = cameraFor({ ...camera, width, height })
  const light = daylight(timeOfDay)
  const table = lightTable(timeOfDay, rain)
  const rgba = new Uint8Array(width * height * 4)
  const depth = new Float32Array(width * height) // the terrain's distance per pixel, what a name label is hidden by
  const mobs = entities
    .filter(e => Math.hypot(e.x - eye.x, e.y - eye.y, e.z - eye.z) <= maxDist)
    .map(e => mobFor(e, eye))
    .map(m => ({ ...m, rect: screenRect(cam.project, eye, m.box, width, height), light: entityLight(grid, m.e) }))
    .filter(m => m.rect)
  let nearPixels = 0
  // one block description serves every cell of that state for as long as `info` keeps it, so its pictures are looked
  // up once, not once a hit
  const picture = (block, face) => {
    const key = face === 'top' || face === 'bottom' || face === 'cross' ? face : 'side'
    const faces = block.pictures ??= {}
    if (!(key in faces)) {
      const color = colorOf(block.name)
      faces[key] = color ? { width: 1, height: 1, rgba: Uint8Array.from([...color, 255]) } : texture(block.name, key, block.props)
    }
    return faces[key]
  }
  const accept = hit => {
    const image = picture(hit.block, hit.face)
    hit.image = image
    return !image || image.rgba[texel(image, hit.u, hit.v) + 3] >= 128
  }
  const d = { x: 0, y: 0, z: 0 }
  const local = { x: 0, y: 0, z: 0 }
  const nearestLocal = { x: 0, y: 0, z: 0 }
  for (let py = 0; py < height; py++) {
    const rowMobs = mobs.filter(m => py >= m.rect[1] && py <= m.rect[3])
    for (let px = 0; px < width; px++) {
      cam.ray(px, py, d)
      const up = clamp01(d.y)
      const skyR = mix(200, 105, up) * light
      const skyG = mix(222, 160, up) * light
      const skyB = mix(255, 250, up) * light
      const hit = castRay(grid, info, eye, d, maxDist, accept)
      const limit = hit ? hit.t : maxDist
      depth[py * width + px] = limit
      let nearest = null
      let nearestT = Infinity
      let nearestFace = null
      let nearestPart = null
      for (const m of rowMobs) {
        if (px < m.rect[0] || px > m.rect[2]) continue
        local.x = d.x * m.right.x + d.z * m.right.z
        local.y = d.y
        local.z = d.x * m.forward.x + d.z * m.forward.z
        const h = m.hull
        if (!rayBox(m.eye, local, h[0], h[1], h[2], h[3], h[4], h[5], boxHit) || boxHit.t >= limit || boxHit.t >= nearestT) continue
        for (const p of m.parts) {
          if (!rayBox(m.eye, local, p[0], p[1], p[2], p[3], p[4], p[5], boxHit)) continue
          if (boxHit.t < limit && boxHit.t < nearestT) { nearest = m; nearestT = boxHit.t; nearestFace = boxHit.face; nearestPart = p; nearestLocal.x = local.x; nearestLocal.y = local.y; nearestLocal.z = local.z }
        }
      }
      let r = skyR
      let g = skyG
      let b = skyB
      if (nearest) {
        depth[py * width + px] = nearestT // a mob hides the labels behind it too
        nearest.pixels++
        nearest.sumX += px
        nearest.sumY += py
        if (px < nearest.x1) nearest.x1 = px
        if (px > nearest.x2) nearest.x2 = px
        if (py < nearest.y1) nearest.y1 = py
        if (py > nearest.y2) nearest.y2 = py
        // 'south' is the mob's own front: the ray came in through its +z face
        const base = mobPaint(nearest, nearestPart, nearestFace, nearestT, nearestLocal, mobPictures)
        const lit = table[nearest.light]
        r = base[0] * FACE_SHADE[nearestFace] * lit[0]
        g = base[1] * FACE_SHADE[nearestFace] * lit[1]
        b = base[2] * FACE_SHADE[nearestFace] * lit[2]
      } else if (hit) {
        if (hit.t < NEAR) nearPixels++
        const byte = hitLight(grid, hit)
        const lit = table[byte]
        // fog fades toward the sky only as far as the sky reaches the hit: a dark cave stays dark far off
        const fog = (hit.t / maxDist) ** 2 * (byte >> 4) / 15
        const shade = FACE_SHADE[hit.face]
        if (hit.image) {
          const at = texel(hit.image, hit.u, hit.v)
          const tint = hit.image.tint
          r = mix(hit.image.rgba[at] * (tint?.[0] ?? 255) / 255 * shade * lit[0], skyR, fog)
          g = mix(hit.image.rgba[at + 1] * (tint?.[1] ?? 255) / 255 * shade * lit[1], skyG, fog)
          b = mix(hit.image.rgba[at + 2] * (tint?.[2] ?? 255) / 255 * shade * lit[2], skyB, fog)
        } else {
          const base = hashColor(hit.block.name ?? String(hit.id))
          r = mix(base[0] * shade * lit[0], skyR, fog)
          g = mix(base[1] * shade * lit[1], skyG, fog)
          b = mix(base[2] * shade * lit[2], skyB, fog)
        }
      }
      const at = (py * width + px) * 4
      rgba[at] = r
      rgba[at + 1] = g
      rgba[at + 2] = b
      rgba[at + 3] = 255
    }
  }
  const seen = mobs.filter(m => m.pixels > 0).map(({ e, light: byte, pixels, sumX, sumY, x1, y1, x2, y2 }) => ({
    name: e.label ?? e.name,
    kind: e.kind,
    px: Math.round(sumX / pixels),
    py: Math.round(sumY / pixels),
    dist: Math.round(Math.hypot(e.x - eye.x, e.y + e.height / 2 - eye.y, e.z - eye.z)),
    box: [x1, y1, x2, y2],
    light: lightReport(table, byte)
  }))
  if (cam.project) drawLabels({ rgba, width, height, depth, labels: placeLabels({ eye, project: cam.project, width, height, entities: visibleMobs(mobs, table, eye) }) })
  return { width, height, rgba, seen, near: nearPixels / (width * height) }
}
