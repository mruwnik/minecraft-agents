// Eyes for the bot: a small software raycaster over the chunk data mineflayer already holds.
// Everything here is pure (no bot, no disk) so it can be tested without a server; the body feeds it the world.
import zlib from 'node:zlib'

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
// shared memory, so the render worker reads the very cells block updates are written into, without a copy per look
export function makeGrid (origin, size) {
  const data = new Uint16Array(new SharedArrayBuffer(size.x * size.y * size.z * 2))
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
const HOSTILE = [225, 35, 35]
const hashColor = name => {
  let h = 0
  for (const c of name) h = (h * 31 + c.charCodeAt(0)) >>> 0
  return [90 + h % 130, 90 + (h >> 8) % 130, 90 + (h >> 16) % 130]
}
// [body, head, limbs, face] in the game's colours; the face is the front of the head, so it shows which way a mob looks
const plain = (c, face = c.map(v => v * 0.55)) => [c, c, c, face]
const PALETTES = {
  player: [[235, 60, 235], [215, 160, 125], [235, 60, 235], [120, 80, 60]],
  zombie: [[40, 150, 155], [95, 150, 80], [65, 60, 160], [40, 70, 40]],
  zombie_villager: [[110, 80, 60], [95, 150, 80], [90, 65, 50], [40, 70, 40]],
  husk: [[150, 125, 85], [185, 160, 110], [110, 90, 65], [90, 75, 50]],
  drowned: [[60, 140, 130], [85, 165, 150], [70, 110, 140], [35, 80, 75]],
  skeleton: [[205, 205, 195], [215, 215, 205], [190, 190, 180], [70, 70, 70]],
  stray: [[165, 185, 190], [205, 210, 210], [150, 170, 175], [70, 80, 85]],
  bogged: [[150, 165, 120], [190, 195, 170], [130, 140, 105], [60, 70, 50]],
  wither_skeleton: [[45, 45, 45], [55, 55, 55], [35, 35, 35], [20, 20, 20]],
  creeper: [[85, 175, 65], [95, 185, 75], [70, 150, 55], [25, 35, 25]],
  spider: [[45, 38, 35], [55, 48, 45], [35, 30, 28], [150, 25, 25]],
  cave_spider: [[25, 60, 70], [30, 70, 80], [20, 45, 55], [150, 25, 25]],
  enderman: [[25, 20, 30], [30, 25, 35], [20, 15, 25], [200, 90, 235]],
  witch: [[80, 45, 100], [145, 170, 105], [60, 35, 75], [70, 90, 50]],
  pillager: [[85, 90, 95], [150, 155, 145], [55, 55, 60], [80, 85, 80]],
  vindicator: [[60, 70, 75], [150, 155, 145], [45, 45, 50], [80, 85, 80]],
  evoker: [[40, 40, 45], [150, 155, 145], [130, 110, 50], [80, 85, 80]],
  piglin: [[200, 150, 90], [230, 160, 140], [110, 75, 50], [160, 100, 90]],
  zombified_piglin: [[225, 150, 140], [225, 150, 140], [110, 140, 80], [120, 80, 75]],
  blaze: plain([250, 190, 40], [120, 70, 20]),
  slime: plain([110, 190, 90], [40, 80, 35]),
  cow: [[95, 65, 45], [95, 65, 45], [225, 220, 210], [230, 225, 215]],
  mooshroom: [[170, 30, 30], [170, 30, 30], [225, 220, 210], [230, 225, 215]],
  pig: [[240, 160, 165], [240, 160, 165], [225, 140, 145], [250, 190, 190]],
  sheep: [[235, 235, 230], [215, 185, 160], [215, 185, 160], [150, 120, 100]],
  chicken: [[250, 250, 250], [250, 250, 250], [235, 165, 50], [235, 165, 50]],
  horse: [[150, 110, 70], [150, 110, 70], [120, 85, 55], [60, 45, 30]],
  wolf: [[215, 210, 210], [215, 210, 210], [200, 195, 195], [60, 55, 55]],
  cat: [[200, 150, 80], [200, 150, 80], [180, 130, 70], [90, 70, 40]],
  fox: [[225, 120, 45], [225, 120, 45], [60, 40, 30], [240, 235, 225]],
  villager: [[120, 85, 60], [200, 150, 120], [100, 70, 50], [150, 105, 85]],
  wandering_trader: [[50, 80, 150], [200, 150, 120], [40, 60, 115], [150, 105, 85]],
  iron_golem: [[205, 200, 190], [215, 210, 200], [185, 180, 170], [110, 90, 70]],
  item: plain([255, 225, 40], [255, 225, 40])
}
const paletteFor = e => PALETTES[e.name] ?? PALETTES[e.kind] ?? plain(e.kind === 'hostile' ? HOSTILE : hashColor(e.name))
// brightness of the day, 0.3 (midnight) .. 1 (day); timeOfDay 0 is sunrise, 6000 noon, 18000 midnight
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

// ---------------------------------------------------------------- mobs
// A mob is drawn as a few boxes in its own frame: x across and z forward in widths, y up in heights, so one table
// serves a chicken and a ravager. The last number picks the palette entry: 0 body, 1 head, 2 limbs.
const FAMILIES = {
  biped: [[-0.42, 0.75, -0.42, 0.42, 1, 0.42, 1], [-0.42, 0.375, -0.21, 0.42, 0.75, 0.21, 0], [-0.83, 0.375, -0.21, -0.42, 0.75, 0.21, 0],
    [0.42, 0.375, -0.21, 0.83, 0.75, 0.21, 0], [-0.42, 0, -0.21, 0, 0.375, 0.21, 2], [0, 0, -0.21, 0.42, 0.375, 0.21, 2]],
  quadruped: [[-0.5, 0.4, -0.8, 0.5, 0.8, 0.55, 0], [-0.33, 0.55, 0.55, 0.33, 1, 0.95, 1], [-0.45, 0, -0.75, -0.15, 0.4, -0.45, 2],
    [0.15, 0, -0.75, 0.45, 0.4, -0.45, 2], [-0.45, 0, 0.2, -0.15, 0.4, 0.5, 2], [0.15, 0, 0.2, 0.45, 0.4, 0.5, 2]],
  creeper: [[-0.42, 0.7, -0.42, 0.42, 1, 0.42, 1], [-0.42, 0.25, -0.25, 0.42, 0.7, 0.25, 0], [-0.42, 0, 0.25, 0, 0.25, 0.6, 2],
    [0, 0, 0.25, 0.42, 0.25, 0.6, 2], [-0.42, 0, -0.6, 0, 0.25, -0.25, 2], [0, 0, -0.6, 0.42, 0.25, -0.25, 2]],
  spider: [[-0.3, 0.25, -0.55, 0.3, 0.8, 0, 0], [-0.2, 0.25, 0, 0.2, 0.65, 0.3, 1], ...[-0.25, -0.1, 0.05, 0.2].map(z => [-0.5, 0.05, z, 0.5, 0.4, z + 0.06, 2])],
  bird: [[-0.5, 0.3, -0.5, 0.5, 0.75, 0.4, 0], [-0.3, 0.6, 0.25, 0.3, 1, 0.65, 1], [-0.3, 0, -0.05, -0.1, 0.3, 0.1, 2], [0.1, 0, -0.05, 0.3, 0.3, 0.1, 2]],
  blob: [[-0.5, 0, -0.5, 0.5, 1, 0.5, 1]]
}
const FAMILY_OF = Object.fromEntries(Object.entries({
  biped: 'player zombie husk drowned skeleton stray bogged parched wither_skeleton villager wandering_trader pillager vindicator evoker illusioner witch piglin piglin_brute zombified_piglin zombie_villager enderman iron_golem snow_golem creaking warden',
  quadruped: 'cow mooshroom pig sheep goat horse donkey mule skeleton_horse zombie_horse llama trader_llama camel camel_husk wolf fox cat ocelot polar_bear panda hoglin zoglin ravager sniffer armadillo turtle',
  creeper: 'creeper',
  spider: 'spider cave_spider',
  bird: 'chicken parrot'
}).flatMap(([family, names]) => names.split(' ').map(name => [name, family])))

// The mob's parts sized to it, the eye turned into its frame (rays are turned per pixel), and the world-space box
// round its turned parts for screenRect. Turning keeps lengths, so a hit's t compares with the terrain's directly.
const mobFor = (e, eye) => {
  const parts = FAMILIES[FAMILY_OF[e.name] ?? (e.height >= 2 * e.width ? 'biped' : 'blob')]
    .map(([x1, y1, z1, x2, y2, z2, paint]) => [x1 * e.width, y1 * e.height, z1 * e.width, x2 * e.width, y2 * e.height, z2 * e.width, paint])
  const hull = [0, 1, 2].map(i => Math.min(...parts.map(p => p[i]))).concat([3, 4, 5].map(i => Math.max(...parts.map(p => p[i]))))
  const yaw = e.yaw ?? 0
  const right = { x: Math.cos(yaw), z: -Math.sin(yaw) }
  const forward = { x: -Math.sin(yaw), z: -Math.cos(yaw) }
  const corners = [[hull[0], hull[2]], [hull[3], hull[2]], [hull[0], hull[5]], [hull[3], hull[5]]]
    .map(([x, z]) => [e.x + x * right.x + z * forward.x, e.z + x * right.z + z * forward.z])
  const ox = eye.x - e.x
  const oz = eye.z - e.z
  return {
    e,
    parts,
    hull,
    right,
    forward,
    palette: paletteFor(e),
    eye: { x: ox * right.x + oz * right.z, y: eye.y - e.y, z: ox * forward.x + oz * forward.z },
    box: [Math.min(...corners.map(c => c[0])), e.y + hull[1], Math.min(...corners.map(c => c[1])), Math.max(...corners.map(c => c[0])), e.y + hull[4], Math.max(...corners.map(c => c[1]))],
    pixels: 0,
    sumX: 0,
    sumY: 0,
    x1: Infinity,
    y1: Infinity,
    x2: -Infinity,
    y2: -Infinity
  }
}

// Draw the world. `near` is the fraction of the picture closer than NEAR. `texture(blockName, face, props)` returns {width,height,rgba,tint?} or null; entities are
// {name, kind?, x, y, z, width, height, yaw?}, drawn as their family's parts. Returns {width,height,rgba,seen} where `seen` lists the entities
// that actually ended up on screen (not hidden behind blocks) with the pixel they are centred on and the box of pixels
// they cover.
export function render ({ grid, info, texture, eye, entities = [], timeOfDay, width, height, maxDist = 64, ...camera }) {
  const cam = cameraFor({ ...camera, width, height })
  const light = daylight(timeOfDay)
  const rgba = new Uint8Array(width * height * 4)
  const mobs = entities
    .filter(e => Math.hypot(e.x - eye.x, e.y - eye.y, e.z - eye.z) <= maxDist)
    .map(e => mobFor(e, eye))
    .map(m => ({ ...m, rect: screenRect(cam.project, eye, m.box, width, height) }))
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
      let nearest = null
      let nearestT = Infinity
      let nearestFace = null
      let nearestPaint = 0
      for (const m of rowMobs) {
        if (px < m.rect[0] || px > m.rect[2]) continue
        local.x = d.x * m.right.x + d.z * m.right.z
        local.y = d.y
        local.z = d.x * m.forward.x + d.z * m.forward.z
        const h = m.hull
        if (!rayBox(m.eye, local, h[0], h[1], h[2], h[3], h[4], h[5], boxHit) || boxHit.t >= limit || boxHit.t >= nearestT) continue
        for (const p of m.parts) {
          if (!rayBox(m.eye, local, p[0], p[1], p[2], p[3], p[4], p[5], boxHit)) continue
          if (boxHit.t < limit && boxHit.t < nearestT) { nearest = m; nearestT = boxHit.t; nearestFace = boxHit.face; nearestPaint = p[6] }
        }
      }
      let r = skyR
      let g = skyG
      let b = skyB
      if (nearest) {
        nearest.pixels++
        nearest.sumX += px
        nearest.sumY += py
        if (px < nearest.x1) nearest.x1 = px
        if (px > nearest.x2) nearest.x2 = px
        if (py < nearest.y1) nearest.y1 = py
        if (py > nearest.y2) nearest.y2 = py
        // 'south' is the mob's own front: the ray came in through its +z face
        const base = nearest.palette[nearestPaint === 1 && nearestFace === 'south' ? 3 : nearestPaint]
        r = base[0] * FACE_SHADE[nearestFace] * Math.max(light, 0.6)
        g = base[1] * FACE_SHADE[nearestFace] * Math.max(light, 0.6)
        b = base[2] * FACE_SHADE[nearestFace] * Math.max(light, 0.6)
      } else if (hit) {
        if (hit.t < NEAR) nearPixels++
        const fog = (hit.t / maxDist) ** 2
        if (hit.image) {
          const at = texel(hit.image, hit.u, hit.v)
          const tint = hit.image.tint
          r = mix(hit.image.rgba[at] * (tint?.[0] ?? 255) / 255 * FACE_SHADE[hit.face] * light, skyR, fog)
          g = mix(hit.image.rgba[at + 1] * (tint?.[1] ?? 255) / 255 * FACE_SHADE[hit.face] * light, skyG, fog)
          b = mix(hit.image.rgba[at + 2] * (tint?.[2] ?? 255) / 255 * FACE_SHADE[hit.face] * light, skyB, fog)
        } else {
          const base = hashColor(hit.block.name ?? String(hit.id))
          r = mix(base[0] * FACE_SHADE[hit.face] * light, skyR, fog)
          g = mix(base[1] * FACE_SHADE[hit.face] * light, skyG, fog)
          b = mix(base[2] * FACE_SHADE[hit.face] * light, skyB, fog)
        }
      }
      const at = (py * width + px) * 4
      rgba[at] = r
      rgba[at + 1] = g
      rgba[at + 2] = b
      rgba[at + 3] = 255
    }
  }
  const seen = mobs.filter(m => m.pixels > 0).map(({ e, pixels, sumX, sumY, x1, y1, x2, y2 }) => ({
    name: e.label ?? e.name,
    kind: e.kind,
    px: Math.round(sumX / pixels),
    py: Math.round(sumY / pixels),
    dist: Math.round(Math.hypot(e.x - eye.x, e.y + e.height / 2 - eye.y, e.z - eye.z)),
    box: [x1, y1, x2, y2]
  }))
  return { width, height, rgba, seen, near: nearPixels / (width * height) }
}
