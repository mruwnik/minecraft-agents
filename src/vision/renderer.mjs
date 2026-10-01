// Eyes for the bot: a small software raycaster over the chunk data mineflayer already holds.
// Everything here is pure (no bot, no disk) so it can be tested without a server; bot.mjs feeds it the world.
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
  return Buffer.concat([PNG_MAGIC, pngChunk('IHDR', header), pngChunk('IDAT', zlib.deflateSync(raw)), pngChunk('IEND', Buffer.alloc(0))])
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
const AXES = ['x', 'y', 'z']
const ENTRY_FACE = { x: ['east', 'west'], y: ['top', 'bottom'], z: ['south', 'north'] } // [moving negative, moving positive]

// Nearest intersection of a ray with an axis-aligned box [x1,y1,z1,x2,y2,z2], or null. t >= 0.
function rayBox (o, d, box) {
  let tNear = -Infinity
  let tFar = Infinity
  let axis = 'x'
  for (const [i, a] of AXES.entries()) {
    if (d[a] === 0) {
      if (o[a] < box[i] || o[a] > box[i + 3]) return null
      continue
    }
    const t1 = (box[i] - o[a]) / d[a]
    const t2 = (box[i + 3] - o[a]) / d[a]
    if (Math.min(t1, t2) > tNear) { tNear = Math.min(t1, t2); axis = a }
    tFar = Math.min(tFar, Math.max(t1, t2))
  }
  if (tNear > tFar || tNear < 0) return null
  return { t: tNear, face: ENTRY_FACE[axis][d[axis] > 0 ? 1 : 0] }
}

// The two diagonal quads that plants are drawn on.
function rayCross (o, d, x, y, z) {
  const planes = [[1, -1, x - z], [1, 1, x + z + 1]] // a*px + b*pz = c
  const hits = planes.flatMap(([a, b, c]) => {
    const denom = a * d.x + b * d.z
    if (denom === 0) return []
    const t = (c - a * o.x - b * o.z) / denom
    const [lx, ly, lz] = [o.x + d.x * t - x, o.y + d.y * t - y, o.z + d.z * t - z]
    return t >= 0 && lx >= 0 && lx <= 1 && ly >= 0 && ly <= 1 && lz >= 0 && lz <= 1 ? [{ t, face: 'cross', u: lx, v: 1 - ly }] : []
  })
  return hits.sort((p, q) => p.t - q.t)
}

function faceUV (face, o, d, t, x, y, z) {
  const [lx, ly, lz] = [o.x + d.x * t - x, o.y + d.y * t - y, o.z + d.z * t - z]
  if (face === 'top' || face === 'bottom') return { u: lx, v: lz }
  return { u: face === 'north' || face === 'south' ? lx : lz, v: 1 - ly }
}

const clamp01 = v => Math.min(1, Math.max(0, v))

// Walk the voxels along a ray (Amanatides & Woo) until something solid is hit. `info(stateId)` describes a block:
// null for air, {kind:'cube'}, {kind:'boxes', boxes} or {kind:'cross'}. `accept(hit)` can reject see-through texels.
// Every pixel walks a hundred-odd cells, so the walk keeps to scalars: an array or two per step was most of a frame.
export function castRay (grid, info, o, d, maxDist, accept = () => true) {
  const { data, origin, size } = grid
  let x = Math.floor(o.x)
  let y = Math.floor(o.y)
  let z = Math.floor(o.z)
  const sx = Math.sign(d.x)
  const sy = Math.sign(d.y)
  const sz = Math.sign(d.z)
  const dx = d.x === 0 ? Infinity : Math.abs(1 / d.x)
  const dy = d.y === 0 ? Infinity : Math.abs(1 / d.y)
  const dz = d.z === 0 ? Infinity : Math.abs(1 / d.z)
  let nx = d.x === 0 ? Infinity : ((d.x > 0 ? x + 1 : x) - o.x) / d.x
  let ny = d.y === 0 ? Infinity : ((d.y > 0 ? y + 1 : y) - o.y) / d.y
  let nz = d.z === 0 ? Infinity : ((d.z > 0 ? z + 1 : z) - o.z) / d.z
  let t = 0
  let entered = null
  while (t <= maxDist) {
    const lx = x - origin.x
    const ly = y - origin.y
    const lz = z - origin.z
    const inside = lx >= 0 && ly >= 0 && lz >= 0 && lx < size.x && ly < size.y && lz < size.z
    // past the grid's far side there is nothing left to hit
    if (!inside && ((lx < 0 && sx <= 0) || (lx >= size.x && sx >= 0) || (ly < 0 && sy <= 0) || (ly >= size.y && sy >= 0) || (lz < 0 && sz <= 0) || (lz >= size.z && sz >= 0))) return null
    const id = inside ? data[(ly * size.z + lz) * size.x + lx] : 0
    const block = id ? info(id) : null
    if (block) {
      const found = block.kind === 'cube'
        ? (entered ? [{ t, face: entered }] : [])
        : block.kind === 'cross'
          ? rayCross(o, d, x, y, z)
          : block.boxes.map(b => rayBox(o, d, [x + clamp01(b[0]), y + clamp01(b[1]), z + clamp01(b[2]), x + clamp01(b[3]), y + clamp01(b[4]), z + clamp01(b[5])])).filter(Boolean).sort((p, q) => p.t - q.t)
      for (const f of found) {
        const hit = { x, y, z, id, block, ...faceUV(f.face, o, d, f.t, x, y, z), ...f }
        if (hit.t <= maxDist && accept(hit)) return hit
      }
    }
    if (nx <= ny && nx <= nz) {
      t = nx
      nx += dx
      x += sx
      entered = sx > 0 ? 'west' : 'east'
    } else if (ny <= nz) {
      t = ny
      ny += dy
      y += sy
      entered = sy > 0 ? 'bottom' : 'top'
    } else {
      t = nz
      nz += dz
      z += sz
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

function rayMaker ({ panorama, yaw = 0, pitch = 0, fov = 90, width, height }) {
  if (panorama) {
    // 360 degrees around, north in the middle, square pixels at the horizon
    const vfov = 2 * Math.PI * height / width
    return (px, py) => directionFor((0.5 - (px + 0.5) / width) * 2 * Math.PI, (0.5 - (py + 0.5) / height) * vfov)
  }
  const forward = directionFor(yaw, pitch)
  const right = directionFor(yaw - Math.PI / 2, 0)
  const up = cross(right, forward)
  const half = Math.tan(fov * Math.PI / 360)
  return (px, py) => {
    const sx = ((px + 0.5) / width * 2 - 1) * half
    const sy = (1 - (py + 0.5) / height * 2) * half * height / width
    const v = { x: forward.x + right.x * sx + up.x * sy, y: forward.y + right.y * sx + up.y * sy, z: forward.z + right.z * sx + up.z * sy }
    const len = Math.hypot(v.x, v.y, v.z)
    return { x: v.x / len, y: v.y / len, z: v.z / len }
  }
}

// ---------------------------------------------------------------- render
const FACE_SHADE = { top: 1, bottom: 0.5, north: 0.8, south: 0.8, east: 0.62, west: 0.62, cross: 0.95 }
const ENTITY_COLORS = {
  player: [235, 60, 235], sheep: [240, 240, 235], cow: [110, 75, 50], pig: [240, 160, 165], chicken: [250, 250, 250],
  item: [255, 225, 40], villager: [150, 100, 70], horse: [150, 110, 70], wolf: [215, 210, 210]
}
const HOSTILE = [225, 35, 35]
const hashColor = name => {
  let h = 0
  for (const c of name) h = (h * 31 + c.charCodeAt(0)) >>> 0
  return [90 + h % 130, 90 + (h >> 8) % 130, 90 + (h >> 16) % 130]
}
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

// Draw the world. `near` is the fraction of the picture closer than NEAR. `texture(blockName, face, props)` returns {width,height,rgba,tint?} or null; entities are
// {name, kind?, x, y, z, width, height} boxes. Returns {width,height,rgba,seen} where `seen` lists the entities
// that actually ended up on screen (not hidden behind blocks) with the pixel they are centred on.
export function render ({ grid, info, texture, eye, entities = [], timeOfDay, width, height, maxDist = 64, ...camera }) {
  const rayFor = rayMaker({ ...camera, width, height })
  const light = daylight(timeOfDay)
  const rgba = new Uint8Array(width * height * 4)
  const boxes = entities
    .filter(e => Math.hypot(e.x - eye.x, e.y - eye.y, e.z - eye.z) <= maxDist)
    .map(e => ({ e, box: [e.x - e.width / 2, e.y, e.z - e.width / 2, e.x + e.width / 2, e.y + e.height, e.z + e.width / 2], pixels: 0, sumX: 0, sumY: 0 }))
  let nearPixels = 0
  // one block description serves every cell of that state, so its pictures are looked up once a frame, not once a hit
  const pictures = new Map()
  const picture = (block, face) => {
    const key = face === 'top' || face === 'bottom' || face === 'cross' ? face : 'side'
    if (!pictures.has(block)) pictures.set(block, {})
    const faces = pictures.get(block)
    if (!(key in faces)) faces[key] = texture(block.name, key, block.props)
    return faces[key]
  }
  const accept = hit => {
    const image = picture(hit.block, hit.face)
    hit.image = image
    return !image || image.rgba[texel(image, hit.u, hit.v) + 3] >= 128
  }
  for (let py = 0; py < height; py++) {
    for (let px = 0; px < width; px++) {
      const d = rayFor(px, py)
      const up = clamp01(d.y)
      const sky = [mix(200, 105, up) * light, mix(222, 160, up) * light, mix(255, 250, up) * light]
      const hit = castRay(grid, info, eye, d, maxDist, accept)
      let nearest = null
      for (const b of boxes) {
        const h = rayBox(eye, d, b.box)
        if (h && h.t < (hit?.t ?? maxDist) && h.t < (nearest?.t ?? Infinity)) nearest = { ...h, b }
      }
      let color = sky
      if (nearest) {
        nearest.b.pixels++
        nearest.b.sumX += px
        nearest.b.sumY += py
        const base = ENTITY_COLORS[nearest.b.e.name] ?? ENTITY_COLORS[nearest.b.e.kind] ?? (nearest.b.e.kind === 'hostile' ? HOSTILE : hashColor(nearest.b.e.name))
        color = base.map(c => c * FACE_SHADE[nearest.face] * Math.max(light, 0.6))
      } else if (hit) {
        if (hit.t < NEAR) nearPixels++
        const at = hit.image ? texel(hit.image, hit.u, hit.v) : 0
        const base = hit.image ? [0, 1, 2].map(i => hit.image.rgba[at + i] * (hit.image.tint?.[i] ?? 255) / 255) : hashColor(hit.block.name ?? String(hit.id))
        const fog = (hit.t / maxDist) ** 2
        color = base.map((c, i) => mix(c * FACE_SHADE[hit.face] * light, sky[i], fog))
      }
      const at = (py * width + px) * 4
      rgba[at] = color[0]
      rgba[at + 1] = color[1]
      rgba[at + 2] = color[2]
      rgba[at + 3] = 255
    }
  }
  const seen = boxes.filter(b => b.pixels > 0).map(({ e, pixels, sumX, sumY }) => ({
    name: e.label ?? e.name,
    px: Math.round(sumX / pixels),
    py: Math.round(sumY / pixels),
    dist: Math.round(Math.hypot(e.x - eye.x, e.y + e.height / 2 - eye.y, e.z - eye.z))
  }))
  return { width, height, rgba, seen, near: nearPixels / (width * height) }
}
