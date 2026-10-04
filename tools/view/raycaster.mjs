// The renderer's picture split in two so terrain can be drawn by worker threads in bands and entities after, on the
// main thread: renderBand() is render()'s per-pixel terrain work for a range of rows, drawEntities() its mob work on a
// picture already drawn. Copied from src/vision/renderer.mjs because its render() cannot draw a band; keep it in step.
// Every floating-point expression below is the original's, in the original's order: the output is byte-identical.
import { castRay, colorOf, directionFor } from './renderer.mjs'

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

const clamp01 = v => Math.min(1, Math.max(0, v))

const FACE_SHADE = { top: 1, bottom: 0.5, north: 0.8, south: 0.8, east: 0.62, west: 0.62, cross: 0.95 }
const HOSTILE = [225, 35, 35]
const boxHit = { t: 0, face: '' }
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
export const NEAR = 2

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

// ---------------------------------------------------------------- bands
// render()'s `picture`/`accept` closures: a block's pictures are looked up once per block description, not once a hit
const acceptFor = texture => {
  const picture = (block, face) => {
    const key = face === 'top' || face === 'bottom' || face === 'cross' ? face : 'side'
    const faces = block.pictures ??= {}
    if (!(key in faces)) {
      const color = colorOf(block.name)
      faces[key] = color ? { width: 1, height: 1, rgba: Uint8Array.from([...color, 255]) } : texture(block.name, key, block.props)
    }
    return faces[key]
  }
  return hit => {
    const image = picture(hit.block, hit.face)
    hit.image = image
    return !image || image.rgba[texel(image, hit.u, hit.v) + 3] >= 128
  }
}

// Terrain (or sky) for rows rowStart <= py < rowEnd into rgba and depth; returns the band's pixels with terrain closer than NEAR.
export function renderBand ({ grid, info, texture, eye, timeOfDay, width, height, maxDist = 64, rgba, depth, rowStart, rowEnd, ...camera }) {
  const cam = cameraFor({ ...camera, width, height })
  const light = daylight(timeOfDay)
  const accept = acceptFor(texture)
  const d = { x: 0, y: 0, z: 0 }
  let nearPixels = 0
  for (let py = rowStart; py < rowEnd; py++) {
    for (let px = 0; px < width; px++) {
      cam.ray(px, py, d)
      const up = clamp01(d.y)
      const skyR = mix(200, 105, up) * light
      const skyG = mix(222, 160, up) * light
      const skyB = mix(255, 250, up) * light
      const hit = castRay(grid, info, eye, d, maxDist, accept)
      let r = skyR
      let g = skyG
      let b = skyB
      if (hit) {
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
      const p = py * width + px
      const at = p * 4
      rgba[at] = r
      rgba[at + 1] = g
      rgba[at + 2] = b
      rgba[at + 3] = 255
      depth[p] = hit ? hit.t : maxDist
    }
  }
  return nearPixels
}

// render()'s entity work on a picture renderBand has drawn: the mobs nearer than the terrain (depth) overwrite its pixels.
export function drawEntities ({ eye, entities = [], timeOfDay, width, height, maxDist = 64, rgba, depth, ...camera }) {
  const cam = cameraFor({ ...camera, width, height })
  const light = daylight(timeOfDay)
  const mobs = entities
    .filter(e => Math.hypot(e.x - eye.x, e.y - eye.y, e.z - eye.z) <= maxDist)
    .map(e => mobFor(e, eye))
    .map(m => ({ ...m, rect: screenRect(cam.project, eye, m.box, width, height) }))
    .filter(m => m.rect)
  const d = { x: 0, y: 0, z: 0 }
  const local = { x: 0, y: 0, z: 0 }
  const y1 = mobs.reduce((lo, m) => Math.min(lo, m.rect[1]), height)
  const y2 = mobs.reduce((hi, m) => Math.max(hi, m.rect[3]), -1)
  for (let py = y1; py <= y2; py++) {
    const rowMobs = mobs.filter(m => py >= m.rect[1] && py <= m.rect[3])
    for (let px = 0; px < width; px++) {
      cam.ray(px, py, d)
      const limit = depth[py * width + px]
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
      if (!nearest) continue
      nearest.pixels++
      nearest.sumX += px
      nearest.sumY += py
      if (px < nearest.x1) nearest.x1 = px
      if (px > nearest.x2) nearest.x2 = px
      if (py < nearest.y1) nearest.y1 = py
      if (py > nearest.y2) nearest.y2 = py
      // 'south' is the mob's own front: the ray came in through its +z face
      const base = nearest.palette[nearestPaint === 1 && nearestFace === 'south' ? 3 : nearestPaint]
      const at = (py * width + px) * 4
      rgba[at] = base[0] * FACE_SHADE[nearestFace] * Math.max(light, 0.6)
      rgba[at + 1] = base[1] * FACE_SHADE[nearestFace] * Math.max(light, 0.6)
      rgba[at + 2] = base[2] * FACE_SHADE[nearestFace] * Math.max(light, 0.6)
    }
  }
  return mobs.filter(m => m.pixels > 0).map(({ e, pixels, sumX, sumY, x1, y1, x2, y2 }) => ({
    name: e.label ?? e.name,
    kind: e.kind,
    px: Math.round(sumX / pixels),
    py: Math.round(sumY / pixels),
    dist: Math.round(Math.hypot(e.x - eye.x, e.y + e.height / 2 - eye.y, e.z - eye.z)),
    box: [x1, y1, x2, y2]
  }))
}
