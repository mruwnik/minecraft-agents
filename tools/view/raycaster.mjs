// Why JavaScript: graphics/performance; software raycaster split so worker threads can draw terrain bands.
// The renderer's picture split in two so terrain can be drawn by worker threads in bands and entities after, on the
// main thread: renderBand() is render()'s per-pixel terrain work for a range of rows, drawEntities() its mob work on a
// picture already drawn. Split out of renderer.mjs's render() because that cannot draw a band; keep the two in step.
// Every floating-point expression below is the original's, in the original's order: the output is byte-identical.
import { placeLabels } from './web/mobs.mjs'
import { mobFor, mobPaint } from './mob-draw.mjs'
import { mobImage } from './mob-textures.mjs'
import { drawLabels } from './labels.mjs'
import { castRay, colorOf, directionFor, visibleMobs, hitLight, entityLight, lightTable, lightReport } from './renderer.mjs'

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
const boxHit = { t: 0, face: '' }
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
export function renderBand ({ grid, info, texture, eye, timeOfDay, rain = 0, width, height, maxDist = 64, rgba, depth, rowStart, rowEnd, ...camera }) {
  const cam = cameraFor({ ...camera, width, height })
  const light = daylight(timeOfDay)
  const table = lightTable(timeOfDay, rain)
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
export function drawEntities ({ grid, eye, entities = [], timeOfDay, rain = 0, width, height, maxDist = 64, rgba, depth, mobPictures = mobImage, ...camera }) {
  const cam = cameraFor({ ...camera, width, height })
  const table = lightTable(timeOfDay, rain)
  const mobs = entities
    .filter(e => Math.hypot(e.x - eye.x, e.y - eye.y, e.z - eye.z) <= maxDist)
    .map(e => mobFor(e, eye))
    .map(m => ({ ...m, rect: screenRect(cam.project, eye, m.box, width, height), light: entityLight(grid, m.e) }))
    .filter(m => m.rect)
  const d = { x: 0, y: 0, z: 0 }
  const local = { x: 0, y: 0, z: 0 }
  const nearestLocal = { x: 0, y: 0, z: 0 }
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
      if (!nearest) continue
      depth[py * width + px] = nearestT // a mob hides the labels behind it too; drawLabels is the depth's only reader after this
      nearest.pixels++
      nearest.sumX += px
      nearest.sumY += py
      if (px < nearest.x1) nearest.x1 = px
      if (px > nearest.x2) nearest.x2 = px
      if (py < nearest.y1) nearest.y1 = py
      if (py > nearest.y2) nearest.y2 = py
      // 'south' is the mob's own front: the ray came in through its +z face
      const base = mobPaint(nearest, nearestPart, nearestFace, nearestT, nearestLocal, mobPictures)
      const at = (py * width + px) * 4
      const lit = table[nearest.light]
      rgba[at] = base[0] * FACE_SHADE[nearestFace] * lit[0]
      rgba[at + 1] = base[1] * FACE_SHADE[nearestFace] * lit[1]
      rgba[at + 2] = base[2] * FACE_SHADE[nearestFace] * lit[2]
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
  return seen
}
