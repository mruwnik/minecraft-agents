// Why JavaScript: the sight ray loop is the hottest code of the body; one function over typed arrays, no per-cell calls or allocation.
// Casts sight rays from an eye through the raw world's section arrays and the perception's slice marks. The rules are
// those documented in engine/src/engine/perception/rays.cljs (cast!): a cell an earlier ray of the slice recorded is
// not read again (its mark says whether it stops every line, never, or only some), a blocking cell is lit by the
// brighter of itself and the cell the ray came from, a cell too dark to make out is not recorded and the ray goes on.

const UNSET = 0xFFFE // a cell of a lazy section not read yet
const UNLOADED = 0xFFFF
const LIGHT_UNSET = 255 // block-light nibbles are 0..15
// A marks array is a Uint8Array of MARKS_LENGTH for the current mark stamp (marksOf hands out zeroed ones for a new
// stamp): the mark kind of each cell of the section, 0 while not recorded, else markKind (or DARK).
export const MARKS_LENGTH = 4096
const DARK = 4 // a clear cell too dark to make out: recorded only within near of the eye, so it is read again only there

// Sections filled cell by cell from raw.stateAt / raw.lightAt as the rays reach them, for raw worlds that have no
// section arrays (the test fakes). Valid while the world does not change; the perception drops it with its marks.
const LAZY_RANGE = { minY: -1024, sections: 128 }
export function lazySections () {
  const states = new Map()
  const lights = new Map()
  return {
    sectionRange: () => LAZY_RANGE,
    sectionStates: (cx, s, cz) => {
      const key = (cx * 65536 + cz) * 256 + s
      let a = states.get(key)
      if (!a) states.set(key, a = new Uint16Array(4096).fill(UNSET))
      return a
    },
    sectionLight: (cx, s, cz) => {
      const key = (cx * 65536 + cz) * 256 + s
      let a = lights.get(key)
      if (!a) lights.set(key, a = { sky: new Uint8Array(4096), block: new Uint8Array(4096).fill(LIGHT_UNSET) })
      return a
    }
  }
}

// The three section caches of a call, reused across calls (castRays is synchronous, so none runs inside another):
// cleared to undefined, the "not fetched yet" value, instead of allocated per call.
const caches = [[], [], []]
function scratch (k, size) {
  const a = caches[k]
  if (a.length < size) a.length = size
  return a.fill(undefined, 0, size)
}

// ctx: {raw (stateAt, lightAt), sections (sectionRange, sectionStates, sectionLight: the raw world's own or lazySections), sight, visible, radius, near, marksOf(cx, cy, cz), record(x, y, z, id), markKind(id), thin(id, x, y, z, ox, oy, oz, ex, ey, ez)}
// dirs: Float64Array of unit vectors (3 per ray), NaN x = skip the ray. Returns the number of cells entered.
export function castRays (ctx, ox, oy, oz, dirs, count) {
  const { raw, sections: src, sight, visible, radius, near, marksOf, record } = ctx
  const kinds = ctx.kinds ?? (ctx.kinds = new Uint8Array(sight.length))
  const { minY, sections } = src.sectionRange()
  const sectionStates = src.sectionStates
  const sectionLight = src.sectionLight
  const s0 = minY >> 4 // world section index of section 0
  let total = 0

  // The sections around the eye by offset, fetched on first entry: a ray crossing a section boundary finds its marks,
  // state ids and light by index, not by hashing. Plain locals for the section the ray is in: the hot loop reads them every step.
  const R = ((radius + 2) >> 4) + 2
  const W = 2 * R + 1
  const ecx = Math.floor(ox) >> 4; const ecy = Math.floor(oy) >> 4; const ecz = Math.floor(oz) >> 4
  const marksC = scratch(0, W * W * W)
  const idsC = scratch(1, W * W * W)
  const lightC = scratch(2, W * W * W)
  let cx = ecx; let cy = ecy; let cz = ecz
  let slot = (R * W + R) * W + R
  let marks = marksC[slot] = marksOf(cx, cy, cz)
  let ids = idsC[slot] = cy - s0 < 0 || cy - s0 >= sections ? null : sectionStates(cx, cy - s0, cz)
  let lightSec = lightC[slot]
  const eyeSlot = slot

  for (let r = 0; r < count; r++) {
    const dx = dirs[3 * r]; const dy = dirs[3 * r + 1]; const dz = dirs[3 * r + 2]
    if (dx !== dx) continue
    const ex = ox + dx * radius; const ey = oy + dy * radius; const ez = oz + dz * radius
    const sx = dx > 0 ? 1 : -1; const sy = dy > 0 ? 1 : -1; const sz = dz > 0 ? 1 : -1
    const tdx = dx === 0 ? Infinity : Math.abs(1 / dx)
    const tdy = dy === 0 ? Infinity : Math.abs(1 / dy)
    const tdz = dz === 0 ? Infinity : Math.abs(1 / dz)
    let x = Math.floor(ox) | 0; let y = Math.floor(oy) | 0; let z = Math.floor(oz) | 0
    let tmx = dx === 0 ? Infinity : tdx * (dx > 0 ? (x + 1 - ox) : (ox - x))
    let tmy = dy === 0 ? Infinity : tdy * (dy > 0 ? (y + 1 - oy) : (oy - y))
    let tmz = dz === 0 ? Infinity : tdz * (dz > 0 ? (z + 1 - oz) : (oz - z))
    let n = 1
    let prev = -1 // light of the cell the ray came from; -1: not read yet
    if (slot !== eyeSlot) { cx = ecx; cy = ecy; cz = ecz; slot = eyeSlot; marks = marksC[slot]; ids = idsC[slot]; lightSec = lightC[slot] }

    // the cell the eye is in
    let i = ((y & 15) << 8) | ((z & 15) << 4) | (x & 15)
    const kind0 = marks[i]
    if (kind0 !== 1) {
      const id0 = kind0 === 2 ? 0 : stateIn(raw, ids, x, y, z, i)
      if (kind0 < 1 || kind0 > 3) {
        if (id0 >= 0) {
          record(x, y, z, id0)
          const k0 = kinds[id0] || (kinds[id0] = ctx.markKind(id0))
          marks[i] = k0
        }
      }
      if (kind0 === 2 || id0 < 0) { total += n; continue }
      const k = kinds[id0] || (kinds[id0] = ctx.markKind(id0))
      if (k === 2 || (k === 3 && ctx.thin(id0, x, y, z, ox, oy, oz, ex, ey, ez))) { total += n; continue }
    }

    // The walk keeps the cell's index in its section (i) beside its world cell: a step is an add on one axis, and the
    // section changes where that add runs off the section (a carry into the next field of i).
    const wrapx = sx > 0 ? 0 : 15; const wrapz = sz > 0 ? 0 : 15
    const stepx = sx << 4; const stepz = sz << 8
    for (;;) {
      const t = tmx < tmy ? (tmx < tmz ? tmx : tmz) : (tmy < tmz ? tmy : tmz)
      if (t > radius) break
      const px = x; const py = y; const pz = z
      let moved = false
      if (t === tmx) {
        x += sx; tmx += tdx; i += sx
        if ((i & 15) === wrapx) { i -= stepx; cx += sx; moved = true }
      } else if (t === tmy) {
        y += sy; tmy += tdy; i += sy << 8
        if (i < 0 || i > 4095) { i -= sy << 12; cy += sy; moved = true }
      } else {
        z += sz; tmz += tdz; i += sz << 4
        if (((i >> 4) & 15) === wrapz) { i -= stepz; cz += sz; moved = true }
      }
      if (moved) {
        lightC[slot] = lightSec
        slot = (((cx - ecx + R) * W) + (cz - ecz + R)) * W + (cy - ecy + R)
        marks = marksC[slot]
        if (marks === undefined) {
          marks = marksC[slot] = marksOf(cx, cy, cz)
          ids = idsC[slot] = cy - s0 < 0 || cy - s0 >= sections ? null : sectionStates(cx, cy - s0, cz)
        } else ids = idsC[slot]
        lightSec = lightC[slot]
      }
      const kind = marks[i]
      if (kind === 1 || (kind === DARK && t > near)) { n++; prev = -1; continue }
      if (kind === 2) { n++; break }
      const id = stateIn(raw, ids, x, y, z, i)
      if (id < 0) break
      n++
      const k = kinds[id] || (kinds[id] = ctx.markKind(id))
      const blocks = k === 2 || (k === 3 && ctx.thin(id, x, y, z, ox, oy, oz, ex, ey, ez))
      if (kind === 3) {
        if (blocks) break
        prev = -1
        continue
      }
      if (lightSec === undefined) lightSec = sectionLight(cx, cy - s0, cz)
      const light = lightIn(raw, lightSec, x, y, z, i)
      let shown = light
      if (blocks) {
        let p = prev
        if (p < 0) {
          const pi = ((py & 15) << 8) | ((pz & 15) << 4) | (px & 15)
          p = (px >> 4) === cx && (py >> 4) === cy && (pz >> 4) === cz
            ? lightIn(raw, lightSec, px, py, pz, pi)
            : lightIn(raw, sectionLight(px >> 4, (py >> 4) - s0, pz >> 4), px, py, pz, pi)
        }
        const a = light & 0xF0; const b = p & 0xF0; const c = light & 15; const d = p & 15
        shown = ((a > b ? a : b) | (c > d ? c : d))
      }
      if (visible[shown] === 1 || t <= near) {
        record(x, y, z, id)
        marks[i] = k
      } else if (k === 1) marks[i] = DARK
      if (blocks) break
      prev = light
    }
    total += n
  }
  return total
}

// the light (sky << 4 | block) of the cell at index i of its section's light arrays, read from raw on first use; 0 where unloaded
function lightIn (raw, sec, x, y, z, i) {
  if (sec === null) return 0
  if (sec.block[i] === LIGHT_UNSET) {
    const v = raw.lightAt(x, y, z)
    sec.sky[i] = v >> 4
    sec.block[i] = v & 15
  }
  return (sec.sky[i] << 4) | sec.block[i]
}

// the state id of the cell at index i of the section ids (read from raw on first use), -1 where unloaded
function stateIn (raw, ids, x, y, z, i) {
  if (ids === null) return -1
  let v = ids[i]
  if (v >= UNSET) {
    if (v === UNSET) { const r = raw.stateAt(x, y, z); ids[i] = v = r < 0 ? UNLOADED : r }
    if (v === UNLOADED) return -1
  }
  return v
}

// The unit directions of rays [from, to) of a pass into out (3 per ray; NaN x: a ray outside the cone or past the grid).
// A pass casts its aimed rows first, then its grid in tiles of tileRays taken in golden-stride order (see rays.cljs).
// p: {b (the view basis: 9 numbers), hx, hy, cols, rows, jx, jy, ground (flat [d l dy ...]: forward, sideways, height above the eye), tiles, stride, tileRays, fx, fz,
// rx, rz}.
export function sliceDirs (out, from, to, p) {
  const { b, hx, hy, cols, rows, jx, jy, ground, tiles, stride, tileRays, fx, fz, rx, rz } = p
  const nGrid = cols * rows
  const nGround = ground.length / 3
  const du = (2 * hx) / cols; const dv = (2 * hy) / rows
  for (let q = from; q < to; q++) {
    const j = q - from
    let dx; let dy; let dz
    if (q >= nGround) {
      const g = q - nGround
      const k = tileRays * ((Math.floor(g / tileRays) * stride) % tiles) + (g % tileRays)
      if (k >= nGrid) { out[3 * j] = NaN; continue }
      const a = -hx + du * ((k % cols) + jx)
      const v = -hy + dv * (Math.floor(k / cols) + jy)
      dx = b[0] + a * b[3] + v * b[6]
      dy = b[1] + v * b[7]
      dz = b[2] + a * b[5] + v * b[8]
    } else {
      const d = ground[3 * q]; const l = ground[3 * q + 1]
      dx = d * fx + l * rx; dy = ground[3 * q + 2]; dz = d * fz + l * rz
      const f = dx * b[0] + dy * b[1] + dz * b[2]
      if (!(f > 0 && Math.abs(dx * b[3] + dz * b[5]) <= hx * f && Math.abs(dx * b[6] + dy * b[7] + dz * b[8]) <= hy * f)) { out[3 * j] = NaN; continue }
    }
    const len = Math.hypot(dx, dy, dz)
    out[3 * j] = dx / len; out[3 * j + 1] = dy / len; out[3 * j + 2] = dz / len
  }
}

const FACE_INSET = 0.15

// The light of the last cell before the point's own cell on the straight line from the eye (ox oy oz) to the point
// (tx ty tz), or -1 when a cell on the way stops the line (ctx.blocks), is unloaded, or the walk overruns its budget.
// Reads state ids from the section arrays when the raw world has them, else cell by cell through raw.stateAt.
function walkLine (ctx, ox, oy, oz, tx, ty, tz) {
  const { raw, sight, src } = ctx
  const vx = tx - ox; const vy = ty - oy; const vz = tz - oz
  const d = Math.hypot(vx, vy, vz)
  const x1 = Math.floor(tx); const y1 = Math.floor(ty); const z1 = Math.floor(tz)
  const x0 = Math.floor(ox); const y0 = Math.floor(oy); const z0 = Math.floor(oz)
  const dx = d === 0 ? 0 : vx / d; const dy = d === 0 ? 0 : vy / d; const dz = d === 0 ? 0 : vz / d
  const sx = dx > 0 ? 1 : -1; const sy = dy > 0 ? 1 : -1; const sz = dz > 0 ? 1 : -1
  const tdx = dx === 0 ? Infinity : Math.abs(1 / dx)
  const tdy = dy === 0 ? Infinity : Math.abs(1 / dy)
  const tdz = dz === 0 ? Infinity : Math.abs(1 / dz)
  let cx = x0; let cy = y0; let cz = z0
  let tmx = dx === 0 ? Infinity : tdx * (dx > 0 ? (x0 + 1 - ox) : (ox - x0))
  let tmy = dy === 0 ? Infinity : tdy * (dy > 0 ? (y0 + 1 - oy) : (oy - y0))
  let tmz = dz === 0 ? Infinity : tdz * (dz > 0 ? (z0 + 1 - oz) : (oz - z0))
  let budget = 3 + Math.abs(x1 - x0) + Math.abs(y1 - y0) + Math.abs(z1 - z0)
  const { minY, sections } = src ? src.sectionRange() : { minY: 0, sections: 0 }
  const s0 = minY >> 4
  let secKey = -1
  let ids = null
  for (;;) {
    const t = Math.min(tmx, tmy, tmz)
    if ((cx === x1 && cy === y1 && cz === z1) || t > d) return raw.lightAt(cx, cy, cz) || 0
    if (budget <= 0) return -1
    const px = cx; const py = cy; const pz = cz
    if (t === tmx) { cx += sx; tmx += tdx } else if (t === tmy) { cy += sy; tmy += tdy } else { cz += sz; tmz += tdz }
    if (cx === x1 && cy === y1 && cz === z1) return raw.lightAt(px, py, pz) || 0
    let id
    if (src) {
      const s = (cy >> 4) - s0
      if (s < 0 || s >= sections) return -1
      const key = ((cx >> 4) * 65536 + (cz >> 4)) * 256 + s
      if (key !== secKey) { ids = src.sectionStates(cx >> 4, s, cz >> 4); secKey = key }
      if (ids === null) return -1
      id = ids[((cy & 15) << 8) | ((cz & 15) << 4) | (cx & 15)]
      if (id === UNLOADED) return -1
    } else {
      id = raw.stateAt(cx, cy, cz)
      if (id < 0) return -1
    }
    if (sight[id] === 1 && ctx.blocks(id, cx, cy, cz, ox, oy, oz, tx, ty, tz)) return -1
    budget--
  }
}

// The light before the cell (x y z) as seen from the eye: along the line to its centre, else to inset points on the
// faces towards the eye (four per face), first that is clear. -1 when none is. ctx: {raw, sight, src (the raw
// world when it has section arrays, else null), blocks(id, x, y, z, ox, oy, oz, tx, ty, tz)}.
export function cellLight (ctx, ox, oy, oz, x, y, z) {
  const centre = walkLine(ctx, ox, oy, oz, x + 0.5, y + 0.5, z + 0.5)
  if (centre >= 0) return centre
  const lo = FACE_INSET; const hi = 1 - FACE_INSET
  const side = (o, c) => o < c ? -1 : o > c + 1 ? 1 : 0
  const sx = side(ox, x); const sy = side(oy, y); const sz = side(oz, z)
  if (sx !== 0) {
    const fx = x + (sx < 0 ? lo : hi)
    for (const b of [lo, hi]) for (const c of [lo, hi]) {
      const r = walkLine(ctx, ox, oy, oz, fx, y + b, z + c)
      if (r >= 0) return r
    }
  }
  if (sy !== 0) {
    const fy = y + (sy < 0 ? lo : hi)
    for (const a of [lo, hi]) for (const c of [lo, hi]) {
      const r = walkLine(ctx, ox, oy, oz, x + a, fy, z + c)
      if (r >= 0) return r
    }
  }
  if (sz !== 0) {
    const fz = z + (sz < 0 ? lo : hi)
    for (const a of [lo, hi]) for (const b of [lo, hi]) {
      const r = walkLine(ctx, ox, oy, oz, x + a, y + b, fz)
      if (r >= 0) return r
    }
  }
  return -1
}
