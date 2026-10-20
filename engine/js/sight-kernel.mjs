// Why JavaScript: the sight ray loop is the hottest code of the body; one function over typed arrays, no per-cell calls or allocation.
// Casts sight rays from an eye through the raw world's section arrays and the perception's slice marks. The rules are
// those documented in engine/src/engine/perception/rays.cljs (cast!): a cell an earlier ray of the slice recorded is
// not read again (its mark says whether it stops every line, never, or only some), a blocking cell is lit by the
// brighter of itself and the cell the ray came from, a cell too dark to make out is not recorded and the ray goes on.

const UNSET = 0xFFFE // a cell of a lazy section not read yet
const UNLOADED = 0xFFFF
const LIGHT_UNSET = 255 // block-light nibbles are 0..15

// Sections filled cell by cell from raw.stateAt / raw.lightAt as the rays reach them, for raw worlds that have no
// section arrays (the test fakes). Valid while the world does not change; the perception drops it with its marks.
export function lazySections () {
  const states = new Map()
  const lights = new Map()
  return {
    sectionRange: () => ({ minY: -1024, sections: 128 }),
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

// ctx: {raw (stateAt, lightAt), sections (sectionRange, sectionStates, sectionLight: the raw world's own or lazySections), sight, visible, base, radius, near, marksOf(cx, cy, cz), record(x, y, z, id), markKind(id), thin(id, x, y, z, ox, oy, oz, ex, ey, ez)}
// dirs: Float64Array of unit vectors (3 per ray), NaN x = skip the ray. Returns the number of cells entered.
export function castRays (ctx, ox, oy, oz, dirs, count) {
  const { raw, sections: src, sight, visible, base, radius, near, marksOf, record } = ctx
  const kinds = ctx.kinds ?? (ctx.kinds = new Uint8Array(sight.length))
  const { minY, sections } = src.sectionRange()
  const sectionStates = src.sectionStates
  const sectionLight = src.sectionLight
  const s0 = minY >> 4 // world section index of section 0
  let total = 0

  // the section of the cell the ray is in: its marks and state ids (null: unloaded), its light when first needed
  let cx = 0; let cy = 0; let cz = 0
  let marks = null
  let ids = null
  let lightSec

  const kindOf = id => kinds[id] || (kinds[id] = ctx.markKind(id))
  const enter = () => {
    marks = marksOf(cx, cy, cz)
    const s = cy - s0
    ids = s < 0 || s >= sections ? null : sectionStates(cx, s, cz)
    lightSec = undefined
  }
  const lightAt = (x, y, z) => {
    const px = x >> 4; const py = y >> 4; const pz = z >> 4
    let sec
    if (px === cx && py === cy && pz === cz) sec = lightSec === undefined ? (lightSec = sectionLight(cx, cy - s0, cz)) : lightSec
    else sec = sectionLight(px, py - s0, pz)
    if (sec === null) return 0
    const i = (((y & 15) << 8) | ((z & 15) << 4) | (x & 15))
    if (sec.block[i] === LIGHT_UNSET) {
      const v = raw.lightAt(x, y, z)
      sec.sky[i] = v >> 4
      sec.block[i] = v & 15
    }
    return (sec.sky[i] << 4) | sec.block[i]
  }
  const stateAt = (x, y, z, i) => {
    if (ids === null) return -1
    let v = ids[i]
    if (v >= UNSET) {
      if (v === UNSET) { const r = raw.stateAt(x, y, z); ids[i] = v = r < 0 ? UNLOADED : r }
      if (v === UNLOADED) return -1
    }
    return v
  }

  const ecx = Math.floor(ox) >> 4; const ecy = Math.floor(oy) >> 4; const ecz = Math.floor(oz) >> 4
  cx = ecx; cy = ecy; cz = ecz
  enter()
  const eyeMarks = marks; const eyeIds = ids

  for (let r = 0; r < count; r++) {
    const dx = dirs[3 * r]; const dy = dirs[3 * r + 1]; const dz = dirs[3 * r + 2]
    if (dx !== dx) continue
    const ex = ox + dx * radius; const ey = oy + dy * radius; const ez = oz + dz * radius
    const sx = dx > 0 ? 1 : -1; const sy = dy > 0 ? 1 : -1; const sz = dz > 0 ? 1 : -1
    const tdx = dx === 0 ? Infinity : Math.abs(1 / dx)
    const tdy = dy === 0 ? Infinity : Math.abs(1 / dy)
    const tdz = dz === 0 ? Infinity : Math.abs(1 / dz)
    let x = Math.floor(ox); let y = Math.floor(oy); let z = Math.floor(oz)
    let tmx = dx === 0 ? Infinity : tdx * (dx > 0 ? (x + 1 - ox) : (ox - x))
    let tmy = dy === 0 ? Infinity : tdy * (dy > 0 ? (y + 1 - oy) : (oy - y))
    let tmz = dz === 0 ? Infinity : tdz * (dz > 0 ? (z + 1 - oz) : (oz - z))
    let n = 1
    let prev = -1 // light of the cell the ray came from; -1: not read yet
    if (cx !== ecx || cy !== ecy || cz !== ecz) { cx = ecx; cy = ecy; cz = ecz; marks = eyeMarks; ids = eyeIds; lightSec = undefined }

    // the cell the eye is in
    let i = ((y & 15) << 8) | ((z & 15) << 4) | (x & 15)
    const kind0 = marks[i] - base
    if (kind0 !== 1) {
      const id0 = kind0 === 2 ? 0 : stateAt(x, y, z, i)
      if (kind0 < 1 || kind0 > 3) {
        if (id0 >= 0) {
          record(x, y, z, id0)
          marks[i] = base + kindOf(id0)
        }
      }
      if (kind0 === 2 || id0 < 0) { total += n; continue }
      const k = kindOf(id0)
      if (k === 2 || (k === 3 && ctx.thin(id0, x, y, z, ox, oy, oz, ex, ey, ez))) { total += n; continue }
    }

    for (;;) {
      const t = tmx < tmy ? (tmx < tmz ? tmx : tmz) : (tmy < tmz ? tmy : tmz)
      if (t > radius) break
      const px = x; const py = y; const pz = z
      if (t === tmx) {
        x += sx; tmx += tdx
        if ((x >> 4) !== cx) { cx = x >> 4; enter() }
      } else if (t === tmy) {
        y += sy; tmy += tdy
        if ((y >> 4) !== cy) { cy = y >> 4; enter() }
      } else {
        z += sz; tmz += tdz
        if ((z >> 4) !== cz) { cz = z >> 4; enter() }
      }
      i = ((y & 15) << 8) | ((z & 15) << 4) | (x & 15)
      const kind = marks[i] - base
      if (kind === 1) { n++; prev = -1; continue }
      if (kind === 2) { n++; break }
      const id = stateAt(x, y, z, i)
      if (id < 0) break
      n++
      const k = kindOf(id)
      const blocks = k === 2 || (k === 3 && ctx.thin(id, x, y, z, ox, oy, oz, ex, ey, ez))
      if (kind === 3) {
        if (blocks) break
        prev = -1
        continue
      }
      const light = lightAt(x, y, z)
      let shown = light
      if (blocks) {
        const p = prev < 0 ? lightAt(px, py, pz) : prev
        const a = light & 0xF0; const b = p & 0xF0; const c = light & 15; const d = p & 15
        shown = ((a > b ? a : b) | (c > d ? c : d))
      }
      if (visible[shown] === 1 || t <= near) {
        record(x, y, z, id)
        marks[i] = base + k
      }
      if (blocks) break
      prev = light
    }
    total += n
  }
  return total
}
