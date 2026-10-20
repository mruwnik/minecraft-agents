// Why JavaScript: drives sight-kernel.mjs over a real raw world (sight-bench-world.mjs) without the cljs perception; used by the kernel bench.
import { MARKS_LENGTH } from './sight-kernel.mjs'
import { makeRaw, groundAt } from './sight-bench-world.mjs'

export const basis = (yaw, pitch) => {
  const cp = Math.cos(pitch); const sp = Math.sin(pitch); const cy = Math.cos(yaw); const sy = Math.sin(yaw)
  const fx = -sy * cp; const fy = sp; const fz = -cy * cp
  return [fx, fy, fz, cy, 0, -sy, sy * fy, -sy * fx - cy * fz, cy * fy]
}

// the grid rays of a full pass (ray-deg 1, fov 70, aspect 16/9) in tiles of 16 in golden-stride order
export function gridDirs (yaw, pitch, { rayDeg = 1, fov = 70, aspect = 16 / 9, jx = 0.5, jy = 0.5 } = {}) {
  const hy = Math.tan(fov * Math.PI / 360); const hx = hy * aspect
  const sp = Math.tan(rayDeg * Math.PI / 180)
  const cols = Math.ceil(2 * hx / sp); const rows = Math.ceil(2 * hy / sp)
  const total = cols * rows; const tiles = Math.ceil(total / 16)
  const gcd = (a, b) => b === 0 ? a : gcd(b, a % b)
  let stride = Math.max(1, Math.round(0.618034 * tiles)); while (gcd(stride, tiles) !== 1) stride++
  const b = basis(yaw, pitch)
  const out = new Float64Array(3 * tiles * 16).fill(NaN)
  for (let q = 0; q < tiles * 16; q++) {
    const k = 16 * ((Math.floor(q / 16) * stride) % tiles) + (q % 16)
    if (k >= total) continue
    const a = -hx + (2 * hx / cols) * ((k % cols) + jx)
    const v = -hy + (2 * hy / rows) * (Math.floor(k / cols) + jy)
    const dx = b[0] + a * b[3] + v * b[6]; const dy = b[1] + v * b[7]; const dz = b[2] + a * b[5] + v * b[8]
    const len = Math.hypot(dx, dy, dz)
    out[3 * q] = dx / len; out[3 * q + 1] = dy / len; out[3 * q + 2] = dz / len
  }
  return out
}

// A kernel context over a raw world: marks per section for one stamp, records into an array of packed [x y z id].
export function makeRig (raw, { radius = 48, near = 4 } = {}) {
  const sight = raw.sightTable()
  const visible = new Uint8Array(256)
  for (let l = 0; l < 256; l++) visible[l] = Math.max(l >> 4, l & 15) >= 3 ? 1 : 0
  const marks = new Map()
  const records = []
  const ctx = {
    raw, sections: raw, sight, visible, radius, near,
    marksOf: (cx, cy, cz) => {
      const k = (cx * 65536 + cz) * 256 + cy
      let m = marks.get(k)
      if (!m) marks.set(k, m = new Uint8Array(MARKS_LENGTH))
      return m
    },
    record: (x, y, z, id) => records.push(x, y, z, id),
    markKind: id => (sight[id] !== 1 ? 1 : 2),
    thin: () => true
  }
  return {
    ctx, records,
    fresh () { marks.clear(); records.length = 0 }
  }
}

export { makeRaw, groundAt }
