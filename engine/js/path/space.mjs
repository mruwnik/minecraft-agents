// Free space for the body centre inside a cell: which of the 17x17 sub-positions (1/16 apart) let the 0.62-wide body
// stand clear of every collision box nearby. Boxes are world-coordinate, 6 floats each: x0 y0 z0 x1 y1 z1.
import { UNLOADED } from './snapshot.mjs'
import { blockOffset } from '../offsets.mjs'

export const HALF_WIDTH = 0.31
export const GRID = 17
// the server refuses a move that leaves the body's box exactly touching a face, so touching counts as overlap
const EPS = 1e-4
const SCRATCH = new Float32Array(6 * 512)

// Collision boxes of the 3x3 cells around (x, z), cell rows y-1 .. y+2, that overlap [lo, hi) vertically.
// Unloaded cells are a full box (unknown is solid). Bamboo / dripstone get their per-position offset.
export function boxesNear (snapshot, table, x, y, z, lo, hi) {
  const { boxStart, boxCount, boxes, offsetMax } = table
  let out = SCRATCH
  let n = 0
  const push = (x0, y0, z0, x1, y1, z1) => {
    if (y1 <= lo || y0 >= hi) return
    if (n + 6 > out.length) {
      const bigger = new Float32Array(out.length * 2)
      bigger.set(out)
      out = bigger
    }
    out.set([x0, y0, z0, x1, y1, z1], n)
    n += 6
  }
  for (let cy = y - 1; cy <= y + 2; cy++) {
    for (let cz = z - 1; cz <= z + 1; cz++) {
      for (let cx = x - 1; cx <= x + 1; cx++) {
        const id = snapshot.stateAt(cx, cy, cz)
        if (id === UNLOADED) {
          push(cx, cy, cz, cx + 1, cy + 1, cz + 1)
          continue
        }
        const count = boxCount[id]
        if (count === 0) continue
        const max = offsetMax[id]
        const { dx, dz } = max > 0 ? blockOffset(cx, cz, max) : { dx: 0, dz: 0 }
        for (let b = 0, at = boxStart[id] * 6; b < count; b++, at += 6) {
          push(cx + boxes[at] + dx, cy + boxes[at + 1], cz + boxes[at + 2] + dz, cx + boxes[at + 3] + dx, cy + boxes[at + 4], cz + boxes[at + 5] + dz)
        }
      }
    }
  }
  return out.slice(0, n)
}

// does the body centred at (px, pz) touch or overlap any of the boxes? (vertical overlap was filtered by boxesNear)
export function bodyHits (boxes, px, pz) {
  for (let i = 0; i < boxes.length; i += 6) {
    if (px + HALF_WIDTH > boxes[i] - EPS && px - HALF_WIDTH < boxes[i + 3] + EPS &&
        pz + HALF_WIDTH > boxes[i + 2] - EPS && pz - HALF_WIDTH < boxes[i + 5] + EPS) return true
  }
  return false
}

// 1 where the body centred at (x + i/16, z + j/16) hits no box; index j * 17 + i
export function freeMask (boxes, x, z) {
  const mask = new Uint8Array(GRID * GRID)
  for (let j = 0; j < GRID; j++) {
    for (let i = 0; i < GRID; i++) mask[j * GRID + i] = bodyHits(boxes, x + i / 16, z + j / 16) ? 0 : 1
  }
  return mask
}

// 4-connected components of the free positions: { size, px, pz }, the point nearest the cell centre in 1/16 (ties: lowest j, then i)
export function regions (mask) {
  const seen = new Uint8Array(mask.length)
  const stack = new Int32Array(mask.length)
  const found = []
  for (let start = 0; start < mask.length; start++) {
    if (!mask[start] || seen[start]) continue
    let top = 0
    let size = 0
    let best = start
    let bestD = Infinity
    stack[top++] = start
    seen[start] = 1
    while (top > 0) {
      const k = stack[--top]
      const i = k % GRID
      const j = (k - i) / GRID
      size++
      const d = (i - 8) * (i - 8) + (j - 8) * (j - 8)
      const bj = (best - best % GRID) / GRID
      if (d < bestD || d === bestD && (j < bj || j === bj && i < best % GRID)) { best = k; bestD = d }
      const visit = nk => { if (mask[nk] && !seen[nk]) { seen[nk] = 1; stack[top++] = nk } }
      if (i > 0) visit(k - 1)
      if (i < GRID - 1) visit(k + 1)
      if (j > 0) visit(k - GRID)
      if (j < GRID - 1) visit(k + GRID)
    }
    found.push({ size, px: best % GRID, pz: (best - best % GRID) / GRID })
  }
  return found
}

// can the body centre move in a straight line from -> to ({ x, z } world) at height [lo, hi)? Sampled every 1/16 block.
export function segmentFree (snapshot, table, from, to, lo, hi) {
  const y = Math.floor(lo)
  const cache = new Map()
  const near = (cx, cz) => {
    const key = cx * 65536 + cz // cells within +-32k of origin; a collision only costs a wrong cache hit far away
    let boxes = cache.get(key)
    if (boxes === undefined) cache.set(key, boxes = boxesNear(snapshot, table, cx, y, cz, lo, hi))
    return boxes
  }
  const n = Math.max(1, Math.ceil(Math.hypot(to.x - from.x, to.z - from.z) * 16))
  for (let s = 0; s <= n; s++) {
    const px = from.x + (to.x - from.x) * s / n
    const pz = from.z + (to.z - from.z) * s / n
    if (bodyHits(near(Math.floor(px), Math.floor(pz)), px, pz)) return false
  }
  return true
}
