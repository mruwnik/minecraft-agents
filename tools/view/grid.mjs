// The renderer's grid, filled from loaded chunk columns: a box `across` blocks out each way and `up` above and below
// the eye (centred, as a ray starts from inside it). Reads the palette sections directly, not getBlockStateId per
// cell, and skips sections that hold nothing.
import { makeGrid } from '../../src/vision/renderer.mjs'

const SECTION_VOLUME = 4096

// Writes every cell of chunk column (cx, cz) that lies inside the grid; returns the highest non-air y written.
export function fillColumn (grid, col, cx, cz) {
  const { origin, size, data } = grid
  const minY = col.minY
  const yLow = Math.max(minY, origin.y)
  const yHigh = Math.min(minY + col.worldHeight - 1, origin.y + size.y - 1)
  const x0 = Math.max(origin.x, cx * 16) - cx * 16
  const x1 = Math.min(origin.x + size.x - 1, cx * 16 + 15) - cx * 16
  const z0 = Math.max(origin.z, cz * 16) - cz * 16
  const z1 = Math.min(origin.z + size.z - 1, cz * 16 + 15) - cz * 16
  let top = -Infinity
  for (let s = (yLow - minY) >> 4; s <= (yHigh - minY) >> 4; s++) {
    const section = col.sections[s]
    if (!section || section.solidBlockCount === 0) continue
    const base = minY + s * 16
    for (let y = Math.max(yLow, base); y <= Math.min(yHigh, base + 15); y++) {
      const rowBase = (y - origin.y) * size.z
      for (let z = z0; z <= z1; z++) {
        const cellBase = (rowBase + (cz * 16 + z - origin.z)) * size.x + cx * 16 - origin.x
        const secBase = ((y - base) << 8) | (z << 4)
        for (let x = x0; x <= x1; x++) {
          const id = section.data.get(secBase | x)
          if (!id) continue
          data[cellBase + x] = id
          if (y > top) top = y
        }
      }
    }
  }
  return top
}

// Zeroes every cell of chunk column (cx, cz) inside the grid, at all heights.
export function clearColumn (grid, cx, cz) {
  const { origin, size, data } = grid
  const x0 = Math.max(origin.x, cx * 16) - origin.x
  const x1 = Math.min(origin.x + size.x - 1, cx * 16 + 15) - origin.x
  const z0 = Math.max(origin.z, cz * 16) - origin.z
  const z1 = Math.min(origin.z + size.z - 1, cz * 16 + 15) - origin.z
  if (x1 < x0 || z1 < z0) return
  for (let y = 0; y < size.y; y++) {
    for (let z = z0; z <= z1; z++) {
      const row = (y * size.z + z) * size.x
      data.fill(0, row + x0, row + x1 + 1)
    }
  }
}

// One chunk-aligned, full-height grid held across frames. The eye moving inside the same chunk range costs nothing;
// a replaced column object (the column cache returns the same object while a file is unchanged) is patched in place.
export function gridCache () {
  let held = null // { key, grid, seen: Map<"cx,cz", column|null> }
  let rebuilds = 0
  let patches = 0

  const get = ({ column, eye, across }) => {
    const [ex, ey, ez] = [Math.floor(eye.x), Math.floor(eye.y), Math.floor(eye.z)]
    const cxLo = (ex - across) >> 4
    const cxHi = (ex + across) >> 4
    const czLo = (ez - across) >> 4
    const czHi = (ez + across) >> 4
    const cols = new Map()
    for (let cx = cxLo; cx <= cxHi; cx++) {
      for (let cz = czLo; cz <= czHi; cz++) cols.set(`${cx},${cz}`, column(cx, cz) ?? null)
    }
    const first = [...cols.values()].find(Boolean)
    const y0 = first ? Math.min(first.minY, ey - 1) : ey - 48
    const y1 = first ? Math.max(first.minY + first.worldHeight - 1, ey + 1) : ey + 48
    const key = [cxLo, czLo, cxHi, czHi, y0, y1].join(',')

    if (!held || held.key !== key) {
      const grid = makeGrid({ x: cxLo * 16, y: y0, z: czLo * 16 }, { x: (cxHi - cxLo + 1) * 16, y: y1 - y0 + 1, z: (czHi - czLo + 1) * 16 })
      let top = -Infinity
      for (const [k, col] of cols) {
        if (!col) continue
        const [cx, cz] = k.split(',').map(Number)
        top = Math.max(top, fillColumn(grid, col, cx, cz))
      }
      held = { key, grid: Object.assign(grid, { top }), seen: cols }
      rebuilds++
      return held.grid
    }

    for (const [k, col] of cols) {
      if (held.seen.get(k) === col) continue
      const [cx, cz] = k.split(',').map(Number)
      clearColumn(held.grid, cx, cz)
      if (col) held.grid.top = Math.max(held.grid.top, fillColumn(held.grid, col, cx, cz))
      held.seen.set(k, col)
      patches++
    }
    return held.grid
  }

  return { get, stats: () => ({ rebuilds, patches }) }
}

export function buildGrid ({ column, eye, across, up }) {
  const at = { x: Math.floor(eye.x), y: Math.floor(eye.y), z: Math.floor(eye.z) }
  const origin = { x: at.x - across, y: at.y - up, z: at.z - across }
  const size = { x: across * 2 + 1, y: up * 2 + 1, z: across * 2 + 1 }
  const grid = makeGrid(origin, size)
  let top = -Infinity
  for (let cx = origin.x >> 4; cx <= (origin.x + size.x - 1) >> 4; cx++) {
    for (let cz = origin.z >> 4; cz <= (origin.z + size.z - 1) >> 4; cz++) {
      const col = column(cx, cz)
      if (!col) continue
      top = Math.max(top, fillColumn(grid, col, cx, cz))
    }
  }
  return Object.assign(grid, { top })
}
