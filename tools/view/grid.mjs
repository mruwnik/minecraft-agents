// The renderer's grid, filled from loaded chunk columns: a box `across` blocks out each way and `up` above and below
// the eye (centred, as a ray starts from inside it). Reads the palette sections directly, not getBlockStateId per
// cell, and skips sections that hold nothing.
import { makeGrid } from '../../src/vision/renderer.mjs'

const SECTION_VOLUME = 4096

export function buildGrid ({ column, eye, across, up }) {
  const at = { x: Math.floor(eye.x), y: Math.floor(eye.y), z: Math.floor(eye.z) }
  const origin = { x: at.x - across, y: at.y - up, z: at.z - across }
  const size = { x: across * 2 + 1, y: up * 2 + 1, z: across * 2 + 1 }
  const grid = makeGrid(origin, size)
  const { data } = grid
  let top = -Infinity
  for (let cx = origin.x >> 4; cx <= (origin.x + size.x - 1) >> 4; cx++) {
    for (let cz = origin.z >> 4; cz <= (origin.z + size.z - 1) >> 4; cz++) {
      const col = column(cx, cz)
      if (!col) continue
      const minY = col.minY
      const yLow = Math.max(minY, origin.y)
      const yHigh = Math.min(minY + col.worldHeight - 1, origin.y + size.y - 1)
      const x0 = Math.max(origin.x, cx * 16) - cx * 16
      const x1 = Math.min(origin.x + size.x - 1, cx * 16 + 15) - cx * 16
      const z0 = Math.max(origin.z, cz * 16) - cz * 16
      const z1 = Math.min(origin.z + size.z - 1, cz * 16 + 15) - cz * 16
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
    }
  }
  return Object.assign(grid, { top })
}
