// The planner's snapshot over a mineflayer world, copied lazily: a section's 4096 ids are read (sectionIds) the first
// time stateAt, setState or sectionHas touches it. A fresh snapshot is made per plan, so no block-update listener is
// needed and the cost is one section copy per section the plan touches. A column or section the world lacks is
// remembered as tried (no re-read) and reads UNLOADED.
import { createSnapshot, sectionIds, UNLOADED } from './snapshot.mjs'

export function liveSnapshot (world, { minY, height }) {
  const inner = createSnapshot({ minY, height })
  const columns = new Map() // "cx,cz" -> column or null
  const tried = new Set() // "cx,sy,cz"

  const columnAt = (cx, cz) => {
    const key = `${cx},${cz}`
    if (columns.has(key)) return columns.get(key)
    const column = world.getColumn(cx, cz) ?? null
    columns.set(key, column)
    return column
  }

  const ensure = (cx, sy, cz) => {
    const key = `${cx},${sy},${cz}`
    if (tried.has(key)) return
    tried.add(key)
    const container = columnAt(cx, cz)?.sections[sy]?.data
    if (container) inner.setSection(cx, sy, cz, sectionIds(container))
  }

  const inRange = y => y >= minY && y < minY + height
  const touch = (x, y, z) => ensure(x >> 4, (y - minY) >> 4, z >> 4)

  return {
    minY,
    height,
    stateAt: (x, y, z) => {
      if (!inRange(y)) return UNLOADED
      touch(x, y, z)
      return inner.stateAt(x, y, z)
    },
    setState: (x, y, z, stateId) => {
      if (!inRange(y)) return
      touch(x, y, z)
      inner.setState(x, y, z, stateId)
    },
    sectionHas: (flagTable, cx, sy, cz) => {
      ensure(cx, sy, cz)
      return inner.sectionHas(flagTable, cx, sy, cz)
    },
    hasColumn: (cx, cz) => columnAt(cx, cz) !== null,
    columns: () => [...columns].filter(([, column]) => column !== null).map(([key]) => key.split(',').map(Number))
  }
}
