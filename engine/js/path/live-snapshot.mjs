// Why JavaScript: Mineflayer boundary; lazy copy over Mineflayer's world.
// The planner's snapshot over a mineflayer world, copied lazily: a section's 4096 ids are read (sectionIds, or the
// body's memory with unknown) the first time stateAt, setState or sectionHas touches it. A fresh snapshot is made per plan, so no block-update listener is
// needed and the cost is one section copy per section the plan touches (the region map's snapshot is kept: its source
// follows changes with setState and forgetColumn). A column or section the world lacks is
// remembered as tried (no re-read) and reads UNLOADED.
// With unknown {id, known, ...}: a copied section is masked once, every cell the body never saw or felt reading unknown.id
// (known(cx, sy, cz), sy the WORLD section y, is the body's memory of that section, ids + 1 and 0 for never seen, or null:
// all unseen); a known cell reads its remembered id, so a change made out of sight stays unknown to the planner. unknown.blocked, a Set of "x,y,z" cells the body looked at and still could not see, reads unknown.blockedId while still unseen (stone: hidden behind other blocks, as rock is). The snapshot carries unknown (the planner's options.unknown), except with unknown.edge: the unseen cells read UNLOADED (the planner's loaded edge, walked to as a frontier) and the planner is not masked.
import { createSnapshot, sectionIds, UNLOADED } from './snapshot.mjs'

const mask = (ids, memory, id) => {
  for (let i = 0; i < ids.length; i++) ids[i] = !memory || memory[i] === 0 ? id : memory[i] - 1
  return ids
}

export function liveSnapshot (world, { minY, height, unknown }) {
  const inner = createSnapshot({ minY, height })
  const block = (ids, u, cx, sy, cz, memory) => {
    for (const key of u.blocked ?? []) {
      const [x, y, z] = key.split(',').map(Number)
      if (x >> 4 !== cx || (y - minY) >> 4 !== sy || z >> 4 !== cz) continue
      const i = (((y - minY) & 15) << 8) | ((z & 15) << 4) | (x & 15)
      // a cell seen since (a wall the body dug through) reads as it is
      if (!memory || memory[i] === 0) ids[i] = u.blockedId
    }
    return ids
  }
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
    if (!container) return
    if (!unknown) return inner.setSection(cx, sy, cz, sectionIds(container))
    const memory = unknown.known(cx, sy + (minY >> 4), cz)
    inner.setSection(cx, sy, cz, block(mask(new Uint16Array(4096), memory, unknown.id), unknown, cx, sy, cz, memory))
  }

  const inRange = y => y >= minY && y < minY + height
  const touch = (x, y, z) => ensure(x >> 4, (y - minY) >> 4, z >> 4)

  return {
    minY,
    height,
    unknown: unknown?.edge ? undefined : unknown,
    // the planner's frontier is any seen/unseen border, not only a chunk side (planner inBand)
    edge: Boolean(unknown?.edge),
    stateAt: (x, y, z) => {
      if (!inRange(y)) return UNLOADED
      // fast path: a copied section answers directly; only an UNLOADED read pays the string key and tried lookup
      const state = inner.stateAt(x, y, z)
      if (state !== UNLOADED) return state
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
    // with unknown.edge no column counts as loaded, so the goal flood leaks into its UNLOADED cells instead of walling on them
    hasColumn: (cx, cz) => !unknown?.edge && columnAt(cx, cz) !== null,
    // a column loaded or unloaded since it was read: forget the copy, read it again when next asked (a snapshot kept
    // across plans)
    forgetColumn: (cx, cz) => {
      columns.delete(`${cx},${cz}`)
      for (let sy = 0; sy < height >> 4; sy++) tried.delete(`${cx},${sy},${cz}`)
      inner.dropColumn(cx, cz)
    },
    columns: () => [...columns].filter(([, column]) => column !== null).map(([key]) => key.split(',').map(Number))
  }
}
