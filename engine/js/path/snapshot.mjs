// The planner's view of the world: 16x16x16 sections of block state ids, read with no allocation per lookup.
import fs from 'node:fs'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { decodeColumnFile, restoreColumn } from '../view.mjs'

export const UNLOADED = 0xFFFF
const VOLUME = 4096
const MC_VERSION = '26.1'

// two 16-bit halves in one int32: collides only beyond +-32767 chunks (+-524k blocks)
const keyOf = (cx, cz) => (cx & 0xFFFF) * 65536 + (cz & 0xFFFF)

export function createSnapshot ({ minY = -64, height = 384 } = {}) {
  const columnsByKey = new Map()
  const sectionCount = height >> 4
  let lastKey = -1
  let lastEntry = null

  const entryAt = (cx, cz) => {
    const key = keyOf(cx, cz)
    if (key === lastKey) return lastEntry
    const entry = columnsByKey.get(key)
    if (entry === undefined) return undefined
    lastKey = key
    lastEntry = entry
    return entry
  }

  const setSection = (cx, sy, cz, ids) => {
    const key = keyOf(cx, cz)
    const entry = columnsByKey.get(key) ?? { cx, cz, sections: new Array(sectionCount).fill(undefined), flagTable: null, flags: null }
    columnsByKey.set(key, entry)
    entry.sections[sy] = ids
    if (entry.flags) entry.flags[sy] = 0
  }

  const dropColumn = (cx, cz) => {
    columnsByKey.delete(keyOf(cx, cz))
    lastKey = -1
    lastEntry = null
  }

  const stateAt = (x, y, z) => {
    const ry = y - minY
    if (ry < 0 || ry >= height) return UNLOADED
    const entry = entryAt(x >> 4, z >> 4)
    if (entry === undefined) return UNLOADED
    const section = entry.sections[ry >> 4]
    if (section === undefined) return UNLOADED
    return section[((ry & 15) << 8) | ((z & 15) << 4) | (x & 15)]
  }

  const setState = (x, y, z, id) => {
    const ry = y - minY
    if (ry < 0 || ry >= height) return
    const section = entryAt(x >> 4, z >> 4)?.sections[ry >> 4]
    if (section === undefined) return
    section[((ry & 15) << 8) | ((z & 15) << 4) | (x & 15)] = id
    const entry = entryAt(x >> 4, z >> 4)
    if (entry.flags) entry.flags[ry >> 4] = 0
  }

  // does the section hold any state with flagTable[id] set? Scanned once per section and table, so a planner can skip whole
  // sections that cannot matter; writes into the section reset its answer.
  const sectionHas = (flagTable, cx, sy, cz) => {
    const entry = entryAt(cx, cz)
    const section = entry?.sections[sy]
    if (section === undefined) return false
    if (entry.flagTable !== flagTable) {
      entry.flagTable = flagTable
      entry.flags = new Uint8Array(sectionCount)
    }
    let known = entry.flags[sy]
    if (known === 0) {
      known = 1
      for (let i = 0; i < VOLUME && known === 1; i++) if (flagTable[section[i]]) known = 2
      entry.flags[sy] = known
    }
    return known === 2
  }

  return {
    minY,
    height,
    setSection,
    dropColumn,
    stateAt,
    setState,
    sectionHas,
    hasColumn: (cx, cz) => columnsByKey.has(keyOf(cx, cz)),
    columns: () => [...columnsByKey.values()].map(e => [e.cx, e.cz])
  }
}

export const sectionIds = container => {
  const ids = new Uint16Array(VOLUME)
  if (container.value !== undefined && container.palette === undefined && container.data === undefined) return ids.fill(container.value)
  for (let i = 0; i < VOLUME; i++) ids[i] = container.get(i)
  return ids
}

// prismarine ChunkColumn -> one Uint16Array per section, same local index order as the snapshot
export const columnSections = column => column.sections.map(section => sectionIds(section.data))

export function loadColumn (snapshot, cx, cz, column) {
  columnSections(column).forEach((ids, sy) => snapshot.setSection(cx, sy, cz, ids))
}

const ChunkColumn = prismarineChunk(prismarineRegistry(MC_VERSION))

const readColumn = file => {
  const decoded = decodeColumnFile(fs.readFileSync(file))
  const { minY, worldHeight } = decoded.header
  return restoreColumn(new ChunkColumn({ minY, worldHeight }), decoded)
}

const chunkFiles = (chunksDir, { cxMin = -Infinity, cxMax = Infinity, czMin = -Infinity, czMax = Infinity } = {}) =>
  fs.readdirSync(chunksDir)
    .map(name => /^(-?\d+)\.(-?\d+)\.bin$/.exec(name))
    .filter(Boolean)
    .map(m => ({ cx: Number(m[1]), cz: Number(m[2]), file: `${chunksDir}/${m[0]}` }))
    .filter(({ cx, cz }) => cx >= cxMin && cx <= cxMax && cz >= czMin && cz <= czMax)

export function loadRecordedWorld (snapshot, chunksDir, range = {}) {
  const files = chunkFiles(chunksDir, range)
  files.forEach(({ cx, cz, file }) => loadColumn(snapshot, cx, cz, readColumn(file)))
  return files.length
}
