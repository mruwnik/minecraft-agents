// blockAt over the chunk columns the bodies dumped: state/worlds/<world>/chunks/<cx>.<cz>.bin (cx = floor(x / 16)),
// each read on first use and again when its mtime changes. The game version comes from the first column's header.
// This is glue to the view's column decoders; everything about plans lives in the ClojureScript (dashboard.plan*).
import fs from 'node:fs'
import path from 'node:path'
import { decodeColumnFile, columnCache, makeChunkClass } from '../../tools/view/columns.mjs'

// -> {blockAt(x, y, z) -> block name | null (no column dumped), close()}
export function createWorldBlocks ({ stateDir, world }) {
  const dir = path.join(stateDir, 'worlds', world, 'chunks')
  let columns = null
  const columnsFor = file => {
    if (columns) return columns
    const header = decodeColumnFile(fs.readFileSync(file)).header
    columns = columnCache(makeChunkClass(header.mcVersion))
    return columns
  }
  const blockAt = (x, y, z) => {
    const file = path.join(dir, `${x >> 4}.${z >> 4}.bin`)
    if (!fs.existsSync(file)) return null
    const column = columnsFor(file).get(file)
    if (!column) return null
    try { return column.getBlock({ x: x & 15, y, z: z & 15 })?.name ?? null } catch { return null }
  }
  return { blockAt, close: () => { columns = null } }
}
