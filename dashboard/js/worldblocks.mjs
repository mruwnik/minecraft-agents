// Why JavaScript: it sits on the binary chunk-column decoders and PNG encoder of tools/view (JS, binary formats and rendering).
import { worldsDir } from '../../engine/js/bodies.mjs'
// blockAt over the chunk columns the bodies dumped: worlds/<world>/chunks/<cx>.<cz>.bin (cx = floor(x / 16)),
// each read on first use and again when its mtime changes. The game version comes from the first column's header.
// This is glue to the view's column decoders; everything about plans lives in the ClojureScript (dashboard.plan*).
import fs from 'node:fs'
import path from 'node:path'
import { decodeColumnFile, columnCache, makeChunkClass } from '../../tools/view/columns.mjs'
import { encodePng } from '../../tools/view/renderer.mjs'

// -> {blockAt(x, y, z) -> {name, state: {property: value}} | null (no column dumped), close()}
export function createWorldBlocks ({ stateDir, world }) {
  const dir = path.join(worldsDir(stateDir), world, 'chunks')
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
    try {
      const block = column.getBlock({ x: x & 15, y, z: z & 15 })
      return block ? { name: block.name, state: block.getProperties() } : null
    } catch { return null }
  }
  return { blockAt, close: () => { columns = null } }
}

// ---------------------------------------------------------------- terrain tiles
// The top of every (x, z) of one dumped column, for the map's terrain layer. A fresh column is decoded per call and
// dropped (the server keeps only the PNG), so memory stays flat however much of the world is panned over.
// `skip` names the blocks the eye looks through (air, tall grass, flowers): the top is the first block under them.
// -> {palette: [name], top, y, depth, floor} per cell index z * 16 + x: top/floor index palette, y is the top block's
// height (the water surface for water), depth how many water blocks lie over the floor (0 when dry). null: not dumped.
export function createWorldTiles ({ stateDir, world }) {
  const dir = path.join(worldsDir(stateDir), world, 'chunks')
  const classes = new Map()
  const chunkClass = version => {
    if (!classes.has(version)) classes.set(version, makeChunkClass(version))
    return classes.get(version)
  }
  const column = (cx, cz, skip) => {
    const file = path.join(dir, `${cx}.${cz}.bin`)
    if (!fs.existsSync(file)) return null
    const { header, parts } = decodeColumnFile(fs.readFileSync(file))
    const Chunk = chunkClass(header.mcVersion)
    const chunk = new Chunk({ minY: header.minY, worldHeight: header.worldHeight })
    chunk.load(parts.sections)
    const skipSet = new Set(skip)
    const blocks = Chunk.registry.blocksByStateId
    const palette = []
    const index = new Map()
    const nameIndex = name => {
      if (!index.has(name)) { index.set(name, palette.length); palette.push(name) }
      return index.get(name)
    }
    const stateInfo = new Map()
    const infoOf = id => {
      if (!stateInfo.has(id)) {
        const name = blocks[id]?.name ?? 'air'
        stateInfo.set(id, { name, skipped: skipSet.has(name), water: name === 'water' })
      }
      return stateInfo.get(id)
    }
    const minY = header.minY
    const maxY = minY + header.worldHeight - 1
    const top = new Uint16Array(256)
    const floor = new Uint16Array(256)
    const y = new Int16Array(256)
    const depth = new Uint8Array(256)
    for (let z = 0; z < 16; z++) {
      for (let x = 0; x < 16; x++) {
        const cell = z * 16 + x
        let yy = maxY
        while (yy >= minY && infoOf(chunk.getBlockStateId({ x, y: yy, z })).skipped) yy--
        const found = yy < minY ? 'air' : infoOf(chunk.getBlockStateId({ x, y: yy, z })).name
        top[cell] = nameIndex(found)
        y[cell] = yy
        floor[cell] = top[cell]
        if (found !== 'water') continue
        let under = yy
        while (under >= minY && infoOf(chunk.getBlockStateId({ x, y: under, z })).water) under--
        depth[cell] = Math.min(255, yy - under)
        floor[cell] = nameIndex(under < minY ? 'air' : infoOf(chunk.getBlockStateId({ x, y: under, z })).name)
      }
    }
    return { palette, top, y, depth, floor }
  }
  return { column, close: () => classes.clear() }
}

// the view's own PNG encoder (src/vision/renderer.mjs), for 8-bit RGBA. A tile is a few hundred bytes, which Buffer.concat
// hands out as a slice of the 64 KB pool slab; the server caches thousands, and each would pin its slab (130 MB of
// ArrayBuffers for 2048 tiles), so the tile gets bytes of its own.
export function encodeTile (width, height, rgba) {
  return Buffer.from(new Uint8Array(encodePng(width, height, rgba)).buffer)
}
