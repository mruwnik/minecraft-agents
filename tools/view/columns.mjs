// Reads the chunk column files a body dumps (format v1, see docs): zlib( u32le headerLength, JSON header, parts... ).
// `sections` is exactly what prismarine-chunk's ChunkColumn.dump() returned, so column.load(sections) restores it.
import fs from 'node:fs'
import zlib from 'node:zlib'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'

export const FORMAT_VERSION = 1

export const makeChunkClass = version => {
  const registry = prismarineRegistry(version)
  return Object.assign(prismarineChunk(registry), { registry })
}

export function decodeColumnFile (buffer) {
  const raw = zlib.inflateSync(buffer)
  const headerLength = raw.readUInt32LE(0)
  const header = JSON.parse(raw.subarray(4, 4 + headerLength).toString('utf8'))
  if (header.v !== FORMAT_VERSION) throw new Error(`unsupported chunk file version ${header.v}`)
  let at = 4 + headerLength
  const parts = {}
  for (const { name, len } of header.parts) {
    parts[name] = raw.subarray(at, at + len)
    at += len
  }
  return { header, parts }
}

export function loadColumn (file, Chunk) {
  const { header, parts } = decodeColumnFile(fs.readFileSync(file))
  const column = new Chunk({ minY: header.minY, worldHeight: header.worldHeight })
  column.load(parts.sections)
  return column
}

// columns by path, kept while the file's mtime is the same; a missing file is null
export const columnCache = Chunk => {
  const entries = new Map()
  const everLoaded = new Set()
  let loads = 0
  const get = file => {
    const mtime = (() => {
      try { return fs.statSync(file).mtimeMs } catch { return null }
    })()
    if (mtime === null) {
      entries.delete(file)
      return null
    }
    const held = entries.get(file)
    if (held?.mtime === mtime) return held.column
    const column = loadColumn(file, Chunk)
    loads++
    everLoaded.add(file)
    entries.set(file, { mtime, column })
    return column
  }
  return { get, size: () => entries.size, stats: () => ({ loaded: entries.size, reloads: loads - everLoaded.size }) }
}
