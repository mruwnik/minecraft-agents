// Why JavaScript: binary/graphics; the view dump's column, light and pose encoding.
// View dump chunk column file encoding and decoding.
import zlib from 'node:zlib'
import { encodeLight } from './view-light.mjs'

export const VIEW_VERSION = 1

// ---- chunk column encoding ----

// the uncompressed file content: uint32le header length, JSON header, then the parts
export function encodeColumn ({ column, x, z, t, body, mcVersion, overlay }) {
  const sections = column.dump()
  const biomes = column.dumpBiomes?.() ?? Buffer.alloc(0)
  const light = encodeLight(column, overlay)
  const header = Buffer.from(JSON.stringify({
    v: VIEW_VERSION, x, z, t, body, mcVersion, minY: column.minY, worldHeight: column.worldHeight,
    parts: [
      { name: 'sections', len: sections.length },
      { name: 'biomes', len: biomes.length },
      { name: 'light', len: light.buffer.length, meta: light.meta }
    ]
  }), 'utf8')
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  return Buffer.concat([length, header, sections, biomes, light.buffer])
}

// the reader's side: the file as written (deflated) -> {header, sections, biomes, light: {meta, buffer}}
export function decodeColumnFile (file) {
  const raw = zlib.inflateSync(file)
  const n = raw.readUInt32LE(0)
  const header = JSON.parse(raw.subarray(4, 4 + n).toString('utf8'))
  let at = 4 + n
  const parts = Object.fromEntries(header.parts.map(p => {
    const buffer = raw.subarray(at, at + p.len)
    at += p.len
    return [p.name, { buffer, meta: p.meta }]
  }))
  return { header, sections: parts.sections.buffer, biomes: parts.biomes.buffer, light: parts.light }
}

// load a decoded file into a fresh prismarine ChunkColumn built with {minY, worldHeight} from the header
export function restoreColumn (column, { sections, light }) {
  column.load(sections)
  const { meta, buffer } = light
  if (!meta.sectionBytes) return column
  const slice = (from, count) => Array.from({ length: count }, (_, i) => buffer.subarray((from + i) * meta.sectionBytes, (from + i + 1) * meta.sectionBytes))
  column.loadParsedLight(slice(0, meta.skyCount), slice(meta.skyCount, meta.blockCount),
    meta.skyLightMask, meta.blockLightMask, meta.emptySkyLightMask, meta.emptyBlockLightMask)
  return column
}
