// Why JavaScript: binary file format; packs typed arrays and deflates them.
// The seen-block memory on disk (engine.perception owns what is in it). One deflated buffer:
//   u32 LE header length, header JSON {v, version, dims: [name], sections: [[dimIndex, cx, sy, cz, seen, base]]},
//   then per section in header order 8192 bytes (4096 little-endian u16, stateId + 1, 0 = unknown) and 4096 bytes
//   (each cell's seen time, minutes after the section's base ms).
// Written atomically (temp file and rename); a missing or damaged file loads as null.
import fs from 'node:fs'
import zlib from 'node:zlib'
import { promisify } from 'node:util'
import { writeAtomic } from './view.mjs'

const deflate = promisify(zlib.deflate)
const FORMAT = 2
const IDS_BYTES = 8192
const TIMES_BYTES = 4096
const SECTION_BYTES = IDS_BYTES + TIMES_BYTES

// {version, sections: [{dim, cx, sy, cz, seen, ids: Uint16Array(4096)}]} -> the file; resolves when written
export async function saveSeen (file, { version, sections }) {
  const dims = [...new Set(sections.map(s => s.dim))]
  const header = Buffer.from(JSON.stringify({
    v: FORMAT,
    version,
    dims,
    sections: sections.map(s => [dims.indexOf(s.dim), s.cx, s.sy, s.cz, s.seen, s.base])
  }))
  const out = Buffer.alloc(4 + header.length + sections.length * SECTION_BYTES)
  out.writeUInt32LE(header.length, 0)
  header.copy(out, 4)
  sections.forEach((s, i) => {
    const at = 4 + header.length + i * SECTION_BYTES
    Buffer.from(s.ids.buffer, s.ids.byteOffset, IDS_BYTES).copy(out, at)
    Buffer.from(s.times.buffer, s.times.byteOffset, TIMES_BYTES).copy(out, at + IDS_BYTES)
  })
  await writeAtomic(file, await deflate(out))
}

const readBuffer = file => {
  try { return zlib.inflateSync(fs.readFileSync(file)) } catch { return null }
}

const parseHeader = text => {
  try { return JSON.parse(text) } catch { return null }
}

export function loadSeen (file) {
  const buf = readBuffer(file)
  if (!buf || buf.length < 4) return null
  const length = buf.readUInt32LE(0)
  const header = parseHeader(buf.subarray(4, 4 + length).toString())
  if (header?.v !== FORMAT || !Array.isArray(header.sections)) return null
  if (buf.length !== 4 + length + header.sections.length * SECTION_BYTES) return null
  return {
    version: header.version,
    sections: header.sections.map(([dim, cx, sy, cz, seen, base], i) => {
      const ids = new Uint16Array(4096)
      const times = new Uint8Array(4096)
      const at = 4 + length + i * SECTION_BYTES
      Buffer.from(ids.buffer).set(buf.subarray(at, at + IDS_BYTES))
      times.set(buf.subarray(at + IDS_BYTES, at + SECTION_BYTES))
      return { dim: header.dims[dim], cx, sy, cz, seen, base, ids, times }
    })
  }
}
