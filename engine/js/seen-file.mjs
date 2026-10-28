// Why JavaScript: binary file format; packs typed arrays and deflates them.
// The seen-block memory on disk (engine.perception owns what is in it). One deflated buffer:
//   u32 LE header length, header JSON {v, version, dims: [name], sections: [[dimIndex, cx, sy, cz, seen, base]]},
//   then per section in header order 8192 bytes (4096 little-endian u16, stateId + 1, 0 = unknown) and 8192 bytes
//   (4096 little-endian u16, each cell's seen time, seconds after the section's base ms); then, for each section the
//   header's skies lists (by index), 512 bytes (a bit per cell by cell index: seen at full sky light).
// Format 2 (u8 minutes) still loads, its times scaled to seconds.
// Written atomically (temp file and rename); a missing or damaged file loads as null.
import fs from 'node:fs'
import zlib from 'node:zlib'
import { promisify } from 'node:util'
import { writeAtomic } from './write-atomic.mjs'

const deflate = promisify(zlib.deflate)
const FORMAT = 3
const IDS_BYTES = 8192
const TIMES_BYTES = 8192
const SECTION_BYTES = IDS_BYTES + TIMES_BYTES
const V2_SECTION_BYTES = IDS_BYTES + 4096
const SKY_BYTES = 512

// {version, sections: [{dim, cx, sy, cz, seen, ids: Uint16Array(4096), times, sky: Uint8Array(512) or null}]} -> the file; resolves when written
export async function saveSeen (file, { version, sections }) {
  const dims = [...new Set(sections.map(s => s.dim))]
  const skies = sections.flatMap((s, i) => (s.sky ? [i] : []))
  const header = Buffer.from(JSON.stringify({
    v: FORMAT,
    version,
    dims,
    sections: sections.map(s => [dims.indexOf(s.dim), s.cx, s.sy, s.cz, s.seen, s.base]),
    skies
  }))
  const skyAt = 4 + header.length + sections.length * SECTION_BYTES
  const out = Buffer.alloc(skyAt + skies.length * SKY_BYTES)
  out.writeUInt32LE(header.length, 0)
  header.copy(out, 4)
  sections.forEach((s, i) => {
    const at = 4 + header.length + i * SECTION_BYTES
    Buffer.from(s.ids.buffer, s.ids.byteOffset, IDS_BYTES).copy(out, at)
    Buffer.from(s.times.buffer, s.times.byteOffset, TIMES_BYTES).copy(out, at + IDS_BYTES)
  })
  skies.forEach((k, i) => Buffer.from(sections[k].sky.buffer, sections[k].sky.byteOffset, SKY_BYTES).copy(out, skyAt + i * SKY_BYTES))
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
  if ((header?.v !== FORMAT && header?.v !== 2) || !Array.isArray(header.sections)) return null
  const v2 = header.v === 2
  const stride = v2 ? V2_SECTION_BYTES : SECTION_BYTES
  const skies = Array.isArray(header.skies) ? header.skies : []
  const skyAt = 4 + length + header.sections.length * stride
  if (buf.length !== skyAt + skies.length * SKY_BYTES) return null
  const skyOf = new Map(skies.map((k, i) => [k, i]))
  return {
    version: header.version,
    sections: header.sections.map(([dim, cx, sy, cz, seen, base], i) => {
      const ids = new Uint16Array(4096)
      const times = new Uint16Array(4096)
      const at = 4 + length + i * stride
      Buffer.from(ids.buffer).set(buf.subarray(at, at + IDS_BYTES))
      if (v2) buf.subarray(at + IDS_BYTES, at + stride).forEach((minutes, j) => { times[j] = minutes * 60 })
      else Buffer.from(times.buffer).set(buf.subarray(at + IDS_BYTES, at + stride))
      const j = skyOf.get(i)
      const sky = j === undefined ? null : new Uint8Array(buf.subarray(skyAt + j * SKY_BYTES, skyAt + (j + 1) * SKY_BYTES))
      return { dim: header.dims[dim], cx, sy, cz, seen, base, ids, times, sky }
    })
  }
}
