// Decodes the chunk column files a body dumps (docs/view-format.md) in a browser: no node imports. A port of what
// prismarine-chunk's 1.18 ChunkColumn.load reads, keeping only block state ids (biome containers are parsed to be skipped).

const SECTION_VOLUME = 4096
const BIOME_VOLUME = 64
const MAX_BLOCK_PALETTE_BITS = 8
const MAX_BIOME_PALETTE_BITS = 3

export async function inflate (bytes) {
  const stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream('deflate'))
  return new Uint8Array(await new Response(stream).arrayBuffer())
}

export function parseColumnFile (raw) {
  const view = new DataView(raw.buffer, raw.byteOffset, raw.byteLength)
  const headerLength = view.getUint32(0, true)
  const header = JSON.parse(new TextDecoder().decode(raw.subarray(4, 4 + headerLength)))
  if (header.v !== 1) throw new Error(`unsupported chunk file version ${header.v}`)
  const parts = {}
  let at = 4 + headerLength
  for (const { name, len } of header.parts) {
    parts[name] = raw.subarray(at, at + len)
    at += len
  }
  return { header, sections: parts.sections }
}

const reader = bytes => {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength)
  let at = 0
  return {
    u8: () => bytes[at++],
    i16: () => { const v = view.getInt16(at); at += 2; return v },
    u32: () => { const v = view.getUint32(at); at += 4; return v },
    skip: n => { at += n },
    varint: () => {
      let result = 0
      let shift = 0
      for (;;) {
        const b = bytes[at++]
        result |= (b & 0x7f) << shift
        if (!(b & 0x80)) return result >>> 0
        shift += 7
      }
    }
  }
}

// longs hold floor(64 / bits) values each and a value never spans two; a long is two big-endian uint32, high word first.
// Calls `put(i, value)` for each of `capacity` values.
const readPacked = (r, bits, longs, capacity, put) => {
  const perLong = Math.floor(64 / bits)
  const mask = bits >= 32 ? 0xffffffff : (1 << bits) - 1
  const words = new Uint32Array(longs * 2) // [low, high] per long
  for (let i = 0; i < longs; i++) {
    words[i * 2 + 1] = r.u32()
    words[i * 2] = r.u32()
  }
  for (let i = 0; i < capacity; i++) {
    const long = Math.floor(i / perLong)
    const offset = (i - long * perLong) * bits
    const value = offset >= 32
      ? (words[long * 2 + 1] >>> (offset - 32)) & mask
      : ((words[long * 2] >>> offset) | (offset + bits > 32 ? words[long * 2 + 1] << (32 - offset) : 0)) & mask
    put(i, value)
  }
}

// a paletted container: bits 0 single value, <= maxPaletteBits a local palette, above that the global one
const readContainer = (r, format, capacity, maxPaletteBits, put) => {
  const bits = r.u8()
  if (bits === 0) {
    const value = r.varint()
    if (!format.noSizePrefix) r.u8()
    for (let i = 0; i < capacity; i++) put(i, value)
    return
  }
  const palette = bits <= maxPaletteBits ? Array.from({ length: r.varint() }, () => r.varint()) : null
  const longs = format.noSizePrefix ? Math.ceil(capacity / Math.floor(64 / bits)) : r.varint()
  readPacked(r, bits, longs, capacity, palette ? (i, v) => put(i, palette[v]) : put)
}

export function decodeSections (bytes, format) {
  const r = reader(bytes)
  const ids = new Uint16Array(format.numSections * SECTION_VOLUME)
  const nonEmpty = new Uint8Array(format.numSections)
  const ignore = () => {}
  for (let s = 0; s < format.numSections; s++) {
    r.i16() // solid block count
    if (format.hasFluidCount) r.i16()
    const base = s * SECTION_VOLUME
    readContainer(r, format, SECTION_VOLUME, MAX_BLOCK_PALETTE_BITS, (i, v) => { ids[base + i] = v })
    nonEmpty[s] = ids.subarray(base, base + SECTION_VOLUME).some(v => v !== 0) ? 1 : 0
    readContainer(r, format, BIOME_VOLUME, MAX_BIOME_PALETTE_BITS, ignore)
  }
  return { ids, nonEmpty }
}
