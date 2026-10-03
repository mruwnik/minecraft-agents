// Biome ids of a dumped column, in a browser: no node imports. The reader helpers below are copies of the ones in
// decode.mjs (which was off limits when this was written): merge them (export from decode.mjs) when it is free.

const SECTION_VOLUME = 4096
const BIOME_VOLUME = 64
const MAX_BLOCK_PALETTE_BITS = 8
const MAX_BIOME_PALETTE_BITS = 3

const reader = bytes => {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength)
  let at = 0
  return {
    u8: () => bytes[at++],
    i16: () => { const v = view.getInt16(at); at += 2; return v },
    u32: () => { const v = view.getUint32(at); at += 4; return v },
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

const readPacked = (r, bits, longs, capacity, put) => {
  const perLong = Math.floor(64 / bits)
  const mask = bits >= 32 ? 0xffffffff : (1 << bits) - 1
  const words = new Uint32Array(longs * 2)
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

// biome ids, 64 per section (4x4x4 cells of 4 blocks), section s at s * 64, index y4 << 4 | z4 << 2 | x4 (y4 up within the section).
// Ids above 255 become 255.
export function decodeBiomes (bytes, format) {
  const r = reader(bytes)
  const out = new Uint8Array(format.numSections * BIOME_VOLUME)
  const ignore = () => {}
  for (let s = 0; s < format.numSections; s++) {
    r.i16()
    if (format.hasFluidCount) r.i16()
    readContainer(r, format, SECTION_VOLUME, MAX_BLOCK_PALETTE_BITS, ignore)
    const base = s * BIOME_VOLUME
    readContainer(r, format, BIOME_VOLUME, MAX_BIOME_PALETTE_BITS, (i, v) => { out[base + i] = v > 255 ? 255 : v })
  }
  return out
}

// section order to a 3D texture of 4 x (numSections * 4) x 4 cells: index (z4 * (numSections * 4) + y4global) * 4 + x4
export function biomeTextureOrder (ids, numSections, out = new Uint8Array(ids.length)) {
  const height = numSections * 4
  for (let i = 0; i < ids.length; i++) {
    const y = (i >> 6) * 4 + ((i >> 4) & 3)
    out[(((i >> 2) & 3) * height + y) * 4 + (i & 3)] = ids[i]
  }
  return out
}
