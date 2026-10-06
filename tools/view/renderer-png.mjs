// Why JavaScript: graphics/performance; software raycaster pieces.
// 8-bit PNG encoder and decoder for the software renderer and the view tools.
import zlib from 'node:zlib'

// ---------------------------------------------------------------- png
const PNG_MAGIC = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10])
const CRC_TABLE = Array.from({ length: 256 }, (_, n) => {
  let c = n
  for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1
  return c >>> 0
})
const crc32 = buf => {
  let c = 0xffffffff
  for (const b of buf) c = CRC_TABLE[(c ^ b) & 0xff] ^ (c >>> 8)
  return (c ^ 0xffffffff) >>> 0
}
const pngChunk = (type, data) => {
  const body = Buffer.concat([Buffer.from(type, 'ascii'), data])
  const out = Buffer.alloc(body.length + 8)
  out.writeUInt32BE(data.length, 0)
  body.copy(out, 4)
  out.writeUInt32BE(crc32(body), body.length + 4)
  return out
}

export function encodePng (width, height, rgba) {
  const header = Buffer.alloc(13)
  header.writeUInt32BE(width, 0)
  header.writeUInt32BE(height, 4)
  header.set([8, 6, 0, 0, 0], 8)
  const stride = width * 4
  const raw = Buffer.alloc((stride + 1) * height)
  for (let y = 0; y < height; y++) raw.set(rgba.subarray(y * stride, (y + 1) * stride), y * (stride + 1) + 1)
  // the fastest level: a frame is 9 KB bigger and 2.5 ms sooner, and the stream sends ten a second
  return Buffer.concat([PNG_MAGIC, pngChunk('IHDR', header), pngChunk('IDAT', zlib.deflateSync(raw, { level: 1 })), pngChunk('IEND', Buffer.alloc(0))])
}

const CHANNELS = { 0: 1, 2: 3, 3: 1, 4: 2, 6: 4 }
const paeth = (a, b, c) => {
  const p = a + b - c
  const [pa, pb, pc] = [Math.abs(p - a), Math.abs(p - b), Math.abs(p - c)]
  return pa <= pb && pa <= pc ? a : pb <= pc ? b : c
}

function unfilter (data, rowBytes, bpp, height) {
  const out = new Uint8Array(rowBytes * height)
  for (let y = 0; y < height; y++) {
    const filter = data[y * (rowBytes + 1)]
    for (let i = 0; i < rowBytes; i++) {
      const x = data[y * (rowBytes + 1) + 1 + i]
      const a = i >= bpp ? out[y * rowBytes + i - bpp] : 0
      const b = y > 0 ? out[(y - 1) * rowBytes + i] : 0
      const c = i >= bpp && y > 0 ? out[(y - 1) * rowBytes + i - bpp] : 0
      const predicted = [0, a, b, (a + b) >> 1, paeth(a, b, c)][filter]
      out[y * rowBytes + i] = (x + predicted) & 0xff
    }
  }
  return out
}

// Handles what Mojang ships: 8-bit grey/rgb/rgba and 1/2/4/8-bit palettes, non-interlaced.
export function decodePng (file) {
  if (!file.subarray(0, 8).equals(PNG_MAGIC)) throw new Error('not a png')
  const chunks = {}
  const idat = []
  for (let at = 8; at < file.length;) {
    const length = file.readUInt32BE(at)
    const type = file.toString('ascii', at + 4, at + 8)
    const data = file.subarray(at + 8, at + 8 + length)
    if (type === 'IDAT') idat.push(data)
    else chunks[type] = data
    at += length + 12
  }
  const width = chunks.IHDR.readUInt32BE(0)
  const height = chunks.IHDR.readUInt32BE(4)
  const [depth, colorType, , , interlace] = chunks.IHDR.subarray(8)
  if (interlace || depth > 8) throw new Error(`unsupported png (depth ${depth}, interlace ${interlace})`)
  const channels = CHANNELS[colorType]
  const rowBytes = Math.ceil(width * channels * depth / 8)
  const pixels = unfilter(zlib.inflateSync(Buffer.concat(idat)), rowBytes, Math.max(1, channels * depth >> 3), height)
  const sample = (x, y, ch) => {
    if (depth === 8) return pixels[y * rowBytes + x * channels + ch]
    const bit = x * depth
    return (pixels[y * rowBytes + (bit >> 3)] >> (8 - depth - (bit & 7))) & ((1 << depth) - 1)
  }
  const greyScale = 255 / ((1 << depth) - 1)
  const rgba = new Uint8Array(width * height * 4)
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const s = ch => sample(x, y, ch)
      const px = colorType === 3
        ? [...chunks.PLTE.subarray(s(0) * 3, s(0) * 3 + 3), chunks.tRNS?.[s(0)] ?? 255]
        : colorType === 0 ? [s(0) * greyScale, s(0) * greyScale, s(0) * greyScale, 255]
          : colorType === 4 ? [s(0), s(0), s(0), s(1)]
            : colorType === 2 ? [s(0), s(1), s(2), 255]
              : [s(0), s(1), s(2), s(3)]
      rgba.set(px, (y * width + x) * 4)
    }
  }
  return { width, height, rgba }
}
