// Why JavaScript: replaces a CommonJS dependency's stream transform (minecraft-protocol's framing splitter) at its load boundary; byte-level code.
// minecraft-protocol's Splitter concatenates its whole buffer on every TCP chunk (quadratic while a large chunk packet arrives in many reads) and reads
// the length with protodef's readVarInt, which throws a PartialReadError (stack captured) for each incomplete header. This one keeps the chunks in a list,
// joins them only once the frame being waited for is complete, and reads the length without throwing. Same frames out.
import { createRequire } from 'node:module'

const require = createRequire(import.meta.url)
const framing = require('minecraft-protocol/src/transforms/framing.js')
const { Transform } = require('readable-stream')
const [, writeVarInt, sizeOfVarInt] = require('protodef').types.varint

const LEGACY_PING_PACKET_ID = 0xfe

// {value, size} of the varint at offset, or null while its bytes have not all arrived.
export function readLength (buffer, offset) {
  let value = 0
  for (let size = 1, shift = 0; offset + size <= buffer.length; size++, shift += 7) {
    if (shift >= 35) throw new Error('varint is too big')
    const byte = buffer[offset + size - 1]
    value |= (byte & 0x7f) << shift
    if (!(byte & 0x80)) return { value, size }
  }
  return null
}

export class FrameSplitter extends Transform {
  constructor () {
    super()
    this.chunks = []
    this.length = 0
    this.need = 0
    this.recognizeLegacyPing = false
  }

  _transform (chunk, enc, cb) {
    this.chunks.push(chunk)
    this.length += chunk.length
    if (!this.recognizeLegacyPing && this.length < this.need) return cb()
    const buffer = this.chunks.length === 1 ? chunk : Buffer.concat(this.chunks, this.length)

    if (this.recognizeLegacyPing && buffer[0] === LEGACY_PING_PACKET_ID) {
      const header = Buffer.alloc(sizeOfVarInt(LEGACY_PING_PACKET_ID))
      writeVarInt(LEGACY_PING_PACKET_ID, header, 0)
      let payload = buffer.slice(1)
      if (payload.length === 0) payload = Buffer.from('\0')
      this.push(Buffer.concat([header, payload]))
      return cb()
    }

    let offset = 0
    this.need = 0
    for (;;) {
      const head = readLength(buffer, offset)
      if (!head) break
      const end = offset + head.size + head.value
      if (buffer.length < end) {
        this.need = end - offset
        break
      }
      this.push(buffer.slice(offset + head.size, end))
      offset = end
    }
    const rest = buffer.slice(offset)
    this.chunks = rest.length ? [rest] : []
    this.length = rest.length
    return cb()
  }
}

framing.createSplitter = () => new FrameSplitter()
