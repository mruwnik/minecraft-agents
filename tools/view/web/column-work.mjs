// Everything the view does to a column file before the GPU: inflate, parse, decode block ids, map them to materials and
// unpack the light. Pure, so it runs in a worker (decode-worker.mjs) or on the main thread. Every output is a typed array
// that owns its whole buffer, so it can be transferred.
import { inflate, parseColumnFile, decodeSections, lightColumn } from './decode.mjs'

// state ids of a decoded column to material indices in the texture's order (x fastest, then y, then z)
export const materialColumn = (ids, height, materialOf) => {
  const mats = new Uint16Array(256 * height)
  const flags = new Uint8Array(height >> 4)
  for (let y = 0; y < height; y++) {
    const section = y >> 4
    const base = section * 4096 + ((y & 15) << 8)
    for (let z = 0; z < 16; z++) {
      for (let x = 0; x < 16; x++) {
        const material = materialOf[ids[base + (z << 4 | x)]] ?? 0
        if (material === 0) continue
        mats[(z * height + y) * 16 + x] = material
        flags[section] = 1
      }
    }
  }
  return { mats, flags }
}

export const transferables = ({ mats, flags, light }) => [mats.buffer, flags.buffer, light.buffer]

// syncMs: the time spent in the synchronous steps (everything but the awaited inflate), which is what blocks a thread
export const decodeColumn = async (bytes, { format, materialOf }) => {
  const raw = await inflate(bytes)
  const started = performance.now()
  const { header, sections, light } = parseColumnFile(raw)
  const { ids } = decodeSections(sections, { ...format, numSections: header.worldHeight >> 4 })
  const { mats, flags } = materialColumn(ids, header.worldHeight, materialOf)
  const lightStarted = performance.now()
  const lit = lightColumn(light, header.worldHeight)
  const done = performance.now()
  return { header, mats, flags, light: lit, syncMs: done - started, lightMs: done - lightStarted }
}
