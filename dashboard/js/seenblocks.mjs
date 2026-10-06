// Why JavaScript: it sits on the seen-block file format (binary, engine/js/seen-file.mjs) and the prismarine block registry.
import path from 'node:path'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { worldsDir } from '../../engine/js/bodies.mjs'
import { loadSeen } from '../../engine/js/seen-file.mjs'

const MINUTE_MS = 60000
const OVERWORLD = 'minecraft:overworld'

// The body's own block memory: worlds/<world>/agents/<body>/engine/seen.bin, only what it has seen.
// -> {blockAt(x, y, z) -> {name, state} | null (never seen), seenAt(x, y, z) -> ms | null, sections, close()}
export function createSeenBlocks ({ stateDir, world, body }) {
  const file = path.join(worldsDir(stateDir), world, 'agents', body, 'engine', 'seen.bin')
  const data = loadSeen(file)
  const dim = data?.sections.some(s => s.dim === OVERWORLD) ? OVERWORLD : data?.sections[0]?.dim
  const sections = new Map()
  for (const s of data?.sections ?? []) if (s.dim === dim) sections.set(`${s.cx},${s.sy},${s.cz}`, s)
  const Block = data ? prismarineBlock(prismarineRegistry(data.version)) : null
  const cell = (x, y, z) => {
    const s = sections.get(`${x >> 4},${y >> 4},${z >> 4}`)
    if (!s) return null
    const i = ((y & 15) << 8) | ((z & 15) << 4) | (x & 15)
    return s.ids[i] ? { s, i } : null
  }
  return {
    found: data !== null,
    sections: sections.size,
    blockAt: (x, y, z) => {
      const c = cell(x, y, z)
      if (!c) return null
      const block = Block.fromStateId(c.s.ids[c.i] - 1, 0)
      return { name: block.name, state: block.getProperties() }
    },
    seenAt: (x, y, z) => {
      const c = cell(x, y, z)
      return c ? c.s.base + MINUTE_MS * c.s.times[c.i] : null
    },
    close: () => {}
  }
}
