// Why JavaScript: it sits on the seen-block file format (binary, engine/js/seen-file.mjs) and the prismarine block registry.
import path from 'node:path'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { worldsDir } from '../../engine/js/bodies.mjs'
import { loadSeen } from '../../engine/js/seen-file.mjs'

const SECOND_MS = 1000
const OVERWORLD = 'overworld'
const bare = dim => String(dim).replace(/^minecraft:/, '')

// The body's own block memory: worlds/<world>/agents/<body>/engine/seen.bin, only what it has seen.
// dim: the dimension to read, the overworld when omitted (a plan's :dim); the minecraft: prefix is optional.
// -> {blockAt(x, y, z) -> {name, state} | null (never seen), seenAt(x, y, z) -> ms | null, sections, close()}
export function createSeenBlocks ({ stateDir, world, body, dim = OVERWORLD }) {
  const file = path.join(worldsDir(stateDir), world, 'agents', body, 'engine', 'seen.bin')
  const data = loadSeen(file)
  const sections = new Map()
  for (const s of data?.sections ?? []) if (bare(s.dim) === bare(dim)) sections.set(`${s.cx},${s.sy},${s.cz}`, s)
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
      return c ? c.s.base + SECOND_MS * c.s.times[c.i] : null
    },
    close: () => {}
  }
}
