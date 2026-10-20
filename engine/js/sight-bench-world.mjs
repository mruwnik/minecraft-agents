// Why JavaScript: builds a real raw world (engine/js/raw-world.mjs) over prismarine chunk columns, for engine.sight-bench-test.
// Test-only worlds: rolling hills, a lit cave room in stone, a pen of fences in a field. Light is sky 15 above the surface, 0 inside stone.
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { createRawWorld } from './raw-world.mjs'
import { packNibbles } from './view.mjs'

const { Vec3 } = vec3
const SECTIONS = 384 >> 4
const registry = prismarineRegistry('26.1')
const Column = prismarineChunk(registry)
const state = name => registry.blocksByName[name].defaultState
const hill = (x, z) => 64 + Math.round(6 * Math.sin(x / 9) * Math.cos(z / 11) + 3 * Math.sin((x + z) / 5))

const heights = {
  hills: hill,
  cave: () => 64,
  pen: (x, z) => 64 + ((x + 100) % 40 === 0 ? 1 : 0)
}

// cells[x, y, z] -> block name for the kinds with extra structure; null = leave as generated
const extras = {
  hills: (x, y, z) => (y > hill(x, z) && y <= hill(x, z) + 5 && ((x * 7 + z * 13) % 29 === 0) ? 'oak_log' : null),
  cave: (x, y, z) => (y >= 58 && y <= 72 ? (Math.abs(x) <= 6 && Math.abs(z) <= 6 && y >= 62 && y <= 68 ? 'air' : 'stone') : null),
  pen: (x, y, z) => (y === 65 && (Math.abs(x) === 5 || Math.abs(z) === 5) && Math.abs(x) <= 5 && Math.abs(z) <= 5 ? 'oak_fence' : null)
}

export const groundAt = (kind, x, z) => (kind === 'cave' ? 64 : heights[kind](Math.floor(x), Math.floor(z)))

export function makeRaw (kind, radius = 4) {
  const columns = new Map()
  for (let cx = -radius; cx < radius; cx++) for (let cz = -radius; cz < radius; cz++) {
    const c = new Column({ minY: -64, worldHeight: 384 })
    for (let x = 0; x < 16; x++) for (let z = 0; z < 16; z++) {
      const X = cx * 16 + x, Z = cz * 16 + z, h = heights[kind](X, Z)
      for (let y = 40; y <= h; y++) c.setBlockStateId(new Vec3(x, y, z), state(y === h && kind !== 'cave' ? 'grass_block' : 'stone'))
      for (let y = h + 1; y <= h + 12 || (kind === 'cave' && y <= 72); y++) {
        const name = extras[kind](X, y, Z)
        if (name && name !== 'air') c.setBlockStateId(new Vec3(x, y, z), state(name))
        if (name === 'air') c.setBlockStateId(new Vec3(x, y, z), state('air'))
      }
      if (kind === 'cave') for (let y = 62; y <= 68; y++) if (Math.abs(X) <= 6 && Math.abs(Z) <= 6) c.setBlockStateId(new Vec3(x, y, z), state('air'))
    }
    if (kind === 'cave') {
      const zero = Array.from({ length: SECTIONS }, () => Buffer.from(packNibbles(new Uint8Array(4096))))
      const mask = [[0, ((1 << SECTIONS) - 1) << 1]]
      c.loadParsedLight(zero, zero, mask, mask, [[0, 0]], [[0, 0]])
    }
    columns.set(cx + ',' + cz, c)
  }
  const bot = new EventEmitter()
  Object.assign(bot, {
    registry, version: '26.1', game: { minY: -64, height: 384, dimension: 'overworld' }, time: { timeOfDay: 6000 }, rainState: 0,
    entity: { position: new Vec3(0.5, kind === 'cave' ? 64 : heights[kind](0, 0) + 1, 0.5), yaw: 0, pitch: 0 },
    world: { getColumn: (cx, cz) => columns.get(cx + ',' + cz) }
  })
  return { raw: createRawWorld({ getBot: () => bot }), bot }
}
