// A tiny synthetic world for the pixel regression check (tools/view-web-check.mjs): a stone floor, a wall of stripes
// at z=4 (lit stone, shaded stone, leaves over red wool, diamond ore) and explicit light in every cell.
import fs from 'node:fs'
import path from 'node:path'
import zlib from 'node:zlib'
import prismarineBlock from 'prismarine-block'
import { encodeColumn, columnFile, poseFile, hudFile, LIGHT_SECTION_BYTES } from '../../engine/js/view.mjs'
import { makeChunkClass } from './columns.mjs'

export const MC_VERSION = '26.1'
export const WORLD = 'fixture'
export const AGENT = 'Fixture'
const MIN_Y = -64
const WORLD_HEIGHT = 384
const CHUNKS = [[0, 0], [0, -1], [-1, 0], [-1, -1]]

const FLOOR_Y = 64
const WALL_Z = 4
const WALL_Y = [65, 68] // inclusive cell ranges
const DARK = { x: [5, 8], z: [5, 6] } // air in front of the dark stripe
const TORCH = { x: [6, 9], z: [8, 10], y: 65 } // torch-lit floor top, in the middle of the view

// a bottom slab and a two-layer snow, each with a wool / gold block right behind (north of) it: their upper parts are empty air
const SLAB = { x: 4, y: 65, z: 8 }
const SNOW = { x: 11, y: 65, z: 8 }
// a glass wall two blocks deep (z and z - 1) with a lime wool block right behind it: without the same-material cull the
// inner face between the two glass blocks draws its border across the middle of the front face
const GLASS = { x: [7, 9], y: [69, 70], z: 3 }
export const GLASS_BACK = 'lime_wool'
export const STATES = { oak_slab: { type: 'bottom' }, snow: { layers: 2 } }
const LIT_BLOCKS = new Set(['oak_leaves', 'oak_slab', 'snow', 'glass']) // not opaque: light passes through (partial blocks have their own light)

// the eye is far enough back (and in x centred on the wall) that the whole wall and the torch patch fit a 70 degree view
export const FIXTURE = {
  eye: { x: 8, y: 66.62, z: 15.5 },
  yaw: 0,
  pitch: 0,
  torch: TORCH,
  // world-space face rectangles (block edges, so cell 1..4 is [1, 5]); the wall's south faces are at z=5
  regions: [
    { name: 'lit', face: { axis: 'z', at: WALL_Z + 1, x: [1, 5], y: [65, 69] } },
    { name: 'dark', face: { axis: 'z', at: WALL_Z + 1, x: [5, 9], y: [65, 69] } },
    { name: 'leaves', face: { axis: 'z', at: WALL_Z + 1, x: [10, 11], y: [65, 69] } },
    { name: 'diamond', face: { axis: 'z', at: WALL_Z + 1, x: [12, 15], y: [65, 69] } },
    { name: 'torch', face: { axis: 'y', at: TORCH.y, x: [TORCH.x[0], TORCH.x[1] + 1], z: [TORCH.z[0], TORCH.z[1] + 1] } },
    // south faces of the slab's and the snow's cells; the upper regions start a bit above the top so rays that graze it are not counted
    { name: 'slab lower', face: { axis: 'z', at: SLAB.z + 1, x: [SLAB.x, SLAB.x + 1], y: [SLAB.y, SLAB.y + 0.5] } },
    { name: 'slab upper', face: { axis: 'z', at: SLAB.z + 1, x: [SLAB.x + 0.55, SLAB.x + 1], y: [SLAB.y + 0.7, SLAB.y + 1] } },
    { name: 'snow lower', face: { axis: 'z', at: SNOW.z + 1, x: [SNOW.x, SNOW.x + 1], y: [SNOW.y, SNOW.y + 0.25] } },
    // inside the middle cell's front face, inset 0.1 (the glass texture is transparent except for a thin border at the edges; 0.25 would be too small on screen at this distance)
    { name: 'glass', face: { axis: 'z', at: GLASS.z + 1, x: [8.1, 8.9], y: [69.1, 69.9] } },
    { name: 'snow upper', face: { axis: 'z', at: SNOW.z + 1, x: [SNOW.x, SNOW.x + 0.45], y: [SNOW.y + 0.55, SNOW.y + 1] } }
  ]
}

const within = (v, [lo, hi]) => v >= lo && v <= hi

export const blockNameAt = (x, y, z) => {
  if (x < 0 || x > 15 || z < 0 || z > 15) return 'air'
  if (y === FLOOR_Y) return 'stone'
  if (z === WALL_Z && within(y, WALL_Y)) {
    if (within(x, [1, 8])) return 'stone'
    if (x === 10) return 'oak_leaves'
    if (within(x, [12, 14])) return 'diamond_ore'
  }
  if (x === SLAB.x && y === SLAB.y && z === SLAB.z) return 'oak_slab'
  if (x === SLAB.x && y === SLAB.y && z === SLAB.z - 1) return 'red_wool'
  if (x === SNOW.x && y === SNOW.y && z === SNOW.z) return 'snow'
  if (x === SNOW.x && y === SNOW.y && z === SNOW.z - 1) return 'gold_block'
  if (within(x, GLASS.x) && within(y, GLASS.y)) {
    if (z === GLASS.z || z === GLASS.z - 1) return 'glass'
    if (z === GLASS.z - 2) return GLASS_BACK
  }
  // the backdrop is bigger than the glass: rays through its upper rows still rise past the glass's own height
  if (z === GLASS.z - 2 && within(x, [GLASS.x[0] - 1, GLASS.x[1] + 1]) && within(y, [GLASS.y[0], GLASS.y[1] + 5])) return GLASS_BACK
  if (z === WALL_Z - 1 && x === 10 && within(y, WALL_Y)) return 'red_wool'
  return 'air'
}

export const lightAt = (x, y, z) => {
  const name = blockNameAt(x, y, z)
  if (LIT_BLOCKS.has(name)) return { sky: 15, block: 0 } // light passes through leaves, so the wool behind them is lit
  if (name !== 'air') return { sky: 0, block: 0 }
  if (within(x, DARK.x) && within(z, DARK.z) && within(y, WALL_Y)) return { sky: 0, block: 0 }
  if (y === TORCH.y && within(x, TORCH.x) && within(z, TORCH.z)) return { sky: 0, block: 14 }
  return { sky: 15, block: 0 }
}

// keys name a block, or a block with properties when `states` maps the key to { name, props }
const stateIds = (registry, keys, states = {}) => {
  const Block = prismarineBlock(registry)
  return Object.fromEntries(keys.map(key => {
    const { name, props } = states[key] ?? { name: key, props: STATES[key] }
    return [key, props ? Block.fromProperties(name, props, 0).stateId : registry.blocksByName[name].defaultState]
  }))
}

// the column's light dump written straight in vanilla nibble order (cell i of a section: byte i >> 1, low nibble when i is
// even); prismarine's setSkyLight/setBlockLight scramble the order inside each 16-cell row. Light section l covers the
// layer l - 1 above the lowest section, so l = 0 and the top one stay empty (sky mask bits 1..numSections).
const lightDump = (cx, cz, light) => {
  const sections = WORLD_HEIGHT >> 4
  const buffers = key => Array.from({ length: sections }, (_, s) => {
    const buffer = Buffer.alloc(LIGHT_SECTION_BYTES)
    for (let i = 0; i < 4096; i++) {
      const at = light(cx * 16 + (i & 15), MIN_Y + s * 16 + (i >> 8), cz * 16 + ((i >> 4) & 15))
      buffer[i >> 1] |= at[key] << ((i & 1) * 4)
    }
    return buffer
  })
  const mask = [[0, 2 ** (sections + 1) - 2]]
  return { skyLight: buffers('sky'), blockLight: buffers('block'), skyLightMask: mask, blockLightMask: mask, emptySkyLightMask: [[0, 0]], emptyBlockLightMask: [[0, 0]] }
}

const buildColumn = (Chunk, ids, cx, cz, blockAt, light) => {
  const column = new Chunk({ minY: MIN_Y, worldHeight: WORLD_HEIGHT })
  for (let lx = 0; lx < 16; lx++) {
    for (let lz = 0; lz < 16; lz++) {
      for (let y = MIN_Y; y < MIN_Y + WORLD_HEIGHT; y++) {
        const x = cx * 16 + lx
        const z = cz * 16 + lz
        const name = blockAt(x, y, z)
        const at = { x: lx, y, z: lz }
        if (name !== 'air') column.setBlockStateId(at, ids[name])
      }
    }
  }
  column.dumpLight = () => lightDump(cx, cz, light)
  return column
}

const writeJson = (file, data) => {
  fs.mkdirSync(path.dirname(file), { recursive: true })
  fs.writeFileSync(file, JSON.stringify(data))
}

export const poseFor = (now, { eye = FIXTURE.eye, yaw = FIXTURE.yaw, pitch = FIXTURE.pitch, world = WORLD } = {}) => ({
  v: 1, t: now, world, status: 'online', dimension: 'overworld', mcVersion: MC_VERSION,
  pos: { x: eye.x, y: 65, z: eye.z }, eye, yaw, pitch,
  velocity: { x: 0, y: 0, z: 0 }, onGround: true, entities: [], timeOfDay: 6000, rain: 0
})

// Writes a world of four columns (chunks CHUNKS) for one agent: blockAt(x, y, z) names a block (a key of `states`, or a block name),
// light(x, y, z) is { sky, block }, camera is the pose ({ eye, yaw, pitch }).
export const writeWorld = ({ stateDir, world = WORLD, agent = AGENT, blockAt, light, states = {}, camera = FIXTURE, keys }) => {
  const Chunk = makeChunkClass(MC_VERSION)
  const ids = stateIds(Chunk.registry, keys, states)
  for (const [cx, cz] of CHUNKS) {
    const file = columnFile(stateDir, world, cx, cz)
    fs.mkdirSync(path.dirname(file), { recursive: true })
    const raw = encodeColumn({ column: buildColumn(Chunk, ids, cx, cz, blockAt, light), x: cx, z: cz, t: Date.now(), body: agent, mcVersion: MC_VERSION })
    fs.writeFileSync(file, zlib.deflateSync(raw, { level: 1 }))
  }
  writeJson(poseFile(stateDir, agent), poseFor(Date.now(), { ...camera, world }))
  writeJson(hudFile(stateDir, agent), { v: 1, t: Date.now(), health: 20, food: 20, saturation: 5, oxygen: 20, xp: { level: 0, points: 0, progress: 0 }, effects: [], held: null, inventory: [], window: null })
}

export const writeFixture = stateDir => writeWorld({
  stateDir,
  blockAt: blockNameAt,
  light: lightAt,
  keys: ['stone', 'oak_leaves', 'red_wool', 'diamond_ore', 'oak_slab', 'snow', 'gold_block', 'glass', GLASS_BACK]
})
