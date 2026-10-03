// A tiny synthetic world for the pixel regression check (tools/view-web-check.mjs): a stone floor, a wall of stripes
// at z=4 (lit stone, shaded stone, leaves over red wool, diamond ore) and explicit light in every cell.
import fs from 'node:fs'
import path from 'node:path'
import zlib from 'node:zlib'
import { encodeColumn, columnFile, poseFile, hudFile } from '../../engine/js/view.mjs'
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
    { name: 'torch', face: { axis: 'y', at: TORCH.y, x: [TORCH.x[0], TORCH.x[1] + 1], z: [TORCH.z[0], TORCH.z[1] + 1] } }
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
  if (z === WALL_Z - 1 && x === 10 && within(y, WALL_Y)) return 'red_wool'
  return 'air'
}

export const lightAt = (x, y, z) => {
  if (blockNameAt(x, y, z) !== 'air') return { sky: 0, block: 0 }
  if (within(x, DARK.x) && within(z, DARK.z) && within(y, WALL_Y)) return { sky: 0, block: 0 }
  if (y === TORCH.y && within(x, TORCH.x) && within(z, TORCH.z)) return { sky: 0, block: 14 }
  return { sky: 15, block: 0 }
}

const stateIds = (registry, names) => Object.fromEntries(names.map(name => [name, registry.blocksByName[name].defaultState]))

const buildColumn = (Chunk, ids, cx, cz) => {
  const column = new Chunk({ minY: MIN_Y, worldHeight: WORLD_HEIGHT })
  for (let lx = 0; lx < 16; lx++) {
    for (let lz = 0; lz < 16; lz++) {
      for (let y = MIN_Y; y < MIN_Y + WORLD_HEIGHT; y++) {
        const x = cx * 16 + lx
        const z = cz * 16 + lz
        const name = blockNameAt(x, y, z)
        const light = lightAt(x, y, z)
        const at = { x: lx, y, z: lz }
        if (name !== 'air') column.setBlockStateId(at, ids[name])
        column.setSkyLight(at, light.sky)
        column.setBlockLight(at, light.block)
      }
    }
  }
  return column
}

const writeJson = (file, data) => {
  fs.mkdirSync(path.dirname(file), { recursive: true })
  fs.writeFileSync(file, JSON.stringify(data))
}

export const poseFor = now => ({
  v: 1, t: now, world: WORLD, status: 'online', dimension: 'overworld', mcVersion: MC_VERSION,
  pos: { x: FIXTURE.eye.x, y: 65, z: FIXTURE.eye.z }, eye: FIXTURE.eye, yaw: FIXTURE.yaw, pitch: FIXTURE.pitch,
  velocity: { x: 0, y: 0, z: 0 }, onGround: true, entities: [], timeOfDay: 6000, rain: 0
})

export const writeFixture = stateDir => {
  const Chunk = makeChunkClass(MC_VERSION)
  const ids = stateIds(Chunk.registry, ['stone', 'oak_leaves', 'red_wool', 'diamond_ore'])
  for (const [cx, cz] of CHUNKS) {
    const file = columnFile(stateDir, WORLD, cx, cz)
    fs.mkdirSync(path.dirname(file), { recursive: true })
    const raw = encodeColumn({ column: buildColumn(Chunk, ids, cx, cz), x: cx, z: cz, t: Date.now(), body: AGENT, mcVersion: MC_VERSION })
    fs.writeFileSync(file, zlib.deflateSync(raw, { level: 1 }))
  }
  writeJson(poseFile(stateDir, AGENT), poseFor(Date.now()))
  writeJson(hudFile(stateDir, AGENT), { v: 1, t: Date.now(), health: 20, food: 20, saturation: 5, oxygen: 20, xp: { level: 0, points: 0, progress: 0 }, effects: [], held: null, inventory: [], window: null })
}
