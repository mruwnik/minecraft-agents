// Why JavaScript: test data for the WebGL no-ghost pixel check (JS view stack).
// A two-place world for the no-ghost check (tools/view-web-check.mjs --ghost). At radius 1 the window is 3x3 columns and a
// column lives in slot (cx mod 3, cz mod 3); the places A (chunk 10,10) and B (chunk 130,130) are 120 chunks apart and
// their windows share every slot, so a slot that kept A's voxels or light when the view jumps to B would show A inside B.
//   A: stone floor, tall stone walls to the east, a torch-lit patch on the floor.
//   B: sand floor with low pillars along the east edge of the middle column, whose east neighbour column and a corner column
//      are missing on disk (the view crosses a loaded/unloaded border; smooth light and AO look into the neighbour slot).
import fs from 'node:fs'
import path from 'node:path'
import zlib from 'node:zlib'
import { encodeColumn, columnFile, poseFile, hudFile, LIGHT_SECTION_BYTES } from '../../engine/js/view.mjs'
import { makeChunkClass } from './columns.mjs'

export const MC_VERSION = '26.1'
export const WORLD = 'ghost'
export const AGENT = 'Ghost'
const MIN_Y = -64
const WORLD_HEIGHT = 384
const FLOOR_Y = 64

export const PLACES = { A: { cx: 10, cz: 10 }, B: { cx: 130, cz: 130 } }
const MISSING_B = new Set(['131.129', '131.130', '131.131', '129.129'])

const within = (v, lo, hi) => v >= lo && v <= hi
const inChunk = (place, x, z) => Math.floor(x / 16) - place.cx
const placeOf = (x, z) => Object.values(PLACES).find(p => Math.abs(Math.floor(x / 16) - p.cx) <= 1 && Math.abs(Math.floor(z / 16) - p.cz) <= 1)

export const blockNameAt = (x, y, z) => {
  const place = placeOf(x, z)
  if (!place) return 'air'
  const lx = x - place.cx * 16
  const lz = z - place.cz * 16
  if (place === PLACES.A) {
    if (y === FLOOR_Y) return 'stone'
    if (inChunk(place, x, z) === 1 && within(lx, 8, 9) && within(y, 65, 90)) return 'stone'
    if (within(lx, 10, 11) && within(lz, 4, 5) && within(y, 65, 70)) return 'stone'
    return 'air'
  }
  if (y === FLOOR_Y) return 'sand'
  if (inChunk(place, x, z) === 0 && within(lx, 12, 15) && lz % 4 === 0 && within(y, 65, 67)) return 'sand'
  return 'air'
}

export const lightAt = (x, y, z) => {
  if (blockNameAt(x, y, z) !== 'air') return { sky: 0, block: 0 }
  const place = placeOf(x, z)
  const lx = x - place.cx * 16
  const lz = z - place.cz * 16
  if (place === PLACES.A && y === 65 && within(lx, 12, 15) && within(lz, 6, 9)) return { sky: 0, block: 14 } // the torch patch
  if (place === PLACES.A && inChunk(place, x, z) === 1 && within(lx, 6, 7) && within(y, 65, 90)) return { sky: 4, block: 0 } // in the walls' shade
  return { sky: 15, block: 0 }
}

// the eye is in the middle of the window's middle column, looking east (+x, yaw -pi/2)
export const eyeOf = place => ({ x: place.cx * 16 + 8, y: 66.62, z: place.cz * 16 + 8 })
export const YAW = -Math.PI / 2

// the column's light dump in vanilla nibble order, as tools/view/fixture.mjs writes it
const lightDump = (cx, cz) => {
  const sections = WORLD_HEIGHT >> 4
  const buffers = key => Array.from({ length: sections }, (_, s) => {
    const buffer = Buffer.alloc(LIGHT_SECTION_BYTES)
    for (let i = 0; i < 4096; i++) {
      const light = lightAt(cx * 16 + (i & 15), MIN_Y + s * 16 + (i >> 8), cz * 16 + ((i >> 4) & 15))
      buffer[i >> 1] |= light[key] << ((i & 1) * 4)
    }
    return buffer
  })
  const mask = [[0, 2 ** (sections + 1) - 2]]
  return { skyLight: buffers('sky'), blockLight: buffers('block'), skyLightMask: mask, blockLightMask: mask, emptySkyLightMask: [[0, 0]], emptyBlockLightMask: [[0, 0]] }
}

const buildColumn = (Chunk, ids, cx, cz) => {
  const column = new Chunk({ minY: MIN_Y, worldHeight: WORLD_HEIGHT })
  for (let lx = 0; lx < 16; lx++) {
    for (let lz = 0; lz < 16; lz++) {
      for (let y = FLOOR_Y; y <= 90; y++) {
        const name = blockNameAt(cx * 16 + lx, y, cz * 16 + lz)
        if (name !== 'air') column.setBlockStateId({ x: lx, y, z: lz }, ids[name])
      }
    }
  }
  column.dumpLight = () => lightDump(cx, cz)
  return column
}

export const poseAt = (place, now) => {
  const eye = eyeOf(place)
  return {
    v: 1, t: now, world: WORLD, status: 'online', dimension: 'overworld', mcVersion: MC_VERSION,
    pos: { x: eye.x, y: 65, z: eye.z }, eye, yaw: YAW, pitch: 0,
    velocity: { x: 0, y: 0, z: 0 }, onGround: true, entities: [], timeOfDay: 6000, rain: 0
  }
}

export const writePose = (stateDir, place) => {
  const file = poseFile(stateDir, WORLD, AGENT)
  fs.mkdirSync(path.dirname(file), { recursive: true })
  const tmp = `${file}.tmp`
  fs.writeFileSync(tmp, JSON.stringify(poseAt(place, Date.now())))
  fs.renameSync(tmp, file)
}

export const writeGhostWorld = (stateDir, start = PLACES.A) => {
  const Chunk = makeChunkClass(MC_VERSION)
  const ids = Object.fromEntries(['stone', 'sand'].map(name => [name, Chunk.registry.blocksByName[name].defaultState]))
  for (const place of Object.values(PLACES)) {
    for (let cx = place.cx - 1; cx <= place.cx + 1; cx++) {
      for (let cz = place.cz - 1; cz <= place.cz + 1; cz++) {
        if (MISSING_B.has(`${cx}.${cz}`)) continue
        const file = columnFile(stateDir, WORLD, cx, cz)
        fs.mkdirSync(path.dirname(file), { recursive: true })
        const raw = encodeColumn({ column: buildColumn(Chunk, ids, cx, cz), x: cx, z: cz, t: Date.now(), body: AGENT, mcVersion: MC_VERSION })
        fs.writeFileSync(file, zlib.deflateSync(raw, { level: 1 }))
      }
    }
  }
  writePose(stateDir, start)
  fs.writeFileSync(hudFile(stateDir, WORLD, AGENT), JSON.stringify({ v: 1, t: Date.now(), health: 20, food: 20, saturation: 5, oxygen: 20, xp: { level: 0, points: 0, progress: 0 }, effects: [], held: null, inventory: [], window: null }))
}
