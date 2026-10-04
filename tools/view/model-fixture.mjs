// Why JavaScript: test data for the WebGL jar-model pixel checks (JS view stack).
// Worlds for the pixel checks of the jar-modelled blocks (tools/view-web-check.mjs --models):
//  MODELS: a row of blocks the old renderer draws wrong (leaf litter, a stair, a fence, two dispensers, a grass block), each with a red wool
//          wall right behind it, so a gap in the shape shows red.
//  CUBES:  only full cubes (stone, ore, planks, logs on three axes, leaves, glass): the fast path, which must look the same with and without the jar.
import { writeWorld, MC_VERSION } from './fixture.mjs'

export const AGENT = 'Models'
const FLOOR_Y = 64
const ROW_Z = 8
const PIECE_Y = 65
export const EYE = { x: 7, y: 66.1, z: 15.5 } // 70 degrees wide: x 2.5..11.5 at the row's distance

// x of each piece (cell), the cell's block key
const PIECES = {
  litter: { x: 4, key: 'litter' },
  stairs: { x: 5, key: 'stairs' },
  fence: { x: 6, key: 'fence' },
  dispenserSouth: { x: 7, key: 'dispenser_south' },
  dispenserNorth: { x: 8, key: 'dispenser_north' },
  grass: { x: 9, key: 'grass' }
}

export const MODEL_STATES = {
  litter: { name: 'leaf_litter', props: { segment_amount: 4, facing: 'north' } },
  stairs: { name: 'oak_stairs', props: { facing: 'east', half: 'bottom', shape: 'straight', waterlogged: false } },
  fence: { name: 'oak_fence', props: { north: false, east: true, south: false, west: true, waterlogged: false } },
  dispenser_south: { name: 'dispenser', props: { facing: 'south', triggered: false } },
  dispenser_north: { name: 'dispenser', props: { facing: 'north', triggered: false } },
  grass: { name: 'grass_block', props: { snowy: false } }
}

const within = (v, [lo, hi]) => v >= lo && v <= hi

const modelBlockAt = (x, y, z) => {
  if (x < 0 || x > 15 || z < 0 || z > 15) return 'air'
  if (y === FLOOR_Y) return 'stone'
  if (y === PIECE_Y && z === ROW_Z) return Object.values(PIECES).find(p => p.x === x)?.key ?? 'air'
  if (z === ROW_Z - 1 && within(x, [1, 13]) && within(y, [PIECE_Y, PIECE_Y + 2])) return 'red_wool'
  return 'air'
}

const skyLight = () => ({ sky: 15, block: 0 })
const OPAQUE_LIT = new Set(['air', 'litter', 'stairs', 'fence', 'grass', 'dispenser_south', 'dispenser_north'])
const modelLight = (x, y, z) => OPAQUE_LIT.has(modelBlockAt(x, y, z)) ? skyLight() : { sky: 0, block: 0 }

const cubeBlockAt = (x, y, z) => {
  if (x < 0 || x > 15 || z < 0 || z > 15) return 'air'
  if (y === FLOOR_Y) return 'stone'
  if (z !== ROW_Z || !within(y, [PIECE_Y, PIECE_Y + 2])) return 'air'
  return ['stone', 'diamond_ore', 'oak_planks', 'oak_log', 'cobblestone', 'spruce_planks', 'oak_leaves', 'glass', 'furnace', 'bricks'][(x + y) % 10] ?? 'air'
}
export const CUBE_STATES = {
  oak_log: { name: 'oak_log', props: { axis: 'y' } }, // the old renderer mirrors a log lying on x or z, so only the upright one is the same
}
const cubeLight = (x, y, z) => cubeBlockAt(x, y, z) === 'air' ? skyLight() : { sky: 0, block: 0 }

const face = (x, y, z) => ({ axis: 'z', at: ROW_Z + 1, x, y })
const cell = key => PIECES[key].x
// regions: the south faces (z = 9) of the pieces, in block coordinates
export const MODEL_REGIONS = [
  // 1 segment of litter fills only a corner; 4 cover the cell: its top face seen from the eye (13 degrees above it)
  { name: 'litter top', face: { axis: 'y', at: PIECE_Y + 1 / 16, x: [cell('litter') + 0.1, cell('litter') + 0.9], z: [ROW_Z + 0.1, ROW_Z + 0.9] } },
  { name: 'litter above', face: face([cell('litter') + 0.1, cell('litter') + 0.9], [PIECE_Y + 0.3, PIECE_Y + 0.9]) },
  // the stair faces east: its west half is only a slab high, the notch shows the wool behind
  { name: 'stair notch', face: face([cell('stairs') + 0.05, cell('stairs') + 0.4], [PIECE_Y + 0.62, PIECE_Y + 0.95]) },
  // the arms run along x at y 6..9 and 12..15 (sixteenths); between them, away from the post at 6..10, is air
  { name: 'fence gap', face: face([cell('fence') + 0.05, cell('fence') + 0.3], [PIECE_Y + 0.62, PIECE_Y + 0.72]) },
  { name: 'fence arm', face: face([cell('fence') + 0.05, cell('fence') + 0.3], [PIECE_Y + 0.42, PIECE_Y + 0.52]) },
  { name: 'dispenser front', face: face([cell('dispenserSouth') + 0.05, cell('dispenserSouth') + 0.95], [PIECE_Y + 0.05, PIECE_Y + 0.95]) },
  { name: 'dispenser side', face: face([cell('dispenserNorth') + 0.05, cell('dispenserNorth') + 0.95], [PIECE_Y + 0.05, PIECE_Y + 0.95]) },
  // the top sixth of the grass block's side: the overlay, tinted
  { name: 'grass fringe', face: face([cell('grass') + 0.05, cell('grass') + 0.95], [PIECE_Y + 0.82, PIECE_Y + 0.98]) }
]

// GRAZING: a field for the frame-time budget, seen at a low angle: grass blocks with tall grass, bamboo, kelp and a fence wall on top
const hash = (x, z) => ((Math.imul(x + 101, 73856093) ^ Math.imul(z + 57, 19349663)) >>> 0) % 100
const FENCE_Z = 0
export const GRAZING_STATES = {
  grass_block: { name: 'grass_block', props: { snowy: false } },
  tall_lower: { name: 'tall_grass', props: { half: 'lower' } },
  tall_upper: { name: 'tall_grass', props: { half: 'upper' } },
  bamboo: { name: 'bamboo', props: { age: 0, leaves: 'large', stage: 0 } },
  fence: { name: 'oak_fence', props: { north: false, east: true, south: false, west: true, waterlogged: false } },
  kelp: { name: 'kelp_plant', props: {} }
}
const grazingBlockAt = (x, y, z) => {
  if (x < -16 || x > 15 || z < -16 || z > 15) return 'air'
  if (y === FLOOR_Y) return 'grass_block'
  if (y < PIECE_Y || y > PIECE_Y + 2) return 'air'
  if (z > 10) return 'air' // clear around the eye
  if (z === FENCE_Z) return y <= PIECE_Y + 1 ? 'fence' : 'air'
  const h = hash(x, z)
  if (h < 45) return y === PIECE_Y ? 'tall_lower' : y === PIECE_Y + 1 ? 'tall_upper' : 'air'
  if (h < 55) return 'bamboo'
  if (h < 62) return 'kelp'
  return 'air'
}
export const GRAZING_EYE = { x: 0.5, y: 65.7, z: 14.5 }

export const MODELS = { agent: AGENT, regions: MODEL_REGIONS }
export const CUBES = { agent: AGENT, regions: [{ name: 'cubes', face: face([3, 12], [PIECE_Y, PIECE_Y + 3]) }] }

export const writeModelWorld = (stateDir, which) => which === 'grazing' ? writeWorld({
  stateDir,
  agent: AGENT,
  camera: { eye: GRAZING_EYE, yaw: 0, pitch: -0.02 },
  blockAt: grazingBlockAt,
  light: () => skyLight(),
  states: GRAZING_STATES,
  keys: Object.keys(GRAZING_STATES)
}) : writeWorld({
  stateDir,
  agent: AGENT,
  camera: { eye: EYE, yaw: 0, pitch: 0 },
  blockAt: which === 'cubes' ? cubeBlockAt : modelBlockAt,
  light: which === 'cubes' ? cubeLight : modelLight,
  states: which === 'cubes' ? CUBE_STATES : MODEL_STATES,
  keys: which === 'cubes'
    ? ['stone', 'diamond_ore', 'oak_planks', 'oak_log', 'cobblestone', 'spruce_planks', 'oak_leaves', 'glass', 'furnace', 'bricks']
    : [...Object.keys(MODEL_STATES), 'red_wool', 'stone']
})
export { MC_VERSION }
