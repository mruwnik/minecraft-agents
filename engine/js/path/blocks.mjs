// What the planner needs to know about a block state, as flat typed arrays indexed by state id: built once, read in the
// search's inner loop with no allocation. Heights are in 1/16 block above the cell's floor.
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { OFFSET_MAX } from '../offsets.mjs'

export const OPEN = 0 // no collision, not fluid, not hazard
export const SOLID = 1 // has collision
export const WATER = 2
export const LAVA = 3
export const CLIMB = 4 // ladder, vines, scaffolding: the body climbs while its feet are inside
export const OPENABLE = 5 // doors, gates, trapdoors: stage 2 adds the open move; for now their current collision
export const NARROW = 6 // collision that does not cover the cell (posts, panes, bamboo): a walk may not pass through

export const HAZARD_NONE = 0
export const HAZARD_AVOID = 1 // forbidden by default
export const DAMAGE_STAND = 2 // hurts when stood on
export const DAMAGE_TOUCH = 3 // hurts when walked into
export const SLOW = 4 // slows walking

const MC_VERSION = '26.1'

const WATER_NAMES = new Set(['water', 'bubble_column', 'kelp', 'kelp_plant', 'seagrass', 'tall_seagrass'])
const CLIMB_NAMES = /^(ladder|vine|scaffolding|(weeping|twisting)_vines(_plant)?|cave_vines(_plant)?)$/
// iron doors and trapdoors do not open by hand; copper ones do, so they count
const OPENABLE_NAMES = /^(?!iron_)\w+_(door|fence_gate|trapdoor)$/
const TRAPDOOR = /_trapdoor$/
const NARROW_NAMES = /^bamboo$|_pane$|_bars$|(^|_)fence$|_wall$|(^|_)chain$|^end_rod$|lightning_rod$/
const AVOID_NAMES = new Set(['lava', 'fire', 'soul_fire', 'powder_snow', 'cobweb'])
const TOUCH_NAMES = new Set(['sweet_berry_bush', 'wither_rose', 'cactus'])
const SLOW_NAMES = new Set(['soul_sand', 'honey_block'])
const LIT_CAMPFIRES = new Set(['campfire', 'soul_campfire'])

// the direction code a body walks to climb a bottom straight stairs block, as the planner numbers its cardinal moves:
// the high back is on the facing side (facing=north has its tall half at z 0..0.5), so you enter from the opposite side.
// Corner shapes stay 0: their tall part blocks a centred entry.
const STAIR_UP = { east: 1, west: 2, south: 3, north: 4 }
const STAIRS = /_stairs$/

// `climb` per state: 1 the body climbs inside it, 2 an open trapdoor (vanilla counts it as ladder when it sits directly above a
// ladder of the same facing), 3 a closed trapdoor a hand can open (iron ones are 0). `climbName` says which climbable it is.
export const CLIMB_NONE = 0
export const CLIMB_INSIDE = 1
export const CLIMB_TRAP_OPEN = 2
export const CLIMB_TRAP_SHUT = 3
export const LADDER = 1
export const VINES = 2
export const SCAFFOLDING = 3
const FACING = { east: 1, west: 2, south: 3, north: 4 }

const climbOf = (name, props) => {
  if (CLIMB_NAMES.test(name)) return CLIMB_INSIDE
  if (!TRAPDOOR.test(name)) return CLIMB_NONE
  if (props.open === true) return CLIMB_TRAP_OPEN
  return OPENABLE_NAMES.test(name) ? CLIMB_TRAP_SHUT : CLIMB_NONE
}
const climbNameOf = name => name === 'ladder' ? LADDER : name === 'scaffolding' ? SCAFFOLDING : CLIMB_NAMES.test(name) ? VINES : 0

// the data's bamboo box is wrong (0.156..0.344); vanilla is Block.box(6.5, 0, 6.5, 9.5, 16, 9.5), offset per position at lookup
const BAMBOO_BOX = [0.40625, 0, 0.40625, 0.59375, 1, 0.59375]
const WHOLE = 16
const FOOTPRINT = 16 // sample cells per axis for the coverage test; every vanilla shape edge is a multiple of 1/16

// true when the boxes' xz projections leave part of the 1x1 footprint uncovered (cell centres sampled at 1/16)
const leavesGaps = boxes => {
  for (let i = 0; i < FOOTPRINT; i++) {
    for (let j = 0; j < FOOTPRINT; j++) {
      const x = (i + 0.5) / FOOTPRINT
      const z = (j + 0.5) / FOOTPRINT
      if (!boxes.some(b => x > b[0] && x < b[3] && z > b[2] && z < b[5])) return true
    }
  }
  return false
}

const sixteenths = v => Math.round(v * 16)

const kindOf = (name, props, top) => {
  if (WATER_NAMES.has(name) || props.waterlogged === true && top === 0) return WATER
  if (name === 'lava') return LAVA
  if (CLIMB_NAMES.test(name)) return CLIMB
  if (OPENABLE_NAMES.test(name)) return OPENABLE
  if (top > 0 && NARROW_NAMES.test(name)) return NARROW
  return top > 0 ? SOLID : OPEN
}

const hazardOf = (name, props) => {
  if (AVOID_NAMES.has(name)) return HAZARD_AVOID
  if (name === 'magma_block' || LIT_CAMPFIRES.has(name) && props.lit === true) return DAMAGE_STAND
  if (TOUCH_NAMES.has(name)) return DAMAGE_TOUCH
  if (SLOW_NAMES.has(name)) return SLOW
  return HAZARD_NONE
}

export function buildStateTable (registry) {
  const Block = prismarineBlock(registry)
  const size = registry.blocksArray.reduce((m, b) => Math.max(m, b.maxStateId), 0) + 1
  const top = new Uint8Array(size)
  const base = new Uint8Array(size).fill(16)
  const kind = new Uint8Array(size)
  const hazard = new Uint8Array(size)
  const stairUp = new Uint8Array(size)
  const boxStart = new Uint32Array(size)
  const boxCount = new Uint8Array(size)
  const offsetMax = new Float32Array(size)
  const partial = new Uint8Array(size)
  const climb = new Uint8Array(size)
  const climbName = new Uint8Array(size)
  const facing = new Uint8Array(size) // ladder and trapdoor facing: 1 east, 2 west, 3 south, 4 north
  const special = new Uint8Array(size) // partial collision or climbable: the states a search must look closer at near a cell
  const floor = new Uint8Array(size) // height a body can stand on, 1/16 of the cell: the top, but none for a ladder
  const floats = []
  for (const block of registry.blocksArray) {
    for (let id = block.minStateId; id <= block.maxStateId; id++) {
      const state = Block.fromStateId(id, 0)
      // a body inside scaffolding meets nothing: its collision is only for a body standing on top, so it is a floor, not a wall
      const shapes = block.name === 'bamboo' ? [BAMBOO_BOX] : block.name === 'scaffolding' ? [] : state.shapes ?? []
      const props = state.getProperties()
      // the data's shape for snow layers matches the server: (layers - 1) * 2 / 16, so no correction is needed
      top[id] = shapes.reduce((m, s) => Math.max(m, sixteenths(s[4])), 0)
      base[id] = shapes.reduce((m, s) => Math.min(m, sixteenths(s[1])), 16)
      kind[id] = kindOf(block.name, props, top[id])
      hazard[id] = hazardOf(block.name, props)
      boxStart[id] = floats.length / 6
      boxCount[id] = shapes.length
      shapes.forEach(s => floats.push(...s))
      offsetMax[id] = OFFSET_MAX[block.name] ?? 0
      partial[id] = shapes.length > 0 && leavesGaps(shapes) ? 1 : 0
      climb[id] = climbOf(block.name, props)
      climbName[id] = climbNameOf(block.name)
      facing[id] = climbName[id] === LADDER || TRAPDOOR.test(block.name) ? FACING[props.facing] ?? 0 : 0
      floor[id] = block.name === 'scaffolding' ? WHOLE : climbName[id] === 0 ? top[id] : 0
      special[id] = partial[id] | (climb[id] === CLIMB_NONE ? 0 : 1)
      if (STAIRS.test(block.name) && props.half === 'bottom' && props.shape === 'straight') stairUp[id] = STAIR_UP[props.facing] ?? 0
    }
  }
  return { top, base, kind, hazard, stairUp, climb, climbName, facing, floor, special, boxStart, boxCount, boxes: Float32Array.from(floats), offsetMax, partial }
}

let shared
// one table per process: building walks every state in the registry
export const defaultStateTable = () => shared ??= buildStateTable(prismarineRegistry(MC_VERSION))
