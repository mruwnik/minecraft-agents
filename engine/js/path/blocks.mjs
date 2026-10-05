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
export const OPENABLE = 5 // doors, gates, trapdoors: walked as their current collision
export const NARROW = 6 // collision that does not cover the cell (posts, panes, bamboo): a walk may not pass through

export const HAZARD_NONE = 0
export const HAZARD_AVOID = 1 // forbidden by default
export const DAMAGE_STAND = 2 // hurts when stood on
export const DAMAGE_TOUCH = 3 // hurts when walked into
export const SLOW = 4 // slows walking
export const PORTAL = 5 // nether portal, end portal, end gateway: stepping in sends the body elsewhere

const MC_VERSION = '26.1'

const WATER_NAMES = new Set(['water', 'bubble_column', 'kelp', 'kelp_plant', 'seagrass', 'tall_seagrass'])
const CLIMB_NAMES = /^(ladder|vine|scaffolding|(weeping|twisting)_vines(_plant)?|cave_vines(_plant)?)$/
// iron doors and trapdoors do not open by hand; copper ones do, so they count
const OPENABLE_NAMES = /^(?!iron_)\w+_(door|fence_gate|trapdoor)$/
const TRAPDOOR = /_trapdoor$/
const DOOR = /_door$/
const GATE = /_fence_gate$/
const IRON_OPENABLE = /^iron_(door|trapdoor)$/
const BUTTON = /(^|_)button$/
const PLATE = /_pressure_plate$/
const NARROW_NAMES = /^bamboo$|_pane$|_bars$|(^|_)fence$|_wall$|(^|_)chain$|^end_rod$|lightning_rod$/
const AVOID_NAMES = new Set(['lava', 'fire', 'soul_fire', 'powder_snow', 'cobweb'])
const TOUCH_NAMES = new Set(['sweet_berry_bush', 'wither_rose', 'cactus'])
const SLOW_NAMES = new Set(['soul_sand', 'honey_block'])
const PORTAL_NAMES = new Set(['nether_portal', 'end_portal', 'end_gateway'])
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
// `openable` per state: 1 a closed door, gate or trapdoor a hand opens (wood, copper), 2 one only redstone opens (iron), 0 anything
// else (an open one is plain geometry); `openState` is the same block with open=true; `openKind` says which of the three it is.
// `activator`: what a body can use to open an iron door.
export const OPEN_HAND = 1
export const OPEN_REDSTONE = 2
export const KIND_DOOR = 1
export const KIND_GATE = 2
export const KIND_TRAPDOOR = 3
export const ACT_BUTTON = 1
export const ACT_LEVER = 2
export const ACT_PLATE = 3
export const LADDER = 1
export const VINES = 2
export const SCAFFOLDING = 3
const FACING = { east: 1, west: 2, south: 3, north: 4 }
// a wall button on a block's east face has facing=east and sits on the block to its west
const ATTACH_OPPOSITE = { east: 2, west: 1, south: 4, north: 3 }

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
  if (PORTAL_NAMES.has(name)) return PORTAL
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
  const flowing = new Uint8Array(size) // water that is not a source: it pushes the body
  const bubble = new Uint8Array(size) // bubble column: 1 lifts (drag=false, over soul sand), 2 drags down (drag=true, over magma)
  const magma = new Uint8Array(size) // a magma block: a body must not end its route on it
  const dripleaf = new Uint8Array(size) // a big dripleaf leaf with collision: a floor that tilts under a body
  const farmland = new Uint8Array(size) // farmland: a body landing on it from a jump or a fall tramples it to dirt
  const openable = new Uint8Array(size)
  const openState = new Uint32Array(size)
  const openKind = new Uint8Array(size)
  const doorHalf = new Uint8Array(size) // 1 lower half of a door, 2 upper half
  const activator = new Uint8Array(size)
  const attach = new Uint8Array(size) // where a button's or lever's supporting block lies from it: 1 +x, 2 -x, 3 +z, 4 -z, 5 +y, 6 -y
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
      facing[id] = climbName[id] === LADDER || TRAPDOOR.test(block.name) || DOOR.test(block.name) || GATE.test(block.name) ? FACING[props.facing] ?? 0 : 0
      if (OPENABLE_NAMES.test(block.name) || IRON_OPENABLE.test(block.name)) {
        openKind[id] = TRAPDOOR.test(block.name) ? KIND_TRAPDOOR : GATE.test(block.name) ? KIND_GATE : KIND_DOOR
        doorHalf[id] = openKind[id] !== KIND_DOOR ? 0 : props.half === 'upper' ? 2 : 1
        if (props.open === false) {
          openable[id] = IRON_OPENABLE.test(block.name) ? OPEN_REDSTONE : OPEN_HAND
          openState[id] = Block.fromProperties(block.name, { ...props, open: true }, 0).stateId
        }
      }
      activator[id] = BUTTON.test(block.name) ? ACT_BUTTON : block.name === 'lever' ? ACT_LEVER : PLATE.test(block.name) ? ACT_PLATE : 0
      if (activator[id] === ACT_BUTTON || activator[id] === ACT_LEVER) attach[id] = props.face === 'floor' ? 6 : props.face === 'ceiling' ? 5 : ATTACH_OPPOSITE[props.facing] ?? 0
      floor[id] = block.name === 'scaffolding' ? WHOLE : climbName[id] === 0 ? top[id] : 0
      flowing[id] = block.name === 'water' && Number(props.level) !== 0 ? 1 : 0
      bubble[id] = block.name === 'bubble_column' ? (props.drag === true ? 2 : 1) : 0
      magma[id] = block.name === 'magma_block' ? 1 : 0
      farmland[id] = block.name === 'farmland' ? 1 : 0
      // (a magma bubble column is special too: the cells beside it cost risk, which a search only looks for where one is near)
      special[id] = partial[id] | (climb[id] === CLIMB_NONE ? 0 : 1) | (bubble[id] === 2 ? 1 : 0) | (openable[id] > 0 ? 1 : 0)
      if (block.name === 'big_dripleaf' && top[id] > 0) {
        // the leaf's box is 11..15/16 (or lower tilted): a body stands on its top, so for the planner it is a low block like a carpet
        dripleaf[id] = 1
        base[id] = 0
      }
      if (STAIRS.test(block.name) && props.half === 'bottom' && props.shape === 'straight') stairUp[id] = STAIR_UP[props.facing] ?? 0
    }
  }
  return { top, base, kind, hazard, stairUp, climb, climbName, facing, openable, openState, openKind, doorHalf, activator, attach, floor, special, flowing, bubble, magma, dripleaf, farmland, boxStart, boxCount, boxes: Float32Array.from(floats), offsetMax, partial }
}

let shared
// one table per process: building walks every state in the registry
export const defaultStateTable = () => shared ??= buildStateTable(prismarineRegistry(MC_VERSION))
