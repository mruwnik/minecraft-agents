// What the planner needs to know about a block state, as flat typed arrays indexed by state id: built once, read in the
// search's inner loop with no allocation. Heights are in 1/16 block above the cell's floor.
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'

export const OPEN = 0 // no collision, not fluid, not hazard
export const SOLID = 1 // has collision
export const WATER = 2
export const LAVA = 3
export const CLIMB = 4 // ladder, vines, scaffolding: marked now, moves come in stage 2
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
const NARROW_NAMES = /^bamboo$|_pane$|_bars$|(^|_)fence$|_wall$|(^|_)chain$|^end_rod$|lightning_rod$/
const AVOID_NAMES = new Set(['lava', 'fire', 'soul_fire', 'powder_snow', 'cobweb'])
const TOUCH_NAMES = new Set(['sweet_berry_bush', 'wither_rose', 'cactus'])
const SLOW_NAMES = new Set(['soul_sand', 'honey_block'])
const LIT_CAMPFIRES = new Set(['campfire', 'soul_campfire'])

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
  for (const block of registry.blocksArray) {
    for (let id = block.minStateId; id <= block.maxStateId; id++) {
      const state = Block.fromStateId(id, 0)
      const shapes = state.shapes ?? []
      const props = state.getProperties()
      // the data's shape for snow layers matches the server: (layers - 1) * 2 / 16, so no correction is needed
      top[id] = shapes.reduce((m, s) => Math.max(m, sixteenths(s[4])), 0)
      base[id] = shapes.reduce((m, s) => Math.min(m, sixteenths(s[1])), 16)
      kind[id] = kindOf(block.name, props, top[id])
      hazard[id] = hazardOf(block.name, props)
    }
  }
  return { top, base, kind, hazard }
}

let shared
// one table per process: building walks every state in the registry
export const defaultStateTable = () => shared ??= buildStateTable(prismarineRegistry(MC_VERSION))
