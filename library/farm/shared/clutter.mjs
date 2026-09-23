// What sits over a farm that its plan never asked for. A plan says what each cell holds: the ground itself at y, and
// what stands on it at y+1 (a crop, a fence, a gate, a chest, a composter, a sapling; a torch on its post at y+2, a
// waterlogged slab laid into a `~` source at y). Anything ELSE in those two cells is clutter: the dirt a walk bridged
// with, the cobblestone a pathfinder towered on, a log from the tree that grew into the field, a stray sapling.
//
// Three things that stand over a plan are never clutter, because clearing them would do harm:
//  - a living crop or its fruit: digging one is harvesting, which is farm.harvest's job. Cane and bamboo stand two and
//    three blocks over their own cell, and a melon lands on the cell BESIDE its stem, often a path cell.
//  - weeds and flowers: farmJobs already clears those off a bed, and they grow straight back. A census that counted
//    them would read `clutter=40` on any field cut out of a meadow and say nothing.
//  - water and lava: `dig` refuses a fluid outright (one such dig ran 167 seconds), and a `~` cell is meant to hold water.
// And two that are somebody's block, so they are reported (`keep`) and left standing: a light, because digging the
// torch out of a field is how it starts spawning mobs at night, and a container or workstation, because breaking a
// chest scatters whatever was inside it over the ground.
import { PLAN_LEGEND, cropNames, STALKS, isAir, isGroundCover, inAnyZone } from '../../../src/lib.mjs'

const LEVELS = [1, 2]
// every crop block, not just the ones with a ripeness: a stem, its fruit and the stalks that stand over their own cell
const CROP_BLOCKS = new Set([...cropNames, ...STALKS, 'melon', 'pumpkin', 'melon_stem', 'pumpkin_stem',
  'attached_melon_stem', 'attached_pumpkin_stem', 'sweet_berry_bush', 'nether_wart', 'torchflower_crop', 'pitcher_crop'])
const FLUIDS = new Set(['water', 'lava', 'flowing_water', 'flowing_lava', 'bubble_column'])
// the flowers a bone-mealed field springs, on top of the grass isGroundCover already names
const FLOWERS = new Set(['dandelion', 'poppy', 'cornflower', 'oxeye_daisy', 'azure_bluet', 'blue_orchid', 'allium',
  'lily_of_the_valley', 'red_tulip', 'orange_tulip', 'white_tulip', 'pink_tulip', 'wither_rose', 'sunflower', 'lilac',
  'rose_bush', 'peony', 'wildflowers', 'bush', 'firefly_bush', 'short_dry_grass', 'tall_dry_grass', 'pink_petals'])
const LIGHTS = /(^|_)(torch|lantern|candle)$|^(campfire|soul_campfire|end_rod|sea_lantern|glowstone|shroomlight|jack_o_lantern)$/
const WORKSTATIONS = new Set(['chest', 'trapped_chest', 'ender_chest', 'barrel', 'hopper', 'dispenser', 'dropper',
  'furnace', 'blast_furnace', 'smoker', 'crafting_table', 'composter', 'brewing_stand', 'enchanting_table', 'cauldron',
  'water_cauldron', 'grindstone', 'loom', 'smithing_table', 'stonecutter', 'cartography_table', 'fletching_table',
  'lectern', 'bell', 'jukebox', 'note_block', 'beacon', 'conduit', 'lodestone', 'respawn_anchor', 'spawner', 'beehive',
  'bee_nest', 'flower_pot', 'decorated_pot', 'armor_stand'])
const KEPT = /_bed$|_sign$|_banner$|_shulker_box$|_head$|_skull$|^shulker_box$/

const kept = name => LIGHTS.test(name) ? 'a light' : (WORKSTATIONS.has(name) || KEPT.test(name)) ? "somebody's block" : null
const weed = name => isGroundCover(name) || FLOWERS.has(name)

// A plan names one wood for a fence, a gate, a slab or a sapling and the world is full of the others: Chani's wheat
// field is fenced in birch, its plan says `#` (oak_fence), and every post of it read as clutter until this.
const KIN = [/_fence_gate$/, /_fence$/, /_slab$/, /_sapling$/]
const sameKind = (want, got) => want === got || KIN.some(r => r.test(want) && r.test(got))

// what the plan itself puts in the cell `dy` blocks over its ground block
const planHolds = (spec, dy, name) => {
  if (spec.crop && name === spec.crop) return true
  if (spec.cover && sameKind(spec.cover, name)) return true
  if (dy !== 1) return spec.kind === 'torch' && LIGHTS.test(name)
  if (spec.kind === 'gate') return name.endsWith('_fence_gate')
  return Boolean(spec.item) && sameKind(spec.item, name)
}

// every block standing over the plan's footprint that the plan does not account for, in plan order (row by row).
// `keep` says why one of them is being left standing rather than cleared.
export function strays (cells, worldAt) {
  const out = []
  for (const cell of cells) {
    const spec = PLAN_LEGEND[cell.ch]
    if (!spec) continue
    for (const dy of LEVELS) {
      const here = worldAt(cell.x, cell.y + dy, cell.z)
      // a cell nobody has loaded is nobody's business: only a block we can actually see is clutter
      if (!here || isAir(here.name)) continue
      const name = here.name
      if (planHolds(spec, dy, name) || CROP_BLOCKS.has(name) || FLUIDS.has(name) || weed(name)) continue
      const why = kept(name)
      out.push({ x: cell.x, y: cell.y + dy, z: cell.z, name, ...(why ? { keep: why } : {}) })
    }
  }
  return out
}

export const clutterBlocks = (cells, worldAt) => strays(cells, worldAt).filter(b => !b.keep)

// commonest kind first, then by name, so the same field always reads the same way
export const clutterKinds = blocks => {
  const counts = {}
  for (const { name } of blocks) counts[name] = (counts[name] ?? 0) + 1
  return Object.entries(counts).sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])).map(([name]) => name).join(',')
}

// how the census says it: `clutter=3(dirt,cobblestone)`, and nothing at all when the field is clean
export const clutterLine = blocks => blocks.length ? `${blocks.length}(${clutterKinds(blocks)})` : undefined

// whose ground a block stands on. Zones are named owner-something and `starter-*` is everybody's, the same rule a
// body picks a bed by: a zone that is not mine is one I do not dig in, whatever is lying in it.
export const foreignZone = (zones, me, pos) => (zones ?? []).find(z =>
  inAnyZone([z], pos) && !new RegExp(`^(${String(me ?? '').toLowerCase()}|starter)-`).test(String(z.name).toLowerCase()))
