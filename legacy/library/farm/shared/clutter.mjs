import { treeSpec, treePart } from '../../../src/tree/inspect.mjs'
import { treeProfile } from '../../../src/tree/profiles.mjs'
// What sits over a farm that its plan never asked for. A plan says what each cell holds: the ground itself at y, and
// what stands on it at y+1 (a crop, a fence, a gate, a chest, a composter, a sapling; a torch on its post at y+2, a
// waterlogged slab laid into a `~` source at y). Anything ELSE in those two cells is clutter: the dirt a walk bridged
// with, the cobblestone a pathfinder towered on, a log from the tree that grew into the field, a stray sapling.
//
// Three things that stand over a plan are never clutter, because clearing them would do harm:
//  - the crop planned for its bed, and crops on non-structure cells: cane and bamboo stand two and three blocks over
//    their own cell, and a melon lands BESIDE its stem, often on a path. A melon on a wheat bed or a crop where a chest belongs is misplaced.
//  - weeds and flowers: farmJobs already clears those off a bed, and they grow straight back. A census that counted
//    them would read `clutter=40` on any field cut out of a meadow and say nothing.
//  - water and lava: `dig` refuses a fluid outright (one such dig ran 167 seconds), and a `~` cell is meant to hold water.
// And two that are somebody's block, so they are reported (`keep`) and left standing: a light, because digging the
// torch out of a field is how it starts spawning mobs at night, and a container or workstation, because breaking a
// chest scatters whatever was inside it over the ground.
import { PLAN_LEGEND, planSpec, planItemMatches, planClaimAt, cropNames, STALKS, isAir, isGroundCover, inAnyZone, harvestOrder } from '../../../src/lib.mjs'
import { planCropMatches } from '../../../src/lib/plan.mjs'
import { recoverFarm } from '../../../src/farm/attention.mjs'

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
// Built structures need their column clear before farmJobs can restore them.
// Plants, paths and unplanned spaces keep the existing fruit/stalk protection.
const structure = spec => spec.kind === 'air' || Boolean(spec.item) && !['flower', 'sapling'].includes(spec.kind)

// A plan names one wood for a fence, a gate, a slab or a sapling and the world is full of the others: Chani's wheat
// field is fenced in birch, its plan says `#` (oak_fence), and every post of it read as clutter until this.
const KIN = [/_fence_gate$/, /_fence$/, /_slab$/, /_sapling$/]
const sameKind = (want, got) => want === got || KIN.some(r => r.test(want) && r.test(got))

// what the plan itself puts in the cell `dy` blocks over its ground block
const planHolds = (spec, dy, name) => {
  if (planCropMatches(spec, name)) return true
  if (spec.cover && sameKind(spec.cover, name)) return true
  if (dy !== 1) return spec.kind === 'torch' && LIGHTS.test(name)
  if (!spec.literal && spec.kind === 'gate') return name.endsWith('_fence_gate')
  return Boolean(spec.item) && planItemMatches(spec, name, sameKind)
}

// every block standing over the plan's footprint that the plan does not account for, in plan order (row by row).
// `keep` says why one of them is being left standing rather than cleared.
export function strays (cells, worldAt) {
  const out = []
  const trees = cells.flatMap(c => { const spec = treeSpec(c); return spec ? [{ ...c, profile: treeProfile(spec.species, spec.form) }] : [] })
  for (const cell of cells) {
    const spec = planSpec(cell)
    if (!spec) continue
    for (const dy of spec.kind === 'air' ? [1] : LEVELS) {
      const here = worldAt(cell.x, cell.y + dy, cell.z)
      // a cell nobody has loaded is nobody's business: only a block we can actually see is clutter
      if (!here || isAir(here.name)) continue
      const name = here.name
      if (planClaimAt(cells, { x: cell.x, y: cell.y + dy, z: cell.z }, cell)) continue
      if (treePart(name) && trees.some(t => Math.abs(cell.x - t.x) <= t.profile.radius && Math.abs(cell.z - t.z) <= t.profile.radius && cell.y + dy <= t.y + t.profile.height)) {
        out.push({ x: cell.x, y: cell.y + dy, z: cell.z, name, keep: 'planned tree: use forestry.maintain or tree.harvest' })
        continue
      }
      if (spec.kind === 'reserved') continue
      if (planHolds(spec, dy, name) || (CROP_BLOCKS.has(name) && spec.kind !== 'crop' && !structure(spec)) || FLUIDS.has(name) || weed(name)) continue
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

// top down, then row by row across the field, so the body sweeps it once instead of criss-crossing it and a stack of
// rubble comes off from the top (gravel and sand under a dug block would otherwise fall into the cell below)
const sweepOrder = blocks => harvestOrder([...blocks].sort((a, b) => b.y - a.y))

// the clearing jobs over these cells, farm.tidy's and farm.maintain's alike: `todo` the strays to dig in sweep order,
// `guarded` those inside a zone that is not `me`'s (with the zone's name), `aside` the lights and somebody's blocks
export const clearJobs = (cells, worldAt, zones, me) => {
  const found = strays(cells, worldAt)
  const loose = found.filter(b => !b.keep).map(b => ({ block: b, zone: foreignZone(zones, me, b) }))
  return {
    todo: sweepOrder(loose.filter(l => !l.zone).map(l => l.block)),
    guarded: loose.filter(l => l.zone).map(l => ({ ...l.block, zone: l.zone.name })),
    aside: found.filter(b => b.keep)
  }
}

// how the left-standing are said: `inZone=chani-farm:2`, `leftAlone=torch@102,72,200 chest@102,72,202`
export const zoneLine = guarded => [...new Set(guarded.map(b => b.zone))].map(z => `${z}:${guarded.filter(b => b.zone === z).length}`).join(' ') || undefined
export const asideLine = aside => aside.map(b => `${b.name}@${b.x},${b.y},${b.z}`).join(' ') || undefined

// walk to each job (`walk(cell)`), dig it, and believe the world, not the click: a dig the server quietly dropped leaves
// the block standing and is not counted. The first job that cannot be walked to or dug stops the round, and is said.
// `pause(cleared)` is the caller's checkpoint between blocks, handed what is cleared so far
export async function clearStrays (api, todo, walk, pause = () => api.checkpoint()) {
  const cleared = []
  for (const block of todo) {
    const where = { x: block.x, y: block.y, z: block.z }
    const failed = await walk(where).then(() => null, recoverFarm(e => e.message)) ??
      await api.act('dig', where).then(() => null, recoverFarm(e => e.message))
    if (failed) return { cleared, stopped: `${block.name} at ${block.x},${block.y},${block.z}: ${failed}` }
    if (api.block(block.x, block.y, block.z)?.name !== block.name) cleared.push(block)
    api.report({ cleared: cleared.length })
    await pause(cleared)
  }
  return { cleared }
}
