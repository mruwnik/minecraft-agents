// Crop bookkeeping: ripeness, replant order and spots, seed sourcing, and a farm's surplus/compost plan.
import { planBill, FARMLAND_CROPS, GENERIC_SEED } from './plan.mjs'

// farming: the seed to replant a ripe crop with, or null when it is not a crop or not ripe yet
const CROPS = { wheat: [7, 'wheat_seeds'], carrots: [7, 'carrot'], potatoes: [7, 'potato'], beetroots: [3, 'beetroot_seeds'], cocoa: [2, 'cocoa_beans'] }
export const ripeCrop = (name, age) => CROPS[name] && Number(age) >= CROPS[name][0] ? CROPS[name][1] : null
export const cropNames = Object.keys(CROPS)

// the replant sweep: the cut cells, in the order they were cut, that the seeds I carry can fill. A place batch that names a seed
// I do not carry throws for the whole batch, so what the pockets cannot cover is left out here and said by the caller
export function replantBatch (cut, carried) {
  const left = { ...carried }
  return cut.filter(c => (left[c.seed] ?? 0) > 0 && left[c.seed]--).map(c => ({ item: c.seed, x: c.x, y: c.y, z: c.z }))
}

// boustrophedon: row by row (z), alternating direction, so the farmer sweeps a field instead of criss-crossing it
export function harvestOrder (positions) {
  const rows = [...new Set(positions.map(p => p.z))].sort((a, b) => a - b)
  return rows.flatMap((z, i) => positions.filter(p => p.z === z).sort((a, b) => i % 2 ? b.x - a.x : a.x - b.x))
}

// farm.find_spot scores a patch of ground the way someone choosing where to farm would: flat first, because every cell
// off the common level is a block to dig or fill, then water (farmland dries without a source within 4), then open sky
// (crops need light), then how far you had to walk. Ground a zone or a saved plan already claims is never a candidate,
// and neither is a patch with a hole in it: tops are the surface of every column, and a null is a lake or a drop.
const commonest = xs => Number(Object.entries(xs.reduce((n, x) => ({ ...n, [x]: (n[x] ?? 0) + 1 }), {}))
  .sort((a, b) => b[1] - a[1] || Number(a[0]) - Number(b[0]))[0][0])

export const spotScore = ({ tops, taken, water, sky, away = 0 }) => {
  if (taken || tops.some(t => t === null || t === undefined)) return null
  const y = commonest(tops)
  const work = tops.reduce((n, t) => n + Math.abs(t - y), 0)
  const level = Math.round(100 * tops.filter(t => t === y).length / tops.length)
  const score = level + (water ? 25 : 0) + (sky ? 15 : 0) - Math.min(40, work) - Math.min(30, Math.round(away / 4))
  return { y, level, work, water: Boolean(water), sky: Boolean(sky), away: Math.round(away), score }
}

export const bestSpots = (scored, limit = 3) => scored.filter(Boolean)
  .sort((a, b) => b.score - a.score || a.away - b.away).slice(0, limit)

// where a harvested crop is planted again, relative to the crop: the block to place against and the face of it. Field crops stand on the
// farmland below; a cocoa pod hangs on the side of a jungle log, and its `facing` points at that log
const FACING = { north: [0, 0, -1], south: [0, 0, 1], west: [-1, 0, 0], east: [1, 0, 0] }
export function replantSpot (name, props) {
  if (name !== 'cocoa') return { against: [0, -1, 0], face: [0, 1, 0] }
  const against = FACING[props.facing]
  // `|| 0`: no -0 in the answer
  return { against, face: against.map(v => -v || 0) }
}

// bamboo and sugar cane: cutting the second segment brings down everything above it, and the base grows back
export const STALKS = ['bamboo', 'sugar_cane']
export const isStalkCut = (block, below, belowThat) => STALKS.includes(block) && below === block && belowThat !== block
// what the wedge reflex dug [{name, below, at:[x,y,z]}] -> the bamboo BASES among it, to plant again: an upper segment regrows, a base never does
export const wedgeReplant = dug => dug.filter(d => d.name === 'bamboo' && d.below !== 'bamboo').map(d => ({ x: d.at[0], y: d.at[1], z: d.at[2], item: 'bamboo' }))

// after cutting stalks: the bases that are gone all the same, and which of them I can plant again from my pockets (the cut itself never takes a base: isStalkCut)
export function stalkReplant (cut, carried) {
  const gone = cut.filter(c => c.baseNow !== c.stalk)
  return { plant: gone.filter(c => carried.includes(c.stalk)).map(c => ({ x: c.base[0], y: c.base[1], z: c.base[2], item: c.stalk })), lost: gone.length }
}

// ---------------------------------------------------------------- composite actions: the farm's produce
// the seed a field is sown from: kept back from the chest, so a farm always carries enough to sow itself again
export const SEED_ITEMS = new Set(['wheat_seeds', 'beetroot_seeds', 'melon_seeds', 'pumpkin_seeds', 'carrot', 'potato', 'sugar_cane', 'bamboo'])
// the seed kept back from the compost and the chest: enough to sow every one of these plans twice over. A homestead
// is several plans and the body carries one pocket: a reserve read off the plan being swept alone had the crop-field
// and cane passes compost every wheat seed the melon patch would need (09-26, 44-53 a day). farm.maintain's
// reserve_for= names the other plans; the routine fills it with $places
export const STALK_RESERVE_MAX = 64
export const seedReserve = (parsedPlans, items = {}) => {
  const keep = {}
  let generic = 0
  for (const parsed of parsedPlans) {
    for (const [item, n] of Object.entries(planBill(parsed))) {
      if (SEED_ITEMS.has(item)) keep[item] = (keep[item] ?? 0) + n * 2
      if (item === GENERIC_SEED) generic += n * 2
    }
  }
  // Cane and bamboo regrow from retained bases. One shared stack per species is
  // spare stock for damaged bases; the rest belongs in configured storage, not
  // sixteen inventory slots kept for sowing an intact 483-cell cane plot twice.
  for (const stalk of STALKS) if (keep[stalk]) keep[stalk] = Math.min(keep[stalk], STALK_RESERVE_MAX)
  // Generic beds share one reserve budget. Round-robin among carried species
  // retains a mix without reserving a whole field's worth of every crop.
  while (generic > 0) {
    const available = FARMLAND_CROPS.map(s => s.seed).filter(seed => (items[seed] ?? 0) > (keep[seed] ?? 0))
    if (!available.length) break
    for (const seed of available) {
      keep[seed] = (keep[seed] ?? 0) + 1
      if (--generic === 0) break
    }
  }
  return keep
}
// what a farm makes. Anything else I carry (tools, armour, cobblestone, the bread I live on) is mine, not the chest's.
const FARM_GOODS = new Set([...SEED_ITEMS, 'wheat', 'beetroot', 'melon_slice', 'melon', 'pumpkin', 'poisonous_potato', 'hay_block', 'cocoa_beans'])
// what belongs in the farm's chest: its produce, above the reserve the plan needs to sow itself again
export function farmSurplus (items, reserve = {}) {
  const out = {}
  for (const [name, count] of Object.entries(items)) {
    if (!FARM_GOODS.has(name)) continue
    const spare = count - (reserve[name] ?? 0)
    if (spare > 0) out[name] = spare
  }
  return out
}

// the seed a farm makes above what it needs to sow itself again is waste: a composter's, not the chest's. Roots
// (carrot, potato) are seed AND food, so they stay produce; the harvest itself is what the chest is for
const WASTE_SEED = new Set(['wheat_seeds', 'beetroot_seeds', 'melon_seeds', 'pumpkin_seeds'])
export const farmWaste = surplus => Object.fromEntries(Object.entries(surplus).filter(([name]) => WASTE_SEED.has(name)))

// where that seed goes: compost=false keeps it with the harvest; compost=<place> or compost=x,y,z names a composter
// or a chest-like block anywhere (no second composter needed to share one); nothing named means the cell the plan
// marks K, if it has one. Marked places are taken as the block itself, not the ground under it
export function seedTarget (planCell, places, arg) {
  if (arg === false) return null
  if (arg === undefined || arg === true) return planCell
  const text = String(arg)
  const nums = text.split(',').map(Number)
  if (nums.length === 3 && nums.every(Number.isFinite)) return { x: nums[0], y: nums[1], z: nums[2] }
  const place = places.find(p => p.name === text)
  if (!place) return { error: `compost=${text} is neither x,y,z nor a marked place: places lists them` }
  return { x: Math.floor(place.x), y: Math.floor(place.y), z: Math.floor(place.z) }
}
// what takes the seed at that cell: a composter is fed (farm.compost), a chest-like block is deposited into
const SEED_BINS = new Set(['chest', 'trapped_chest', 'barrel', 'hopper'])
export const seedDrop = block => block?.name === 'composter' ? 'farm.compost' : SEED_BINS.has(block?.name) || /shulker_box$/.test(block?.name ?? '') ? 'deposit' : null

// ---------------------------------------------------------------- composite actions: composting
// The game's own odds that one item raises a composter by a level; 7 raises fill it and it yields 1 bone meal.
// Anything not here a composter refuses.
const COMPOST_GROUPS = [
  [0.3, ['wheat_seeds', 'beetroot_seeds', 'melon_seeds', 'pumpkin_seeds', 'torchflower_seeds', 'pitcher_pod', 'short_grass', 'tall_grass', 'fern', 'large_fern', 'seagrass', 'kelp', 'dried_kelp', 'oak_leaves', 'birch_leaves', 'spruce_leaves', 'jungle_leaves', 'acacia_leaves', 'dark_oak_leaves', 'cherry_leaves', 'mangrove_leaves', 'azalea_leaves', 'oak_sapling', 'birch_sapling', 'spruce_sapling', 'jungle_sapling', 'acacia_sapling', 'dark_oak_sapling', 'cherry_sapling', 'mangrove_propagule', 'sweet_berries', 'glow_berries', 'hanging_roots', 'moss_carpet', 'small_dripleaf', 'dead_bush']],
  [0.5, ['sugar_cane', 'cactus', 'vine', 'melon_slice', 'pumpkin', 'carved_pumpkin', 'nether_wart', 'glow_lichen', 'tall_seagrass', 'big_dripleaf', 'lily_pad']],
  [0.65, ['apple', 'beetroot', 'carrot', 'potato', 'wheat', 'cocoa_beans', 'melon', 'brown_mushroom', 'red_mushroom', 'sea_pickle', 'moss_block', 'pink_petals', 'bamboo', 'nether_sprouts']],
  [0.85, ['bread', 'cookie', 'baked_potato', 'hay_block', 'brown_mushroom_block', 'red_mushroom_block', 'nether_wart_block', 'warped_wart_block', 'waterlily']],
  [1, ['cake', 'pumpkin_pie']]
]
export const COMPOST_CHANCE = Object.fromEntries(COMPOST_GROUPS.flatMap(([chance, items]) => items.map(name => [name, chance])))
// a composter eats bread as happily as a seed: my own food is never fed to it unless it is asked for by name
// (a sweep of the farm put all 7 loaves in the composter, 22:1)
const COMPOST_FOOD = new Set(['bread', 'cookie', 'baked_potato', 'apple', 'sweet_berries', 'glow_berries', 'melon_slice', 'cake', 'pumpkin_pie', 'beetroot', 'carrot', 'potato'])

// items= as an object, a list or a bare name; a count of 'all' or none means whatever I carry
const wantedCounts = want => {
  if (!want) return null
  if (typeof want === 'string') return { [want]: Infinity }
  const rows = Array.isArray(want) ? want.map(w => [w.name, w.count]) : Object.entries(want)
  return Object.fromEntries(rows.map(([name, count]) => [name, count === undefined || count === 'all' ? Infinity : count]))
}

// what to feed a composter: what I asked for, or everything compostable I carry that is not seed I need to sow again
export function compostPlan (items, { want, keep = {} } = {}) {
  const asked = wantedCounts(want)
  const feed = []
  const skipped = []
  for (const [name, wanted] of Object.entries(asked ?? items)) {
    const chance = COMPOST_CHANCE[name]
    // with no items= every slot is offered, so saying 'a composter will not take it' about each tool is noise: only answer for what was asked for
    if (!chance) {
      if (asked) skipped.push({ name, why: 'a composter will not take it' })
      continue
    }
    const held = items[name] ?? 0
    const heldBack = SEED_ITEMS.has(name) ? 'kept to sow the fields again' : COMPOST_FOOD.has(name) ? 'food, not compost' : null
    const reserve = keep[name] ?? (!asked && heldBack ? held : 0)
    const count = Math.min(asked ? wanted : held, held - reserve)
    if (count <= 0) { skipped.push({ name, why: asked ? `you carry ${held}` : heldBack ?? 'none to spare' }); continue }
    feed.push({ name, count, chance })
  }
  return { feed, skipped }
}

// ---------------------------------------------------------------- where seed comes from
// Every crop has one renewable source. Wheat seed falls out of grass; cane and bamboo are cut above the base so the
// stand lives; melons and pumpkins are cut off the stem; roots are never found wild, so they come out of a farm chest.
const SEED_SOURCES = {
  wheat: { from: 'grass', block: 'short_grass', item: 'wheat_seeds', advice: 'break grass until the seed adds up; tall grass drops it too' },
  carrot: { from: 'chest', block: 'carrots', item: 'carrot', advice: 'carrots never drop from grass: take some from a farm chest, a village plot or a trade' },
  potato: { from: 'chest', block: 'potatoes', item: 'potato', advice: 'potatoes never drop from grass: take some from a farm chest, a village plot or a trade' },
  beetroot: { from: 'chest', block: 'beetroots', item: 'beetroot_seeds', advice: 'beetroot seed never drops from grass: take some from a farm chest or a village plot' },
  sugar_cane: { from: 'stalk', block: 'sugar_cane', item: 'sugar_cane', advice: 'cut the stand above its base so it grows back' },
  bamboo: { from: 'stalk', block: 'bamboo', item: 'bamboo', advice: 'cut the stand above its base so it grows back' },
  melon: { from: 'wild', block: 'melon', item: 'melon_slice', advice: 'cut the fruit and leave the stem: it grows another' },
  pumpkin: { from: 'wild', block: 'pumpkin', item: 'pumpkin', advice: 'cut the fruit and leave the stem: it grows another' }
}
const CROP_ALIASES = {
  potatoes: 'potato', carrots: 'carrot', beetroots: 'beetroot', beets: 'beetroot', melons: 'melon', pumpkins: 'pumpkin',
  wheat_seeds: 'wheat', beetroot_seeds: 'beetroot', melon_seeds: 'melon', pumpkin_seeds: 'pumpkin',
  sugarcane: 'sugar_cane', cane: 'sugar_cane'
}
export const seedSource = crop => SEED_SOURCES[CROP_ALIASES[crop] ?? crop] ?? null

// ---------------------------------------------------------------- what a sweep leaves bare, and why
// The human, 09-26: an all-wheat plan stood mostly bare while its harvest was replanted day after day. The body carried
// no hoe: the first till failed, the seed thrown on that dirt failed, the second till failed the same way and the
// runner's "twice in a row" ended the sweep before one plant job on ready farmland had run, with nothing in the day's
// summary but stopped=. So a sweep never tries a till it has no hoe for, and counts every planned crop cell it leaves
// empty under one of these words: unfilled (a bed whose ground is gone and nothing filled it: src/lib/fill.mjs),
// dry (no water within 4 yet: the till is held until the channel holds water), untilled (no hoe, or a bed the hoe
// could not work), no seed, unreachable (nothing to stand on within work range), water (standing on the bed),
// failed (anything else the place primitive said)
export const NO_HOE = 'no hoe: craft item=wooden_hoe (2 planks + 2 sticks)'
// the till primitive takes any hoe: an item whose name ends in _hoe
export const hasHoe = items => Object.keys(items ?? {}).some(name => name.endsWith('_hoe'))
// a plant job's failure in the summary's word: nowhere to stand or walk to is unreachable, anything else failed
export const bareWhy = message => /nowhere to stand|no cell to stand|no walkable path|no path/.test(String(message)) ? 'unreachable' : 'failed'
const BARE_ORDER = ['unfilled', 'dry', 'untilled', 'no seed', 'unreachable', 'water', 'failed']
// one line for bare=: the count, then each reason with its count and its notes (a tool to craft, the seed short, the cells)
export function bareLine (entries) {
  if (!entries.length) return null
  const groups = new Map()
  for (const { why, note } of entries) {
    const group = groups.get(why) ?? { n: 0, notes: [] }
    group.n++
    if (note && !group.notes.includes(note)) group.notes.push(note)
    groups.set(why, group)
  }
  const said = [...groups].sort((a, b) => BARE_ORDER.indexOf(a[0]) - BARE_ORDER.indexOf(b[0]))
    .map(([why, { n, notes }]) => `${why}:${n}${notes.length ? ` ${notes.join(', ')}` : ''}`)
  return `${entries.length} (${said.join('; ')})`
}
