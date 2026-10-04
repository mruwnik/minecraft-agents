// Placing blocks: what a click lands against, refusals (obstacles, stray fluid), and scaffold bookkeeping.

import { isAir } from './world.mjs'
// can a block be placed at `block` without walking? Within reach of the eyes, and not where the body (0.6 wide, 1.8 tall) is.
export function canPlaceFromHere (feet, block, reach = 4) {
  const overlaps = (lo, hi, cell) => lo < cell + 1 && hi > cell
  const inBody = overlaps(feet.x - 0.3, feet.x + 0.3, block.x) && overlaps(feet.z - 0.3, feet.z + 0.3, block.z) && overlaps(feet.y, feet.y + 1.8, block.y)
  if (inBody) return false
  return Math.hypot(block.x + 0.5 - feet.x, block.y + 0.5 - (feet.y + 1.62), block.z + 0.5 - feet.z) <= reach
}

// what `place` should do about the block already at a target: leaves in a forest are cleared, anything else solid is somebody's
export function occupiedBy (existing, wanted) {
  if (!existing || existing.boundingBox !== 'block') return 'free'
  if (existing.name === wanted) return 'skip'
  return /_leaves$/.test(existing.name) ? 'clear' : 'blocked'
}

// what to take from a chest and what it cannot give: a withdraw that silently skips a missing item reads as success
// a right-click on these opens or uses them instead of placing a block against them
const CLICKABLE = /(_bed|_door|_trapdoor|_fence_gate|_button|chest|barrel|shulker_box|crafting_table|furnace|smoker|hopper|dispenser|dropper|anvil|lever|loom|stonecutter|grindstone|smithing_table|cartography_table|brewing_stand|enchanting_table|beacon|note_block|repeater|comparator|composter|cauldron)$/

// which neighbour of a free spot to place against: a plain solid one if there is any, else a clickable one while sneaking
export function placeAgainst (neighbours) {
  const solid = neighbours.map((b, index) => ({ b, index })).filter(n => n.b?.boundingBox === 'block')
  const plain = solid.find(n => !CLICKABLE.test(n.b.name))
  if (plain) return { index: plain.index, sneak: false }
  return solid.length ? { index: solid[0].index, sneak: true } : null
}

// what a `place` run reports. Cells it could not reach or attach are skipped, not fatal: one awkward cell used to end a 60-block list
// Did the block really land? mineflayer's placeBlock resolves as soon as the server answers anything, and a placement the
// server dislikes is dropped without a word: the cell must have CHANGED (a seed lands as a crop, so its name is no help).
export const placeMissed = (before, after) => after === before
  ? 'the server dropped it without a word (something in the way, or you moved out of reach): nothing was placed'
  : null

// Where did the bucket really empty? A click that misses pours at the player's own eye level instead, and the flood then
// breaks every crop it runs over. `cells` is what stands around me now, `before` the keys of the sources that were
// already there: what is new, and is a source (level 0), is the one to take back.
export const strayFluid = (before, cells, fluid) =>
  cells.find(c => c.name === fluid && Number(c.level) === 0 && !before.has(`${c.x},${c.y},${c.z}`)) ?? null

// cells: what really stands there now, read back off the world, {x,y,z,name}. The @x,y,z of any reply is where the BODY
// stands, and AhuraMazda took it for the block he had just placed: he dug what he thought was his own bed remnant and hit
// someone else's pressure plate. Grouped by block, because a batch usually lays one kind
export const placedAt = cells => {
  if (!cells.length) return null
  const kinds = [...new Set(cells.map(c => c.name))]
  return kinds.map(kind => {
    const mine = cells.filter(c => c.name === kind).map(c => `${c.x},${c.y},${c.z}`)
    const shown = mine.slice(0, 6).join(' ')
    return `${shown}${mine.length > 6 ? ` and ${mine.length - 6} more` : ''} (${kind})`
  }).join(' ')
}

export function placeOutcome (placed, skipped, verb = 'placed', already = 0, cells = []) {
  // one line per different reason: a batch that hit solid ground AND had nothing to attach to showed only the first, and the second stayed a riddle
  const reasons = [...new Set(skipped.map(s => s.why))]
  const why = reasons.map(r => `${skipped.filter(s => s.why === r).length} ${r} (first ${skipped.find(s => s.why === r).at})`).join('; ')
  if (!placed && why) return { error: `${verb} nothing: ${why}${already ? `; ${already} were already there` : ''}` }
  const there = already ? { alreadyThere: already } : {}
  const at = placedAt(cells) ? { at: placedAt(cells) } : {}
  return why ? { [verb]: placed, skipped: skipped.length, why, ...there, ...at } : { [verb]: placed, ...there, ...at }
}

// place blocks=[...] item=<default>: entries that name no item of their own take the default
export const dryCells = (tilled, waters) => tilled.filter(([x, y, z]) => !waters.some(([wx, wy, wz]) => Math.abs(wx - x) <= 4 && Math.abs(wz - z) <= 4 && (wy === y || wy === y + 1))).map(c => c.join(','))
export const withDefaultItem = (blocks, item) => blocks.map(b => b.item || item === undefined ? b : { ...b, item })

// `fill` answered ok with an empty bucket (standing in the source, the click goes elsewhere)
export const fillOutcome = held => /_bucket$/.test(held ?? '')
  ? { holding: held }
  : { error: 'the bucket is still empty: stand on the shore 1-2 blocks from the source with a clear view of it, not in the water, and fill again' }

// `place` blamed reach ("the server refused it") when the spot itself was the trouble: a seed on dried-out farmland, a carrot on a
// tile that had one. Three agents lost many retries to that. null = nothing in the way that I know of
const GIVES_WAY = /^(air|cave_air|void_air|water|lava|short_grass|tall_grass|fern|large_fern|dead_bush|snow|fire|vine|seagrass|tall_seagrass|leaf_litter|glow_lichen|hanging_roots|nether_sprouts|crimson_roots|warped_roots|light|bubble_column|structure_void)$/
const ON_FARMLAND = /^(wheat_seeds|beetroot_seeds|melon_seeds|pumpkin_seeds|carrot|potato|torchflower_seeds|pitcher_pod)$/
const SOIL = /^(dirt|grass_block|coarse_dirt|podzol|rooted_dirt|moss_block|mud|farmland|mycelium)$/
export function placeObstacle (item, existing, below) {
  if (!GIVES_WAY.test(existing)) return `${existing} is already there (not a block, but in the way): harvest or dig it first`
  if (ON_FARMLAND.test(item) && below !== 'farmland') return `${item} needs farmland under it, and there is ${below}: till that block first`
  if (/_sapling$/.test(item) && !SOIL.test(below)) return `${item} needs dirt or grass under it, and there is ${below}`
  return null
}

// The pathfinder climbs and bridges with any placeable block it carries, and spent Chani's cobblestone twice without a word.
// cells: what a walk built, {x,y,z,name}. feet: where the body stands now. Which of them it can take back without dropping itself
export const scaffoldTakeBack = (cells, feet) => cells.filter(c =>
  !(c.x === Math.floor(feet.x) && c.z === Math.floor(feet.z)) &&
  Math.abs(c.y - Math.floor(feet.y)) <= 3 &&
  Math.hypot(c.x + 0.5 - feet.x, c.z + 0.5 - feet.z) <= 4.5)

// tried: every cell the pathfinder aimed a placement at (it retries one cell several times a tick, and its own place call
// rejects over blocks the server did put down). Recover only the recorded material;
// a replanted sapling or another replacement is no longer an owned support.
export const scaffoldBuilt = (tried, nameAt) => {
  const seen = new Set()
  return tried.filter(c => {
    const key = `${c.x},${c.y},${c.z}`
    if (seen.has(key)) return false
    seen.add(key)
    return true
  }).filter(c => c.name && !isAir(c.name) && nameAt(c) === c.name)
}

const countLine = counts => Object.entries(counts).map(([name, n]) => `${name}:${n}`).join(' ')

// spent/taken: {cobblestone: 4}. left: the cells still standing. Reported beside the drops, so vanishing stone has a reason
export const scaffoldNote = (spent, taken, left) => {
  if (!Object.keys(spent).length) return null
  const back = Object.keys(taken).length ? `dug back ${countLine(taken)} (a drop that falls off a height is left below)` : 'dug none back'
  const where = left.length
    ? `; ${left.length} still ${left.length > 1 ? 'stand' : 'stands'} at ${left.slice(0, 6).map(c => `${c.x},${c.y},${c.z}`).join(' ')}${left.length > 6 ? ' ...' : ''}: dig ${left.length > 1 ? 'them' : 'it'} when you pass`
    : ''
  return `${countLine(spent)} went into the towers and bridges the walk built (the pathfinder climbs with whatever placeable block you carry); ${back}${where}`
}
