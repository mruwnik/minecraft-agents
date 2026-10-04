// Reading a farm's built state against its plan: field census, matching a build convention, and anchoring
// (finding the y-offset and orientation) a plan against what already stands.

import { PLAN_LEGEND, planSpec, planItemMatches, planLane, planCropMatches } from './plan.mjs'
import { holdsWater, isAir, isGroundCover, range } from './world.mjs'
import { ripeCrop } from './farm.mjs'
// ---------------------------------------------------------------- composite actions: what a farm needs
// worldAt(x,y,z) answers { name, properties } or null (not loaded). Both of these are read-only judgements: no walking, no digging.
// what stands on a farm right now, judged against its plan
export function fieldCensus (cells, worldAt) {
  const out = { crops: {}, cells: cells.length, ripe: 0, growing: 0, empty: 0, untilled: 0, dry: 0 }
  for (const cell of cells) {
    const spec = planSpec(cell)
    if (!spec) continue
    const ground = worldAt(cell.x, cell.y, cell.z)
    const here = worldAt(cell.x, cell.y + 1, cell.z)
    if (spec.kind === 'water') {
      if (ground && !holdsWater(ground)) out.dry++
      // open water in a field is a hole: the body wades in, `dig` refuses the blocks beside it and no walk will cross it
      else if (spec.cover && ground?.name === 'water') out.open = (out.open ?? 0) + 1
      continue
    }
    if (spec.kind !== 'crop') continue
    if (spec.ground === 'farmland' && ground && ground.name !== 'farmland') out.untilled++
    if (!planCropMatches(spec, here?.name)) { out.empty++; continue }
    const crop = here.name.replace(/^attached_/, '')
    out.crops[crop] = (out.crops[crop] ?? 0) + 1
    if (ripeCrop(here.name, here.properties?.age)) out.ripe++
    else out.growing++
  }
  // and whether the body can stand next to what it is being asked to work: read off the plan, not the world, because a
  // lane is a property of the SHAPE - a field with no lane through it has none whether or not its crops have grown
  return { ...out, ...planLane(cells) }
}

// A plan whose y is one off reads as a field of empty, untilled beds (Chani's wheat field: 28 wheat stood one block
// above where the code looked) or has a build dig the turf out and lay its floor one lower (her sheep pen). A plan's y
// is the GROUND block, and this answers `off`: what to add to that y to reach the level the world is really at.
// Two ways to tell, in order:
//  - the world's own copy of the plan, standing one block up or one down. Water is no evidence of a shift on its own
//    (a channel reads as water either way), and one matching block is a coincidence, so it takes two.
//  - nothing built there yet, but every cell the plan names is open air (or grass) over solid ground: that is the level
//    you STAND on, one above the ground block a plan wants.
// The legend names ONE wood for every wooden thing - oak_fence, oak_fence_gate, oak_slab, oak_sapling - because a plan
// is a drawing, not a shopping list. A pen actually built of birch or cherry fence is the same pen. Until this, the
// levelling pass called every post of such a wall a boulder standing in the cell and dug the wall out, and the anchor
// check found no evidence of the plan at all, so a field one block low read as bare ground.
const WOODS = ['dark_oak', 'pale_oak', 'oak', 'spruce', 'birch', 'jungle', 'acacia', 'mangrove', 'cherry', 'bamboo', 'crimson', 'warped']
const woodless = name => {
  const wood = WOODS.find(w => String(name).startsWith(`${w}_`))
  return wood ? String(name).slice(wood.length + 1) : null
}
export const sameFamily = (want, got) => {
  if (!want || !got) return false
  if (want === got) return true
  const [a, b] = [woodless(want), woodless(got)]
  return Boolean(a) && a === b
}

const anchorHit = (spec, here) => {
  if (!here) return false
  if (spec.kind === 'crop') return planCropMatches(spec, here.name)
  if (spec.kind === 'gate') return here.name.endsWith('_fence_gate')
  return planItemMatches(spec, here.name, sameFamily)
}
const CONVENTION = "a plan's y is the GROUND block (the farmland, pen floor or path itself; crops, fences, gates, chests and a water cover stand at y+1)"
// what you can stand in: air, or the grass and flowers that grow on open ground
const isOpenCell = name => isAir(name) || isGroundCover(name) || WEEDS.has(name)
// what you can stand on
const isFooting = name => Boolean(name) && !isOpenCell(name) && name !== 'water' && name !== 'lava'
function freshGround (cells, worldAt, y) {
  const seen = cells.filter(c => planSpec(c) && worldAt(c.x, c.y, c.z))
  const standing = seen.filter(c => isOpenCell(worldAt(c.x, c.y, c.z).name) && isFooting(worldAt(c.x, c.y - 1, c.z)?.name))
  if (seen.length < 2 || standing.length !== seen.length) return { off: 0 }
  return {
    off: -1,
    fresh: standing.length,
    note: `the plan says y=${y}, but all ${standing.length} of its cells are open air over solid ground at y=${y - 1}: you gave the level you stand on. ${CONVENTION}, so re-save it with y=${y - 1}`
  }
}
// planAnchor looks one block up and one block down, and nowhere to the side: a pen ring standing a few cells ACROSS
// from its plan is bare ground as far as it can tell. Chani's pen was marked 2 east and 3 south of the ring it
// describes, so pen.build read an empty field, laid half a second ring through the middle of the first and let 4
// sheep out (2026-09-23 02:55Z). This is the sideways half of the same question: where does the thing the plan
// describes actually stand? Answers null unless a shift within `reach` explains more than half the plan's solid
// cells and more of them than the plan's own spot does - a neighbour's wall brushing the plan is not its own ring.
const compassOf = (dx, dz) => [dx && `${Math.abs(dx)} ${dx < 0 ? 'west' : 'east'}`, dz && `${Math.abs(dz)} ${dz < 0 ? 'north' : 'south'}`]
  .filter(Boolean).join(' and ')
export function planBeside (cells, worldAt, { name = 'the place', reach = 3 } = {}) {
  const solidCells = cells.filter(c => planSpec(c) && planSpec(c).kind !== 'path' && planSpec(c).kind !== 'water')
  if (solidCells.length < 6) return null
  const score = (dx, dy, dz) => solidCells.filter(c => anchorHit(planSpec(c), worldAt(c.x + dx, c.y + 1 + dy, c.z + dz))).length
  // the plan's own spot, judged the way planAnchor judges it: a build that is simply unfinished is not a plan in the
  // wrong place, and a plan that already stands where it says is not searched for at all (the whole of a big farm, every shift)
  const here = Math.max(...[0, -1, 1].map(dy => score(0, dy, 0)))
  if (here === solidCells.length) return null
  const shifts = range(-reach, reach).flatMap(dx => range(-reach, reach).flatMap(dz =>
    dx === 0 && dz === 0 ? [] : [0, -1, 1].map(dy => ({ dx, dy, dz, found: score(dx, dy, dz) }))))
  const away = s => Math.abs(s.dx) + Math.abs(s.dz) + Math.abs(s.dy)
  const best = [...shifts].sort((a, b) => b.found - a.found || away(a) - away(b))[0]
  if (!best || best.found < Math.max(6, solidCells.length / 2) || best.found <= here) return null
  const at = { x: Math.min(...cells.map(c => c.x)) + best.dx, y: cells[0].y + best.dy, z: Math.min(...cells.map(c => c.z)) + best.dz }
  return {
    ...best,
    at,
    note: `${best.found} of the ${solidCells.length} blocks it describes stand ${compassOf(best.dx, best.dz)} of where the plan puts them, on ground at y=${at.y}: what the plan describes is already built, just not where the place is marked. A place's x,z is the NORTH-WEST corner cell of its plan (its lowest x and lowest z) and its y is the GROUND block, so mark it where the thing itself stands and build again: ./mc mark name=${name} x=${at.x} y=${at.y} z=${at.z} (marking again keeps the plan). If you meant to build a SECOND one here, move the plan further off: this one would run through the middle of what stands`
  }
}
// Does what a plan describes stand at its own anchor? Half its solid cells (fences, gates, crops, chests...) at least,
// and two: the evidence a mark needs before it moves a place off something built (see markMove)
export function planStands (cells, worldAt) {
  const solidCells = cells.filter(c => planSpec(c) && planSpec(c).kind !== 'path' && planSpec(c).kind !== 'water')
  const found = solidCells.filter(c => [0, -1, 1].some(dy => anchorHit(planSpec(c), worldAt(c.x, c.y + 1 + dy, c.z)))).length
  return found >= 2 && found >= solidCells.length / 2
}
// The GROUND a plan's cells sit on is evidence its own contents cannot spoil. Chani's carrot patch was stored at y=72
// over farmland built at y=71, and nothing caught it: farm.maintain got stuck twice on ten cells it could not till
// (BUGS.md 09-24 01:23Z). The crop test could not see it, because somebody had planted wheat in a patch the plan calls
// carrots, so no cell matched at any level and the whole check fell through to "no evidence". A crop cell is farmland
// whatever grew in it, and a channel is water whoever covered it. Only those two count: the `dirt` under a path is the
// same dirt one block down and one block up, and scoring it would invent a shift under every plan laid on soil.
const hasGround = spec => Boolean(spec) && (spec.kind === 'water' || spec.ground === 'farmland')
const groundHit = (spec, here) => {
  if (!here) return false
  return spec.kind === 'water' ? holdsWater(here) : here.name === 'farmland'
}
export function planAnchor (cells, worldAt) {
  const solidCells = cells.filter(c => planSpec(c) && planSpec(c).kind !== 'path' && planSpec(c).kind !== 'water')
  const groundCells = cells.filter(c => hasGround(planSpec(c)))
  const score = dy =>
    solidCells.filter(c => anchorHit(planSpec(c), worldAt(c.x, c.y + 1 + dy, c.z))).length +
    groundCells.filter(c => groundHit(planSpec(c), worldAt(c.x, c.y + dy, c.z))).length
  const here = score(0)
  const best = [{ dy: 1, n: score(1) }, { dy: -1, n: score(-1) }].sort((a, b) => b.n - a.n)[0]
  const y = cells[0]?.y
  if (!best || best.n < 2 || best.n <= here) return cells.length ? freshGround(cells, worldAt, y) : { off: 0 }
  return {
    off: best.dy,
    found: best.n,
    note: `the plan says y=${y}, but ${best.n} of the cells it describes match the world one block ${best.dy > 0 ? 'UP' : 'DOWN'}: ${CONVENTION}, so re-save it with y=${y + best.dy}`
  }
}

// the work that would put a farm back the way its plan says, in the order it has to happen: clear the bed, till it,
// refill the channel, plant, then build what is missing. A cell with somebody's block on it is left alone.
// what may be broken to clear a crop bed: weeds and the flowers that spring up around bone meal. A crop cell holding anything
// else (cobblestone, someone's torch) is left alone and shows up as `empty` in the census instead.
export const WEEDS = new Set(['short_grass', 'tall_grass', 'fern', 'large_fern', 'dead_bush', 'snow', 'dandelion', 'poppy', 'cornflower',
  'oxeye_daisy', 'azure_bluet', 'blue_orchid', 'allium', 'lily_of_the_valley', 'red_tulip', 'orange_tulip', 'white_tulip', 'pink_tulip',
  'bush', 'firefly_bush', 'leaf_litter', 'wildflowers', 'short_dry_grass', 'tall_dry_grass'])
