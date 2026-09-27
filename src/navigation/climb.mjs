// Out of a 1-wide shaft by hand (card 2b2d1f65). From the bottom of a flee shaft `goto dig=true` aimed at the surface dug
// or scaffolded further DOWN, twice (09-24, 09-26): from a cell boxed in on four sides the pathfinder's best partial path
// goes the one way it can dig, and its 1x1 tower never starts. `place` at the body's own feet answered "cannot get within
// reach": a block cannot go into the cell the body fills, and no other cell can be walked to from a shaft. What climbed
// out, by hand: a niche dug to the SIDE at head height, a block placed under the feet from it, and so on up, the two
// columns taking turns. This module reads that ladder off the world; nothing here touches a body.
//
// A cell is { name, solid } or null when its chunk is not loaded.
import { looksBuilt } from '../lib.mjs'

const BY_HAND = 'Dig by hand where you can see what stands there'
export const SIDES = [{ dx: 1, dz: 0, name: 'east' }, { dx: -1, dz: 0, name: 'west' }, { dx: 0, dz: 1, name: 'south' }, { dx: 0, dz: -1, name: 'north' }]
// what must not be dug into or under: a fluid pours into the shaft, a gravity block falls on the head
const DANGER = /^(water|flowing_water|lava|flowing_lava|bubble_column|gravel|suspicious_gravel|sand|red_sand|suspicious_sand|.*_concrete_powder|anvil|chipped_anvil|damaged_anvil)$/
// how far over the top of the climb a column is read: the cells dug reach two over the last feet cell, and what stands over them falls
const OVERHEAD = 3
const PLAIN = ['cobblestone', 'dirt']
// never stood on: gravity blocks, leaves (they decay), ground cover, and the things that open or hang instead of standing
const NOT_A_FOOTING = /sand$|gravel|_concrete_powder$|_bed$|_gate$|_door$|torch|sapling|leaves|leaf_litter|anvil|scaffolding|^(short_grass|tall_grass|fern|large_fern|dead_bush|snow)$/

const key = ({ x, y, z }) => `${x},${y},${z}`
const AIR = { name: 'air', solid: false }
// a cell the body can stand in: air or water, never a block, never lava, never one it cannot see
const passable = cell => Boolean(cell) && !cell.solid && cell.name !== 'lava'

// the first cell of a column, from fromY to toY, that is a danger to dig near, as 'gravel at x,y,z'; null when the column is
// safe. A side column is somebody's work when a block in it looks built (a fence post at the mouth of the first field climb,
// 09-26): never dug into, like the dig walk never breaks one. The body's own column is its own shaft, cap included
const dangerIn = (blockAt, x, z, fromY, toY, { built = false } = {}) => {
  for (let y = fromY; y <= toY; y++) {
    const cell = blockAt(x, y, z)
    if (!cell) return `a cell I cannot read at ${x},${y},${z}`
    if (DANGER.test(cell.name)) return `${cell.name} at ${x},${y},${z}`
    if (built && looksBuilt(cell.name)) return `${cell.name} at ${x},${y},${z} (built: not mine to dig)`
  }
  return null
}

const total = carried => Object.values(carried).reduce((n, c) => n + c, 0)
const shortfall = (needed, carried) => {
  const have = total(carried)
  if (needed <= have) return null
  const pack = have ? `carrying ${Object.entries(carried).map(([name, n]) => `${name}:${n}`).join(' ')}` : 'and nothing placeable carried (dirt or cobblestone)'
  return `short by ${needed - have} blocks: ${needed} to place under the feet${have ? ', ' : ' '}${pack}`
}

// The ladder from `feet` up to goalY: one level per step, each into the other of two columns (the shaft's own and the
// niche's), with the cells to dig first (the ceiling over the head for the first step, then the target column three high
// so the body can jump on out of it) and the block to place under the target's feet when nothing solid stands there.
// The side: the first of east, west, south, north whose column holds no fluid, no gravity block and no unread cell,
// preferring one with rock at head height (a wall to dig into, not a drop). `carried` is what may be placed, by name in
// order of preference, and each place takes the next of it; `short` says how many the climb lacks
export function climbPlan ({ feet, goalY, blockAt, carried = {} }) {
  const have = total(carried)
  const none = { side: null, levels: [], needed: 0, have, short: null, danger: null }
  if (goalY <= feet.y) return none
  const own = dangerIn(blockAt, feet.x, feet.z, feet.y + 2, goalY + OVERHEAD)
  if (own) return { ...none, danger: `no safe way up my own column: ${own} over my head. ${BY_HAND}` }
  const sides = SIDES.map(s => ({ ...s, x: feet.x + s.dx, z: feet.z + s.dz }))
    .map(s => ({ ...s, danger: dangerIn(blockAt, s.x, s.z, feet.y, goalY + OVERHEAD, { built: true }) }))
  const safe = sides.filter(s => !s.danger)
  const side = safe.find(s => blockAt(s.x, feet.y + 1, s.z)?.solid) ?? safe[0]
  if (!side) return { ...none, danger: `no safe side for a niche: ${sides.map(s => `${s.name} has ${s.danger}`).join(', ')}. ${BY_HAND}` }
  // the ladder is simulated: every dig and place is remembered, so each level reads the world the body will find there
  const changed = new Map()
  const cellAt = cell => changed.get(key(cell)) ?? blockAt(cell.x, cell.y, cell.z)
  const columns = [{ x: feet.x, z: feet.z }, { x: side.x, z: side.z }]
  const items = Object.entries(carried).flatMap(([name, n]) => Array.from({ length: n }, () => name))
  let placed = 0
  const levels = []
  for (let k = 1; k <= goalY - feet.y; k++) {
    const y = feet.y + k
    const into = columns[k % 2]
    const from = columns[(k + 1) % 2]
    const ceiling = k === 1 ? [{ x: from.x, y: y + 1, z: from.z }] : []
    const dig = [...ceiling, ...[y, y + 1, y + 2].map(dy => ({ x: into.x, y: dy, z: into.z }))].filter(cell => !passable(cellAt(cell)))
    dig.forEach(cell => changed.set(key(cell), AIR))
    const floor = { x: into.x, y: y - 1, z: into.z }
    const place = cellAt(floor)?.solid ? null : { ...floor, item: items[placed++] ?? null }
    if (place) changed.set(key(floor), { name: place.item ?? 'dirt', solid: true })
    levels.push({ to: { x: into.x, y, z: into.z }, dig, place })
  }
  return { side: side.name, levels, needed: placed, have, short: shortfall(placed, carried), danger: null }
}

// the pathfinder's answer from a shaft for a goal on the surface: a path that ends lower than the feet. Only a goal ABOVE the
// body makes a way down a refusal; an empty path is arrivalError's business
export const descendingLeg = ({ from, path, goalY }) => goalY > from.y && path.length > 0 && path[path.length - 1].y < from.y
export const descentNote = end => `the path goes down (to ${end.x},${end.y},${end.z}) for a goal above me: climbing instead`

// still in the shaft or its niche: fewer than two sides open at feet and head height. On the surface, or at a cave junction,
// the walk is the pathfinder's again. passable(dx, dy, dz) answers about a cell relative to the feet, like boxedIn's
export const inPocket = passable => SIDES.filter(({ dx, dz }) => passable(dx, 0, dz) && passable(dx, 1, dz)).length < 2

// what in the pack a climb may stand on, the plainest first: full blocks that stay where they are put. `solid(name)` is the
// registry's word on whether the block has a box
export function climbBlocks (counts, solid) {
  const usable = Object.keys(counts).filter(name => counts[name] > 0 && solid(name) && !NOT_A_FOOTING.test(name))
  const rank = name => PLAIN.includes(name) ? PLAIN.indexOf(name) : PLAIN.length
  const names = usable.sort((a, b) => rank(a) - rank(b) || a.localeCompare(b))
  return Object.fromEntries(names.map(name => [name, counts[name]]))
}

// `place` aimed at the cell the body fills while boxed in a 1-wide shaft: no cell beside it to place from, so the walk to one
// searched 5 s and said "cannot get within reach"
export const ownCellRefusal = ({ feet, target, boxed }) =>
  boxed && target.x === feet.x && target.z === feet.z && (target.y === feet.y || target.y === feet.y + 1)
    ? 'no cell beside me to place from: dig a side niche at head height first (climb does this), or goto x= y= z= dig=true which climbs by itself'
    : null

// The climb itself, level by level, with whatever digs, places and steps the caller hands in (a body's primitives, or a
// composite's api.act). The plan is read once; after every step the feet are read back, and a step that did not land where
// the plan said ends the climb naming where the body stands. `until(feet)` stops it early: out of the pocket, for a walk
export async function climbShaft ({ feetAt, goalY, blockAt, carried, dig, place, step, until = () => false }) {
  const feet = feetAt()
  const plan = climbPlan({ feet, goalY, blockAt, carried })
  if (plan.danger) throw new Error(plan.danger)
  const done = { side: plan.side, from: feet, climbed: 0, dug: 0, placed: 0 }
  const progress = () => `climbed ${done.climbed} of ${plan.levels.length} from ${key(feet)}`
  for (const level of plan.levels) {
    if (level.place && !level.place.item) throw new Error(`${progress()}, then out of blocks at ${key(feetAt())}: ${plan.short}`)
    for (const cell of level.dig) { await dig(cell); done.dug++ }
    if (level.place) { await place(level.place); done.placed++ }
    await step(level.to)
    const now = feetAt()
    if (key(now) !== key(level.to)) throw new Error(`${progress()}, then the step up to ${key(level.to)} did not happen: standing at ${key(now)}`)
    done.climbed++
    if (until(now)) break
  }
  return { ...done, to: feetAt() }
}
