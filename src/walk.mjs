// Where a walk can end, judged before the pathfinder searches. mineflayer-pathfinder's nodes are feet cells, and a walk
// steps round crops (blocksToAvoid), so a goal deep inside a planted field has no node that can satisfy it: A* then
// visits the whole search radius (15-19k nodes) and gives up at its 5 s thinkTimeout, once per job. That was farm.build's
// 5 s creep on jizo-melon-patch. A goal with no cell to stand on within its range is refused here in a millisecond,
// naming what fills the cells, so the composite skips the cell and goes on.
//
// A cell is { name, solid, liquid, crop } or null when its chunk is not loaded; the caller reads it off the world.

// the arm reaches 4.5 from the eyes. Work (till, plant, cover) walks to within this node distance of the cell it works:
// standing one up and four across from a ground cell is 4.12 away, so a lane every eight rows serves a whole field
export const WORK_RANGE = 4.2

// a passable feet or head cell: air, water, a carpet, a door the walk opens; never a crop, never lava
const passable = cell => Boolean(cell) && !cell.solid && !cell.crop && cell.name !== 'lava'

// can the body stand (or swim) with its feet in this cell: feet and head clear, and a floor under it or water round it
export const standable = (cellAt, { x, y, z }) => {
  const feet = cellAt(x, y, z)
  if (!passable(feet) || !passable(cellAt(x, y + 1, z))) return false
  return Boolean(feet.liquid) || Boolean(cellAt(x, y - 1, z)?.solid)
}

// every cell the pathfinder's GoalNear would accept: integer offsets with dx²+dy²+dz² <= range², round a floored target
export const cellsWithin = (target, range) => {
  const at = { x: Math.floor(target.x), y: Math.floor(target.y), z: Math.floor(target.z) }
  const reach = Math.ceil(range)
  const offsets = Array.from({ length: 2 * reach + 1 }, (_, i) => i - reach)
  return offsets.flatMap(dx => offsets.flatMap(dy => offsets.map(dz => ({ dx, dy, dz }))))
    .filter(({ dx, dy, dz }) => dx * dx + dy * dy + dz * dz <= range * range)
    .map(({ dx, dy, dz }) => ({ x: at.x + dx, y: at.y + dy, z: at.z + dz }))
}

// what stops the body standing in a cell that is not underground: a crop in it, a block over it, or a chunk not loaded
const blocker = (cellAt, { x, y, z }) => {
  const feet = cellAt(x, y, z)
  if (!feet) return 'unloaded'
  if (feet.solid) return null
  if (feet.crop) return feet.name
  const head = cellAt(x, y + 1, z)
  return head?.solid ? head.name : null
}

const tally = names => Object.entries(names.reduce((n, name) => ({ ...n, [name]: (n[name] ?? 0) + 1 }), {}))
  .sort((a, b) => b[1] - a[1]).slice(0, 2).map(([name, n]) => `${name}:${n}`).join(' ')

// null when some cell within range of the target can be stood in, else one line saying why not and what to do
export const noStanding = (cellAt, target, range) => {
  const cells = cellsWithin(target, range)
  if (cells.some(cell => standable(cellAt, cell))) return null
  const at = `${Math.floor(target.x)},${Math.floor(target.y)},${Math.floor(target.z)}`
  const head = `nowhere to stand within ${range} of ${at}`
  const only = cells.length === 1 ? cellAt(cells[0].x, cells[0].y, cells[0].z) : null
  if (only?.solid) return `${head}: it is ${only.name}, a block; aim at the cell above it (y=${Math.floor(target.y) + 1}) or pass range=1`
  const blockers = cells.map(cell => blocker(cellAt, cell)).filter(Boolean)
  const crops = cells.some(cell => cellAt(cell.x, cell.y, cell.z)?.crop)
  if (crops) return `${head}: every cell in reach is planted (${tally(blockers)}); a walk steps round crops, so leave a . path or a covered channel through the field, or work the rows from one`
  if (blockers.includes('unloaded')) return `${head}: that part of the world is not loaded here (${tally(blockers)}): walk nearer first`
  if (blockers.length) return `${head}: every cell in reach is blocked (${tally(blockers)})`
  return `${head}: nothing to stand on there (mid-air, or inside a block)`
}

// how long a search may think, by how far off the goal is. The plugin gives every search 5 s of wall time (about 16k nodes):
// a goal three blocks off that is not found in 1.5 s (5k nodes) is walled in, and the job wants to hear so then, not 3.5 s
// later; a walk across the map keeps the full 5 s. Only walks are budgeted: reflexes (flee, fight) keep the plugin's default
export const THINK_CAP_MS = 5000
export const thinkBudget = distance => distance === null || distance === undefined ? THINK_CAP_MS : Math.min(THINK_CAP_MS, 1500 + 25 * distance)

// straight-line distance from here to a goal that has coordinates (GoalNear, GoalBlock, GoalNearXZ, GoalPlaceBlock); null for one that follows an entity
export const goalDistance = (goal, from) => {
  if (typeof goal?.x !== 'number' || typeof goal?.z !== 'number') return null
  return Math.hypot(goal.x - from.x, typeof goal.y === 'number' ? goal.y - from.y : 0, goal.z - from.z)
}
