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

// where a JOB stands: the body swims through water but works from dry footing (a pour from a pond cell drifts, drops float off)
export const dryStandable = (cellAt, cell) => standable(cellAt, cell) && !cellAt(cell.x, cell.y, cell.z).liquid

// every cell the pathfinder's GoalNear would accept: integer offsets with dx²+dy²+dz² <= range², round a floored target
export const cellsWithin = (target, range) => {
  const at = { x: Math.floor(target.x), y: Math.floor(target.y), z: Math.floor(target.z) }
  const reach = Math.ceil(range)
  const offsets = Array.from({ length: 2 * reach + 1 }, (_, i) => i - reach)
  return offsets.flatMap(dx => offsets.flatMap(dy => offsets.map(dz => ({ dx, dy, dz }))))
    .filter(({ dx, dy, dz }) => dx * dx + dy * dy + dz * dz <= range * range)
    .map(({ dx, dy, dz }) => ({ x: at.x + dx, y: at.y + dy, z: at.z + dz }))
}

// is every cell a GoalNear of this range could end in loaded? A judgement of a goal's cells is only worth making over cells
// the body can see: a far goal is walked towards and judged by the pathfinder as its chunks arrive
export const loadedAround = (cellAt, target, range) => cellsWithin(target, range).every(cell => Boolean(cellAt(cell.x, cell.y, cell.z)))

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
  if (crops) return `${head}: every cell in reach is planted (${tally(blockers)}); work the rows from a . path or a covered channel (a walk crosses crops only where it must, at a walking pace)`
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

// A pit: a floor walled in by rises of two or more on every side, which a walk can jump into and not out of. Eight dig walks
// aimed at the mouth of the 1-wide, 3-deep test pit by spawn jumped in (card 3fe30fb4): the pathfinder's nearest node it
// could stand on was the pit floor. So the cell a walk would land on is judged before the search, like noStanding: on a pit
// floor, the goal moves to the pit's rim, unless the floor is meant (into=true, or the body is already down there)
const SIDES = [[1, 0], [-1, 0], [0, 1], [0, -1]]
// how far round the landing cell the way out is looked for: a floor that reaches this far is open ground (or a trench longer
// than the look), and the pathfinder is trusted with it
export const LOOK = 6
// how far below a goal in mid-air its floor is looked for, and how far up a pit's rim
const DEPTH = 8
const key = ({ x, y, z }) => `${x},${y},${z}`
const floored = ({ x, y, z }) => ({ x: Math.floor(x), y: Math.floor(y), z: Math.floor(z) })
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)

// the cell a walk to this goal lands on: the standable cell nearest the goal within its range (the higher of two as near),
// else, for a goal in mid-air, the floor a fall from it reaches; null when there is none (inside a block, or not loaded)
const landing = (cellAt, goal, range) => {
  const at = floored(goal)
  const near = cellsWithin(at, range).filter(cell => standable(cellAt, cell)).sort((a, b) => dist(a, at) - dist(b, at) || b.y - a.y)
  if (near.length) return near[0]
  const drop = Array.from({ length: DEPTH }, (_, i) => ({ x: at.x, y: at.y - 1 - i, z: at.z }))
  const solid = drop.findIndex(cell => !passable(cellAt(cell.x, cell.y, cell.z)))
  return drop.slice(0, solid === -1 ? DEPTH : solid).find(cell => standable(cellAt, cell)) ?? null
}

// every cell a walk reaches from `start` by steps of one across and at most one up or down (a step up needs head room);
// null once a cell LOOK away is reached: that floor is open ground, not a pit
const floorAround = (cellAt, start) => {
  const seen = new Map([[key(start), start]])
  const queue = [start]
  while (queue.length) {
    const cell = queue.shift()
    if (Math.abs(cell.x - start.x) >= LOOK || Math.abs(cell.z - start.z) >= LOOK) return null
    for (const [dx, dz] of SIDES) {
      for (const dy of [0, 1, -1]) {
        const next = { x: cell.x + dx, y: cell.y + dy, z: cell.z + dz }
        if (seen.has(key(next)) || !standable(cellAt, next)) continue
        if (dy === 1 && !passable(cellAt(cell.x, cell.y + 2, cell.z))) continue
        seen.set(key(next), next)
        queue.push(next)
      }
    }
  }
  return [...seen.values()]
}

// the tops of a floor's walls: the lowest standable cell two or more above a floor cell, in a column beside it, off the floor
const rimOf = (cellAt, floor) => {
  const inside = new Set(floor.map(key))
  const tops = floor.flatMap(cell => SIDES.map(([dx, dz]) =>
    Array.from({ length: DEPTH }, (_, i) => ({ x: cell.x + dx, y: cell.y + 2 + i, z: cell.z + dz })).find(top => standable(cellAt, top))))
  return [...new Map(tops.filter(top => top && !inside.has(key(top))).map(top => [key(top), top])).values()]
}

// null when the goal may stand, else the rim cell to walk to instead (range 0) with the note for the driver. `from` is the
// body's feet cell: a body already on the pit floor means to be there (a walk out is aimed at the rim, which is no pit)
export const rimGoal = (cellAt, goal, range, { into = false, from = null } = {}) => {
  if (into) return null
  const floor = landing(cellAt, goal, range)
  const pit = floor && floorAround(cellAt, floor)
  if (!pit || (from && pit.some(cell => key(cell) === key(floored(from))))) return null
  const at = floored(goal)
  const rim = rimOf(cellAt, pit).sort((a, b) => dist(a, at) - dist(b, at) || (from ? dist(a, from) - dist(b, from) : 0))[0]
  if (!rim) return null
  return { x: rim.x, y: rim.y, z: rim.z, range: 0, note: `the goal is the floor of a pit: standing at the rim ${rim.x},${rim.y},${rim.z} instead` }
}
