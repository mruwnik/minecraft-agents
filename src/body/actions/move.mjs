// Getting about (help section move): the walk movements, goto and follow.
import { groveExit, steer } from '../../navigation/bamboo.mjs'
import { openGateWalk, besideNames, noFooting, thicketCost, gateStepCost, waterWary, coordsError, looksBuilt, plansFromOwnCell, feetCell, inAnyZone, within } from '../../lib.mjs'
import { noStanding, loadedAround, thinkBudget, goalDistance, rimGoal } from '../../navigation/walk.mjs'
import { configureTerrainMoves, scaffoldingAvailable, climbableVinesAvailable } from '../../navigation/terrain-moves.mjs'
import { digLegs } from '../../navigation/dig-legs.mjs'
import { farmWalk } from '../../lib/path.mjs'
import { spareTest } from '../../farm/leg.mjs'
import { climbShaft, climbBlocks, inPocket, descendingLeg, descentNote } from '../../navigation/climb.mjs'
import { zones, readPlaces } from '../events.mjs'
import { inventoryCounts, cellAt } from '../helpers.mjs'
import { walkMoves, digMoves } from '../../bot.mjs'
import { Movements, goals, Vec3, bot, cancelGuard, pos } from '../state.mjs'
import { isWoodDoor } from '../doors.mjs'
import { resumeFollow } from '../reflexes.mjs'
import { surfaceWalkRuntime } from '../runtimes.mjs'
import { followTarget, setFollowTarget } from '../jobs.mjs'
import { amBoxedIn, amBoxedByBamboo } from '../explain.mjs'
import { long } from './tables.mjs'

export let digging = false
// whether a dig walk must leave a block whole, whatever it is: the cells and the ground of the plan a farm sweep walks
// inside (goto spare= floor=, src/farm/leg.mjs spareTest). Set for one walk and cleared after it; looksBuilt keeps
// guarding everything else
const NONE_SPARED = () => false
let spared = NONE_SPARED
const FarmMovements = farmWalk(Movements)
export function makeMoves (dig) {
  const moves = new FarmMovements(bot)
  moves.allowParkour = true
  moves.canOpenDoors = true
  for (const block of Object.values(bot.registry.blocksByName)) if (plansFromOwnCell(block.name)) moves.emptyBlocks.add(block.id)
  // the pathfinder's list of gates it may open is older than cherry, mangrove, bamboo, pale oak, crimson and warped: to it those were walls
  // (Vivenna's and Aviendha's cherry gates: lead said "no way", walks went over the fence by parkour or not at all)
  for (const block of Object.values(bot.registry.blocksByName)) if (block.name.endsWith('_fence_gate')) moves.openable.add(block.id)
  for (const block of Object.values(bot.registry.blocksByName)) if (noFooting(block.name)) moves.fences.add(block.id)
  moves.canDig = dig
  Object.assign(moves, waterWary(dig))
  moves.allow1by1towers = dig
  if (!dig) moves.scafoldingBlocks = []
  // The pathfinder only knows fence gates; to it a door is a wall to smash. Call wooden doors walkable and let doorTick work the handle.
  const getBlock = moves.getBlock.bind(moves)
  moves.getBlock = (...at) => {
    const b = getBlock(...at)
    if (isWoodDoor(b)) return Object.assign(b, { safe: true, physical: false, height: at[0].y + at[2] })
    // and an OPEN gate is air to walk through, not the wall prismarine-block makes of it (see openGateWalk)
    const open = b?.name?.endsWith('_fence_gate') ? openGateWalk({ name: b.name, open: b.getProperties().open }) : null
    return open ? Object.assign(b, open) : b
  }
  const zoneCost = block => inAnyZone(zones, block.position) ? 100 : 0
  moves.exclusionAreasBreak.push(zoneCost)
  // nor anything that looks built, protected or not
  moves.exclusionAreasBreak.push(block => looksBuilt(block.name) ? 100 : 0)
  moves.exclusionAreasBreak.push(block => block.position && spared(block.position, block.name) ? 100 : 0)
  // Dig walks can place emergency footing as well as break obstructions; keep
  // that placement inside the same caller-authorized cells and bounds.
  moves.exclusionAreasPlace.push(block => block.position && spared(block.position, block.name) ? 100 : 0)
  moves.exclusionAreasPlace.push(zoneCost)
  moves.exclusionAreasStep.push(block => gateStepCost(block.name))
  moves.exclusionAreasStep.push(block => thicketCost(besideNames(block.position, (x, y, z) => bot.blockAt(new Vec3(x, y, z), false)?.name)))
  // collectBlock switches both of these off on the movements it is given; with them off a tunnel under gravel buried and killed me
  for (const guard of ['dontMineUnderFallingBlock', 'dontCreateFlow']) Object.defineProperty(moves, guard, { get: () => true, set () {} })
  configureTerrainMoves(moves, { blockAt: (x, y, z) => bot.blockAt(new Vec3(x, y, z)), scaffolding: scaffoldingAvailable(), climbableVines: climbableVinesAvailable() })
  return moves
}
export function useMoves (dig) {
  digging = dig
  bot.pathfinder.setMovements(dig ? digMoves : walkMoves)
}

// the walk itself: goto sets the cells spared from digging round it
async function gotoWalk (a) {
  const wriggled = await wriggleOut()
  let walked = { legs: 1 }
  if (a.place) {
    const p = readPlaces().find(q => q.name === a.place)
    if (!p) throw new Error(`no place called ${a.place}; see ./mc places`)
    walked = await walkLegs({ x: p.x, y: p.y, z: p.z }, a.range ?? 2, a.into === true)
  } else if (a.player) {
    const e = bot.players[a.player]?.entity
    if (!e) throw new Error(`can't see ${a.player}`)
    await bot.pathfinder.goto(new goals.GoalFollow(e, a.range ?? 2))
  } else if (coordsError(a, a.y !== undefined)) {
    throw new Error(coordsError(a, a.y !== undefined))
  } else if (a.y === undefined) {
    await bot.pathfinder.goto(new goals.GoalNearXZ(a.x, a.z, a.range ?? 1))
    // an x/z goal is met at any depth, and a walk that may not dig likes caves: say so rather than let the driver assume the surface
    if (bot.blockAt(bot.entity.position.offset(0, 1, 0))?.skyLight === 0) return { pos: pos(), ...(wriggled && { note: wriggled }), underground: 'no sky above you: an x/z goal is met at any depth. For a spot on the surface pass y= as well' }
  } else {
    walked = await walkLegs({ x: a.x, y: a.y, z: a.z }, a.range ?? 1, a.into === true)
  }
  const note = [wriggled, walked.note].filter(Boolean).join('; ')
  return { pos: pos(), ...(walked.legs > 1 && { legs: walked.legs }), ...(note && { note }) }
}

export const moveLong = {
  async goto (a) {
    if (bot.vehicle) throw new Error('cannot walk while mounted; use the vehicle controller or confirm a safe dismount first')
    if (a.surface !== undefined) return surfaceWalkRuntime.walk(a)
    spared = spareTest(a)
    try { return await gotoWalk(a) } finally { spared = NONE_SPARED }
  }
}

export const moveQuick = {
  follow (a) {
    if (!bot.players[a.player]?.entity) throw new Error(`can't see ${a.player} right now`)
    setFollowTarget(a.player)
    resumeFollow()
    return { following: a.player }
  }
}
// the path a walk would take, searched the way path_to searches it: one 40 ms slice at a time until it is done or the budget is out
function searchPath (aim, range) {
  const began = Date.now()
  const budget = thinkBudget(goalDistance(aim, bot.entity.position))
  let r = bot.pathfinder.getPathTo(bot.pathfinder.movements, new goals.GoalNear(aim.x, aim.y, aim.z, range), budget)
  while (r.status === 'partial' && r.context && Date.now() - began < budget) r = Object.assign(r.context.compute(), { context: r.context })
  return r.path
}
// one jump into the cell beside and one up, with the legs (a shaft is where the pathfinder found nothing, so it is not asked first)
async function stepUp (cell) {
  const there = () => { const feet = feetCell(bot.entity.position, bot.entity.onGround); return feet.x === cell.x && feet.y === cell.y && feet.z === cell.z }
  await bot.lookAt(new Vec3(cell.x + 0.5, cell.y + 1.62, cell.z + 0.5), true).catch(() => {})
  bot.setControlState('forward', true)
  bot.setControlState('jump', true)
  try {
    for (let t = 0; t < 30 && !(there() && bot.entity.onGround); t++) await bot.waitForTicks(1)
  } finally {
    bot.setControlState('forward', false)
    bot.setControlState('jump', false)
  }
  await bot.waitForTicks(4)
  if (!there()) await within(3000, bot.pathfinder.goto(new goals.GoalBlock(cell.x, cell.y, cell.z)), 'stepping up').catch(() => {})
  bot.pathfinder.setGoal(null)
}
// From the bottom of a 1-wide shaft a dig walk aimed at the surface dug or scaffolded further DOWN (card 2b2d1f65): from a cell
// boxed in on four sides the pathfinder's best partial path goes the one way it can dig. A goal above the body is climbed first
// when the body is boxed in or the search's path ends lower than the feet: by hand (src/navigation/climb.mjs), a niche to the side at
// head height, a block under the feet, a step up, until the shaft opens on two sides; the legs take it from there
async function climbFirst (to) {
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  if (to.y <= feet.y) return null
  const boxed = amBoxedIn()
  const path = boxed ? [] : searchPath(digLegs(bot.entity.position, to)[0], 1)
  if (!boxed && !descendingLeg({ from: feet, path, goalY: to.y })) return null
  const why = boxed ? 'in a 1-wide shaft with the goal above me: climbing first' : descentNote(path[path.length - 1])
  const passable = (x, y, z) => { const cell = cellAt(x, y, z); return Boolean(cell) && !cell.solid && cell.name !== 'lava' }
  const out = await climbShaft({
    feetAt: () => feetCell(bot.entity.position, bot.entity.onGround),
    goalY: to.y,
    blockAt: cellAt,
    carried: climbBlocks(inventoryCounts(), name => bot.registry.blocksByName[name]?.boundingBox === 'block'),
    dig: cell => long.dig({ x: cell.x, y: cell.y, z: cell.z, batch: true, by_hand: true }),
    place: block => long.place({ item: block.item, x: block.x, y: block.y, z: block.z }),
    step: stepUp,
    until: now => !inPocket((dx, dy, dz) => passable(now.x + dx, now.y + dy, now.z + dz))
  }).catch(e => { throw new Error(`${why}; ${e.message}`) })
  return `${why}; climbed ${out.climbed} (${out.side} niche, ${out.placed} placed, ${out.dug} dug) to ${out.to.x},${out.to.y},${out.to.z}`
}
// walked, never dug: a dug base never regrows, and the free space between the stalks always leads out of a grove that is not sealed
async function wriggleOut () {
  if (!amBoxedByBamboo()) return null
  const alive = cancelGuard()
  const { y } = feetCell(bot.entity.position, bot.entity.onGround)
  const at = (x, dy, z) => bot.blockAt(new Vec3(x, y + dy, z))
  const bambooAt = (x, z) => [0, 1].some(dy => at(x, dy, z)?.name === 'bamboo')
  const clear = (x, dy, z) => { const block = at(x, dy, z); return Boolean(block) && (block.name === 'bamboo' || block.boundingBox === 'empty') }
  const openAt = (x, z) => clear(x, 0, z) && clear(x, 1, z) && at(x, -1, z)?.boundingBox === 'block'
  const waypoints = groveExit({ from: bot.entity.position, bambooAt, openAt })
  if (!waypoints) return null
  const reached = async waypoint => {
    for (let t = 0; t < 40; t++) {
      const { yaw, sneak, arrived } = steer(bot.entity.position, waypoint)
      if (arrived) return true
      alive()
      await bot.look(yaw, 0, true)
      bot.setControlState('forward', true)
      bot.setControlState('sneak', sneak)
      await bot.waitForTicks(1)
    }
    return steer(bot.entity.position, waypoint).arrived
  }
  try {
    for (const waypoint of waypoints) if (!await reached(waypoint)) return null
  } finally {
    bot.setControlState('forward', false)
    bot.setControlState('sneak', false)
  }
  const out = waypoints.at(-1)
  return `wriggled out of the bamboo to ${out.x.toFixed(2)},${out.z.toFixed(2)}`
}
// a dig walk goes in legs of 6 (src/navigation/dig-legs.mjs): a straight line of 20 through rock is more search than the 5 s budget
// holds, and legs of 5-8 arrived all afternoon where 10+ timed out (card 5e16aff9). A plain walk keeps its one goal
async function walkLegs (to, range, into = false) {
  const notes = []
  const climbed = digging ? await climbFirst(to) : null
  if (climbed) notes.push(climbed)
  const legs = digging ? digLegs(bot.entity.position, to) : [to]
  for (const [i, leg] of legs.entries()) {
    const last = i === legs.length - 1
    // a leg on (or in mid-air over) the floor of a pit walks to the pit's rim instead (src/navigation/walk.mjs rimGoal, card 3fe30fb4)
    const rim = rimGoal(cellAt, leg, last ? range : 1, { into, from: feetCell(bot.entity.position, bot.entity.onGround) })
    if (rim) notes.push(rim.note)
    const aim = rim ?? { x: leg.x, y: leg.y, z: leg.z, range: last ? range : 1 }
    // the judgement path_to and goNear make before the search, which this walk alone did not: a goal with no cell to stand in
    // within its range (the middle of a planted field: farm.maintain's first walk, card 29167296) is refused in a millisecond
    // with the reason, not after A* has run its budget out ("ran out of time") or its radius ("no walkable path"). Only over
    // loaded cells: a far goal is walked towards and judged by the pathfinder as its chunks arrive; a dig walk makes its own room
    const nowhere = !digging && loadedAround(cellAt, aim, aim.range) ? noStanding(cellAt, aim, aim.range) : null
    if (nowhere) throw new Error(nowhere)
    await bot.pathfinder.goto(new goals.GoalNear(aim.x, aim.y, aim.z, aim.range))
      .catch(e => { throw new Error(last && legs.length === 1 ? e.message : `leg ${i + 1} of ${legs.length}, to ${aim.x},${aim.y},${aim.z}: ${e.message}`) })
  }
  return { legs: legs.length, ...(notes.length && { note: notes.join('; ') }) }
}
