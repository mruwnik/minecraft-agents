// A failed walk explained from what the body can read off itself: boxed in, in a hole, perched, a reflex that took over.
import { FLUIDS, feetCell, inAnyZone, explainInterrupt, explainNoPath, boxedIn } from '../lib.mjs'
import { noPathAdvice, inHole, perchedOverField } from '../navigation/cave-exit.mjs'
import { noFirstMove } from '../lib/path.mjs'
import { zones } from './events.mjs'
import { lastWalkGoal, lastReflex } from '../bot.mjs'
import { Vec3, bot } from './state.mjs'
import { digging } from './actions/move.mjs'

const recentReflex = () => lastReflex && { ...lastReflex, agoMs: Date.now() - lastReflex.at }
// #128: every goto out of a 1x1 natural shaft fails in a second with "no walkable path", a goto one block away
// included. True, and useless: read once from the body's own cell, the answer is about the block it is ON
const passableAboutFeet = (through = () => false) => {
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  return (dx, dy, dz) => { const block = bot.blockAt(new Vec3(feet.x + dx, feet.y + dy, feet.z + dz)); return block?.boundingBox !== 'block' || through(block) }
}
export const amBoxedIn = () => Boolean(bot?.entity) && boxedIn(passableAboutFeet())
// bamboo the pathfinder reads as walls (a fence-like thicket), though the server's offset stalks leave the body room to walk out between
export const amBoxedByBamboo = () => amBoxedIn() && !boxedIn(passableAboutFeet(block => block.name === 'bamboo'))
// a hole one block deep (card 94e6dcb1): the walk out of it is a jump, and a failed one reads as a distant obstacle
const amInHole = () => Boolean(bot?.entity) && inHole(passableAboutFeet())
// one block above a field, on a log in the rows (Jizo, 09-26 23:24Z): the way down is a drop onto farmland
const amPerched = () => {
  if (!bot?.entity) return false
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  return perchedOverField((dx, dy, dz) => bot.blockAt(new Vec3(feet.x + dx, feet.y + dy, feet.z + dz))?.name)
}
// what the body can read off itself when a walk finds no path (src/navigation/cave-exit.mjs): no sky over the head and the goal up
// on the surface, water in or beside its cell (a dig walk breaks nothing beside a liquid), a protected zone round it
const noPathEvidence = () => {
  if (!bot?.entity) return {}
  const me = bot.entity.position
  const feet = feetCell(me, bot.entity.onGround)
  const beside = [[0, 0, 0], [0, -1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]]
  const wet = bot.entity.isInWater || beside.some(([dx, dy, dz]) => FLUIDS.has(bot.blockAt(new Vec3(feet.x + dx, feet.y + dy, feet.z + dz))?.name ?? ''))
  const goalY = typeof lastWalkGoal?.y === 'number' ? lastWalkGoal.y : null
  return { underground: bot.blockAt(me.offset(0, 1, 0))?.skyLight === 0, goalDy: goalY === null ? null : goalY - feet.y, wet, zoned: inAnyZone(zones, new Vec3(feet.x, feet.y, feet.z)) }
}
// the search's start with no move the walk keeps (path_to: noPath nodes=0 visited=1), named: src/lib/path.mjs noFirstMove
export const firstMoveNote = moves => {
  if (!bot?.entity || !moves?.firstMoves) return null
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  return noFirstMove(moves.firstMoves({ ...feet, remainingBlocks: moves.countScaffoldingItems() }))
}
export const explainFailure = message => {
  const boxed = amBoxedIn()
  // appended, never instead: the sweeps' dig retry and the stuck count read the no-path words (src/farm/leg.mjs PATH_FAILURE)
  const stuckHere = /no path to the goal|no walkable path/i.test(message) ? firstMoveNote(bot.pathfinder.movements) : null
  const advice = noPathAdvice({ text: explainNoPath(explainInterrupt(message, recentReflex()), digging, boxed), dig: digging, boxed, holed: amInHole(), perched: amPerched(), ...noPathEvidence() })
  return stuckHere ? `${advice}. ${stuckHere}` : advice
}
