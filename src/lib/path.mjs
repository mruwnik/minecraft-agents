// Explaining a failed or refused walk: why a goal was refused, why a path is stalled or circling, and the
// per-step costs (thickets, crops underfoot) that shape the search.

import { STEPS } from './world.mjs'
// why a long action may not start now, or null. Moving while the server has us in bed desyncs the body.
export function refuseReason ({ name, health, force, sleeping }) {
  if (sleeping && name !== 'run') return 'you are asleep: `wake` first'
  if (!['mine.get', 'dig'].includes(name) || health > 8 || force) return null
  return `health is ${Math.round(health)}: eat/rest first, or pass force=true`
}

// the pathfinder's "goal was changed"/"path was stopped" usually means a reflex took over the legs: say which one
export function explainInterrupt (error, reflex) {
  if (!reflex || reflex.agoMs > 10000 || !/goal was changed|path was stopped/i.test(error)) return error
  const what = reflex.kind === 'leashed'
    ? `breaking off a fight with ${reflex.mob} that pulled me too far: walking back to where it started`
    : reflex.kind === 'fleeing' ? `fleeing from ${reflex.mob}` : `fighting ${reflex.mob}`
  const advice = reflex.kind === 'fleeing' && reflex.mob !== 'creeper' ? ' (unarmed or badly hurt bodies flee: carry a sword, keep your health up)' : ''
  return `interrupted: ${what}. Wait until it is over, then retry${advice}`
}

// a healthy body sits near 200 MB and start-body caps the heap at 1536 MB; a runaway path search gets there within half a minute
export const overMemory = heapMb => heapMb >= 800

// the pathfinder snaps each waypoint onto the top of the block it is in; in a doorway that is the top of the door panel (a block
// up, off-centre), which no body can reach from the doorstep. Put such a waypoint back on the floor in the middle of the doorway.
export function doorwayNode (node, door) {
  if (!door) return node
  return { ...node, x: door.x + 0.5, y: door.half === 'upper' ? door.y - 1 : door.y, z: door.z + 0.5 }
}

// walks are walk-only (no digging, no scaffold: they used to tunnel through hills and leave pillars) unless asked; mining must dig
export const mayDig = (name, args) => args.dig === true

// Standing on a partial block, the pathfinder plans from the cell ABOVE it. Right for farmland and slabs, wrong for a bed: you wake up on
// it, and in a small hut with a low roof the cell above has no way out (no headroom, the door is one level down). Plan from the bed's cell
export const plansFromOwnCell = blockName => /_bed$/.test(blockName)

// standing on a bed under a low roof, the pathfinder plans from the cell above the bed, finds the roof there and no move at all.
// One plain step onto free floor next to me cures it: which side? cells: [{feet, head, ground}] bounding boxes of the four sides
// Second choice, when there is no free floor beside me (a bed along a 1-wide room): another low block (`low`: the bed's other half, a
// slab) that I can walk onto level, and step off again from there
export function stepOffChoice (cells) {
  const floor = cells.findIndex(c => c.feet === 'empty' && c.head === 'empty' && c.ground === 'block')
  if (floor >= 0) return floor
  // a wooden door is a way out (the reflex opens it): a bed whose only free neighbour was the door sent the body up and down the bed for ever
  const door = cells.findIndex(c => c.door)
  return door >= 0 ? door : cells.findIndex(c => c.low && c.head === 'empty')
}

// mineflayer-pathfinder's goto resolves as a success when the search comes back with an empty path (boxed in, in a shaft):
// the caller must check the goal itself
export const arrivalError = reached => reached ? null : 'no path to the goal: the search found nothing to walk from here'

// A 1x1 natural shaft: the only 2-high air column for blocks around is the one the body stands in. Every goto then
// fails in a second with "no walkable path", a goto one block away included - true, and useless, because it reads as
// "the destination cannot be reached" when what is meant is "you cannot leave the block you are on" (#128).
// `passable(dx, dy, dz)` answers about a cell relative to the feet: the body's own feet are (0,0,0), its head (0,1,0).
export const boxedIn = passable => {
  // level or down (the far column's floor may be lower: the fall is the pathfinder's business, not this question's)
  const across = ([dx, dz]) => passable(dx, 0, dz) && passable(dx, 1, dz)
  // up one, which needs my own ceiling open to rise into as well as the ledge being clear
  const up = ([dx, dz]) => passable(0, 2, 0) && passable(dx, 1, dz) && passable(dx, 2, dz)
  return !STEPS.some(d => across(d) || up(d))
}

const SHAFT_NOTE = 'you are standing in a 1-wide shaft with its walls at head height: nothing at all can be walked to from here, however near it is. The answer is about the block you are ON, not the one you asked for. `goto` the same place again with dig=true and the body digs itself out, or place a block at your feet and step up on it'
const SEARCH_TIMEOUT = 'the search ran out of time (5 s) before it found a way, which is not the same as there being none. The usual cause is a dead end close to the goal (a fenced alley beside a pen gate) that the search keeps trying first, and from inside that dead end even path_to finds nothing. Step back 10-20 blocks the way you came, then `path_to x= y= z= route=true` there names the gates of the long way round: walk it in legs, gate by gate'
export function explainNoPath (error, dig, boxed = false) {
  if (dig || !/no path to the goal|took to long to decide/i.test(error)) return error
  if (boxed) return SHAFT_NOTE
  // the search gave up on time, not for want of a way: a dead end near the goal (the fenced alley by Perrin's sheep pen gate) draws it in
  if (/took to long to decide/i.test(error)) return SEARCH_TIMEOUT
  return 'no walkable path (walks don\'t dig or bridge): look for a way round, go in shorter legs, or pass dig=true if breaking and placing blocks on the way is fine'
}

// the search found nothing (no or a partial path with not one step in it) and the legs stand still, but the pathfinder keeps the goal
// and whoever awaits the walk waits for ever: 13 of 61 stalls on 09-19, each killing a whole harvest or collect after 12 s.
// End just that walk, early: the task sees an ordinary "no path" and goes on to its next block or drop
export const deadWalk = ({ hasGoal, moved, digging, seconds, path }) =>
  hasGoal && !digging && moved < 0.2 && seconds >= 4 && Boolean(path) && path.status !== 'success' && path.nodes.length === 0

// a task that wants to walk somewhere but has neither moved nor dug for a while is hung (seen: walks started right beside a door)
export const stackTop = stack => String(stack ?? '').split('\n').filter(l => /^\s+at /.test(l)).slice(0, 3)
  .map(l => l.trim().replace(/^at /, '').replace(/\(?file:\/\/\S*\/([^/)]+)\)?$/, '$1').replace(/[()]/g, '')).join(' < ')
export const progressed = (from, here) => Math.hypot(here.x - from.x, here.z - from.z) >= 0.2 || Math.abs(here.y - from.y) >= 1.5
export const isStalled = ({ hasGoal, moved, digging, seconds }) => hasGoal && !digging && moved < 0.2 && seconds >= 12
// a walk that never stands still can still go nowhere: 40 s without getting nearer than its best is going round in
// circles (Perrin's cow pen gate, 170-290 s; mine round a birch pen every 11 s). `best` is the nearest it has been
export const CIRCLING_MS = 40000
export const circling = ({ dist, best, bestAgeMs }) => dist > 2 && bestAgeMs >= CIRCLING_MS

// mineflayer-pathfinder's goto rejects on a search timeout or a no-path and keeps the goal it set, and the pathfinder walks
// the best partial path of every new search for as long as a goal stands: a failed goto walked on with no task running
// (Jizo, 09-26 23:18Z: failed at 19.7,65,-96.5, found 25 s later at 19.5,65,-91.5, no forcedMove). `run(goal)` is the
// walk; on its failure the goal is cleared when it is still the walk's own (a flee's or a follow's goal is theirs)
export const clearGoalOnFailure = (pathfinder, run) => goal => run(goal).catch(e => {
  if (pathfinder.goal === goal) pathfinder.setGoal(null)
  throw e
})

// ---------------------------------------------------------------- walking over farmland (card fcd996fe)
// Farmland turns to dirt only when something LANDS on it (a fall of more than half a block: a jump, a drop), never
// from plain walking, and the crop on it pops off with it. The pathfinder sprints and jumps by default, which is why
// crops used to be fenced off from every walk (blocksToAvoid), a pocket of them refused every goal and trample=true
// was the way out. Now a crop cell is passable at CROP_STEP a cell, so any crop-free way wins and a way across the
// rows exists when there is no other; no parkour leap and no drop of two or more may land on a farmland floor, a drop
// of one only at TRAMPLE_STEP (the way down off a perch);
// and a leg with a crop or farmland node in it walks without sprinting (legFlags).
const CROPS_UNDERFOOT = new Set([
  'wheat', 'carrots', 'potatoes', 'beetroots', 'melon_stem', 'pumpkin_stem',
  'attached_melon_stem', 'attached_pumpkin_stem', 'sweet_berry_bush', 'torchflower_crop', 'pitcher_crop'
])
export const breaksUnderfoot = name => CROPS_UNDERFOOT.has(name)
// ten, not a hundred: every point widens the search (see gateStepCost), and ten a cell already sends a walk round a
// three-row bed rather than across it whenever the lane is within thirty steps
export const CROP_STEP = 10
export const cropStepCost = name => breaksUnderfoot(name) ? CROP_STEP : 0
// may the planner keep this move? `floor` is the name of the block under the move's landing cell: a landing from
// above (a drop, or a leap) on farmland is what tramples it (vanilla: a fall of more than half a block); a level step
// onto it is harmless, and so is a jump up of one (the body clears the 15/16 top by a quarter block and lands from
// there). Without the jump a body in a one-deep hole ringed by farmland had no move out at all (card 94e6dcb1); without
// the drop of one, a body on a log in the rows had no move down (Jizo, 09-26 23:24Z). The drop tramples, and the
// next farm.maintain re-tills: it stays at TRAMPLE_STEP. A leap or a drop of two has a way round
export const keepMove = (from, move, floor) => floor !== 'farmland' || (move.y >= from.y - 1 && move.y <= from.y + 1 && !move.parkour)
// twenty, two crop steps: a drop into a field is taken only when no level way is within twenty steps
export const TRAMPLE_STEP = 20
export const trampleCost = (from, move, floor) => floor === 'farmland' && move.y < from.y ? TRAMPLE_STEP : 0
// does the goto step sideways off this block after a failed walk? A bed, a slab or a chest is lower than a block and the
// pathfinder plans from the cell above it, where a low roof leaves no move (see stepOffChoice). Farmland and a dirt path
// are lower than a block too, but the planner walks them from the cell above like any floor; stepping off one scans for
// free floor at the farmland's own level, and the only such floor in a field is a hole in it
export const stepsOff = blockName => !['farmland', 'dirt_path'].includes(blockName)
// the flags the leg in `nodes` walks with: sprint only when no node of it stands in a crop or on farmland (the executor
// sprint-jumps a straight line it cannot walk in one go, and that lands). `nameAt(x, y, z)` is the block name there
export const legFlags = (nodes, nameAt) => ({
  allowSprinting: !nodes.some(n => breaksUnderfoot(nameAt(n.x, n.y, n.z)) || nameAt(n.x, n.y - 1, n.z) === 'farmland')
})
// mineflayer-pathfinder's Movements with the two rules above on every move it generates: exclusionAreasStep prices
// only straight steps (diagonals and jumps skip it), so the pricing and the filter sit on getNeighbors instead
export const farmWalk = Base => class extends Base {
  getNeighbors (node) {
    const floor = move => this.getBlock(move, 0, -1, 0)?.name
    return super.getNeighbors(node)
      .filter(move => keepMove(node, move, floor(move)))
      .map(move => Object.assign(move, { cost: move.cost + cropStepCost(this.getBlock(move, 0, 0, 0)?.name) + trampleCost(node, move, floor(move)) }))
  }

  // how many moves the pathfinder makes off `node`, and how many of them the farmland rule keeps (see noFirstMove)
  firstMoves (node) {
    return { made: super.getNeighbors(node).length, kept: this.getNeighbors(node).length }
  }
}
// why a search that visited one node ended `here` (path_to: noPath nodes=0 visited=1): its start had no move the walk
// keeps. Made but refused is the farmland rule; none made is a wall, a low roof or a deep drop. null: a move was kept
export const noFirstMove = ({ made, kept }) => {
  if (kept) return null
  if (made) return `no first move from here: all ${made} moves off this cell land on farmland from a leap or a drop of two or more, which a walk never takes. Dig the block underfoot or step down by hand, or goto with dig=true`
  return 'no first move from here: no cell beside, above or below this one can be walked, jumped or dropped to (walled in, a roof too low to jump, or a drop too deep). Read the four sides (block_at), then dig the block in the way, or goto with dig=true'
}
export const thicketCost = neighbours => neighbours.includes('bamboo') ? 25 : 0

// Which gates a planned route opens and where it goes, a waypoint every six steps and the last: path_to said gates=3 and
// nothing else while the walk circled a birch pen 15 blocks off Perrin's cow pen (13:58Z)
export function routeSummary (path) {
  const cell = n => `${n.x},${n.y},${n.z}`
  const gatesAt = path.flatMap(n => (n.toPlace ?? []).filter(t => t.useOne)).map(cell)
  const through = path.filter((_, i) => i % 6 === 5 || i === path.length - 1).map(cell)
  return { gatesAt: [...new Set(gatesAt)].join(' '), through: [...new Set(through)].join(' ') }
}

// the pathfinder previews each step with a physics simulation, and when that says "can't" it presses no key at all: it then throws the
// path away as 'stuck' every 3.5 s and finds the same one again, for ever. After 1.5 s of that, walk at the next node by hand
export function idleNudge ({ hasGoal, busy, idleTicks, node, pos, collided }) {
  if (!hasGoal || busy || idleTicks < 30 || !node) return null
  const [dx, dz] = [node.x - pos.x, node.z - pos.z]
  if (Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35) return null
  return { dx, dz, jump: collided || node.y - pos.y > 0.5 }
}

// what water costs a walk: a digging one tunnelled into an underground lake and half drowned in its own shaft (Aviendha)
export const waterWary = dig => dig ? { liquidCost: 40, infiniteLiquidDropdownDistance: false } : { liquidCost: 1, infiniteLiquidDropdownDistance: true }

// what stepping into this block costs a walk, in steps: pen gates are doors for those with business in the pen, not shortcuts (a cow left the human's pen with my mine)
// 8, not 30: every extra point widens the search (30 made a 9-step walk into my paddock visit 1308 nodes, and Ganesha's walks back to their pen over
// hilly ground ran into the 5 s limit: `lead` arrived with=0 three times). Two gates = 16: a pen is still no shortcut unless the way round is longer than that
export const gateStepCost = name => name?.endsWith('_fence_gate') ? 8 : 0
