// Where a farm job stands to work its cell (cards 9608fc11 and 29167296).
//
// The primitives walk to "within 3" of a cell and work from wherever that lands them. A pour lands where the eyes really
// land, and inside a finished field almost nothing in range can see the top face it needs: from two cells off on level
// ground the neighbour's own block is in the way, in a lane of top slabs the next slab is, and across a bed the crop is.
// farm.build on jizo-melon-patch (09-26) twice answered "cannot see the top" where a driver standing one cell off
// poured at once. So a job chooses its cell BEFORE it calls the primitive: the cells round the target in order of
// preference, kept when the pathfinder can stand there dry, the cell is within the primitive's own walk range (so the
// primitive stays put), and a line from eye height to the target's top face crosses nothing. The line is walked over
// what api.block answers, in the terms test/helpers.mjs uses: a slab by its half, a crop by its age, a fence a block and
// a half tall, a cell not loaded as a wall (nobody sees through the unknown).
import { WORK_RANGE, cellsWithin, dryStandable } from './walk.mjs'
import { cellOf } from '../farm/field.mjs'
import { breaksUnderfoot, isAir, FLUIDS, jobCall } from '../lib.mjs'

// the eyes are 1.62 over the feet; a full bucket reaches 4.5 (src/body/actions/block.mjs's pour looks 5 out, the server 4.5)
export const EYE = 1.62
export const REACH = 4.5
// how near the pour primitive walks to its block (src/body/actions/block.mjs goNear(at, 3)): a spot inside that keeps the body where it was put
export const POUR_RANGE = 3
// the ray is sampled this often along its length
const STEP = 0.05

const key = ({ x, y, z }) => `${x},${y},${z}`
const isSlab = name => /_slab$/.test(String(name))
const isFenceLike = name => /_fence$|_wall$|_fence_gate$/.test(String(name))
// the eight-stage crops stand (age + 1) / 8 of a block high; everything else that grows is taken as a full block
const STAGED = new Set(['wheat', 'carrots', 'potatoes', 'beetroots', 'melon_stem', 'pumpkin_stem'])

// does this block fill the point `dy` (0..1) up its own cell? null (not loaded) fills everything
const fills = (block, dy) => {
  if (!block) return true
  if (isAir(block.name) || FLUIDS.has(block.name)) return false
  if (isSlab(block.name)) {
    const half = block.properties?.type
    return half === 'top' ? dy >= 0.5 : half === 'bottom' ? dy < 0.5 : true
  }
  if (STAGED.has(block.name)) return dy < Math.min(1, (Number(block.properties?.age ?? 7) + 1) / 8)
  if (/_carpet$/.test(block.name)) return dy < 1 / 16
  return true
}

// where the feet really are over this floor: a bottom slab holds the body half a block lower than its cell says
const feetY = (blockAt, { x, y, z }) => {
  const floor = blockAt(x, y - 1, z)
  return isSlab(floor?.name) && floor.properties?.type === 'bottom' ? y - 0.5 : y
}

// can eyes at `from` (a feet cell) see the centre of the TOP face of `target`? The line is sampled from the eyes to just
// short of the face; the eyes' own cell is not judged (the body stands in it), and a fence a cell below reaches up into it
export const seesTop = (blockAt, from, target, eye = EYE) => {
  const start = { x: from.x + 0.5, y: feetY(blockAt, from) + eye, z: from.z + 0.5 }
  const end = { x: target.x + 0.5, y: target.y + 1, z: target.z + 0.5 }
  const length = Math.hypot(end.x - start.x, end.y - start.y, end.z - start.z)
  if (length > REACH || start.y <= end.y) return false
  const head = key({ x: from.x, y: from.y + 1, z: from.z })
  const steps = Math.ceil(length / STEP)
  for (let i = 1; i < steps; i++) {
    const t = i / steps
    const p = { x: start.x + (end.x - start.x) * t, y: start.y + (end.y - start.y) * t, z: start.z + (end.z - start.z) * t }
    const cell = { x: Math.floor(p.x), y: Math.floor(p.y), z: Math.floor(p.z) }
    if (key(cell) === head) continue
    const dy = p.y - cell.y
    if (fills(blockAt(cell.x, cell.y, cell.z), dy)) return false
    if (dy < 0.5 && isFenceLike(blockAt(cell.x, cell.y - 1, cell.z)?.name)) return false
  }
  return true
}

// the cells a walk can end in, as walk.mjs judges them, except that a crop is never a floor (test worlds call everything
// that is not air solid; a body stands in a crop's cell, never on it)
const cellAtOf = blockAt => (x, y, z) => {
  const cell = cellOf(blockAt(x, y, z))
  return cell && { ...cell, solid: cell.solid && !cell.crop }
}

// every cell a job could stand in to work `target`, best first: nearest across, a floor that is not a bed before a
// harvested bed, higher before lower, then west to east and north to south among equals. Never the target's own column
// (the body would stand in the hole it pours into, or on the bed it plants), never water, never a crop's cell. With
// `see` (the default) only the cells whose eyes see the target's top face
export const standingSpots = ({ target, blockAt, range = WORK_RANGE, eye = EYE, see = true }) => {
  const cellAt = cellAtOf(blockAt)
  const across = cell => Math.hypot(cell.x - target.x, cell.z - target.z)
  const bed = cell => blockAt(cell.x, cell.y - 1, cell.z)?.name === 'farmland' ? 1 : 0
  return cellsWithin(target, range)
    .filter(cell => cell.x !== target.x || cell.z !== target.z)
    .filter(cell => dryStandable(cellAt, cell))
    .filter(cell => !see || seesTop(blockAt, cell, target, eye))
    .sort((a, b) => across(a) - across(b) || bed(a) - bed(b) || b.y - a.y || a.x - b.x || a.z - b.z)
    .map(({ x, y, z }) => ({ x, y, z }))
}

// the first of them, or why there is none
export const standingSpot = opts => {
  const [at] = standingSpots(opts)
  return at ? { at } : { why: noSpot(opts) }
}
const noSpot = ({ target, range = WORK_RANGE }) => `no cell to stand within ${range} of ${target.x},${target.y},${target.z} that sees its top`

// what a job has to see, and from how near: a pour names the block it pours ONTO; a place (a seed, a slab, a fence,
// dirt into a hole) names the cell the block goes in, so the top face under it is the one to see. A dig or a till has no
// such rule: the primitive walks itself
const SEEN = { pour: job => ({ at: job, range: POUR_RANGE }), place: job => ({ at: { x: job.x, y: job.y - 1, z: job.z }, range: WORK_RANGE }) }
export const jobSight = job => {
  const [action] = jobCall(job) ?? []
  return SEEN[action]?.(job) ?? null
}

// the answer that means the spot was wrong, not the job: a pour that looked at something else, a slab whose click landed elsewhere
const BLIND = /cannot see the top|did not take/

// do one job from a cell that sees it: walk there (range 0, the cell itself), call the primitive, and when it still
// answers blind, once more from the next spot. With no spot to offer (the target's chunks not loaded, or nothing in
// sight of it) the primitive is left to walk by itself, as before, and a blind answer then carries the reason
// `walk` is how the spot is reached: the plain goto by default; a farm sweep passes its own leg (src/farm/leg.mjs)
export async function workFrom (api, job, walk = spot => api.act('goto', { x: spot.x, y: spot.y, z: spot.z, range: 0 })) {
  const [action, args] = jobCall(job)
  const sight = jobSight(job)
  if (!sight) return api.act(action, args)
  const spots = standingSpots({ target: sight.at, blockAt: api.block, range: sight.range })
  const from = async spot => {
    if (spot) await walk({ x: spot.x, y: spot.y, z: spot.z })
    return api.act(action, args)
  }
  const first = await from(spots[0]).then(r => ({ r }), e => ({ e }))
  if (!first.e) return first.r
  if (!BLIND.test(first.e.message)) throw first.e
  if (!spots.length) throw new Error(`${noSpot({ target: sight.at, range: sight.range })}: ${first.e.message}`)
  if (!spots[1]) throw first.e
  return from(spots[1])
}
