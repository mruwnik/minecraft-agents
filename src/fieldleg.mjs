// One walk of a farm sweep (card 72e49b3d).
//
// A field that has grown in has no lane the pathfinder likes: the walk to the field's edge, and the walk to the cell a
// pour or a plant is worked from, answered "no walkable path" or "the search ran out of time" every few sweeps on one
// carrot patch (09-26), and the driver's workaround was a goto dig=true a few cells further in by hand, then the sweep
// from there. A leg does that itself now: plain first, and on a path failure once more with dig=true, but only when
// both ends of the leg lie inside the plan's footprint (its cells and the ring they are worked from), so the digging
// stays at the field. It never digs a plan block: a dig walk refuses everything that looks built (farmland, crops,
// slabs, fences, gates, chests, composters, torches: looksBuilt in src/lib/world.mjs) wherever it walks. Both walks
// failing is one line naming the cell, which the sweep reports as stuck=.
import { WORK_RANGE } from './walk.mjs'

// how far round the plan's cells a sweep stands to work them: the ring a dig retry may cut through
export const RING = Math.ceil(WORK_RANGE)

// the plan's footprint: the x/z box round its cells, grown by the ring; any depth
export const footprintOf = (cells, ring = RING) => ({
  x1: Math.min(...cells.map(c => c.x)) - ring,
  z1: Math.min(...cells.map(c => c.z)) - ring,
  x2: Math.max(...cells.map(c => c.x)) + ring,
  z2: Math.max(...cells.map(c => c.z)) + ring
})
export const inFootprint = (box, at) => Math.floor(at.x) >= box.x1 && Math.floor(at.x) <= box.x2 && Math.floor(at.z) >= box.z1 && Math.floor(at.z) <= box.z2

// the failures a second walk with dig=true can answer: the search found no way, or ran out of time looking, or was
// refused before it started because nothing in range of the goal can be stood in
export const PATH_FAILURE = /no walkable path|ran out of time|no path to the goal|nowhere to stand|took to long/i

const at = ({ x, y, z }) => `${Math.floor(x)},${Math.floor(y)},${Math.floor(z)}`

// null when a leg that failed with `error` is worth walking again with dig=true; a reason when it is not; undefined when
// the failure is not about the path at all (the error stands as the primitive gave it)
export const digRetryRefusal = ({ error, from, to, box }) => {
  if (!PATH_FAILURE.test(error)) return undefined
  if (!inFootprint(box, to)) return `${at(to)} is outside the plan's footprint`
  if (!inFootprint(box, from)) return `I stand outside the plan's footprint, at ${at(from)}`
  return null
}

// the cells a dig walk round a plan may never break, whatever stands there: every plan cell at its ground level and
// the level above it (the crop, the fence, the slab), for goto spare=. looksBuilt guards what looks built wherever a
// dig walk goes; this guards the plan's dirt path cells and the ground under its beds as well
export const spareCells = cells => cells.flatMap(c => [{ x: c.x, y: c.y, z: c.z }, { x: c.x, y: c.y + 1, z: c.z }])

// walk one leg of a sweep to `to` (x, y, z, range). The result is the walk's own, with `dug` naming the cell when it
// took the second, digging walk to get there; that walk is told the cells to spare
export async function fieldLeg (api, to, box, spare = []) {
  const goal = { x: to.x, y: to.y, z: to.z, range: to.range ?? 0 }
  const first = await api.act('goto', goal).then(r => ({ r }), e => ({ e: e.message }))
  if (!first.e) return first.r
  const refusal = digRetryRefusal({ error: first.e, from: api.pos(), to, box })
  if (refusal === undefined) throw new Error(first.e)
  if (refusal) throw new Error(`${at(to)}: ${first.e} (no dig=true retry: ${refusal})`)
  const second = await api.act('goto', { ...goal, dig: true, ...(spare.length ? { spare } : {}) }).then(r => ({ r }), e => ({ e: e.message }))
  if (second.e) throw new Error(`${at(to)}: ${first.e}; with dig=true inside the plan's footprint: ${second.e}`)
  return { ...second.r, dug: at(to) }
}
