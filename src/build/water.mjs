// Building a plan over water (card 013d4477).
//
// A plan laid over a dip holds water where its cells should be: pooled a block low along a channel row, or a pond
// where the beds go. The levelling jobs (a floor under every cell, open air over it) come out of groundJobs in plan
// order, which is not the order they can be DONE in: a fill is worked from dry footing, and in a wet interior the only
// dry footing is the shore, so a fill in the middle first is a swim, a "cannot see the top", or a walk with no cell
// to end in. And water standing IN a cell was no job at all to groundJobs (a fluid is not solid), so it stayed: dig
// refuses every block under it, and farm.maintain on Chani's channels (09-23) gave up on "dig: that block is under
// water" twice in a row.
//
// Three pure judgements. The dams: dirt into every fluid cell of a plan column, which removes the sources. The shore
// order: fills and dams in an order where each one has a dry cell to stand on that sees it, counting the ones before
// it as ground, so the work goes from the shore inward. The reopening: the dams the plan wants as air again, dug once
// nothing beside them can flow back in; a dam with water outside the plan against it stays, named with that cell.
import { FLUIDS } from '../lib.mjs'
import { cellsWithin } from '../navigation/walk.mjs'
import { cellOf } from '../farm/field.mjs'
import { jobSight, standingSpots } from '../navigation/stand.mjs'
import { PLAN_LEGEND } from '../lib/plan.mjs'

const key = ({ x, y, z }) => `${x},${y},${z}`
const SIDES = [[1, 0], [-1, 0], [0, 1], [0, -1]]
const fluid = block => Boolean(block) && FLUIDS.has(block.name)
const DIRT = { name: 'dirt', properties: {}, solid: true }

// dirt into every fluid cell over a plan cell (the cell itself and the one over it), as fill jobs flagged dam: true.
// The floor under a cell is groundJobs' own fill; a channel's water cell is water by design and is never dammed
export const drainJobs = (cells, worldAt) => cells.filter(cell => PLAN_LEGEND[cell.ch]?.kind !== 'water').flatMap(cell =>
  [cell.y + 1, cell.y + 2]
    .filter(y => fluid(worldAt(cell.x, y, cell.z)))
    .map(y => ({ do: 'fill', dam: true, x: cell.x, y, z: cell.z, why: `${worldAt(cell.x, y, cell.z).name} stands in the cell`, item: 'dirt' })))

// the dry cells a job can be worked from, in src/navigation/stand.mjs's terms but without its line of sight to the top face:
// a fill into a row of water is clicked onto the side of the dirt laid before it as readily as onto the floor under
// it, and from a shore two cells up the floor's top face is behind that dirt. The primitive picks the face; what a
// levelling job needs is footing in reach. A job with no sight rule (a dig, a till) walks by itself: null
const footing = (job, blockAt) => {
  const sight = jobSight(job)
  return sight && { ...sight, spots: standingSpots({ target: sight.at, blockAt, range: sight.range, see: false }) }
}
const workable = (job, blockAt) => {
  const has = footing(job, blockAt)
  return !has || has.spots.length > 0
}

// the jobs in an order they can be done in: whichever remaining job can be worked from dry footing goes next (the
// earliest in the given order among those), and once done its block counts as placed for the ones after it. A job
// nothing ever reaches keeps its place at the end, for the build to name
export const shoreOrder = (jobs, blockAt) => {
  const placed = new Map()
  const see = (x, y, z) => placed.get(`${x},${y},${z}`) ?? blockAt(x, y, z)
  const left = [...jobs]
  const out = []
  while (left.length) {
    const next = left.find(job => workable(job, see)) ?? left[0]
    left.splice(left.indexOf(next), 1)
    out.push(next)
    placed.set(key(next), DIRT)
  }
  return out
}

// which of the dams the plan wants as open air again, now that they stand: a dam with water on a side that is not
// another dam stays in and is named with the cell that would flow back. Top first, so no dig has water over it
export const reopenJobs = (dams, worldAt) => {
  const dammed = new Set(dams.map(key))
  const open = []
  const kept = []
  for (const dam of [...dams].sort((a, b) => b.y - a.y)) {
    const wet = SIDES.map(([dx, dz]) => ({ x: dam.x + dx, y: dam.y, z: dam.z + dz })).find(side => !dammed.has(key(side)) && fluid(worldAt(side.x, side.y, side.z)))
    if (wet) { kept.push(`${key(dam)} (water at ${key(wet)} would flow back in)`); continue }
    open.push({ do: 'clear', x: dam.x, y: dam.y, z: dam.z, why: 'the dam the plan wants as open air' })
  }
  return { open, kept }
}

// why a job cannot be worked now, or null: no dry cell in reach to work from, and water in reach is what keeps the
// body off - the nearest of it is what to fill or drain first. With no water in reach the job is not judged here
// (a plan at the edge of what is loaded has no footing on record either) and the primitive walks by itself, as before
export const wetFooting = (job, blockAt) => {
  const has = footing(job, blockAt)
  if (!has || has.spots.length) return null
  const across = cell => Math.hypot(cell.x - has.at.x, cell.y - has.at.y, cell.z - has.at.z)
  const wet = cellsWithin(has.at, has.range).filter(cell => cellOf(blockAt(cell.x, cell.y, cell.z))?.liquid).sort((a, b) => across(a) - across(b))[0]
  return wet ? `${key(job)}: no dry cell within ${has.range} of it to stand on, water at ${key(wet)}: fill or drain that first` : null
}
