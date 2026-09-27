// A bed whose ground is gone. farm.maintain assumed the ground stood and only farm.build partial=true put it back: on
// jizo-melon-patch (09-26) a sweep read 71 beds unreachable, and block_at showed AIR at the farmland level over dirt
// one below, all down the south rows (card 1ac82851; where the holes came from is card 94e6dcb1). So a sweep fills
// such beds first: a planned crop cell whose ground is air, or water the plan never asked for, gets its floor block
// back before its till and plant, from the pockets or the plan's chest. The same for a `.` lane cell and the ground
// under a chest or composter: a hole in the lane is the worst kind, since the sweep parks the body ON the lane, and a
// one-deep hole there is a trap it walks out of only at a cost. Pure judgements only; farm.maintain walks.
import { PLAN_LEGEND, planSpec, groundItem } from './plan.mjs'
import { isAir, isGroundCover } from './world.mjs'
import { itemShortfall } from './inventory.mjs'

// Compatibility export; the ground-material policy lives with the plan legend.
export const floorItem = groundItem
export const missingGround = name => isAir(name) || name === 'water' || isGroundCover(name)
// Maintain places the plan's structures as well as its crops, so it repairs their
// supporting ground too. A channel's own refill remains farmJobs's responsibility.

// the fill jobs of a plan: one per kept cell whose ground is missing, in plan order, each carrying the block it wants
// and whether any is carried (the way farmJobs marks its jobs). A cell with water standing OVER it is flooded, not a
// hole: dirt under standing water is still under water, and for a bed the sweep's water count says it. A cell nobody
// can see is nobody's job
export function holeJobs ({ cells, worldAt, items = {} }) {
  const jobs = []
  for (const cell of cells) {
    const spec = planSpec(cell)
    if (['tree', 'reserved'].includes(spec?.kind)) continue
    if (!spec || spec.kind === 'water') continue
    const ground = worldAt(cell.x, cell.y, cell.z)
    if (!ground || !missingGround(ground.name)) continue
    if (worldAt(cell.x, cell.y + 1, cell.z)?.name === 'water') continue
    const item = floorItem(spec)
    jobs.push({ do: 'fill', x: cell.x, y: cell.y, z: cell.z, item, have: (items[item] ?? 0) > 0, why: `${ground.name} where ${spec.ground} should be` })
  }
  return jobs
}

// what the fills want beyond the pockets, per floor block: what to ask the plan's chest for before the sweep
export function fillShortfall (jobs, items = {}) {
  const want = {}
  for (const { item } of jobs) want[item] = (want[item] ?? 0) + 1
  return itemShortfall(want, items)
}
