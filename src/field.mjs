// Where to stand at a field, and in what order to put its seeds back (card f30fd998).
//
// A walk steps round crops, so the geometric centre of a dense field has no cell a walk can end in: farm.harvest place=
// asked for one and the search timed out. The place to go is the nearest cell the body CAN stand in that is within work
// range of some cell of the plan: the field's edge, or a lane through it.
//
// Planting needs a cell to stand in within reach of the cell planted, and every seed put down takes one away. In cut
// order the last cells of a row lost theirs (14 of 108 came back notReplanted on jizo-melon-patch). Far end first, the
// cells still bare are always the ones nearer the standing cell: there is one to stand in within reach of the next, and
// the body backs out of the field as it plants.
import { WORK_RANGE, dryStandable, cellsWithin, noStanding } from './walk.mjs'
import { breaksUnderfoot, FLUIDS } from './lib.mjs'

// what api.block answers, read as the cell walk.mjs judges: solid is what a walk cannot enter, crop is what it steps round
export const cellOf = block => block && { name: block.name, solid: Boolean(block.solid), liquid: FLUIDS.has(block.name), crop: breaksUnderfoot(block.name) }

const key = c => `${c.x},${c.y},${c.z}`
const across = (a, b) => Math.hypot(a.x - b.x, a.z - b.z)

// every cell within work range of what stands on some cell of the plan (a plan's y is the ground, the crop is at y+1)
const candidates = cells => {
  const seen = new Map()
  cells.flatMap(c => cellsWithin({ x: c.x, y: c.y + 1, z: c.z }, WORK_RANGE)).forEach(c => seen.set(key(c), c))
  return [...seen.values()]
}

// the nearest cell to `from` the body can stand in, dry, within work range of the plan, and how far it must reach from there
// to cover every cell of the plan; null when the plan is empty or nothing within reach of it can be stood in (not loaded, all
// planted). Dry: a pond beside the field is where a walk can end and drops float off, not where a harvest stands
export const fieldEdge = (cellAt, cells, from) => {
  const spots = candidates(cells).filter(c => dryStandable(cellAt, c))
  if (!spots.length) return null
  const distance = c => Math.hypot(c.x - from.x, c.y - from.y, c.z - from.z)
  const spot = spots.reduce((best, c) => distance(c) < distance(best) ? c : best)
  return { ...spot, span: Math.ceil(Math.max(...cells.map(c => across(c, spot)))) + 2 }
}

export const standingLine = spot => `standing at ${spot.x},${spot.y},${spot.z}, plan's edge`

// the cut cells far end first from where the body stands, rows north to south and west to east among equals
export const plantOrder = (cut, stand) => {
  if (!stand) return cut
  return [...cut].sort((a, b) => across(b, stand) - across(a, stand) || a.z - b.z || a.x - b.x)
}

// only what stands over a cell of the plan is the field's: the neighbour's rows a few blocks off are theirs
export const fieldCrops = (cells, positions) => {
  if (!cells) return positions
  const mine = new Set(cells.map(c => `${c.x},${c.z}`))
  return positions.filter(p => mine.has(`${p.x},${p.z}`))
}

// why a cut cell is still bare after the replant: nowhere to stand within reach of it, or the place itself failed. The
// bare cell is a standing cell too, and the one a body cannot plant from: it reads as planted here (not as a block: that
// would give the cell above it a floor)
export const bareReason = (cellAt, cell) => {
  const butItself = (x, y, z) => x === cell.x && y === cell.y && z === cell.z ? { name: 'the cell itself', solid: false, crop: true } : cellAt(x, y, z)
  return noStanding(butItself, cell, WORK_RANGE) ? 'no standing cell within reach' : 'the place failed twice (out of reach, or I stood in it)'
}

// which cells stayed bare and why, grouped by reason, the first three of each named
export const notReplantedLine = failed => {
  if (!failed.length) return null
  const reasons = [...new Set(failed.map(f => f.why))]
  const named = cells => `${cells.slice(0, 3).map(c => `${c.x},${c.z}`).join(' ')}${cells.length > 3 ? ` +${cells.length - 3} more` : ''}`
  return `${failed.length}: ${reasons.map(why => `${why} at ${named(failed.filter(f => f.why === why))}`).join('; ')}`
}
