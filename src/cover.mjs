// Capping a channel: the pure judgements around laying a slab into a plan's water cell. The rules come from the human
// (09-24): a TOP slab over water walks level with the ground and still counts as water (waterlogged, so the farmland
// beside it stays wet), where a bottom slab is a half-step down into every channel that bodies float and wedge on;
// and a slab belongs only over a settled source, since one dropped into flowing water is not waterlogged and cuts the
// flow. lib.mjs's hasWaterSource decides what is a source; farmJobs decides the jobs; this is the rest.
import { PLAN_LEGEND, holdsWater } from './lib.mjs'

// why a flowing cell gets no slab: what farmJobs says when it skips one for want of a bucket
export const FLOW_REASON = ({ x, y, z }) => `flowing water at ${x},${y},${z}: pour a source first, then cover`

// The six neighbours a block can be placed against, in the order `place` tries them. Vanilla reads the cursor height
// only on a SIDE face: a click on the top of the block below always gives a bottom slab, a click on the underside of
// the block above always a top one. So the neighbour that fixes the wrong half goes last, and the one that fixes the
// right half first. A plain block keeps the old order (the block below first: it is the one most often there).
const BELOW = [0, -1, 0]
const ABOVE = [0, 1, 0]
const SIDES = [[1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]]
export const facesForHalf = half => {
  if (half === 'top') return [ABOVE, ...SIDES, BELOW]
  if (half === 'bottom') return [BELOW, ...SIDES, ABOVE]
  return [BELOW, ABOVE, ...SIDES]
}

// The channel cells capped the old way: a waterlogged BOTTOM slab. They hold their water and farm.maintain leaves them
// alone (no churn), but they are counted so the driver knows the field walks worse than it could.
const isLowSlab = block => Boolean(block) && /_slab$/.test(block.name) && holdsWater(block) && block.properties?.type !== 'top'
export const lowSlabs = (cells, worldAt) =>
  cells.filter(c => PLAN_LEGEND[c.ch]?.kind === 'water' && isLowSlab(worldAt(c.x, c.y, c.z)))
export const lowSlabLine = n => `${n} (bottom slabs: top slabs walk better; dig and cover again to raise)`

// A cell already covered - a waterlogged slab, top half or bottom - is a finished channel: farmJobs must never touch
// it again. A cover job aimed at one anyway is exactly how a channel cell still holding an old bottom slab (from
// before the top-slab cards) merged into a double slab and sealed for good (jizo-melon-patch, 09-26). Named here so
// farmJobs needs only call it, not carry the boolean itself; a DRY slab is not covered by this - it is a broken
// channel, dug and repoured (see farmJobs's own tests in test/lib.test.mjs for that path).
export const channelCovered = block => Boolean(block) && holdsWater(block) && block.name !== 'water'
