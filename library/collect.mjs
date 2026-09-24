// Pick up what lies on the ground: walk onto each drop in turn, nearest first, and say what had to be left.
// Inside a pen it takes only what lies inside it: a walk out after a drop beyond the fence takes the herd with you.
import { nextDrop, leftLying, collectTally } from '../src/lib.mjs'

const RANGE = 16
const ROUNDS = 40

export default {
  doc: 'collect [range=16] [wet=]: pick up everything lying on the ground nearby, one drop at a time. picked= counts what actually left the ground, and whatever is still lying is named where it lies',
  stops: 'nothing left within reach, or a full inventory that will not stack',
  args: { range: 'number', wet: 'boolean' },

  async run (api, a) {
    const range = a.range ?? RANGE
    const wet = a.wet === true
    const all = () => api.drops(range)
    const mine = () => all().filter(drop => !drop.outsidePen)
    const seen = new Set()
    const tried = []
    let full = false
    for (let round = 0; round < ROUNDS && !full; round++) {
      const drop = nextDrop(mine(), seen, wet)
      if (!drop) break
      seen.add(drop.id)
      // whether the walk arrived is the whole difference between "no cell to stand on beside it" and "I stood on it and
      // it would not come to hand", and those want different things from the driver, so it is remembered here (#142)
      const reached = await api.act('goto', { x: drop.x, y: drop.y, z: drop.z, range: 0 }).then(() => true, () => false)
      tried.push({ ...drop, reached })
      await api.pause(0.5)
      // full, and it did not stack either: the rest will not come
      full = api.freeSlots() === 0 && mine().some(left => left.id === drop.id)
      // only the count runs live: a drop that is stuck this round may be gone the next, and report= never un-says a key
      api.report({ picked: collectTally(tried, all()).picked })
      await api.checkpoint()
    }
    const left = mine()
    const outside = all().length - left.length
    return {
      ...leftLying(api.freeSlots(), left.map(drop => drop.item), wet ? [] : left.filter(drop => drop.deep).map(drop => drop.item)),
      ...collectTally(tried, all()),
      ...(outside ? { outsidePen: `${outside} drops lie outside this pen and were left: walk out yourself (goto), then collect again` } : {})
    }
  }
}
