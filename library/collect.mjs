// Pick up what lies on the ground: walk onto each drop in turn, nearest first, and say what had to be left.
// Inside a pen it takes only what lies inside it: a walk out after a drop beyond the fence takes the herd with you.
import { nextDrop, leftLying } from '../src/lib.mjs'

const RANGE = 16
const ROUNDS = 40

export default {
  doc: 'collect [range=16] [wet=]: pick up everything lying on the ground nearby, one drop at a time',
  stops: 'nothing left within reach, or a full inventory that will not stack',
  args: { range: 'number', wet: 'boolean' },

  async run (api, a) {
    const range = a.range ?? RANGE
    const wet = a.wet === true
    const mine = () => api.drops(range).filter(drop => !drop.outsidePen)
    const tried = new Set()
    let full = false
    for (let round = 0; round < ROUNDS && !full; round++) {
      const drop = nextDrop(mine(), tried, wet)
      if (!drop) break
      tried.add(drop.id)
      await api.act('goto', { x: drop.x, y: drop.y, z: drop.z, range: 0 }).catch(() => {})
      await api.pause(0.5)
      // full, and it did not stack either: the rest will not come
      full = api.freeSlots() === 0 && mine().some(left => left.id === drop.id)
      api.report({ picked: tried.size })
      await api.checkpoint()
    }
    const left = mine()
    const outside = api.drops(range).length - left.length
    return {
      ...leftLying(api.freeSlots(), left.map(drop => drop.item), wet ? [] : left.filter(drop => drop.deep).map(drop => drop.item)),
      picked: tried.size,
      ...(outside ? { outsidePen: `${outside} drops lie outside this pen and were left: walk out yourself (goto), then collect again` } : {})
    }
  }
}
