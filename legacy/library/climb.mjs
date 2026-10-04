// Out of a 1-wide shaft by hand: a niche dug to the side at head height, a block placed under the feet from it, a step up,
// and again with the columns swapped (card 2b2d1f65: what got a body out when goto dig=true dug further down and place at
// its own feet found no cell to place from). The ladder is src/navigation/climb.mjs; this runs it with dig, place and goto.
import { climbShaft, climbBlocks, inPocket } from '../src/navigation/climb.mjs'

// how far a climb with no y= may go before it stops to ask: a shaft deeper than this is a mine, not a hole
const MAX_CLIMB = 24
const key = ({ x, y, z }) => `${x},${y},${z}`
const floored = ({ x, y, z }) => ({ x: Math.floor(x), y: Math.floor(y), z: Math.floor(z) })

export default {
  doc: 'climb [y=] [item=]: out of a 1-wide shaft by hand: dig a niche to the side at head height, place a block under the feet from it, step up, and again with the columns swapped, up to y= (default: until the shaft opens on two sides, at most 24 up). item= names the block to stand on (default: cobblestone, then dirt, then any full block carried)',
  stops: 'the feet reach y= or open ground, the blocks carried run out (short= says how many more), or a step does not land where the ladder said',
  args: { y: 'number', item: 'string' },

  async run (api, a) {
    const feetAt = () => floored(api.pos())
    const feet = feetAt()
    const goalY = a.y ?? feet.y + MAX_CLIMB
    const counts = api.inv()
    const carried = a.item ? climbBlocks({ [a.item]: counts[a.item] ?? 0 }, api.solid) : climbBlocks(counts, api.solid)
    const passable = (x, y, z) => { const cell = api.block(x, y, z); return Boolean(cell) && !cell.solid && cell.name !== 'lava' }
    const out = await climbShaft({
      feetAt,
      goalY,
      blockAt: api.block,
      carried,
      dig: cell => api.act('dig', { x: cell.x, y: cell.y, z: cell.z, batch: true }),
      place: block => api.act('place', { item: block.item, x: block.x, y: block.y, z: block.z }),
      step: async cell => {
        await api.act('goto', { x: cell.x, y: cell.y, z: cell.z, range: 0 })
        await api.checkpoint()
      },
      until: now => a.y === undefined && !inPocket((dx, dy, dz) => passable(now.x + dx, now.y + dy, now.z + dz))
    })
    return { climbed: out.climbed, side: out.side, from: key(out.from), to: key(out.to), dug: out.dug, placed: out.placed }
  }
}
