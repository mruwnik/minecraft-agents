// Get that many of a block: find them, dig them one at a time (dig picks up its own drop), and leave the place tidy.
// What it will not touch: anything inside a protected zone (someone's build, mine included), anything in or beside
// water (bodies drown fetching sand off a lake bed), and the ground it stands on when animals are penned around it.
import { mineTargets, inAnyZone, holesLeft, penShaftRefusal } from '../../src/lib.mjs'

const MAX = 48
const ROUNDS = 6
const AROUND = [-2, -1, 0, 1, 2]

export default {
  doc: 'mine.get block= count= [maxDistance=48] [wet=] [rounds=6]: find, dig and pick up that many of a block, leaving protected builds and anything wet alone',
  stops: 'count= reached, nothing left it may take, six rounds, or a full inventory',
  args: { block: 'string!', count: 'number', maxDistance: 'number', wet: 'boolean', rounds: 'number', force: 'boolean' },

  async run (api, a) {
    const want = a.count ?? 1
    const maxDistance = a.maxDistance ?? MAX
    const what = `${a.block} within ${maxDistance} blocks`
    // dig picks its own drop up, and a pickup with nowhere to go leaves the block lying in the pit
    if (!api.freeSlots()) throw new Error('inventory full: mine.get needs one free slot. Drop or deposit something first')
    const pen = api.pen()
    const shaft = penShaftRefusal(pen?.enclosed ? pen.census?.inside : undefined, a.force)
    if (shaft) throw new Error(shaft)

    // the ground around the start, to mend the shaft mouth afterwards: a pit at my own front door is nobody's idea of tidy
    const start = api.pos()
    const groundCells = AROUND.flatMap(dx => AROUND.flatMap(dz => [-2, -1].map(dy => ({ x: Math.floor(start.x) + dx, y: Math.floor(start.y) + dy, z: Math.floor(start.z) + dz }))))
    const ground = () => Object.fromEntries(groundCells
      .map(c => [`${c.x},${c.y},${c.z}`, api.block(c.x, c.y, c.z)?.name])
      .filter(([, name]) => name !== a.block))
    const groundBefore = ground()

    const nameAt = p => api.block(p.x, p.y, p.z)?.name
    // water beside it, or anywhere in the 3 blocks above: sand under a lake bed is as wet as the bed itself
    const wet = p => [[0, 1, 0], [0, 2, 0], [0, 3, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]]
      .some(([dx, dy, dz]) => nameAt({ x: p.x + dx, y: p.y + dy, z: p.z + dz }) === 'water')

    let got = 0
    let rounds = 0
    let skippedWet = 0
    let gaveUp = null
    while (got < want && rounds < (a.rounds ?? ROUNDS)) {
      rounds++
      // ask for extra: some of what comes back is in a zone, or wet, or simply out of reach
      const { positions = [] } = await api.act('find_blocks', { block: a.block, maxDistance, count: want - got + 16 })
      if (!positions.length && !rounds - 1) throw new Error(`no ${what}`)
      const { zones = [] } = await api.act('zones')
      const choice = mineTargets({ nearby: positions, wanted: want - got, inZone: p => inAnyZone(zones, p), wet, allowWet: a.wet === true, what })
      if (choice.error) { gaveUp = choice.error; break }
      skippedWet = Math.max(skippedWet, choice.skippedWet ?? 0)
      let dug = 0
      for (const p of choice.found) {
        const failed = await api.act('dig', { x: p.x, y: p.y, z: p.z, dig: true, ...(a.wet === true ? { wet: true } : {}) }).then(() => null, e => e.message)
        gaveUp = failed ? gaveUp ?? failed : gaveUp
        if (!failed && nameAt(p) !== a.block) { got++; dug++ }
        api.report({ got })
        await api.checkpoint()
      }
      // a whole round that moved nothing: looking again will find the same blocks and fail the same way
      if (!dug) break
      gaveUp = got >= want ? null : gaveUp
    }

    const summary = {
      got,
      rounds,
      gaveUp: got >= want ? undefined : gaveUp ?? `nothing more within ${maxDistance} blocks`,
      skippedWet: skippedWet ? `${skippedWet} lay in or by water and were left alone (wet=true takes them; safer: dig x= y= z= from the shore)` : undefined
    }

    // mining ends in the pit it dug, and a walk (which may not dig) cannot leave one: get back to where I started
    const home = { x: Math.floor(start.x), y: Math.floor(start.y), z: Math.floor(start.z), range: 2 }
    const { status } = await api.act('path_to', home)
    if (status !== 'success') {
      const stuck = await api.act('goto', home).then(() => null, e => e)
      if (stuck) return { ...summary, pit: 'you are in the pit you dug and could not get back out: goto with dig=true' }
      summary.climbedOut = 'back where you started'
    }

    const holes = holesLeft(groundBefore, ground(), Object.keys(api.inv()), api.solid).sort((p, q) => p.y - q.y)
    if (!holes.length) return summary
    const { placed = 0 } = await api.act('place', { blocks: holes }).catch(() => ({ placed: 0 }))
    return { ...summary, mended: `${placed} of ${holes.length} blocks of the ground you broke open at the start put back${placed < holes.length ? ': fill the rest by hand (scan around the start)' : ''}` }
  }
}
