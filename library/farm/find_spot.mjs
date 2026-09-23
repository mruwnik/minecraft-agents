// Where to put a farm. This reads the ground rather than walking it: the body's own copy of the world already holds
// every loaded chunk, so a search that would take ten minutes on foot takes a moment here. It scores every w x h patch
// in range for flatness, water, sky and distance (spotScore), refuses ground a zone or a saved plan already claims,
// then walks to the best one so you can `look` at it and decide. It never digs, tills or marks anything.
import { spotScore, bestSpots, inAnyZone, planCells } from '../../src/lib.mjs'

const RANGE = 48
const SIZE = 5
const DEPTH = 12
const WATER_REACH = 4
const AIR = /^(air|cave_air|void_air)$/
const NO_FLOOR = /^(water|lava|bubble_column|ice|frosted_ice|magma_block|powder_snow)$/

export default {
  doc: 'farm.find_spot [w=5] [h=5] [near=<place>] [range=48] [limit=3]: read the ground nearby and name the flattest, best-watered patches to farm, then stand on the best one',
  stops: 'nothing: it reads the world and walks to one spot',
  args: { w: 'number', h: 'number', near: 'string', range: 'number', limit: 'number' },

  async run (api, a) {
    const w = a.w ?? SIZE
    const h = a.h ?? SIZE
    const range = a.range ?? RANGE
    const at = a.near ? api.places().find(p => p.name === a.near) : api.pos()
    if (!at) throw new Error(`no place called ${a.near}: places q=${a.near} searches the map`)
    const from = { x: Math.floor(at.x), y: Math.floor(at.y), z: Math.floor(at.z) }
    const { zones = [] } = await api.act('zones', {})
    const claimed = api.places().filter(p => p.plan).flatMap(p => planCells(p)).map(c => `${c.x},${c.z}`)
    const taken = new Set(claimed)

    // the surface of one column: the highest solid block with air over it, within DEPTH of where we started looking
    const top = (x, z) => {
      for (let y = from.y + DEPTH; y > from.y - DEPTH; y--) {
        const here = api.block(x, y, z)
        if (!here || AIR.test(here.name)) continue
        if (NO_FLOOR.test(here.name)) return null
        return AIR.test(api.block(x, y + 1, z)?.name ?? 'air') ? y : null
      }
      return null
    }
    const tops = new Map()
    const topAt = (x, z) => {
      const key = `${x},${z}`
      if (!tops.has(key)) tops.set(key, top(x, z))
      return tops.get(key)
    }
    // every column is read ONCE: the naive search asks the same column thousands of times, and a 48-block range is 9,000 of them
    const wets = new Map()
    const wet = (x, z) => {
      const key = `${x},${z}`
      if (!wets.has(key)) {
        let found = false
        for (let y = from.y + DEPTH; y > from.y - DEPTH && !found; y--) found = api.block(x, y, z)?.name === 'water'
        wets.set(key, found)
      }
      return wets.get(key)
    }
    const skies = new Map()
    const sky = (x, z) => {
      const key = `${x},${z}`
      if (!skies.has(key)) skies.set(key, AIR.test(api.block(x, (topAt(x, z) ?? from.y) + 2, z)?.name ?? 'air'))
      return skies.get(key)
    }

    const scored = []
    for (let x = from.x - range; x + w <= from.x + range; x++) {
      for (let z = from.z - range; z + h <= from.z + range; z++) {
        const cells = []
        for (let dx = 0; dx < w; dx++) for (let dz = 0; dz < h; dz++) cells.push([x + dx, z + dz])
        const claimedHere = cells.some(([cx, cz]) => taken.has(`${cx},${cz}`)) ||
          cells.some(([cx, cz]) => inAnyZone(zones, { x: cx, y: topAt(cx, cz) ?? from.y, z: cz }))
        const patch = spotScore({
          tops: cells.map(([cx, cz]) => topAt(cx, cz)),
          taken: claimedHere,
          water: cells.some(([cx, cz]) => [[0, 0], [WATER_REACH, 0], [-WATER_REACH, 0], [0, WATER_REACH], [0, -WATER_REACH]].some(([ox, oz]) => wet(cx + ox, cz + oz))),
          sky: cells.every(([cx, cz]) => sky(cx, cz)),
          away: Math.hypot(x + (w - 1) / 2 - from.x, z + (h - 1) / 2 - from.z)
        })
        if (patch) scored.push({ ...patch, x, z })
      }
      await api.checkpoint()
    }

    const best = bestSpots(scored, a.limit ?? 3)
    if (!best.length) throw new Error(`no ${w}x${h} patch of open ground within ${range} of ${from.x},${from.y},${from.z}: try a smaller w= h=, a wider range=, or near= somewhere else`)
    const pick = best[0]
    await api.act('goto', { x: pick.x, y: pick.y + 1, z: pick.z, range: 2 }).catch(e => api.note(`could not walk to ${pick.x},${pick.y},${pick.z}: ${e.message}`))
    return {
      size: `${w}x${h}`,
      // the anchor a plan would use: the NORTH-WEST corner at GROUND level, ready for farm.plan x= y= z=
      best: `${pick.x},${pick.y},${pick.z}`,
      spots: best.map(s => `${s.x},${s.y},${s.z} score=${s.score} level=${s.level}% work=${s.work}${s.water ? ' water' : ' DRY'}${s.sky ? '' : ' roofed'} ${s.away}m`).join('; '),
      note: `these are the plan anchors (north-west corner, ground level): look at the ground before you build. ${pick.water ? '' : 'The best one has no water within 4: carry a bucket, or dig a channel. '}farm.plan name=... x=${pick.x} y=${pick.y} z=${pick.z} map='...'`
    }
  }
}
