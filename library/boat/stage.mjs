import { observePassengerBoat } from '../../src/boat/observe.mjs'
import { villagerBoatRoute } from '../../src/lib.mjs'
import { entityUuid } from '../../src/boat/passenger.mjs'

const water = b => ['water', 'bubble_column', 'seagrass', 'tall_seagrass', 'kelp', 'kelp_plant'].includes(b?.name)
const position = boat => {
  const [x, y, z] = boat.exact.split(',').map(Number)
  return { x, y, z }
}
const gap = (a, b) => Math.hypot(a.x - b.x, a.z - b.z)

export default {
  doc: 'boat.stage uuid= boat= z= [minX= tolerance=0.3 timeout=90 startX= startY= startZ= plan=true]: align a boat carrying a supported adult villager, cow, sheep or pig in open water before an east-facing dock approach; an optional shallow start stance exits the dock without adding tension; checks actual boat position, reduces lead tension, and requires stable alignment',
  stops: 'the exact passenger boat is stably aligned with slack lead in open water, or terrain, leash, progress, or timeout prevents it',
  args: { uuid: 'string!', boat: 'number!', z: 'number!', minX: 'number', tolerance: 'number', timeout: 'number', startX: 'number', startY: 'number', startZ: 'number', plan: 'boolean' },
  async run (api, a) {
    const tolerance = a.tolerance ?? 0.3
    const timeout = a.timeout ?? 90
    if (!entityUuid(a.uuid) || !Number.isInteger(a.boat) || !Number.isFinite(a.z)) throw new Error('stage needs an exact uuid=, integer boat= and finite z=')
    if (a.minX !== undefined && !Number.isFinite(a.minX)) throw new Error('minX= must be finite')
    if (!Number.isFinite(tolerance) || tolerance < 0.1 || tolerance > 0.3 || !Number.isFinite(timeout) || timeout < 10 || timeout > 180) throw new Error('tolerance= must be 0.1..0.3 and timeout= 10..180')
    const startGiven = [a.startX, a.startY, a.startZ].some(v => v !== undefined)
    const start = startGiven ? { x: a.startX, y: a.startY, z: a.startZ } : null
    if (start && (!Object.values(start).every(Number.isInteger) || !water(api.block(start.x, start.y, start.z)) || !api.block(start.x, start.y - 1, start.z)?.solid || !api.block(start.x, start.y + 1, start.z) || api.block(start.x, start.y + 1, start.z).solid || water(api.block(start.x, start.y + 1, start.z)))) throw new Error('stage startX/Y/Z needs a loaded shallow water stance with solid floor and clear air above')
    let passengerName
    const checked = async () => {
      const { boat } = await observePassengerBoat(api, a.boat, a.uuid, { lead: 'self' })
      passengerName = boat.passengers[0].name
      const p = position(boat)
      if (a.minX !== undefined && p.x < a.minX) throw new Error(`boat moved west of the safe staging boundary x=${a.minX}; stop before the dock wall`)
      if (!water(api.block(Math.floor(p.x), Math.floor(p.y), Math.floor(p.z)))) throw new Error('staging requires the boat afloat in loaded open water')
      if (gap(api.pos(), p) > 8) throw new Error('boat is too far behind for a staging stroke')
      return p
    }
    const routeCheck = p => {
      const route = villagerBoatRoute({ from: p, to: { ...p, z: a.z }, margin: 2, blockAt: (x, y, z) => api.block(x, y, z) })
      if (route.error) throw new Error(`staging hull blocked at ${route.at.x},${route.at.y},${route.at.z}: ${route.error}`)
      return route
    }
    let p = await checked()
    if (start && gap(start, p) > 5.5) throw new Error('stage start stance is too far from the boat to approach without additional lead tension')
    const route = routeCheck(p)
    if (a.plan === true) return { planned: true, aligned: Math.abs(p.z - a.z) <= tolerance, route: route.points, ...(start ? { start } : {}), secure: false }
    if (start) {
      await api.act('goto', { ...start, range: 0, into: true })
      p = await checked()
      routeCheck(p)
    }
    let previous = p
    let stable = 0
    let strokes = 0
    await api.until(async () => {
      p = await checked()
      routeCheck(p)
      const aligned = Math.abs(p.z - a.z) <= tolerance
      const slack = gap(api.pos(), p) <= 5.5
      stable = aligned && slack && gap(p, previous) < 0.03 ? stable + 1 : 0
      previous = p
      if (stable >= 4) return true
      if (aligned && slack) return false
      // Remove tension once aligned, so delayed server spring motion cannot
      // turn a transient in-tolerance observation into a false success.
      const body = api.pos()
      const target = { x: p.x + (aligned ? Math.sign(body.x - p.x) || 1 : 1) * 2, y: Math.floor(p.y), z: aligned ? p.z : p.z + Math.sign(a.z - p.z) * 7.5 }
      const n = Math.max(1, Math.ceil(gap(body, target) * 4))
      for (let i = 0; i <= n; i++) {
        const x = body.x + (target.x - body.x) * i / n
        const z = body.z + (target.z - body.z) * i / n
        for (const dx of [-0.3, 0.3]) for (const dz of [-0.3, 0.3]) {
          const b = api.block(Math.floor(x + dx), Math.floor(p.y), Math.floor(z + dz))
          const head = api.block(Math.floor(x + dx), Math.floor(p.y) + 1, Math.floor(z + dz))
          if (!water(b) || !head || head.solid || water(head) || ['lava', 'fire', 'soul_fire'].includes(head.name)) throw new Error(`staging leader corridor is not clear open water at ${Math.floor(x + dx)},${Math.floor(p.y)},${Math.floor(z + dz)}`)
        }
      }
      // The boat can still be catching up after the swimmer arrives. Keep
      // observing under the same bounded wait rather than steering at an
      // endpoint that the primitive correctly rejects as too close.
      if (gap(body, target) <= 0.45) return false
      await api.act('boat_swim', { ...target, ms: 500 })
      p = await checked()
      if (++strokes > 120) throw new Error('boat staging exceeded its bounded stroke count')
      api.report({ boatAt: `${p.x},${p.y},${p.z}`, alignmentError: Math.abs(p.z - a.z) })
      return false
    }, { timeout, every: 0.5, what: `boat ${a.boat} did not settle aligned in open water` })
    return { boat: a.boat, ...(passengerName === 'villager' ? { villagerUuid: a.uuid } : { passengerUuid: a.uuid, kind: passengerName }), at: `${p.x},${p.y},${p.z}`, aligned: true, leashHeld: true, passengerSeated: true, secure: false }
  }
}
