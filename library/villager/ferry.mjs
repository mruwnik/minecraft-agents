import { villagerBoatRoute, villagerBoatStatus, villagerTowWaypoints, villagerUuid } from '../../src/lib.mjs'

const at = boat => {
  const [x, y, z] = boat.exact.split(',').map(Number)
  return { x, y, z }
}
const distance = (a, b) => Math.hypot(a.x - b.x, a.z - b.z)

export default {
  doc: 'villager.ferry uuid= boat= x= y= z= [centerX= centerZ=] [pullX= pullY= pullZ=] [pullVia=x:y:z,x:y:z] [radius=0.8] [plan=true]: precheck the full 1.375-wide nonascending boat route, then tow with a lead; centerX/Z set an exact boat landing within 0.8 block of the named cell center; pullVia names short bot waypoints beyond it; plan=true only reads terrain',
  stops: 'the boat reaches the named bank with its villager and lead intact, or the tow stalls',
  args: { uuid: 'string!', boat: 'number!', x: 'number!', y: 'number!', z: 'number!', centerX: 'number', centerZ: 'number', pullX: 'number', pullY: 'number', pullZ: 'number', pullVia: 'string', radius: 'number', plan: 'boolean' },

  async run (api, a) {
    if (!villagerUuid(a.uuid)) throw new Error('uuid= must be the observed villager UUID')
    if (!Number.isInteger(a.boat) || ![a.x, a.y, a.z].every(Number.isInteger)) throw new Error('boat= and x= y= z= must be integers')
    const centerGiven = a.centerX !== undefined || a.centerZ !== undefined
    if (centerGiven && (![a.centerX, a.centerZ].every(Number.isFinite) || Math.abs(a.centerX - (a.x + 0.5)) > 0.8 || Math.abs(a.centerZ - (a.z + 0.5)) > 0.8)) throw new Error('centerX= and centerZ= must both be numbers within 0.8 block of the landing cell center')
    const landing = { x: centerGiven ? a.centerX : a.x + 0.5, y: a.y, z: centerGiven ? a.centerZ : a.z + 0.5 }
    const radius = a.radius ?? 0.8
    if (!Number.isFinite(radius) || radius < 0.5 || radius > 4) throw new Error('radius= must be 0.5..4 blocks')
    const pullGiven = [a.pullX, a.pullY, a.pullZ].some(v => v !== undefined)
    if (pullGiven && ![a.pullX, a.pullY, a.pullZ].every(Number.isInteger)) throw new Error('pullX= pullY= pullZ= must all be integers')
    const pull = pullGiven ? { x: a.pullX, y: a.pullY, z: a.pullZ } : a
    if (distance(pull, landing) > 8) throw new Error('bot pull point must be within eight horizontal blocks of the boat landing')
    const pullVia = a.pullVia === undefined ? [] : String(a.pullVia).split(',').map(raw => {
      const parts = raw.split(':').map(Number)
      if (parts.length !== 3 || !parts.every(Number.isInteger)) throw new Error('pullVia= needs comma-separated integer x:y:z waypoints')
      return { x: parts[0], y: parts[1], z: parts[2] }
    })
    if (pullVia.length > 8 || (pullVia.length && !pullGiven)) throw new Error('pullVia= needs pullX= pullY= pullZ= and at most eight waypoints')
    const beyondPath = [...pullVia, ...(pullGiven ? [pull] : [])]
    if (pullVia.length && beyondPath.some((p, i) => distance(p, i ? beyondPath[i - 1] : landing) > 3.5 || distance(p, landing) > 8)) throw new Error('pullVia/pull waypoints must be within 3.5 blocks of each other and eight blocks of the boat landing')
    const state = async () => api.act('boat_state', { id: a.boat })
    const checked = async () => {
      const s = await state()
      const issue = villagerBoatStatus(s, a.boat, a.uuid)
      if (issue) throw new Error(issue)
      if (s.mounted === a.boat) throw new Error(`dismount boat ${a.boat} before towing it`)
      return { s, boat: s.boats[0] }
    }
    let { s, boat } = await checked()
    if (boat.passengers.length !== 1) throw new Error(`boat ${a.boat} must carry only villager ${a.uuid} while being towed`)
    if (boat.leashHolderId !== null && boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} is leashed to somebody else`)
    // The bot's walk can climb a bank that the lower boat cannot. Check the
    // whole hull route before any leash or walking action.
    const route = villagerBoatRoute({
      from: at(boat),
      to: landing,
      blockAt: (x, y, z) => api.block(x, y, z)
    })
    if (route.error) throw new Error(`boat route blocked at ${route.at.x},${route.at.y},${route.at.z}: ${route.error}`)
    if (a.plan === true) return { boat: a.boat, villagerUuid: a.uuid, route: route.points, planned: true, secure: false }
    if (boat.leashHolderId === null) {
      await api.act('boat_leash', { id: a.boat })
      ;({ s, boat } = await checked())
    }
    if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} was not leashed to this bot`)

    const from = api.pos()
    if (distance(from, at(boat)) > 7) throw new Error(`boat ${a.boat} is ${distance(from, at(boat)).toFixed(1)} blocks from the bot before towing; approach it before starting`)
    // Join nearby half-block hull centers into short straight walking legs.
    // Keep every bend so the tow does not aim across an unchecked corner.
    const nearest = route.points.reduce((best, p, i) => distance(from, p) < best.dist ? { i, dist: distance(from, p) } : best, { i: 0, dist: Infinity }).i
    const planned = route.points.slice(nearest + 1)
    const turns = []
    let last = route.points[nearest]
    let heading = null
    for (const p of planned) {
      const nextHeading = `${Math.sign(p.x - last.x)},${Math.sign(p.z - last.z)}`
      if (heading && nextHeading !== heading && last) turns.push(last)
      // The boat settles roughly six blocks behind the bot on a taut lead.
      // Two-block legs leave room for that slack at bends before the 11-block
      // safety stop, including the one-block goto arrival range.
      if (distance(turns.at(-1) ?? route.points[nearest], p) >= 2) turns.push(p)
      heading = nextHeading
      last = p
    }
    if (planned.length && turns.at(-1) !== planned.at(-1)) turns.push(planned.at(-1))
    const beyond = pullVia.length ? beyondPath : pullGiven ? villagerTowWaypoints(route.points.at(-1), pull, 3.5) : []
    const points = [...turns, ...beyond]
    if (!points.length) throw new Error('destination is already at this bank; give a different shore waypoint')
    api.report({ boat: a.boat, villagerUuid: a.uuid, tow: `0/${points.length}` })
    for (let i = 0; i < points.length; i++) {
      const next = { x: points[i].x, y: points[i].y, z: points[i].z }
      if (Math.hypot(next.x - at(boat).x, next.y - at(boat).y, next.z - at(boat).z) > 11) throw new Error(`boat ${a.boat} is too far behind for the next tow step; stop before the lead snaps`)
      let stalled = 0
      for (let stroke = 0; distance(api.pos(), next) > 1 && stroke < 12; stroke++) {
        const before = distance(api.pos(), next)
        const body = api.pos()
        const belowFeet = api.block(Math.floor(body.x), Math.floor(body.y - 0.5), Math.floor(body.z))
        const swimming = (await api.act('state')).inWater === true || ['water', 'bubble_column', 'seagrass', 'tall_seagrass', 'kelp', 'kelp_plant'].includes(belowFeet?.name)
        if (swimming) await api.act('boat_swim', { ...next, ms: 700 })
        // Explicit pull waypoints may be a one-cell pedestrian doorway. A
        // one-cell arrival radius can accept the adjacent wall-side cell and
        // leave the body motionless before it passes through that doorway.
        else await api.act('goto', { x: next.x, y: Math.floor(next.y), z: next.z, range: pullVia.length && i >= turns.length ? 0 : 1 })
        ;({ s, boat } = await checked())
        if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} leash broke during tow; stopped at ${boat.exact}`)
        if (!swimming && distance(api.pos(), next) > 1 && distance(api.pos(), next) >= before - 0.05) throw new Error(`goto did not advance toward tow waypoint ${next.x},${next.y},${next.z}; boat remains at ${boat.exact}`)
        if (distance(api.pos(), next) < before - 0.05) stalled = 0
        else if (++stalled >= 2) throw new Error(`tow toward ${next.x},${next.y},${next.z} made no progress; boat remains at ${boat.exact}`)
        if (distance(api.pos(), at(boat)) > 8) throw new Error(`boat ${a.boat} fell too far behind during tow; stopped at ${boat.exact}`)
      }
      if (distance(api.pos(), next) > 1) throw new Error(`tow toward ${next.x},${next.y},${next.z} did not reach the waypoint; boat remains at ${boat.exact}`)
      ;({ s, boat } = await checked())
      if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} leash broke during tow; stopped at ${boat.exact}`)
      // The lead starts pulling at six blocks and snaps at twelve. Give the
      // server time to draw the boat in before the next short walking leg.
      await api.until(async () => {
        ;({ s, boat } = await checked())
        if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} leash broke during tow`)
        return distance(api.pos(), at(boat)) <= 7
      }, { timeout: 12, every: 0.25, what: `boat ${a.boat} did not follow this tow step` })
      api.report({ tow: `${i + 1}/${points.length}`, boatAt: boat.exact })
    }
    if (distance(at(boat), landing) > radius) throw new Error(`boat ${a.boat} stayed at ${boat.exact}; it did not reach the landing near ${landing.x},${landing.y},${landing.z}`)
    return { boat: a.boat, villagerUuid: a.uuid, from: `${from.x},${from.y},${from.z}`, at: boat.exact, pullAt: `${pull.x},${pull.y},${pull.z}`, passengerSeated: true, leashHeld: true, secure: false }
  }
}
