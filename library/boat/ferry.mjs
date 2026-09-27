import { villagerBoatRoute, villagerTowWaypoints } from '../../src/lib.mjs'
import { boatPassengerStatus, entityUuid } from '../../src/boat/passenger.mjs'

const at = boat => {
  const [x, y, z] = boat.exact.split(',').map(Number)
  return { x, y, z }
}
const distance = (a, b) => Math.hypot(a.x - b.x, a.z - b.z)

export default {
  doc: 'boat.ferry uuid= boat= x= y= z= [centerX= centerZ=] [pullX= pullY= pullZ=] [pullVia=x:y:z,x:y:z] [alignZ= alignTolerance=0.3] [radius=0.8] [plan=true]: precheck the full 1.375-wide nonascending route for a supported adult villager, cow, sheep or pig, then tow with a lead; centerX/Z set an exact boat landing within 0.8 block of the named cell center; pullVia names short bot waypoints beyond it; alignZ requires the actual boat to be staged laterally before a narrow dock approach; plan=true only reads terrain',
  stops: 'the boat reaches the named bank with its supported passenger and lead intact, or the tow stalls',
  args: { uuid: 'string!', boat: 'number!', x: 'number!', y: 'number!', z: 'number!', centerX: 'number', centerZ: 'number', pullX: 'number', pullY: 'number', pullZ: 'number', pullVia: 'string', pullInto: 'boolean', alignZ: 'number', alignTolerance: 'number', radius: 'number', plan: 'boolean' },

  async run (api, a) {
    if (!entityUuid(a.uuid)) throw new Error('uuid= must be the observed passenger UUID')
    if (!Number.isInteger(a.boat) || ![a.x, a.y, a.z].every(Number.isInteger)) throw new Error('boat= and x= y= z= must be integers')
    const centerGiven = a.centerX !== undefined || a.centerZ !== undefined
    if (centerGiven && (![a.centerX, a.centerZ].every(Number.isFinite) || Math.abs(a.centerX - (a.x + 0.5)) > 0.8 || Math.abs(a.centerZ - (a.z + 0.5)) > 0.8)) throw new Error('centerX= and centerZ= must both be numbers within 0.8 block of the landing cell center')
    const landing = { x: centerGiven ? a.centerX : a.x + 0.5, y: a.y, z: centerGiven ? a.centerZ : a.z + 0.5 }
    const radius = a.radius ?? 0.8
    if (!Number.isFinite(radius) || radius < 0.5 || radius > 4) throw new Error('radius= must be 0.5..4 blocks')
    const alignTolerance = a.alignTolerance ?? 0.3
    if ((a.alignZ !== undefined && !Number.isFinite(a.alignZ)) || !Number.isFinite(alignTolerance) || alignTolerance < 0.1 || alignTolerance > 0.3) throw new Error('alignZ= must be finite; alignTolerance= must be 0.1..0.3')
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
      const issue = boatPassengerStatus(s, a.boat, a.uuid)
      if (issue) throw new Error(issue)
      if (s.mounted === a.boat) throw new Error(`dismount boat ${a.boat} before towing it`)
      return { s, boat: s.boats[0] }
    }
    let { s, boat } = await checked()
    if (boat.passengers.length !== 1) throw new Error(`boat ${a.boat} must carry only passenger ${a.uuid} while being towed`)
    if (boat.leashHolderId !== null && boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} is leashed to somebody else`)
    // The bot's walk can climb a bank that the lower boat cannot. Check the
    // whole hull route before any leash or walking action.
    const route = villagerBoatRoute({
      from: at(boat),
      to: landing,
      blockAt: (x, y, z) => api.block(x, y, z)
    })
    if (route.error) throw new Error(`boat route blocked at ${route.at.x},${route.at.y},${route.at.z}: ${route.error}`)
    const alignment = () => a.alignZ === undefined || Math.abs(at(boat).z - a.alignZ) <= alignTolerance
    const kind = boat.passengers[0].name
    const identity = kind === 'villager' ? { villagerUuid: a.uuid } : { passengerUuid: a.uuid, kind }
    if (a.plan === true) return { boat: a.boat, ...identity, route: route.points, aligned: alignment(), planned: true, secure: false }
    if (!alignment()) throw new Error(`boat ${a.boat} needs water staging before this dock approach: actual z=${at(boat).z}, required ${a.alignZ} +/- ${alignTolerance}; no towing started`)
    if (boat.leashHolderId === null) {
      await api.act('boat_leash', { id: a.boat })
      ;({ s, boat } = await checked())
    }
    if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} was not leashed to this bot`)
    const checkPull = async leader => {
      const current = at(boat)
      const gap = distance(current, leader)
      // A taut lead draws the hull along the actual boat-to-leader ray. Its
      // trailing position can cut a planned turn even while the walker fits.
      const travel = Math.max(0, gap - 5.5)
      if (travel < 0.05) return
      const endpoint = { x: current.x + (leader.x - current.x) * travel / gap, y: current.y, z: current.z + (leader.z - current.z) * travel / gap }
      const swept = villagerBoatRoute({ from: current, to: endpoint, direct: true, blockAt: (x, y, z) => api.block(x, y, z) })
      if (swept.error) {
        const reason = `actual boat pull blocked at ${swept.at.x},${swept.at.y},${swept.at.z}: ${swept.error}`
        ;({ s, boat } = await checked())
        if (boat.passengers.length !== 1 || boat.leashHolderId !== s.selfId) throw new Error(`${reason}; exact owned sole-passenger leash no longer confirmed; no detach attempted`)
        const leadsBefore = api.inv().lead ?? 0
        try {
          await api.act('boat_unleash', { id: a.boat })
          ;({ s, boat } = await checked())
          if (boat.leashHolderId !== null) throw new Error('detachment was not observed')
        } catch (error) { throw new Error(`${reason}; safe lead detachment pending: ${error.message}`) }
        const recovered = Math.max(0, (api.inv().lead ?? 0) - leadsBefore)
        api.report({ stopped: true, boat: a.boat, boatAt: boat.exact, leadDetached: true, leadRecovered: recovered, leadRecoveryNear: boat.exact })
        throw new Error(`${reason}; exact boat lead detached at ${boat.exact}, recovered ${recovered}; recover the dropped lead before pausing and stage before this turn`)
      }
    }
    const catchUp = async () => api.until(async () => {
      ;({ s, boat } = await checked())
      if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} leash broke during tow`)
      await checkPull(api.pos())
      const gap = distance(api.pos(), at(boat))
      if (gap > 11) throw new Error(`boat ${a.boat} fell too far behind during tow; stopped at ${boat.exact}`)
      return gap <= 7
    }, { timeout: 12, every: 0.25, what: `boat ${a.boat} did not follow this tow step` })

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
      // safety stop, including ordinary walking offsets within a block cell.
      if (distance(turns.at(-1) ?? route.points[nearest], p) >= 2) turns.push(p)
      heading = nextHeading
      last = p
    }
    if (planned.length && turns.at(-1) !== planned.at(-1)) turns.push(planned.at(-1))
    const beyond = pullVia.length ? beyondPath : pullGiven ? villagerTowWaypoints(route.points.at(-1), pull, 3.5) : []
    const points = [...turns, ...beyond]
    if (!points.length) throw new Error('destination is already at this bank; give a different shore waypoint')
    api.report({ boat: a.boat, ...identity, tow: `0/${points.length}` })
    for (let i = 0; i < points.length; i++) {
      if (i === turns.length && !alignment()) throw new Error(`boat ${a.boat} lost dock alignment before the dry pull: actual z=${at(boat).z}, required ${a.alignZ} +/- ${alignTolerance}; stop and stage in open water`)
      const next = { x: points[i].x, y: points[i].y, z: points[i].z }
      const walkTarget = { ...next, y: Math.floor(next.y) }
      let dryTarget = false
      // Hull support can come from a neighboring bank cell. A pedestrian
      // needs support in its own column, which may be up to two cells lower.
      for (let down = 0; down <= 2; down++) {
        const y = Math.floor(next.y) - down
        const support = api.block(Math.floor(next.x), y - 1, Math.floor(next.z))
        const feet = api.block(Math.floor(next.x), y, Math.floor(next.z))
        const head = api.block(Math.floor(next.x), y + 1, Math.floor(next.z))
        const safeSupport = support?.solid && !/(?:_slab|_stairs|_fence|_fence_gate|_wall|_bed|_trapdoor|_leaves)$|^(?:magma_block|cactus|campfire|soul_campfire|powder_snow|soul_sand|farmland|dirt_path)$/.test(support.name)
        const clear = b => b && !b.solid && !['lava', 'fire', 'soul_fire', 'cobweb', 'sweet_berry_bush', 'powder_snow'].includes(b.name)
        if (safeSupport && clear(feet) && clear(head)) {
          walkTarget.y = y
          dryTarget = !['water', 'bubble_column', 'seagrass', 'tall_seagrass', 'kelp', 'kelp_plant'].includes(feet.name)
          break
        }
      }
      if (Math.hypot(next.x - at(boat).x, next.y - at(boat).y, next.z - at(boat).z) > 11) throw new Error(`boat ${a.boat} is too far behind for the next tow step; stop before the lead snaps`)
      let stalled = 0
      let reachedDryCell = false
      const inTargetCell = () => {
        const body = api.pos()
        return Math.floor(body.x) === Math.floor(walkTarget.x) && Math.floor(body.y) === walkTarget.y && Math.floor(body.z) === Math.floor(walkTarget.z)
      }
      for (let stroke = 0; distance(api.pos(), next) > 1 && !reachedDryCell && stroke < 12; stroke++) {
        const before = distance(api.pos(), next)
        const body = api.pos()
        const belowFeet = api.block(Math.floor(body.x), Math.floor(body.y - 0.5), Math.floor(body.z))
        const inWater = (await api.act('state')).inWater === true || ['water', 'bubble_column', 'seagrass', 'tall_seagrass', 'kelp', 'kelp_plant'].includes(belowFeet?.name)
        const swimming = inWater && !dryTarget
        await checkPull(swimming ? next : { x: Math.floor(walkTarget.x) + 0.5, y: walkTarget.y, z: Math.floor(walkTarget.z) + 0.5 })
        if (swimming) await api.act('boat_swim', { ...next, ms: 700 })
        // GoalNear rounds coordinates to a block cell. Range one can accept
        // its neighbor without moving; require the exact dry target cell.
        else await api.act('goto', { ...walkTarget, range: 0, ...(a.pullInto === true && i >= turns.length ? { into: true } : {}) })
        ;({ s, boat } = await checked())
        await checkPull(api.pos())
        reachedDryCell = !swimming && inTargetCell()
        if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} leash broke during tow; stopped at ${boat.exact}`)
        if (!swimming && !inTargetCell() && distance(api.pos(), next) > 1 && distance(api.pos(), next) >= before - 0.05) throw new Error(`goto did not advance toward tow waypoint ${next.x},${next.y},${next.z}; boat remains at ${boat.exact}`)
        if ((!swimming && inTargetCell()) || distance(api.pos(), next) < before - 0.05) stalled = 0
        else if (++stalled >= 2) throw new Error(`tow toward ${next.x},${next.y},${next.z} made no progress; boat remains at ${boat.exact}`)
        if (distance(api.pos(), at(boat)) > (swimming ? 8 : 11)) throw new Error(`boat ${a.boat} fell too far behind during tow; stopped at ${boat.exact}`)
        if (!swimming && distance(api.pos(), at(boat)) > 7) await catchUp()
      }
      if (distance(api.pos(), next) > 1 && !reachedDryCell) throw new Error(`tow toward ${next.x},${next.y},${next.z} did not reach the waypoint; boat remains at ${boat.exact}`)
      ;({ s, boat } = await checked())
      if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} leash broke during tow; stopped at ${boat.exact}`)
      // The lead starts pulling at six blocks and snaps at twelve. Give the
      // server time to draw the boat in before the next short walking leg.
      await catchUp()
      api.report({ tow: `${i + 1}/${points.length}`, boatAt: boat.exact })
    }
    if (distance(at(boat), landing) > radius) await api.until(async () => {
      ;({ s, boat } = await checked())
      if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${a.boat} leash broke while settling at the landing`)
      await checkPull(api.pos())
      if (distance(api.pos(), at(boat)) > 11) throw new Error(`boat ${a.boat} fell too far behind while settling; stopped at ${boat.exact}`)
      return distance(at(boat), landing) <= radius
    }, { timeout: 12, every: 0.25, what: `boat ${a.boat} stayed outside the landing near ${landing.x},${landing.y},${landing.z}` })
    return { boat: a.boat, ...identity, from: `${from.x},${from.y},${from.z}`, at: boat.exact, pullAt: `${pull.x},${pull.y},${pull.z}`, passengerSeated: true, leashHeld: true, secure: false }
  }
}
