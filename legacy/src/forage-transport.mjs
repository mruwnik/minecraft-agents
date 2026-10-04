import { dryTravelExit, tripDistance } from './navigation/travel.mjs'
import { CompositeHandBack } from './composite.mjs'
import { checkedBoatLanding } from './navigation/boat-landing.mjs'

export function forageTransportOptions (args) {
  const transport = args.transport ?? 'walk'
  if (!['walk', 'auto', 'boat'].includes(transport)) throw new Error('transport must be walk, auto or boat')
  if (args.boat !== undefined && (!Number.isInteger(args.boat) || args.boat < 0)) throw new Error('boat must be an explicit authorized entity id')
  if (transport === 'boat' && args.boat === undefined) throw new Error('transport=boat requires boat=<authorized entity id>')
  if (transport === 'walk' && args.boat !== undefined) throw new Error('boat= requires transport=auto or transport=boat')
  if (transport !== 'walk' && (args.pattern ?? 'outward') !== 'outward') throw new Error('boat transport currently supports pattern=outward only')
  return { transport, boat: args.boat }
}

// Route geometry and physics belong to shared navigation. Search only chooses
// nearby exploration goals and asks its checked route planner to validate them.
export function waterFrontiers (blockAt, from, columns, checkedRoute, surface, RouteError = Error) {
  const routes = [], failures = []
  for (const column of columns) {
    const to = surface(blockAt, Math.floor(column.x), Math.floor(column.z), from.y)
    if (!to) { failures.push({ ...column, reason: 'no loaded hull-clear source-water surface' }); continue }
    try {
      const route = checkedRoute(blockAt, from, to)
      routes.push({ goal: to, route })
    } catch (error) {
      if (!(error instanceof RouteError) || error instanceof TypeError || error instanceof RangeError) throw error
      failures.push({ ...to, reason: error.message })
    }
  }
  return { routes, failures }
}

// These are merely landing candidates. The shared controller must revalidate
// and confirm actual dismount before search may resume walking.
export function nearbyDryLandings (blockAt, boat) {
  const boatPosition = boat.position
  const candidates = []
  const bx = Math.floor(boatPosition.x), by = Math.floor(boatPosition.y), bz = Math.floor(boatPosition.z)
  for (let x = bx - 3; x <= bx + 3; x++) for (let z = bz - 3; z <= bz + 3; z++) {
    for (let y = by; y <= by + 2; y++) {
      const exit = { x, y, z }
      const center = { x: x + 0.5, y, z: z + 0.5 }
      const distance = tripDistance(boatPosition, center)
      if (distance <= 2.5 && dryTravelExit(blockAt, exit)) {
        try { checkedBoatLanding(blockAt, boat, exit); candidates.push({ exit, distance }) } catch (error) {
          if (!(error instanceof Error) || error instanceof TypeError || error instanceof RangeError) throw error
        }
      }
    }
  }
  return candidates.sort((a, b) => a.distance - b.distance).map(candidate => candidate.exit)
}

// Grid-centred water goals often stop too far from a bank to dismount. Choose
// a fractional approach whose predicted facing and full hull route are valid.
export function boatShoreApproaches (blockAt, boat, navigation) {
  const { position } = boat, candidates = []
  const reach = (1.375 * Math.SQRT2 + 0.6 + 0.00001) / 2
  const bx = Math.floor(position.x), bz = Math.floor(position.z)
  const upper = Math.floor(position.y + (boat.height ?? 0.5625))
  for (let x = bx - 5; x <= bx + 5; x++) for (let z = bz - 5; z <= bz + 5; z++) for (const y of [upper, upper - 1]) {
    const exit = { x, y, z }
    if (!dryTravelExit(blockAt, exit)) continue
    for (const [dx, dz] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
      const goal = { x: x + 0.5 - dx * reach, y: position.y, z: z + 0.5 - dz * reach }
      const distance = tripDistance(position, goal)
      if (distance > 6 || distance < 0.1) continue
      const yaw = Math.atan2(position.x - goal.x, position.z - goal.z)
      try {
        const route = navigation.checkedBoatRoute(blockAt, position, goal)
        checkedBoatLanding(blockAt, { ...boat, position: goal, yaw }, exit)
        candidates.push({ goal, exit, route, distance })
      } catch (error) {
        if (!(error instanceof Error) || error instanceof TypeError || error instanceof RangeError) throw error
      }
    }
  }
  return candidates.sort((a, b) => a.distance - b.distance)
}

export async function createForageTransport (api, options, navigation) {
  let state, asset, position
  const invoke = async (name, args) => {
    const result = await api.act(name, args)
    if (result?.stopped) throw new CompositeHandBack(result.stopped)
    return result
  }
  const refresh = async () => {
    state = await invoke('boat_state', { id: options.boat })
    if (!state || !Object.hasOwn(state, 'mounted')) throw new Error('boat_state did not confirm whether the traveler is mounted')
    asset = state.boats?.find(boat => boat.id === options.boat)
    position = asset?.position
    if (!position && asset?.exact) {
      const values = asset.exact.split(',').map(Number)
      if (values.length === 3) position = Object.fromEntries(['x', 'y', 'z'].map((key, i) => [key, values[i]]))
    }
    if (position && !['x', 'y', 'z'].every(key => Number.isFinite(position[key]))) position = null
    if (state.mounted !== null && state.mounted !== options.boat) throw new Error(`forage.search cannot control mounted vehicle ${state.mounted}; safely dismount it first`)
    if (state.mounted === options.boat && asset?.controller?.id !== state.selfId) throw new Error('the supplied boat is controlled by another passenger; do not steer')
  }
  const unavailable = () => {
    if (!state.goalTravel) return 'boat goal-travel controller is unavailable; use transport=walk or reload the ready controller'
    if (!asset || !position) return 'authorized boat is not observed with a numeric position; approach its safe service point first'
    if (state.mounted === null && (!Array.isArray(asset.passengers) || asset.passengers.length || asset.leashHolderId != null)) return 'boat must be confirmed empty and unleaded before boarding'
    if (state.mounted === null && tripDistance(api.pos(), position) > 2.5) return 'approach the authorized boat from a dry service point within 2.5 blocks first'
    if (state.mounted === null && !dryTravelExit(api.block, { x: Math.floor(api.pos().x), y: Math.floor(api.pos().y), z: Math.floor(api.pos().z) })) return 'boarding requires a verified dry service point; search never walks blindly into water'
    return null
  }
  await refresh()
  let blocked = options.boat === undefined ? 'no authorized boat id supplied' : unavailable()
  if (blocked && (options.transport === 'boat' || state.mounted !== null)) throw new Error(blocked)
  const status = () => ({ transport: state.mounted === null ? 'walk' : 'boat', mounted: state.mounted, boat: options.boat, transportUnavailable: blocked ?? undefined })
  const land = async () => {
    await refresh()
    if (state.mounted === null) return true
    let exits = nearbyDryLandings(api.block, { ...asset, position })
    if (!exits.length && navigation) {
      const began = Date.now()
      const approach = boatShoreApproaches(api.block, { ...asset, position }, navigation)[0]
      api.performance?.('forage.search.boat_landing', Date.now() - began, { candidatesRadius: 5 })
      if (approach) {
        await invoke('boat_drive', { id: options.boat, ...approach.goal })
        await refresh()
        if (state.mounted !== options.boat) throw new Error('shore approach lost the confirmed boat seat')
        // The observed stopping point, not the predicted endpoint, decides
        // whether the server can put this passenger safely onto dry ground.
        exits = nearbyDryLandings(api.block, { ...asset, position })
      }
    }
    for (const exit of exits) {
      // A controller failure is a safety hand-back, not permission to try an
      // unverified swimming/walking exit or silently abandon a mounted body.
      await invoke('boat_land', { id: options.boat, ...exit })
      await refresh()
      if (state.mounted !== null) throw new Error('boat landing did not confirm dismount; traveler remains mounted')
      const feet = api.pos()
      if (!dryTravelExit(api.block, { x: Math.floor(feet.x), y: Math.floor(feet.y), z: Math.floor(feet.z) })) throw new Error('boat landing did not confirm dry settled footing; do not resume walking')
      return true
    }
    return false
  }
  return {
    status,
    async frontiers (columns) {
      await refresh()
      blocked = options.boat === undefined ? 'no authorized boat id supplied' : unavailable()
      if (blocked) {
        if (state.mounted !== null || options.transport === 'boat') throw new Error(blocked)
        return { routes: [], failures: [], fallback: true }
      }
      const result = waterFrontiers(api.block, position, columns, navigation.checkedBoatRoute, navigation.boatSurface, navigation.BoatRouteError)
      if (!result.routes.length && state.mounted === null && options.transport === 'boat') throw new Error('no loaded hull-clear water frontier for this heading; inspect launch water or use transport=auto for walking')
      return { ...result, fallback: !result.routes.length && state.mounted === null }
    },
    async move (goal) {
      try { await invoke('boat_drive', { id: options.boat, ...goal }) } catch (error) {
        // Boarding may have happened before a safety stop. Never report the
        // pre-drive on-foot state as proof that a walking fallback is safe.
        state = { ...state, mounted: 'unconfirmed' }
        api.report({ transport: 'boat', mounted: 'unconfirmed', boat: options.boat })
        throw error
      }
      await refresh()
      if (state.mounted !== options.boat) throw new Error('boat drive did not confirm the traveler remains mounted')
    },
    land,
    position: () => position
  }
}
