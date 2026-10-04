// Complete itineraries, not top-speed comparisons. All times are estimates.
import { createTerrainGeometry } from './terrain.mjs'
import { checkedBoatRoute, BoatRouteError } from './boat-travel.mjs'
import { checkedBoatLanding } from './boat-landing.mjs'
export const TRAVEL_CAPABILITIES = {
  walk: { available: true },
  rail: { available: true, requires: 'explicit empty cart, loaded straight powered track, three braking rails and dry exit' },
  boat: { available: true, requires: 'explicit ordinary wooden boat, loaded level source-water corridor, dry boarding point and checked shore landing' },
  horse: { available: true, requires: 'explicit adult tamed saddled horse, confirmed speed, loaded flat dry corridor no longer than 128 blocks' }
}
export const tripDistance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
export const atRailLaunch = (position, route) => position && tripDistance(position, { ...route.from, x: route.from.x + 0.5, z: route.from.z + 0.5 }) <= 0.8
export class TravelValidationError extends Error {}
export function travelPoint (value, name = 'point') {
  const parts = String(value ?? '').split(':')
  const values = parts.map(Number)
  if (parts.some(part => !part.trim()) || values.length !== 3 || !values.every(Number.isInteger)) throw new TravelValidationError(`${name} must be integer x:y:z`)
  return Object.fromEntries(['x', 'y', 'z'].map((key, i) => [key, values[i]]))
}
const air = b => b && /^(air|cave_air|void_air)$/.test(b.name)
export function dryTravelExit (blockAt, p) {
  const floor = blockAt(p.x, p.y - 1, p.z)
  return floor?.solid === true && !/magma|cactus|campfire|ice|leaves|fence|wall|farmland|powder_snow/.test(floor.name) && air(blockAt(p.x, p.y, p.z)) && air(blockAt(p.x, p.y + 1, p.z))
}

export function checkedBoatItinerary (blockAt, asset, shore, traveler, { mounted = false } = {}) {
  let readFailure = null
  const read = (x, y, z) => {
    try { return blockAt(x, y, z) } catch (error) { readFailure = error; throw error }
  }
  const from = asset.position
  if (!from || !Number.isFinite(asset.yaw) || !/(?:^boat$|_boat$)/.test(asset.name ?? '') || /chest|bamboo/.test(asset.name)) throw new TravelValidationError('boat travel needs an observed ordinary wooden boat with a known heading')
  const geometry = createTerrainGeometry(blockAt, { dry: true, openDoors: false, avoidCrops: true })
  const boardings = []
  if (mounted) boardings.push({ node: null, position: from })
  else for (let x = Math.floor(from.x) - 3; x <= Math.floor(from.x) + 3; x++) for (let z = Math.floor(from.z) - 3; z <= Math.floor(from.z) + 3; z++) for (const y of [Math.floor(from.y), Math.floor(from.y) + 1, Math.floor(from.y) + 2]) {
    const stand = geometry.stand(x, y, z)
    if (!stand?.grounded) continue
    const position = { x: stand.centerX ?? x + 0.5, y: stand.height, z: stand.centerZ ?? z + 0.5 }
    // Leave room for native waypoint arrival tolerance while remaining within
    // the controller's three-block boarding reach.
    if (tripDistance(from, position) <= 2.4) boardings.push({ node: { x, y, z }, position })
  }
  boardings.sort((a, b) => tripDistance(traveler, a.position) - tripDistance(traveler, b.position))
  if (!boardings.length) throw new TravelValidationError('boat needs a loaded dry boarding stance within reach')
  const reach = (1.375 * Math.SQRT2 + 0.6 + 0.00001) / 2
  const choices = []
  for (const [dx, dz] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
    const to = { x: shore.x + 0.5 - dx * reach, y: from.y, z: shore.z + 0.5 - dz * reach }
    const yaw = Math.atan2(from.x - to.x, from.z - to.z)
    try {
      const route = checkedBoatRoute(read, from, to)
      checkedBoatLanding(read, { ...asset, position: to, yaw }, shore)
      choices.push(route)
    } catch (error) {
      if (error === readFailure) throw error
      if (!(error instanceof BoatRouteError) && error?.constructor !== Error) throw error
    }
  }
  choices.sort((a, b) => a.distance - b.distance)
  if (!choices.length) throw new TravelValidationError('no loaded hull-clear source-water route reaches the requested dry shore landing')
  return { ...choices[0], shore, boarding: boardings[0].node, boardingPosition: boardings[0].position, mounted }
}

// Endpoints expand into every rail cell. No switches, curves, slopes, building,
// power changes or implicit use of another player's vehicle are involved.
export function checkedRailRoute (blockAt, track, exit) {
  const endpoints = String(track ?? '').split(',').map(value => travelPoint(value, 'track endpoint'))
  if (endpoints.length !== 2) throw new TravelValidationError('track= needs exactly two endpoints x:y:z,x:y:z')
  const [from, to] = endpoints
  const dx = Math.sign(to.x - from.x), dz = Math.sign(to.z - from.z)
  const length = Math.abs(to.x - from.x) + Math.abs(to.z - from.z) + 1
  if (from.y !== to.y || Math.abs(dx) + Math.abs(dz) !== 1 || length < 8 || length > 512) throw new TravelValidationError('rail route must be flat, straight, and 8..512 cells long')
  const shape = dx ? 'east_west' : 'north_south'
  const points = Array.from({ length }, (_, i) => ({ x: from.x + dx * i, y: from.y, z: from.z + dz * i }))
  for (const [i, p] of points.entries()) {
    const rail = blockAt(p.x, p.y, p.z)
    if (!rail || !air(blockAt(p.x, p.y + 1, p.z)) || !air(blockAt(p.x, p.y + 2, p.z)) || blockAt(p.x, p.y - 1, p.z)?.solid !== true) throw new TravelValidationError(`unloaded or obstructed rail corridor at ${p.x},${p.y},${p.z}`)
    const brake = i >= length - 3
    if (rail.properties?.shape !== shape || (i === 0 ? rail.name !== 'rail' : rail.name !== 'powered_rail') || (i > 0 && rail.properties?.powered !== !brake)) throw new TravelValidationError(`rail at ${p.x},${p.y},${p.z} needs ${i === 0 ? 'normal launch rail' : brake ? 'unpowered braking rail' : 'powered rail'} facing ${shape}`)
  }
  const buffer = { x: to.x + dx, y: to.y, z: to.z + dz }
  if (blockAt(buffer.x, buffer.y, buffer.z)?.solid !== true) throw new TravelValidationError('rail terminus needs an existing solid buffer')
  const stop = points.at(-3)
  // Vanilla chooses the dismount side. Require a platform on BOTH sides of
  // the braking area instead of assuming it uses the requested exit cell.
  for (const p of points.slice(-4)) for (const side of [-1, 1]) {
    if (!dryTravelExit(blockAt, { x: p.x - dz * side, y: p.y, z: p.z + dx * side })) throw new TravelValidationError('rail stop needs dry platforms on both sides of its braking area')
  }
  if (!dryTravelExit(blockAt, exit) || tripDistance({ ...stop, x: stop.x + 0.5, z: stop.z + 0.5 }, { ...exit, x: exit.x + 0.5, z: exit.z + 0.5 }) > 2.5) throw new TravelValidationError('exit must be dry loaded ground within 2.5 blocks of the first braking rail')
  return { from, to, stop, exit, points, dx, dz, distance: length - 3 }
}

export function planTravel ({ from, to, rail = null, horse = null, boat = null, mode = 'auto', returnTrip = false }) {
  if (!['auto', ...Object.keys(TRAVEL_CAPABILITIES)].includes(mode)) throw new Error('mode must be auto, walk, rail, boat or horse')
  if (![from, to].every(p => ['x', 'y', 'z'].every(key => Number.isFinite(p?.[key])))) throw new Error('travel needs finite coordinates')
  const walkSeconds = tripDistance(from, to) / 4.3
  const back = returnTrip ? walkSeconds : 0
  const options = [{ mode: 'walk', available: true, seconds: walkSeconds + back, legs: [{ mode: 'walk', to }], returnSeconds: back }]
  if (rail) {
    const approach = tripDistance(from, rail.from) / 4.3
    const last = tripDistance(rail.exit, to) / 4.3
    options.push({ mode: 'rail', available: true, seconds: approach + 6 + rail.distance / 7 + last + back,
      legs: [{ mode: 'walk', to: rail.from }, { mode: 'rail', to: rail.stop }, { mode: 'walk', to }],
      approachSeconds: approach, boardingAndExitSeconds: 6, rideSeconds: rail.distance / 7, finalWalkSeconds: last, returnSeconds: back })
  } else options.push({ mode: 'rail', available: false, reason: 'no checked cart/track/exit supplied' })
  if (horse) {
    const approach = tripDistance(from, horse.from) / 4.3
    // Native ordinary-ground steady speed is attribute * .98 / (1-.546)
    // blocks/tick. Discount acceleration, braking and runtime overhead.
    const rideSeconds = horse.distance / (horse.speed * 36)
    options.push({ mode: 'horse', available: true, seconds: approach + 6 + rideSeconds + back,
      legs: [{ mode: 'walk', to: horse.from }, { mode: 'horse', to }, { mode: 'walk', to }],
      approachSeconds: approach, boardingAndExitSeconds: 6, rideSeconds, finalWalkSeconds: 0, returnSeconds: back })
  } else options.push({ mode: 'horse', available: false, reason: 'no checked horse/controller/corridor supplied' })
  if (boat) {
    const approach = boat.mounted ? 0 : tripDistance(from, boat.boardingPosition) / 4.3
    const finalWalk = tripDistance({ x: boat.shore.x + 0.5, y: boat.shore.y, z: boat.shore.z + 0.5 }, to) / 4.3
    const rideSeconds = boat.distance / 7 + 3 // below 8m/s steady speed, plus steering and precise shore approach
    const boardingAndExitSeconds = boat.mounted ? 3 : 6
    options.push({ mode: 'boat', available: true, seconds: approach + boardingAndExitSeconds + rideSeconds + finalWalk + back,
      legs: [...(boat.mounted ? [] : [{ mode: 'walk', to: boat.boarding }]), { mode: 'boat', to: boat.to }, { mode: 'land', to: boat.shore }, { mode: 'walk', to }],
      approachSeconds: approach, boardingAndExitSeconds, rideSeconds, finalWalkSeconds: finalWalk, returnSeconds: back })
  } else options.push({ mode: 'boat', available: false, reason: 'no checked authorized boat, dry boarding point and shore landing supplied' })
  const selected = options.filter(option => option.available && (mode === 'auto' || mode === option.mode)).sort((a, b) => a.seconds - b.seconds)[0]
  return { selected: selected?.mode ?? null, seconds: selected?.seconds ?? null, options, returnPlan: returnTrip ? { mode: 'walk', to: from, seconds: back } : null,
    note: 'ETAs include geometric walking approaches, boarding/landing, conservative vehicle speed and final walking; return=true prices a walked return but does not execute it.' }
}
