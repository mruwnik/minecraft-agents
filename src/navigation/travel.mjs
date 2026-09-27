// Complete itineraries, not top-speed comparisons. All times are estimates.
export const TRAVEL_CAPABILITIES = {
  walk: { available: true },
  rail: { available: true, requires: 'explicit empty cart, loaded straight powered track, three braking rails and dry exit' },
  boat: { available: false, reason: 'mounted boat physics and steering are not implemented; passenger towing is walking-speed transport' },
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

export function planTravel ({ from, to, rail = null, horse = null, mode = 'auto', returnTrip = false }) {
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
  options.push({ mode: 'boat', ...TRAVEL_CAPABILITIES.boat })
  const selected = options.filter(option => option.available && (mode === 'auto' || mode === option.mode)).sort((a, b) => a.seconds - b.seconds)[0]
  return { selected: selected?.mode ?? null, seconds: selected?.seconds ?? null, options, returnPlan: returnTrip ? { mode: 'walk', to: from, seconds: back } : null,
    note: 'ETAs use geometric walking distances, conservative rail speed and the observed horse speed attribute; return=true prices a walked return but does not execute it.' }
}
