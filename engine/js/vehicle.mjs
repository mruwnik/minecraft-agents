// Vehicles: who rides what, getting on, getting off. Boats, rafts, minecarts and rideable mobs, for the body and for
// any mob as a passenger.
// Why JavaScript: Mineflayer boundary — set_passengers packets, entity objects, use_entity, raw look and sneak input.
// What to mount and when, and what a landing means, is decided in cljs (jobs.movement.vehicle, jobs.movement.*).
//
// Mineflayer 4.39 only handles set_passengers lists that include the body: it never clears bot.vehicle on a dismount
// and never takes a dropped passenger out of vehicle.passengers. So the full lists are tracked per bot from the
// moment it is adopted, and bot.vehicle is repaired from them. While mounted Mineflayer runs no physics tick and sends
// no look or position, so every wait here is wall time and the dismount look is a raw packet.
import vec3 from 'vec3'
import { emptyHand } from './unequip.mjs'

const POLL_MS = 50
export const MOUNT_REACH = 3
const MOUNT_WAIT_MS = 1000
const DISMOUNT_WAIT_MS = 1000

const BOAT = /_(boat|raft)$/
const CHEST_BOAT = /_chest_(boat|raft)$/
const RIDEABLE = ['minecart', 'pig', 'strider', 'horse', 'donkey', 'mule', 'skeleton_horse', 'zombie_horse', 'camel', 'camel_husk', 'llama', 'trader_llama', 'happy_ghast']
const SEATS = { camel: 2, camel_husk: 2, happy_ghast: 4 }

// seats of a mountable entity by name; 0 when the body cannot ride it
export const seatsOf = name => {
  if (CHEST_BOAT.test(name)) return 1
  if (BOAT.test(name)) return 2
  if (RIDEABLE.includes(name)) return SEATS[name] ?? 1
  return 0
}

// A boat or minecart takes the body with anything in hand; a mob would be leashed or fed by a held lead or food.
export const needsEmptyHand = name => seatsOf(name) > 0 && !BOAT.test(name) && !CHEST_BOAT.test(name) && name !== 'minecart'

// Why a mount must not be tried, from the name and how many ride it now; null when it may. A non-player in a boat's
// front seat does not count against the body: vanilla puts a boarding player first.
export const mountRefusal = (name, riders) => {
  const seats = seatsOf(name)
  if (seats === 0) return 'not-mountable'
  if (riders >= seats) return 'occupied'
  return null
}

// Mineflayer 4.39 never moves a passenger with its mount (the server sends a rider only rotation), so a rider's
// position is set from the mount's: mount position + seat height - the rider's riding offset. Small tables, vanilla
// values; unknown names fall back to 0.75 of the mount's height and 0.14.
const SEAT_Y = { skeleton_horse: 1.31875, zombie_horse: 1.31875, horse: 1.31875, donkey: 1.2, mule: 1.2, camel: 2.0, strider: 1.4, spider: 0.8, chicken: 0.5, pig: 0.7 }
const RIDING_OFFSET = { skeleton: 0.7, stray: 0.7, wither_skeleton: 0.7, bogged: 0.7, zombie: 0.14, drowned: 0.14, husk: 0.14, player: 0.14 }
const BOAT_SEAT_Y = -0.1
export const seatPosition = (mount, rider) => {
  const seat = BOAT.test(mount.name ?? '') ? BOAT_SEAT_Y : SEAT_Y[mount.name] ?? (mount.height ?? 1) * 0.75
  const p = mount.position
  return vec3(p.x, p.y + seat - (RIDING_OFFSET[rider.name] ?? 0.14), p.z)
}

const tracked = new WeakMap() // bot -> Map vehicleId -> [passenger ids], as the server last listed them

export function trackVehicles (bot) {
  if (!bot._client || tracked.has(bot)) return
  const lists = new Map()
  tracked.set(bot, lists)
  bot._client.on('set_passengers', ({ entityId, passengers }) => {
    lists.set(entityId, [...passengers])
    const body = bot.entity?.id
    if (passengers.includes(body)) bot.vehicle = bot.entities?.[entityId] ?? bot.vehicle
    else if (bot.vehicle?.id === entityId) bot.vehicle = null
  })
  bot.on('entityMoved', mount => {
    for (const id of lists.get(mount.id) ?? []) {
      const rider = bot.entities?.[id]
      if (!rider || rider === bot.entity || !mount.position) continue
      rider.position = seatPosition(mount, rider)
    }
  })
  bot.on('entityGone', e => {
    lists.delete(e.id)
    if (bot.vehicle?.id === e.id) bot.vehicle = null
  })
}

// ids riding entity e: the tracked list, else what mineflayer holds (before any packet for it)
export const ridersOf = (bot, e) => tracked.get(bot)?.get(e.id) ?? (e.passengers ?? []).map(p => p.id)

// the id of what e rides, or null
const vehicleIdOf = (bot, e) => {
  const lists = tracked.get(bot) ?? new Map()
  for (const [vehicleId, ids] of lists) if (ids.includes(e.id)) return vehicleId
  const kept = e.vehicle?.id
  return kept !== undefined && !lists.has(kept) ? kept : null
}

// Keys appear only when they apply: passengers when someone rides e, vehicle when e rides something
export function vehicleFields (bot, e) {
  const riders = ridersOf(bot, e)
  const vehicle = vehicleIdOf(bot, e)
  return { ...(riders.length > 0 && { passengers: [...riders] }), ...(vehicle !== null && { vehicle }) }
}

const describe = v => ({ id: v.id, uuid: v.uuid ?? null, name: v.name ?? null })

export const selfVehicle = bot => bot.vehicle ? describe(bot.vehicle) : null

const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))
const middle = e => vec3(e.position.x, e.position.y + (e.height ?? 1) / 2, e.position.z)
const eyeOf = bot => vec3(bot.entity.position.x, bot.entity.position.y + (bot.entity.height ?? 1.62), bot.entity.position.z)

const waitFor = async (ctx, done, ms) => {
  const deadline = Date.now() + ms
  while (!done() && Date.now() < deadline) {
    await sleepMs(POLL_MS)
    ctx.alive()
  }
  return done()
}

// Gets the body on entity a.id: refused before any use when it cannot work, then a use, with an empty hand for a mob (a held
// lead or food would leash or feed instead), then a wall-time wait for the server to list the body as a passenger.
export async function mountVehicle (bot, ctx, a, { timeScale = 1, reach = MOUNT_REACH } = {}) {
  if (bot.vehicle) return { status: 'already-mounted', vehicle: describe(bot.vehicle) }
  const target = bot.entities[a.id]
  if (!target) return { status: 'gone' }
  const refused = mountRefusal(target.name, ridersOf(bot, target).length)
  if (refused) return { status: refused }
  if (eyeOf(bot).distanceTo(middle(target)) > reach) return { status: 'out-of-reach' }
  if (needsEmptyHand(target.name)) {
    const emptied = await emptyHand(bot, ctx)
    if (emptied.status === 'full' || emptied.status === 'failed') return { status: 'hand-full' }
  }
  await bot.lookAt(middle(target), true)
  ctx.alive()
  bot.mount(target)
  const seated = await waitFor(ctx, () => bot.vehicle?.id === a.id, MOUNT_WAIT_MS * timeScale)
  return seated ? { status: 'mounted', vehicle: describe(bot.vehicle) } : { status: 'timeout' }
}

// Minecraft degrees (0 south, 90 west; pitch down positive) as a raw look: mounted Mineflayer sends none, and the
// server picks the exit from the look it holds. The local rotation follows, in Mineflayer's radians.
const writeLook = (bot, yaw, pitch) => {
  bot._client.write('look', { yaw, pitch, onGround: false, flags: { onGround: false, hasHorizontalCollision: false } })
  bot.entity.yaw = Math.PI - yaw * Math.PI / 180
  bot.entity.pitch = -pitch * Math.PI / 180
}

// Gets the body off: 26.1 dismounts on sneak (Mineflayer's dismount() sends jump, which does nothing). Done once the
// body is off the tracked list and a server position arrived; the landing is the caller's to judge. Sneak is
// released on every exit.
export async function dismountVehicle (bot, ctx, a, { timeScale = 1 } = {}) {
  if (!bot.vehicle) return { status: 'not-mounted' }
  let placed = false
  const onPlaced = () => { placed = true }
  const release = () => {
    bot.removeListener('forcedMove', onPlaced)
    bot.setControlState('sneak', false)
  }
  bot.on('forcedMove', onPlaced)
  ctx.onAbort(release)
  try {
    if (typeof a.yaw === 'number' && Number.isFinite(a.yaw)) writeLook(bot, a.yaw, typeof a.pitch === 'number' ? a.pitch : 0)
    bot.setControlState('sneak', true)
    const off = await waitFor(ctx, () => !bot.vehicle && placed, DISMOUNT_WAIT_MS * timeScale)
    const p = bot.entity.position
    return off ? { status: 'dismounted', pos: { x: p.x, y: p.y, z: p.z } } : { status: 'timeout', mounted: Boolean(bot.vehicle) }
  } finally {
    release()
  }
}
