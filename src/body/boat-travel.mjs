import { CompositeHandBack } from '../composite.mjs'
import { isNight } from '../lib/sleep.mjs'
import { checkedBoatLanding } from '../navigation/boat-landing.mjs'

const point = p => ({ x: p.x, y: p.y, z: p.z })
const distance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const isBoat = entity => /(^|_)boat$/.test(entity?.name ?? '')

// Self-travel is separate from passenger towing. The injected controller owns
// vehicle physics; this adapter owns identity, boarding and task cancellation.
export function makeBoatTravelRuntime ({ getBot, cancelGuard, readBoatState, getLeashHolder = () => null,
  goNear, driveBoat, Vec3, edibleCarried = () => false, report = () => {}, reportPerformance = () => {},
  pause = ms => new Promise(resolve => setTimeout(resolve, ms)) }) {
  const target = (bot, id) => {
    if (!Number.isInteger(id)) throw new Error('boat travel needs an explicit authorized boat id')
    const entity = bot.entities[id]
    if (!isBoat(entity) || entity.isValid === false) throw new Error('the requested boat is not visible')
    return entity
  }
  const neutral = bot => {
    if (!isBoat(bot.vehicle)) return
    try {
      if (bot.supportFeature?.('newPlayerInputPacket')) bot._client.write('player_input', { inputs: {} })
      else bot._client.write('steer_vehicle', { sideways: 0, forward: 0, jump: 0 })
      bot._client.write('steer_boat', { leftPaddle: false, rightPaddle: false })
    } catch { /* Preserve the original disconnect/cancellation. */ }
  }
  const watch = (bot, entity, departure = true) => {
    const alive = cancelGuard()
    const check = () => {
      alive()
      if (departure && (bot.health <= 8 || bot.oxygenLevel <= 5 || bot.food <= 6 && !edibleCarried() || isNight(bot.time?.timeOfDay))) throw new CompositeHandBack('boat travel paused for health, oxygen, hunger or night; inspect the boat and a landing before leaving it')
      if (bot.entities[entity.id] !== entity || entity.isValid === false) throw new Error('boat identity changed during travel')
      if (getLeashHolder(entity.id) != null || entity.leashHolder) throw new Error('release the boat lead before self-travel')
      if (bot.vehicle && bot.vehicle.id !== entity.id) throw new Error('already riding another vehicle')
      const passengers = entity.passengers ?? []
      if (passengers.some(e => e.id !== bot.entity.id)) throw new Error('self-travel needs an empty boat or this traveler as its only passenger')
      if (bot.vehicle?.id === entity.id && passengers[0]?.id !== bot.entity.id) throw new Error('boat controlling passenger was not confirmed')
    }
    const packet = p => {
      if (p.entityId !== entity.id || p.passengers.includes(bot.entity.id)) return
      entity.passengers = (entity.passengers ?? []).filter(e => p.passengers.includes(e.id))
      if (bot.entity.vehicle === entity) bot.entity.vehicle = null
      if (bot.vehicle === entity) { bot.vehicle = null; bot.emit('dismount', entity) }
    }
    bot._client.on('set_passengers', packet)
    return { check, dispose: () => bot._client.removeListener('set_passengers', packet) }
  }
  return {
    quick: {
      boat_state (a) {
        const bot = getBot(), state = readBoatState(a)
        return { ...state, goalTravel: Boolean(driveBoat), boats: state.boats.map(view => {
          const entity = bot.entities[view.id]
          return { ...view, ...(entity?.position ? { name: entity.name, position: point(entity.position), yaw: entity.yaw, width: entity.width, height: entity.height } : {}) }
        }) }
      }
    },
    long: {
      async boat_land (a) {
        const bot = getBot(), entity = target(bot, a.id), guard = watch(bot, entity, false)
        let requested = false, positioned = false
        const serverPosition = () => { if (requested) positioned = true }
        bot.on('forcedMove', serverPosition)
        try {
          guard.check()
          if (bot.vehicle?.id !== entity.id) throw new Error('boat_land requires this traveler to be aboard the requested boat')
          neutral(bot)
          const before = point(entity.position)
          await pause(250); guard.check()
          if (distance(before, entity.position) > 0.05 || Math.hypot(entity.velocity?.x ?? 0, entity.velocity?.z ?? 0) > 0.04) throw new Error('wait for the boat to stop before landing')
          const blockAt = (x, y, z) => bot.blockAt(Vec3 ? new Vec3(x, y, z) : { x, y, z })
          const landing = checkedBoatLanding(blockAt, entity, point(a))
          await bot.look(landing.yaw, 0, true)
          // Mineflayer's mounted player physics stops its normal look packets.
          // Vanilla's dismount vector uses the passenger's server-side look,
          // so local camera rotation alone cannot select the checked shore.
          bot._client.write('look', { yaw: 180 - landing.yaw * 180 / Math.PI, pitch: 0,
            onGround: false, flags: { onGround: false, hasHorizontalCollision: false } })
          await pause(50); guard.check()
          checkedBoatLanding(blockAt, entity, point(a))
          requested = true
          if (bot.supportFeature?.('newPlayerInputPacket')) bot._client.write('player_input', { inputs: { shift: true } })
          else bot._client.write('steer_vehicle', { sideways: 0, forward: 0, jump: 2 })
          for (let i = 0; i < 60; i++) {
            guard.check()
            if (!bot.vehicle && positioned && bot.entity.onGround && distance(bot.entity.position, landing) < 0.35) {
              checkedBoatLanding(blockAt, entity, point(a))
              return { dismounted: entity.id, at: point(bot.entity.position), serverConfirmed: true }
            }
            await pause(50)
          }
          throw new Error('boat landing was not confirmed on the checked dry ground; inspect state before walking')
        } finally {
          bot.removeListener('forcedMove', serverPosition)
          if (requested) {
            try {
              if (bot.supportFeature?.('newPlayerInputPacket')) bot._client.write('player_input', { inputs: {} })
              else bot._client.write('steer_vehicle', { sideways: 0, forward: 0, jump: 0 })
            } catch { /* Preserve the action failure. */ }
          } else neutral(bot)
          guard.dispose()
        }
      },
      async boat_drive (a) {
        if (!driveBoat || !['x', 'y', 'z'].every(k => Number.isFinite(a[k]))) throw new Error('boat_drive needs a ready controller and finite x/y/z coordinates')
        const bot = getBot(), entity = target(bot, a.id), guard = watch(bot, entity)
        try {
          guard.check()
          const goal = point(a)
          await driveBoat.validate?.({ bot, entity, goal, check: guard.check, reportPerformance })
          if (bot.vehicle?.id !== entity.id) {
            if (distance(bot.entity.position, entity.position) > 3 && goNear) { await goNear(entity, guard.check); guard.check() }
            if (distance(bot.entity.position, entity.position) > 3) throw new Error('reach a checked boarding point within three blocks of the boat')
            bot.pathfinder?.setGoal(null)
            bot.clearControlStates?.()
            bot.mount(entity)
            for (let i = 0; i < 40 && bot.vehicle?.id !== entity.id; i++) { guard.check(); await pause(50) }
            guard.check()
            if (bot.vehicle?.id !== entity.id) throw new Error('boat boarding was not confirmed by the server')
          }
          const trip = await driveBoat({ bot, entity, goal, check: guard.check, pause, report, reportPerformance })
          return { id: entity.id, mounted: bot.vehicle?.id === entity.id, trip }
        } finally { neutral(bot); guard.dispose() }
      }
    }
  }
}
