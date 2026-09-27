import { atRailLaunch, checkedRailRoute, dryTravelExit, travelPoint, tripDistance } from '../navigation/travel.mjs'
import { CompositeHandBack } from '../composite.mjs'
import { isNight } from '../lib/sleep.mjs'

// Minecarts have server-side rail physics. This driver sends ordinary rider
// input and observes movement; it never synthesizes vehicle positions.
export function makeTravelRuntime ({ getBot, Vec3, cancelGuard, edibleCarried = () => false, reportPerformance = () => {}, pause = ms => new Promise(resolve => setTimeout(resolve, ms)) }) {
  const passengers = (bot, cart) => cart.passengers ?? Object.values(bot.entities).filter(e => e.vehicle?.id === cart.id)
  const cartView = (bot, cart) => cart?.name === 'minecart' ? {
    id: cart.id, position: { x: cart.position.x, y: cart.position.y, z: cart.position.z }, passengers: passengers(bot, cart).map(e => e.id)
  } : null
  return {
    quick: {
      rail_state (a) {
        const bot = getBot()
        return { mounted: bot.vehicle?.id ?? null, cart: cartView(bot, bot.entities[a.id]) }
      }
    },
    long: {
      async rail_ride (a) {
        const bot = getBot(), alive = cancelGuard()
        if (!Number.isInteger(a.id)) throw new Error('rail_ride needs an explicit minecart id')
        const blockAt = (x, y, z) => {
          const b = bot.blockAt(new Vec3(x, y, z))
          return b && { name: b.name, solid: b.boundingBox === 'block', properties: b.getProperties?.() ?? {} }
        }
        const exit = travelPoint(a.exit, 'exit')
        const validate = () => {
          const began = performance.now()
          try { return checkedRailRoute(blockAt, a.track, exit) } finally { reportPerformance('rail.route', performance.now() - began, { track: a.track }) }
        }
        const route = validate()
        const cart = bot.entities[a.id]
        if (!cartView(bot, cart) || passengers(bot, cart).length || bot.vehicle) throw new Error('rail_ride needs an empty ordinary minecart and an unmounted traveler')
        if (!atRailLaunch(cart.position, route) || tripDistance(bot.entity.position, cart.position) > 3) throw new Error('reach the cart at the rail launch before riding')
        const allowed = p => Math.abs((route.dx ? p.z - route.from.z : p.x - route.from.x) - 0.5) <= 0.8 && Math.abs(p.y - route.from.y) <= 1.2 &&
          ((p.x - route.from.x - 0.5) * route.dx + (p.z - route.from.z - 0.5) * route.dz) >= -0.8 &&
          ((p.x - route.from.x - 0.5) * route.dx + (p.z - route.from.z - 0.5) * route.dz) <= route.points.length - 0.2
        const check = () => {
          alive()
          const risk = bot.health <= 8 ? `health ${bot.health}` : bot.food <= 6 && !edibleCarried() ? `food ${bot.food}` : bot.oxygenLevel <= 5 ? `oxygen ${bot.oxygenLevel}` : isNight(bot.time?.timeOfDay) ? 'night' : null
          if (risk) throw new CompositeHandBack(`${risk}; rail input released, traveler remains mounted; powered rails may keep the cart moving to its checked stop`)
          const seated = passengers(bot, cart)
          if (bot.entities[a.id] !== cart || cart.isValid === false || bot.vehicle?.id !== a.id || seated.length !== 1 || seated[0].id !== bot.entity.id) throw new Error('rail ride lost its cart or exclusive passenger identity; inspect before continuing')
          if (!allowed(cart.position)) throw new Error('minecart left its checked rail corridor; traveler remains mounted')
        }
        let dismountRequested = false, dismountPosition = null
        // Mineflayer 4.39 only updates bot.vehicle when self is INCLUDED in a
        // set_passengers packet. The server's full list also authoritatively
        // removes self; repair that stale pointer without inventing movement.
        const passengerPacket = packet => {
          if (packet.entityId !== cart.id || packet.passengers.includes(bot.entity.id)) return
          cart.passengers = (cart.passengers ?? []).filter(e => packet.passengers.includes(e.id))
          if (bot.entity.vehicle === cart) bot.entity.vehicle = null
          if (bot.vehicle === cart) { bot.vehicle = null; bot.emit('dismount', cart) }
        }
        // Native physics applies the server position and reenables walking
        // before emitting forcedMove. Removal alone cannot resume walking.
        const positioned = () => {
          if (dismountRequested) dismountPosition = { x: bot.entity.position.x, y: bot.entity.position.y, z: bot.entity.position.z }
        }
        bot._client.on('set_passengers', passengerPacket)
        bot.on('forcedMove', positioned)
        try {
          alive()
          // Do not board when the same safety condition would stop the ride.
          if (bot.health <= 8 || (bot.food <= 6 && !edibleCarried()) || bot.oxygenLevel <= 5 || isNight(bot.time?.timeOfDay)) throw new CompositeHandBack('rail departure postponed for health, hunger, oxygen or night')
          bot.pathfinder.setGoal(null)
          // Mounting disables Mineflayer's player physics/look packets. Send
          // the launch heading while still on foot, then allow it to flush.
          await bot.look(Math.atan2(-route.dx, -route.dz), 0, true)
          await pause(100)
          alive()
          if (bot.entities[a.id] !== cart || passengers(bot, cart).length || !atRailLaunch(cart.position, route)) throw new Error('minecart changed while preparing to board')
          bot.mount(cart)
          // Mounted Mineflayer bodies do not emit physicsTick, so vehicle
          // observation must use wall time rather than waitForTicks.
          for (let i = 0; i < 20 && bot.vehicle?.id !== a.id; i++) { alive(); await pause(50) }
          if (bot.vehicle?.id !== a.id) throw new Error('minecart boarding was not confirmed')
          check()
          // A normal launch rail accepts the rider's initial push; the powered
          // corridor drives after that. Unsupported servers fail on no progress.
          bot.moveVehicle(0, 1)
          await pause(250)
          check()
          bot.moveVehicle(0, 0)
          let previous = { ...cart.position }, best = -Infinity, idleTicks = 0, stillTicks = 0, arrived = false
          const maxTicks = Math.ceil((route.distance / 4 + 20) * 20)
          for (let ticks = 0; ticks < maxTicks; ticks += 2) {
            check()
            if (ticks % 20 === 0) validate()
            const forward = (cart.position.x - route.from.x - 0.5) * route.dx + (cart.position.z - route.from.z - 0.5) * route.dz
            const moved = tripDistance(previous, cart.position)
            previous = { ...cart.position }
            stillTicks = moved < 0.02 ? stillTicks + 2 : 0
            if (forward > best + 0.1) { best = forward; idleTicks = 0 } else idleTicks += 2
            const atBrake = forward >= route.points.length - 3.3 && forward <= route.points.length - 0.2
            if (atBrake && stillTicks >= 10) { arrived = true; break }
            if (idleTicks >= 100) throw new Error('minecart made no forward progress for five seconds; traveler remains mounted')
            await pause(100)
          }
          if (!arrived) throw new Error('minecart did not reach its checked stop in time; traveler remains mounted')
          check()
          if (!dryTravelExit(blockAt, exit) || tripDistance(cart.position, new Vec3(exit.x + 0.5, exit.y, exit.z + 0.5)) > 3) throw new Error('dry rail exit changed or is out of reach; traveler remains mounted')
          // Mineflayer 4.39 sends jump for modern dismount, but the modern
          // player_input schema has a distinct shift bit for leaving a vehicle.
          dismountRequested = true
          if (bot.supportFeature?.('newPlayerInputPacket')) bot._client.write('player_input', { inputs: { shift: true } })
          else bot.dismount()
          for (let i = 0; i < 40 && (bot.vehicle || !dismountPosition); i++) { alive(); await pause(50) }
          if (bot.vehicle) throw new Error('minecart dismount was not confirmed')
          if (!dismountPosition) throw new Error('minecart passenger removal confirmed, but no server dismount position arrived; inspect before walking')
          const feet = Object.fromEntries(['x', 'y', 'z'].map(key => [key, Math.floor(dismountPosition[key])]))
          if (!dryTravelExit(blockAt, feet) || tripDistance(dismountPosition, new Vec3(exit.x + 0.5, exit.y, exit.z + 0.5)) > 3) throw new Error('server dismount position is not on the checked dry landing; inspect before walking')
          return { arrived: true, cart: a.id, exit, parkedAt: { x: cart.position.x, y: cart.position.y, z: cart.position.z } }
        } finally {
          bot._client.removeListener('set_passengers', passengerPacket)
          bot.removeListener('forcedMove', positioned)
          // Never jump from a moving cart or start walking after interruption.
          if (dismountRequested && bot.supportFeature?.('newPlayerInputPacket')) {
            try { bot._client.write('player_input', { inputs: {} }) } catch { /* Preserve the original hand-back. */ }
          } else if (bot.vehicle?.id === a.id) {
            try { bot.moveVehicle(0, 0) } catch { /* Disconnect cleanup must preserve the original hand-back. */ }
          }
        }
      }
    }
  }
}
