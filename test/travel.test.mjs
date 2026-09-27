import test from 'node:test'
import assert from 'node:assert/strict'
import { performance } from 'node:perf_hooks'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import { checkedRailRoute, planTravel, travelPoint } from '../src/navigation/travel.mjs'
import { makeTravelRuntime } from '../src/body/travel.mjs'
import travel from '../library/travel.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
const { Vec3 } = vec3

function terrain (length = 101, alter = () => null) {
  return (x, y, z) => alter(x, y, z) ?? (y < 64 || (x === length && y === 64 && z === 0)
    ? { name: 'stone', solid: true, properties: {} }
    : y === 64 && z === 0 && x >= 0 && x < length
      ? { name: x === 0 ? 'rail' : 'powered_rail', solid: false, properties: { shape: 'east_west', powered: x < length - 3 } }
      : { name: 'air', solid: false, properties: {} })
}
const routeArgs = { id: 7, track: '0:64:0,100:64:0', exit: '98:64:1' }
const route = () => checkedRailRoute(terrain(), routeArgs.track, travelPoint(routeArgs.exit))
test('rail validation checks full corridor, power, clearance, stop and both dismount platforms', () => {
  assert.equal(route().points.length, 101)
  assert.equal(route().stop.x, 98)
  for (const [name, alter] of [
    ['unloaded', (x, y, z) => x === 50 && y === 64 && z === 0 ? { name: 'air', properties: {}, solid: false } : null],
    ['power', (x, y, z) => x === 50 && y === 64 && z === 0 ? { name: 'powered_rail', properties: { powered: false, shape: 'east_west' } } : null],
    ['shape', (x, y, z) => x === 50 && y === 64 && z === 0 ? { name: 'powered_rail', properties: { powered: true, shape: 'north_south' } } : null],
    ['roof', (x, y, z) => x === 50 && y === 66 && z === 0 ? { name: 'stone', solid: true } : null],
    ['brake', (x, y, z) => x === 98 && y === 64 && z === 0 ? { name: 'powered_rail', properties: { powered: true, shape: 'east_west' } } : null],
    ['opposite platform', (x, y, z) => x === 98 && y === 63 && z === -1 ? { name: 'air', solid: false } : null]
  ]) assert.throws(() => checkedRailRoute(terrain(101, alter), routeArgs.track, travelPoint(routeArgs.exit)), undefined, name)
  assert.throws(() => checkedRailRoute(() => null, routeArgs.track, travelPoint(routeArgs.exit)), /unloaded/)
  assert.throws(() => checkedRailRoute(terrain(), '0:64:0,10:65:0', travelPoint(routeArgs.exit)), /flat/)
})
test('complete itineraries prefer useful rail and price approaches, setup, final walk and return', () => {
  const from = { x: 0, y: 64, z: 0 }, to = { x: 101, y: 64, z: 1 }
  const plan = planTravel({ from, to, rail: route(), returnTrip: true })
  assert.equal(plan.selected, 'rail')
  assert.ok(plan.options.find(o => o.mode === 'rail').seconds < plan.options[0].seconds)
  assert.equal(plan.returnPlan.mode, 'walk')
  assert.equal(planTravel({ from: { ...from, x: 95 }, to, rail: route() }).selected, 'walk')
  assert.equal(planTravel({ from, to, mode: 'boat' }).selected, null)
  assert.match(plan.options.find(o => o.mode === 'horse').reason, /controller/)
  assert.throws(() => planTravel({ from, to, mode: 'teleport' }))
})
test('rail planner validates 512-cell workload in milliseconds and exposes measured timing', t => {
  let reads = 0
  const blockAt = terrain(512)
  const began = performance.now()
  for (let i = 0; i < 20; i++) checkedRailRoute((...p) => { reads++; return blockAt(...p) }, '0:64:0,511:64:0', { x: 509, y: 64, z: 1 })
  const ms = (performance.now() - began) / 20
  t.diagnostic(`512-cell rail: ${ms.toFixed(3)} ms/plan; ${reads / 20} block reads`)
  assert.ok(reads / 20 < 2200)
})

function runtime (options = {}) {
  let tick = 0, dismounts = 0
  const inputs = []
  const blockAt = terrain()
  const cart = { id: 7, name: 'minecart', isValid: true, passengers: [], position: new Vec3(0.5, 64, 0.5) }
  const bot = Object.assign(new EventEmitter(), {
    health: 20, food: 20, oxygenLevel: 20, time: { timeOfDay: 1000 },
    entity: { id: 1, position: new Vec3(0.5, 64, 1.5) }, entities: { 7: cart }, vehicle: null,
    pathfinder: { setGoal () {} }, look: async () => {},
    blockAt (p) { const b = blockAt(p.x, p.y, p.z); return { name: b.name, boundingBox: b.solid ? 'block' : 'empty', getProperties: () => b.properties } },
    mount () { this.vehicle = cart; this.entity.vehicle = cart; cart.passengers = [this.entity] },
    dismount () {
      dismounts++
      const serverPosition = () => {
        if (!options.noPosition) this._client.emit('position', options.badPosition ? { x: 40.5, y: 64, z: 1.5 } : { x: 98.5, y: 64, z: 1.5 })
      }
      if (options.positionFirst) serverPosition()
      this._client.emit('set_passengers', { entityId: cart.id, passengers: [] })
      if (!options.positionFirst) serverPosition()
    },
    moveVehicle (...input) { inputs.push(input) },
    async waitForTicks (count) {
      for (let i = 0; i < count; i++) {
        tick++
        if (!options.stalled && this.vehicle) { cart.position.x = Math.min(98.5, cart.position.x + 0.35); this.entity.position = cart.position.clone() }
        options.onTick?.({ bot, cart, tick })
      }
    }
  })
  bot._client = Object.assign(new EventEmitter(), { write () {} })
  // Match the installed native plugin's omission: self exclusion does NOT
  // clear bot.vehicle. The runtime must process the authoritative full list.
  bot._client.on('set_passengers', packet => {
    if (packet.passengers.includes(bot.entity.id)) bot.vehicle = cart
  })
  // Native physics applies this server packet then emits forcedMove; it is
  // distinct from (and may arrive before) the passenger removal packet.
  bot._client.on('position', packet => { bot.entity.position = new Vec3(packet.x, packet.y, packet.z); bot.emit('forcedMove') })
  const driver = makeTravelRuntime({ getBot: () => bot, Vec3, pause: ms => bot.waitForTicks(ms / 50), cancelGuard: () => () => { if (options.cancelAt && tick >= options.cancelAt) throw new Error('cancelled') } })
  return { bot, cart, driver, inputs, dismounts: () => dismounts }
}
test('rail runtime rides with ordinary input, observes stop, and dismounts only after braking', async () => {
  const r = runtime()
  assert.deepEqual(r.driver.quick.rail_state({ id: 7 }).cart.passengers, [])
  const result = await r.driver.long.rail_ride(routeArgs)
  assert.equal(result.arrived, true)
  assert.equal(r.dismounts(), 1)
  assert.equal(r.cart.position.x, 98.5)
  assert.deepEqual(r.inputs[0], [0, 1])
  assert.deepEqual(r.inputs.at(-1), [0, 0])
})
test('modern rail dismount uses the shift packet and clears it after confirmed departure', async () => {
  const r = runtime()
  const packets = []
  r.bot.supportFeature = feature => feature === 'newPlayerInputPacket'
  r.bot._client.write = (name, packet) => { packets.push({ name, packet }); if (packet.inputs.shift) r.bot.dismount() }
  await r.driver.long.rail_ride(routeArgs)
  assert.deepEqual(packets, [
    { name: 'player_input', packet: { inputs: { shift: true } } },
    { name: 'player_input', packet: { inputs: {} } }
  ])
})
test('rail departure requires authoritative passenger removal and a safe fresh server position in either packet order', async () => {
  for (const positionFirst of [true, false]) {
    const r = runtime({ positionFirst })
    const exits = []
    r.bot.on('dismount', vehicle => exits.push(vehicle.id))
    const initialPassengerListeners = r.bot._client.listenerCount('set_passengers')
    await r.driver.long.rail_ride(routeArgs)
    assert.equal(r.bot.vehicle, null)
    assert.equal(r.bot.entity.vehicle, null)
    assert.deepEqual(r.cart.passengers, [])
    assert.deepEqual(exits, [7])
    assert.equal(r.bot._client.listenerCount('set_passengers'), initialPassengerListeners)
    assert.equal(r.bot.listenerCount('forcedMove'), 0)
  }
  for (const options of [{ noPosition: true }, { badPosition: true }]) {
    const r = runtime(options)
    await assert.rejects(r.driver.long.rail_ride(routeArgs), /server dismount position/)
    assert.equal(r.bot.vehicle, null, 'server passenger removal is retained despite absent/unsafe landing evidence')
    assert.equal(r.bot.listenerCount('forcedMove'), 0)
  }
})
test('cancel, blocked rail, no progress and passenger takeover stop without unsafe dismount', async () => {
  for (const options of [
    { cancelAt: 30 }, { stalled: true },
    { onTick: ({ bot, tick }) => { if (tick === 20) bot.health = 5 } },
    { onTick: ({ bot, tick }) => { if (tick === 20) bot.food = 3 } },
    { onTick: ({ bot, tick }) => { if (tick === 20) bot.oxygenLevel = 3 } },
    { onTick: ({ bot, tick }) => { if (tick === 20) bot.time.timeOfDay = 14000 } },
    { onTick: ({ cart, tick }) => { if (tick === 20) cart.passengers.push({ id: 9 }) } },
    { onTick: ({ bot, tick }) => { if (tick === 20) bot.blockAt = () => null } }
  ]) {
    const r = runtime(options)
    await assert.rejects(r.driver.long.rail_ride(routeArgs))
    assert.equal(r.dismounts(), 0)
    assert.deepEqual(r.inputs.at(-1), [0, 0])
  }
})
function fakeApi (respond = () => null) {
  const calls = []
  return { calls, pos: () => ({ x: 0, y: 64, z: 0 }), block: terrain(), checkpoint: async () => {}, report: () => {},
    async act (name, args) {
      calls.push({ name, args })
      return respond(name, args) ?? (name === 'rail_state' ? { mounted: null, cart: { position: { x: 0.5, y: 64, z: 0.5 }, passengers: [] } } : {})
    }
  }
}
const tripArgs = { x: 101, y: 64, z: 1, cart: 7, track: routeArgs.track, exit: routeArgs.exit }
test('travel plan is read-only, actual rail includes approach and final walk, ordinary walk remains compatible', async () => {
  let api = fakeApi()
  assert.equal((await travel.run(api, { ...tripArgs, plan: true })).selected, 'rail')
  assert.deepEqual(api.calls.map(c => c.name), ['rail_state'])
  api = fakeApi()
  assert.equal((await travel.run(api, tripArgs)).mode, 'rail')
  assert.deepEqual(api.calls.map(c => c.name), ['rail_state', 'rail_state', 'goto', 'rail_ride', 'rail_state', 'goto'])
  api = fakeApi()
  assert.equal((await travel.run(api, { x: 10, y: 64, z: 0 })).mode, 'walk')
  assert.deepEqual(api.calls.map(c => c.name), ['rail_state', 'rail_state', 'goto'])
})
test('travel excludes stale assets, refuses forced unsupported modes, never walks after failed ride or handback', async () => {
  const api = fakeApi(name => name === 'rail_state' ? { mounted: null, cart: null } : null)
  assert.equal((await travel.run(api, tripArgs)).mode, 'walk')
  await assert.rejects(travel.run(fakeApi(), { x: 1, y: 64, z: 0, mode: 'boat' }), /physics/)
  for (const failure of [new Error('no progress'), new CompositeHandBack('health')]) {
    const failed = fakeApi(name => { if (name === 'rail_ride') throw failure })
    await assert.rejects(travel.run(failed, tripArgs), error => error === failure)
    assert.equal(failed.calls.at(-1).name, 'rail_ride')
  }
})
test('travel refuses mounted walking, reports mounted plans, and rechecks before every walk', async () => {
  for (const args of [{ x: 1, y: 64, z: 0 }, { ...tripArgs, mode: 'walk' }, tripArgs]) {
    const api = fakeApi(name => name === 'rail_state' ? { mounted: 7, cart: null } : null)
    await assert.rejects(travel.run(api, args), /riding vehicle 7/)
    assert.equal(api.calls.some(call => call.name === 'goto'), false)
    const plan = await travel.run(api, { ...args, plan: true })
    assert.equal(plan.selected, null)
    assert.equal(plan.mounted, 7)
    assert.equal(plan.options.some(option => option.available), false)
  }
  const beforeWalk = fakeApi()
  beforeWalk.checkpoint = async () => { beforeWalk.act = async name => name === 'rail_state' ? { mounted: 9 } : assert.fail('no walk while mounted') }
  await assert.rejects(travel.run(beforeWalk, { x: 1, y: 64, z: 0 }), /riding vehicle 9/)
  let rode = false
  const afterRide = fakeApi(name => {
    if (name === 'rail_ride') rode = true
    if (name === 'rail_state' && rode) return { mounted: 7 }
  })
  await assert.rejects(travel.run(afterRide, tripArgs), /riding vehicle 7/)
  assert.equal(afterRide.calls.filter(call => call.name === 'goto').length, 1)
})
test('travel falls back only for expected rail validation errors; programming and safety errors escape', async () => {
  const blocked = fakeApi()
  blocked.block = () => null
  assert.equal((await travel.run(blocked, tripArgs)).mode, 'walk')
  for (const error of [new TypeError('bad terrain shape'), new ReferenceError('missing lookup'), new CompositeHandBack('health'), new Error('cancelled')]) {
    const api = fakeApi()
    api.block = () => { throw error }
    await assert.rejects(travel.run(api, tripArgs), caught => caught === error)
    assert.equal(api.calls.some(call => call.name === 'goto'), false)
  }
})

test('horse itineraries price approach and speed, validate the full corridor, and dismount before final walking', async () => {
  const horse = { from: { x: 0.5, y: 64, z: 0.5 }, distance: 100, speed: 0.3 }
  assert.equal(planTravel({ from: horse.from, to: { x: 100, y: 64, z: 0 }, horse }).selected, 'horse')
  assert.equal(planTravel({ from: horse.from, to: { x: 100, y: 64, z: 0 }, horse: { ...horse, speed: 0.09 } }).selected, 'walk')
  const args = { x: 100, y: 64, z: 0, horse: 8 }
  const build = (failure = null) => {
    const api = fakeApi(name => {
      if (name === 'horse_state') return { mounted: null, goalTravel: true, horse: { id: 8, at: horse.from, tamed: true, saddled: true, baby: false, passengers: [], movementSpeed: 0.3 } }
      if (name === 'horse_dismount' && failure) throw failure
    })
    api.block = (x, y, z) => ({ name: y < 64 ? 'stone' : 'air', solid: y < 64, shapes: y < 64 ? [[0, 0, 0, 1, 1, 1]] : [], properties: {} })
    return api
  }
  const plan = await travel.run(build(), { ...args, plan: true })
  assert.equal(plan.selected, 'horse')
  assert.match(plan.resume, /horse=8/)
  const api = build()
  assert.equal((await travel.run(api, args)).mode, 'horse')
  assert.deepEqual(api.calls.map(c => c.name), ['rail_state', 'horse_state', 'rail_state', 'goto', 'ride', 'horse_dismount', 'rail_state', 'goto'])
  const blocked = build(new Error('landing unavailable'))
  await assert.rejects(travel.run(blocked, args), /landing unavailable/)
  assert.equal(blocked.calls.at(-1).name, 'horse_dismount')
  const cliff = build(); cliff.block = () => null
  assert.equal((await travel.run(cliff, { ...args, plan: true })).selected, 'walk')
})
