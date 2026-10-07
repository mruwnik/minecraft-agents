// Why JavaScript: tests the Mineflayer adapter code for vehicles, which stays JS (Mineflayer boundary).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import { trackVehicles, ridersOf, vehicleFields, selfVehicle, mountRefusal, mountVehicle, dismountVehicle } from './vehicle.mjs'

const BODY = 4
const ctx = () => {
  const aborts = []
  return { aborts, alive: () => {}, onAbort: fn => aborts.push(fn) }
}
const opts = { timeScale: 0.02 }

const entity = (id, name, extra = {}) => ({ id, name, uuid: `u-${id}`, type: 'mob', position: vec3(1, 64, 0), height: 0.6, ...extra })

// a hand-made bot; `onMount(bot, target)` and `onSneak(bot, on)` script what the server answers
const makeBot = ({ entities = [], vehicle = null, held = null, free = true, onMount = () => {}, onSneak = () => {} } = {}) => {
  const bot = new EventEmitter()
  const written = []
  const controls = []
  const calls = []
  Object.assign(bot, {
    written,
    controls,
    calls,
    entity: { id: BODY, type: 'player', position: vec3(0, 64, 0), height: 1.62, yaw: 0, pitch: 0 },
    entities: Object.fromEntries(entities.map(e => [e.id, e])),
    vehicle,
    heldItem: held,
    inventory: { slots: free ? {} : Object.fromEntries(Array.from({ length: 9 }, (_, i) => [36 + i, { name: 'dirt' }])), firstEmptyInventorySlot: () => free ? 9 : null },
    _client: Object.assign(new EventEmitter(), { write: (name, data) => written.push([name, data]) }),
    setControlState: (name, on) => { controls.push([name, on]); if (name === 'sneak') onSneak(bot, on) },
    unequip: async () => { calls.push('unequip'); bot.heldItem = null },
    lookAt: async () => { calls.push('lookAt') },
    mount: target => { calls.push('mount'); onMount(bot, target) }
  })
  bot.entities[BODY] = bot.entity
  return bot
}

const passengers = (bot, entityId, ids) => bot._client.emit('set_passengers', { entityId, passengers: ids })

test('the tracker keeps full passenger lists and repairs bot.vehicle', async t => {
  await t.test('a list with the body sets bot.vehicle', () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat] })
    trackVehicles(bot)
    passengers(bot, 9, [BODY])
    assert.equal(bot.vehicle, boat)
    assert.deepEqual(ridersOf(bot, boat), [BODY])
  })
  await t.test('a list without the body clears bot.vehicle', () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat], vehicle: boat })
    trackVehicles(bot)
    passengers(bot, 9, [7])
    assert.equal(bot.vehicle, null)
    assert.deepEqual(ridersOf(bot, boat), [7])
  })
  await t.test('a list for another vehicle leaves bot.vehicle', () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat, entity(10, 'minecart')], vehicle: boat })
    trackVehicles(bot)
    passengers(bot, 10, [])
    assert.equal(bot.vehicle, boat)
  })
  await t.test('a gone vehicle drops its list and the body is off', () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat], vehicle: boat })
    trackVehicles(bot)
    passengers(bot, 9, [BODY, 7])
    bot.emit('entityGone', boat)
    assert.equal(bot.vehicle, null)
    assert.deepEqual(ridersOf(bot, boat), [])
  })
  await t.test('tracking twice listens once', () => {
    const bot = makeBot()
    trackVehicles(bot)
    trackVehicles(bot)
    assert.equal(bot._client.listenerCount('set_passengers'), 1)
  })
})

test('ridersOf falls back to mineflayer passengers before any packet', () => {
  const bot = makeBot()
  trackVehicles(bot)
  assert.deepEqual(ridersOf(bot, entity(9, 'oak_boat', { passengers: [{ id: 7 }] })), [7])
})

test('vehicleFields', async t => {
  const rows = [
    ['an empty boat', 9, [], {}],
    ['a cow outside the boat', 7, [], {}],
    ['a boat with a cow and the body', 9, [7, BODY], { passengers: [7, BODY] }],
    ['a cow in the boat', 7, [7], { vehicle: 9 }]
  ]
  for (const [label, id, riders, expected] of rows) {
    await t.test(label, () => {
      const bot = makeBot({ entities: [entity(9, 'oak_boat'), entity(7, 'cow')] })
      trackVehicles(bot)
      passengers(bot, 9, riders)
      assert.deepEqual(vehicleFields(bot, bot.entities[id]), expected)
    })
  }
  await t.test('a cow dropped from the list rides nothing, whatever mineflayer kept', () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat, entity(7, 'cow', { vehicle: boat })] })
    trackVehicles(bot)
    passengers(bot, 9, [7])
    passengers(bot, 9, [])
    assert.deepEqual(vehicleFields(bot, bot.entities[7]), {})
  })
})

test('selfVehicle', async t => {
  await t.test('on foot', () => assert.equal(selfVehicle(makeBot()), null))
  await t.test('aboard', () => assert.deepEqual(selfVehicle(makeBot({ vehicle: entity(9, 'oak_boat') })), { id: 9, uuid: 'u-9', name: 'oak_boat' }))
})

test('mountRefusal', async t => {
  const rows = [
    ['empty boat', 'oak_boat', [], null],
    ['boat with one cow', 'oak_boat', ['cow'], null],
    ['boat with two', 'birch_boat', ['cow', 'sheep'], 'occupied'],
    ['bamboo raft with one', 'bamboo_raft', ['villager'], null],
    ['chest boat with one', 'oak_chest_boat', ['cow'], 'occupied'],
    ['empty minecart', 'minecart', [], null],
    ['minecart with a villager', 'minecart', ['villager'], 'occupied'],
    ['pig', 'pig', [], null],
    ['horse ridden', 'horse', ['player'], 'occupied'],
    ['camel with one', 'camel', ['player'], null],
    ['cow', 'cow', [], 'not-mountable'],
    ['chest minecart', 'chest_minecart', [], 'not-mountable'],
    ['villager', 'villager', [], 'not-mountable']
  ]
  for (const [label, name, riders, expected] of rows) {
    await t.test(label, () => assert.equal(mountRefusal(name, riders.length), expected))
  }
})

const seat = (bot, target) => passengers(bot, target.id, [BODY])

test('mountVehicle refuses', async t => {
  const boat = entity(9, 'oak_boat')
  const rows = [
    ['already-mounted', { entities: [boat], vehicle: boat }, { id: 9 }, { status: 'already-mounted', vehicle: { id: 9, uuid: 'u-9', name: 'oak_boat' } }],
    ['gone', {}, { id: 9 }, { status: 'gone' }],
    ['not-mountable', { entities: [entity(9, 'cow')] }, { id: 9 }, { status: 'not-mountable' }],
    ['out-of-reach', { entities: [entity(9, 'oak_boat', { position: vec3(5, 64, 0) })] }, { id: 9 }, { status: 'out-of-reach' }],
    ['hand-full', { entities: [entity(9, 'pig')], held: { name: 'lead', count: 1 }, free: false }, { id: 9 }, { status: 'hand-full' }]
  ]
  for (const [label, spec, a, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot(spec)
      trackVehicles(bot)
      assert.deepEqual(await mountVehicle(bot, ctx(), a, opts), expected)
      assert.equal(bot.calls.includes('mount'), false)
    })
  }
  await t.test('occupied', async () => {
    const bot = makeBot({ entities: [entity(9, 'oak_boat'), entity(7, 'cow'), entity(8, 'cow')] })
    trackVehicles(bot)
    passengers(bot, 9, [7, 8])
    assert.deepEqual(await mountVehicle(bot, ctx(), { id: 9 }, opts), { status: 'occupied' })
  })
})

test('mountVehicle boards a boat or minecart with the hand as it is', async t => {
  for (const name of ['oak_boat', 'bamboo_chest_raft', 'minecart']) {
    await t.test(name, async () => {
      const bot = makeBot({ entities: [entity(9, name)], held: { name: 'lead', count: 1 }, free: false, onMount: seat })
      trackVehicles(bot)
      assert.equal((await mountVehicle(bot, ctx(), { id: 9 }, opts)).status, 'mounted')
      assert.deepEqual(bot.calls, ['lookAt', 'mount'])
    })
  }
})

test('mountVehicle mounts once the server lists the body, emptying the hand first', async () => {
  const bot = makeBot({ entities: [entity(9, 'pig')], held: { name: 'lead', count: 1 }, onMount: seat })
  trackVehicles(bot)
  const r = await mountVehicle(bot, ctx(), { id: 9 }, opts)
  assert.deepEqual(r, { status: 'mounted', vehicle: { id: 9, uuid: 'u-9', name: 'pig' } })
  assert.deepEqual(bot.calls, ['unequip', 'lookAt', 'mount'])
})

test('mountVehicle times out when the server never seats the body', async () => {
  const bot = makeBot({ entities: [entity(9, 'pig')] })
  trackVehicles(bot)
  assert.deepEqual(await mountVehicle(bot, ctx(), { id: 9 }, opts), { status: 'timeout' })
})

// the server answers a sneak by dropping the body from the list, then teleporting it beside the boat
const leaves = (bot, on) => {
  if (!on) return
  passengers(bot, 9, [])
  bot.entity.position = vec3(2, 65, 0)
  bot.emit('forcedMove')
}

test('dismountVehicle', async t => {
  await t.test('not-mounted', async () => {
    const bot = makeBot()
    assert.deepEqual(await dismountVehicle(bot, ctx(), {}, opts), { status: 'not-mounted' })
    assert.deepEqual(bot.controls, [])
  })
  await t.test('sneaks until the body is off and the server placed it, then releases sneak', async () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat], vehicle: boat, onSneak: leaves })
    trackVehicles(bot)
    const r = await dismountVehicle(bot, ctx(), {}, opts)
    assert.deepEqual(r, { status: 'dismounted', pos: { x: 2, y: 65, z: 0 } })
    assert.deepEqual(bot.controls, [['sneak', true], ['sneak', false]])
    assert.deepEqual(bot.written, [])
  })
  await t.test('a yaw is written as a raw look first', async () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat], vehicle: boat, onSneak: leaves })
    trackVehicles(bot)
    await dismountVehicle(bot, ctx(), { yaw: 90 }, opts)
    assert.equal(bot.written[0][0], 'look')
    assert.equal(bot.written[0][1].yaw, 90)
    assert.equal(bot.written[0][1].pitch, 0)
  })
  await t.test('removal without a server position is not done: timeout, still releasing sneak', async () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat], vehicle: boat, onSneak: (b, on) => on && passengers(b, 9, []) })
    trackVehicles(bot)
    assert.deepEqual(await dismountVehicle(bot, ctx(), {}, opts), { status: 'timeout', mounted: false })
    assert.deepEqual(bot.controls.at(-1), ['sneak', false])
  })
  await t.test('the server keeps the body aboard: timeout, mounted', async () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat], vehicle: boat })
    trackVehicles(bot)
    assert.deepEqual(await dismountVehicle(bot, ctx(), {}, opts), { status: 'timeout', mounted: true })
  })
  await t.test('an abort releases sneak', async () => {
    const boat = entity(9, 'oak_boat')
    const bot = makeBot({ entities: [boat], vehicle: boat })
    trackVehicles(bot)
    const c = ctx()
    const pending = dismountVehicle(bot, c, {}, { timeScale: 1 })
    await new Promise(resolve => setTimeout(resolve, 5))
    c.aborts.forEach(fn => fn())
    assert.deepEqual(bot.controls.at(-1), ['sneak', false])
    passengers(bot, 9, [])
    bot.emit('forcedMove')
    await pending
  })
})

test('a passenger follows its mount (the server sends a rider only rotation)', async t => {
  const horse = () => entity(9, 'skeleton_horse', { position: vec3(0, 64, 0), height: 1.6 })
  await t.test('moving the horse 10 blocks moves the skeleton with a seat offset', () => {
    const h = horse()
    const skel = entity(12, 'skeleton', { position: vec3(0, 64, 0) })
    const bot = makeBot({ entities: [h, skel] })
    trackVehicles(bot)
    passengers(bot, 9, [12])
    h.position = vec3(10, 64, 0)
    bot.emit('entityMoved', h)
    assert.equal(skel.position.x, 10)
    assert.ok(Math.abs(skel.position.y - 64.61875) < 1e-9)
    assert.equal(skel.position.z, 0)
  })
  await t.test('a rider whose list is gone is left alone', () => {
    const h = horse()
    const skel = entity(12, 'skeleton', { position: vec3(3, 64, 3) })
    const bot = makeBot({ entities: [h, skel] })
    trackVehicles(bot)
    h.position = vec3(10, 64, 0)
    bot.emit('entityMoved', h)
    assert.equal(skel.position.x, 3)
  })
})
