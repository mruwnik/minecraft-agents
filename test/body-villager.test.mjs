import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import { makeBoatRuntime } from '../src/body/boat.mjs'
import { makeVillagerRuntime } from '../src/body/villager.mjs'
const { Vec3 } = vec3
const entity = (id, name, uuid = `00000000-0000-4000-8000-${String(id).padStart(12, '0')}`) => ({ id, name, uuid, isValid: true, position: new Vec3(0, 64, 0), width: .6, height: 1.95, metadata: { 16: false, 19: { profession: 'farmer' } } })
const client = () => ({ entity: entity(1, 'player'), entities: {}, _client: new EventEmitter() })
function boatHarness () {
  let bot = client(), leads = new Map()
  const runtime = makeBoatRuntime({ getBot: () => bot, getBoatLeashHolder: () => leads })
  return { runtime, get bot () { return bot }, get leads () { return leads }, reconnect () { bot = client(); leads = new Map(); runtime.attach() } }
}

test('boat runtime replaces packet passenger membership rather than retaining a former rider', () => {
  const h = boatHarness(), boat = entity(2, 'oak_chest_boat'), old = entity(3, 'villager'), next = entity(4, 'villager')
  Object.assign(h.bot.entities, { 2: boat, 3: old, 4: next })
  boat.passengers = [old]; old.vehicle = boat
  h.runtime.attach()
  h.bot._client.emit('set_passengers', { entityId: 2, passengers: [4] })
  assert.equal(old.vehicle, null)
  assert.equal(next.vehicle, boat)
  assert.deepEqual(h.runtime.quick.boat_state({}).boats[0].passengers.map(p => [p.uuid, p.width, p.height, p.baby]), [[next.uuid, .6, 1.95, false]])
  h.bot._client.emit('set_passengers', { entityId: 2, passengers: [] })
  assert.equal(next.vehicle, null)
  assert.deepEqual(boat.passengers, [])
})

test('boat reconnect hooks read the new body and reset lead map, with one subscription per new client', () => {
  const h = boatHarness(), first = h.bot
  first.entities[2] = entity(2, 'oak_boat'); first.entities[3] = entity(3, 'cow')
  h.runtime.attach()
  first._client.emit('attach_entity', { entityId: 2, vehicleId: 1 })
  first._client.emit('attach_entity', { entityId: 3, vehicleId: 1 })
  assert.deepEqual([...h.leads], [[2, 1]])
  h.reconnect()
  assert.equal(h.leads.size, 0)
  assert.equal(h.bot._client.listenerCount('set_passengers'), 1)
  assert.equal(h.bot._client.listenerCount('attach_entity'), 1)
  assert.deepEqual(h.runtime.quick.boat_state({}).boats, [])
  h.bot.entities[5] = entity(5, 'birch_boat')
  h.bot._client.emit('attach_entity', { entityId: 5, vehicleId: 1 })
  assert.equal(h.runtime.quick.boat_state({}).boats[0].leashHolderId, 1)
  h.bot._client.emit('attach_entity', { entityId: 5, vehicleId: -1 })
  assert.equal(h.runtime.quick.boat_state({}).boats[0].leashHolderId, null)
})

test('vehicle identity ignores destroyed, invalid and replaced cached boat objects', () => {
  const h = boatHarness(), boat = entity(2, 'oak_boat'), rider = entity(3, 'villager')
  rider.vehicle = boat; h.bot.entities[2] = boat
  assert.equal(h.runtime.currentVehicleId(rider), 2)
  boat.isValid = false
  assert.equal(h.runtime.currentVehicleId(rider), null)
  boat.isValid = true; h.bot.entities[2] = entity(2, 'oak_boat')
  assert.equal(h.runtime.currentVehicleId(rider), null)
  delete h.bot.entities[2]
  assert.equal(h.runtime.currentVehicleId(rider), null)
})

function merchantHarness () {
  const bot = client(); bot.entities[2] = entity(2, 'villager')
  bot.registry = { enchantments: {} }; bot.waitForTicks = async () => {}
  let inventory = { emerald: 10 }, opens = 0, purchases = 0, closed = 0
  const offer = { inputItem1: { name: 'emerald', count: 1 }, outputItem: { name: 'bread', count: 1 }, nbTradeUses: 0, maximumNbTradeUses: 10, tradeDisabled: false }
  const window = { trades: [offer], close: () => { closed++ } }
  bot.openVillager = async () => { opens++; return window }
  bot.trade = async () => { purchases++; inventory = { emerald: inventory.emerald - offer.inputItem1.count, bread: 1 } }
  const observations = []
  const deps = { getBot: () => bot, Vec3, goNear: async () => {}, inventoryCounts: () => inventory, by: 'Probe', recordVillagerObservation: v => observations.push(v) }
  return { bot, runtime: makeVillagerRuntime(deps), deps, window, offer, observations, get opens () { return opens }, get purchases () { return purchases }, get closed () { return closed }, setInventory: value => { inventory = value } }
}
const microtasks = async () => { for (let i = 0; i < 8; i++) await Promise.resolve() }

test('timed-out villager windows stay blocked, and a late window is closed before a fresh runtime can reopen', async t => {
  t.mock.timers.enable({ apis: ['setTimeout'] })
  const h = merchantHarness(); let resolve
  h.bot.openVillager = () => new Promise(r => { resolve = r })
  const opening = h.runtime.long.trades({})
  await microtasks(); t.mock.timers.tick(5000)
  await assert.rejects(opening, /took longer than 5s/)
  resolve(h.window); await microtasks()
  assert.equal(h.closed, 1)
  await assert.rejects(h.runtime.long.trades({}), /window timed out/)
  h.bot.openVillager = async () => h.window
  const fresh = makeVillagerRuntime(h.deps)
  assert.equal((await fresh.long.trades({})).offers.length, 1)
})

test('a timed-out purchase remains uncertain even after the server promise resolves, preventing duplicate trades', async t => {
  t.mock.timers.enable({ apis: ['setTimeout'] })
  const h = merchantHarness(); let resolve, attempts = 0
  h.bot.trade = () => { attempts++; return new Promise(r => { resolve = r }) }
  const buying = h.runtime.long.trade({ offer: 1 })
  await microtasks(); t.mock.timers.tick(10000)
  await assert.rejects(buying, /trade outcome uncertain/)
  resolve(); await microtasks()
  await assert.rejects(h.runtime.long.trade({ offer: 1 }), /prior trade outcome is uncertain/)
  assert.equal(attempts, 1)
  assert.equal(h.closed, 1)
})

test('lectern cache invalidation lets a changed offer be checked afresh and preserves confirmed payment', async () => {
  const h = merchantHarness()
  h.offer.inputItem1.count = 3
  await h.runtime.long.trades({})
  h.setInventory({ emerald: 1 })
  await assert.rejects(h.runtime.long.trade({ offer: 1 }), /trade needs 3 emerald/)
  h.offer.inputItem1.count = 1
  h.runtime.invalidateOffers()
  const result = await h.runtime.long.trade({ offer: 1 })
  assert.equal(result.bought, 'bread:1')
  assert.deepEqual(result.paid, { emerald: 1 })
  assert.equal(h.purchases, 1)
})

test('UUID trading refuses recycled numeric IDs, including replacement during approach', async () => {
  const h = merchantHarness(), original = h.bot.entities[2]
  const uuid = original.uuid
  h.bot.entities[2] = entity(2, 'villager', '00000000-0000-4000-8000-999999999999')
  await assert.rejects(h.runtime.long.trades({ id: 2, uuid }), /with UUID/)
  assert.equal(h.opens, 0)
  h.bot.entities[2] = original
  const deps = { ...h.deps, goNear: async () => { h.bot.entities[2] = entity(2, 'villager') } }
  const runtime = makeVillagerRuntime(deps)
  await assert.rejects(runtime.long.trade({id:2,uuid,offer:1}), /exact villager changed/)
  assert.equal(h.purchases, 0)
})

test('merchant reads save observed offers and only an inventory-confirmed purchase adds lock evidence', async () => {
  const h = merchantHarness(), uuid = h.bot.entities[2].uuid
  await h.runtime.long.trades({ uuid })
  assert.equal(h.observations.length, 1)
  assert.equal(h.observations[0].uuid, uuid)
  assert.equal(h.observations[0].offers[0].outputItem.name, 'bread')
  assert.equal(h.observations[0].purchase, undefined)
  const bought = await h.runtime.long.trade({ uuid, offer: 1 })
  assert.equal(bought.bought, 'bread:1')
  assert.equal(h.observations.length, 2)
  assert.equal(h.observations[1].purchase.bought, 'bread')
  assert.equal(h.observations[1].purchase.boughtCount, 1)
})
