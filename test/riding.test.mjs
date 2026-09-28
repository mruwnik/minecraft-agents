import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { makeRidingRuntime, horseState, horseInventoryAction, repairHorseWindowInventory } from '../src/body/riding.mjs'

function fixture (options = {}) {
  const horse = { id: 9, name: options.name ?? 'horse', isValid: true, position: { x: 1, y: 64, z: 0 }, metadata: [false, options.flags ?? 0], passengers: [] }
  const bot = Object.assign(new EventEmitter(), {
    registry: { entitiesByName: Object.fromEntries(['horse', 'donkey', 'mule'].map(name => [name, { metadataKeys: ['baby', 'flags'] }])) },
    entity: { id: 1, position: { x: 0, y: 64, z: 0 } }, entities: { 9: horse }, health: 20, oxygenLevel: 20,
    inventory: { items: () => options.saddle === false ? [] : [{ name: 'saddle' }] },
    pathfinder: { setGoal: () => {} }, supportFeature: () => true,
    unequip: async () => {}, mount () { this.vehicle = horse; horse.passengers = [this.entity] },
    closeWindow () { this.currentWindow = null },
    transfer: async () => { horse.metadata[1] |= 4 }
  })
  const client = new EventEmitter(), writes = []
  client.write = (name, packet) => {
    writes.push({ name, packet })
    if (name === 'entity_action') {
      client.emit('open_horse_window', { entityId: 9, windowId: 2 })
      const slots = Array(38).fill(null)
      slots[2] = { name: 'saddle', type: 99 }
      bot.currentWindow = { id: 2, inventoryStart: 0, inventoryEnd: 36, slots }
      client.emit('window_items', { windowId: 2, items: slots })
    }
  }
  bot._client = client
  let ticks = 0, clock = 0, cancelled = false
  const reports = []
  const runtime = makeRidingRuntime({ getBot: () => bot, allowEntity: () => options.allowed !== false,
    cancelGuard: () => () => { if (cancelled) throw new Error('cancelled') },
    now: () => clock, report: p => reports.push(p), driveHorse: options.driveHorse,
    pause: async ms => { ticks++; clock += ms; await options.onPause?.({ bot, horse, client, ticks, ms, cancel: () => { cancelled = true } }) } })
  return { bot, horse, client, writes, reports, runtime }
}

test('horse state follows registry keys and unknown metadata stays unknown', () => {
  const { bot, horse } = fixture({ flags: 6 })
  assert.equal(horseState(bot, horse).tamed, true)
  assert.equal(horseState(bot, horse).saddled, true)
  horse.metadata = []
  assert.equal(horseState(bot, horse).tamed, null)
  assert.equal(horseState(bot, { ...horse, name: 'pig' }), null)
})
test('received sparse horse flags retain the server default adult age', () => {
  const { bot, horse } = fixture()
  horse.metadata = []
  assert.equal(horseState(bot, horse).baby, null)
  horse.metadata[1] = 16
  assert.equal(horseState(bot, horse).baby, false)
  assert.equal(horseState(bot, horse).tamed, false)
  horse.metadata[0] = true
  assert.equal(horseState(bot, horse).baby, true)
})
test('horse inventory opening uses the actual negotiated protocol action', () => {
  const protocol = createRequire(import.meta.url)('minecraft-protocol')
  for (const version of ['1.21.5', '26.1']) {
    const actionId = horseInventoryAction({ version })
    const serializer = protocol.createSerializer({ state: 'play', version })
    const deserializer = protocol.createDeserializer({ state: 'play', isServer: true, version })
    const packet = { name: 'entity_action', params: { entityId: 9, actionId, jumpBoost: 0 } }
    const decoded = deserializer.parsePacketBuffer(serializer.createPacketBuffer(packet)).data.params
    assert.equal(decoded.actionId, version === '26.1' ? 'open_vehicle_inventory' : 6)
  }
})
test('horse inventory uses authoritative equipment offset before native clicks', () => {
  const registry = createRequire(import.meta.url)('prismarine-registry')('26.1')
  const windows = createRequire(import.meta.url)('prismarine-windows')(registry)
  const window = windows.createWindow(2, 'HorseWindow', 'Horse', 0)
  window.slots.length = 38
  repairHorseWindowInventory(window, 38)
  assert.equal(window.inventoryStart, 2)
  assert.equal(window.inventoryEnd, 38)
  assert.equal(window.hotbarStart, 29)
  assert.throws(() => repairHorseWindowInventory(window, 37), /unsupported horse inventory layout/)
})
test('modern horse saddle confirmation follows the actual server equipment packet', () => {
  const require = createRequire(import.meta.url)
  const protocol = require('minecraft-protocol')
  for (const version of ['1.21.5', '26.1']) {
    const registry = require('prismarine-registry')(version)
    const Item = require('prismarine-item')(registry)
    const Entity = require('prismarine-entity')(registry)
    const entity = new Entity(9)
    entity.name = 'horse'
    entity.position.set(0, 64, 0)
    entity.metadata = []
    entity.metadata[registry.entitiesByName.horse.metadataKeys.indexOf('flags')] = 2
    const serializer = protocol.createSerializer({ state: 'play', isServer: true, version })
    const deserializer = protocol.createDeserializer({ state: 'play', version })
    const saddle = new Item(registry.itemsByName.saddle.id, 1)
    const packet = { name: 'entity_equipment', params: { entityId: 9, equipments: [{ slot: 7, item: Item.toNotch(saddle) }] } }
    const decoded = deserializer.parsePacketBuffer(serializer.createPacketBuffer(packet)).data.params
    for (const equipment of decoded.equipments) entity.setEquipment(equipment.slot, Item.fromNotch(equipment.item))
    assert.equal(horseState({ registry }, entity).saddled, true)
    entity.setEquipment(7, null)
    assert.equal(horseState({ registry }, entity).saddled, false)
  }
})
test('tame remounts after server bucking and requires confirmed tame metadata', async () => {
  const f = fixture({ onPause ({ horse, bot, client, ticks }) {
    if (ticks === 2) client.emit('set_passengers', { entityId: 9, passengers: [] })
    if (ticks === 5) horse.metadata[1] |= 2
  } })
  const result = await f.runtime.long.tame({ id: 9 })
  assert.equal(result.tamed, true)
  assert.equal(result.attempts, 2)
  assert.ok(f.reports.some(p => p.status.startsWith('bucked')))
  assert.equal(f.client.listenerCount('set_passengers'), 0)
  assert.deepEqual(f.writes.at(-1), { name: 'player_input', packet: { inputs: {} } })
})
test('untamed persistent seating yields a finite partial result', async () => {
  const f = fixture()
  const result = await f.runtime.long.tame({ id: 9, seconds: 1 })
  assert.equal(result.complete, false)
  assert.equal(result.tamed, false)
  assert.equal(result.mounted, true)
  assert.equal(f.client.listenerCount('set_passengers'), 0)
})
test('cancellation neutralizes controls and does not dismount', async () => {
  const f = fixture({ onPause: ({ cancel }) => cancel() })
  await assert.rejects(f.runtime.long.tame({ id: 9 }), /cancelled/)
  assert.equal(f.bot.vehicle, f.horse)
  assert.deepEqual(f.writes.at(-1).packet, { inputs: {} })
  assert.equal(f.client.listenerCount('set_passengers'), 0)
})
test('taming refuses protected, unsupported, baby and unknown flags before mounting', async () => {
  for (const options of [{ allowed: false }, { name: 'pig' }]) {
    const f = fixture(options)
    await assert.rejects(f.runtime.long.tame({ id: 9 }))
    assert.equal(f.bot.vehicle, undefined)
  }
  const f = fixture(); f.horse.metadata[0] = true
  await assert.rejects(f.runtime.long.tame({ id: 9 }), /adult/)
  f.horse.metadata = []
  await assert.rejects(f.runtime.long.tame({ id: 9 }), /confirmed/)
})
test('ride auto-saddles through correlated horse inventory and confirmed metadata', async () => {
  const f = fixture({ flags: 2 })
  const result = await f.runtime.long.ride({ id: 9 })
  assert.equal(result.mounted, true)
  assert.equal(result.saddled, true)
  assert.equal(f.bot.currentWindow, null)
  assert.equal(f.client.listenerCount('open_horse_window'), 0)
})
test('missing saddle and unproved goal controller refuse before boarding', async () => {
  const f = fixture({ flags: 2, saddle: false })
  await assert.rejects(f.runtime.long.ride({ id: 9 }), /carry a saddle/)
  assert.equal(f.bot.vehicle, undefined)
  await assert.rejects(f.runtime.long.ride({ id: 9, x: 5, y: 64, z: 0 }), /verified horse physics/)
  assert.equal(f.bot.vehicle, undefined)
})
test('injected verified goal driver gets safety guard and exact requested goal', async () => {
  let received
  const f = fixture({ flags: 6, driveHorse: async a => { received = a; a.check(); return { arrived: true } } })
  const result = await f.runtime.long.ride({ id: 9, x: 6, y: 64, z: 0 })
  assert.equal(received.entity, f.horse)
  assert.deepEqual(received.goal, { x: 6, y: 64, z: 0 })
  assert.equal(result.trip.arrived, true)
})
test('vitals, occupied horse and lost horse identity abort without unsafe dismount', async () => {
  const f = fixture({ flags: 6 })
  f.bot.health = 4
  await assert.rejects(f.runtime.long.ride({ id: 9 }), /health/)
  f.bot.health = 20; f.horse.passengers = [{ id: 20 }]
  await assert.rejects(f.runtime.long.ride({ id: 9 }), /occupied/)
  const changed = fixture({ onPause: ({ bot }) => { bot.entities[9] = { ...bot.entities[9] } } })
  await assert.rejects(changed.runtime.long.tame({ id: 9 }), /identity/)
  assert.equal(changed.client.listenerCount('set_passengers'), 0)
})

test('saddle rejects an unconfirmed transfer and cleans its owned window', async () => {
  const f = fixture({ flags: 2 })
  f.bot.transfer = async () => {}
  await assert.rejects(f.runtime.long.horse_saddle({ id: 9 }), /not confirmed by horse metadata/)
  assert.equal(f.bot.currentWindow, null)
  assert.equal(f.client.listenerCount('open_horse_window'), 0)
  assert.equal(f.client.listenerCount('set_passengers'), 0)
  assert.equal(f.bot.vehicle, f.horse)
})
test('horse window identity is verified before transferring saddle', async () => {
  const f = fixture({ flags: 2 })
  let transferred = false
  f.bot.transfer = async () => { transferred = true }
  f.client.write = (name, packet) => {
    if (name === 'entity_action') {
      f.client.emit('open_horse_window', { entityId: 50, windowId: 2 })
      f.bot.currentWindow = { id: 2 }
    }
  }
  await assert.rejects(f.runtime.long.horse_saddle({ id: 9 }), /inventory was not confirmed/)
  assert.equal(transferred, false)
  assert.equal(f.bot.currentWindow.id, 2, 'does not close an unowned window')
})
test('approach rechecks animal identity and access before mounting', async () => {
  const f = fixture({ flags: 6 })
  f.horse.position.x = 20
  let observedArgs
  const runtime = makeRidingRuntime({ getBot: () => f.bot, cancelGuard: () => () => {},
    allowEntity: (entity, args) => { observedArgs = args; return true },
    goNear: async () => { f.bot.entities[9] = { ...f.horse } } })
  await assert.rejects(runtime.long.ride({ id: 9, penned: true }), /identity/)
  assert.equal(observedArgs.penned, true)
  assert.equal(f.bot.vehicle, undefined)
})

test('horse_state without id lists nearby supported animals', () => {
  const f = fixture()
  assert.deepEqual(f.runtime.quick.horse_state({}).horses.map(h => h.id), [9])
})
function dismountFixture () {
  const f = fixture({ flags: 6 })
  f.bot.vehicle = f.horse; f.horse.passengers = [f.bot.entity]
  Object.assign(f.horse, { yaw: 0, width: 1.4, height: 1.6 })
  f.bot.blockAt = p => ({ name: p.y < 64 ? 'stone' : 'air', boundingBox: p.y < 64 ? 'block' : 'empty', getProperties: () => ({}) })
  return f
}
test('explicit dismount waits for passenger removal and authoritative dry landing', async () => {
  const f = dismountFixture()
  const write = f.client.write
  f.client.write = (name, packet) => {
    write(name, packet)
    if (name === 'player_input' && packet.inputs.shift) {
      f.client.emit('set_passengers', { entityId: 9, passengers: [] })
      f.bot.entity.position = { x: 2, y: 64, z: 0 }
      f.bot.emit('forcedMove')
    }
  }
  const result = await f.runtime.long.horse_dismount({})
  assert.equal(result.dismounted, 9)
  assert.equal(f.bot.vehicle, null)
  assert.equal(f.bot.listenerCount('forcedMove'), 0)
  assert.deepEqual(f.writes.at(-1).packet, { inputs: {} })
})
test('dismount rejects moving horse and unsafe landing before sending shift', async () => {
  const moving = dismountFixture(); moving.horse.velocity = { x: 0.2, y: 0, z: 0 }
  await assert.rejects(moving.runtime.long.horse_dismount({}), /stationary/)
  assert.equal(moving.writes.some(w => w.packet.inputs?.shift), false)
  const unsafe = dismountFixture(); unsafe.bot.blockAt = () => null
  await assert.rejects(unsafe.runtime.long.horse_dismount({}), /unloaded/)
  assert.equal(unsafe.writes.some(w => w.packet.inputs?.shift), false)
})
test('passenger removal alone never claims a successful dismount', async () => {
  const f = dismountFixture(), write = f.client.write
  f.client.write = (name, packet) => {
    write(name, packet)
    if (packet.inputs?.shift) f.client.emit('set_passengers', { entityId: 9, passengers: [] })
  }
  await assert.rejects(f.runtime.long.horse_dismount({}), /server landing position/)
  assert.equal(f.bot.listenerCount('forcedMove'), 0)
  assert.deepEqual(f.writes.at(-1).packet, { inputs: {} })
})

test('horse state exposes confirmed movement attribute and goal controller availability', () => {
  const f = fixture()
  f.bot.registry.attributesByName = { movementSpeed: { resource: 'minecraft:movement_speed' } }
  assert.equal(horseState(f.bot, f.horse).movementSpeed, null)
  f.horse.attributes = { 'minecraft:movement_speed': { value: 0.2, modifiers: [{ operation: 0, amount: 0.05 }] } }
  assert.equal(horseState(f.bot, f.horse).movementSpeed, 0.25)
  assert.equal(f.runtime.quick.horse_state({}).goalTravel, false)
})
test('goal preflight refuses before saddling or mounting', async () => {
  const controller = async () => { throw new Error('must not drive') }
  controller.validate = async () => { throw new Error('blocked horse corridor') }
  const f = fixture({ flags: 2, driveHorse: controller })
  await assert.rejects(f.runtime.long.ride({ id: 9, x: 10, y: 64, z: 0 }), /blocked horse corridor/)
  assert.equal(f.bot.vehicle, undefined)
  assert.equal(f.writes.some(w => w.name === 'entity_action'), false)
})

// Exercise the actual registered animals handler without booting a live body.
import fs from 'node:fs'
import { createRequire } from 'node:module'
const { Vec3 } = createRequire(import.meta.url)('vec3')
test('registered animals discovers horse family within range with version-aware adult metadata', () => {
  const source = fs.readFileSync(new URL('../src/bot.mjs', import.meta.url), 'utf8')
  const begin = source.indexOf('  animals (a) {')
  const end = source.indexOf('\n  // x= y= z= anchors', begin)
  const build = new Function('bot', 'CREATURE_FOOD', 'matcher', 'penAround', 'vecOf', 'unpenned', 'isBaby', 'horseState', `return ({${source.slice(begin, end)}}).animals`)
  const f = fixture()
  f.bot.entity.position = new Vec3(0, 64, 0)
  for (const [i, name] of ['horse', 'donkey', 'mule'].entries()) {
    f.bot.entities[9 + i] = { ...f.horse, id: 9 + i, name, position: new Vec3(i + 1, 64, 0), metadata: [i === 1, 0] }
  }
  f.bot.entities[12] = { ...f.horse, id: 12, name: 'horse', position: new Vec3(50, 64, 0) }
  f.bot.entities[13] = { ...f.horse, id: 13, name: 'horse', position: new Vec3(4, 64, 0), metadata: [] }
  const animals = build(f.bot, {}, value => name => value === name, () => null, () => null, () => [], () => false, horseState)
  const all = animals({ within: 8 }).found
  assert.deepEqual(all.map(e => [e.mob, e.grown]), [['horse', true], ['donkey', false], ['mule', true], ['horse', false]])
  assert.deepEqual(animals({ mob: 'mule', within: 8 }).found.map(e => e.id), [11])
})

test('explicit safe dismount remains available at night and low vitals', async () => {
  const f = dismountFixture(), write = f.client.write
  f.bot.health = 4; f.bot.food = 2; f.bot.oxygenLevel = 3; f.bot.time = { timeOfDay: 14000 }
  f.client.write = (name, packet) => {
    write(name, packet)
    if (packet.inputs?.shift) {
      f.client.emit('set_passengers', { entityId: 9, passengers: [] })
      f.bot.entity.position = { x: 2, y: 64, z: 0 }; f.bot.emit('forcedMove')
    }
  }
  assert.equal((await f.runtime.long.horse_dismount({})).dismounted, 9)
})

test('ride step preview checks the requested terrain without saddling or boarding',async()=>{
  let received,driven=false
  const driveHorse=async()=>{driven=true}
  driveHorse.validate=args=>{received=args;return{terrain:args.terrain,ticks:60}}
  const f=fixture({flags:2,driveHorse})
  const result=await f.runtime.long.ride({id:9,x:6,y:65,z:0,terrain:'steps',plan:true})
  assert.equal(received.terrain,'steps')
  assert.deepEqual(result.plan,{terrain:'steps',ticks:60})
  assert.equal(f.bot.vehicle,undefined)
  assert.equal(driven,false)
  assert.equal(f.writes.length,0)
  await assert.rejects(f.runtime.long.ride({id:9,plan:true}),/requires/)
  await assert.rejects(f.runtime.long.ride({id:9,terrain:'jump'}),/terrain/)
})

test('unsafe preferred horse exit refuses before shift even with a safe other neighbour',async()=>{
  const f=dismountFixture()
  f.bot.blockAt=p=>({name:p.y<(p.x>=2?63:64)?'stone':'air',boundingBox:p.y<(p.x>=2?63:64)?'block':'empty',getProperties:()=>({})})
  await assert.rejects(f.runtime.long.horse_dismount({}),/fallback is unproved/)
  assert.equal(f.bot.vehicle,f.horse)
  assert.equal(f.writes.some(w=>w.packet.inputs?.shift),false)
})
test('unexpected authoritative horse landing returns actual recovery context and never starts walking',async()=>{
  const f=dismountFixture(),write=f.client.write
  f.client.write=(name,packet)=>{
    write(name,packet)
    if(name==='player_input'&&packet.inputs.shift){
      f.client.emit('set_passengers',{entityId:9,passengers:[]})
      f.bot.entity.position={x:1,y:63,z:0.2}
      f.bot.emit('forcedMove')
    }
  }
  await assert.rejects(f.runtime.long.horse_dismount({}),/server dismount landing differs/)
  assert.equal(f.bot.vehicle,null)
  const context=f.reports.find(p=>p.status==='server landing differed; movement stopped')
  assert.deepEqual(context.actual,{x:1,y:63,z:0.2})
  assert.equal(context.horse.id,9)
  assert.equal(context.mounted,false)
  assert.equal(f.bot.listenerCount('forcedMove'),0)
})
