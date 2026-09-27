import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { createRequire } from 'node:module'
import { readFileSync } from 'node:fs'
import { checkedHorseRoute, driveHorse, horseSpeed, observedHorseSpeed, HorseRouteError } from '../src/navigation/horse.mjs'
const require = createRequire(import.meta.url)
const { Vec3 } = require('vec3')
function fixture (choose = () => null, version = '26.1') {
  const registry = require('minecraft-data')(version)
  const Block = require('prismarine-block')(registry)
  const entity = { id: 2, name: 'horse', position: new Vec3(0.5, 1, 0.5), velocity: new Vec3(0, 0, 0), width: 1.4, height: 1.6,
    attributes: { [registry.attributesByName.movementSpeed.resource]: { value: 0.225, modifiers: [] } } }
  const packets = [], client = new EventEmitter()
  client.write = (name, data) => packets.push({ name, data })
  const bot = { version, registry, _client: client, entity: { id: 1, position: new Vec3(0, 1, 0) }, vehicle: entity, entities: { 2: entity }, supportFeature: () => true,
    blockAt: pos => {
      const p = pos.floored(), chosen = choose(p) ?? (p.y < 1 ? 'stone' : 'air')
      if (chosen === 'unloaded') return null
      const b = Block.fromProperties(chosen, {}, 0); b.position = p; return b
    } }
  return { bot, entity, packets, at: (x, y, z) => bot.blockAt(new Vec3(x, y, z)) }
}
test('horse corridors check the whole body and rider, ground continuity, hazards and loaded state', () => {
  const start = { x: 0.5, y: 1, z: 0.5 }, goal = { x: 12, y: 1, z: 0 }
  assert.equal(checkedHorseRoute(fixture().at, start, goal).distance, 12)
  for (const choose of [
    p => p.x === 5 && p.y === 3 ? 'stone' : null,
    p => p.x === 5 && p.y === 1 && p.z === 1 ? 'stone' : null,
    p => p.x === 5 && p.y === 0 && p.z === 1 ? 'air' : null,
    p => p.x === 5 && p.y === 0 ? 'magma_block' : null,
    p => p.x === 5 && p.y === 0 ? 'ice' : null,
    p => p.x === 5 ? 'unloaded' : null
  ]) assert.throws(() => checkedHorseRoute(fixture(choose).at, start, goal), HorseRouteError)
  assert.throws(() => checkedHorseRoute(fixture().at, start, { ...goal, y: 2 }), /flat/)
  assert.throws(() => checkedHorseRoute(fixture().at, start, { ...goal, x: 129 }), /128/)
})
test('diagonal clearance sweeps thin rider-height obstructions between sampled footprints', () => {
  const f = fixture()
  const at = (x, y, z) => x === 1 && y === 3 && z === -1
    ? { name: 'thin_overhang', shapes: [[0.275, 0, 0.965, 0.285, 1, 0.975]], solid: true }
    : f.at(x, y, z)
  assert.throws(() => checkedHorseRoute(at, { x: 0.5, y: 1, z: 0.5 }, { x: 1.5, y: 1, z: 1.5 }), /clear/)
})
test('native horse-sized physics drives straight and diagonal routes with real acceleration and settles without jumping', async () => {
  for (const version of ['1.21.5', '26.1']) for (const goal of [{ x: 24, y: 1, z: 0 }, { x: 16, y: 1, z: 16 }, { x: -18, y: 1, z: 7 }]) {
    const f = fixture(undefined, version)
    const result = await driveHorse({ ...f, goal, check: () => {}, pause: async () => {} })
    assert.equal(result.arrived, true)
    assert.equal(result.mounted, true)
    assert.equal(f.bot._client.listenerCount('vehicle_move'), 0)
    const positions = f.packets.filter(p => p.name === 'vehicle_move').map(p => p.data)
    assert.ok(positions.length > 20)
    assert.ok(positions.every(p => p.onGround && p.y === 1))
    let previous = new Vec3(0.5, 1, 0.5)
    for (const p of positions) {
      const next = new Vec3(p.x, p.y, p.z)
      assert.ok(previous.distanceTo(next) < 0.55, 'no teleport or speed jump')
      previous = next
    }
    assert.ok(f.entity.position.distanceTo(new Vec3(goal.x + 0.5, goal.y, goal.z + 0.5)) <= 0.8)
    assert.equal(f.bot.entity.position.x, f.entity.position.x)
    assert.equal(f.bot.entity.position.z, f.entity.position.z)
    assert.equal(f.bot.entity.position.y, f.entity.position.y + 1.44375 - 0.6)
    assert.deepEqual(f.packets.at(-1), { name: 'player_input', data: { inputs: {} } })
    assert.equal(f.packets.some(p => p.data.inputs?.jump || p.data.inputs?.shift), false)
  }
})
test('server vehicle corrections stop prediction and preserve authoritative position', async () => {
  const f = fixture(); let ticks = 0
  await assert.rejects(driveHorse({ ...f, goal: { x: 20, y: 1, z: 0 }, check: () => {}, pause: async () => {
    if (++ticks === 5) f.bot._client.emit('vehicle_move', { x: 1, y: 1, z: 0.5, yaw: 90, pitch: 0 })
  } }), /server corrected/)
  assert.deepEqual(f.entity.position, new Vec3(1, 1, 0.5))
  assert.equal(f.bot._client.listenerCount('vehicle_move'), 0)
  assert.deepEqual(f.packets.at(-1).data.inputs, {})
})
test('cancellation, changed terrain and passenger loss release input without unsafe movement or dismount', async () => {
  for (const failure of ['cancel', 'terrain', 'rider', 'entity']) {
    let changed = false, ticks = 0
    const f = fixture(p => changed && failure === 'terrain' && p.x >= 2 && p.y === 1 ? 'stone' : null)
    await assert.rejects(driveHorse({ ...f, goal: { x: 20, y: 1, z: 0 }, check: () => { if (changed && failure === 'cancel') throw new Error('cancelled') }, pause: async () => {
      if (++ticks === 2) {
        changed = true
        if (failure === 'rider') f.bot.vehicle = null
        if (failure === 'entity') f.bot.entities[3] = { id: 3, name: 'cow', position: new Vec3(2, 1, 0.5), width: 0.9, height: 1.4 }
      }
    } }))
    assert.equal(f.bot._client.listenerCount('vehicle_move'), 0)
    assert.deepEqual(f.packets.at(-1).data.inputs, {})
    assert.equal(f.packets.some(p => p.data.inputs?.shift), false)
  }
})
test('cancelling a fast ride coasts under neutral native physics until subsequent dismount is possible', async () => {
  const f = fixture(); let ticks = 0, cancelled = false, stoppedAt
  await assert.rejects(driveHorse({ ...f, goal: { x: 100, y: 1, z: 0 }, check: () => { if (cancelled) throw new Error('cancelled') }, pause: async () => {
    if (++ticks === 20) { cancelled = true; stoppedAt = f.entity.position.x }
  } }), /cancelled/)
  assert.ok(f.entity.position.x > stoppedAt, 'momentum decelerates instead of disappearing')
  assert.ok(f.entity.position.x < stoppedAt + 1.5)
  assert.ok(Math.hypot(f.entity.velocity.x, f.entity.velocity.z) <= 0.003)
  assert.ok(f.entity.velocity.norm() <= 0.08, 'native resting gravity fits the dismount stationary check')
  assert.ok(ticks <= 35)
  const afterCancel = f.packets.filter(p => p.name === 'player_input').slice(20)
  assert.ok(afterCancel.every(p => !p.data.inputs.forward))
})
test('horse speed requires the server attribute rather than assuming a fast horse', () => {
  const f = fixture()
  assert.equal(horseSpeed(f.bot, f.entity), 0.225)
  f.entity.attributes = {}
  assert.throws(() => horseSpeed(f.bot, f.entity), /confirmed/)
})
test('rearing and leashed horses refuse goal travel before motion', async () => {
  for (const rearing of [true, false]) {
    const f = fixture()
    if (rearing) f.entity.metadata = { [f.bot.registry.entitiesByName.horse.metadataKeys.indexOf('flags')]: 38 }
    else f.entity.leashHolder = { id: 99 }
    await assert.rejects(driveHorse({ ...f, goal: { x: 10, y: 1, z: 0 }, check: () => {}, pause: async () => {} }), rearing ? /rearing/ : /lead/)
    assert.equal(f.packets.length, 0)
  }
})
test('all horse planning phases report duration through the advisory shared reporter', async () => {
  const f = fixture(), timings = []
  const reportPerformance = (operation, elapsed, details) => timings.push({ operation, elapsed, ...details })
  const args = { ...f, goal: { x: 8, y: 1, z: 0 }, check: () => {}, pause: async () => {}, reportPerformance }
  driveHorse.validate(args)
  assert.equal((await driveHorse(args)).arrived, true)
  assert.ok(timings.every(t => t.operation === 'horse.route' && t.elapsed >= 0 && t.id === 2))
  assert.deepEqual([...new Set(timings.map(t => t.phase))], ['preflight', 'departure', 'clearance'])
  const blocked = fixture(p => p.x === 2 && p.y === 1 ? 'stone' : null)
  assert.throws(() => driveHorse.validate({ ...args, ...blocked }), HorseRouteError)
  assert.equal(timings.at(-1).phase, 'preflight', 'failed scans are timed too')
})

test('actual decoded horse attribute packets feed both state inspection and native mounted physics without fallback speed', async () => {
  const protocol = require('minecraft-protocol')
  const source = readFileSync(require.resolve('mineflayer/lib/plugins/entities'), 'utf8')
  const start = source.indexOf('  const updateAttributes = (packet) => {')
  const end = source.indexOf("  bot._client.on('update_attributes'", start)
  assert.ok(start >= 0 && end > start)
  for (const version of ['1.21.5', '26.1']) {
    const wire = fixture(undefined, version), canonical = fixture(undefined, version)
    const attribute = { value: 0.225, modifiers: [{ uuid: 'test:horse_speed', amount: 0.1, operation: 2 }] }
    wire.entity.attributes = {}
    const wireKey = version === '26.1' ? 'minecraft:movement_speed' : 'generic.movement_speed'
    const packet = { name: 'entity_update_attributes', params: { entityId: 2, properties: [{ key: wireKey, ...attribute }] } }
    const serializer = protocol.createSerializer({ state: 'play', isServer: true, version })
    const deserializer = protocol.createDeserializer({ state: 'play', version })
    const decoded = deserializer.parsePacketBuffer(serializer.createPacketBuffer(packet)).data.params
    const apply = new Function('fetchEntity', 'bot', `${source.slice(start, end)}; return updateAttributes`)(() => wire.entity, { emit: () => {} })
    apply(decoded)
    assert.deepEqual(Object.keys(wire.entity.attributes), [wireKey])
    assert.equal(observedHorseSpeed(wire.bot, wire.entity), 0.2475)
    canonical.entity.attributes = { [canonical.bot.registry.attributesByName.movementSpeed.resource]: structuredClone(attribute) }
    const before = structuredClone(wire.entity.attributes)
    const args = { goal: { x: 24, y: 1, z: 0 }, check: () => {}, pause: async () => {} }
    const actual = await driveHorse({ ...wire, ...args }), expected = await driveHorse({ ...canonical, ...args })
    assert.equal(actual.ticks, expected.ticks, version)
    assert.deepEqual(actual.at, expected.at, 'native simulation consumes the actual horse attribute, not default player speed')
    assert.deepEqual(wire.entity.attributes, before, 'native physics cannot mutate server attribute records')
  }
})
