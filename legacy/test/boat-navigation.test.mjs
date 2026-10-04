import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { EventEmitter } from 'node:events'
import { readFileSync } from 'node:fs'
import { checkedBoatRoute, boatSurface, stepBoatWater, planBoatLeg, driveBoat, BoatRouteError } from '../src/navigation/boat-travel.mjs'
import { checkedBoatLanding } from '../src/navigation/boat-landing.mjs'
const require = createRequire(import.meta.url)
const { Vec3 } = require('vec3')
const registry = require('minecraft-data')('26.1')
const Block = require('prismarine-block')(registry)
const water = Block.fromProperties('water', { level: 0 }, 0), air = Block.fromProperties('air', {}, 0), stone = Block.fromProperties('stone', {}, 0)
const pond = (x, y, z) => y === 63 ? water : y < 63 ? stone : air
const surface = boatSurface(pond, 0, 0, 63)
const boat = (yaw = Math.PI) => ({ id: 2, name: 'oak_boat', width: 1.375, height: 0.5625, yaw, position: new Vec3(surface.x, surface.y, surface.z), velocity: new Vec3(0, 0, 0), passengers: [{ id: 1 }] })

test('boat surface and swept hull reject land, current, unloaded, low ceiling and submerged water', () => {
  assert.ok(surface && surface.y > 63.5 && surface.y < 63.6)
  assert.equal(checkedBoatRoute(pond, surface, { ...surface, z: 10 }).distance, 9.5)
  for (const blocked of [null, stone, Block.fromProperties('water', { level: 1 }, 0), Block.fromProperties('bubble_column', {}, 0)]) {
    assert.throws(() => checkedBoatRoute((x, y, z) => x === 1 && y === 63 && z === 3 ? blocked : pond(x, y, z), surface, { ...surface, z: 10 }), BoatRouteError)
  }
  assert.throws(() => checkedBoatRoute((x, y, z) => y === 65 && z === 3 ? stone : pond(x, y, z), surface, { ...surface, z: 10 }), BoatRouteError)
  assert.throws(() => checkedBoatRoute((x, y, z) => y === 64 && z === 3 ? water : pond(x, y, z), surface, { ...surface, z: 10 }), BoatRouteError)
  assert.equal(boatSurface(() => null, 0, 0, 63), null)
})

test('source-water physics reproduces vanilla acceleration, drag, buoyancy and turn momentum', () => {
  const start = { ...surface, vx: 0, vy: 0, vz: 0, yaw: 0, rotation: 0 }
  const first = stepBoatWater(start, { forward: true }, 63 + Math.fround(8 / 9))
  assert.ok(Math.abs(first.vz - Math.fround(0.04)) < 1e-10)
  assert.ok(Math.abs(first.y - start.y) < 1e-10, 'equilibrium immersion')
  const second = stepBoatWater(first, {}, 63 + Math.fround(8 / 9))
  assert.ok(Math.abs(second.vz - first.vz * Math.fround(0.9)) < 1e-10)
  const turn = stepBoatWater(start, { left: true }, 63 + Math.fround(8 / 9))
  assert.equal(turn.yaw, -1); assert.equal(turn.rotation, -1)
  assert.ok(Math.abs(Math.hypot(turn.vx, turn.vz) - Math.fround(0.005)) < 1e-10)
})

test('checked controller steers and settles from all headings without teleporting onto the goal', () => {
  for (const yaw of [0, Math.PI / 2, Math.PI, -Math.PI / 2]) {
    const entity = boat(yaw), goal = { ...surface, z: 12.5 }
    const plan = planBoatLeg(pond, entity, goal), end = plan.steps.at(-1).state
    assert.ok(Math.hypot(end.x - goal.x, end.z - goal.z) < 0.85)
    assert.ok(Math.hypot(end.vx, end.vz) < 0.003)
    for (let i = 1; i < plan.steps.length; i++) assert.ok(Math.hypot(plan.steps[i].state.x - plan.steps[i - 1].state.x, plan.steps[i].state.z - plan.steps[i - 1].state.z) <= 0.401)
  }
})

function fixture () {
  const entity = boat(), packets = [], metrics = [], reports = [], client = new EventEmitter()
  client.write = (name, data) => packets.push({ name, data })
  const bot = { _client: client, vehicle: entity, entities: { 2: entity }, health: 20, entity: { id: 1, position: new Vec3(0.5, 64, 0.5), yaw: 0 }, blockAt: p => pond(p.x, p.y, p.z), supportFeature: () => true }
  return { bot, entity, packets, metrics, reports, report: details => reports.push(details), goal: { ...surface, z: 8.5 }, check: () => {}, pause: async () => {}, reportPerformance: (...args) => metrics.push(args) }
}
test('drive sends physical positions and paddle input, updates mounted senses and settles before returning', async () => {
  const f = fixture(), result = await driveBoat(f)
  assert.equal(result.arrived, true); assert.equal(result.mounted, true)
  assert.ok(f.packets.some(p => p.name === 'steer_boat' && p.data.leftPaddle && p.data.rightPaddle))
  assert.ok(f.packets.some(p => p.name === 'vehicle_move' && p.data.z > 7))
  assert.equal(f.bot.entity.position.z, f.entity.position.z)
  assert.ok(f.entity.velocity.norm() < 0.01)
  assert.equal(f.bot._client.listenerCount('vehicle_move'), 0)
  assert.ok(f.metrics.length > 1)
})
test('cancellation neutral-coasts, keeps the seat, and settles the nested movement before rejecting', async () => {
  const f = fixture(); let ticks = 0
  f.check = () => { if (ticks >= 20) throw new Error('cancelled') }
  f.pause = async () => { ticks++ }
  await assert.rejects(driveBoat(f), /cancelled/)
  assert.ok(ticks > 20)
  assert.ok(f.entity.velocity.norm() < 0.01)
  assert.equal(f.bot.vehicle, f.entity)
  assert.deepEqual(f.packets.at(-1), { name: 'steer_boat', data: { leftPaddle: false, rightPaddle: false } })
})
test('server corrections stop prediction and keep the authoritative boat pose', async () => {
  const f = fixture(); let ticks = 0
  f.pause = async () => { if (++ticks === 5) f.bot._client.emit('vehicle_move', { x: 5, y: surface.y, z: 6, yaw: 45, pitch: 0 }) }
  await assert.rejects(driveBoat(f), /corrected/)
  assert.equal(f.entity.position.x, 5); assert.equal(f.entity.position.z, 6)
  assert.equal(f.bot._client.listenerCount('vehicle_move'), 0)
  const stopped = f.reports.find(r => r.status === 'movement interrupted')
  assert.equal(stopped.source, 'vehicle_move'); assert.equal(stopped.tick, 5)
  assert.deepEqual(stopped.actual, { x: 5, y: surface.y, z: 6 })
})

test('native tracked entity updates are diagnosed separately and still stop prediction', async () => {
  const f = fixture(); let ticks = 0
  // Execute the installed Mineflayer handler, whose direct position write is
  // distinct from the vehicle_move correction/acknowledgement protocol.
  const source = readFileSync(require.resolve('mineflayer/lib/plugins/entities'), 'utf8')
  const handler = source.match(/bot\._client\.on\('sync_entity_position', \(packet\) => \{([\s\S]*?)\n  \}\)/)?.[1]
  assert.ok(handler)
  const native = new Function('bot', 'fetchEntity', 'conv', 'packet', handler)
  f.bot.emit = () => {}
  f.bot._client.on('sync_entity_position', packet => native(f.bot, () => f.entity, { fromNotchianYaw: y => (180 - y) * Math.PI / 180, fromNotchianPitch: p => -p * Math.PI / 180 }, packet))
  f.pause = async () => {
    if (++ticks === 5) f.bot._client.emit('sync_entity_position', { entityId: f.entity.id, x: f.entity.position.x + 0.15, y: f.entity.position.y, z: f.entity.position.z, dx: 0, dy: 0, dz: 0, yaw: 0, pitch: 0, onGround: false })
  }
  await assert.rejects(driveBoat(f), /entity_position_changed/)
  const stopped = f.reports.find(r => r.status === 'movement interrupted')
  assert.equal(stopped.source, 'entity_position_changed'); assert.equal(stopped.tick, 5)
  assert.ok(Math.abs(stopped.positionDelta - 0.15) < 1e-6)
  assert.equal(stopped.recentEntityPackets.at(-1).name, 'sync_entity_position')
  assert.equal(stopped.mounted, f.entity.id)
  assert.equal(f.bot._client.listenerCount('sync_entity_position'), 1, 'only the original native handler remains')
  assert.equal(f.bot._client.listenerCount('entity_teleport'), 0)
  assert.equal(f.packets.filter(p => p.name === 'vehicle_move').length, 5, 'no acknowledgement or new prediction follows an ordinary tracked update')
})

test('route changes and other entities stop thrust without walking or dismounting', async () => {
  const f = fixture(); let ticks = 0
  f.pause = async () => {
    if (++ticks === 8) f.bot.entities[9] = { id: 9, name: 'cow', position: f.entity.position.offset(0, 0, 0.8), width: 0.9, height: 1.4 }
  }
  await assert.rejects(driveBoat(f), /another entity/)
  assert.equal(f.bot.vehicle, f.entity)
  assert.deepEqual(f.packets.at(-1).data, { leftPaddle: false, rightPaddle: false })
  assert.equal(f.bot._client.listenerCount('vehicle_move'), 0)
})

test('actual protocol encodes vehicle positions, paddles and inputs for supported client versions', async () => {
  const { createSerializer } = require('minecraft-protocol')
  const f = fixture(); await driveBoat(f)
  for (const version of ['1.21.5', '26.1']) {
    const serializer = createSerializer({ state: 'play', isServer: false, version })
    for (const packet of f.packets) assert.ok(serializer.createPacketBuffer({ name: packet.name, params: packet.data }).length > 0, `${version} ${packet.name}`)
  }
})

test('physical fine paddle approach stops close enough for the checked server shore landing', () => {
  const shore = (x, y, z) => z >= 10 && y <= 63 ? stone : pond(x, y, z)
  const reach = (1.375 * Math.SQRT2 + 0.6 + 0.00001) / 2
  const goal = { x: 0.5, y: surface.y, z: 10.5 - reach }
  for (const distance of [0.15, 0.35, 0.8, 3, 8]) {
    const entity = boat(); entity.position.z = goal.z - distance
    const plan = planBoatLeg(shore, entity, goal), end = plan.steps.at(-1).state
    assert.ok(Math.hypot(end.x - goal.x, end.z - goal.z) < 0.12)
    const exit = checkedBoatLanding(shore, { ...entity, position: end, yaw: (180 - end.yaw) * Math.PI / 180 }, { x: 0, y: 64, z: 10 })
    assert.equal(exit.y, 64)
    assert.ok(exit.z >= 10.32, 'entire passenger footprint reaches dry bank')
  }
})

test('short boat legs converge after turns instead of braking on sideways momentum or orbiting', () => {
  for (let degrees = 0; degrees < 360; degrees += 30) for (const distance of [0.35, 2, 8]) {
    const entity = boat(degrees * Math.PI / 180), goal = { ...surface, z: surface.z + distance }
    const plan = planBoatLeg(pond, entity, goal), end = plan.steps.at(-1).state
    assert.ok(Math.hypot(end.x - goal.x, end.z - goal.z) <= 0.12, `heading=${degrees} distance=${distance}`)
    assert.ok(Math.hypot(end.vx, end.vz) < 0.003)
    assert.ok(plan.steps.length < 500)
  }
})

test('Fern live pose turns back to the checked shore using real backward packet input', async () => {
  const y = 62 + Math.fround(8 / 9) - 0.5625 * 0.65
  const waterAt = (x, by, z) => by < 62 || by === 62 && z <= -89 ? stone : by === 62 ? water : air
  // Both the previously rejected two-metre departure and the return after the
  // successful server-verified forward pilot need a heading change.
  const departure = { ...boat(3.60498), position: new Vec3(-9.5496, y, -86.5979) }
  assert.ok(planBoatLeg(waterAt, departure, { x: -9.55, y, z: -84.6 }).steps.length < 150)
  const f = fixture()
  f.entity.position.set(-8.6264164432, y, -84.8497502611)
  f.entity.yaw = 3.6021952524
  f.bot.blockAt = p => waterAt(p.x, p.y, p.z)
  f.goal = { x: -9.5, y, z: -87.2277 }
  const result = await driveBoat(f)
  assert.equal(result.arrived, true)
  assert.ok(f.packets.some(p => p.name === 'player_input' && p.data.inputs.backward === true))
  assert.ok(f.packets.every(p => p.name !== 'player_input' || !Object.hasOwn(p.data.inputs, 'back')), 'wire flag is backward, not the internal control name back')
  const exit = checkedBoatLanding(waterAt, f.entity, { x: -10, y: 63, z: -89 })
  assert.equal(exit.y, 63)
  assert.ok(exit.z + 0.32 <= -88, 'entire dismount footprint reaches the actual bank')
})
