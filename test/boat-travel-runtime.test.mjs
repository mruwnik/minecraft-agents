import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { makeBoatTravelRuntime } from '../src/body/boat-travel.mjs'

function fixture ({ passengers = [], mounted = false, failValidation = false, cancel = false, night = false, leash = null } = {}) {
  const entity = { id: 4, name: 'oak_boat', isValid: true, position: { x: 1, y: 64, z: 0 }, passengers }
  const bot = Object.assign(new EventEmitter(), { entity: { id: 1, position: { x: 0, y: 64, z: 0 } }, entities: { 4: entity }, health: 20, food: 20, oxygenLevel: 20, time: { timeOfDay: night ? 14000 : 1000 }, supportFeature: () => true })
  if (mounted) { entity.passengers = [bot.entity]; bot.vehicle = entity }
  const writes = [], calls = []
  bot._client = new EventEmitter()
  bot._client.write = (name, value) => writes.push({ name, value })
  bot.mount = boat => { calls.push('mount'); boat.passengers = [bot.entity]; bot.vehicle = boat }
  bot.pathfinder = { setGoal: () => calls.push('clear goal') }
  const driveBoat = async ({ check }) => { check(); calls.push('drive'); return { arrived: true } }
  driveBoat.validate = () => { calls.push('validate'); if (failValidation) throw new Error('obstructed corridor') }
  const runtime = makeBoatTravelRuntime({ getBot: () => bot, cancelGuard: () => () => { if (cancel) throw new Error('cancelled') }, getLeashHolder: () => leash, driveBoat,
    readBoatState: () => ({ selfId: 1, mounted: bot.vehicle?.id ?? null, boats: [{ id: 4, exact: '1,64,0', passengers: entity.passengers }] }), pause: async () => {} })
  return { bot, entity, runtime, calls, writes }
}
const goal = { id: 4, x: 10, y: 64, z: 0 }
test('boat route is validated before boarding and completed legs stay mounted', async () => {
  const f = fixture()
  const result = await f.runtime.long.boat_drive(goal)
  assert.deepEqual(f.calls, ['validate', 'clear goal', 'mount', 'drive'])
  assert.equal(result.mounted, true)
  assert.equal(f.bot._client.listenerCount('set_passengers'), 0)
  assert.deepEqual(f.writes.at(-1), { name: 'steer_boat', value: { leftPaddle: false, rightPaddle: false } })
})
test('subsequent boat legs retain the controlling seat', async () => {
  const f = fixture({ mounted: true })
  await f.runtime.long.boat_drive(goal)
  assert.deepEqual(f.calls, ['validate', 'drive'])
})
test('bad routes never board or move', async () => {
  const f = fixture({ failValidation: true })
  await assert.rejects(f.runtime.long.boat_drive(goal), /obstructed/)
  assert.deepEqual(f.calls, ['validate'])
  assert.equal(f.bot.vehicle, undefined)
})
test('occupied and leashed boats are refused before any action', async () => {
  for (const options of [{ passengers: [{ id: 2 }] }, { leash: 2 }]) {
    const f = fixture(options)
    await assert.rejects(f.runtime.long.boat_drive(goal), /passenger|lead/)
    assert.deepEqual(f.calls, [])
  }
})
test('cancellation and night preserve a mounted traveler and release input', async () => {
  for (const options of [{ cancel: true }, { night: true }]) {
    const f = fixture({ ...options, mounted: true })
    await assert.rejects(f.runtime.long.boat_drive(goal), /cancelled|night/)
    assert.equal(f.bot.vehicle.id, 4)
    assert.equal(f.bot._client.listenerCount('set_passengers'), 0)
    assert.equal(f.writes.at(-1).name, 'steer_boat')
  }
})
test('state exposes numeric observed boat position and controller availability', () => {
  const f = fixture()
  const state = f.runtime.quick.boat_state({})
  assert.equal(state.goalTravel, true)
  assert.equal(state.boats[0].name, 'oak_boat')
  assert.deepEqual(state.boats[0].position, { x: 1, y: 64, z: 0 })
  assert.equal(state.boats[0].exact, '1,64,0')
})

const air = { name: 'air', shapes: [] }
const full = name => ({ name, shapes: [[0, 0, 0, 1, 1, 1]] })
const shore = (x, y, z) => y === 63 ? (x >= 1 ? full('stone') : { name: 'water', shapes: [] }) : air
function landFixture (confirm = true) {
  const f = fixture({ mounted: true, night: true })
  Object.assign(f.entity, { position: { x: 0.15, y: 63.5, z: 0.5 }, yaw: -Math.PI / 2, width: 1.375, height: 0.5625, velocity: { x: 0, y: 0, z: 0 } })
  f.bot.blockAt = p => shore(p.x, p.y, p.z)
  f.bot.look = async yaw => { f.bot.entity.yaw = yaw }
  const write = f.bot._client.write
  f.bot._client.write = (name, value) => {
    write(name, value)
    if (confirm && value.inputs?.shift) {
      f.bot._client.emit('set_passengers', { entityId: 4, passengers: [] })
      f.bot.entity.position = { x: 0.15 + (1.375 * Math.SQRT2 + 0.6 + 0.00001) / 2, y: 64, z: 0.5 }
      f.bot.entity.onGround = true
      f.bot.emit('forcedMove')
    }
  }
  return f
}
test('boat landing permits night and waits for actual server landing and passenger removal', async () => {
  const f = landFixture()
  const result = await f.runtime.long.boat_land({ id: 4, x: 1, y: 64, z: 0 })
  assert.equal(result.serverConfirmed, true)
  assert.equal(f.bot.vehicle, null)
  assert.equal(f.bot.listenerCount('forcedMove'), 0)
  assert.equal(f.bot._client.listenerCount('set_passengers'), 0)
})
test('unconfirmed landing never reports success or leaves input/listeners behind', async () => {
  const f = landFixture(false)
  await assert.rejects(f.runtime.long.boat_land({ id: 4, x: 1, y: 64, z: 0 }), /not confirmed/)
  assert.equal(f.bot.vehicle.id, 4)
  assert.equal(f.bot.listenerCount('forcedMove'), 0)
  assert.deepEqual(f.writes.at(-1).value, { inputs: {} })
})
