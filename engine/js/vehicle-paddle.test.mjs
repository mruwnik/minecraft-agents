// Why JavaScript: tests the Mineflayer adapter code for steering a boat (vehicle.mjs), which stays JS (Mineflayer boundary).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import { boatStep, paddleBoat, BOAT_TURN, BOAT_SPEED } from './vehicle.mjs'

const BODY = 4
const opts = { timeScale: 0.02 }
const ctx = () => {
  const aborts = []
  return { aborts, alive: () => {}, onAbort: fn => aborts.push(fn) }
}

// a boat at (0.5, 63, 0.5) facing south (yaw 0 degrees); water at every cell of y 63 whose z is below `wet`
const rig = ({ wet = 100, mounted = true, name = 'oak_boat' } = {}) => {
  const written = []
  const boat = { id: 9, name, position: vec3(0.5, 63, 0.5), yaw: Math.PI, height: 0.56 }
  const bot = new EventEmitter()
  Object.assign(bot, {
    written,
    entity: { id: BODY, position: vec3(0.5, 63, 0.5), height: 1.62, yaw: 0, pitch: 0 },
    entities: { 9: boat },
    vehicle: mounted ? boat : null,
    _client: Object.assign(new EventEmitter(), { write: (n, d) => written.push([n, d]) }),
    blockAt: v => ({ name: v.y === 63 && v.z < wet ? 'water' : 'air' })
  })
  return { bot, boat }
}

test('boatStep: yaw turns by BOAT_TURN, forward moves along the yaw (0 south +z, 90 west -x)', async t => {
  const rows = [
    ['forward south', { x: 0, y: 63, z: 0, yaw: 0 }, { forward: true }, { x: 0, z: BOAT_SPEED, yaw: 0 }],
    ['forward west', { x: 0, y: 63, z: 0, yaw: 90 }, { forward: true }, { x: -BOAT_SPEED, z: 0, yaw: 90 }],
    ['right raises the yaw', { x: 0, y: 63, z: 0, yaw: 350 }, { turn: 'right' }, { x: 0, z: 0, yaw: (350 + BOAT_TURN) % 360 }],
    ['left lowers the yaw, wrapped', { x: 0, y: 63, z: 0, yaw: 2 }, { turn: 'left' }, { x: 0, z: 0, yaw: 360 + 2 - BOAT_TURN }],
    ['idle stays', { x: 1, y: 63, z: 2, yaw: 10 }, {}, { x: 1, z: 2, yaw: 10 }]
  ]
  for (const [label, from, input, want] of rows) {
    await t.test(label, () => {
      const got = boatStep(from, input)
      assert.ok(Math.abs(got.x - want.x) < 1e-9 && Math.abs(got.z - want.z) < 1e-9, label)
      assert.ok(Math.abs(got.yaw - want.yaw) < 1e-9, label)
      assert.equal(got.y, from.y)
    })
  }
})

test('paddleBoat', async t => {
  await t.test('on foot', async () => {
    const { bot } = rig({ mounted: false })
    assert.deepEqual(await paddleBoat(bot, ctx(), { ticks: 3, forward: true }, opts), { status: 'not-mounted' })
  })
  await t.test('a vehicle that is not a boat', async () => {
    const { bot } = rig({ name: 'minecart' })
    assert.deepEqual(await paddleBoat(bot, ctx(), { ticks: 3, forward: true }, opts), { status: 'not-a-boat' })
  })
  await t.test('zero ticks reads the pose and sends nothing', async () => {
    const { bot } = rig()
    const r = await paddleBoat(bot, ctx(), { ticks: 0 }, opts)
    assert.deepEqual(r, { status: 'ok', pos: { x: 0.5, y: 63, z: 0.5 }, yaw: 0, ticks: 0, turnDeg: BOAT_TURN, speed: BOAT_SPEED })
    assert.deepEqual(bot.written, [])
  })
  await t.test('forward strokes move the boat and the rider, and send the vehicle position each tick', async () => {
    const { bot, boat } = rig()
    const r = await paddleBoat(bot, ctx(), { ticks: 5, forward: true }, opts)
    assert.equal(r.status, 'ok')
    assert.equal(r.ticks, 5)
    assert.ok(Math.abs(r.pos.z - (0.5 + 5 * BOAT_SPEED)) < 1e-9)
    assert.equal(boat.position.z, r.pos.z)
    assert.equal(bot.written.filter(([n]) => n === 'vehicle_move').length, 5)
    assert.equal(bot.written.filter(([n]) => n === 'player_input').length, 6, 'an input per tick and a release')
  })
  await t.test('a turn changes the yaw, as the boat sees it', async () => {
    const { bot, boat } = rig()
    const r = await paddleBoat(bot, ctx(), { ticks: 3, turn: 'right' }, opts)
    assert.equal(r.yaw, 3 * BOAT_TURN)
    assert.ok(Math.abs(boat.yaw - (Math.PI - r.yaw * Math.PI / 180)) < 1e-9)
  })
  await t.test('ends blocked at the last water cell, boat stays in the water', async () => {
    const { bot } = rig({ wet: 2 })
    const r = await paddleBoat(bot, ctx(), { ticks: 40, forward: true }, opts)
    assert.equal(r.status, 'blocked')
    assert.ok(r.pos.z < 2)
    assert.ok(r.ticks > 0 && r.ticks < 40)
  })
  await t.test('ticks are clamped', async () => {
    const { bot } = rig()
    const r = await paddleBoat(bot, ctx(), { ticks: 1000, turn: 'left' }, opts)
    assert.equal(r.ticks, 40)
  })
  await t.test('every input is released at the end, and by a cut', async () => {
    const { bot } = rig()
    const c = ctx()
    await paddleBoat(bot, c, { ticks: 2, forward: true }, opts)
    assert.deepEqual(bot.written.at(-1), ['player_input', { inputs: {} }])
    assert.equal(c.aborts.length, 1)
    c.aborts[0]()
    assert.deepEqual(bot.written.at(-1), ['player_input', { inputs: {} }])
  })
  await t.test('a cut in the middle of a stroke releases every input', async () => {
    const { bot } = rig()
    let alive = 0
    const c = { aborts: [], onAbort: fn => c.aborts.push(fn), alive: () => { if (++alive === 3) throw new Error('cut') } }
    await assert.rejects(paddleBoat(bot, c, { ticks: 10, forward: true }, opts), /cut/)
    assert.deepEqual(bot.written.at(-1), ['player_input', { inputs: {} }])
    assert.equal(bot.written.filter(([n]) => n === 'vehicle_move').length, 3)
  })
  await t.test('every result carries the model constants, so a job need not copy them', async () => {
    const { bot } = rig()
    for (const ticks of [0, 2]) {
      const r = await paddleBoat(bot, ctx(), { ticks, forward: true }, opts)
      assert.equal(r.turnDeg, BOAT_TURN)
      assert.equal(r.speed, BOAT_SPEED)
    }
  })
})
