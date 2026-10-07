// Why JavaScript: tests the tick-rate physics shim (tick-rate-physics.mjs), a Mineflayer boundary.
import { test, mock } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import mineflayer from 'mineflayer'
import vec3 from 'vec3'
import { tickRatePhysics } from './tick-rate-physics.mjs'

const { Vec3 } = vec3
const air = { name: 'air', type: 0, boundingBox: 'empty', shapes: [], stateId: 0, getProperties: () => ({}) }

// A real mineflayer bot on a fake packet stream with the shim as its physics plugin and a hand-driven clock:
// `advance(ms)` moves the clock and the mocked setInterval together.
async function fakeBot (t, shimOptions = {}) {
  mock.timers.enable({ apis: ['setInterval'] })
  t.after(() => { mock.timers.reset() })
  const client = new EventEmitter()
  const written = []
  Object.assign(client, { state: 'play', version: '26.1', write: (name, packet) => written.push(name), end () {}, registerChannel () {}, writeChannel () {} })
  let clock = 0
  const bot = mineflayer.createBot({ client, version: '26.1', username: 'x', plugins: { physics: tickRatePhysics({ now: () => clock, ...shimOptions }) } })
  await new Promise(resolve => bot.once('inject_allowed', resolve)) // the plugins load on it
  bot.entity = { position: new Vec3(0.5, 64, 0.5), velocity: new Vec3(0, 0, 0), yaw: 0, pitch: 0, onGround: true, effects: {}, height: 1.8, isInWater: false }
  bot.blockAt = () => air
  bot.game = { gameMode: 'survival' }
  bot.emit('login')
  client.emit('position', { x: 0.5, y: 64, z: 0.5, yaw: 0, pitch: 0, flags: {}, teleportId: 1 }) // physics starts after the first position
  const ticks = { n: 0 }
  bot.on('physicsTick', () => ticks.n++)
  const advance = ms => { for (let t = 0; t < ms; t += 5) { clock += 5; mock.timers.tick(5) } }
  t.after(() => bot.emit('end'))
  return { bot, client, ticks, advance }
}

test('the shim installs a physics plugin that follows the vanilla rate by default', async t => {
  const { bot, ticks, advance } = await fakeBot(t)
  assert.equal(typeof bot.setControlState, 'function')
  assert.equal(bot.physicsClock.rate(), 20)
  advance(1000)
  assert.equal(ticks.n, 20, 'at 20 TPS a second is 20 ticks, as stock')
})

test('a set_ticking_state packet changes the physics interval', async t => {
  const { bot, client, ticks, advance } = await fakeBot(t)
  client.emit('set_ticking_state', { tick_rate: 40, is_frozen: false })
  assert.equal(bot.physicsClock.rate(), 40)
  advance(1000)
  assert.equal(ticks.n, 40)
  client.emit('set_ticking_state', { tick_rate: 20, is_frozen: false })
  ticks.n = 0
  advance(1000)
  assert.equal(ticks.n, 20)
})

test('frozen runs no physics ticks and step_tick runs n', async t => {
  const { client, ticks, advance } = await fakeBot(t)
  client.emit('set_ticking_state', { tick_rate: 20, is_frozen: true })
  advance(1000)
  assert.equal(ticks.n, 0)
  client.emit('step_tick', { tick_steps: 3 })
  assert.equal(ticks.n, 3)
  client.emit('set_ticking_state', { tick_rate: 20, is_frozen: false })
  advance(500)
  assert.equal(ticks.n, 13, 'thawing resumes without a catch-up burst')
})

test('dig time is the vanilla ticks at the current rate', async t => {
  const { bot, client } = await fakeBot(t)
  assert.equal(bot.physicsClock.digMs(1000), 1000)
  client.emit('set_ticking_state', { tick_rate: 40, is_frozen: false })
  assert.equal(bot.physicsClock.digMs(1000), 500)
})

test('bot.digTime is wrapped by the shim: stock digging loaded first, its time follows the rate', async t => {
  const { bot, client } = await fakeBot(t)
  const block = { diggable: true, digTime: () => 1000, material: 'rock', name: 'stone' }
  const stock = bot.digTime.length >= 0 && bot.digTime(block)
  assert.equal(Number.isFinite(stock), true)
  client.emit('set_ticking_state', { tick_rate: 40, is_frozen: false })
  assert.equal(bot.digTime(block), stock / 2)
})

test('the shim replaces stock physics: one physics plugin, the shim one, runs', async t => {
  const { bot, ticks, advance } = await fakeBot(t)
  assert.equal(typeof bot.physicsClock?.digMs, 'function')
  advance(1000)
  assert.equal(ticks.n, 20, 'a stock plugin loaded as well would tick twice')
})

test('a patch target that no longer matches falls back to stock physics and reports it once', async t => {
  const reasons = []
  const { bot } = await fakeBot(t, { source: 'module.exports = () => {}', onStock: reason => reasons.push(reason) })
  assert.equal(typeof bot.setControlState, 'function', 'the stock physics plugin is loaded')
  assert.equal(bot.physicsClock, undefined)
  assert.equal(reasons.length, 1)
  assert.match(reasons[0], /patch target not found/)
})
