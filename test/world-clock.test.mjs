import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { createRequire } from 'node:module'
import { installWorldClock } from '../src/world-clock.mjs'
const require = createRequire(import.meta.url)
const nativeTime = require('mineflayer/lib/plugins/time')
const nbt = require('prismarine-nbt')
function fixture () {
  let ms = 0
  const bot = Object.assign(new EventEmitter(), { _client: new EventEmitter(), game: { dimension: 'overworld' }, registry: { dimensionsById: { 0: { name: 'overworld' }, 1: { name: 'the_end' } } } })
  nativeTime(bot)
  let publish, cleared = false
  installWorldClock(bot, { now: () => ms, setInterval: callback => { publish = callback; return 1 }, clearInterval: () => { cleared = true } })
  const update = (ticks, rate = 1, partialTick = 0, age = 1000) => bot._client.emit('update_time', { age: [0, age], clockUpdates: [{ id: 0, totalTicks: ticks, rate, partialTick }] })
  return { bot, update, elapse: amount => { ms += amount }, publish: () => publish(), cleared: () => cleared }
}
test('mounted clocks advance without physicsTick and refresh every published time field', () => {
  const f = fixture(); f.bot.vehicle = { id: 2 }; f.update(11478)
  f.elapse((18847 - 11478) * 50)
  assert.equal(f.bot.time.timeOfDay, 18847)
  assert.equal(f.bot.time.isDay, false)
  assert.equal(f.bot.time.time, 18847); assert.equal(f.bot.time.bigTime, 18847n)
  assert.equal(f.bot.time.age, 1000 + 18847 - 11478)
  f.elapse(6000 * 50)
  assert.equal(f.bot.time.day, 1); assert.equal(f.bot.time.moonPhase, 1)
})
test('walking physics cannot double-count the private clock or mutate authoritative packet anchors', () => {
  const f = fixture(); f.update(100)
  for (let tick = 0; tick < 20; tick++) { f.elapse(50); f.bot.emit('physicsTick') }
  assert.equal(f.bot.time.timeOfDay, 120)
  f.bot._client.emit('update_time', { age: [0, 1020], clockUpdates: [] })
  assert.equal(f.bot.time.timeOfDay, 120)
  f.elapse(50); assert.equal(f.bot.time.timeOfDay, 121)
})
test('absolute resets, fractional rates, stopped daylight and backwards clock rates remain authoritative', () => {
  const f = fixture(); f.update(100, 0.5, 0.75); f.elapse(50)
  assert.equal(f.bot.time.timeOfDay, 101)
  f.update(6000, 0, 0, 1001); f.elapse(100000)
  assert.equal(f.bot.time.timeOfDay, 6000); assert.equal(f.bot.time.doDaylightCycle, false)
  assert.equal(f.bot.time.age, 3001, 'world age advances even while its daylight clock is paused')
  f.update(2, -1, 0, 3001); f.elapse(150)
  assert.equal(f.bot.time.timeOfDay, 23999); assert.equal(f.bot.time.bigTime, -1n)
})
test('server age corrects lag, while changed ticking rates and freeze apply without player physics', () => {
  const f = fixture(); f.update(100)
  f.elapse(2000); assert.equal(f.bot.time.timeOfDay, 140)
  f.bot._client.emit('update_time', { age: [0, 1010], clockUpdates: [] })
  assert.equal(f.bot.time.timeOfDay, 110)
  f.bot._client.emit('set_ticking_state', { tick_rate: 10, is_frozen: false })
  f.elapse(1000); assert.equal(f.bot.time.timeOfDay, 120)
  f.bot._client.emit('set_ticking_state', { tick_rate: 10, is_frozen: true })
  f.elapse(10000); assert.equal(f.bot.time.timeOfDay, 120)
  f.bot._client.emit('step_tick', { tick_steps: 4 }); assert.equal(f.bot.time.timeOfDay, 120)
  f.elapse(200); assert.equal(f.bot.time.timeOfDay, 122)
  f.elapse(1000); assert.equal(f.bot.time.timeOfDay, 124, 'step prediction is bounded by the scheduled ticks')
  f.bot._client.emit('update_time', { age: [0, 1020], clockUpdates: [{ id: 0, totalTicks: 124, partialTick: 0, rate: 1 }] })
  assert.equal(f.bot.time.timeOfDay, 124)
})
test('age updates after rate changes reanchor once, without losing or double-counting subsequent intervals', () => {
  const f = fixture(); f.update(100)
  f.elapse(500); f.bot._client.emit('set_ticking_state', { tick_rate: 10, is_frozen: false })
  f.elapse(1000); assert.equal(f.bot.time.timeOfDay, 120)
  f.bot._client.emit('update_time', { age: [0, 1020], clockUpdates: [] })
  assert.equal(f.bot.time.timeOfDay, 120)
  f.elapse(1000); f.bot._client.emit('update_time', { age: [0, 1030], clockUpdates: [] })
  assert.equal(f.bot.time.timeOfDay, 130)
})
test('world-clock registry IDs and dimension default clock override unrelated dimension IDs', () => {
  const f = fixture()
  f.bot._client.emit('registry_data', { id: 'minecraft:world_clock', entries: [{ key: 'minecraft:the_end' }, { key: 'minecraft:overworld' }] })
  f.bot._client.emit('registry_data', { id: 'minecraft:dimension_type', entries: [{ key: 'minecraft:custom', value: nbt.comp({ default_clock: nbt.string('minecraft:overworld') }) }] })
  f.bot._client.emit('update_time', { age: [0, 1000], clockUpdates: [{ id: 0, totalTicks: 1, rate: 1, partialTick: 0 }, { id: 1, totalTicks: 14000, rate: 1, partialTick: 0 }] })
  assert.equal(f.bot.time.timeOfDay, 14000)
  f.bot.game.dimension = 'the_end'; assert.equal(f.bot.time.timeOfDay, 1)
  f.bot.game.dimension = 'custom'; assert.equal(f.bot.time.timeOfDay, 14000)
})
test('legacy absolute signed-time packets remain native and disconnect freezes prediction', () => {
  const f = fixture()
  f.bot._client.emit('update_time', { age: [0, 1000], time: [-1, -6000] })
  assert.equal(f.bot.time.timeOfDay, 6000)
  f.elapse(1000); assert.equal(f.bot.time.timeOfDay, 6000)
  f.update(100); f.elapse(1000); f.bot.emit('end'); f.elapse(1000)
  assert.equal(f.bot.time.timeOfDay, 120)
  assert.equal(f.bot._client.listenerCount('set_ticking_state'), 0)
  assert.equal(f.cleared(), true)
})
test('time events see authoritative resets immediately and keep announcing dusk while mounted', () => {
  const f = fixture(), seen = []
  f.bot.on('time', () => seen.push(f.bot.time.timeOfDay))
  f.update(12540); assert.equal(seen.at(-1), 12540)
  f.elapse(1000); f.publish(); assert.equal(seen.at(-1), 12560)
  f.update(1000); assert.equal(seen.at(-1), 1000)
})
