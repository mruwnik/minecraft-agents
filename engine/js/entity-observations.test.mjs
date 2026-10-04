// Mineflayer boundary only; cache identity, TTL and bounds are tested in CLJS.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createPrimitivesFromBot } from './primitives.mjs'
import { stubBot } from './stub-bot.mjs'

test('entity observations expose every local tracked entity without radius or32 filters', async () => {
  const entities = Object.fromEntries(Array.from({ length: 64 }, (_, id) => [id, {
    id, name: id % 2 ? 'villager' : 'player', uuid: `uuid-${id}`,
    position: { x: 1000 + id, y: 64, z: 0 }
  }]))
  const bot = stubBot({ entities })
  bot.game = { dimension: 'overworld' }
  const primitives = createPrimitivesFromBot(bot)
  try {
    const sample = primitives.entityObservation()
    assert.equal(sample.online, true)
    assert.equal(sample.dimension, 'overworld')
    assert.equal(sample.source, bot)
    assert.equal(sample.entities.length, 64)
    assert.equal(sample.entities[0], entities[0])
    assert.equal(bot.calls.length, 0)
  } finally { await primitives.close() }
})

test('disconnected and stalled bot caches never report fresh observations', async t => {
  t.mock.timers.enable({ apis: ['Date', 'setInterval'] })
  const bot = stubBot()
  const primitives = createPrimitivesFromBot(bot)
  try {
    assert.equal(primitives.entityObservation().online, true)
    t.mock.timers.tick(10001)
    assert.equal(primitives.entityObservation().online, false)
    bot.emit('physicsTick')
    assert.equal(primitives.entityObservation().online, true)
    bot.emit('end', 'disconnected')
    assert.equal(primitives.entityObservation().online, false)
  } finally { await primitives.close() }
  assert.equal(primitives.entityObservation().online, false)
})

test('the physics watchdog stops refreshing before the stale connection deadline', async t => {
  t.mock.timers.enable({ apis: ['Date', 'setInterval'] })
  const bot = stubBot({ unloaded: true })
  const primitives = createPrimitivesFromBot(bot)
  try {
    assert.equal(primitives.entityObservation().online, true)
    t.mock.timers.tick(2251)
    assert.equal(primitives.entityObservation().online, false)
    bot.emit('physicsTick')
    assert.equal(primitives.entityObservation().online, true)
  } finally { await primitives.close() }
})

test('explicit deaths pass raw identity and dimension once; unload is not death and cleanup unregisters', async () => {
  const e = { id: 1, uuid: 'villager-id', name: 'villager', position: { x: 0, y: 64, z: 0 } }
  const bot = stubBot({ entities: { 1: e } })
  bot.game = { dimension: 'the_nether' }
  const primitives = createPrimitivesFromBot(bot)
  const events = []
  const stop = primitives.onEntityDeath(sample => events.push(sample))
  bot.emit('entityGone', e)
  assert.equal(events.length, 0)
  bot.emit('entityDead', e)
  assert.deepEqual(events, [{ entity: e, source: bot, dimension: 'the_nether' }])
  stop()
  bot.emit('entityDead', e)
  assert.equal(events.length, 1)
  await primitives.close()
  assert.equal(bot.listenerCount('entityDead'), 0)
})

test('reconnect rebinds observations and deaths to the fresh bot and cleans the old listener', async () => {
  const old = stubBot(), fresh = stubBot()
  old.game = { dimension: 'overworld' }
  fresh.game = { dimension: 'the_nether' }
  const primitives = createPrimitivesFromBot(old, { timeScale: 0.001, reconnect: async () => fresh })
  const deaths = []
  const stop = primitives.onEntityDeath(sample => deaths.push(sample))
  primitives.setOwner('probe')
  try {
    await primitives.offline('probe', { ms: 0 })
    assert.equal(old.listenerCount('entityDead'), 0)
    assert.equal(primitives.entityObservation().source, fresh)
    assert.equal(primitives.entityObservation().online, true)
    const e = { uuid: 'reconnected-cow' }
    old.emit('entityDead', e)
    fresh.emit('entityDead', e)
    assert.deepEqual(deaths, [{ entity: e, source: fresh, dimension: 'the_nether' }])
    stop()
    assert.equal(fresh.listenerCount('entityDead'), 0)
  } finally { await primitives.close() }
})

test('closing unregisters lifecycle callbacks but safely ignores late socket errors', async () => {
  const bot = stubBot()
  const primitives = createPrimitivesFromBot(bot)
  await primitives.close()
  assert.equal(bot.listenerCount('physicsTick'), 0)
  assert.equal(bot.listenerCount('entityDead'), 0)
  assert.doesNotThrow(() => bot.emit('error', new Error('late quit error')))
})
