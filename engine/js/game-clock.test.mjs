// Why JavaScript: tests the game clock (game-clock.mjs), a Mineflayer packet adapter.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { createGameClock } from './game-clock.mjs'
import { connectBot } from './connect.mjs'

test('without a packet the rate is 20, unfrozen, assumed', () => {
  const clock = createGameClock()
  assert.deepEqual([clock.rate, clock.frozen, clock.source], [20, false, 'assumed'])
})

test('a set_ticking_state packet sets the rate, the frozen flag and the source, and calls the listeners', () => {
  const clock = createGameClock()
  const client = new EventEmitter()
  const seen = []
  clock.onChange((...args) => seen.push(args))
  clock.attach(client)
  client.emit('set_ticking_state', { tick_rate: 40, is_frozen: false })
  assert.deepEqual([clock.rate, clock.frozen, clock.source], [40, false, 'packet'])
  client.emit('set_ticking_state', { tick_rate: 40, is_frozen: true })
  assert.equal(clock.frozen, true)
  assert.deepEqual(seen, [[40, false, 'packet'], [40, true, 'packet']])
})

test('a clock attached to a second client (a reconnect) keeps listening', () => {
  const clock = createGameClock()
  const a = new EventEmitter()
  const b = new EventEmitter()
  clock.attach(a)
  clock.attach(b)
  b.emit('set_ticking_state', { tick_rate: 10, is_frozen: false })
  assert.equal(clock.rate, 10)
})

test('connectBot attaches the clock before spawn: a packet sent at join, then a drop, is still caught', async () => {
  const clock = createGameClock()
  const client = new EventEmitter()
  const bot = Object.assign(new EventEmitter(), { _client: client, loadPlugin () {}, end () {} })
  const create = () => {
    setImmediate(() => { client.emit('set_ticking_state', { tick_rate: 40, is_frozen: false }); bot.emit('end', 'closed') })
    return bot
  }
  await assert.rejects(connectBot({ host: 'h', port: 1, username: 'u', gameClock: clock }, { create }), /ended before spawn/)
  assert.deepEqual([clock.rate, clock.source], [40, 'packet'])
})
