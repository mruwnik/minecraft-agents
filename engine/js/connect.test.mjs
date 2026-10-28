// Why JavaScript: tests the Mineflayer connection adapter (connect.mjs), which is JS.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import prismarineRegistry from 'prismarine-registry'
import { botOptions, connectBot, DEFAULTS } from './connect.mjs'
import { createSense } from './prim-sense.mjs'

test('botOptions passes the default view distance to mineflayer', () => {
  assert.equal(botOptions({ host: 'h', port: 1, username: 'u' }).viewDistance, DEFAULTS.viewDistance)
})

test('botOptions passes a configured view distance', () => {
  assert.equal(botOptions({ host: 'h', port: 1, username: 'u', viewDistance: 5 }).viewDistance, 5)
})

test('botOptions keeps the connection settings', () => {
  assert.deepEqual(botOptions({ host: 'h', port: 1, username: 'u', followTickRate: false }), { host: 'h', port: 1, username: 'u', auth: 'offline', version: '26.1', viewDistance: DEFAULTS.viewDistance, validateChannelProtocol: false })
})

test('botOptions refuses a version the planner block table is not built for', () => {
  assert.throws(() => botOptions({ host: 'h', port: 1, username: 'u', version: '1.20.4' }), /not supported/)
})

test('botOptions swaps in tick-rate physics by default and not when turned off', () => {
  const base = { host: 'h', port: 1, username: 'u' }
  assert.equal(typeof botOptions(base).plugins.physics, 'function')
  assert.equal(typeof botOptions({ ...base, followTickRate: true }).plugins.physics, 'function')
  assert.equal(botOptions({ ...base, followTickRate: false }).plugins, undefined)
})

// A join packet that dropped before spawn still ends up on the bot, so each bot of a reconnect carries its own value.
const dropAfter = async packets => {
  const client = new EventEmitter()
  const bot = Object.assign(new EventEmitter(), { _client: client, registry: prismarineRegistry(DEFAULTS.version), supportFeature: () => false, loadPlugin () {}, end () {} })
  const create = () => {
    setImmediate(() => { for (const [name, packet] of packets) client.emit(name, packet); bot.emit('end', 'closed') })
    return bot
  }
  await assert.rejects(connectBot({ host: 'h', port: 1, username: 'u' }, { create }), /ended before spawn/)
  return bot
}

test('the simulation distance is caught from the login packet and kept current by simulation_distance, on each bot', async () => {
  const first = await dropAfter([['login', { simulationDistance: 6 }]])
  assert.equal(first.simulationDistance, 6)
  const updated = await dropAfter([['login', { simulationDistance: 6 }], ['simulation_distance', { distance: 4 }]])
  assert.equal(updated.simulationDistance, 4)
  const env = { bot: first, isOffline: () => false }
  const sense = createSense(env)
  assert.equal(sense.serverRules().simulationDistance, 6)
  env.bot = updated
  assert.equal(sense.serverRules().simulationDistance, 4)
})

test('the difficulty packet sets the difficulty name, given as 26.1 names it or as an index', async () => {
  const named = await dropAfter([['difficulty', { difficulty: 'easy', difficultyLocked: false }]])
  assert.equal(createSense({ bot: named, isOffline: () => false }).serverRules().difficulty, 'easy')
  const indexed = await dropAfter([['difficulty', { difficulty: 3, difficultyLocked: false }]])
  assert.equal(indexed.difficulty, 'hard')
})
