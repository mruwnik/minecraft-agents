// Why JavaScript: tests the Mineflayer connection adapter (connect.mjs), which is JS.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { botOptions, DEFAULTS } from './connect.mjs'

test('botOptions passes the default view distance to mineflayer', () => {
  assert.equal(botOptions({ host: 'h', port: 1, username: 'u' }).viewDistance, DEFAULTS.viewDistance)
})

test('botOptions passes a configured view distance', () => {
  assert.equal(botOptions({ host: 'h', port: 1, username: 'u', viewDistance: 5 }).viewDistance, 5)
})

test('botOptions keeps the connection settings', () => {
  assert.deepEqual(botOptions({ host: 'h', port: 1, username: 'u' }), { host: 'h', port: 1, username: 'u', auth: 'offline', version: '26.1', viewDistance: DEFAULTS.viewDistance })
})

test('botOptions refuses a version the planner block table is not built for', () => {
  assert.throws(() => botOptions({ host: 'h', port: 1, username: 'u', version: '1.20.4' }), /not supported/)
})

test('botOptions swaps in tick-rate physics only when asked', () => {
  const base = { host: 'h', port: 1, username: 'u' }
  assert.equal(botOptions({ ...base, followTickRate: false }).plugins, undefined)
  assert.equal(typeof botOptions({ ...base, followTickRate: true }).plugins.physics, 'function')
})
