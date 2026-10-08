// Why JavaScript: tests the JavaScript primitives' settings reader (node --test).
import test from 'node:test'
import assert from 'node:assert/strict'
import { SPECS, setting, useSettings } from './settings.mjs'
import { waitForWorld } from './prim-base.mjs'

const unloaded = { entity: { position: { x: 0, y: 0, z: 0 } }, blockAt: () => null }
const waited = async () => { const t = Date.now(); await waitForWorld(unloaded); return Date.now() - t }

test('without a reader a setting is its spec default', () => {
  useSettings(null)
  assert.equal(setting('prim/poll-ms'), SPECS['prim/poll-ms'].default)
})

test('a reader answers first, a nil answer falls back to the default', () => {
  useSettings(k => (k === 'prim/poll-ms' ? 7 : undefined))
  assert.equal(setting('prim/poll-ms'), 7)
  assert.equal(setting('prim/reach'), 4.5)
  useSettings(null)
})

test('a reload changes what the next call of a primitive reads', async () => {
  let ms = 150
  useSettings(k => (k === 'prim/world-timeout-ms' ? ms : undefined))
  const slow = await waited()
  ms = 5
  const quick = await waited()
  useSettings(null)
  assert.ok(slow >= 140, `first wait ${slow}`)
  assert.ok(quick < 100, `second wait ${quick}`)
})

test('every spec has a default that fits its own spec', () => {
  for (const [k, { default: d, spec: [kind, lo, hi] }] of Object.entries(SPECS)) {
    assert.ok(Number.isFinite(d) && (lo === null || d >= lo) && (hi === null || d <= hi), k)
    assert.ok(kind === 'num-in' || (kind === 'int-in' && Number.isInteger(d)), k)
  }
})
