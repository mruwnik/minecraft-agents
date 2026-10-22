// Why JavaScript: tests the JavaScript primitives' settings reader (node --test).
import test from 'node:test'
import assert from 'node:assert/strict'
import { SPECS, setting, useSettings } from './settings.mjs'
import { waitForWorld } from './prim-base.mjs'
import { readdirSync, readFileSync } from 'node:fs'

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
  let ms = 600
  useSettings(k => (k === 'prim/world-timeout-ms' ? ms : undefined))
  const slow = await waited()
  ms = 5
  const quick = await waited()
  useSettings(null)
  assert.ok(slow >= 590, `first wait ${slow}`)
  assert.ok(quick < slow / 2, `second wait ${quick}`)
})

test('every spec has a default that fits its own spec', () => {
  for (const [k, { default: d, spec: [kind, lo, hi] }] of Object.entries(SPECS)) {
    assert.ok(Number.isFinite(d) && (lo === null || d >= lo) && (hi === null || d <= hi), k)
    assert.ok(kind === 'num-in' || (kind === 'int-in' && Number.isInteger(d)), k)
  }
})

test('every declared setting is read by some primitive file', () => {
  const sources = readdirSync(new URL('.', import.meta.url))
    .filter(f => f.endsWith('.mjs') && !f.includes('.test.') && f !== 'settings.mjs')
    .map(f => readFileSync(new URL(f, import.meta.url), 'utf8')).join('\n')
    + readFileSync(new URL('../../tools/view/renderer.mjs', import.meta.url), 'utf8') // the viewer reads view/held-torch-light
  const unread = Object.keys(SPECS).filter(k => !sources.includes(`'${k}'`))
  assert.deepEqual(unread, [])
})

test('trade and enchant keep their 25 ms poll', () => {
  assert.equal(SPECS['trade/poll-ms'].default, 25)
  assert.equal(SPECS['enchant/poll-ms'].default, 25)
})
