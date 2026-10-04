import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { execute, main, options } from '../../tools/entities.mjs'
import { readEDN, writeEDN } from '../../tools/observe-lib.mjs'

function fixture (t) {
  const state = fs.mkdtempSync(path.join(os.tmpdir(), 'entities-cli-'))
  const worldDir = path.join(state, 'worlds', 'w')
  const socketPath = path.join(worldDir, 'agents', 'Probe', 'engine', 'control.sock')
  fs.mkdirSync(path.dirname(socketPath), { recursive: true })
  fs.writeFileSync(path.join(worldDir, 'world.json'), '{}\n')
  t.after(() => fs.rmSync(state, { recursive: true, force: true }))
  const paths = []
  const getImpl = async (socket, url, options = {}) => {
    paths.push([socket, url, options.maxBytes])
    return { status: 200, contentType: 'application/edn', text: writeEDN(fx.value) }
  }
  const fx = { state, socketPath, worldDir, paths, getImpl, value: null }
  return fx
}

const entity = (key, type, pos, now, age = 0, extra = {}) => ({
  key: `${type}/${key}`, uuid: `uuid-${key}`, type, dimension: 'overworld', pos,
  'observed-at': now - age, 'expires-at': now + 120000 - age, ...extra
})

function snapshot (now, entities, online = true) {
  return { ok: true, world: 'w', body: 'Probe', now, 'ttl-ms': 120000, 'online?': online,
    entities, count: entities.length, 'cached-count': entities.length, cap: 10000,
    'snapshot-cap-bytes': 4 * 1024 * 1024, 'truncated?': false, dropped: 0 }
}

test('requires a body and world and supplies a self-centred bounded default query', t => {
  const fx = fixture(t)
  fx.value = snapshot(Date.now(), [])
  const request = options(['Probe', '--world', 'w', '--state', fx.state])
  assert.equal(request.body, 'Probe')
  assert.equal(request.radius, 64)
  assert.equal(request.limit, 10)
  assert.throws(() => options(['Probe', '--state', fx.state]), { reason: 'invalid-world' })
  assert.throws(() => options(['Probe', '--world', 'w', '--state', fx.state, '--center', '1,,2']), { reason: 'invalid-center' })
  assert.throws(() => options(['Probe', '--world', 'w', '--state', fx.state, '--limit', '51']), { reason: 'invalid-limit' })
})

test('reads only /entities over the body socket and compactly reports cached ages', async t => {
  const now = Date.now()
  const body = snapshot(now, [
    entity('self', 'player', { x: 0.45, y: 64, z: 0.52 }, now, 100, { 'self?': true, username: 'Probe' }),
    entity('cow', 'cow', { x: 3.24, y: 64, z: 0.54 }, now, 1234),
    entity('alex', 'player', { x: 7, y: 64, z: 0 }, now, 850, { username: 'Alex' }),
    entity('stale', 'zombie', { x: 2, y: 64, z: 0 }, now, 120000)
  ], false)
  const fx = fixture(t)
  fx.value = body
  const result = await execute(options(['Probe', '--world', 'w', '--state', fx.state]), fx.getImpl)
  assert.deepEqual(fx.paths, [[fx.socketPath, '/entities', 4 * 1024 * 1024 + 4096]])
  assert.ok(fx.paths[0][0].endsWith(path.join('engine', 'control.sock')))
  assert.equal(result.ok, true)
  assert.equal(result.online, undefined)
  assert.equal(result['online?'], false)
  assert.equal(result.dimension, 'overworld')
  assert.deepEqual(result.center, [0.5, 64, 0.5])
  assert.equal(result.total, 2)
  assert.equal(result.items[0].uuid, 'uuid-cow')
  assert.equal(result.items[0].type, 'cow')
  assert.deepEqual(result.items[0].pos, [3.2, 64, 0.5])
  assert.equal(result.items[0]['age-ms'], 1234)
  assert.equal(result.items[1].player, 'Alex')
  assert.equal('world' in result, false)
})

test('type, player, dimension and explicit centre filters compose; dimension cannot borrow the wrong origin', async t => {
  const now = Date.now()
  const body = snapshot(now, [
    entity('self', 'player', { x: 0, y: 64, z: 0 }, now, 5, { 'self?': true, username: 'Probe' }),
    entity('alex', 'player', { x: 4, y: 64, z: 0 }, now, 10, { username: 'Alex' }),
    entity('steve', 'player', { x: 5, y: 64, z: 0 }, now, 10, { username: 'Steve' }),
    { ...entity('nether', 'cow', { x: 0, y: 64, z: 0 }, now), dimension: 'the_nether' }
  ])
  const fx = fixture(t)
  fx.value = body
  const request = options(['Probe', '--world', 'w', '--state', fx.state, '--type', 'player', '--player', 'Alex', '--radius', '10'])
  const result = await execute(request, fx.getImpl)
  assert.deepEqual(result.items.map(item => item.player), ['Alex'])
  const mismatch = await execute(options(['Probe', '--world', 'w', '--state', fx.state, '--dimension', 'the_nether']), fx.getImpl)
  assert.equal(mismatch.reason.key, 'dimension-origin-mismatch')
  const explicit = await execute(options(['Probe', '--world', 'w', '--state', fx.state, '--dimension', 'the_nether', '--center', '1,64,1']), fx.getImpl)
  assert.equal(explicit.dimension, 'the_nether')
})

test('compact rows keep an ephemeral cache key distinct from a UUID', async t => {
  const now = Date.now()
  const self = entity('self', 'player', { x: 0, y: 64, z: 0 }, now, 1, { 'self?': true })
  const transient = { key: 'item/temporary', type: 'item', dimension: 'overworld',
    pos: { x: 1, y: 64, z: 0 }, 'observed-at': now - 5, 'expires-at': now + 119995 }
  const fx = fixture(t)
  fx.value = snapshot(now, [self, transient])
  const result = await execute(options(['Probe', '--world', 'w', '--state', fx.state]), fx.getImpl)
  assert.equal(result.items[0].uuid, undefined)
  assert.equal(result.items[0].key, 'item/temporary')
})

test('raw output retains server timestamps but caps pages and never refreshes a row', async t => {
  const now = Date.now()
  const rows = [entity('self', 'player', { x: 0, y: 64, z: 0 }, now, 1, { 'self?': true })]
  for (let i = 0; i < 80; i++) rows.push(entity(`cow-${i}`, 'cow', { x: i % 10, y: 64, z: Math.floor(i / 10) }, now, i * 100))
  const fx = fixture(t)
  fx.value = snapshot(now, rows)
  const result = await execute(options(['Probe', '--world', 'w', '--state', fx.state, '--center', '0,64,0', '--dimension', 'overworld', '--raw', '--limit', '50']), fx.getImpl)
  assert.equal(result.items.length, 50)
  assert.equal(result.total, 80)
  assert.equal(result['more?'], true)
  assert.equal(result['next-offset'], 50)
  assert.equal(result.now, now)
  assert.equal(result.items[0]['observed-at'], now)
  assert.ok(Buffer.byteLength(writeEDN(result)) < 65536)
  const second = await execute(options(['Probe', '--world', 'w', '--state', fx.state, '--center', '0,64,0', '--dimension', 'overworld', '--raw', '--limit', '50', '--offset', '50']), fx.getImpl)
  assert.equal(second.items.length, 30)
  assert.equal(second['more?'], false)
})

test('a missing origin is an explicit error and CLI validation stays compact', async t => {
  const now = Date.now()
  const fx = fixture(t)
  fx.value = snapshot(now, [entity('cow', 'cow', { x: 0, y: 64, z: 0 }, now)])
  const result = await execute(options(['Probe', '--world', 'w', '--state', fx.state]), fx.getImpl)
  assert.equal(result.ok, false)
  assert.equal(result.reason.key, 'origin-unavailable')
  const lines = []
  const status = await main(['Probe'], text => lines.push(text))
  assert.equal(status, 2)
  assert.equal(readEDN(lines[0]).ok, false)
})
