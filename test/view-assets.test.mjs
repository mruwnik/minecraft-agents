// The view's table, texture and element bytes are built in a worker thread that exits afterwards, so the long-lived server keeps none of the build's heap.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { buildAssets, buildAssetsInWorker, createAssetCache } from '../tools/view/view-assets.mjs'
import { findClientJar } from '../tools/view/jar-read.mjs'

const textureDir = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'textures')

for (const [label, jarPath] of [['no jar', null], ['client jar', findClientJar()]]) {
  test(`the worker build returns the same bytes as the in-thread build (${label})`, { skip: label === 'client jar' && jarPath === null }, async () => {
    const local = buildAssets('26.1', textureDir, jarPath)
    const remote = await buildAssetsInWorker('26.1', textureDir, jarPath)
    assert.equal(remote.table, local.table)
    assert.ok(Buffer.isBuffer(remote.textures) && Buffer.isBuffer(remote.elements))
    assert.ok(remote.textures.length > 0)
    assert.ok(remote.textures.equals(local.textures))
    assert.ok(remote.elements.equals(local.elements))
  })
}

test('concurrent requests for one version share one build, other versions build separately', async () => {
  const calls = []
  const cache = createAssetCache(async version => { calls.push(version); await new Promise(r => setTimeout(r, 10)); return { version } })
  const [a, b, c] = await Promise.all([cache('1.0'), cache('1.0'), cache('2.0')])
  assert.equal(a, b)
  assert.deepEqual(calls, ['1.0', '2.0'])
  assert.equal(c.version, '2.0')
  assert.equal(await cache('1.0'), a)
  assert.deepEqual(calls, ['1.0', '2.0'])
})

test('a failed build rejects every waiter and a later request retries', async () => {
  let attempts = 0
  const cache = createAssetCache(async () => {
    attempts++
    if (attempts === 1) throw new Error('boom')
    return { ok: true }
  })
  const first = [cache('1.0'), cache('1.0')]
  await Promise.all(first.map(p => assert.rejects(p, /boom/)))
  assert.equal(attempts, 1)
  assert.deepEqual(await cache('1.0'), { ok: true })
  assert.equal(attempts, 2)
})

test('a worker that fails rejects with its message', async () => {
  await assert.rejects(buildAssetsInWorker('not-a-version', textureDir, null))
})
