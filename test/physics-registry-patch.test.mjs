// Why JavaScript: node:test for the dependency patch tools/dependency-patches/physics-registry.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { patchPhysicsRegistry } from '../tools/dependency-patches/physics-registry.mjs'

test('PlayerState reads the bot registry instead of building minecraft-data per tick; the patch is idempotent', () => {
  const source = fs.readFileSync(new URL('../node_modules/prismarine-physics/index.js', import.meta.url), 'utf8')
  const result = patchPhysicsRegistry(source)
  assert.ok(['patched', 'already'].includes(result.status))
  assert.ok(result.source.includes('bot.registry ?? require'))
  assert.equal(patchPhysicsRegistry(result.source).status, 'already')
  const unknown = 'function unrelated () {}'
  assert.deepEqual(patchPhysicsRegistry(unknown), { status: 'anchor missing', source: unknown })
})
