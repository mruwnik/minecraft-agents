// Why JavaScript: node --test file for tools/sky-shares.mjs (a Node tool over recorded chunk columns).
import test from 'node:test'
import assert from 'node:assert/strict'
import { columnSky } from './sky-shares.mjs'

test('each level counts the open blocks at that sky light or more against the other open ones; solid ones not at all', () => {
  const solid = new Uint8Array(4096)
  const sky = new Uint8Array(4096)
  sky[0] = 15
  sky[1] = 8
  solid[2] = 1
  sky[2] = 15
  const levels = columnSky(solid, sky)
  assert.equal(levels.length, 15)
  assert.deepEqual([1, 8, 9, 15].map(l => [levels[l - 1].wall[0], levels[l - 1].open[0]]), [[2, 4093], [2, 4093], [1, 4094], [1, 4094]])
})
