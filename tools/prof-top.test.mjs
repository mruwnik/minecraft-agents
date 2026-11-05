// Why JavaScript: node --test file for tools/prof-top.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import { topSelf } from './prof-top.mjs'
import { profArgs } from './test-run.mjs'

test('topSelf: sums sample deltas per function, biggest first, k cut', () => {
  const profile = {
    nodes: [{ id: 1, callFrame: { functionName: 'a', url: 'x/y/a.js', lineNumber: 4 } }, { id: 2, callFrame: { functionName: '', url: 'x/y/b.js', lineNumber: 0 } }],
    samples: [1, 2, 2, 1], timeDeltas: [1000, 5000, 4000, 1000],
  }
  assert.deepEqual(topSelf(profile, 1), ['       9 ms  (anonymous) y/b.js:1'])
  assert.equal(topSelf(profile).length, 2)
  assert.match(topSelf(profile)[1], /2 ms  a y\/a\.js:5/)
})

test('profArgs: --cpu-prof flags only when a dir is given', () => {
  assert.deepEqual(profArgs(''), [])
  assert.deepEqual(profArgs('/d'), ['--cpu-prof', '--cpu-prof-dir=/d'])
})
