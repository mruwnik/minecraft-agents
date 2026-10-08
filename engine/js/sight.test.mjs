import { test } from 'node:test'
import assert from 'node:assert/strict'
import { blocksSight } from './sight.mjs'

test('blocksSight: lava blocks, beds and glass do not, a full stone does', () => {
  const full = name => ({ name, boundingBox: 'block' })
  const none = name => ({ name, boundingBox: 'empty' })
  assert.deepEqual([full('stone'), none('lava'), full('red_bed'), full('glass'), null].map(b => blocksSight(b)), [true, true, false, false, false])
})

test('blocksSight: every leaf block is see-through (a player looks through foliage)', () => {
  const full = name => ({ name, boundingBox: 'block' })
  assert.deepEqual(['oak_leaves', 'azalea_leaves', 'flowering_azalea_leaves', 'cherry_leaves'].map(n => blocksSight(full(n))), [false, false, false, false])
})
