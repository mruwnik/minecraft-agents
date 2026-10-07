import { test } from 'node:test'
import assert from 'node:assert/strict'
import { blocksSight } from './sight.mjs'

test('blocksSight: lava blocks, beds and glass do not, a full stone does', () => {
  const full = name => ({ name, boundingBox: 'block' })
  const none = name => ({ name, boundingBox: 'empty' })
  assert.deepEqual([full('stone'), none('lava'), full('red_bed'), full('glass'), null].map(b => blocksSight(b)), [true, true, false, false, false])
})
