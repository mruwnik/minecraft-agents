import { test } from 'node:test'
import assert from 'node:assert/strict'
import { itemShortfall } from '../src/lib/inventory.mjs'
import { billShortfall } from '../src/lib/jobs.mjs'
import { shortfall, stageLine } from '../src/blueprint/format.mjs'

test('farm and blueprint bill aliases share the same positive item deficit calculation', () => {
  assert.equal(billShortfall, itemShortfall)
  assert.equal(shortfall, itemShortfall)
  for (const [bill, have, expected] of [
    [{}, {}, {}],
    [{ oak_planks: 5, torch: 2 }, {}, { oak_planks: 5, torch: 2 }],
    [{ oak_planks: 5, torch: 2 }, { oak_planks: 3, torch: 2 }, { oak_planks: 2 }],
    [{ oak_planks: 5 }, { oak_planks: 7 }, {}]
  ]) {
    assert.deepEqual(itemShortfall(bill, have), expected)
    assert.deepEqual(billShortfall(bill, have), expected)
    assert.deepEqual(shortfall(bill, have), expected)
  }
})

test('blueprint stage reporting keeps the existing bill-shortfall presentation', () => {
  assert.equal(stageLine({ n: 1, of: 2, from: 0, to: 2, bill: { oak_planks: 5, torch: 2 } }, { oak_planks: 3 }),
    'stage 1/2 y0..y2 carry=oak_planks:5 torch:2 short=oak_planks:2 torch:2')
})
