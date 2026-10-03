import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const stacks = n => Array.from({ length: n }, (_, i) => ({ name: `item_${i}`, count: 1 }))

const rows = [
  { label: 'empty hand', held: null, inv: [], expect: { status: 'empty' }, after: null },
  { label: 'ok', held: 'stick', inv: stacks(3), expect: { status: 'ok', item: 'stick' }, after: null },
  { label: 'full', held: 'stick', inv: stacks(36), expect: { status: 'full' }, after: 'stick' }
]

for (const c of rows) {
  test(`unequip: ${c.label}`, async () => {
    const p = createFake({ self: { held: c.held }, inventory: c.inv })
    p.setOwner('t')
    assert.deepEqual(await p.unequip('t', {}), c.expect)
    assert.equal(p.world.state.self.held, c.after)
  })
}

test('unequip: a held call rejects with cut when the owner changes', async () => {
  const p = createFake()
  p.setOwner('t')
  p.world.hold('unequip')
  const pending = p.unequip('t', {})
  p.setOwner('u')
  await assert.rejects(pending, { code: 'cut' })
})
