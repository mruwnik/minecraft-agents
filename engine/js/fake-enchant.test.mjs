import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = { x: 1, y: 64, z: 0 }
const spot = '1,64,0'
const sword = { name: 'diamond_sword', count: 1 }
const lapis = n => ({ name: 'lapis_lazuli', count: n })

const owned = spec => {
  const p = createFake({ blocks: { [spot]: 'enchanting_table' }, ...spec })
  p.setOwner('t')
  return p
}
const carried = p => Object.fromEntries(p.world.state.inventory.map(i => [i.name, i.count]))
const level = p => p.world.state.self.experience.level
const offers = (p, item = 'diamond_sword') => p.enchant('t', { pos: at, op: 'offers', item })
const enchant = (p, choice, extra = {}) => p.enchant('t', { pos: at, op: 'enchant', item: 'diamond_sword', choice, ...extra })

test('offers with no bookshelves are low, with fifteen they reach level 30; lapis cost is the slot number', async () => {
  const bare = await offers(owned({ inventory: [sword, lapis(3)], self: { experience: { level: 5 } } }))
  assert.deepEqual(bare.offers.map(o => [o.levelCost, o.lapisCost]), [[2, 1], [3, 2], [5, 3]])
  assert.deepEqual([bare.status, bare.xpLevel, bare.lapis], ['ok', 5, 3])
  const shelved = await offers(owned({ inventory: [sword], enchantTables: { [spot]: { shelves: 15 } } }))
  assert.deepEqual(shelved.offers.map(o => o.levelCost), [10, 20, 30])
})

test('a table may be given exact offers and hints', async () => {
  const p = owned({ inventory: [sword], enchantTables: { [spot]: { offers: [7, 11, 13], hints: [['sharpness', 2], null, ['unbreaking', 1]] } } })
  const r = await offers(p)
  assert.deepEqual(r.offers.map(o => [o.levelCost, o.hint]), [[7, { enchant: 'sharpness', level: 2 }], [11, null], [13, { enchant: 'unbreaking', level: 1 }]])
})

test('offers only read: the item stays carried, nothing is spent', async () => {
  const p = owned({ inventory: [sword, lapis(3)], self: { experience: { level: 9 } } })
  await offers(p)
  assert.deepEqual(carried(p), { diamond_sword: 1, lapis_lazuli: 3 })
  assert.equal(level(p), 9)
})

test('enchant spends the slot number in lapis and levels and the item comes out enchanted', async () => {
  const p = owned({ inventory: [sword, lapis(5)], self: { experience: { level: 30 } }, enchantTables: { [spot]: { shelves: 15 } } })
  const r = await enchant(p, 2, { levelCost: 30 })
  assert.equal(r.status, 'enchanted')
  assert.deepEqual([r.lapisSpent, r.levelsSpent, r.xpLevel], [3, 3, 27])
  assert.ok(r.enchants.length >= 1)
  assert.deepEqual(carried(p), { diamond_sword: 1, lapis_lazuli: 2 })
  assert.deepEqual(p.world.state.inventory.find(i => i.name === 'diamond_sword').enchants, r.enchants)
})

test('a book comes out as an enchanted book, one of a stack at a time', async () => {
  const p = owned({ inventory: [{ name: 'book', count: 3 }, lapis(3)], self: { experience: { level: 10 } } })
  const r = await p.enchant('t', { pos: at, op: 'enchant', item: 'book', choice: 0 })
  assert.equal(r.status, 'enchanted')
  assert.deepEqual(carried(p), { book: 2, enchanted_book: 1, lapis_lazuli: 2 })
})

const refusals = [
  ['missing', { blocks: {} }, 'offers', {}, { status: 'missing' }],
  ['a chest', { blocks: { [spot]: 'chest' } }, 'offers', {}, { status: 'cannot', reason: 'not-a-table' }],
  ['too far', { blocks: { '9,64,0': 'enchanting_table' } }, 'offers', { pos: { x: 9, y: 64, z: 0 } }, { status: 'unreachable', reason: 'too-far' }],
  ['item not carried', { inventory: [lapis(3)] }, 'offers', {}, { status: 'no-item', item: 'diamond_sword' }],
  ['already enchanted', { inventory: [{ ...sword, enchants: [{ name: 'sharpness', level: 1 }] }, lapis(3)] }, 'offers', {}, { status: 'cannot', reason: 'already-enchanted', item: 'diamond_sword' }],
  ['no lapis', { inventory: [sword], self: { experience: { level: 30 } } }, 'enchant', { choice: 0 }, { status: 'no-lapis', have: 0, need: 1 }],
  ['too few levels', { inventory: [sword, lapis(3)], self: { experience: { level: 2 } }, enchantTables: { [spot]: { offers: [4, 9, 16] } } }, 'enchant', { choice: 0 }, { status: 'no-levels', need: 4, have: 2 }],
  ['not enchantable', { inventory: [{ name: 'dirt', count: 1 }, lapis(3)], self: { experience: { level: 30 } } }, 'enchant', { item: 'dirt', choice: 0 }, { status: 'cannot', reason: 'not-enchantable' }],
  ['offer changed', { inventory: [sword, lapis(3)], self: { experience: { level: 30 } } }, 'enchant', { choice: 0, levelCost: 99 }, { status: 'cannot', reason: 'offer-changed', offers: [2, 3, 5] }]
]
for (const [label, spec, op, extra, expected] of refusals) {
  test(`refusal: ${label}`, async () => {
    const p = owned(spec)
    const before = structuredClone(p.world.state.inventory)
    const r = await p.enchant('t', { pos: at, op, item: 'diamond_sword', ...extra })
    assert.deepEqual(Object.fromEntries(Object.keys(expected).map(k => [k, r[k]])), expected)
    assert.deepEqual(p.world.state.inventory, before)
  })
}

test('a table can be busy: the window does not open', async () => {
  const p = owned({ inventory: [sword], enchantTables: { [spot]: { busy: true } } })
  assert.deepEqual(await offers(p), { status: 'failed', reason: 'window-did-not-open' })
})

test('bad args are rejected', async () => {
  const p = owned({ inventory: [sword] })
  for (const a of [{}, { pos: at }, { pos: at, op: 'stir', item: 'x' }, { pos: at, op: 'offers' }, { pos: at, op: 'enchant', item: 'x', choice: 3 }, { pos: at, op: 'enchant', item: 'x' }]) {
    await assert.rejects(p.enchant('t', a), { code: 'bad-args' })
  }
})
