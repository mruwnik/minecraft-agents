import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = { x: 1, y: 64, z: 0 }
const spot = '1,64,0'

const owned = spec => {
  const p = createFake({ blocks: { [spot]: 'furnace' }, ...spec })
  p.setOwner('t')
  return p
}
const carried = p => Object.fromEntries(p.world.state.inventory.map(i => [i.name, i.count]))
const lit = p => p.blockAt(at).properties?.lit
const load = (p, input, fuel) => p.furnace('t', { pos: at, op: 'load', ...(input && { input }), ...(fuel && { fuel }) })
const read = p => p.furnace('t', { pos: at, op: 'read' })
const stock = [{ name: 'raw_iron', count: 5 }, { name: 'beef', count: 5 }, { name: 'coal', count: 3 }, { name: 'oak_planks', count: 4 }]

test('load puts input and fuel in and answers what moved and what the furnace holds', async () => {
  const p = owned({ inventory: stock })
  const r = await load(p, { item: 'raw_iron', count: 3 }, { item: 'coal', count: 1 })
  assert.deepEqual([r.status, r.moved, r.input, r.fuel, r.output], ['ok', { input: 3, fuel: 1 }, { name: 'raw_iron', count: 3 }, { name: 'coal', count: 1 }, null])
  assert.deepEqual(carried(p), { raw_iron: 2, beef: 5, coal: 2, oak_planks: 4 })
})

const cooking = [
  { label: 'a furnace smelts iron in 10 s an item', block: 'furnace', input: 'raw_iron', output: 'iron_ingot', ticks: 200 },
  { label: 'a blast furnace in half that', block: 'blast_furnace', input: 'raw_iron', output: 'iron_ingot', ticks: 100 },
  { label: 'a smoker cooks food in half that', block: 'smoker', input: 'beef', output: 'cooked_beef', ticks: 100 },
  { label: 'a furnace cooks food at the plain rate', block: 'furnace', input: 'beef', output: 'cooked_beef', ticks: 200 }
]
for (const c of cooking) {
  test(`cooking: ${c.label}`, async () => {
    const p = owned({ inventory: stock, blocks: { [spot]: c.block } })
    await load(p, { item: c.input, count: 2 }, { item: 'coal', count: 1 })
    p.world.advance(c.ticks - 1)
    assert.equal((await read(p)).output, null)
    p.world.advance(1)
    assert.deepEqual((await read(p)).output, { name: c.output, count: 1 })
    p.world.advance(c.ticks)
    const r = await read(p)
    assert.deepEqual([r.output, r.input], [{ name: c.output, count: 2 }, null])
  })
}

test('the block is lit while it burns and goes out with the last fuel', async () => {
  const p = owned({ inventory: stock })
  assert.equal(lit(p), false)
  await load(p, { item: 'raw_iron', count: 1 }, { item: 'oak_planks', count: 1 })
  p.world.advance(1)
  assert.equal(lit(p), true)
  p.world.advance(300)
  assert.equal(lit(p), false)
})

test('one coal burns for eight items, then the rest wait with the fire out', async () => {
  const p = owned({ inventory: [{ name: 'raw_iron', count: 10 }, { name: 'coal', count: 1 }] })
  await load(p, { item: 'raw_iron', count: 10 }, { item: 'coal', count: 1 })
  p.world.advance(200 * 10)
  const r = await read(p)
  assert.deepEqual([r.output.count, r.input.count, r.lit, lit(p)], [8, 2, false, false])
})

test('a smoker and a blast furnace burn fuel twice as fast: the coal bar is half, the eight items the same', async () => {
  const p = owned({ inventory: [{ name: 'beef', count: 10 }, { name: 'coal', count: 1 }], blocks: { [spot]: 'smoker' } })
  const loaded = await load(p, { item: 'beef', count: 10 }, { item: 'coal', count: 1 })
  p.world.advance(1)
  assert.deepEqual((await read(p)).burn, { left: 800, total: 800 })
  p.world.advance(2000)
  assert.deepEqual([(await read(p)).output.count, loaded.status], [8, 'ok'])
})

test('the input slot takes anything, and what the kind cannot smelt just sits there, even burning', async () => {
  const p = owned({ inventory: stock, blocks: { [spot]: 'smoker' } })
  const r = await load(p, { item: 'raw_iron', count: 2 }, { item: 'coal', count: 1 })
  p.world.advance(1000)
  const after = await read(p)
  assert.deepEqual([r.status, after.input, after.output, after.fuel, after.lit], ['ok', { name: 'raw_iron', count: 2 }, null, { name: 'coal', count: 1 }, false])
})

test('without fuel nothing cooks', async () => {
  const p = owned({ inventory: stock })
  await load(p, { item: 'raw_iron', count: 2 })
  p.world.advance(1000)
  const r = await read(p)
  assert.deepEqual([r.input.count, r.output, r.lit], [2, null, false])
})

test('read reports the bars of the burning fuel and of the item in the fire', async () => {
  const p = owned({ inventory: stock })
  await load(p, { item: 'raw_iron', count: 2 }, { item: 'coal', count: 1 })
  p.world.advance(50)
  const r = await read(p)
  assert.deepEqual([r.lit, r.burn, r.cook], [true, { left: 1551, total: 1600 }, { done: 50, total: 200 }])
})

test('take: the output comes to the pockets, input and fuel only on request', async () => {
  const p = owned({ inventory: stock })
  await load(p, { item: 'raw_iron', count: 3 }, { item: 'coal', count: 2 })
  p.world.advance(400)
  const r = await p.furnace('t', { pos: at, op: 'take' })
  assert.deepEqual([r.status, r.taken, r.input, r.fuel], ['ok', [{ part: 'output', name: 'iron_ingot', count: 2 }], { name: 'raw_iron', count: 1 }, { name: 'coal', count: 1 }])
  const all = await p.furnace('t', { pos: at, op: 'take', input: true, fuel: true })
  assert.deepEqual(all.taken.map(t => t.part), ['input', 'fuel'])
  assert.deepEqual(carried(p), { raw_iron: 3, beef: 5, coal: 2, oak_planks: 4, iron_ingot: 2 })
})

test('take with no room in the pockets leaves the output where it is', async () => {
  const filler = Array.from({ length: 34 }, (_, i) => ({ name: `item_${i}`, count: 1 }))
  const p = owned({ inventory: [...filler, { name: 'raw_iron', count: 1 }, { name: 'coal', count: 2 }] })
  await load(p, { item: 'raw_iron', count: 1 }, { item: 'coal', count: 1 })
  p.world.state.inventory.push({ name: 'dirt', count: 1 })
  p.world.advance(200)
  const r = await p.furnace('t', { pos: at, op: 'take' })
  assert.deepEqual([r.status, r.taken, r.output], ['full', [], { name: 'iron_ingot', count: 1 }])
})

const refusals = [
  { label: 'no block there', args: { pos: { x: 2, y: 64, z: 0 }, op: 'read' }, expect: { status: 'missing' } },
  { label: 'a chest', blocks: { [spot]: 'chest' }, args: { op: 'read' }, expect: { status: 'cannot', reason: 'not-a-furnace' } },
  { label: 'too far', args: { pos: { x: 9, y: 64, z: 0 }, op: 'read' }, blocks: { '9,64,0': 'furnace' }, expect: { status: 'unreachable', reason: 'too-far', distance: 9 } },
  { label: 'an item not carried', args: { op: 'load', input: { item: 'cobblestone', count: 1 } }, expect: { status: 'no-item', slot: 'input', item: 'cobblestone' } },
  { label: 'beef is given as fuel', args: { op: 'load', fuel: { item: 'beef', count: 1 } }, expect: { status: 'rejected', slot: 'fuel', item: 'beef', reason: 'not-accepted' } }
]
for (const r of refusals) {
  test(`refused as data: ${r.label}`, async () => {
    const p = owned({ inventory: stock, ...(r.blocks && { blocks: r.blocks }) })
    const before = carried(p)
    assert.deepEqual(await p.furnace('t', { pos: at, ...r.args }), r.expect)
    assert.deepEqual(carried(p), before)
  })
}

test('a slot that holds another item is busy', async () => {
  const p = owned({ inventory: stock })
  await load(p, { item: 'beef', count: 2 })
  assert.deepEqual(await load(p, { item: 'raw_iron', count: 1 }), { status: 'busy', slot: 'input', holds: { name: 'beef', count: 2 } })
})

test('a furnace can start with stacks in it', async () => {
  const p = owned({ furnaces: { [spot]: { input: { name: 'raw_iron', count: 2 }, fuel: { name: 'coal', count: 1 } } } })
  p.world.advance(200)
  assert.deepEqual((await read(p)).output, { name: 'iron_ingot', count: 1 })
})

test('a held call rejects with cut when the owner changes', async () => {
  const p = owned({})
  p.world.hold('furnace')
  const pending = read(p)
  p.setOwner('u')
  await assert.rejects(pending, { code: 'cut' })
})
