import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake, CutError } from './fake.mjs'

const at = (x, y, z) => ({ x, y, z })
const P = at(1, 64, 0)
const owned = (spec = {}) => {
  const p = createFake({ self: { pos: at(0, 64, 0) }, ...spec })
  p.setOwner('t')
  return p
}
const dropsOf = (p) => p.world.state.entities.filter(e => e.kind === 'item').map(e => [e.item.name, e.item.count, e.pos])

test('air or an absent block is missing and carries only the status', async () => {
  assert.deepEqual(await owned().useOn('t', { pos: P }), { status: 'missing' })
})

test('an item that is not carried is no-item and nothing changes', async () => {
  const p = owned({ blocks: { '1,64,0': 'dirt' } })
  assert.deepEqual(await p.useOn('t', { pos: P, item: 'bone_meal' }), {
    status: 'no-item', before: { name: 'dirt', properties: {} }, after: { name: 'dirt', properties: {} }, consumed: 0
  })
})

test('a block out of reach is unreachable', async () => {
  const p = owned({ blocks: { '9,64,0': 'dirt' }, inventory: [{ name: 'iron_hoe', count: 1 }] })
  const r = await p.useOn('t', { pos: at(9, 64, 0), item: 'iron_hoe' })
  assert.equal(r.status, 'unreachable')
  assert.equal(r.consumed, 0)
  assert.equal(p.world.state.blocks.get('9,64,0'), 'dirt')
})

test('the item becomes held, an empty hand holds nothing', async () => {
  const p = owned({ blocks: { '1,64,0': 'stone' }, inventory: [{ name: 'bone_meal', count: 1 }], self: { held: 'bread' } })
  await p.useOn('t', { pos: P, item: 'bone_meal' })
  assert.equal(p.self().held, 'bone_meal')
  await p.useOn('t', { pos: P })
  assert.equal(p.self().held, null)
})

for (const [hoe, ground] of [['wooden_hoe', 'dirt'], ['iron_hoe', 'grass_block'], ['diamond_hoe', 'dirt_path']]) {
  test(`${hoe} on ${ground} with air above makes farmland`, async () => {
    const p = owned({ blocks: { '1,64,0': ground }, inventory: [{ name: hoe, count: 1 }] })
    const r = await p.useOn('t', { pos: P, item: hoe })
    assert.deepEqual(r, { status: 'used', before: { name: ground, properties: {} }, after: { name: 'farmland', properties: {} }, consumed: 0 })
    assert.equal(p.blockAt(P).name, 'farmland')
  })
}

for (const [label, blocks, face] of [
  ['stone', { '1,64,0': 'stone' }, 'up'],
  ['dirt with a block above', { '1,64,0': 'dirt', '1,65,0': 'stone' }, 'up'],
  ['dirt from below', { '1,64,0': 'dirt' }, 'down']
]) {
  test(`a hoe on ${label} is unchanged`, async () => {
    const p = owned({ blocks, inventory: [{ name: 'iron_hoe', count: 1 }] })
    const r = await p.useOn('t', { pos: P, item: 'iron_hoe', face })
    assert.equal(r.status, 'unchanged')
    assert.equal(r.consumed, 0)
    assert.equal(p.world.state.blocks.get('1,64,0'), blocks['1,64,0'])
  })
}

for (const [crop, age, status, after, consumed, left] of [
  ['wheat', 0, 'used', 2, 1, 2],
  ['wheat', 6, 'used', 7, 1, 2],
  ['carrots', 5, 'used', 7, 1, 2],
  ['potatoes', 7, 'unchanged', 7, 0, 3],
  ['beetroots', 0, 'used', 2, 1, 2],
  ['beetroots', 2, 'used', 3, 1, 2],
  ['beetroots', 3, 'unchanged', 3, 0, 3]
]) {
  test(`bone meal on ${crop} age ${age} is ${status}`, async () => {
    const p = owned({ blocks: { '1,64,0': crop }, ages: { '1,64,0': age }, inventory: [{ name: 'bone_meal', count: 3 }] })
    const r = await p.useOn('t', { pos: P, item: 'bone_meal' })
    assert.equal(r.status, status)
    assert.equal(r.before.properties.age, age)
    assert.equal(r.after.properties.age, after)
    assert.equal(r.consumed, consumed)
    assert.equal(p.blockAt(P).age, after)
    assert.equal(p.self().inventory[0].count, left)
  })
}

for (const block of ['oak_sapling', 'birch_sapling', 'grass_block']) {
  test(`bone meal on ${block} is used and leaves the block`, async () => {
    const p = owned({ blocks: { '1,64,0': block }, inventory: [{ name: 'bone_meal', count: 1 }] })
    const r = await p.useOn('t', { pos: P, item: 'bone_meal' })
    assert.equal(r.status, 'used')
    assert.equal(r.consumed, 1)
    assert.equal(p.blockAt(P).name, block)
    assert.deepEqual(p.self().inventory, [])
  })
}

test('an emptied composter spits bone meal above and resets', async () => {
  const p = owned({ blocks: { '1,64,0': 'composter' }, states: { '1,64,0': { level: 8 } } })
  const r = await p.useOn('t', { pos: P })
  assert.deepEqual(r, { status: 'used', before: { name: 'composter', properties: { level: 8 } }, after: { name: 'composter', properties: { level: 0 } }, consumed: 0 })
  assert.deepEqual(dropsOf(p), [['bone_meal', 1, at(1, 65, 0)]])
})

test('a full composter empties for a hand holding something too', async () => {
  const p = owned({ blocks: { '1,64,0': 'composter' }, states: { '1,64,0': { level: 8 } }, inventory: [{ name: 'wheat', count: 2 }] })
  const r = await p.useOn('t', { pos: P, item: 'wheat' })
  assert.equal(r.status, 'used')
  assert.equal(r.consumed, 0)
  assert.equal(dropsOf(p).length, 1)
  assert.equal(p.self().inventory[0].count, 2)
})

for (const [level, states, after] of [[undefined, {}, 1], [0, { '1,64,0': { level: 0 } }, 1], [3, { '1,64,0': { level: 3 } }, 4], [6, { '1,64,0': { level: 6 } }, 8]]) {
  test(`composting wheat_seeds at level ${level} gives ${after}`, async () => {
    const p = owned({
      blocks: { '1,64,0': 'composter' },
      states,
      inventory: [{ name: 'wheat_seeds', count: 2 }]
    })
    const r = await p.useOn('t', { pos: P, item: 'wheat_seeds' })
    assert.equal(r.status, 'used')
    assert.equal(r.consumed, 1)
    assert.equal(r.after.properties.level, after)
    assert.equal(p.self().inventory[0].count, 1)
    assert.deepEqual(dropsOf(p), [])
  })
}

for (const [label, level, args] of [
  ['level 7 with seeds', 7, { item: 'wheat_seeds' }],
  ['a non-compostable item', 2, { item: 'stick' }],
  ['an empty hand', 2, {}]
]) {
  test(`composter ${label} is unchanged`, async () => {
    const p = owned({ blocks: { '1,64,0': 'composter' }, states: { '1,64,0': { level } }, inventory: [{ name: 'wheat_seeds', count: 1 }, { name: 'stick', count: 1 }] })
    const r = await p.useOn('t', { pos: P, ...args })
    assert.equal(r.status, 'unchanged')
    assert.equal(r.consumed, 0)
    assert.equal(r.after.properties.level, level)
    assert.equal(p.self().inventory.length, 2)
  })
}

test('anything else is unchanged', async () => {
  const p = owned({ blocks: { '1,64,0': 'stone' }, inventory: [{ name: 'apple', count: 1 }] })
  const r = await p.useOn('t', { pos: P, item: 'apple' })
  assert.deepEqual(r, { status: 'unchanged', before: { name: 'stone', properties: {} }, after: { name: 'stone', properties: {} }, consumed: 0 })
})

test('blockAt and blocks show properties for a composter, and none for plain blocks', () => {
  const p = owned({ blocks: { '1,64,0': 'composter', '2,64,0': 'stone', '3,64,0': 'wheat' }, states: { '1,64,0': { level: 3 } }, ages: { '3,64,0': 4 } })
  assert.deepEqual(p.blockAt(P).properties, { level: 3 })
  assert.equal('properties' in p.blockAt(at(2, 64, 0)), false)
  assert.deepEqual(p.blockAt(at(3, 64, 0)), { name: 'wheat', pos: at(3, 64, 0), age: 4, properties: { age: 4 } })
  assert.deepEqual(p.blocks({ names: ['composter'], properties: true })[0].properties, { level: 3 })
  assert.equal('properties' in p.blocks({ names: ['composter'] })[0], false)
  assert.equal(p.blocks({ names: ['wheat'] })[0].age, 4)
  assert.deepEqual(p.blocks({ names: ['wheat'], properties: true })[0].properties, { age: 4 })
})

test('digging a block drops its states', async () => {
  const p = owned({ blocks: { '1,64,0': 'composter' }, states: { '1,64,0': { level: 3 } } })
  await p.dig('t', { pos: P })
  assert.equal(p.world.state.states.has('1,64,0'), false)
})

test('a cut owner rejects useOn like other acts', async () => {
  const p = owned({ blocks: { '1,64,0': 'dirt' } })
  p.setOwner('other')
  await assert.rejects(p.useOn('t', { pos: P }), CutError)
})

test('world.calls records useOn', async () => {
  const p = owned({ blocks: { '1,64,0': 'dirt' } })
  await p.useOn('t', { pos: P, face: 'north' })
  assert.deepEqual(p.world.calls, [{ name: 'useOn', token: 't', args: { pos: P, face: 'north' } }])
})

for (const [label, block, item, reason] of [
  ['a bed', 'white_bed', 'iron_hoe', 'bed'],
  ['a respawn anchor', 'respawn_anchor', 'iron_hoe', 'bed'],
  ['a chest', 'chest', 'iron_hoe', 'container'],
  ['a crafting table', 'crafting_table', 'iron_hoe', 'container'],
  ['a barrel', 'barrel', 'iron_hoe', 'container'],
  ['a crafter', 'crafter', 'iron_hoe', 'container'],
  ['a command_block', 'command_block', 'iron_hoe', 'container'],
  ['a chain_command_block', 'chain_command_block', 'iron_hoe', 'container'],
  ['a repeating_command_block', 'repeating_command_block', 'iron_hoe', 'container'],
  ['a structure_block', 'structure_block', 'iron_hoe', 'container'],
  ['a jigsaw', 'jigsaw', 'iron_hoe', 'container'],
  ['a vault', 'vault', 'iron_hoe', 'container'],
  ['flint_and_steel', 'dirt', 'flint_and_steel', 'hazard'],
  ['fire_charge', 'dirt', 'fire_charge', 'hazard'],
  ['lava_bucket', 'dirt', 'lava_bucket', 'hazard'],
  ['a block item', 'dirt', 'cobblestone', 'use-place'],
  ['oak_planks', 'stone', 'oak_planks', 'use-place']
]) {
  test(`guard: ${label} is cannot/${reason} and the world is unchanged`, async () => {
    const p = owned({ blocks: { '1,64,0': block }, inventory: [{ name: item, count: 2 }] })
    const r = await p.useOn('t', { pos: P, item })
    assert.deepEqual(r, { status: 'cannot', reason, before: { name: block, properties: {} }, after: { name: block, properties: {} }, consumed: 0 })
    assert.equal(p.blockAt(P).name, block)
    assert.equal(p.self().inventory[0].count, 2)
  })
}

test('guard: a composter still accepts oak_leaves', async () => {
  const p = owned({ blocks: { '1,64,0': 'composter' }, inventory: [{ name: 'oak_leaves', count: 2 }] })
  const r = await p.useOn('t', { pos: P, item: 'oak_leaves' })
  assert.equal(r.status, 'used')
  assert.equal(r.consumed, 1)
})

test('too far: unreachable carries reason too-far and the distance', async () => {
  const p = owned({ blocks: { '9,64,0': 'dirt' }, inventory: [{ name: 'iron_hoe', count: 1 }] })
  const r = await p.useOn('t', { pos: at(9, 64, 0), item: 'iron_hoe' })
  assert.deepEqual([r.status, r.reason, r.distance], ['unreachable', 'too-far', 9.53])
})
