import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createPrimitivesFromBot } from './primitives.mjs'
import { stubBot, names, Vec3 } from './stub-bot.mjs'

const SCALE = 0.01
const at = (x, y, z) => ({ x, y, z })
const hoe = { name: 'diamond_hoe', count: 1, slot: 36 }
const key = (x, y, z) => `${x},${y},${z}`

const rig = (spec) => {
  const bot = stubBot({ blocks: { '1,64,0': 'dirt' }, items: [hoe], ...spec })
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  p.setOwner('t1')
  return { bot, p }
}
const calls = (bot, name) => bot.calls.filter(c => c.name === name)
const dirt = { pos: at(1, 64, 0) }

test('a hoe on dirt that the world turns into farmland is used', async () => {
  const blocks = { '1,64,0': 'dirt' }
  const { p } = rig({ blocks, onUseBlock: () => { blocks[key(1, 64, 0)] = 'farmland' } })
  const r = await p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  assert.deepEqual(r, { status: 'used', before: { name: 'dirt', properties: {} }, after: { name: 'farmland', properties: {} }, consumed: 0 })
})

test('nothing changing is unchanged', async () => {
  const { p } = rig()
  const r = await p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  assert.deepEqual(r, { status: 'unchanged', before: { name: 'dirt', properties: {} }, after: { name: 'dirt', properties: {} }, consumed: 0 })
})

test('an item eaten while the properties change reports consumed', async () => {
  const items = [{ name: 'wheat', count: 3, slot: 36 }]
  const props = { [key(1, 64, 0)]: { level: '1' } }
  const { p } = rig({
    blocks: { '1,64,0': 'composter' },
    props,
    items,
    onUseBlock: () => { props[key(1, 64, 0)] = { level: '2' }; items[0].count -= 1 }
  })
  const r = await p.useOn('t1', { ...dirt, item: 'wheat' })
  assert.deepEqual(r, { status: 'used', before: { name: 'composter', properties: { level: 1 } }, after: { name: 'composter', properties: { level: 2 } }, consumed: 1 })
})

test('an inventory update arriving shortly after the block update still reports consumed', async () => {
  const items = [{ name: 'wheat', count: 3, slot: 36 }]
  const props = { [key(1, 64, 0)]: { level: '1' } }
  const { p } = rig({
    blocks: { '1,64,0': 'composter' },
    props,
    items,
    onUseBlock: () => { props[key(1, 64, 0)] = { level: '2' }; setTimeout(() => { items[0].count -= 1 }, 1) }
  })
  const r = await p.useOn('t1', { ...dirt, item: 'wheat' })
  assert.deepEqual(r, { status: 'used', before: { name: 'composter', properties: { level: 1 } }, after: { name: 'composter', properties: { level: 2 } }, consumed: 1 })
})

test('an item consumed with the block unchanged is still used', async () => {
  const items = [{ name: 'bone_meal', count: 3, slot: 36 }]
  const { p } = rig({ items, onUseBlock: () => { items[0].count -= 1 } })
  const r = await p.useOn('t1', { ...dirt, item: 'bone_meal' })
  assert.equal(r.status, 'used')
  assert.equal(r.consumed, 1)
})

const refusals = [
  { name: 'air', spec: {}, args: { pos: at(5, 64, 0), item: 'diamond_hoe' }, expect: { status: 'missing' } },
  { name: 'no item', spec: {}, args: { ...dirt, item: 'shears' }, expect: { status: 'no-item', consumed: 0 } },
  { name: 'too far', spec: { blocks: { '1,64,9': 'dirt' } }, args: { pos: at(1, 64, 9), item: 'diamond_hoe' }, expect: { status: 'unreachable', reason: 'too-far', distance: 9.68, consumed: 0 } }
]
for (const c of refusals) {
  test(`${c.name}: ${c.expect.status} and activateBlock is never called`, async () => {
    const { bot, p } = rig(c.spec)
    const r = await p.useOn('t1', c.args)
    assert.deepEqual({ ...r, before: undefined, after: undefined }, { ...c.expect, before: undefined, after: undefined })
    assert.equal(calls(bot, 'activateBlock').length, 0)
  })
}

test('an item click equips the stack to the hand', async () => {
  const { bot, p } = rig()
  await p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  assert.deepEqual(calls(bot, 'equip')[0].args, [hoe, 'hand'])
})

const hotbarFull = Array.from({ length: 9 }, (_, i) => ({ name: 'stone', count: 1, slot: 36 + i }))
const hands = [
  { name: 'nothing held: no hand change', held: null, items: [], free: 9, want: [] },
  { name: 'held with a free hotbar slot: setQuickBarSlot', held: hoe, items: [hoe], free: 9, want: ['setQuickBarSlot'] },
  { name: 'held with a full hotbar but a free slot: unequip', held: hotbarFull[0], items: hotbarFull, free: 9, want: ['unequip'] },
  { name: 'held with a full inventory: no-room', held: hotbarFull[0], items: hotbarFull, free: null, want: [], status: 'no-room' }
]
for (const c of hands) {
  test(`empty hand, ${c.name}`, async () => {
    const { bot, p } = rig({ items: c.items, freeSlot: c.free })
    bot.heldItem = c.held
    const r = await p.useOn('t1', dirt)
    const hand = names(bot).filter(n => n === 'setQuickBarSlot' || n === 'unequip' || n === 'equip')
    assert.deepEqual(hand, c.want)
    assert.equal(r.status, c.status ?? 'unchanged')
    assert.equal(calls(bot, 'activateBlock').length, c.status ? 0 : 1)
  })
}

test('empty hand picks the first empty hotbar slot', async () => {
  const items = [{ name: 'stone', count: 1, slot: 36 }, { name: 'stone', count: 1, slot: 37 }, hoe]
  const { bot, p } = rig({ items: [...items.slice(0, 2), { ...hoe, slot: 39 }] })
  bot.heldItem = items[0]
  await p.useOn('t1', dirt)
  assert.deepEqual(calls(bot, 'setQuickBarSlot')[0].args, [2])
})

const faces = [
  { face: undefined, vec: [0, 1, 0], cursor: [0.5, 1, 0.5] },
  { face: 'up', vec: [0, 1, 0], cursor: [0.5, 1, 0.5] },
  { face: 'down', vec: [0, -1, 0], cursor: [0.5, 0, 0.5] },
  { face: 'north', vec: [0, 0, -1], cursor: [0.5, 0.5, 0] },
  { face: 'south', vec: [0, 0, 1], cursor: [0.5, 0.5, 1] },
  { face: 'west', vec: [-1, 0, 0], cursor: [0, 0.5, 0.5] },
  { face: 'east', vec: [1, 0, 0], cursor: [1, 0.5, 0.5] }
]
for (const c of faces) {
  test(`face ${c.face} passes its vector and cursor and looks at that face`, async () => {
    const { bot, p } = rig()
    await p.useOn('t1', { ...dirt, item: 'diamond_hoe', face: c.face })
    const [block, face, cursor] = calls(bot, 'activateBlock')[0].args
    assert.deepEqual([block.name, face, cursor], ['dirt', new Vec3(...c.vec), new Vec3(...c.cursor)])
    const [point, force] = calls(bot, 'lookAt')[0].args
    assert.deepEqual([point, force], [new Vec3(1 + c.cursor[0], 64 + c.cursor[1], c.cursor[2]), true])
  })
}

const bad = [
  { name: 'no pos', args: { item: 'diamond_hoe' } },
  { name: 'pos with a string', args: { pos: { x: '1', y: 64, z: 0 } } },
  { name: 'item not a string', args: { ...dirt, item: 5 } },
  { name: 'unknown face', args: { ...dirt, face: 'left' } }
]
for (const c of bad) {
  test(`bad args (${c.name}) reject with bad-args and never touch the bot`, async () => {
    const { bot, p } = rig()
    await assert.rejects(p.useOn('t1', c.args), { code: 'bad-args' })
    assert.deepEqual(names(bot), [])
  })
}

test('a stale token rejects with cut', async () => {
  const { bot, p } = rig()
  await assert.rejects(p.useOn('old', dirt), { code: 'cut' })
  assert.deepEqual(names(bot), [])
})

test('a cut while activateBlock hangs rejects with cut', async () => {
  const { p } = rig({ hang: ['activateBlock'] })
  const call = p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  await new Promise(r => setTimeout(r, 5))
  p.setOwner('t2')
  await assert.rejects(call, { code: 'cut' })
})

test('blockAt and blocks report properties when present and omit them when empty', () => {
  const { p } = rig({ blocks: { '1,64,0': 'composter', '2,64,0': 'dirt' }, props: { [key(1, 64, 0)]: { level: '3' } } })
  assert.deepEqual(p.blockAt(at(1, 64, 0)).properties, { level: 3 })
  assert.equal('properties' in p.blockAt(at(2, 64, 0)), false)
  const found = p.blocks({ names: ['composter', 'dirt'], properties: true })
  assert.deepEqual(found.map(b => [b.name, b.properties]), [['composter', { level: 3 }], ['dirt', undefined]])
  assert.equal('properties' in found.find(b => b.name === 'dirt'), false)
})

test('blocks omits properties unless asked; blockAt always has them; age stays on both', () => {
  const { p } = rig({ blocks: { '1,64,0': 'wheat' }, props: { [key(1, 64, 0)]: { age: '7' } } })
  const plain = p.blocks({ names: ['wheat'] })[0]
  assert.equal('properties' in plain, false)
  assert.equal(plain.age, 7)
  assert.deepEqual(p.blocks({ names: ['wheat'], properties: true })[0].properties, { age: 7 })
  assert.deepEqual(p.blockAt(at(1, 64, 0)).properties, { age: 7 })
})

test('blockAt keeps age as a number next to properties', () => {
  const { p } = rig({ blocks: { '1,64,0': 'wheat' }, props: { [key(1, 64, 0)]: { age: '7' } } })
  const b = p.blockAt(at(1, 64, 0))
  assert.deepEqual([b.age, b.properties], [7, { age: 7 }])
})

const rawProps = { level: '8', age: '4', powered: false, facing: 'north' }
const typedProps = { level: 8, age: 4, powered: false, facing: 'north' }

test('blockAt and blocks report integer states as numbers', () => {
  const { p } = rig({ blocks: { '1,64,0': 'composter' }, props: { [key(1, 64, 0)]: rawProps } })
  assert.deepEqual(p.blockAt(at(1, 64, 0)).properties, typedProps)
  assert.deepEqual(p.blocks({ names: ['composter'], properties: true })[0].properties, typedProps)
})

test('useOn before and after report integer states as numbers', async () => {
  const { p } = rig({ blocks: { '1,64,0': 'composter' }, props: { [key(1, 64, 0)]: rawProps } })
  const r = await p.useOn('t1', dirt)
  assert.deepEqual([r.before.properties, r.after.properties], [typedProps, typedProps])
})

test('a tool with durability never waits for a count change after the block changed', async () => {
  const props = {}
  const { bot, p } = rig({ onUseBlock: () => { props.done = true } })
  bot.registry.itemsByName.diamond_hoe = { id: 9, maxDurability: 1561 }
  const blockAt = bot.blockAt
  let polls = 0
  bot.blockAt = v => {
    if (!props.done) return blockAt(v)
    polls += 1
    return { name: 'farmland', getProperties: () => ({}) }
  }
  const r = await p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  assert.equal(r.status, 'used')
  assert.equal(r.consumed, 0)
  assert.equal(polls, 3) // the change check, the status check and the after snapshot; no extra wait polls
})

const guardRefusals = [
  { name: 'a bed', block: 'red_bed', item: 'diamond_hoe', reason: 'bed' },
  { name: 'a respawn anchor', block: 'respawn_anchor', item: 'diamond_hoe', reason: 'bed' },
  { name: 'a chest', block: 'chest', item: 'diamond_hoe', reason: 'container' },
  { name: 'a crafting table by hand', block: 'crafting_table', item: undefined, reason: 'container' },
  { name: 'a furnace', block: 'furnace', item: 'diamond_hoe', reason: 'container' },
  { name: 'a lectern', block: 'lectern', item: 'diamond_hoe', reason: 'container' },
  { name: 'a crafter', block: 'crafter', item: 'diamond_hoe', reason: 'container' },
  { name: 'a command_block', block: 'command_block', item: 'diamond_hoe', reason: 'container' },
  { name: 'a chain_command_block', block: 'chain_command_block', item: 'diamond_hoe', reason: 'container' },
  { name: 'a repeating_command_block', block: 'repeating_command_block', item: 'diamond_hoe', reason: 'container' },
  { name: 'a structure_block', block: 'structure_block', item: 'diamond_hoe', reason: 'container' },
  { name: 'a jigsaw', block: 'jigsaw', item: 'diamond_hoe', reason: 'container' },
  { name: 'a vault', block: 'vault', item: 'diamond_hoe', reason: 'container' },
  { name: 'flint_and_steel', block: 'dirt', item: 'flint_and_steel', reason: 'hazard' },
  { name: 'fire_charge', block: 'dirt', item: 'fire_charge', reason: 'hazard' },
  { name: 'lava_bucket', block: 'dirt', item: 'lava_bucket', reason: 'hazard' },
  { name: 'a block item on dirt', block: 'dirt', item: 'cobblestone', reason: 'use-place' }
]
for (const c of guardRefusals) {
  test(`guard: ${c.name} is cannot/${c.reason}, nothing equipped or clicked`, async () => {
    const stack = { name: c.item ?? 'diamond_hoe', count: 1, slot: 36 }
    const { bot, p } = rig({ blocks: { '1,64,0': c.block }, items: [stack] })
    bot.registry.blocksByName = { cobblestone: { id: 4 }, dirt: { id: 3 } }
    const r = await p.useOn('t1', { pos: at(1, 64, 0), item: c.item })
    assert.deepEqual(r, { status: 'cannot', reason: c.reason, before: { name: c.block, properties: {} }, after: { name: c.block, properties: {} }, consumed: 0 })
    assert.equal(calls(bot, 'activateBlock').length, 0)
    assert.equal(calls(bot, 'equip').length, 0)
  })
}

test('guard: a composter still accepts a block item (oak_leaves)', async () => {
  const items = [{ name: 'oak_leaves', count: 3, slot: 36 }]
  const props = { [key(1, 64, 0)]: { level: '1' } }
  const { bot, p } = rig({ blocks: { '1,64,0': 'composter' }, props, items, onUseBlock: () => { props[key(1, 64, 0)] = { level: '2' }; items[0].count -= 1 } })
  bot.registry.blocksByName = { oak_leaves: { id: 5 } }
  const r = await p.useOn('t1', { ...dirt, item: 'oak_leaves' })
  assert.equal(r.status, 'used')
  assert.equal(r.consumed, 1)
})

test('window guard: a window opened by the click is closed and reported', async () => {
  const win = { title: 'w' }
  const { bot, p } = rig({ onUseBlock: bot => { bot.emit('windowOpen', win) } })
  const r = await p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  assert.deepEqual(r, { status: 'cannot', reason: 'window', before: { name: 'dirt', properties: {} }, after: { name: 'dirt', properties: {} }, consumed: 0 })
  assert.deepEqual(calls(bot, 'closeWindow').map(c => c.args), [[win]])
  assert.equal(bot.listenerCount('windowOpen'), 0)
})

test('window guard: bot.currentWindow set after the click counts too', async () => {
  const win = { title: 'w' }
  const { bot, p } = rig({ onUseBlock: bot => { bot.currentWindow = win } })
  const r = await p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  assert.equal(r.reason, 'window')
  assert.deepEqual(calls(bot, 'closeWindow').map(c => c.args), [[win]])
})

test('window guard: a window opening after activateBlock returned (during the poll) is closed and reported', async () => {
  const win = { title: 'late' }
  const { bot, p } = rig({ onUseBlock: bot => { setTimeout(() => bot.emit('windowOpen', win), 1) } })
  const r = await p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  assert.equal(r.status, 'cannot')
  assert.equal(r.reason, 'window')
  assert.deepEqual(calls(bot, 'closeWindow').map(c => c.args), [[win]])
  assert.equal(bot.listenerCount('windowOpen'), 0)
})

test('window guard: no window leaves no listener behind', async () => {
  const { bot, p } = rig()
  await p.useOn('t1', { ...dirt, item: 'diamond_hoe' })
  assert.equal(bot.listenerCount('windowOpen'), 0)
})

test('too far: unreachable carries reason too-far and the eye-to-centre distance', async () => {
  const { bot, p } = rig({ blocks: { '1,64,9': 'dirt' } })
  const r = await p.useOn('t1', { pos: at(1, 64, 9), item: 'diamond_hoe' })
  const eyeY = 64 + (bot.entity.height ?? 1.62)
  const want = Math.round(Math.hypot(bot.entity.position.x - 1.5, eyeY - 64.5, bot.entity.position.z - 9.5) * 100) / 100
  assert.deepEqual([r.status, r.reason, r.distance], ['unreachable', 'too-far', want])
})
