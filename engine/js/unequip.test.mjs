// Why JavaScript: tests unequip.mjs, which stays JS: Mineflayer boundary; calls bot.unequip.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { emptyHand, emptySlot, hasRoom } from './unequip.mjs'

const ctx = { alive: () => {} }
const emptyOffHand = (bot, c) => emptySlot(bot, c, 'off-hand')
const stack = name => ({ name, count: 1 })
const full = (from, to) => Object.fromEntries(Array.from({ length: to - from }, (_, i) => [from + i, stack('dirt')]))

// a hand-made bot: `slots` is the slot map, `free` what firstEmptyInventorySlot answers
const makeBot = ({ held = null, slots = {}, free = null, keeps = false } = {}) => {
  const bot = {
    calls: [],
    heldItem: held,
    inventory: { slots, firstEmptyInventorySlot: () => free },
    unequip: async where => { bot.calls.push(where); keeps || (bot.heldItem = null) }
  }
  return bot
}

const rows = [
  { label: 'empty hand', bot: {}, expect: { status: 'empty' }, calls: [] },
  { label: 'ok through an empty hotbar slot', bot: { held: stack('stick'), slots: full(36, 44) }, expect: { status: 'ok', item: 'stick' }, calls: ['hand'] },
  { label: 'ok through a free main slot', bot: { held: stack('stick'), slots: full(36, 45), free: 12 }, expect: { status: 'ok', item: 'stick' }, calls: ['hand'] },
  { label: 'full', bot: { held: stack('stick'), slots: full(36, 45) }, expect: { status: 'full' }, calls: [] },
  { label: 'still held', bot: { held: stack('stick'), keeps: true }, expect: { status: 'failed', reason: 'still-held' }, calls: ['hand'] }
]

for (const r of rows) {
  test(`emptyHand: ${r.label}`, async () => {
    const bot = makeBot(r.bot)
    assert.deepEqual(await emptyHand(bot, ctx), r.expect)
    assert.deepEqual(bot.calls, r.calls)
  })
}

test('hasRoom reads the hotbar then the main slots', () => {
  assert.equal(hasRoom(makeBot({ slots: full(36, 45) })), false)
  assert.equal(hasRoom(makeBot({ slots: full(36, 45), free: 9 })), true)
  assert.equal(hasRoom(makeBot({ slots: full(36, 44) })), true)
})

test('emptyOffHand puts slot 45 back when there is room, and says full otherwise', async () => {
  const bot = makeBot({ slots: { 45: stack('torch') } })
  bot.unequip = async where => { bot.calls.push(where); delete bot.inventory.slots[45] }
  assert.deepEqual(await emptyOffHand(bot, ctx), { status: 'ok', item: 'torch' })
  assert.deepEqual(bot.calls, ['off-hand'])
  assert.deepEqual(await emptyOffHand(makeBot(), ctx), { status: 'empty' })
  const packed = makeBot({ slots: { ...full(36, 45), 45: stack('torch') } })
  assert.deepEqual(await emptyOffHand(packed, ctx), { status: 'full' })
  assert.deepEqual(packed.calls, [])
})

test('emptyOffHand counts an unfilled stack of the same item as room, as putAway merges into it', async () => {
  const torches = (count) => ({ name: 'torch', count, stackSize: 64 })
  const bot = makeBot({ slots: { ...full(36, 45), 20: torches(10), 45: torches(4) } })
  bot.unequip = async where => { bot.calls.push(where); delete bot.inventory.slots[45] }
  assert.deepEqual(await emptyOffHand(bot, ctx), { status: 'ok', item: 'torch' })
  assert.deepEqual(bot.calls, ['off-hand'])
  const filled = makeBot({ slots: { ...full(36, 45), 20: torches(64), 45: torches(4) } })
  assert.deepEqual(await emptyOffHand(filled, ctx), { status: 'full' })
  assert.deepEqual(filled.calls, [])
})

test('emptyOffHand says full when only part of the stack merged and the rest stays in the off hand', async () => {
  const bot = makeBot({ slots: { ...full(36, 45), 20: { name: 'torch', count: 62, stackSize: 64 }, 45: { name: 'torch', count: 4, stackSize: 64 } } })
  bot.unequip = async where => { bot.calls.push(where); bot.inventory.slots[45] = { name: 'torch', count: 2, stackSize: 64 } }
  assert.deepEqual(await emptyOffHand(bot, ctx), { status: 'full', kept: 2 })
})

test('emptySlot takes an armour piece off into a free slot and says full when the pockets are packed', async () => {
  const bot = makeBot({ slots: { 5: stack('iron_helmet') } })
  bot.unequip = async where => { bot.calls.push(where); delete bot.inventory.slots[5] }
  assert.deepEqual(await emptySlot(bot, ctx, 'head'), { status: 'ok', item: 'iron_helmet' })
  assert.deepEqual(bot.calls, ['head'])
  assert.deepEqual(await emptySlot(makeBot(), ctx, 'feet'), { status: 'empty' })
  const packed = makeBot({ slots: { ...full(36, 45), 8: stack('iron_boots') } })
  assert.deepEqual(await emptySlot(packed, ctx, 'feet'), { status: 'full' })
  assert.deepEqual(packed.calls, [])
})
