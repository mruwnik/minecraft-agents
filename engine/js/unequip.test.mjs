// Why JavaScript: tests unequip.mjs, which stays JS: Mineflayer boundary; calls bot.unequip.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { emptyHand, hasRoom } from './unequip.mjs'

const ctx = { alive: () => {} }
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
