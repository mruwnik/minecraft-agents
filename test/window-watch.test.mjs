import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { watchWindows, LINGER_MS } from '../src/body/window-watch.mjs'

const window = (type, slots, inventoryStart, title = '{"translate":"container.chest"}') => Object.assign(new EventEmitter(), { type, title, slots, inventoryStart })
const bread = { name: 'bread', count: 3 }
const setup = () => {
  const bot = new EventEmitter()
  let t = 1000
  const clock = { now: () => t, tick: ms => { t += ms } }
  const { open, opening } = watchWindows(bot, { now: clock.now })
  return { bot, clock, open, opening }
}

test('no window: nothing', () => assert.equal(setup().open(), null))

test('a chest opens: its stacks by container slot, at the position the opener declared, how big it is', () => {
  const { bot, open, opening } = setup()
  // the opener (containerAt) declares the block it resolved and is about to open, before windowOpen fires
  opening({ x: 96, y: 70, z: -79 })
  // a real generic_9x3 window is 63 slots (27 container + 36 player): 24 nulls, not 25, puts dirt at the container's last slot (26)
  bot.emit('windowOpen', window('minecraft:generic_9x3', [null, bread, ...Array(24).fill(null), { name: 'dirt', count: 64 }, ...Array(36).fill({ name: 'mine', count: 1 })], 27))
  assert.deepEqual(open(), { type: 'minecraft:generic_9x3', title: 'chest', at: { x: 96, y: 70, z: -79 }, size: 27, open: true, closedAt: null, slots: [{ slot: 1, name: 'bread', count: 3 }, { slot: 26, name: 'dirt', count: 64 }] })
})

// the bug this guards against: a blind nearby-block scan reported a barrel when a chest and a barrel both stood
// within range of a chest the body actually opened. There is no scan any more - at is whatever was declared, or null
test('nobody declared a position: at is null, not a nearby guess', () => {
  const { bot, open } = setup()
  bot.emit('windowOpen', window('minecraft:generic_9x3', Array(63).fill(null), 27))
  assert.equal(open().at, null)
})

test('a declared position is used once and then cleared: a later window nobody declared gets null', () => {
  const { bot, open, opening } = setup()
  opening({ x: 1, y: 2, z: 3 })
  bot.emit('windowOpen', window('minecraft:furnace', Array(39).fill(null), 3))
  bot.emit('windowClose')
  bot.emit('windowOpen', window('minecraft:furnace', Array(39).fill(null), 3))
  assert.equal(open().at, null)
})

test('a slot changing while it is open changes the answer; one in my own inventory does not', () => {
  const { bot, open } = setup()
  const w = window('minecraft:hopper', [bread, null, null, null, null, ...Array(36).fill(null)], 5)
  bot.emit('windowOpen', w)
  w.slots[0] = null
  w.emit('updateSlot', 0, bread, null)
  assert.deepEqual(open().slots, [])
  w.slots[7] = bread
  w.emit('updateSlot', 7, null, bread)
  assert.deepEqual(open().slots, [])
})

test('closed: kept for LINGER_MS with the time it closed, then gone', () => {
  const { bot, clock, open } = setup()
  bot.emit('windowOpen', window('minecraft:furnace', [null, null, null, ...Array(36).fill(null)], 3))
  clock.tick(300)
  bot.emit('windowClose')
  assert.deepEqual([open().open, open().closedAt], [false, 1300])
  clock.tick(LINGER_MS - 1)
  assert.equal(open().open, false)
  clock.tick(1)
  assert.equal(open(), null)
})
