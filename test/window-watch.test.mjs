import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { watchWindows, containerBlocks, LINGER_MS } from '../src/body/window-watch.mjs'

const window = (type, slots, inventoryStart, title = '{"translate":"container.chest"}') => Object.assign(new EventEmitter(), { type, title, slots, inventoryStart })
const bread = { name: 'bread', count: 3 }
const setup = () => {
  const bot = new EventEmitter()
  let t = 1000
  const clock = { now: () => t, tick: ms => { t += ms } }
  const open = watchWindows(bot, { nearest: () => ({ x: 96, y: 70, z: -79 }), now: clock.now })
  return { bot, clock, open }
}

test('no window: nothing', () => assert.equal(setup().open(), null))

test('a chest opens: its stacks by container slot, where it stands, how big it is', () => {
  const { bot, open } = setup()
  // a real generic_9x3 window is 63 slots (27 container + 36 player): 24 nulls, not 25, puts dirt at the container's last slot (26)
  bot.emit('windowOpen', window('minecraft:generic_9x3', [null, bread, ...Array(24).fill(null), { name: 'dirt', count: 64 }, ...Array(36).fill({ name: 'mine', count: 1 })], 27))
  assert.deepEqual(open(), { type: 'minecraft:generic_9x3', title: 'chest', at: { x: 96, y: 70, z: -79 }, size: 27, open: true, closedAt: null, slots: [{ slot: 1, name: 'bread', count: 3 }, { slot: 26, name: 'dirt', count: 64 }] })
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

for (const [type, name, matches] of [
  ['minecraft:generic_9x3', 'chest', true], ['minecraft:generic_9x3', 'barrel', true], ['minecraft:generic_9x3', 'red_shulker_box', true],
  ['minecraft:generic_9x6', 'chest', true], ['minecraft:generic_9x6', 'barrel', false], ['minecraft:furnace', 'furnace', true],
  ['minecraft:blast_furnace', 'furnace', false], ['minecraft:smoker', 'smoker', true], ['minecraft:hopper', 'hopper', true],
  ['minecraft:generic_3x3', 'dropper', true], ['minecraft:crafting', 'crafting_table', true]
]) test(`containerBlocks: ${type} ${matches ? 'is' : 'is not'} a ${name}`, () => assert.equal(containerBlocks(type).test(name), matches))
test('containerBlocks: an unknown window has no block', () => assert.equal(containerBlocks('minecraft:beacon'), null))
