// Why JavaScript: tests anvil.mjs, which stays JS: Mineflayer boundary; drives the anvil window.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { anvilVisit } from './anvil.mjs'

const ctx = { alive: () => {}, onAbort: () => {} }
const opts = { timeScale: 0.02, reach: 4.5, distanceTo: () => 2 }
const pos = { x: 3, y: 64, z: 0 }
const registry = { enchantments: { 21: { name: 'mending' }, 5: { name: 'sharpness' } } }

const tool = (enchants = []) => ({ name: 'iron_pickaxe', count: 1, enchants })
// on 26.1 a book's enchantments are in the stored_enchantments component and its enchants getter reads none
const book = (id = 21) => ({ name: 'enchanted_book', count: 1, enchants: [], componentMap: new Map([['stored_enchantments', { data: { enchantments: [{ id, level: 1 }] } }]]) })

// A hand-made bot and anvil window as mineflayer hands it out: slots 0 and 1 the inputs, 2 the result, 3..38 the
// pockets. Once both inputs lie in it the server sends the result and its level cost (window property 0) at once,
// before the move's promise settles; like the server it gives back what lies in the inputs when the window closes.
const makeBot = ({ block = 'anvil', pockets = [], level = 10, cost = 2, combines = true, windowType = 'minecraft:anvil' } = {}) => {
  const bot = new EventEmitter()
  const calls = []
  const places = Array.from({ length: 36 }, (_, i) => pockets[i] ?? null)
  const items = () => places.filter(Boolean)
  const inventory = Object.assign(new EventEmitter(), { items })
  const experience = { level }
  const win = { id: 7, type: windowType, inventoryStart: 3, inventoryEnd: 39, slots: Array(39).fill(null) }
  const sync = () => { for (let i = 0; i < 36; i++) win.slots[3 + i] = places[i] }
  sync()
  const client = new EventEmitter()
  const offer = () => {
    if (!(combines && win.slots[0] && win.slots[1])) return
    win.slots[2] = tool([{ name: 'mending', lvl: 1 }])
    client.emit('craft_progress_bar', { windowId: win.id, property: 0, value: cost })
  }
  return Object.assign(bot, {
    calls,
    registry,
    inventory,
    experience,
    _client: client,
    blockAt: p => (p.x === pos.x && p.y === pos.y && p.z === pos.z ? { name: block, position: p } : null),
    openBlock: async () => { calls.push('open'); return win },
    closeWindow: () => {
      calls.push('close')
      for (const i of [0, 1]) {
        if (!win.slots[i]) continue
        places[places.indexOf(null)] = win.slots[i]
        win.slots[i] = null
      }
      win.slots[2] = null
      sync()
    },
    moveSlotItem: async (from, to) => {
      calls.push(`move ${to}`)
      win.slots[to] = win.slots[from]
      places[from - 3] = null
      sync()
      offer()
    },
    putAway: async slot => {
      calls.push('take')
      places[places.indexOf(null)] = win.slots[slot]
      win.slots[0] = win.slots[1] = win.slots[2] = null
      experience.level -= cost
      sync()
    }
  })
}

const run = (bot, a = {}, o = opts) => anvilVisit(bot, ctx, { pos, item: 'iron_pickaxe', enchant: 'mending', ...a }, o)

test('refusals that need no window come back as data and open none', async t => {
  const rows = [
    ['the block is gone', { block: 'air' }, {}, opts, { status: 'missing' }],
    ['not an anvil', { block: 'crafting_table' }, {}, opts, { status: 'cannot', reason: 'not-an-anvil' }],
    ['too far', { pockets: [tool(), book()] }, {}, { ...opts, distanceTo: () => 9.5 }, { status: 'unreachable', reason: 'too-far', distance: 9.5 }],
    ['the item is not carried', { pockets: [book()] }, {}, opts, { status: 'no-item', item: 'iron_pickaxe' }],
    ['every copy has it already', { pockets: [tool([{ name: 'mending', lvl: 1 }]), book()] }, {}, opts, { status: 'cannot', reason: 'already-has', item: 'iron_pickaxe' }],
    ['no book holds the enchantment', { pockets: [tool(), book(5)] }, {}, opts, { status: 'no-book', enchant: 'mending' }]
  ]
  for (const [label, spec, a, o, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot(spec)
      assert.deepEqual(await run(bot, a, o), expected)
      assert.deepEqual(bot.calls, [])
    })
  }
})

test('a chipped anvil works like a new one, and the book is found by its stored enchantment', async () => {
  const bot = makeBot({ block: 'chipped_anvil', pockets: [tool(), book(5), book()], level: 5 })
  const r = await run(bot)
  assert.deepEqual(r, { status: 'combined', item: 'iron_pickaxe', enchant: 'mending', enchants: [{ name: 'mending', level: 1 }], levelCost: 2, levelsSpent: 2, xpLevel: 3 })
  assert.deepEqual(bot.calls, ['open', 'move 0', 'move 1', 'take', 'close'])
  assert.deepEqual(bot.inventory.items().map(i => i.name).sort(), ['enchanted_book', 'iron_pickaxe'], 'the other book stays, the mending one is used')
})

test('a cost above the level is refused and both inputs come back', async () => {
  const bot = makeBot({ pockets: [tool(), book()], level: 3, cost: 5 })
  assert.deepEqual(await run(bot), { status: 'no-levels', need: 5, have: 3 })
  assert.deepEqual(bot.calls, ['open', 'move 0', 'move 1', 'close'])
  assert.equal(bot.experience.level, 3)
  assert.deepEqual(bot.inventory.items().map(i => i.name).sort(), ['enchanted_book', 'iron_pickaxe'])
})

test('a pair the server does not combine, or one too expensive, is refused', async t => {
  const rows = [
    ['no result', { combines: false }, { status: 'cannot', reason: 'not-combinable' }],
    ['forty levels', { cost: 40, level: 50 }, { status: 'cannot', reason: 'too-expensive', need: 40 }]
  ]
  for (const [label, spec, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ pockets: [tool(), book()], ...spec })
      assert.deepEqual(await run(bot), expected)
      assert.equal(bot.calls.includes('take'), false)
    })
  }
})

test('a window of another kind is closed and the visit fails with the reason', async () => {
  const bot = makeBot({ pockets: [tool(), book()], windowType: 'minecraft:generic_9x3' })
  assert.deepEqual(await run(bot), { status: 'failed', reason: 'not-an-anvil-window' })
  assert.deepEqual(bot.calls, ['open', 'close'])
})
