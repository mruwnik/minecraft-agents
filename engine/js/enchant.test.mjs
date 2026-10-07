// Why JavaScript: tests enchant.mjs, which stays JS: Mineflayer boundary; drives the enchanting-table window.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import { enchantVisit } from './enchant.mjs'

const ctx = { alive: () => {}, onAbort: () => {} }
const opts = { timeScale: 0.02, reach: 4.5, distanceTo: () => 2 }
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))
const pos = { x: 3, y: 64, z: 0 }

const ENCHANTS = { 5: { name: 'sharpness' }, 8: { name: 'unbreaking' } }
const registry = { enchantments: ENCHANTS, itemsByName: { diamond_sword: { stackSize: 1 }, lapis_lazuli: { stackSize: 64 }, book: { stackSize: 64 } } }

const stack = (name, count = 1, enchants = []) => ({ name, count, enchants })

// A hand-made bot and enchanting table window, the way mineflayer 4.39 hands it out: slot 0 the item, slot 1 the lapis,
// 2..37 the pockets; like the server it returns what lies in the table when the window closes, and like mineflayer
// bot.inventory only catches up after the close. The three offers arrive as window properties once an item lies in slot 0.
const makeBot = ({
  block = 'enchanting_table', pockets = [], level = 30, offers = [4, 9, 16], hints = [[5, 1], [8, 2], [5, 3]],
  open = 'now', offersNever = false, enchantHangs = false, enchantThrows = null, enchantable = true, landTimes = 1, afterEnchant = [stack('diamond_sword', 1, [{ name: 'sharpness', lvl: 3 }])],
  levelLands = true, dropsOnClose = false, levelLateMs = null
} = {}) => {
  const bot = new EventEmitter()
  const calls = []
  // the pockets keep their places: taking a stack out leaves a hole, as on the server
  const places = Array.from({ length: 36 }, (_, i) => (pockets[i] ? { ...pockets[i], slot: 36 + i } : null))
  const items = () => places.filter(Boolean)
  const inventory = Object.assign(new EventEmitter(), { items, emptySlotCount: () => places.filter(p => !p).length })
  const experience = { level }
  const win = Object.assign(new EventEmitter(), { id: 3, inventoryStart: 2, inventoryEnd: 38, slots: Array(38).fill(null), enchantments: [0, 1, 2].map(() => ({ level: -1, expected: { enchant: -1, level: -1 } })) })
  const sync = () => { for (let i = 0; i < 36; i++) win.slots[2 + i] = places[i] }
  sync()
  let pendingLevel = level
  const send = () => {
    if (offersNever) return
    setTimeout(() => {
      win.enchantments.forEach((e, i) => {
        e.level = enchantable ? offers[i] : 0
        e.expected = enchantable ? { enchant: hints[i][0], level: hints[i][1] } : { enchant: -1, level: -1 }
      })
    }, 1)
  }
  win.close = () => {
    calls.push(['close'])
    for (const i of [0, 1]) {
      const held = win.slots[i]
      if (!held) continue
      const same = items().find(x => x.name === held.name && !x.enchants.length && !held.enchants.length)
      const free = places.indexOf(null)
      if (same) same.count += held.count
      else if (!dropsOnClose) places[free] = { ...held, slot: 36 + free }
      win.slots[i] = null
    }
    if (levelLateMs === null) experience.level = pendingLevel
    sync()
    setTimeout(() => { inventory.emit('updateSlot', 36) }, 1)
    bot.currentWindow = null
    win.emit('close')
  }
  win.enchant = async choice => {
    calls.push(['enchant', choice])
    if (enchantHangs) return new Promise(() => {})
    if (enchantThrows) throw new Error(enchantThrows)
    const lapis = win.slots[1]
    lapis.count -= choice + 1
    if (lapis.count === 0) win.slots[1] = null
    pendingLevel = level - (choice + 1)
    if (levelLateMs !== null) setTimeout(() => { experience.level = pendingLevel; bot.emit('experience') }, levelLateMs)
    else if (levelLands) experience.level = pendingLevel
    win.slots[0] = { ...afterEnchant[0] }
    return win.slots[0]
  }
  return Object.assign(bot, {
    calls,
    registry,
    inventory,
    experience,
    currentWindow: null,
    entity: { position: vec3(0, 64, 0), height: 1.62 },
    blockAt: p => (p.x === pos.x && p.y === pos.y && p.z === pos.z ? { name: block, position: p } : null),
    openEnchantmentTable: async () => {
      calls.push(['open'])
      if (open === 'never') return new Promise(() => {})
      if (open === 'late') await sleep(200)
      if (open === 'throws') throw new Error('Expected minecraft:enchant when opening table but got minecraft:chest')
      bot.currentWindow = win
      return win
    },
    moveSlotItem: async (from, to) => {
      calls.push(['move', from, to])
      const held = win.slots[from]
      if (to === 0) {
        win.slots[0] = { ...held, count: 1 }
        held.count -= 1
        if (held.count === 0) places[places.indexOf(held)] = null
        send()
      } else {
        win.slots[1] = { ...held }
        places[places.indexOf(held)] = null
      }
      sync()
    },
    win
  })
}
const names = bot => bot.calls.map(c => c[0])
const tally = (bot, name) => bot.inventory.items().filter(i => i.name === name).reduce((n, i) => n + i.count, 0)
const sword = stack('diamond_sword')
const lapis = n => stack('lapis_lazuli', n)
const run = (bot, a) => enchantVisit(bot, ctx, { pos, ...a }, opts)

test('a block that is gone or not a table, or a table out of reach, is refused without a window', async t => {
  const rows = [
    ['gone', makeBot({ block: 'air' }), opts, { status: 'missing' }],
    ['a chest', makeBot({ block: 'chest' }), opts, { status: 'cannot', reason: 'not-a-table' }],
    ['too far', makeBot({ pockets: [sword] }), { ...opts, distanceTo: () => 9.5 }, { status: 'unreachable', reason: 'too-far', distance: 9.5 }]
  ]
  for (const [label, bot, o, expected] of rows) {
    await t.test(label, async () => {
      assert.deepEqual(await enchantVisit(bot, ctx, { pos, op: 'offers', item: 'diamond_sword' }, o), expected)
      assert.deepEqual(names(bot), [])
    })
  }
})

test('an item that is not carried, or only carried enchanted, is refused before the window opens', async t => {
  const enchanted = stack('diamond_sword', 1, [{ name: 'sharpness', lvl: 1 }])
  const rows = [
    ['not carried', [lapis(5)], 'offers', { status: 'no-item', item: 'diamond_sword' }],
    ['enchanted only', [enchanted, lapis(5)], 'offers', { status: 'cannot', reason: 'already-enchanted', item: 'diamond_sword' }]
  ]
  for (const [label, pockets, op, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ pockets })
      assert.deepEqual(await run(bot, { op, item: 'diamond_sword', choice: 0 }), expected)
      assert.deepEqual(names(bot), [])
    })
  }
})

test('offers: the three offers with level cost, lapis cost and the hinted enchantment, the item given back', async () => {
  const bot = makeBot({ pockets: [sword, lapis(7)], level: 12 })
  const r = await run(bot, { op: 'offers', item: 'diamond_sword' })
  assert.deepEqual(r, {
    status: 'ok',
    item: 'diamond_sword',
    xpLevel: 12,
    lapis: 7,
    offers: [
      { index: 0, levelCost: 4, lapisCost: 1, hint: { enchant: 'sharpness', level: 1 } },
      { index: 1, levelCost: 9, lapisCost: 2, hint: { enchant: 'unbreaking', level: 2 } },
      { index: 2, levelCost: 16, lapisCost: 3, hint: { enchant: 'sharpness', level: 3 } }
    ]
  })
  assert.deepEqual(names(bot), ['open', 'move', 'close'])
  assert.equal(tally(bot, 'diamond_sword'), 1)
  assert.equal(bot.currentWindow, null)
})

test('offers: an offer without a hint, an unknown hint id and no lapis at all', async () => {
  const bot = makeBot({ pockets: [sword], hints: [[-1, -1], [99, 1], [5, 1]] })
  const r = await run(bot, { op: 'offers', item: 'diamond_sword' })
  assert.deepEqual(r.offers.map(o => o.hint), [null, { enchant: 'enchantment#99', level: 1 }, { enchant: 'sharpness', level: 1 }])
  assert.equal(r.lapis, 0)
})

test('offers: an item the table has no offer for reads as zero level costs', async () => {
  const bot = makeBot({ pockets: [sword, lapis(3)], enchantable: false })
  const r = await run(bot, { op: 'offers', item: 'diamond_sword' })
  assert.equal(r.status, 'ok')
  assert.deepEqual(r.offers.map(o => o.levelCost), [0, 0, 0])
  assert.equal(tally(bot, 'diamond_sword'), 1)
})

test('offers that never arrive fail and the item is given back', async () => {
  const bot = makeBot({ pockets: [sword], offersNever: true })
  assert.deepEqual(await run(bot, { op: 'offers', item: 'diamond_sword' }), { status: 'failed', reason: 'offers-did-not-arrive' })
  assert.equal(tally(bot, 'diamond_sword'), 1)
  assert.equal(names(bot).at(-1), 'close')
})

test('a window that does not open in time fails, and a late one is closed', async () => {
  const never = makeBot({ pockets: [sword], open: 'never' })
  assert.deepEqual(await run(never, { op: 'offers', item: 'diamond_sword' }), { status: 'failed', reason: 'window-did-not-open' })
  const late = makeBot({ pockets: [sword], open: 'late' })
  assert.deepEqual(await run(late, { op: 'offers', item: 'diamond_sword' }), { status: 'failed', reason: 'window-did-not-open' })
  await sleep(300)
  assert.deepEqual(names(late), ['open', 'close'])
})

test('a window of the wrong kind fails with the reason', async () => {
  const bot = makeBot({ pockets: [sword], open: 'throws' })
  const r = await run(bot, { op: 'offers', item: 'diamond_sword' })
  assert.equal(r.status, 'failed')
  assert.match(r.reason, /Expected minecraft:enchant/)
})

test('enchant: the result is measured, what was spent and what the item has now', async () => {
  const bot = makeBot({ pockets: [sword, lapis(9)], level: 30 })
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 1, levelCost: 9 })
  assert.deepEqual(r, {
    status: 'enchanted',
    item: 'diamond_sword',
    choice: 1,
    enchants: [{ name: 'sharpness', level: 3 }],
    lapisSpent: 2,
    levelsSpent: 2,
    xpLevel: 28
  })
  assert.deepEqual(names(bot), ['open', 'move', 'move', 'enchant', 'close'])
  assert.deepEqual(bot.calls.find(c => c[0] === 'enchant'), ['enchant', 1])
  assert.equal(tally(bot, 'lapis_lazuli'), 7)
  assert.equal(bot.currentWindow, null)
})

test('enchant: the item and the lapis go in by the window slot numbers, not the inventory ones', async () => {
  const bot = makeBot({ pockets: [stack('dirt', 3), sword, lapis(9)] })
  await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 0 })
  assert.deepEqual(bot.calls.filter(c => c[0] === 'move'), [['move', 3, 0], ['move', 4, 1]])
})

test('enchant: a stack of several is put in one at a time by mineflayer and the rest stays carried', async () => {
  const bot = makeBot({ pockets: [stack('book', 3), lapis(3)], afterEnchant: [stack('enchanted_book', 1, [{ name: 'unbreaking', lvl: 2 }])] })
  const r = await run(bot, { op: 'enchant', item: 'book', choice: 0 })
  assert.equal(r.status, 'enchanted')
  assert.equal(tally(bot, 'book'), 2)
  assert.equal(bot.inventory.items().filter(i => i.enchants.length).length, 1)
})

test('enchant: a book comes out as an enchanted book', async () => {
  const bot = makeBot({ pockets: [stack('book', 1), lapis(3)], afterEnchant: [stack('enchanted_book', 1, [{ name: 'unbreaking', lvl: 2 }])] })
  const r = await run(bot, { op: 'enchant', item: 'book', choice: 0 })
  assert.deepEqual([r.status, r.enchants], ['enchanted', [{ name: 'unbreaking', level: 2 }]])
})

test('enchant: an enchanted copy already carried is left alone and the plain one is used', async () => {
  const done = stack('diamond_sword', 1, [{ name: 'unbreaking', lvl: 1 }])
  const bot = makeBot({ pockets: [done, sword, lapis(3)] })
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 0 })
  assert.equal(r.status, 'enchanted')
  assert.deepEqual(r.enchants, [{ name: 'sharpness', level: 3 }])
  assert.deepEqual(bot.calls.find(c => c[0] === 'move'), ['move', 3, 0])
})

test('enchant: refusals found in the window spend nothing and give the item back', async t => {
  const rows = [
    ['no lapis', { pockets: [sword] }, { choice: 0 }, { status: 'no-lapis', have: 0, need: 1 }],
    ['too little lapis for the slot', { pockets: [sword, lapis(2)] }, { choice: 2 }, { status: 'no-lapis', have: 2, need: 3 }],
    ['too few levels', { pockets: [sword, lapis(5)], level: 8 }, { choice: 1 }, { status: 'no-levels', need: 9, have: 8 }],
    ['the slot number is the floor of the levels', { pockets: [sword, lapis(5)], level: 2, offers: [1, 1, 1] }, { choice: 2 }, { status: 'no-levels', need: 3, have: 2 }],
    ['nothing offered', { pockets: [sword, lapis(5)], enchantable: false }, { choice: 0 }, { status: 'cannot', reason: 'not-enchantable' }],
    ['an offer that is not there', { pockets: [sword, lapis(5)], offers: [4, 9, 0] }, { choice: 2 }, { status: 'cannot', reason: 'no-such-offer', offers: [4, 9, 0] }],
    ['the offer changed since it was read', { pockets: [sword, lapis(5)] }, { choice: 1, levelCost: 8 }, { status: 'cannot', reason: 'offer-changed', offers: [4, 9, 16] }]
  ]
  for (const [label, spec, args, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot(spec)
      assert.deepEqual(await run(bot, { op: 'enchant', item: 'diamond_sword', ...args }), expected)
      assert.ok(!names(bot).includes('enchant'))
      assert.equal(tally(bot, 'diamond_sword'), 1)
      assert.equal(tally(bot, 'lapis_lazuli'), spec.pockets.find(p => p.name === 'lapis_lazuli')?.count ?? 0)
      assert.equal(names(bot).at(-1), 'close')
    })
  }
})

test('enchant: a call that never returns is cut by the timeout and the result is still measured', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)], enchantHangs: true })
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 0 })
  assert.deepEqual(r, { status: 'failed', reason: 'enchant-stalled', lapisSpent: 0, levelsSpent: 0 })
  assert.equal(names(bot).at(-1), 'close')
  assert.equal(tally(bot, 'diamond_sword'), 1)
})

test('enchant: a call that throws is a failure with the reason, nothing assumed', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)], enchantThrows: 'bad window' })
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 0 })
  assert.deepEqual(r, { status: 'failed', reason: 'bad window', lapisSpent: 0, levelsSpent: 0 })
})

test('enchant: a stalled call whose enchant landed anyway is reported as enchanted, flagged stalled', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)] })
  const real = bot.win.enchant
  bot.win.enchant = async c => { await real(c); return new Promise(() => {}) }
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 0 })
  assert.equal(r.status, 'enchanted')
  assert.equal(r.stalled, true)
  assert.equal(r.lapisSpent, 1)
})

test('enchant: the body level is read after the close, where it lands', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)], levelLands: false })
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 2 })
  assert.equal(r.levelsSpent, 3)
  assert.equal(r.xpLevel, 27)
})

test('enchant: a level that arrives after the window call returned is waited for', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)], levelLateMs: 30, levelLands: false })
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 1 })
  assert.deepEqual([r.levelsSpent, r.xpLevel], [2, 28])
})

test('enchant: an item that came back unenchanted is a failure, never enchanted', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)], afterEnchant: [stack('diamond_sword')] })
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 0 })
  assert.equal(r.status, 'failed')
  assert.equal(r.reason, 'not-confirmed')
  assert.equal(r.lapisSpent, 1)
})

test('enchant: an item that did not come back with no room left is full', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)], dropsOnClose: true })
  bot.inventory.emptySlotCount = () => 0
  const r = await run(bot, { op: 'enchant', item: 'diamond_sword', choice: 0 })
  assert.equal(r.status, 'full')
})

test('a cut registers the window close for the abort path', async () => {
  let abort
  const bot = makeBot({ pockets: [sword] })
  await enchantVisit(bot, { alive: () => {}, onAbort: f => { abort = f } }, { pos, op: 'offers', item: 'diamond_sword' }, opts)
  abort()
  assert.ok(names(bot).filter(n => n === 'close').length >= 1)
})

test('a cut after the enchant call leaves no experience or slot listener on the bot', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)] })
  let n = 0
  const cut = { alive: () => { if (++n > 3) throw new Error('cut') }, onAbort: () => {} }
  await assert.rejects(enchantVisit(bot, cut, { pos, op: 'enchant', item: 'diamond_sword', choice: 0 }, opts), /cut/)
  assert.equal(bot.listenerCount('experience'), 0)
})

test('a cut inside the visit closes the window and ends it', async () => {
  const bot = makeBot({ pockets: [sword, lapis(5)] })
  let n = 0
  const cut = { alive: () => { if (++n > 2) throw new Error('cut') }, onAbort: () => {} }
  await assert.rejects(enchantVisit(bot, cut, { pos, op: 'enchant', item: 'diamond_sword', choice: 0 }, opts), /cut/)
  assert.equal(names(bot).at(-1), 'close')
})
