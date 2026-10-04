// Why JavaScript: tests trade.mjs, which stays JS: Mineflayer boundary; decodes and drives the villager trade window.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import { tradeWith, professionOf } from './trade.mjs'

const REACH = 3.5
const ctx = { alive: () => {}, onAbort: () => {} }
const opts = { timeScale: 0.02, reach: REACH }
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

const VILLAGER_DATA = 9
const BABY = 8
const registry = {
  entitiesByName: {
    villager: { metadataKeys: ['shared_flags', 'air', 'name', 'name_visible', 'silent', 'no_gravity', 'pose', 'frozen', 'baby', 'villager_data'] },
    cow: { metadataKeys: ['shared_flags', 'air', 'name', 'name_visible', 'silent', 'no_gravity', 'pose', 'frozen', 'baby'] }
  },
  itemsByName: { emerald: { stackSize: 64 }, bread: { stackSize: 64 }, sword: { stackSize: 1 } }
}

const data = (profession, level = 2) => ({ villagerType: 2, villagerProfession: profession, level })
const villager = (extra = {}) => ({ id: 5, uuid: 'v-1', name: 'villager', position: vec3(1, 64, 0), height: 1.95, metadata: { [VILLAGER_DATA]: data(5) }, ...extra })
const FARMER = 5

// a trade as mineflayer 4.39 reads it; `real` is the adjusted price of the first cost stack
const offer = ({ cost = ['emerald', 2], cost2 = null, gives = ['bread', 3], uses = 0, max = 12, disabled = false, real = cost[1] } = {}) => ({
  inputItem1: { name: cost[0], count: cost[1] },
  inputItem2: cost2 && { name: cost2[0], count: cost2[1] },
  hasItem2: cost2 !== null,
  outputItem: { name: gives[0], count: gives[1] },
  tradeDisabled: disabled,
  nbTradeUses: uses,
  maximumNbTradeUses: max,
  realPrice: real
})

const stack = (name, count) => ({ name, count })

// A hand-made bot. Like mineflayer, a trade only lands in the inventory when the window is closed.
const makeBot = ({ target = villager(), stock = [], trades = [offer()], free = 5, open = 'now', confirm = true, tradeThrows = null, landsBeforeThrow = false, hang = false, landTimes = null, at = vec3(0, 64, 0) } = {}) => {
  const bot = new EventEmitter()
  const calls = []
  const items = [...stock]
  const inventory = Object.assign(new EventEmitter(), { items: () => items, emptySlotCount: () => free })
  const win = { id: 1, trades }
  let pending = []
  const add = (name, n) => {
    const idx = items.findIndex(i => i.name === name)
    if (idx < 0) return n > 0 && items.push(stack(name, n))
    items[idx] = stack(name, items[idx].count + n)
  }
  const land = () => {
    pending.forEach(f => f())
    pending = []
    inventory.emit('updateSlot', 36)
  }
  return Object.assign(bot, {
    calls,
    registry,
    entities: target ? { [target.id]: target } : {},
    entity: { id: 4, position: at, height: 1.62 },
    inventory,
    currentWindow: null,
    openVillager: async e => {
      calls.push(['openVillager', e.uuid])
      if (open === 'never') return new Promise(() => {})
      if (open === 'late') await sleep(200)
      bot.currentWindow = win
      return win
    },
    trade: async (w, index, n) => {
      calls.push(['trade', index, n])
      const t = trades[index]
      const times = landTimes ?? n
      confirm && (!tradeThrows || landsBeforeThrow) && pending.push(() => {
        add(t.inputItem1.name, -t.realPrice * times)
        t.hasItem2 && add(t.inputItem2.name, -t.inputItem2.count * times)
        add(t.outputItem.name, t.outputItem.count * times)
      })
      if (hang) return new Promise(() => {})
      if (tradeThrows) throw new Error(tradeThrows)
    },
    closeWindow: w => { calls.push(['closeWindow']); bot.currentWindow = null; land() }
  })
}
const names = bot => bot.calls.map(c => c[0])
const closes = bot => names(bot).filter(n => n === 'closeWindow').length
const run = (bot, a) => tradeWith(bot, ctx, { villager: 'v-1', ...a }, opts)

test('professionOf reads a number, a namespaced string and a bare string', () => {
  const rows = [
    [0, 'unemployed'], [FARMER, 'farmer'], [11, 'nitwit'], [14, 'weaponsmith'],
    ['minecraft:farmer', 'farmer'], ['minecraft:none', 'unemployed'], ['librarian', 'librarian'], [99, 'unknown'], [undefined, 'unemployed'], [null, 'unemployed'], [{}, 'unknown']
  ]
  for (const [raw, expected] of rows) assert.equal(professionOf(raw), expected, String(raw))
})

test('an entity that is gone, not a villager or too far is refused without opening a window', async t => {
  const rows = [
    ['gone', { target: null }, { status: 'gone' }],
    ['a cow', { target: villager({ name: 'cow' }) }, { status: 'cannot', reason: 'not-villager', name: 'cow' }],
    ['too far', { target: villager({ position: vec3(9, 64, 0) }) }, { status: 'out-of-reach', reason: 'too-far', distance: 9.0 }]
  ]
  for (const [label, spec, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot(spec)
      const result = await run(bot, { op: 'offers' })
      assert.equal(result.status, expected.status)
      assert.equal(result.reason, expected.reason)
      assert.equal(result.name, expected.name)
      assert.deepEqual(names(bot), [])
    })
  }
})

test('the distance is measured from the eye to the middle of the villager', async () => {
  const result = await run(makeBot({ target: villager({ position: vec3(0, 60, 0) }) }), { op: 'offers' })
  // eye at y 65.62, middle at 60 + 0.975
  assert.equal(result.status, 'out-of-reach')
  assert.ok(Math.abs(result.distance - 4.645) < 1e-6)
})

test('a villager with nothing to sell is refused without opening a window', async t => {
  const rows = [
    ['unemployed', { metadata: { [VILLAGER_DATA]: data(0, 1) } }, 'unemployed', 'unemployed', 1],
    ['nitwit', { metadata: { [VILLAGER_DATA]: data(11, 1) } }, 'nitwit', 'nitwit', 1],
    ['nitwit by name', { metadata: { [VILLAGER_DATA]: data('minecraft:nitwit', 1) } }, 'nitwit', 'nitwit', 1],
    ['baby', { metadata: { [VILLAGER_DATA]: data(0, 1), [BABY]: true } }, 'baby', 'unemployed', 1],
    ['baby with a profession', { metadata: { [VILLAGER_DATA]: data(FARMER, 3), [BABY]: true } }, 'baby', 'farmer', 3]
  ]
  for (const [label, extra, why, profession, level] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ target: villager(extra) })
      assert.deepEqual(await run(bot, { op: 'offers' }), { status: 'cannot', reason: 'no-offers', why, profession, level })
      assert.deepEqual(names(bot), [])
    })
  }
})

test('a villager whose villager_data was never sent is an unemployed level 1 one, with no window', async t => {
  const rows = [
    ['no metadata at all', { metadata: {} }, 'unemployed'],
    ['metadata slot null', { metadata: { [VILLAGER_DATA]: null } }, 'unemployed']
  ]
  for (const [label, extra, why] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ target: villager(extra) })
      assert.deepEqual(await run(bot, { op: 'offers' }), { status: 'cannot', reason: 'no-offers', why, profession: 'unemployed', level: 1 })
      assert.deepEqual(names(bot), [])
    })
  }
})

test('villager_data is read at index 18 when the registry does not list it', async () => {
  const bot = makeBot({ target: villager({ metadata: { 18: data(FARMER, 4) } }) })
  bot.registry = {}
  const result = await run(bot, { op: 'offers' })
  assert.equal(result.profession, 'farmer')
  assert.equal(result.level, 4)
})

test('a window that does not open in time fails, and a late window is closed', async () => {
  const never = makeBot({ open: 'never' })
  assert.deepEqual(await run(never, { op: 'offers' }), { status: 'failed', reason: 'window-did-not-open' })
  const late = makeBot({ open: 'late' })
  assert.deepEqual(await run(late, { op: 'offers' }), { status: 'failed', reason: 'window-did-not-open' })
  await sleep(300)
  assert.deepEqual(names(late), ['openVillager', 'closeWindow'])
})

test('offers: adjusted price, second cost, uses and the sold-out flags', async () => {
  const trades = [
    offer({ cost: ['emerald', 5], real: 3, gives: ['bread', 6], uses: 2, max: 12 }),
    offer({ cost: ['emerald', 1], cost2: ['bread', 4], gives: ['sword', 1], uses: 1, max: 3, real: 1 }),
    offer({ uses: 4, max: 4 }),
    offer({ uses: 0, max: 8, disabled: true })
  ]
  const bot = makeBot({ trades })
  const rows = await run(bot, { op: 'offers' })
  assert.deepEqual(rows, {
    status: 'ok',
    uuid: 'v-1',
    profession: 'farmer',
    level: 2,
    offers: [
      { index: 0, cost: [{ item: 'emerald', count: 3 }], gives: { item: 'bread', count: 6 }, uses: 2, maxUses: 12, left: 10, disabled: false },
      { index: 1, cost: [{ item: 'emerald', count: 1 }, { item: 'bread', count: 4 }], gives: { item: 'sword', count: 1 }, uses: 1, maxUses: 3, left: 2, disabled: false },
      { index: 2, cost: [{ item: 'emerald', count: 2 }], gives: { item: 'bread', count: 3 }, uses: 4, maxUses: 4, left: 0, disabled: true },
      { index: 3, cost: [{ item: 'emerald', count: 2 }], gives: { item: 'bread', count: 3 }, uses: 0, maxUses: 8, left: 8, disabled: true }
    ]
  })
  assert.deepEqual(names(bot), ['openVillager', 'closeWindow'])
})

test('offers: a profession given by name and an empty window', async () => {
  const named = await run(makeBot({ target: villager({ metadata: { [VILLAGER_DATA]: data('minecraft:librarian', 5) } }) }), { op: 'offers' })
  assert.equal(named.profession, 'librarian')
  assert.equal(named.level, 5)
  const bot = makeBot({ trades: [] })
  assert.deepEqual(await run(bot, { op: 'offers' }), { status: 'cannot', reason: 'no-offers', why: 'none', profession: 'farmer', level: 2 })
  assert.equal(closes(bot), 1)
})

test('buy: refusals leave the inventory alone and close the window', async t => {
  const trades = [
    offer(),
    offer({ uses: 3, max: 3 }),
    offer({ disabled: true }),
    offer({ cost: ['emerald', 5], cost2: ['bread', 4], gives: ['sword', 1] }),
    offer({ cost: ['emerald', 5], cost2: ['bread', 4], gives: ['sword', 1] })
  ]
  const rows = [
    ['no such offer', 9, [stack('emerald', 9)], 5, { status: 'cannot', reason: 'no-such-offer', offers: 5 }],
    ['sold out by uses', 1, [stack('emerald', 9)], 5, { status: 'cannot', reason: 'sold-out' }],
    ['sold out by the flag', 2, [stack('emerald', 9)], 5, { status: 'cannot', reason: 'sold-out' }],
    ['first cost short', 0, [stack('emerald', 1)], 5, { status: 'no-item', short: { emerald: 1 } }],
    ['first cost absent', 0, [], 5, { status: 'no-item', short: { emerald: 2 } }],
    ['second cost short', 3, [stack('emerald', 9), stack('bread', 1)], 5, { status: 'no-item', short: { bread: 3 } }],
    ['both costs short', 4, [stack('emerald', 2)], 5, { status: 'no-item', short: { emerald: 3, bread: 4 } }],
    ['no room', 0, [stack('emerald', 9), stack('bread', 64)], 0, { status: 'full' }],
    ['a full stack of the result is no room', 0, [stack('emerald', 9), stack('bread', 64), stack('wheat', 64)], 0, { status: 'full' }]
  ]
  for (const [label, offerIndex, stock, free, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ trades, stock, free })
      const before = JSON.stringify(stock)
      assert.deepEqual(await run(bot, { op: 'buy', offer: offerIndex }), expected)
      assert.equal(JSON.stringify(bot.inventory.items()), before)
      assert.deepEqual(names(bot), ['openVillager', 'closeWindow'])
    })
  }
})

test('buy: room counts the free part of the stacks already carried', async () => {
  const bot = makeBot({ stock: [stack('emerald', 9), stack('bread', 61)], free: 0 })
  const result = await run(bot, { op: 'buy', offer: 0 })
  assert.equal(result.status, 'bought')
  assert.equal(result.times, 1)
})

test('buy: what was gained and paid is measured after the window closed', async t => {
  const swords = offer({ cost: ['emerald', 1], cost2: ['bread', 2], gives: ['sword', 1] })
  const rows = [
    ['default one trade', {}, [offer()], [stack('emerald', 9)], { times: 1, requested: 1, gained: { bread: 3 }, paid: { emerald: 2 }, stopped: null }, 1],
    ['two trades', { times: 2 }, [offer()], [stack('emerald', 9), stack('bread', 1)], { times: 2, requested: 2, gained: { bread: 6 }, paid: { emerald: 4 }, stopped: null }, 2],
    ['the adjusted price is paid', { times: 2 }, [offer({ cost: ['emerald', 5], real: 3 })], [stack('emerald', 9)], { times: 2, requested: 2, gained: { bread: 6 }, paid: { emerald: 6 }, stopped: null }, 2],
    ['two costs', { times: 2 }, [swords], [stack('emerald', 9), stack('bread', 9)], { times: 2, requested: 2, gained: { sword: 2 }, paid: { emerald: 2, bread: 4 }, stopped: null }, 2]
  ]
  for (const [label, extra, trades, stock, expected, asked] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ trades, stock, free: 5 })
      assert.deepEqual(await run(bot, { op: 'buy', offer: 0, ...extra }), { status: 'bought', ...expected })
      assert.deepEqual(bot.calls.slice(1), [['trade', 0, asked], ['closeWindow']])
    })
  }
})

test('buy: the count is clamped by uses left, payment and room, and says which', async t => {
  const rows = [
    ['uses left', [offer({ uses: 10, max: 12 })], [stack('emerald', 40)], 5, 5, 2, 'sold-out', { bread: 6 }, { emerald: 4 }],
    ['payment', [offer()], [stack('emerald', 5)], 5, 5, 2, 'payment', { bread: 6 }, { emerald: 4 }],
    ['payment of the second cost', [offer({ cost2: ['bread', 3], gives: ['sword', 1] })], [stack('emerald', 40), stack('bread', 7)], 5, 5, 2, 'payment', { sword: 2 }, { emerald: 4, bread: 6 }],
    ['room', [offer({ cost: ['emerald', 1], gives: ['sword', 1] })], [stack('emerald', 40)], 2, 5, 2, 'room', { sword: 2 }, { emerald: 2 }],
    ['room in partial stacks', [offer({ gives: ['bread', 3] })], [stack('emerald', 40), stack('bread', 55)], 0, 5, 3, 'room', { bread: 9 }, { emerald: 6 }]
  ]
  for (const [label, trades, stock, free, ask, did, stopped, gained, paid] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ trades, stock, free })
      assert.deepEqual(await run(bot, { op: 'buy', offer: 0, times: ask }), { status: 'bought', times: did, requested: ask, gained, paid, stopped })
      assert.deepEqual(bot.calls.find(c => c[0] === 'trade'), ['trade', 0, did])
    })
  }
})

test('buy: a trade the server did not carry out is not confirmed', async () => {
  const bot = makeBot({ stock: [stack('emerald', 9)], confirm: false })
  assert.deepEqual(await run(bot, { op: 'buy', offer: 0 }), { status: 'failed', reason: 'not-confirmed', paid: {}, gained: {} })
  assert.equal(closes(bot), 1)
})

test('buy: a trade that throws closes the window and still reports what changed', async t => {
  const long = 'x'.repeat(300)
  const rows = [
    ['nothing changed', 'boom', false, { status: 'failed', reason: 'boom' }],
    ['message cut to 120', long, false, { status: 'failed', reason: long.slice(0, 120) }],
    ['something landed', 'boom', true, { status: 'bought', times: 1, requested: 1, gained: { bread: 3 }, paid: { emerald: 2 }, stopped: null }]
  ]
  for (const [label, message, landsBeforeThrow, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ stock: [stack('emerald', 9)], tradeThrows: message, landsBeforeThrow })
      assert.deepEqual(await run(bot, { op: 'buy', offer: 0 }), expected)
      assert.equal(closes(bot), 1)
    })
  }
})

test('buy: a trade call that never resolves is cut, the window closed and what changed reported', async t => {
  const rows = [
    ['landed in full', { hang: true }, { op: 'buy', offer: 0, times: 1 }, { status: 'bought', times: 1, requested: 1, gained: { bread: 3 }, paid: { emerald: 2 }, stopped: null, stalled: true }],
    ['landed in part', { hang: true, landTimes: 1 }, { op: 'buy', offer: 0, times: 3 }, { status: 'bought', times: 1, requested: 3, gained: { bread: 3 }, paid: { emerald: 2 }, stopped: 'stalled', stalled: true }],
    ['nothing changed', { hang: true, confirm: false }, { op: 'buy', offer: 0, times: 1 }, { status: 'failed', reason: 'trade-stalled', paid: {}, gained: {} }]
  ]
  for (const [label, spec, a, expected] of rows) {
    await t.test(label, async () => {
      const bot = makeBot({ stock: [stack('emerald', 9)], ...spec })
      assert.deepEqual(await run(bot, a), expected)
      assert.equal(closes(bot), 1)
      assert.equal(bot.currentWindow, null)
    })
  }
})

test('the window is closed whatever the call does', async t => {
  const rows = [
    ['offers', { op: 'offers' }, {}],
    ['zero offers', { op: 'offers' }, { trades: [] }],
    ['buy refused', { op: 'buy', offer: 7 }, {}],
    ['buy done', { op: 'buy', offer: 0 }, { stock: [stack('emerald', 9)] }],
    ['buy throws', { op: 'buy', offer: 0 }, { stock: [stack('emerald', 9)], tradeThrows: 'boom' }]
  ]
  for (const [label, a, spec] of rows) {
    await t.test(label, async () => {
      const bot = makeBot(spec)
      await run(bot, a)
      assert.equal(closes(bot), 1)
      assert.equal(bot.currentWindow, null)
    })
  }
})

test('an abort closes an open window, and a cut call closes it too', async () => {
  const aborts = []
  const bot = makeBot({ stock: [stack('emerald', 9)] })
  await tradeWith(bot, { alive: () => {}, onAbort: f => aborts.push(f) }, { villager: 'v-1', op: 'offers' }, opts)
  assert.equal(aborts.length, 1)
  bot.currentWindow = {}
  aborts[0]()
  assert.equal(closes(bot), 2)

  const cut = makeBot()
  const cutCtx = { alive: () => { throw new Error('cut') }, onAbort: () => {} }
  await assert.rejects(tradeWith(cut, cutCtx, { villager: 'v-1', op: 'offers' }, opts), /cut/)
  assert.equal(closes(cut), 1)
})
