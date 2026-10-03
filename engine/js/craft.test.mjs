import { test } from 'node:test'
import { EventEmitter } from 'node:events'
import assert from 'node:assert/strict'
import { stubBot } from './stub-bot.mjs'
import { craftItem, craftShortfall, craftAlternatives } from './craft.mjs'

const REG = {
  1: { id: 1, name: 'wheat', stackSize: 64 },
  2: { id: 2, name: 'bread', stackSize: 64 },
  3: { id: 3, name: 'oak_planks', stackSize: 64 },
  4: { id: 4, name: 'stick', stackSize: 64 },
  5: { id: 5, name: 'crafting_table', stackSize: 64 },
  6: { id: 6, name: 'iron_ingot', stackSize: 64 },
  7: { id: 7, name: 'oak_log', stackSize: 64 },
  8: { id: 8, name: 'cherry_planks', stackSize: 64 }
}
const ID = Object.fromEntries(Object.values(REG).map(i => [i.name, i.id]))

const recipe = (result, count, ingredients, requiresTable = false) => ({
  requiresTable,
  result: { id: ID[result], count },
  delta: [...Object.entries(ingredients).map(([n, c]) => ({ id: ID[n], count: -c })), { id: ID[result], count }]
})

const RECIPES = {
  bread: [recipe('bread', 1, { wheat: 3 }, true)],
  stick: [recipe('stick', 4, { oak_planks: 2 }), recipe('stick', 4, { cherry_planks: 2 })],
  oak_planks: [recipe('oak_planks', 4, { oak_log: 1 })],
  crafting_table: [recipe('crafting_table', 1, { oak_planks: 4 })],
  iron_ingot: []
}

const inv = (...pairs) => pairs.map(([name, count], i) => ({ name, count, type: ID[name], slot: 36 + i }))

const setup = ({ items = [], blocks = {}, mode = 'ok', slots = 36, grid = {}, strictBlocks = false, lateMs = 5 } = {}) => {
  const bot = stubBot({ items, blocks })
  if (strictBlocks) {
    const plain = bot.blockAt
    bot.blockAt = v => { if (typeof v.floored !== 'function') throw new TypeError('pos.floored is not a function'); return plain(v) }
  }
  const events = new EventEmitter()
  Object.assign(bot.inventory, { on: events.on.bind(events), off: events.off.bind(events), removeListener: events.removeListener.bind(events), emit: events.emit.bind(events) })
  const state = { mode, crafts: 0, closed: [], syncs: 0 }
  const carried = name => items.filter(i => i.name === name).reduce((s, i) => s + i.count, 0)
  const add = (name, n) => {
    const stack = items.find(i => i.name === name)
    if (stack) stack.count += n
    else items.push({ name, count: n, type: ID[name], slot: 36 + items.length })
  }
  const runnable = (r, table) => (table || !r.requiresTable) && r.delta.filter(d => d.count < 0).every(d => carried(REG[d.id].name) >= -d.count)
  Object.assign(bot, {
    registry: { items: REG, itemsByName: Object.fromEntries(Object.values(REG).map(i => [i.name, i])) },
    recipesAll: (id, _m, table) => (RECIPES[REG[id].name] ?? []).filter(r => table || !r.requiresTable),
    recipesFor: (id, _m, _n, table) => (RECIPES[REG[id].name] ?? []).filter(r => runnable(r, table)),
    craft: async (r, n, table) => {
      state.crafts++
      if (state.mode === 'reject') throw new Error('window closed')
      if (state.mode === 'noop') return
      if (state.mode === 'noop-twice' && state.crafts <= 2) return
      if (state.mode === 'late') {
        // the whole ingredient stack goes to the cursor; the leftover comes back later through updateSlot
        const taken = r.delta.filter(d => d.count < 0).map(d => [REG[d.id].name, -d.count, carried(REG[d.id].name)])
        taken.forEach(([name]) => items.splice(0, items.length, ...items.filter(i => i.name !== name)))
        r.delta.filter(d => d.count > 0).forEach(d => add(REG[d.id].name, d.count))
        taken.forEach(([name, need, had]) => setTimeout(() => { add(name, had - need); bot.inventory.emit('updateSlot', 36) }, lateMs))
        return
      }
      r.delta.forEach(d => d.count < 0 ? add(REG[d.id].name, d.count) : add(REG[d.id].name, d.count))
      items.splice(0, items.length, ...items.filter(i => i.count > 0))
    },
    _syncWindow: async () => { state.syncs++ },
    closeWindow: w => state.closed.push(w)
  })
  bot.inventory.emptySlotCount = () => slots
  bot.inventory.slots = grid
  bot.inventory.selectedItem = null
  return { bot, state, items, carried }
}

const ctx = { alive: () => {}, onAbort: () => {} }
const opts = { timeScale: 0.01 }
const table = { x: 2, y: 64, z: 0 }
const tableBlocks = { '2,64,0': 'crafting_table' }

test('craftItem: explicit table position is handed to blockAt as a Vec3', async () => {
  const { bot } = setup({ items: inv(['wheat', 3]), blocks: tableBlocks, strictBlocks: true })
  const r = await craftItem(bot, ctx, { item: 'bread', table }, opts)
  assert.equal(r.status, 'crafted')
})

const lateOpts = { timeScale: 0.1 }

test('craftItem: leftovers that come back late are not mistaken for a shortfall', async () => {
  const { bot, carried } = setup({ items: inv(['wheat', 12]), blocks: tableBlocks, mode: 'late' })
  const r = await craftItem(bot, ctx, { item: 'bread', count: 4 }, lateOpts)
  assert.deepEqual(r, { status: 'crafted', item: 'bread', made: 4, used: { wheat: 12 } })
  assert.equal(carried('wheat'), 0)
})

test('craftItem: used counts leftovers that came back late', async () => {
  const { bot, carried } = setup({ items: inv(['wheat', 7]), blocks: tableBlocks, mode: 'late' })
  const r = await craftItem(bot, ctx, { item: 'bread', count: 2 }, lateOpts)
  assert.deepEqual(r, { status: 'crafted', item: 'bread', made: 2, used: { wheat: 6 } })
  assert.equal(carried('wheat'), 1)
})

const unknown = [
  ['unknown item', { item: 'nope' }, { status: 'cannot', reason: 'unknown-item' }],
  ['no recipe', { item: 'iron_ingot' }, { status: 'cannot', reason: 'no-recipe' }]
]
for (const [name, a, expected] of unknown) {
  test(`craftItem: ${name}`, async () => {
    const { bot } = setup()
    assert.deepEqual(await craftItem(bot, ctx, a, opts), expected)
  })
}

test('craftItem: 2x2 planks from a log', async () => {
  const { bot, carried } = setup({ items: inv(['oak_log', 2]) })
  const r = await craftItem(bot, ctx, { item: 'oak_planks', count: 8 }, opts)
  assert.deepEqual(r, { status: 'crafted', item: 'oak_planks', made: 8, used: { oak_log: 2 } })
  assert.equal(carried('oak_planks'), 8)
})

test('craftItem: table recipe with table in reach', async () => {
  const { bot } = setup({ items: inv(['wheat', 6]), blocks: tableBlocks })
  const r = await craftItem(bot, ctx, { item: 'bread', count: 2 }, opts)
  assert.deepEqual(r, { status: 'crafted', item: 'bread', made: 2, used: { wheat: 6 } })
})

test('craftItem: explicit table position', async () => {
  const { bot } = setup({ items: inv(['wheat', 3]), blocks: tableBlocks })
  const r = await craftItem(bot, ctx, { item: 'bread', table }, opts)
  assert.equal(r.status, 'crafted')
})

const tableCases = [
  ['table recipe with no table', { item: 'bread' }, {}, { status: 'unreachable', reason: 'no-table' }],
  ['table too far', { item: 'bread', table: { x: 9, y: 64, z: 0 } }, { '9,64,0': 'crafting_table' }, { status: 'out-of-reach', reason: 'too-far', table: { x: 9, y: 64, z: 0 } }],
  ['table known but out of reach', { item: 'bread' }, { '20,64,0': 'crafting_table' }, { status: 'out-of-reach', reason: 'too-far', table: { x: 20, y: 64, z: 0 } }],
  ['table beyond 32 is not known', { item: 'bread' }, { '40,64,0': 'crafting_table' }, { status: 'unreachable', reason: 'no-table' }],
  ['table arg is not a table', { item: 'bread', table }, { '2,64,0': 'stone' }, { status: 'unreachable', reason: 'not-a-table' }]
]
for (const [name, a, blocks, expected] of tableCases) {
  test(`craftItem: ${name}`, async () => {
    const { bot } = setup({ items: inv(['wheat', 3]), blocks })
    assert.deepEqual(await craftItem(bot, ctx, a, opts), expected)
  })
}

const shortCases = [
  ['missing ingredients', [['wheat', 1]], { item: 'bread' }, { wheat: 2 }, undefined],
  ['closest recipe wins (oak planks held)', [['oak_planks', 1]], { item: 'stick' }, { oak_planks: 1 }, { oak_planks: ['cherry_planks'] }],
  ['closest recipe wins (cherry planks held)', [['cherry_planks', 1]], { item: 'stick' }, { cherry_planks: 1 }, { cherry_planks: ['oak_planks'] }],
  ['nothing held', [], { item: 'stick' }, { oak_planks: 2 }, { oak_planks: ['cherry_planks'] }]
]
for (const [name, held, a, short, alternatives] of shortCases) {
  test(`craftItem: no-item, ${name}`, async () => {
    const { bot } = setup({ items: inv(...held), blocks: tableBlocks })
    assert.deepEqual(await craftItem(bot, ctx, a, opts), { status: 'no-item', short, ...(alternatives && { alternatives }) })
  })
}

test('craftItem: full inventory', async () => {
  const { bot, state } = setup({ items: inv(['oak_log', 1]), slots: 0 })
  const r = await craftItem(bot, ctx, { item: 'oak_planks' }, opts)
  assert.deepEqual(r, { status: 'full', made: 0, used: {} })
  assert.equal(state.crafts, 0)
})

test('craftItem: full inventory but the stack has room', async () => {
  const { bot } = setup({ items: inv(['oak_log', 1], ['oak_planks', 10]), slots: 0 })
  const r = await craftItem(bot, ctx, { item: 'oak_planks' }, opts)
  assert.equal(r.status, 'crafted')
})

test('craftItem: server no-op twice then success', async () => {
  const { bot, state } = setup({ items: inv(['oak_log', 1]), mode: 'noop-twice' })
  const r = await craftItem(bot, ctx, { item: 'oak_planks', count: 4 }, opts)
  assert.deepEqual(r, { status: 'crafted', item: 'oak_planks', made: 4, used: { oak_log: 1 } })
  assert.equal(state.crafts, 3)
})

test('craftItem: server always no-op fails', async () => {
  const { bot } = setup({ items: inv(['oak_log', 1]), mode: 'noop' })
  const r = await craftItem(bot, ctx, { item: 'oak_planks', count: 4 }, opts)
  assert.deepEqual(r, { status: 'failed', item: 'oak_planks', made: 0, used: {}, reason: 'server rejected the craft' })
})

test('craftItem: craft rejection is reported as reason', async () => {
  const { bot } = setup({ items: inv(['oak_log', 1]), mode: 'reject' })
  const r = await craftItem(bot, ctx, { item: 'oak_planks', count: 4 }, opts)
  assert.deepEqual(r, { status: 'failed', item: 'oak_planks', made: 0, used: {}, reason: 'window closed' })
})

test('craftItem: partial when ingredients run out', async () => {
  const { bot } = setup({ items: inv(['oak_log', 1]) })
  const r = await craftItem(bot, ctx, { item: 'oak_planks', count: 8 }, opts)
  assert.deepEqual(r, { status: 'partial', item: 'oak_planks', made: 4, used: { oak_log: 1 }, reason: 'no-item', short: { oak_log: 1 } })
})

test('craftItem: 2x2 leftovers in the grid close the window', async () => {
  const { bot, state } = setup({ items: inv(['oak_log', 1]), grid: { 1: { name: 'oak_log', count: 1 } } })
  await craftItem(bot, ctx, { item: 'oak_planks' }, opts)
  assert.ok(state.syncs >= 1)
  assert.deepEqual(state.closed, [bot.inventory])
})

test('craftItem: clean 2x2 grid does not close the window', async () => {
  const { bot, state } = setup({ items: inv(['oak_log', 1]) })
  await craftItem(bot, ctx, { item: 'oak_planks' }, opts)
  assert.deepEqual(state.closed, [])
})

test('craftItem: a cut propagates and an abort hook is registered', async () => {
  const { bot } = setup({ items: inv(['oak_log', 1]), mode: 'noop' })
  const hooks = []
  const cutCtx = { alive: () => { throw new Error('cut') }, onAbort: fn => hooks.push(fn) }
  await assert.rejects(craftItem(bot, cutCtx, { item: 'oak_planks' }, opts), /cut/)
  bot.currentWindow = { w: 1 }
  const closed = []
  bot.closeWindow = w => closed.push(w)
  hooks.forEach(fn => fn())
  assert.deepEqual(closed, [bot.currentWindow])
})

const shortfallCases = [
  ['empty recipes', [], { a: 1 }, {}],
  ['satisfied', [{ a: 2 }], { a: 2 }, {}],
  ['single missing', [{ a: 3, b: 1 }], { a: 1, b: 1 }, { a: 2 }],
  ['closest recipe', [{ oak: 2 }, { cherry: 2 }], { oak: 1 }, { oak: 1 }],
  ['closest recipe, other side', [{ oak: 2 }, { cherry: 2 }], { cherry: 1 }, { cherry: 1 }],
  ['nothing held', [{ a: 1, b: 2 }], {}, { a: 1, b: 2 }]
]
for (const [name, recipes, have, expected] of shortfallCases) {
  test(`craftShortfall: ${name}`, () => assert.deepEqual(craftShortfall(recipes, have), expected))
}

test('craftItem: count is items wanted, rounded up to whole batches', async () => {
  const { bot } = setup({ items: inv(['oak_log', 1]) })
  const r = await craftItem(bot, ctx, { item: 'oak_planks', count: 1 }, opts)
  assert.deepEqual(r, { status: 'crafted', item: 'oak_planks', made: 4, used: { oak_log: 1 } })
})

test('craftItem: never chains recipes', async () => {
  const { bot } = setup({ items: inv(['oak_log', 1]) })
  const r = await craftItem(bot, ctx, { item: 'stick' }, opts)
  assert.deepEqual(r, { status: 'no-item', short: { oak_planks: 2 }, alternatives: { oak_planks: ['cherry_planks'] } })
})

const pickaxe = [{ deepslate: 3, stick: 2 }, { blackstone: 3, stick: 2 }, { cobblestone: 3, stick: 2 }]

const preferenceCases = [
  ['deepslate, blackstone, cobblestone order', pickaxe, { stick: 2 }, { cobblestone: 3 }],
  ['held stick still prefers cobblestone', pickaxe, { stick: 2 }, { cobblestone: 3 }],
  ['a closer recipe beats preference', pickaxe, { stick: 2, blackstone: 1 }, { blackstone: 2 }]
]
for (const [name, recipes, have, expected] of preferenceCases) {
  test(`craftShortfall: ${name}`, () => assert.deepEqual(craftShortfall(recipes, have), expected))
}

test('craftAlternatives: the cousins of the missing name', () => {
  assert.deepEqual(craftAlternatives(pickaxe, { stick: 2 }), { cobblestone: ['deepslate', 'blackstone'] })
})

test('craftAlternatives: none when the recipes agree', () => {
  assert.deepEqual(craftAlternatives([{ a: 1 }], {}), {})
})

const clickStub = (items, bot) => {
  let cursor = null
  bot.clickWindow = async slot => {
    const here = items.find(i => i.slot === slot)
    if (!cursor) {
      items.splice(items.indexOf(here), 1)
      cursor = here
    } else if (!here) {
      items.push({ ...cursor, slot })
      cursor = null
    } else {
      const moved = Math.min(cursor.count, 64 - here.count)
      here.count += moved
      cursor = cursor.count > moved ? { ...cursor, count: cursor.count - moved } : null
    }
    bot.inventory.selectedItem = cursor
  }
}

test('craftItem: the result stacks are merged into one', async () => {
  const { bot, items } = setup({ items: inv(['bread', 34], ['bread', 1], ['bread', 1], ['wheat', 3]), blocks: tableBlocks })
  clickStub(items, bot)
  const r = await craftItem(bot, ctx, { item: 'bread' }, opts)
  assert.deepEqual(r, { status: 'crafted', item: 'bread', made: 1, used: { wheat: 3 } })
  assert.deepEqual(items.filter(i => i.name === 'bread').map(i => i.count), [37])
})

test('craftItem: a merge error still returns crafted', async () => {
  const { bot } = setup({ items: inv(['bread', 34], ['bread', 1], ['wheat', 3]), blocks: tableBlocks })
  bot.clickWindow = async () => { throw new Error('window gone') }
  const r = await craftItem(bot, ctx, { item: 'bread' }, opts)
  assert.deepEqual(r, { status: 'crafted', item: 'bread', made: 1, used: { wheat: 3 } })
})

test('craftItem: no merge while another window is open', async () => {
  const { bot, items } = setup({ items: inv(['bread', 34], ['bread', 1], ['wheat', 3]), blocks: tableBlocks })
  clickStub(items, bot)
  bot.currentWindow = { other: true }
  await craftItem(bot, ctx, { item: 'bread' }, opts)
  assert.equal(items.filter(i => i.name === 'bread').length, 2)
})

test('craftItem: resyncs the inventory window at entry', async () => {
  const { bot, state } = setup({ items: inv(['wheat', 3]), blocks: tableBlocks })
  await craftItem(bot, ctx, { item: 'bread' }, opts)
  assert.ok(state.syncs >= 1)
})

test('craftItem: a stale local view is fixed by the entry resync', async () => {
  const { bot, items } = setup({ items: [], blocks: tableBlocks })
  bot._syncWindow = async () => { if (!items.some(i => i.name === 'wheat')) items.push({ name: 'wheat', count: 6, type: ID.wheat, slot: 36 }) }
  const r = await craftItem(bot, ctx, { item: 'bread', count: 2 }, opts)
  assert.equal(r.status, 'crafted')
  assert.equal(r.made, 2)
})

test('craftItem: a batch that did not land is followed by a resync before the retry', async () => {
  const { bot, state } = setup({ items: inv(['wheat', 6]), blocks: tableBlocks, mode: 'noop' })
  const syncsAtCraft = []
  const craft = bot.craft
  bot.craft = async (...args) => { syncsAtCraft.push(state.syncs); return craft(...args) }
  await craftItem(bot, ctx, { item: 'bread', count: 1 }, opts)
  assert.ok(syncsAtCraft.length >= 2)
  assert.ok(syncsAtCraft[1] > syncsAtCraft[0])
})

test('craftItem: a cursor holding something at entry closes the open window before the resync', async () => {
  const { bot, state } = setup({ items: inv(['wheat', 3]), blocks: tableBlocks })
  const win = { id: 1 }
  const log = []
  bot.currentWindow = win
  bot.inventory.selectedItem = { name: 'wheat', count: 1 }
  bot.closeWindow = w => log.push(['close', w])
  bot._syncWindow = async () => { state.syncs++; log.push(['sync']) }
  await craftItem(bot, ctx, { item: 'bread' }, opts)
  assert.deepEqual(log[0], ['close', win])
  assert.deepEqual(log[1], ['sync'])
})
