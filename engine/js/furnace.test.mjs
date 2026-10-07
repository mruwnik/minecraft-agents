// Why JavaScript: tests furnace.mjs, which stays JS: Mineflayer boundary; drives the furnace window and its bar packets, which are collected before Mineflayer listens.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { stubBot } from './stub-bot.mjs'
import { furnaceVisit } from './furnace.mjs'

const REG = Object.fromEntries(['coal', 'raw_iron', 'iron_ingot', 'beef', 'cooked_beef', 'oak_planks', 'cobblestone', 'ender_pearl']
  .map((name, i) => [name, { id: i + 1, name, stackSize: name === 'ender_pearl' ? 16 : 64 }]))
const NAME = Object.fromEntries(Object.values(REG).map(i => [i.id, i.name]))
const ACCEPTS = { input: ['raw_iron', 'beef', 'cobblestone'], fuel: ['coal', 'oak_planks'] }
const SLOT = { input: 0, fuel: 1, output: 2 }
const at = { x: 3, y: 64, z: 0 }
const ctx = { alive: () => {}, onAbort: () => {} }

const stack = ([name, count]) => ({ name, count })
const size = name => REG[name].stackSize
const filler = n => Array.from({ length: n }, (_, i) => [`item_${i}`, 1])

// a furnace window the way mineflayer's openFurnace hands it out: slots 0 input, 1 fuel, 2 output, fractions for the
// two bars, and put/take methods that move stacks between the window and the pockets; the server drops what a slot
// does not take silently, and a take moves what fits (shift-click)
const setup = ({ block = 'furnace', carried = [], slots = {}, bars = {}, lit, accepts = ACCEPTS, far = false, aliveAfter = Infinity, eatsFuel = false } = {}) => {
  const bot = stubBot({ blocks: { '3,64,0': block }, props: lit === undefined ? {} : { '3,64,0': { lit } } })
  const pockets = carried.map(stack)
  // the pockets are what the window's inventory part (slots 3..38) shows, as on the server
  const win = {
    id: 7,
    inventoryStart: 3,
    inventoryEnd: 39,
    slots: Object.assign(Array(39).fill(null), Object.fromEntries(Object.entries(slots).map(([k, v]) => [SLOT[k], stack(v)])))
  }
  const sync = () => { for (let i = 0; i < 36; i++) win.slots[3 + i] = pockets[i] ?? null }
  const calls = []
  const puts = []
  sync()
  const room = name => pockets.filter(i => i.name === name).reduce((sum, i) => sum + size(name) - i.count, 0) + (pockets.length < 36 ? size(name) : 0)
  const give = (name, count) => {
    const moved = Math.min(count, room(name))
    let left = moved
    for (const s of pockets.filter(i => i.name === name)) { const n = Math.min(left, size(name) - s.count); s.count += n; left -= n }
    if (left > 0) pockets.push({ name, count: left })
    return moved
  }
  const put = (part, label) => async (type, meta, count) => {
    calls.push(label)
    puts.push(count)
    const name = NAME[type]
    const have = pockets.find(i => i.name === name)
    if (!accepts[part].includes(name) || !have) return
    const held = win.slots[SLOT[part]]
    if (held && held.name !== name) return
    const n = Math.min(count, have.count, size(name) - (held?.count ?? 0))
    have.count -= n
    if (have.count === 0) pockets.splice(pockets.indexOf(have), 1)
    if (!(eatsFuel && part === 'fuel')) win.slots[SLOT[part]] = { name, count: (held?.count ?? 0) + n }
    sync()
  }
  const takeFrom = (part, label) => async () => {
    calls.push(label)
    const held = win.slots[SLOT[part]]
    const moved = give(held.name, held.count)
    held.count -= moved
    if (held.count === 0) win.slots[SLOT[part]] = null
    sync()
  }
  Object.assign(win, {
    putInput: put('input', 'putInput'),
    putFuel: put('fuel', 'putFuel'),
    takeInput: takeFrom('input', 'takeInput'),
    takeFuel: takeFrom('fuel', 'takeFuel'),
    takeOutput: takeFrom('output', 'takeOutput')
  })
  Object.assign(bot.registry, { itemsByName: REG, items: Object.fromEntries(Object.values(REG).map(i => [i.id, i])) })
  // the totals come with the open itself, the running values a moment later, as on the server
  const bar = (property, value, windowId = win.id) => bot._client.emit('craft_progress_bar', { windowId, property, value })
  bot.openFurnace = async () => {
    calls.push('open')
    for (const [property, name] of [[1, 'burnTotal'], [3, 'cookTotal']]) if (bars[name] !== undefined) bar(property, bars[name])
    await Promise.resolve()
    setTimeout(() => { for (const [property, name] of [[0, 'burnLeft'], [2, 'cookDone']]) if (bars[name] !== undefined) bar(property, bars[name]); bar(0, 5, 99) }, 1)
    return win
  }
  win.close = () => calls.push('close')
  let alives = 0
  const c = { alive: () => { if (++alives > aliveAfter) throw Object.assign(new Error('cut'), { code: 'cut' }) }, onAbort: fn => { c.abort = fn } }
  const options = { distanceTo: () => (far ? 9.5714 : 2), reach: 4.5, settle: async () => { calls.push('settle'); await new Promise(resolve => setTimeout(resolve, 5)) } }
  const visit = a => furnaceVisit(bot, c, { pos: at, ...a }, options)
  return { visit, pockets, win, calls, puts, bot, c }
}

const view = pockets => Object.fromEntries(pockets.map(i => [i.name, i.count]))

test('read: stacks, bars and the kind of furnace', async () => {
  const { visit, calls } = setup({
    slots: { input: ['raw_iron', 3], fuel: ['coal', 1], output: ['iron_ingot', 2] },
    bars: { burnTotal: 1600, burnLeft: 800, cookTotal: 200, cookDone: 50 },
    lit: true
  })
  assert.deepEqual(await visit({ op: 'read' }), {
    status: 'ok',
    kind: 'furnace',
    input: { name: 'raw_iron', count: 3 },
    fuel: { name: 'coal', count: 1 },
    output: { name: 'iron_ingot', count: 2 },
    lit: true,
    burn: { left: 800, total: 1600 },
    cook: { done: 50, total: 200 }
  })
  assert.deepEqual(calls, ['open', 'settle', 'close'])
})

test('read: only the totals came, so nothing burns and nothing cooks', async () => {
  const { visit } = setup({ bars: { burnTotal: 1600, cookTotal: 200 }, lit: false })
  const r = await visit({ op: 'read' })
  assert.deepEqual([r.lit, r.burn, r.cook], [false, { left: 0, total: 1600 }, { done: 0, total: 200 }])
})

test('read: a burning bar says lit although the block has not turned lit yet', async () => {
  const { visit } = setup({ bars: { burnTotal: 1600, burnLeft: 1597 }, lit: false })
  assert.equal((await visit({ op: 'read' })).lit, true)
})

test('read: without a lit property on the block the burning bar says whether it is lit', async () => {
  const { visit } = setup({ bars: { burnTotal: 1600, burnLeft: 3 } })
  assert.equal((await visit({ op: 'read' })).lit, true)
})

test('read: the block says lit even when no bar arrived', async () => {
  const { visit } = setup({ lit: true })
  const r = await visit({ op: 'read' })
  assert.deepEqual([r.lit, r.burn], [true, null])
})

test('the bars are only listened for during the visit', async () => {
  const { visit, bot } = setup()
  await visit({ op: 'read' })
  assert.equal(bot._client.listenerCount('craft_progress_bar'), 0)
})

test('read: an empty furnace whose bars never arrived says null for them', async () => {
  const { visit } = setup()
  assert.deepEqual(await visit({ op: 'read' }), { status: 'ok', kind: 'furnace', input: null, fuel: null, output: null, lit: false, burn: null, cook: null })
})

for (const kind of ['furnace', 'blast_furnace', 'smoker']) {
  test(`read: a ${kind}`, async () => {
    const { visit } = setup({ block: kind })
    assert.equal((await visit({ op: 'read' })).kind, kind)
  })
}

const refusals = [
  { label: 'no block', setup: { block: 'air' }, expect: { status: 'missing', block: 'air' } },
  { label: 'another container', setup: { block: 'chest' }, expect: { status: 'cannot', reason: 'not-a-furnace', block: 'chest' } },
  { label: 'too far', setup: { far: true }, expect: { status: 'unreachable', reason: 'too-far', distance: 9.57 } }
]
for (const op of ['read', 'load', 'take']) {
  for (const r of refusals) {
    test(`${op}: ${r.label} is refused as data, with no window opened`, async () => {
      const { visit, calls } = setup(r.setup)
      assert.deepEqual(await visit({ op, input: { item: 'raw_iron', count: 1 } }), r.expect)
      assert.deepEqual(calls, [])
    })
  }
}

test('load: input and fuel go in, the result says what moved and what the furnace holds', async () => {
  const { visit, pockets, calls } = setup({ carried: [['raw_iron', 5], ['coal', 3]] })
  const r = await visit({ op: 'load', input: { item: 'raw_iron', count: 3 }, fuel: { item: 'coal', count: 1 } })
  assert.deepEqual(r, {
    status: 'ok',
    kind: 'furnace',
    moved: { input: 3, fuel: 1 },
    input: { name: 'raw_iron', count: 3 },
    fuel: { name: 'coal', count: 1 },
    output: null,
    lit: false,
    burn: null,
    cook: null
  })
  assert.deepEqual(view(pockets), { raw_iron: 2, coal: 2 })
  assert.deepEqual(calls, ['open', 'putInput', 'settle', 'putFuel', 'settle', 'close'])
})

test('load: one fuel item that burns at once never shows in the slot and still counts as moved', async () => {
  const { visit } = setup({ carried: [['raw_iron', 1], ['coal', 3]], eatsFuel: true })
  const r = await visit({ op: 'load', input: { item: 'raw_iron', count: 1 }, fuel: { item: 'coal', count: 1 } })
  assert.deepEqual([r.status, r.moved, r.fuel], ['ok', { input: 1, fuel: 1 }, null])
})

test('load: fuel alone, or input alone', async () => {
  const { visit } = setup({ carried: [['raw_iron', 5], ['coal', 3]] })
  assert.deepEqual((await visit({ op: 'load', fuel: { item: 'coal', count: 2 } })).moved, { fuel: 2 })
  assert.deepEqual((await visit({ op: 'load', input: { item: 'raw_iron', count: 4 } })).moved, { input: 4 })
})

test('load: asking for more than is carried loads what is carried', async () => {
  const { visit } = setup({ carried: [['raw_iron', 2]] })
  assert.deepEqual((await visit({ op: 'load', input: { item: 'raw_iron', count: 5 } })).moved, { input: 2 })
})

test('load: the click asks for what is carried, never more than that', async () => {
  const { visit, puts } = setup({ carried: [['raw_iron', 2]] })
  await visit({ op: 'load', input: { item: 'raw_iron', count: 5 } })
  assert.deepEqual(puts, [2])
})

test('load: onto the same item already there adds to it, up to the stack', async () => {
  const { visit } = setup({ carried: [['raw_iron', 10]], slots: { input: ['raw_iron', 60] } })
  const r = await visit({ op: 'load', input: { item: 'raw_iron', count: 10 } })
  assert.deepEqual([r.moved, r.input], [{ input: 4 }, { name: 'raw_iron', count: 64 }])
})

const loadRefusals = [
  { label: 'item not carried', setup: { carried: [['coal', 1]] }, args: { input: { item: 'raw_iron', count: 1 } }, expect: { status: 'no-item', slot: 'input', item: 'raw_iron' } },
  { label: 'fuel not carried', setup: { carried: [['raw_iron', 1]] }, args: { input: { item: 'raw_iron', count: 1 }, fuel: { item: 'coal', count: 1 } }, expect: { status: 'no-item', slot: 'fuel', item: 'coal' } },
  { label: 'input slot holds another item', setup: { carried: [['raw_iron', 1]], slots: { input: ['beef', 2] } }, args: { input: { item: 'raw_iron', count: 1 } }, expect: { status: 'busy', slot: 'input', holds: { name: 'beef', count: 2 } } },
  { label: 'fuel slot holds another item', setup: { carried: [['coal', 1]], slots: { fuel: ['oak_planks', 7] } }, args: { fuel: { item: 'coal', count: 1 } }, expect: { status: 'busy', slot: 'fuel', holds: { name: 'oak_planks', count: 7 } } },
  { label: 'a smoker takes no iron', setup: { block: 'smoker', carried: [['raw_iron', 1]], accepts: { input: ['beef'], fuel: ['coal'] } }, args: { input: { item: 'raw_iron', count: 1 } }, expect: { status: 'rejected', slot: 'input', item: 'raw_iron', reason: 'not-accepted' } },
  { label: 'not a fuel', setup: { carried: [['cobblestone', 1]] }, args: { fuel: { item: 'cobblestone', count: 1 } }, expect: { status: 'rejected', slot: 'fuel', item: 'cobblestone', reason: 'not-accepted' } }
]
for (const r of loadRefusals) {
  test(`load: ${r.label} is refused as data, nothing moves`, async () => {
    const { visit, pockets, calls } = setup(r.setup)
    const before = view(pockets)
    assert.deepEqual(await visit({ op: 'load', ...r.args }), r.expect)
    assert.deepEqual(view(pockets), before)
    assert.equal(calls.at(-1), 'close')
  })
}

test('load: a slot already full of the item takes no more and says so', async () => {
  const { visit } = setup({ carried: [['raw_iron', 1]], slots: { input: ['raw_iron', 64] } })
  assert.deepEqual(await visit({ op: 'load', input: { item: 'raw_iron', count: 1 } }), { status: 'rejected', slot: 'input', item: 'raw_iron', reason: 'slot-full' })
})

test('load: a refusal in either slot is found before anything is put in', async () => {
  const { visit, calls } = setup({ carried: [['raw_iron', 1], ['coal', 1]], slots: { fuel: ['oak_planks', 7] } })
  await visit({ op: 'load', input: { item: 'raw_iron', count: 1 }, fuel: { item: 'coal', count: 1 } })
  assert.deepEqual(calls.filter(c => c.startsWith('put')), [])
})

test('load: an input the furnace will not take stops before the fuel goes in', async () => {
  const { visit, calls } = setup({ carried: [['cobblestone', 1], ['coal', 1]], accepts: { input: [], fuel: ['coal'] } })
  const r = await visit({ op: 'load', input: { item: 'cobblestone', count: 1 }, fuel: { item: 'coal', count: 1 } })
  assert.equal(r.status, 'rejected')
  assert.deepEqual(calls.filter(c => c.startsWith('put')), ['putInput'])
})

test('take: the output comes out by default, input and fuel stay', async () => {
  const { visit, pockets, calls } = setup({ slots: { input: ['raw_iron', 1], fuel: ['coal', 1], output: ['iron_ingot', 3] } })
  const r = await visit({ op: 'take' })
  assert.deepEqual([r.status, r.taken, r.output], ['ok', [{ part: 'output', name: 'iron_ingot', count: 3 }], null])
  assert.deepEqual(r.input, { name: 'raw_iron', count: 1 })
  assert.deepEqual(view(pockets), { iron_ingot: 3 })
  assert.deepEqual(calls, ['open', 'takeOutput', 'settle', 'close'])
})

test('take: leftover input and fuel on request', async () => {
  const { visit, pockets } = setup({ slots: { input: ['raw_iron', 1], fuel: ['coal', 2], output: ['iron_ingot', 3] } })
  const r = await visit({ op: 'take', input: true, fuel: true })
  assert.deepEqual(r.taken.map(t => t.part), ['output', 'input', 'fuel'])
  assert.deepEqual(view(pockets), { iron_ingot: 3, raw_iron: 1, coal: 2 })
})

test('take: nothing there is an empty take, not a failure', async () => {
  const { visit, calls } = setup()
  const r = await visit({ op: 'take', input: true, fuel: true })
  assert.deepEqual([r.status, r.taken], ['ok', []])
  assert.deepEqual(calls.filter(c => c.startsWith('take')), [])
})

test('take: with nothing to take it still waits for the bars before it reads them', async () => {
  const { visit } = setup({ bars: { burnTotal: 1600, burnLeft: 700 } })
  const r = await visit({ op: 'take' })
  assert.deepEqual([r.burn, r.lit], [{ left: 700, total: 1600 }, true])
})

test('take: no room at all is refused before any click', async () => {
  const { visit, calls, win } = setup({ carried: [...filler(35), ['coal', 64]], slots: { output: ['iron_ingot', 3] } })
  const r = await visit({ op: 'take' })
  assert.deepEqual([r.status, r.taken, win.slots[2]], ['full', [], { name: 'iron_ingot', count: 3 }])
  assert.deepEqual(calls.filter(c => c.startsWith('take')), [])
})

test('take: room for part of the stack takes that part and says full', async () => {
  const { visit, win } = setup({ carried: [...filler(35), ['iron_ingot', 60]], slots: { output: ['iron_ingot', 10] } })
  const r = await visit({ op: 'take' })
  assert.deepEqual([r.status, r.taken, win.slots[2]], ['full', [{ part: 'output', name: 'iron_ingot', count: 4 }], { name: 'iron_ingot', count: 6 }])
})

test('a read cut right after the open is cut', async () => {
  const { visit, calls } = setup({ aliveAfter: 0 })
  await assert.rejects(visit({ op: 'read' }), { code: 'cut' })
  assert.equal(calls.at(-1), 'close')
})

test('take: a full stack of a 16-stack item leaves no room for another and no click is made', async () => {
  const { visit, calls } = setup({ carried: [...filler(35), ['ender_pearl', 16]], slots: { output: ['ender_pearl', 3] } })
  const r = await visit({ op: 'take' })
  assert.equal(r.status, 'full')
  assert.deepEqual(calls.filter(c => c.startsWith('take')), [])
})

test('the window is closed when the call is cut inside it', async () => {
  const { visit, calls } = setup({ carried: [['raw_iron', 1]], aliveAfter: 0 })
  await assert.rejects(visit({ op: 'load', input: { item: 'raw_iron', count: 1 } }), { code: 'cut' })
  assert.equal(calls.at(-1), 'close')
})

test('a cut registers the window close for the abort path', async () => {
  const { visit, calls, c } = setup()
  await visit({ op: 'read' })
  c.abort()
  assert.deepEqual(calls.slice(-2), ['close', 'close'])
})
