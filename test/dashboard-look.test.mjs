// The look popup: a click on the small picture opens it big and live, fed by one event stream of frames from the
// dashboard until it is closed, its inventory asked for again a second after each answer; the small picture keeps its one look per click.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import vm from 'node:vm'

const html = fs.readFileSync(new URL('../tools/dashboard/index.html', import.meta.url), 'utf8')
const start = html.indexOf('const showLook')
const end = html.indexOf('// ---------------------------------------------------------------- the chat log', start)
const script = html.slice(start, end)

const settle = () => new Promise(resolve => setImmediate(resolve))

const page = (screen = { hp: 20, food: 20, xp: 0, oxygen: 20, armor: 0, slots: [], selected: 0, window: null }) => {
  const nodes = new Map()
  const el = id => {
    if (!nodes.has(id)) nodes.set(id, { id, hidden: true, src: '', textContent: '', innerHTML: '', className: '', style: { setProperty () {} }, listeners: {}, addEventListener (type, fn) { this.listeners[type] = fn } })
    return nodes.get(id)
  }
  const fetches = []
  const timers = []
  const streams = []
  class EventSource {
    constructor (url) { this.url = url; this.closed = false; streams.push(this) }
    close () { this.closed = true }
  }
  const context = {
    EventSource,
    el,
    selected: 'Chani',
    fetch: url => {
      fetches.push(url)
      if (url.startsWith('/api/screen/')) return Promise.resolve({ ok: true, json: async () => screen })
      return Promise.resolve({ ok: true, headers: { get: () => '' }, blob: async () => ({}) })
    },
    URL: { createObjectURL: () => 'blob:look', revokeObjectURL () {} },
    URLSearchParams,
    JSON,
    Date,
    setTimeout: (fn, ms) => { timers.push({ fn, ms }); return timers.length },
    clearTimeout: id => { if (timers[id - 1]) timers[id - 1].cleared = true },
    document: { addEventListener () {} }
  }
  vm.createContext(context)
  vm.runInContext(script, context)
  return { el, fetches, timers, streams }
}

test('look popup: a click on the picture opens one live stream and a 1 s inventory loop, shows each frame as it comes, and closing stops both', async () => {
  const { el, fetches, timers, streams } = page()
  assert.ok(start > 0 && end > start, 'the look script is where the test expects it')

  el('lookimg').listeners.click()
  assert.equal(el('lookOverlay').hidden, false)
  assert.deepEqual([streams.length, streams[0].url], [1, '/api/look/Chani/live?'])
  assert.equal(fetches.length, 1, 'only the inventory is fetched; the picture comes down the stream')
  assert.match(fetches[0], /^\/api\/screen\/Chani\?/)
  assert.equal(timers.length, 0, 'the next inventory ask waits for this one to arrive')

  streams[0].onmessage({ data: JSON.stringify({ png: 'AAAA', view: 'north pitch 0', seen: ['cow 3m @px1,2'], blocked: '' }) })
  assert.equal(el('lookBig').src, 'data:image/png;base64,AAAA')
  assert.equal(el('lookBig').hidden, false)
  assert.equal(el('lookBigMeta').textContent, 'Chani · north pitch 0\nsees: cow 3m @px1,2')

  await settle()
  assert.deepEqual(timers.map(t => t.ms), [1000])
  timers[0].fn()
  assert.equal(fetches.length, 2, 'a second later the inventory is asked for again')

  streams[0].onmessage({ data: JSON.stringify({ error: 'the body did not answer' }) })
  // the picture is already up (the AAAA frame above): an error afterwards only changes the caption, never hides it
  assert.deepEqual([el('lookBig').hidden, el('lookBigMeta').className, el('lookBigMeta').textContent], [false, 'err', 'Chani: the body did not answer'])

  el('lookClose').listeners.click()
  assert.deepEqual([el('lookOverlay').hidden, streams[0].closed, streams.length], [true, true, 1])
  await settle()
  assert.equal(timers.length, 1, 'no new inventory ask is armed after closing')
  assert.equal(fetches.length, 2)
})

test('look popup: a panorama streams as a panorama', () => {
  const { el, streams } = page()
  el('pano').checked = true
  el('lookimg').listeners.click()
  assert.equal(streams[0].url, '/api/look/Chani/live?pano=1')
})

test('look popup: the small picture still refreshes on its own through the same loader, without the inventory', async () => {
  const { el, fetches } = page()
  el('relook').listeners.click()
  assert.equal(fetches.length, 1)
  await settle()
  assert.equal(el('lookimg').src, 'blob:look')
  assert.equal(el('lookOverlay').hidden, true)
})

// the inventory screen: one cell per slot, by mineflayer's window slot number, as the game lays them out
const cells = html => Object.fromEntries([...html.matchAll(/<div class="([^"]*)" data-slot="(\d+)"[^>]*>(.*?)<\/div>/g)].map(([, cls, slot, inside]) => [slot, { cls, inside }]))
const carrying = {
  hp: 13, food: 7, xp: 5, oxygen: 20, armor: 5,
  slots: [{ slot: 5, name: 'iron_helmet', count: 1 }, { slot: 9, name: 'oak_log', count: 12 }, { slot: 36, name: 'iron_pickaxe', count: 1 }, { slot: 44, name: 'bread', count: 3 }, { slot: 45, name: 'shield', count: 1 }],
  selected: 0,
  window: null
}
const shown = async screen => {
  const { el } = page(screen)
  el('lookimg').listeners.click()
  await settle()
  return { el, cells: cells(el('lookInventory').innerHTML) }
}

test('inventory screen: every slot is drawn, armour, offhand, the main grid and the hotbar', async () => {
  const { cells: drawn } = await shown(carrying)
  assert.deepEqual(Object.keys(drawn).map(Number).sort((a, b) => a - b), [...Array(41).keys()].map(i => i + 5))
})

for (const [name, slot, expected] of [
  ['a stack shows its icon and its count', 9, { cls: 'slot', inside: '<img src="/api/icon/oak_log" data-item="oak_log" alt=""><span class="count">12</span>' }],
  ['a single item shows no count', 5, { cls: 'slot', inside: '<img src="/api/icon/iron_helmet" data-item="iron_helmet" alt="">' }],
  ['the selected hotbar slot is marked', 36, { cls: 'slot selected', inside: '<img src="/api/icon/iron_pickaxe" data-item="iron_pickaxe" alt="">' }],
  ['the rest of the hotbar is not', 44, { cls: 'slot', inside: '<img src="/api/icon/bread" data-item="bread" alt=""><span class="count">3</span>' }],
  ['the offhand', 45, { cls: 'slot', inside: '<img src="/api/icon/shield" data-item="shield" alt="">' }],
  ['an empty slot is empty', 10, { cls: 'slot', inside: '' }]
]) {
  test(`inventory screen: ${name}`, async () => assert.deepEqual((await shown(carrying)).cells[slot], expected))
}

test('inventory screen: an item with no picture shows its initials, then and on every later draw', async () => {
  const screen = { ...carrying }
  const { el, fetches, timers } = page(screen)
  el('lookimg').listeners.click()
  await settle()
  const img = { dataset: { item: 'iron_pickaxe' }, outerHTML: '' }
  el('lookInventory').listeners.error({ target: img })
  assert.equal(img.outerHTML, '<span class="abbr">IP</span>')
  screen.slots = [...carrying.slots, { slot: 11, name: 'dirt', count: 2 }]
  timers.at(-1).fn()
  await settle()
  assert.equal(fetches.length, 2)
  assert.equal(cells(el('lookInventory').innerHTML)[36].inside, '<span class="abbr">IP</span>')
})

test('inventory screen: an unchanged answer leaves the screen alone, a changed one draws it again', async () => {
  const screen = { ...carrying }
  const { el, timers } = page(screen)
  el('lookimg').listeners.click()
  await settle()
  el('lookInventory').innerHTML = 'as drawn'
  timers.at(-1).fn()
  await settle()
  assert.equal(el('lookInventory').innerHTML, 'as drawn')
  screen.selected = 1
  timers.at(-1).fn()
  await settle()
  assert.equal(cells(el('lookInventory').innerHTML)[37].cls, 'slot selected')
})

// the HUD: ten-icon rows of hearts, food, armour and air, drawn as the game draws them
const hud = async screen => { const { el } = await shown(screen); return el('lookHud').innerHTML }
const count = (html, cls) => (html.match(new RegExp(`class="${cls}"`, 'g')) ?? []).length
test('hud: 13 hp is six full hearts, a half and three empty; 7 food three full drumsticks, a half and six empty', async () => {
  const html = await hud(carrying)
  assert.deepEqual([count(html, 'heart full'), count(html, 'heart half'), count(html, 'heart empty')], [6, 1, 3])
  assert.deepEqual([count(html, 'food full'), count(html, 'food half'), count(html, 'food empty')], [3, 1, 6])
})
test('hud: armour points show as chestplates only when worn; air bubbles only under water; the xp level as a number', async () => {
  const html = await hud(carrying)
  assert.deepEqual([count(html, 'armor full'), count(html, 'armor half'), count(html, 'bubble full'), count(html, 'bubble empty')], [2, 1, 0, 0])
  assert.match(html, /class="xp">5</)
  const dry = await hud({ ...carrying, armor: 0, oxygen: 20 })
  assert.equal(count(dry, 'armor full') + count(dry, 'armor empty'), 0)
  const wet = await hud({ ...carrying, oxygen: 11 })
  assert.deepEqual([count(wet, 'bubble full'), count(wet, 'bubble half'), count(wet, 'bubble empty')], [5, 1, 4])
})

const chest = { type: 'minecraft:generic_9x3', title: 'chest', at: { x: 96, y: 70, z: -79 }, size: 27, open: true, closedAt: null, slots: [{ slot: 1, name: 'bread', count: 3 }, { slot: 26, name: 'dirt', count: 64 }] }
const windowCells = async window => { const { el } = await shown({ ...carrying, window }); return { cells: cells(el('lookWindow').innerHTML), meta: el('lookWindowMeta').textContent } }
test('container: a chest draws 27 cells in three rows of nine with its stacks, captioned with its place', async () => {
  const { cells: drawn, meta } = await windowCells(chest)
  assert.equal(Object.keys(drawn).length, 27)
  assert.equal(drawn[26].inside, '<img src="/api/icon/dirt" data-item="dirt" alt=""><span class="count">64</span>')
  assert.equal(meta, 'chest at 96,70,-79')
})
test('container: closed a moment ago says so', async () => {
  const { meta } = await windowCells({ ...chest, open: false, closedAt: Date.now() - 3000 })
  assert.equal(meta, 'chest at 96,70,-79 · closed 3 s ago')
})
test('container: none open draws nothing', async () => {
  const { el } = await shown(carrying)
  assert.deepEqual([el('lookWindow').innerHTML, el('lookWindowMeta').textContent], ['', ''])
})
for (const [type, size, slot, place] of [
  ['minecraft:generic_9x6', 54, 53, '6/9'],
  ['minecraft:hopper', 5, 4, '1/5'],
  ['minecraft:furnace', 3, 2, '2/5'],   // input top-left, fuel below it, the output to the right
  ['minecraft:furnace', 3, 1, '3/1'],
  ['minecraft:generic_3x3', 9, 8, '3/3'],
  ['minecraft:crafting', 10, 0, '2/5'],  // the result, right of the 3x3
  ['minecraft:beacon', 1, 0, '1/1']
]) test(`container: ${type} slot ${slot} sits at ${place}`, async () => {
  const { el } = await shown({ ...carrying, window: { ...chest, type, size, slots: [{ slot, name: 'coal', count: 1 }] } })
  assert.match(el('lookWindow').innerHTML, new RegExp(`data-slot="${slot}" style="grid-area:${place}"`))
})
