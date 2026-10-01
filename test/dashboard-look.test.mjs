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

const page = (inventory = { items: {}, freeSlots: 36, armor: {}, slots: [], selected: 0 }) => {
  const nodes = new Map()
  const el = id => {
    if (!nodes.has(id)) nodes.set(id, { id, hidden: true, src: '', textContent: '', innerHTML: '', className: '', listeners: {}, addEventListener (type, fn) { this.listeners[type] = fn } })
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
      if (url.startsWith('/api/screen/')) return Promise.resolve({ ok: true, json: async () => inventory })
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
  assert.deepEqual([el('lookBig').hidden, el('lookBigMeta').className, el('lookBigMeta').textContent], [true, 'err', 'Chani: the body did not answer'])

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
  items: { iron_helmet: 1, oak_log: 12, iron_pickaxe: 1, bread: 3, shield: 1 },
  freeSlots: 32,
  armor: { head: 'iron_helmet' },
  slots: [{ slot: 5, name: 'iron_helmet', count: 1 }, { slot: 9, name: 'oak_log', count: 12 }, { slot: 36, name: 'iron_pickaxe', count: 1 }, { slot: 44, name: 'bread', count: 3 }, { slot: 45, name: 'shield', count: 1 }],
  selected: 0
}
const shown = async inventory => {
  const { el } = page(inventory)
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
  const inventory = { ...carrying }
  const { el, fetches, timers } = page(inventory)
  el('lookimg').listeners.click()
  await settle()
  const img = { dataset: { item: 'iron_pickaxe' }, outerHTML: '' }
  el('lookInventory').listeners.error({ target: img })
  assert.equal(img.outerHTML, '<span class="abbr">IP</span>')
  inventory.slots = [...carrying.slots, { slot: 11, name: 'dirt', count: 2 }]
  timers.at(-1).fn()
  await settle()
  assert.equal(fetches.length, 2)
  assert.equal(cells(el('lookInventory').innerHTML)[36].inside, '<span class="abbr">IP</span>')
})

test('inventory screen: an unchanged answer leaves the screen alone, a changed one draws it again', async () => {
  const inventory = { ...carrying }
  const { el, timers } = page(inventory)
  el('lookimg').listeners.click()
  await settle()
  el('lookInventory').innerHTML = 'as drawn'
  timers.at(-1).fn()
  await settle()
  assert.equal(el('lookInventory').innerHTML, 'as drawn')
  inventory.selected = 1
  timers.at(-1).fn()
  await settle()
  assert.equal(cells(el('lookInventory').innerHTML)[37].cls, 'slot selected')
})
