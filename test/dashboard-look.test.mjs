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

const page = (inventory = { items: {}, freeSlots: 36, armor: {} }) => {
  const nodes = new Map()
  const el = id => {
    if (!nodes.has(id)) nodes.set(id, { id, hidden: true, src: '', textContent: '', className: '', listeners: {}, addEventListener (type, fn) { this.listeners[type] = fn } })
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
    snap: { bodies: [{ name: 'Chani', state: { holding: 'iron_pickaxe' } }] },
    fetch: url => {
      fetches.push(url)
      if (url.startsWith('/api/inventory/')) return Promise.resolve({ ok: true, json: async () => inventory })
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
  assert.match(fetches[0], /^\/api\/inventory\/Chani\?/)
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

test('look popup: the inventory shows item x count, marks the held item, and lists armour worn', async () => {
  const { el } = page({
    items: { oak_log: 12, iron_pickaxe: 1, bread: 3 },
    freeSlots: 20,
    armor: { head: 'leather_helmet', torso: null, legs: null, feet: 'iron_boots' }
  })
  el('lookimg').listeners.click()
  await settle()
  assert.equal(el('lookInventory').textContent, 'bread ×3\n> iron_pickaxe ×1\noak_log ×12\nwearing head: leather_helmet, feet: iron_boots')
})

test('look popup: an empty inventory and no armour says so plainly', async () => {
  const { el } = page({ items: {}, freeSlots: 36, armor: {} })
  el('lookimg').listeners.click()
  await settle()
  assert.equal(el('lookInventory').textContent, 'inventory empty')
})
