// The look popup: a click on the small picture opens it big, and it asks for a fresh look one second after each one
// arrives (never before, so a slow render does not queue up behind itself) until it is closed.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import vm from 'node:vm'

const html = fs.readFileSync(new URL('../tools/dashboard/index.html', import.meta.url), 'utf8')
const start = html.indexOf('const loadLook')
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
  const context = {
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
  return { el, fetches, timers }
}

test('look popup: a click on the picture opens it and keeps the picture and the inventory fresh a second after each look, until closed', async () => {
  const { el, fetches, timers } = page()
  assert.ok(start > 0 && end > start, 'the look script is where the test expects it')

  el('lookimg').listeners.click()
  assert.equal(el('lookOverlay').hidden, false)
  assert.equal(fetches.length, 2, 'the picture and the inventory are both asked for at once')
  assert.match(fetches[0], /^\/api\/look\/Chani\?/)
  assert.match(fetches[1], /^\/api\/inventory\/Chani\?/)
  assert.equal(timers.length, 0, 'the next look waits for these to arrive')

  await settle()
  assert.equal(el('lookBig').src, 'blob:look')
  assert.equal(el('lookBig').hidden, false)
  assert.deepEqual(timers.map(t => t.ms), [1000])

  timers[0].fn()
  assert.equal(fetches.length, 4, 'a second round asks for both again')

  el('lookClose').listeners.click()
  assert.equal(el('lookOverlay').hidden, true)
  await settle()
  assert.equal(timers.length, 1, 'no new look is armed after closing')
  assert.equal(fetches.length, 4)
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
