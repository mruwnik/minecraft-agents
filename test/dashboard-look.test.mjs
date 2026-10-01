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

const page = () => {
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
    fetch: url => { fetches.push(url); return Promise.resolve({ ok: true, headers: { get: () => '' }, blob: async () => ({}) }) },
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

test('look popup: a click on the picture opens it and keeps it fresh a second after each look, until closed', async () => {
  const { el, fetches, timers } = page()
  assert.ok(start > 0 && end > start, 'the look script is where the test expects it')

  el('lookimg').listeners.click()
  assert.equal(el('lookOverlay').hidden, false)
  assert.equal(fetches.length, 1)
  assert.match(fetches[0], /^\/api\/look\/Chani\?/)
  assert.equal(timers.length, 0, 'the next look waits for this one to arrive')

  await settle()
  assert.equal(el('lookBig').src, 'blob:look')
  assert.equal(el('lookBig').hidden, false)
  assert.deepEqual(timers.map(t => t.ms), [1000])

  timers[0].fn()
  assert.equal(fetches.length, 2)

  el('lookClose').listeners.click()
  assert.equal(el('lookOverlay').hidden, true)
  await settle()
  assert.equal(timers.length, 1, 'no new look is armed after closing')
  assert.equal(fetches.length, 2)
})

test('look popup: the small picture still refreshes on its own through the same loader', async () => {
  const { el, fetches } = page()
  el('relook').listeners.click()
  assert.equal(fetches.length, 1)
  await settle()
  assert.equal(el('lookimg').src, 'blob:look')
  assert.equal(el('lookOverlay').hidden, true)
})
