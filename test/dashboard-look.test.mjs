// The look popup: a click on the small picture opens it big and live, fed by one event stream of frames from the
// dashboard until it is closed; the small picture keeps its one look per click.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import vm from 'node:vm'

const html = fs.readFileSync(new URL('../tools/dashboard/index.html', import.meta.url), 'utf8')
const start = html.indexOf('const showLook')
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
  const streams = []
  class EventSource {
    constructor (url) { this.url = url; this.closed = false; streams.push(this) }
    close () { this.closed = true }
  }
  const context = {
    EventSource,
    el,
    selected: 'Chani',
    fetch: url => { fetches.push(url); return Promise.resolve({ ok: true, headers: { get: () => '' }, blob: async () => ({}) }) },
    URL: { createObjectURL: () => 'blob:look', revokeObjectURL () {} },
    URLSearchParams,
    JSON,
    Date,
    document: { addEventListener () {} }
  }
  vm.createContext(context)
  vm.runInContext(script, context)
  return { el, fetches, streams }
}

test('look popup: a click on the picture opens one live stream, shows each frame as it comes, and closes it', () => {
  const { el, fetches, streams } = page()
  assert.ok(start > 0 && end > start, 'the look script is where the test expects it')

  el('lookimg').listeners.click()
  assert.equal(el('lookOverlay').hidden, false)
  assert.deepEqual([streams.length, fetches.length], [1, 0])
  assert.equal(streams[0].url, '/api/look/Chani/live?')

  streams[0].onmessage({ data: JSON.stringify({ png: 'AAAA', view: 'north pitch 0', seen: ['cow 3m @px1,2'], blocked: '' }) })
  assert.equal(el('lookBig').src, 'data:image/png;base64,AAAA')
  assert.equal(el('lookBig').hidden, false)
  assert.equal(el('lookBigMeta').textContent, 'Chani · north pitch 0\nsees: cow 3m @px1,2')

  streams[0].onmessage({ data: JSON.stringify({ error: 'the body did not answer' }) })
  assert.deepEqual([el('lookBig').hidden, el('lookBigMeta').className, el('lookBigMeta').textContent], [true, 'err', 'Chani: the body did not answer'])

  el('lookClose').listeners.click()
  assert.deepEqual([el('lookOverlay').hidden, streams[0].closed, streams.length], [true, true, 1])
})

test('look popup: a panorama streams as a panorama', () => {
  const { el, streams } = page()
  el('pano').checked = true
  el('lookimg').listeners.click()
  assert.equal(streams[0].url, '/api/look/Chani/live?pano=1')
})

test('look popup: the small picture still refreshes on its own through the same loader', async () => {
  const { el, fetches } = page()
  el('relook').listeners.click()
  assert.equal(fetches.length, 1)
  await settle()
  assert.equal(el('lookimg').src, 'blob:look')
  assert.equal(el('lookOverlay').hidden, true)
})
