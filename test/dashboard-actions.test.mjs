// The left-side action log: a per-body scrolling feed, swapped by select() and refreshed on the same 2 s poll
// as the rest of the page. Sliced from index.html the same way test/dashboard-look.test.mjs slices the look popup.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import vm from 'node:vm'

const html = fs.readFileSync(new URL('../tools/dashboard/index.html', import.meta.url), 'utf8')
const start = html.indexOf('const select = name =>')
const end = html.indexOf('// ---------------------------------------------------------------- the chat log', start)
const script = html.slice(start, end)

const settle = () => new Promise(resolve => setImmediate(resolve))

const page = () => {
  const nodes = new Map()
  const el = id => {
    if (!nodes.has(id)) nodes.set(id, { id, hidden: true, innerHTML: '', textContent: '', scrollTop: 0, clientHeight: 0, scrollHeight: 0, listeners: {}, addEventListener (type, fn) { this.listeners[type] = fn } })
    return nodes.get(id)
  }
  const fetches = []
  const pending = []
  const context = {
    el,
    selected: null,
    renderList: () => {},
    draw: () => {},
    fetch: url => {
      fetches.push(url)
      let resolve
      const promise = new Promise(r => { resolve = r })
      pending.push({ url, resolve })
      return promise
    },
    URLSearchParams,
    JSON,
    Date,
    document: { addEventListener () {} }
  }
  vm.createContext(context)
  vm.runInContext(script, context)
  const select = name => vm.runInContext(`select(${JSON.stringify(name)})`, context)
  const run = (name, json) => pending.find(p => p.url === `/api/actions/${name}`).resolve({ json: async () => json })
  return { el, fetches, pending, select, run }
}

test('actions panel: hidden while nothing is selected, even if a poll fires', () => {
  const { el, select } = page()
  assert.equal(el('actions').hidden, true)
  select(null)
  assert.equal(el('actions').hidden, true)
})

test('actions panel: selecting a body fetches its log and shows the entries', async () => {
  const { el, fetches, select, run } = page()
  select('Chani')
  assert.equal(fetches[0], '/api/actions/Chani')
  run('Chani', { entries: [{ t: new Date().toISOString(), type: 'job_started', gist: 'goto x=1', bad: false }] })
  await settle()
  assert.equal(el('actions').hidden, false)
  assert.match(el('actionsLog').innerHTML, /goto x=1/)
})

test('actions panel: a late answer for the previous body is dropped', async () => {
  const { el, select, run, pending } = page()
  select('A')
  select('B')
  assert.deepEqual(pending.map(p => p.url), ['/api/actions/A', '/api/actions/B'])
  run('B', { entries: [{ t: new Date().toISOString(), type: 'job_started', gist: 'B is current', bad: false }] })
  await settle()
  run('A', { entries: [{ t: new Date().toISOString(), type: 'job_started', gist: 'A is stale', bad: false }] })
  await settle()
  assert.match(el('actionsLog').innerHTML, /B is current/)
  assert.doesNotMatch(el('actionsLog').innerHTML, /A is stale/)
})

test('actions panel: a bad entry carries the bad class', async () => {
  const { el, select, run } = page()
  select('Chani')
  run('Chani', { entries: [{ t: new Date().toISOString(), type: 'died', gist: 'fell into lava', bad: true }] })
  await settle()
  assert.match(el('actionsLog').innerHTML, /<div class="bad"[^>]*>-0s died fell into lava<\/div>/)
})

test('actions panel: relative time reads in seconds, minutes and hours', async () => {
  const { el, select, run } = page()
  select('Chani')
  const now = Date.now()
  const ago = s => new Date(now - s * 1000).toISOString()
  run('Chani', {
    entries: [
      { t: ago(12), type: 'job_started', gist: 'a', bad: false },
      { t: ago(125), type: 'job_started', gist: 'b', bad: false },
      { t: ago(7200), type: 'job_started', gist: 'c', bad: false }
    ]
  })
  await settle()
  const html = el('actionsLog').innerHTML
  assert.match(html, /-12s job_started a/)
  assert.match(html, /-2m job_started b/)
  assert.match(html, /-2h job_started c/)
})
