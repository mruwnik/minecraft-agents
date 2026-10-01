// The look popup: a click on the small picture opens it big and live, fed by one event stream of frames from the
// dashboard until it is closed, its inventory asked for again a second after each answer; the small picture keeps its one look per click.
// The action log panel is sliced from the same place: it shares select() with the look popup, so the two live in one file.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import vm from 'node:vm'

const html = fs.readFileSync(new URL('../tools/dashboard/index.html', import.meta.url), 'utf8')
const start = html.indexOf('const select = name =>')
const end = html.indexOf('// ---------------------------------------------------------------- the chat log', start)
// tag()/line() live earlier in the file (the side panel section) and are pulled in on their own, skipping the
// body-list/plan-popup code between them and select() that this slice has no use for and no context to run.
const tagStart = html.indexOf('const tag = (name, cls, text) => {')
const tagEnd = html.indexOf("const line = (cls, text) => tag('div', cls, text)") + "const line = (cls, text) => tag('div', cls, text)".length
const script = `${html.slice(tagStart, tagEnd)}\n${html.slice(start, end)}`

const settle = () => new Promise(resolve => setImmediate(resolve))

const DEFAULT_SCREEN = { hp: 20, food: 20, xp: 0, oxygen: 20, armor: 0, slots: [], selected: 0, window: null }

const page = (screen = DEFAULT_SCREEN, { clock } = {}) => {
  const nodes = new Map()
  const el = id => {
    if (!nodes.has(id)) {
      const node = {
        id, hidden: true, src: '', innerHTML: '', className: '', title: '',
        scrollTop: 0, clientHeight: 0, scrollHeight: 0,
        style: { setProperty () {} },
        children: [],
        append (...kids) { this.children.push(...kids) },
        listeners: {},
        addEventListener (type, fn) { this.listeners[type] = fn }
      }
      // setting textContent (as render* code does to clear a list before redrawing it) drops whatever was appended
      let text = id === 'relook' ? 'watch' : ''
      Object.defineProperty(node, 'textContent', { get: () => text, set: v => { text = v; node.children = [] } })
      nodes.set(id, node)
    }
    return nodes.get(id)
  }
  const fetches = []
  const timers = []
  const streams = []
  const pending = []
  class EventSource {
    constructor (url) { this.url = url; this.closed = false; streams.push(this) }
    close () { this.closed = true }
  }
  const context = {
    EventSource,
    el,
    selected: 'Chani',
    // select() also drives the body list and the map; both are outside this slice, so they are stubbed as no-ops
    renderList: () => {},
    draw: () => {},
    fetch: url => {
      fetches.push(url)
      if (url.startsWith('/api/screen/')) return Promise.resolve({ ok: true, json: async () => screen })
      if (url.startsWith('/api/actions/')) {
        let resolve
        const promise = new Promise(r => { resolve = r })
        pending.push({ url, resolve })
        return promise
      }
      return Promise.resolve({ ok: true, headers: { get: () => '' }, blob: async () => ({}) })
    },
    URL: { createObjectURL: () => 'blob:look', revokeObjectURL () {} },
    URLSearchParams,
    JSON,
    Date: clock === undefined ? Date : { now: () => clock, parse: Date.parse },
    setTimeout: (fn, ms) => { timers.push({ fn, ms }); return timers.length },
    clearTimeout: id => { if (timers[id - 1]) timers[id - 1].cleared = true },
    document: {
      addEventListener () {},
      createElement: name => ({ tagName: name, className: '', textContent: '', title: '' })
    }
  }
  vm.createContext(context)
  vm.runInContext(script, context)
  const select = name => vm.runInContext(`select(${JSON.stringify(name)})`, context)
  const poll = () => vm.runInContext('refreshActions()', context)
  const relAge = iso => vm.runInContext(`relAge(Date.now(), ${JSON.stringify(iso)})`, context)
  const run = (name, json, { ok = true, which = 0 } = {}) =>
    pending.filter(p => p.url === `/api/actions/${name}`)[which].resolve({ ok, json: async () => json })
  return { el, fetches, timers, streams, pending, select, poll, run, relAge }
}

test('look popup: a click on the picture opens one live stream and a 1 s inventory loop, shows each frame as it comes, and closing stops both', async () => {
  const { el, fetches, timers, streams } = page()
  assert.ok(start > 0 && end > start, 'the look script is where the test expects it')

  el('lookimg').listeners.click()
  assert.equal(el('lookOverlay').hidden, false)
  assert.deepEqual([streams.length, streams[0].url], [1, '/api/look/Chani/live'])
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

test('look popup: selecting another body clears the last one\'s picture, so an error does not show it under the new name', async () => {
  const { el, streams, select } = page()
  el('lookimg').listeners.click()
  streams[0].onmessage({ data: JSON.stringify({ png: 'AAAA', view: 'north pitch 0', seen: [], blocked: '' }) })
  assert.equal(el('lookBig').hidden, false)

  select('Bob')
  assert.deepEqual([el('lookimg').hidden, el('lookimg').src, el('lookBig').hidden, el('lookBig').src], [true, '', true, ''])

  streams[0].onmessage({ data: JSON.stringify({ error: 'the body did not answer' }) })
  assert.equal(el('lookBig').hidden, true)
})

// the popup opens on a click on the picture, the small picture streams once "watch" is pressed; both draw the marks
for (const [name, open, marked] of [['the popup', 'lookimg', 'lookBigMarks'], ['the small picture', 'relook', 'lookimgMarks']]) {
  test(`look marks: ${name} outlines and labels the entities each frame shows; a frame with none, or another body, clears them`, () => {
    const { el, streams, select } = page()
    el(open).listeners.click()
    const marks = [{ name: 'cow', kind: 'passive', dist: 3, box: [0.25, 0.5, 0.5, 0.75] }, { name: 'zombie', kind: 'hostile', dist: 9, box: [0.6, 0.1, 0.7, 0.4] }]
    const frame = marks => streams[0].onmessage({ data: JSON.stringify({ png: 'AAAA', view: 'north pitch 0', blocked: '', seen: [], marks }) })
    frame(marks)
    const drawn = el(marked).innerHTML
    assert.deepEqual([drawn.match(/class="mark/g).length, drawn.includes('left:25%;top:50%;width:25%;height:25%'), drawn.includes('cow 3m'), drawn.includes('mark hostile')], [2, true, true, true])
    frame([])
    assert.equal(el(marked).innerHTML, '')
    frame(marks)
    select('Bob')
    assert.equal(el(marked).innerHTML, '')
  })
}

// the header shows where the body stands, from the same stream frame as the caption; both the popup and the
// inline card go through showLook(), so one frame shape is wired into both
for (const [name, open, meta] of [['the popup', 'lookimg', 'lookBigMeta'], ['the small picture', 'relook', 'lookmeta']]) {
  test(`look header: ${name} shows the body's current coordinates`, () => {
    const { el, streams } = page()
    el(open).listeners.click()
    streams[0].onmessage({ data: JSON.stringify({ png: 'AAAA', view: 'north pitch 0', seen: [], blocked: '', at: { x: 102, y: 70, z: -108 } }) })
    assert.equal(el(meta).textContent, 'Chani · north pitch 0 · 102 70 -108\nsees nothing alive')
  })
}

test('look popup: a player name is shown as text, never run as html', () => {
  const { el, streams } = page()
  el('lookimg').listeners.click()
  streams[0].onmessage({ data: JSON.stringify({ png: 'AAAA', view: 'north', blocked: '', seen: [], marks: [{ name: '<img src=x onerror=alert(1)>', kind: 'player', dist: 2, box: [0, 0, 1, 1] }] }) })
  assert.deepEqual([el('lookBigMarks').innerHTML.includes('<img'), el('lookBigMarks').innerHTML.includes('&lt;img src=x onerror=alert(1)&gt; 2m')], [false, true])
})

test('inline card: the watch button starts a live stream and flips to pause; pause closes it and flips back', () => {
  const { el, streams } = page()
  assert.equal(el('relook').textContent, 'watch')
  el('relook').listeners.click()
  assert.equal(el('relook').textContent, 'pause')
  assert.deepEqual([streams.length, streams[0].url, streams[0].closed], [1, '/api/look/Chani/live', false])
  el('relook').listeners.click()
  assert.equal(el('relook').textContent, 'watch')
  assert.equal(streams[0].closed, true)
})

test('inline card: a frame from the watch stream draws the picture and caption, the same as the popup', () => {
  const { el, streams } = page()
  el('relook').listeners.click()
  streams[0].onmessage({ data: JSON.stringify({ png: 'BBBB', view: 'south pitch 0', seen: [], blocked: '' }) })
  assert.equal(el('lookimg').src, 'data:image/png;base64,BBBB')
  assert.equal(el('lookimg').hidden, false)
  assert.equal(el('lookmeta').textContent, 'Chani · south pitch 0\nsees nothing alive')
})

test('inline card: switching the selected body while watching keeps watching, on the new body', () => {
  const { el, streams, select } = page()
  el('relook').listeners.click()
  select('Bob')
  assert.deepEqual([streams.length, streams[0].closed, streams[1].url, streams[1].closed], [2, true, '/api/look/Bob/live', false])
  assert.equal(el('relook').textContent, 'pause')
})

test('inline card: selecting a body starts watching it, even after a pause', () => {
  const { el, streams, select } = page()
  select('Bob')
  assert.deepEqual([streams.length, streams[0].url, el('relook').textContent], [1, '/api/look/Bob/live', 'pause'])
  el('relook').listeners.click()
  assert.deepEqual([streams[0].closed, el('relook').textContent], [true, 'watch'])
  select('Bob')
  assert.deepEqual([streams.length, streams[1].url, streams[1].closed, el('relook').textContent], [2, '/api/look/Bob/live', false, 'pause'])
})

test('inline card: deselecting stops watching', () => {
  const { el, streams, select } = page()
  select('Bob')
  select(null)
  assert.deepEqual([streams[0].closed, el('relook').textContent], [true, 'watch'])
})

test('inline card: no fetch is made for the inline picture - it only ever comes down the stream', () => {
  const { fetches } = page()
  assert.equal(fetches.length, 0)
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
  el('lookCard').listeners.error({ target: img })
  assert.equal(img.outerHTML, '<span class="abbr">IP</span>')
  screen.slots = [...carrying.slots, { slot: 11, name: 'dirt', count: 2 }]
  timers.at(-1).fn()
  await settle()
  assert.equal(fetches.length, 2)
  assert.equal(cells(el('lookInventory').innerHTML)[36].inside, '<span class="abbr">IP</span>')
})

test('container screen: an item with no picture shows its initials, same as the inventory grid', async () => {
  const { el } = await shown({ ...carrying, window: chest })
  const img = { dataset: { item: 'dirt' }, outerHTML: '' }
  el('lookCard').listeners.error({ target: img })
  assert.equal(img.outerHTML, '<span class="abbr">D</span>')
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

// ---------------------------------------------------------------- the action log (now inside the popup)
const entry = (overrides = {}) => ({ t: '2026-10-01T15:21:00.000Z', type: 'job_started', gist: 'goto x=1', bad: false, ...overrides })

const relAgeCases = [
  [12, '-12s'], [59, '-59s'], [60, '-1m'], [125, '-2m'], [3599, '-59m'], [3600, '-1h'], [7200, '-2h']
]
relAgeCases.forEach(([seconds, expected]) => test(`relAge: ${seconds} s reads ${expected}`, () => {
  const FIXED_NOW = Date.parse('2026-10-01T15:21:12.600Z')
  const { relAge } = page(undefined, { clock: FIXED_NOW })
  assert.equal(relAge(new Date(FIXED_NOW - seconds * 1000).toISOString()), expected)
}))

test('actions log: renders the selected body\'s log, whether or not the small picture is paused', async () => {
  const FIXED_NOW = Date.parse('2026-10-01T15:21:24.000Z')
  const { el, select, run } = page(undefined, { clock: FIXED_NOW })
  select('Chani')
  el('relook').listeners.click()
  run('Chani', { entries: [entry({ t: '2026-10-01T15:21:12.000Z' })] })
  await settle()
  assert.deepEqual(el('actionsLog').children.map(c => ({ cls: c.className, text: c.textContent, title: c.title })), [
    { cls: '', text: '-12s job_started goto x=1', title: 'goto x=1' }
  ])
})

test('actions log: switching bodies clears the previous log immediately, before the new one answers', async () => {
  const { el, select, run } = page()
  select('A')
  run('A', { entries: [entry({ gist: 'A thing' })] })
  await settle()
  assert.equal(el('actionsLog').children.length, 1)
  select('B')
  assert.equal(el('actionsLog').children.length, 0)
})

test('actions log: a late answer for a body switched away from is dropped', async () => {
  const { el, select, run, pending } = page()
  select('A')
  select('B')
  assert.deepEqual(pending.map(p => p.url), ['/api/actions/A', '/api/actions/B'])
  run('B', { entries: [entry({ gist: 'B is current' })] })
  await settle()
  run('A', { entries: [entry({ gist: 'A is stale' })] })
  await settle()
  assert.deepEqual(el('actionsLog').children.map(c => c.title), ['B is current'])
})

test('actions log: of two overlapping polls for the same body, only the later answer lands', async () => {
  const { el, select, run, poll } = page()
  select('Chani')
  poll()
  run('Chani', { entries: [entry({ gist: 'second poll' })] }, { which: 1 })
  await settle()
  run('Chani', { entries: [entry({ gist: 'first poll, now stale' })] }, { which: 0 })
  await settle()
  assert.deepEqual(el('actionsLog').children.map(c => c.title), ['second poll'])
})

test('actions log: a bad entry carries the err class and its gist as the title', async () => {
  const { el, select, run } = page()
  select('Chani')
  run('Chani', { entries: [entry({ type: 'died', gist: 'fell into lava', bad: true })] })
  await settle()
  const [row] = el('actionsLog').children
  assert.equal(row.className, 'err')
  assert.equal(row.title, 'fell into lava')
  assert.match(row.textContent, /died fell into lava$/)
})

const badAnswers = [
  ['a non-ok answer', { ok: false, json: { error: 'no agent folder called Chani' } }],
  ['an ok answer with no entries field', { ok: true, json: {} }]
]
badAnswers.forEach(([what, { ok, json }]) => test(`actions log: ${what} is treated as no entries, not a crash or stale lines`, async () => {
  const { el, select, run } = page()
  select('Chani')
  run('Chani', json, { ok })
  await settle()
  assert.deepEqual(el('actionsLog').children, [])
}))

test('actions log: lives inside the popup and nowhere else', () => {
  const popup = html.slice(html.indexOf('<div id="lookOverlay"'), html.indexOf('<div id="planOverlay"'))
  assert.deepEqual([popup.includes('id="actionsLog"'), html.split('id="actionsLog"').length - 1, html.includes('id="actions"')], [true, 1, false])
})
