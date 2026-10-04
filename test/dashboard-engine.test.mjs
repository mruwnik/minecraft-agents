import test from 'node:test'
import assert from 'node:assert/strict'
import { foldEngine, engineView, emptyEngine, parseEngineAgents, engineBody, completeLines, dropTornHead, parseEngineLines, ENGINE_UP_MS, WARN_WINDOW_MS } from '../tools/dashboard/engine.mjs'
import { scopeSnapshot, groupWorlds } from '../tools/dashboard/lib.mjs'
import { mapPoints } from '../tools/dashboard/map.mjs'

const T0 = 1_000_000_000_000
const ev = (seq, extra = {}) => ({ seq, t: T0 + seq * 1000, body: 'B', source: 'job', kind: 'round_started', level: 'info', job: 'j1', chain: ['j1'], pos: { x: seq, y: 64, z: -seq }, ...extra })
const fold = events => foldEngine(emptyEngine, events)
const view = (events, now) => engineView(fold(events), now)

test('thresholds: up within 30 s, warnings kept 10 min', () => {
  assert.equal(ENGINE_UP_MS, 30_000)
  assert.equal(WARN_WINDOW_MS, 600_000)
})

test('foldEngine does not mutate its input and chunks fold the same as one batch', () => {
  const events = [ev(1, { name: '(repeat look)' }), ev(2, { kind: 'failed', error: 'boom', level: 'warn' }), ev(3, { source: 'job', name: 'dig' })]
  const frozen = Object.freeze(emptyEngine)
  assert.deepEqual(foldEngine(foldEngine(frozen, events.slice(0, 1)), events.slice(1)), foldEngine(frozen, events))
  assert.deepEqual(emptyEngine, foldEngine(emptyEngine, []))
})

test('engineView with no events is down with an error', () => {
  const v = engineView(emptyEngine, T0)
  assert.equal(v.up, false)
  assert.equal(v.ageMs, null)
  assert.match(v.error, /no events/)
})

test('engineView: liveness by age of the last event', () => {
  const events = [ev(1)]
  const last = T0 + 1000
  const table = [[0, true], [ENGINE_UP_MS - 1, true], [ENGINE_UP_MS, false], [5 * 60_000, false]]
  table.forEach(([age, up]) => assert.equal(view(events, last + age).up, up, `age ${age}`))
  assert.equal(view(events, last + 3000).ageMs, 3000)
})

test('engineView: pos is the latest event pos', () => {
  assert.deepEqual(view([ev(1), ev(2), ev(3)], T0 + 4000).pos, { x: 3, y: 64, z: -3 })
})

const jobCases = [
  ['named by a top-level job event', [ev(1, { name: '(repeat look)' })], { id: 'j1', name: '(repeat look)' }],
  ['action events do not name the job', [ev(1, { name: '(repeat look)' }), ev(2, { source: 'action', kind: 'started', level: 'debug', name: 'moveTo', chain: ['j1', 'j1/c0'], job: 'j1/c0' })], { id: 'j1', name: '(repeat look)' }],
  ['child job events do not rename the top-level job', [ev(1, { name: 'outer' }), ev(2, { name: 'inner', chain: ['j1', 'j1/c0'], job: 'j1/c0' })], { id: 'j1', name: 'outer' }],
  ['a queued job is not current', [ev(1, { kind: 'queued', name: 'pace' })], null],
  ['completed clears it', [ev(1, { name: 'a' }), ev(2, { kind: 'completed', name: 'a' })], null],
  ['failed clears it', [ev(1, { name: 'a' }), ev(2, { kind: 'failed', name: 'a', error: 'x', level: 'warn' })], null],
  ['cancelled clears it', [ev(1, { name: 'a' }), ev(2, { kind: 'cancelled', name: 'a' })], null],
  ['another job ending leaves it', [ev(1, { name: 'a' }), ev(2, { kind: 'completed', name: 'b', job: 'j2', chain: ['j2'] })], { id: 'j1', name: 'a' }],
  ['a child ending leaves it', [ev(1, { name: 'a' }), ev(2, { kind: 'completed', name: 'c', job: 'j1/c0', chain: ['j1', 'j1/c0'] })], { id: 'j1', name: 'a' }],
  ['system.started clears it', [ev(1, { name: 'a' }), ev(2, { source: 'system', kind: 'started', job: null, chain: undefined })], null],
  ['a new job replaces it', [ev(1, { name: 'a' }), ev(2, { name: 'b', job: 'j2', chain: ['j2'] })], { id: 'j2', name: 'b' }]
]
jobCases.forEach(([title, events, job]) => test(`current job: ${title}`, () => assert.deepEqual(view(events, T0 + 9000).job, job)))

const reflex = (seq, kind, id, extra = {}) => ev(seq, { source: 'reflex', kind, reflex: id, job: 'j9', chain: undefined, ...extra })
const reflexCases = [
  ['fired sets it', [reflex(1, 'fired', 'hungry')], 'hungry'],
  ['ended clears the same reflex', [reflex(1, 'fired', 'hungry'), reflex(2, 'ended', 'hungry', { how: 'cleared' })], null],
  ['ended of another reflex leaves it', [reflex(1, 'fired', 'hungry'), reflex(2, 'ended', 'stuck', { how: 'dropped' })], 'hungry'],
  ['system.started clears it', [reflex(1, 'fired', 'hungry'), ev(2, { source: 'system', kind: 'started', job: null, chain: undefined })], null],
  ['changed does not set it', [reflex(1, 'changed', 'hungry')], null]
]
reflexCases.forEach(([title, events, id]) => test(`active reflex: ${title}`, () => assert.equal(view(events, T0 + 9000).reflex, id)))

test('recent: info and up, heartbeats and debug excluded, shaped with text fallbacks, last 10', () => {
  const events = [
    ev(1, { level: 'debug', source: 'action', kind: 'started', name: 'moveTo' }),
    ev(2, { kind: 'yielded' }),
    ev(3, { kind: 'round_started' }),
    ev(4, { source: 'body', kind: 'chat', text: 'hello', job: null }),
    ev(5, { kind: 'failed', level: 'warn', error: 'no path', name: 'dig' }),
    ev(6, { source: 'job', kind: 'dig_in_failed', level: 'warn', name: 'shelter' }),
    ev(7, { source: 'body', kind: 'hurt', job: null })
  ]
  assert.deepEqual(view(events, T0 + 8000).recent, [
    { t: T0 + 4000, level: 'info', source: 'body', kind: 'chat', text: 'hello' },
    { t: T0 + 5000, level: 'warn', source: 'job', kind: 'failed', text: 'no path' },
    { t: T0 + 6000, level: 'warn', source: 'job', kind: 'dig_in_failed', text: 'job.dig_in_failed shelter' },
    { t: T0 + 7000, level: 'info', source: 'body', kind: 'hurt', text: 'body.hurt' }
  ])
})

test('recent keeps only the newest 10', () => {
  const events = Array.from({ length: 25 }, (_, i) => ev(i + 1, { source: 'body', kind: 'chat', text: `m${i + 1}`, job: null }))
  const recent = view(events, T0 + 30_000).recent
  assert.deepEqual(recent.map(r => r.text), Array.from({ length: 10 }, (_, i) => `m${i + 16}`))
})

test('warn10m / error10m count only the last 10 minutes', () => {
  const at = (seq, level, t) => ev(seq, { level, kind: 'x', t })
  const events = [at(1, 'warn', T0), at(2, 'error', T0 + 1000), at(3, 'warn', T0 + 700_000), at(4, 'warn', T0 + 710_000), at(5, 'error', T0 + 720_000), at(6, 'info', T0 + 730_000)]
  const v = view(events, T0 + 735_000)
  assert.equal(v.warn10m, 2)
  assert.equal(v.error10m, 1)
})

test('warnings age out with now even when nothing new arrives', () => {
  const state = fold([ev(1, { level: 'warn', kind: 'x' })])
  const t = T0 + 1000
  assert.equal(engineView(state, t + WARN_WINDOW_MS - 1).warn10m, 1)
  assert.equal(engineView(state, t + WARN_WINDOW_MS + 1).warn10m, 0)
})

test('fold prunes stored warnings against the newest event', () => {
  const events = [ev(1, { level: 'warn', kind: 'x' }), ev(2000, { level: 'info' })]
  assert.deepEqual(fold(events).warns, [])
})

// ---------------------------------------------------------------- reading the file
const enc = text => new TextEncoder().encode(text)
const dec = bytes => new TextDecoder().decode(bytes)

const chunkCases = [
  ['whole lines', '', 'a\nb\n', 'a\nb\n', ''],
  ['partial tail kept', '', 'a\nb', 'a\n', 'b'],
  ['carry completes', 'b', 'c\nd', 'bc\n', 'd'],
  ['no newline at all', 'ab', 'c', '', 'abc'],
  ['multibyte split across reads', 'é'.slice(0, 0), 'xé', '', 'xé']
]
chunkCases.forEach(([title, carry, chunk, text, rest]) => test(`completeLines: ${title}`, () => {
  const r = completeLines(enc(carry), enc(chunk))
  assert.equal(dec(r.complete), text)
  assert.equal(dec(r.rest), rest)
}))

test('completeLines: a multibyte character cut between reads is whole after the join', () => {
  const bytes = enc('{"t":"é"}\n')
  const first = completeLines(new Uint8Array(0), bytes.subarray(0, 7))
  const second = completeLines(first.rest, bytes.subarray(7))
  assert.equal(dec(second.complete), '{"t":"é"}\n')
})

test('dropTornHead removes bytes up to and including the first newline', () => {
  assert.equal(dec(dropTornHead(enc('rn"}\n{"a":1}\n'))), '{"a":1}\n')
  assert.equal(dec(dropTornHead(enc('no newline'))), '')
})

test('parseEngineLines skips unparseable lines and blanks', () => {
  assert.deepEqual(parseEngineLines('{"seq":1}\n\nnot json\n{"seq":2}\n'), [{ seq: 1 }, { seq: 2 }])
})

// ---------------------------------------------------------------- agents and bodies
test('parseEngineAgents: world from the folder, username from config, apiPort not needed, missing config tolerated', () => {
  const entries = [
    { name: 'ProbeWater', world: 'claude', text: JSON.stringify({ username: 'PW', world: 'elsewhere', apiPort: 3804 }) },
    { name: 'NoPort', world: 'claude', text: JSON.stringify({}) },
    { name: 'NoConfig', world: 'claude', text: '' },
    { name: 'Broken', world: 'claude', text: '{' }
  ]
  assert.deepEqual(parseEngineAgents(entries), [
    { name: 'Broken', username: 'Broken', world: 'claude' },
    { name: 'NoConfig', username: 'NoConfig', world: 'claude' },
    { name: 'NoPort', username: 'NoPort', world: 'claude' },
    { name: 'ProbeWater', username: 'PW', world: 'claude' }
  ])
})

test('engineBody: the shape the page and groupWorlds expect, plus engine', () => {
  const agent = { name: 'P', username: 'P', world: 'claude' }
  const up = engineBody(agent, view([ev(1, { name: 'a' })], T0 + 2000))
  assert.deepEqual({ ...up, engine: undefined }, { name: 'P', username: 'P', world: 'claude', apiPort: null, harness: null, character: null, up: true, error: null, at: T0 + 1000, state: { pos: { x: 1, y: 64, z: -1 } }, engine: undefined })
  assert.equal(up.engine.job.name, 'a')
  const down = engineBody(agent, view([ev(1)], T0 + 99_000))
  assert.equal(down.up, false)
  assert.match(down.error, /last event/)
  const none = engineBody(agent, engineView(emptyEngine, T0))
  assert.deepEqual([none.up, none.state, none.at], [false, null, null])
})

const worldsOf = bodies => [{ name: 'claude', places: [], zones: [] }, { name: 'other', places: [], zones: [] }].map(w => ({ ...w, bodies: bodies.filter(b => b.world === w.name), humans: [] }))

test('scopeSnapshot keeps an engine body of the chosen world and drops one in another world', () => {
  const mine = engineBody({ name: 'A', username: 'A', world: 'claude' }, view([ev(1)], T0 + 2000))
  const theirs = engineBody({ name: 'B', username: 'B', world: 'other' }, view([ev(1)], T0 + 2000))
  const scoped = scopeSnapshot({ agents: ['A', 'B'], bodies: [mine, theirs], worlds: worldsOf([mine, theirs]) }, 'claude')
  assert.deepEqual(scoped.bodies.map(b => b.name), ['A'])
  assert.deepEqual(scoped.agents, ['A'])
  assert.deepEqual(scoped.worlds.map(w => w.bodies.map(b => b.name)), [['A']])
})

test('groupWorlds puts an engine body on its world; the map draws it only while up', () => {
  const up = engineBody({ name: 'A', username: 'A', world: 'claude' }, view([ev(1)], T0 + 2000))
  const down = engineBody({ name: 'D', username: 'D', world: 'claude' }, view([ev(1)], T0 + 99_000))
  const [world] = groupWorlds([{ name: 'claude', places: [], zones: [] }], [up, down], ['A', 'D'])
  assert.deepEqual(world.bodies.map(b => b.name), ['A', 'D'])
  assert.deepEqual(mapPoints(world.bodies, [], [], []), [{ x: 1, z: -1 }])
})
