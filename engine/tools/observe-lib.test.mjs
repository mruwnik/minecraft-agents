import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { keyword as k, readEDN, writeEDN, compactStatus, classify, collect, attentionChanges, waitObserve } from './observe-lib.mjs'
import { requestFor } from './observe.mjs'
const defaults = { chatter: 'addressed', watch: [], danger: false, disconnect: false }
const event = (source, kind, data = {}, message) => ({ source: k(source), kind: k(kind), data, message })
test('compact status omits metadata, empty collections, rounds position, retains EDN keywords', () => {
  const s = readEDN('{:body "Probe" :generation-id "uuid" :cursor {} :mode :scheduled :current nil :position {:x 1.254 :y 70 :z -9.666} :health 20 :food 20 :jobs {:total 0 :items []} :failed {:total 0} :outstanding {:total 0}}')
  assert.equal(writeEDN(compactStatus(s)), '{:mode :scheduled :idle true :pos [1.3 70 -9.7] :health 20 :food 20}')
})
test('addressed chatter matches whole names; none suppresses whispers; sender filter applies', () => {
  const e = event('body', 'chat', { from: 'Dan' }, 'Hello Probe!')
  assert.equal(classify(e, defaults, 'Probe').wake.key, 'chat')
  assert.equal(classify({ ...e, message: 'ProbeExtra' }, defaults, 'Probe'), null)
  assert.equal(classify({ ...e, message: 'banter' }, { ...defaults, chatter: 'all' }, 'Probe').wake.key, 'chat')
  assert.equal(classify(e, { ...defaults, from: 'Other' }, 'Probe'), null)
  assert.equal(classify(event('body', 'whisper', { from: 'Dan' }, 'hi'), defaults, 'Probe').wake.key, 'chat')
  assert.equal(classify(event('body', 'whisper', {}, 'hi'), { ...defaults, chatter: 'none' }, 'Probe'), null)
})
test('danger and disconnection are opt-in; exhausted reconnection wakes by default; tracked jobs wake', () => {
  assert.equal(classify(event('body', 'hurt'), defaults, 'Probe'), null)
  assert.equal(classify(event('body', 'disconnected'), defaults, 'Probe'), null)
  assert.equal(classify(event('body', 'hurt'), { ...defaults, danger: true }, 'Probe').wake.key, 'danger')
  assert.equal(classify(event('body', 'reconnect-failed'), defaults, 'Probe').wake.key, 'reconnect-failed')
  const e = { ...event('job', 'completed'), context: { 'job-id': 'j1' } }
  assert.equal(classify(e, defaults, 'Probe'), null)
  assert.equal(classify(e, { ...defaults, watch: ['j1'] }, 'Probe').wake.key, 'job-finished')
})
test('attention deduplication ignores timestamps/position and reports semantic changes', () => {
  const r = { 'job-id': 'j1', reason: k('blocked'), 'updated-at': 123, event: { kind: k('blocked'), message: 'No food', data: { pos: { x: 1 } } } }
  const first = attentionChanges({ r }, {})
  assert.equal(first.changed.length, 1)
  assert.equal(attentionChanges({ r: { ...r, 'updated-at': 456, event: { ...r.event, data: { pos: { x: 2 } } } } }, first.seen).changed.length, 0)
  assert.equal(attentionChanges({ r: { ...r, event: { ...r.event, message: 'No tools' } } }, first.seen).changed.length, 1)
  assert.deepEqual(attentionChanges({}, first.seen).seen, {})
  const many = Object.fromEntries(Array.from({ length: 10 }, (_, i) => [`r${i}`, r]))
  let seen = {}; let n = 0
  for (let i = 0; i < 3; i++) { const a = attentionChanges(many, seen); seen = a.seen; n += a.changed.length }
  assert.equal(n, 10)
})
test('summaries are bounded and discard routine ticks', () => {
  const s = { counts: {}, items: [], more: false }
  for (let i = 0; i < 1000; i++) collect(s, event('body', 'physics-tick'))
  assert.deepEqual(s.counts, {})
  for (let i = 0; i < 1000; i++) collect(s, event('body', 'picked-up', { item: 'wheat', count: 1 }))
  assert.equal(s.items.length, 4); assert.equal(s.counts['picked-up'], 1000); assert.equal(s.more, true)
})
function fixture (timeout = '30ms') {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'observe-unit-'))
  const req = requestFor(['Probe', '--state', dir, '--wait', '--timeout', timeout, '--poll-ms', '50'])
  const state = { generation: 'g', outstanding: {}, events: [], gap: false }
  const get = async (_socket, endpoint) => {
    let value
    if (endpoint === '/snapshot') value = { body: 'Probe', 'generation-id': state.generation, outstanding: state.outstanding, cursor: { 'stream-id': 's', seq: state.events.length } }
    else if (endpoint === '/status') value = { mode: k('scheduled'), current: null }
    else { const after = Number(new URL(endpoint, 'http://x').searchParams.get('after')); value = { 'gap?': state.gap, 'stream-id': 's', 'latest-seq': state.events.length, cursor: { 'stream-id': 's', seq: state.events.length }, events: state.events.filter(e => e.seq > after) } }
    return { status: 200, contentType: 'application/edn', text: writeEDN(value) }
  }
  return { dir, req, state, get, file: path.join(dir, 'observers', 'Probe', 'agent.edn'), cleanup: () => fs.rmSync(dir, { recursive: true }) }
}
test('wait persists between invocations, reports quiet changes and does not miss between-call chat', async () => {
  const f = fixture()
  try {
    assert.deepEqual(await waitObserve(f.req, f.get), { wake: k('timeout'), changed: false })
    f.state.events.push({ ...event('body', 'picked-up', { item: 'wheat', count: 2 }), seq: 1 })
    const quiet = await waitObserve(f.req, f.get)
    assert.equal(quiet.wake.key, 'timeout'); assert.equal(quiet.summary.counts['picked-up'], 1)
    f.state.events.push({ ...event('body', 'chat', { from: 'Dan' }, 'Probe come home'), seq: 2 })
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'chat')
    assert.equal(readEDN(fs.readFileSync(f.file, 'utf8')).cursor.seq, 2)
    assert.equal((await waitObserve(f.req, f.get)).changed, false)
  } finally { f.cleanup() }
})
test('outstanding requests remain unresolved, wake once, and wake again on change', async () => {
  const f = fixture()
  try {
    f.state.outstanding.r = { 'job-id': 'j1', reason: k('blocked'), event: { message: 'help' } }
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'attention')
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'timeout')
    assert.equal(Object.keys(f.state.outstanding).length, 1)
    f.state.outstanding.r.event.message = 'different help'
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'attention')
  } finally { f.cleanup() }
})
test('cancellation and failed delivery do not checkpoint; concurrent observer rejected; restart/gap explicit', async () => {
  const f = fixture('1s')
  try {
    const c = new AbortController(); const pending = waitObserve(f.req, f.get, c.signal)
    await assert.rejects(waitObserve(f.req, f.get), { code: 'EOBSERVERBUSY' })
    c.abort(); await assert.rejects(pending, { name: 'AbortError' })
    assert.equal(readEDN(fs.readFileSync(f.file, 'utf8')).cursor.seq, 0)
    f.req.waitOptions.timeoutMs = 20
    await assert.rejects(waitObserve(f.req, f.get, undefined, async () => { throw new Error('stdout failed') }), /stdout failed/)
    assert.equal(readEDN(fs.readFileSync(f.file, 'utf8')).cursor.seq, 0)
    await waitObserve(f.req, f.get)
    f.state.generation = 'g2'
    assert.equal((await waitObserve(f.req, f.get)).reason.key, 'engine-restarted')
    f.state.gap = true
    assert.equal((await waitObserve(f.req, f.get)).reason.key, 'event-gap')
  } finally { f.cleanup() }
})
test('wait CLI validates policies, durations and observer names', () => {
  assert.equal(requestFor(['Probe', '--wait']).waitOptions.timeoutMs, 60000)
  assert.equal(requestFor(['Probe', '--wait', '--timeout', '1.5s']).waitOptions.timeoutMs, 1500)
  assert.ok(requestFor(['Probe', '--wait', '--observer', '../bad']).error)
  assert.ok(requestFor(['Probe', '--wait', '--chatter', 'classified']).error)
  assert.ok(requestFor(['Probe', '--wait', '--watch', 'j1,bad']).error)
  assert.ok(requestFor(['Probe', '--wait', '--timeout', 'infinity']).error)
  assert.ok(requestFor(['Probe', '--timeout', '1s']).error)
})

test('attention beyond the first128 entries is delivered, and resolved snapshots do not re-wake', async () => {
  let seen = {}; let delivered = 0
  const requests = Object.fromEntries(Array.from({length: 129}, (_, i) => [`r${i}`, { reason: k('blocked'), event: { message: 'help' } }]))
  for (let i = 0; i < 33; i++) { const a = attentionChanges(requests, seen); seen = a.seen; delivered += a.changed.length }
  assert.equal(delivered, 129)
  const f = fixture()
  try {
    f.state.outstanding.r = requests.r0
    await waitObserve(f.req, f.get)
    f.state.events.push({ ...event('attention', 'resolved'), 'request-id': 'r', seq: 1 })
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'timeout')
  } finally {f.cleanup()}
})
test('first cancelled wait preserves baseline so events before retry are not missed', async () => {
  const f = fixture('1s')
  try {
    const c = new AbortController(); const pending = waitObserve(f.req, f.get, c.signal)
    await new Promise(resolve => setTimeout(resolve, 5)); c.abort()
    await assert.rejects(pending)
    f.state.events.push({...event('body', 'chat', {from: 'Dan'}, 'Probe hello'), seq: 1})
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'chat')
  } finally {f.cleanup()}
})
test('deadline during a read returns quiet summary; engine start notification does not repeat', async () => {
  const f = fixture('20ms')
  try {
    const stalled = async (socket, endpoint, options) => {
      if (endpoint.startsWith('/events')) {
        await new Promise(resolve => setTimeout(resolve, 25))
        throw Object.assign(new Error('deadline'), { code: 'ETIMEDOUT' })
      }
      return f.get(socket, endpoint, options)
    }
    assert.equal((await waitObserve(f.req, stalled)).wake.key, 'timeout')
    f.state.events.push({ ...event('system', 'restored'), seq: 1 })
    assert.equal((await waitObserve(f.req, f.get)).reason.key, 'engine-restarted')
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'timeout')
  } finally { f.cleanup() }
})
test('explicitly watched action completion wakes with bounded result; other action events stay quiet', async () => {
  const e = { ...event('action', 'done', { status: k('arrived'), result: { status: 'arrived', pos: { x: 1.234, y: 64, z: 2 }, distance: 1.234, drops: Array(10000).fill('wheat') } }), context: { 'action-id': 'move-1' } }
  const opts = { ...defaults, watchActions: ['move-1'] }
  assert.deepEqual(classify(e, opts, 'Probe'), { wake: k('action-finished'), action: 'move-1', result: { status: 'arrived', pos: [1.2, 64, 2], distance: 1.2 } })
  assert.equal(classify({ ...e, source: k('job') }, opts, 'Probe'), null)
  assert.equal(classify({ ...e, kind: k('started') }, opts, 'Probe'), null)
  assert.equal(classify(e, { ...opts, watchActions: ['other'] }, 'Probe'), null)
  assert.equal(classify(e, defaults, 'Probe'), null)
  const summary = { counts: {}, items: [], more: false }; collect(summary, e)
  assert.deepEqual(summary.counts, {})
  const f = fixture()
  try {
    await waitObserve(f.req, f.get)
    f.state.events.push({ ...e, seq: 1 })
    f.req.waitOptions.watchActions = ['move-1']
    const result = await waitObserve(f.req, f.get)
    assert.equal(result.wake.key, 'action-finished')
    assert.equal(result.action, 'move-1')
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'timeout')
  } finally { f.cleanup() }
})
test('action watchers accept repeated/comma options, validate limits, require wait', () => {
  const r = requestFor(['Probe', '--wait', '--watch-action', 'move-1,place-1', '--watch-action', 'dig-1', '--watch', 'j1,j2', '--watch', 'j3'])
  assert.deepEqual(r.waitOptions.watchActions, ['move-1', 'place-1', 'dig-1'])
  assert.deepEqual(r.waitOptions.watch, ['j1', 'j2', 'j3'])
  assert.ok(requestFor(['Probe', '--watch-action', 'move-1']).error)
  assert.ok(requestFor(['Probe', '--wait', '--watch-action', 'bad/name']).error)
  assert.ok(requestFor(['Probe', '--wait', '--watch-action', Array(33).fill('move-1').join(',')]).error)
})
test('first use catches recently completed watched actions while ignoring historical chat; later results retained', async () => {
  const f = fixture()
  try {
    f.state.events.push({ ...event('body', 'chat', { from: 'Dan' }, 'Probe historical chat'), seq: 1, 'generation-id': 'g' },
      { ...event('action', 'done', { status: 'dug' }), context: { 'action-id': 'dig-1' }, seq: 2, 'generation-id': 'g' },
      { ...event('action', 'done', { status: 'placed' }), context: { 'action-id': 'place-1' }, seq: 3, 'generation-id': 'g' })
    f.req.waitOptions.watchActions = ['dig-1', 'place-1']
    assert.equal((await waitObserve(f.req, f.get)).action, 'dig-1')
    assert.equal((await waitObserve(f.req, f.get)).action, 'place-1')
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'timeout')
    assert.equal(readEDN(fs.readFileSync(f.file, 'utf8')).cursor.seq, 3)
  } finally { f.cleanup() }
})
test('historical lookup ignores prior generations/newly started attempts and reports unavailable history', async () => {
  const f = fixture()
  try {
    f.req.waitOptions.watchActions = ['move-1']
    f.state.events.push({ ...event('action', 'done', { status: 'arrived' }), context: { 'action-id': 'move-1' }, seq: 1, 'generation-id': 'older' })
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'timeout')
    fs.unlinkSync(f.file)
    f.state.events[0]['generation-id'] = 'g'
    f.state.events.push({ ...event('action', 'started'), context: { 'action-id': 'move-1' }, seq: 2, 'generation-id': 'g' })
    assert.equal((await waitObserve(f.req, f.get)).wake.key, 'timeout')
    fs.unlinkSync(f.file); f.state.gap = true
    assert.equal((await waitObserve(f.req, f.get)).reason.key, 'history-unavailable')
  } finally { f.cleanup() }
})
