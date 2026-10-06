// Why JavaScript: node --test file for tools/test-shards.mjs (a Node launcher).
import fs from 'node:fs'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import test from 'node:test'
import assert from 'node:assert/strict'
import { splitShards, partOf, parsePart, testNamespaces, slowest, memSlots, eventForwarder, shardOutcome, failureDump } from './test-shards.mjs'

test('splitShards: every namespace exactly once, loads balanced by prior timing', () => {
  const nss = ['a-test', 'b-test', 'c-test', 'd-test', 'e-test']
  const ms = { 'a-test': 100, 'b-test': 60, 'c-test': 50, 'd-test': 40, 'e-test': 10 }
  const shards = splitShards(nss, ms, 2)
  assert.deepEqual(shards.flat().sort(), nss)
  const load = shards.map((s) => s.reduce((t, n) => t + ms[n], 0))
  assert.ok(Math.abs(load[0] - load[1]) <= 20, String(load))
})

test('splitShards: unknown timings still split evenly, never empty shards beyond namespace count', () => {
  const shards = splitShards(['a-test', 'b-test', 'c-test'], {}, 5)
  assert.equal(shards.length, 3)
  assert.deepEqual(shards.flat().sort(), ['a-test', 'b-test', 'c-test'])
})

test('testNamespaces finds -test ns forms under engine/test', () => {
  const nss = testNamespaces()
  assert.ok(nss.includes('engine.go-to-test'))
  assert.ok(nss.every((n) => n.endsWith('-test')))
})

test('slowest sorts timing lines descending', () => {
  const lines = ['{"var":"x/a","ms":5}', '{"var":"x/b","ms":50}', '{"peak-rss-kb":1}']
  assert.deepEqual(slowest(lines, 1), [{ var: 'x/b', ms: 50 }])
})

test('shardOutcome: exit 0 with no test logged is a failure; a failed exit keeps its code', () => {
  const ok = ['{"var":"x/a","ms":5}']
  assert.deepEqual(shardOutcome(0, ok), { ok: true })
  assert.equal(shardOutcome(0, ['{"peak-rss-kb":1}']).ok, false)
  assert.match(shardOutcome(0, []).why, /no test/)
  assert.equal(shardOutcome(1, ok).ok, false)
})

test('memSlots: (available - 6 GB floor) / shard peak, at least 1, at most 3', () => {
  assert.equal(memSlots(5900), 1)
  assert.equal(memSlots(12100), 2)
  assert.equal(memSlots(30000), 3)
})

test('testNamespaces covers every file under engine/test that contains deftest', () => {
  const root = path.join(path.dirname(new URL(import.meta.url).pathname), '..', 'engine', 'test')
  const walk = (d) => fs.readdirSync(d, { withFileTypes: true }).flatMap((e) => e.isDirectory() ? walk(path.join(d, e.name)) : [path.join(d, e.name)])
  const nss = testNamespaces()
  const missing = walk(root).filter((f) => /\(deftest\s/.test(fs.readFileSync(f, 'utf8')))
    .map((f) => [f, fs.readFileSync(f, 'utf8').match(/^\(ns\s+(?:\^\S+\s+)*([^\s()]+)[\s)]/m)?.[1]])
    .filter(([, ns]) => !ns?.endsWith('-golden')) // golden namespaces are opt-in (tools/test-engine --golden)
    .filter(([, ns]) => !nss.includes(ns)).map(([f]) => f)
  assert.deepEqual(missing, [])
})

const ev = (e) => `@@test ${JSON.stringify(e)}`
const parsed = (out) => out.map((l) => JSON.parse(l.slice('@@test '.length)))

test('eventForwarder: result and phase lines pass through, other output is dropped, a line split over chunks is joined', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [1])
  const res = ev({ event: 'result', name: 'a/b', outcome: 'passed' })
  f.feed(0, `noise\n${res.slice(0, 20)}`)
  f.feed(0, `${res.slice(20)}\nmore noise\n${ev({ event: 'phase', name: 'x' })}\n`)
  assert.deepEqual(parsed(out), [{ event: 'result', name: 'a/b', outcome: 'passed' }, { event: 'phase', name: 'x' }])
})

test('eventForwarder: a last line without a newline is forwarded by end()', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [1])
  f.feed(0, ev({ event: 'result', name: 'z', outcome: 'passed' }))
  assert.deepEqual(parsed(out), [])
  f.end(0)
  assert.deepEqual(parsed(out), [{ event: 'result', name: 'z', outcome: 'passed' }])
})

test('eventForwarder: one plan, emitted once every shard has reported its own, with the summed total', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [4, 6])
  f.feed(0, `${ev({ event: 'plan', total: 30 })}\n`)
  assert.deepEqual(parsed(out).filter((e) => e.event === 'plan'), [])
  f.feed(1, `${ev({ event: 'plan', total: 50 })}\n`)
  assert.deepEqual(parsed(out).filter((e) => e.event === 'plan'), [{ event: 'plan', total: 80 }])
})

test('eventForwarder: progress total covers all shards from the start, also those not started', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [4, 6, 5])
  f.start()
  assert.deepEqual(parsed(out), [{ event: 'progress', done: 0, total: 15, unit: 'namespaces' }])
  f.feed(0, `${ev({ event: 'progress', done: 2, total: 4, unit: 'namespaces' })}\n`)
  assert.deepEqual(parsed(out).at(-1), { event: 'progress', done: 2, total: 15, unit: 'namespaces' })
})

test('eventForwarder: waiting names the shard and the slot wait is a phase line', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [4, 6])
  f.waiting(1)
  assert.deepEqual(parsed(out), [{ event: 'phase', name: 'waiting for slot (shard 2/2)' }])
})

test('eventForwarder: shard progress lines become one progress summed over the shards', () => {
  const out = []
  const f = eventForwarder((l) => out.push(l), [4, 6])
  f.feed(0, `${ev({ event: 'progress', done: 1, total: 4, unit: 'namespaces' })}\n`)
  f.feed(1, `${ev({ event: 'progress', done: 1, total: 6, unit: 'namespaces' })}\n`)
  f.feed(0, `${ev({ event: 'progress', done: 2, total: 4, unit: 'namespaces' })}\n`)
  assert.deepEqual(parsed(out).at(-1), { event: 'progress', done: 3, total: 10, unit: 'namespaces' })
})

test('main with TEST_EVENTS=1 announces compiling before the compile runs (no early use of events)', () => {
  const script = "import('./tools/test-shards.mjs').then((m) => m.main({ compile: () => ({ status: 3 }) }))"
  const r = spawnSync('node', ['-e', script, '--input-type=module'], { cwd: path.join(path.dirname(fileURLToPath(import.meta.url)), '..'), env: { ...process.env, TEST_EVENTS: '1' }, encoding: 'utf8' })
  assert.doesNotMatch(r.stderr, /ReferenceError/)
  assert.match(r.stdout, /"name":"compiling"/)
  assert.equal(r.status, 3)
})

test('partOf: for N in 1..6 the parts are disjoint and together cover every namespace', () => {
  const nss = Array.from({ length: 23 }, (_, k) => `ns${(k * 7) % 23}-test`)
  for (let n = 1; n <= 6; n++) {
    const parts = Array.from({ length: n }, (_, i) => partOf(nss, i + 1, n))
    assert.deepEqual(parts.flat().sort(), [...nss].sort(), `N=${n}`)
  }
})

test('partOf: depends on the names only, not on the order they are given in', () => {
  const nss = ['d-test', 'a-test', 'c-test', 'b-test', 'e-test']
  assert.deepEqual(partOf(nss, 2, 2), partOf([...nss].reverse(), 2, 2))
  assert.deepEqual(partOf(nss, 2, 2), ['b-test', 'd-test'])
})

test('parsePart: "i/N" with 1 <= i <= N, otherwise an error', () => {
  assert.deepEqual(parsePart('2/4'), { i: 2, n: 4 })
  for (const bad of ['0/4', '5/4', 'x', '2', '1/0']) assert.throws(() => parsePart(bad), /--part/)
})

test('failureDump: long output is cut to its tail, short output kept whole', () => {
  assert.equal(failureDump('short', 100), 'short')
  const d = failureDump('x'.repeat(500) + 'TAIL', 100)
  assert.ok(d.length < 200 && d.endsWith('TAIL'))
})

test('main end: a large stdout write before exit reaches a pipe reader intact', () => {
  const src = "process.stdout.write('y'.repeat(300000) + 'END\\n'); process.exitCode = 1"
  const r = spawnSync(process.execPath, ['-e', src], { maxBuffer: 1e7 })
  assert.equal(r.stdout.length, 300004)
})
