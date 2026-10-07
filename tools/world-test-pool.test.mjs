// Why JavaScript: node --test file for tools/world-test-pool.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import { splitForms, summarize, unitOrder, parsePoolArgs, workerSpecs, runPool, mergeText, poolCap, createReaper } from './world-test-pool.mjs'

const form = (id, status, secs = 1, extra = '') =>
  `{:plot 0, :file "${id.split('/')[0]}", :expects [{:status :pass, :evidence "a } \\" {"}], :status :${status}, :id "${id}", :elapsed-s ${secs}${extra}}`
const vec = (...forms) => `[${forms.join(' ')}]`

test('splitForms: top-level maps of a result vector, braces inside strings and nested maps ignored', () => {
  const forms = splitForms(vec(form('a/x', 'pass'), form('b/y', 'fail')))
  assert.equal(forms.length, 2)
  assert.deepEqual(forms.map((f) => summarize(f).id), ['a/x', 'b/y'])
})
test('summarize: reads the top-level :status, not a nested expectation status', () => {
  assert.equal(summarize(form('a/x', 'fail')).status, 'fail')
  assert.equal(summarize(form('a/x', 'pass', 2.5)).elapsed, 2.5)
})

test('unitOrder: longest first by recorded durations, unseen files at the median, file order when nothing is known', () => {
  const prev = [vec(form('a/1', 'pass', 5), form('a/2', 'pass', 5), form('b/1', 'pass', 30), form('c/1', 'pass', 1))]
  assert.deepEqual(unitOrder(['a', 'b', 'c', 'd'], prev), ['b', 'a', 'd', 'c'])
  assert.deepEqual(unitOrder(['z', 'y', 'x'], []), ['z', 'y', 'x'])
})

test('parsePoolArgs: pool flags are taken out, paths and other flags pass through', () => {
  const p = parsePoolArgs(['dir', 'x.edn', '--bodies', '4', '--body', 'ProbeSweep', '--first-plot', '20', '--results', 'r.edn',
    '--durations', 'o1.edn', '--durations', 'o2.edn', '--phase', 'day', '--allow-time', '--time-log', 't.txt'])
  assert.equal(p.bodies, 4); assert.equal(p.prefix, 'ProbeSweep'); assert.equal(p.firstPlot, 20); assert.equal(p.results, 'r.edn')
  assert.deepEqual(p.durations, ['o1.edn', 'o2.edn'])
  assert.deepEqual(p.paths, ['dir', 'x.edn'])
  assert.deepEqual(p.passthrough, ['--phase', 'day', '--allow-time', '--time-log', 't.txt'])
})
test('parsePoolArgs: defaults', () => {
  const p = parsePoolArgs(['d', '--bodies', '3'])
  assert.equal(p.prefix, 'ProbePool'); assert.equal(p.firstPlot, 0); assert.equal(p.results, null)
})

test('workerSpecs: the cap lives in the tool; one plot block (20) per body', () => {
  const w = workerSpecs(50, 19, 'ProbeSweep', 0)
  assert.equal(w.length, 19)
  assert.deepEqual(w.slice(0, 2), [{ body: 'ProbeSweepA', firstPlot: 0 }, { body: 'ProbeSweepB', firstPlot: 20 }])
  assert.equal(workerSpecs(3, 19, 'P', 40)[2].firstPlot, 80)
  assert.equal(workerSpecs(0, 19, 'P', 0).length, 1)
})

const fakeRunner = (script, log = []) => async ({ file, match, worker }) => {
  log.push({ file, match, body: worker.body })
  return script(file, match, worker, log)
}
const ok = (file) => ({ code: 0, text: vec(form(`${file}/c1`, 'pass'), form(`${file}/c2`, 'pass')) })
const sleepy = () => new Promise((r) => setTimeout(r, 5))

test('runPool: every unit runs once, results merged, exit 0', async () => {
  const log = []
  const r = await runPool({ units: ['a', 'b', 'c'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner((f) => ok(f), log) })
  assert.equal(r.code, 0)
  assert.equal(splitForms(r.text).length, 6)
  assert.deepEqual(log.map((l) => l.file).sort(), ['a', 'b', 'c'])
  assert.equal(new Set(log.map((l) => l.body)).size, 2)
})
test('runPool: never more than the workers run at once', async () => {
  let now = 0, peak = 0
  const run = async ({ file }) => { now++; peak = Math.max(peak, now); await sleepy(); now--; return ok(file) }
  await runPool({ units: ['a', 'b', 'c', 'd', 'e'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(peak, 2)
})
test('runPool: a failed case reruns once on a different body, by --match; a pass is :flaky, others kept', async () => {
  const log = []
  const run = fakeRunner((file, match) => match
    ? { code: 0, text: vec(form('a/c2', 'pass')) }
    : { code: 1, text: vec(form('a/c1', 'pass'), form('a/c2', 'fail')) }, log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(log.length, 2)
  assert.equal(log[1].match, 'a/c2')
  assert.notEqual(log[1].body, log[0].body)
  const sum = splitForms(r.text).map(summarize)
  assert.deepEqual(sum.map((s) => [s.id, s.status]), [['a/c1', 'pass'], ['a/c2', 'flaky']])
  assert.match(r.text, /:first-failure-body "P[AB]"/)
  const flaky = splitForms(r.text).find((f) => summarize(f).id === 'a/c2')
  assert.match(flaky, /:first-failure \{[^]*:status :fail[^]*\}[^]*:first-failure-body/)
  assert.equal(r.code, 0)
})
test('runPool: a case failing twice stays failed, exit 1, and is not rerun a third time', async () => {
  const log = []
  const run = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(log.length, 2)
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => s.status), ['fail'])
  assert.equal(r.code, 1)
})
test('runPool: with one body the rerun uses it (no other to pick)', async () => {
  const log = []
  const run = fakeRunner((f, match) => match ? { code: 0, text: vec(form('a/c1', 'pass')) } : { code: 1, text: vec(form('a/c1', 'fail')) }, log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: run })
  assert.deepEqual(log.map((l) => l.body), ['PA', 'PA'])
  assert.equal(r.code, 0)
})
test('runPool: a unit that dies with no results is rerun whole once', async () => {
  const log = []
  const run = fakeRunner((file, match) => log.length === 1 ? { code: 3, text: null } : ok(file), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(log.length, 2)
  assert.equal(log[1].match, null)
  assert.equal(r.code, 0)
  assert.equal(splitForms(r.text).length, 2)
})
test('runPool: a unit that keeps dying is reported as an :error result, exit 1', async () => {
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: async () => ({ code: 3, text: null }) })
  const s = splitForms(r.text).map(summarize)
  assert.deepEqual(s.map((x) => [x.id, x.status]), [['a', 'error']])
  assert.equal(r.code, 1)
})
test('runPool: inconclusive cases are not rerun and fail the exit code', async () => {
  const log = []
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'inconclusive')) }), log) })
  assert.equal(log.length, 1)
  assert.equal(r.code, 1)
})
const runForm = (id, status, run) => form(id, status, 1, `, :run ${run}`)
test('parsePoolArgs: --retry-failed N is the pool\'s own (not passed to children); default 1', () => {
  const p = parsePoolArgs(['d', '--retry-failed', '3', '--phase', 'day'])
  assert.equal(p.retries, 3)
  assert.deepEqual(p.passthrough, ['--phase', 'day'])
  assert.equal(parsePoolArgs(['d']).retries, 1)
})
test('runPool: --retry-failed N reruns a failing case exactly N times, no more', async () => {
  const log = []
  const run = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 3 })
  assert.equal(log.length, 4)
  assert.equal(r.code, 1)
})
test('runPool: --retry-failed 0 reruns nothing', async () => {
  const log = []
  await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail')) }), log), retries: 0 })
  assert.equal(log.length, 1)
})
test('runPool: an :error case is not rerun (the single-body runner retries failures only)', async () => {
  const log = []
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'error')) }), log) })
  assert.equal(log.length, 1)
  assert.equal(r.code, 1)
})
test('runPool: --repeat results of one id are all kept, and a rerun replaces only its own run', async () => {
  const log = []
  const run = fakeRunner((f, match) => match
    ? { code: 0, text: vec(runForm('a/c1', 'pass', 1)) }
    : { code: 1, text: vec(runForm('a/c1', 'pass', 1), runForm('a/c1', 'fail', 2), runForm('a/c1', 'pass', 3)) }, log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  const s = splitForms(r.text).map(summarize)
  assert.deepEqual(s.map((x) => [x.id, x.run, x.status]), [['a/c1', 1, 'pass'], ['a/c1', 2, 'flaky'], ['a/c1', 3, 'pass']])
  assert.equal(log.length, 2)
  assert.equal(r.code, 0)
})
test('runPool: a rerun asks for the exact case id (and one run), not a substring', async () => {
  const log = []
  const run = async (job) => { log.push(job); return job.match ? { code: 0, text: vec(form('a/c1', 'pass')) } : { code: 1, text: vec(form('a/c1', 'fail'), form('a/c10', 'pass')) } }
  await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(log[1].match, 'a/c1')
  assert.equal(log[1].exact, true)
})
test('mergeText: wraps forms in one vector', () => {
  assert.equal(mergeText(['{:a 1}', '{:b 2}']), '[{:a 1}\n {:b 2}]')
})

test('poolCap: default min(bodies, max(1, cores/4)); --max-parallel overrides', () => {
  assert.equal(poolCap({ bodies: 19, cores: 16 }), 4)
  assert.equal(poolCap({ bodies: 2, cores: 16 }), 2)
  assert.equal(poolCap({ bodies: 5, cores: 2 }), 1)
  assert.equal(poolCap({ bodies: 19, cores: 16, maxParallel: 9 }), 9)
})
test('parsePoolArgs: --max-parallel is a pool flag', () => {
  const p = parsePoolArgs(['d', '--max-parallel', '3'])
  assert.equal(p.maxParallel, 3)
  assert.deepEqual(p.passthrough, [])
  assert.equal(parsePoolArgs(['d']).maxParallel, null)
})
test('runPool: waits (polling) while the load average is above the core count, before starting another child', async () => {
  const loads = [20, 20, 3, 20, 3, 3, 3, 3, 3, 3]
  let polls = 0, sleeps = 0
  const started = []
  const r = await runPool({
    units: ['a', 'b'], workers: workerSpecs(2, 19, 'P', 0),
    runUnit: async ({ file }) => { started.push([file, polls]); await sleepy(); return ok(file) },
    load: () => { polls++; return loads[Math.min(polls - 1, loads.length - 1)] }, cores: 8,
    sleep: async () => { sleeps++ },
  })
  assert.equal(r.code, 0)
  assert.equal(started[0][0], 'a')
  assert.equal(started[0][1], 0, 'nothing running: no wait')
  assert.equal(started[1][0], 'b')
  assert.ok(started[1][1] >= 3, 'the second child waited until the load dropped')
  assert.equal(sleeps, 2)
})
test('runPool: a high load never blocks when nothing is running (no livelock)', async () => {
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: async ({ file }) => ok(file), load: () => 99, cores: 4, sleep: async () => { throw new Error('slept') } })
  assert.equal(r.code, 0)
})

test('createReaper: reap kills every live child by pid and removes its tmp file; finished children are left alone', () => {
  const killed = [], removed = []
  const reaper = createReaper((f) => removed.push(f))
  const child = (pid) => ({ pid, kill: (sig) => killed.push([pid, sig]) })
  const a = reaper.add(child(11), 'ta.edn'), b = reaper.add(child(12), 'tb.edn')
  reaper.done(a)
  reaper.reap()
  assert.deepEqual(killed, [[12, 'SIGTERM']])
  assert.deepEqual(removed, ['tb.edn'])
  reaper.reap()
  assert.equal(killed.length, 1)
})
