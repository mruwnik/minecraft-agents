// Why JavaScript: node --test file for tools/world-test-pool.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import { splitForms, summarize, unitOrder, parsePoolArgs, workerSpecs, runPool, mergeText, poolCap, createReaper, countListed, listArgs, knownFailures, parseListed } from './world-test-pool.mjs'

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

test('poolCap: min(bodies, body slot max, bodies the free memory above the floor holds); --max-parallel overrides', () => {
  const mem = { availableMb: 14000, floorMb: 6000, needMb: 400, bodyMax: 20 }
  assert.equal(poolCap({ bodies: 16, cores: 16, ...mem }), 16, 'cores do not cap it: 8000 MB / 400 holds 20')
  assert.equal(poolCap({ bodies: 30, cores: 16, ...mem }), 20, 'the body slot max')
  assert.equal(poolCap({ bodies: 16, cores: 16, ...mem, availableMb: 8000 }), 5, 'memory: 2000 / 400')
  assert.equal(poolCap({ bodies: 16, cores: 16, ...mem, availableMb: 5000 }), 1, 'below the floor still runs one')
  assert.equal(poolCap({ bodies: 19, cores: 16, ...mem, maxParallel: 9 }), 9)
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

// ---- the @@test view: a fixed total, reruns counted apart, one final status per case
const eventsOf = async (units, runUnit, total, retries = 1) => {
  const seen = []
  await runPool({ units, workers: workerSpecs(2, 19, 'P', 0), runUnit, retries, total, emit: (e) => seen.push(e) })
  return seen
}
const progressOf = (seen) => seen.filter((e) => e.event === 'progress')
const resultsOf = (seen) => seen.filter((e) => e.event === 'result')

test('countListed: one case per --list line, times --repeat; blank lines are not cases', () => {
  assert.equal(countListed('a/x  #{:t}\nb/y  nil  PROBLEMS [\"x\"]\n\n', 1), 2)
  assert.equal(countListed('a/x  nil\n', 3), 3)
})
test('runPool events: the total is the one given from the start, in every progress line', async () => {
  const run = fakeRunner((f) => ({ code: 0, text: vec(form(`${f}/1`, 'pass'), form(`${f}/2`, 'pass')) }), [])
  const seen = await eventsOf(['a', 'b'], run, 4)
  assert.deepEqual(progressOf(seen).map((e) => [e.done, e.total, e.retries]), [[1, 4, 0], [2, 4, 0], [3, 4, 0], [4, 4, 0]])
  assert.equal(resultsOf(seen).length, 4)
})
test('runPool events: a case failing and failing again is one failed result, the reruns are counted as retries', async () => {
  const run = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail'), form('a/c2', 'pass')) }), [])
  const seen = await eventsOf(['a'], run, 2, 2)
  const results = resultsOf(seen)
  assert.deepEqual(results.map((e) => [e.name, e.outcome]).sort(), [['a/c1#1', 'failed'], ['a/c2#1', 'passed']])
  const last = progressOf(seen).at(-1)
  assert.deepEqual([last.done, last.total, last.retries], [2, 2, 2])
})
test('runPool events: a failure that passes on the rerun is reported once, as flaky, not as a failure', async () => {
  const run = fakeRunner((f, match) => match ? { code: 0, text: vec(form('a/c1', 'pass')) } : { code: 1, text: vec(form('a/c1', 'fail')) }, [])
  const seen = await eventsOf(['a'], run, 1)
  assert.deepEqual(resultsOf(seen).map((e) => [e.name, e.outcome]), [['a/c1#1', 'flaky']])
  assert.equal(progressOf(seen).at(-1).retries, 1)
})
test('listArgs: the --list call gets the paths and the case-selecting flags the children get', () => {
  const p = parsePoolArgs(['dir', '--bodies', '4', '--phase', 'night', '--tag', 'air', '--match', 'dive', '--repeat', '2'])
  assert.deepEqual(listArgs(p), ['dir', '--phase', 'night', '--tag', 'air', '--match', 'dive', '--repeat', '2', '--list'])
})

test('knownFailures: ids whose last recorded result was a fail; a later pass or flaky clears them, an error is not one', () => {
  const older = vec(form('a/c1', 'fail'), form('a/c2', 'fail'), form('a/c3', 'fail'), form('a/c4', 'error'))
  const newer = vec(form('a/c2', 'pass'), form('a/c3', 'flaky'))
  assert.deepEqual([...knownFailures([older, newer])], ['a/c1'])
})
test('runPool: a failed case that is a known failure is not rerun, is marked :known-failure true and counted as skipped', async () => {
  const log = []
  const run = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail'), form('a/c2', 'fail')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 2, known: new Set(['a/c1']) })
  assert.deepEqual(log.map((l) => l.match), [null, 'a/c2', 'a/c2'], 'only c2 is rerun')
  assert.equal(r.skipped, 1)
  const forms = splitForms(r.text)
  assert.match(forms.find((f) => summarize(f).id === 'a/c1'), /:known-failure true/)
  assert.doesNotMatch(forms.find((f) => summarize(f).id === 'a/c2'), /:known-failure/)
})

test('parseListed: the --list lines grouped by fixture stem, in order, PROBLEMS lines too', () => {
  const m = parseListed('a/x  #{:t}\na/y  nil\nb/z  nil  PROBLEMS ["x"]\n\n')
  assert.deepEqual([...m], [['a', ['a/x', 'a/y']], ['b', ['b/z']]])
})
test('runPool: a unit that dies twice settles one error per listed case (and run), so done reaches the total', async () => {
  const listed = new Map([['a', ['a/c1', 'a/c2', 'a/c3']]])
  const seen = []
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: null }), []), listed, repeat: 2, total: 6, emit: (e) => seen.push(e) })
  assert.deepEqual(splitForms(r.text).map((f) => { const s = summarize(f); return [s.id, s.run, s.status] }).sort(),
    [['a/c1', 1, 'error'], ['a/c1', 2, 'error'], ['a/c2', 1, 'error'], ['a/c2', 2, 'error'], ['a/c3', 1, 'error'], ['a/c3', 2, 'error']])
  assert.equal(progressOf(seen).at(-1).done, 6)
  assert.equal(resultsOf(seen).length, 6)
})
test('runPool: listed cases missing from a unit that ended with partial results settle as errors', async () => {
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'pass')) }), []), listed, total: 2 })
  assert.deepEqual(splitForms(r.text).map((f) => { const s = summarize(f); return [s.id, s.status] }), [['a/c1', 'pass'], ['a/c2', 'error']])
})
