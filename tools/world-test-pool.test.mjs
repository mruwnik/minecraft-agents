// Why JavaScript: node --test file for tools/world-test-pool.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import net from 'node:net'
import { splitForms, scanForms, failSig, summarize, unitOrder, parsePoolArgs, workerSpecs, runPool, mergeText, poolCap, createReaper, countListed, listArgs, knownFailures, parseListed, unitsWithCases, noCasesSelected, serverProbe, matchIdsFrom, parseNightClasses, nightBatchEndArgs, sleepRuleRestoreArgs, nightBatches, durationsById, runNightBatches } from './world-test-pool.mjs'

const form = (id, status, secs = 1, extra = '') =>
  `{:plot 0, :file "${id.split('/')[0]}", :expects [{:status :pass, :evidence "a } \\" {"}], :status :${status}, :id "${id}", :elapsed-s ${secs}${extra}}`
const vec = (...forms) => `[${forms.join(' ')}]`

test('splitForms: top-level maps of a result vector, braces inside strings and nested maps ignored', () => {
  const forms = splitForms(vec(form('a/x', 'pass'), form('b/y', 'fail')))
  assert.equal(forms.length, 2)
  assert.deepEqual(forms.map((f) => summarize(f).id), ['a/x', 'b/y'])
})
test('splitForms: empty, whitespace-only and truncated text end, returning the complete forms only', () => {
  const whole = form('a/x', 'pass')
  const cut = form('b/y', 'fail')
  assert.deepEqual(splitForms(''), [])
  assert.deepEqual(splitForms('  \n'), [])
  assert.deepEqual(splitForms('['), [])
  assert.deepEqual(splitForms(`[${whole} ${cut.slice(0, 40)}`), [whole])
  assert.deepEqual(splitForms(`[${whole} {:a "unterminated`), [whole])
  assert.deepEqual(splitForms(`[${whole} {:a [1 2`), [whole])
  assert.deepEqual(splitForms(`[${whole}  \n ]  \n`), [whole])
})
test('scanForms: complete only for a closed vector', () => {
  assert.equal(scanForms(vec(form('a/x', 'pass'))).complete, true)
  assert.equal(scanForms('[]\n').complete, true)
  assert.equal(scanForms('').complete, false)
  assert.equal(scanForms(`[${form('a/x', 'pass')}`).complete, false)
})
test('failSig: the :why of a failed case without a failed expectation, else empty', () => {
  assert.equal(failSig('{:status :fail, :why "no body", :id "a/x"}'), '"no body"')
  assert.equal(failSig('{:status :fail, :expects [{:status :pass}], :why "late"}'), '"late"')
  assert.equal(failSig('{:status :fail, :id "a/x"}'), '')
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
test('parsePoolArgs: --durations takes every file up to the next flag, never as fixture paths', () => {
  const p = parsePoolArgs(['dir', '--durations', 'a.edn', 'b.edn', '--durations', 'c.edn', '--phase', 'day'])
  assert.deepEqual(p.durations, ['a.edn', 'b.edn', 'c.edn'])
  assert.deepEqual(p.paths, ['dir'])
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

// a failed case whose first failing check is `check` (an :expect map text)
const failForm = (id, check, secs = 1, extra = '') =>
  `{:file "${id.split('/')[0]}", :expects [{:status :pass, :expect {:event {:kind :ok}}} {:status :fail, :evidence "none", :expect ${check}}], :status :fail, :id "${id}", :elapsed-s ${secs}${extra}}`
test('knownFailures: id -> first failing check of its last recorded result when that was a fail; a later pass or flaky clears it, an error is not one', () => {
  const older = vec(failForm('a/c1', '{:event {:kind :x}}'), form('a/c2', 'fail'), form('a/c3', 'fail'), form('a/c4', 'error'))
  const newer = vec(form('a/c2', 'pass'), form('a/c3', 'flaky'))
  assert.deepEqual([...knownFailures([older, newer])], [['a/c1', '{:event {:kind :x}}']])
})
test('runPool: a failure with a different first failing check than last time is rerun, not a known failure', async () => {
  const log = []
  const run = fakeRunner(() => ({ code: 1, text: vec(failForm('a/c1', '{:event {:kind :y}}')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1, known: new Map([['a/c1', '{:event {:kind :x}}']]) })
  assert.deepEqual(log.map((l) => l.match), [null, 'a/c1'])
  assert.equal(r.skipped, 0)
  assert.doesNotMatch(r.text, /:known-failure/)
})
test('result events carry known-failure and the seconds saved by an early end', async () => {
  const seen = []
  const run = fakeRunner(() => ({ code: 1, text: vec(failForm('a/c1', '{:event {:kind :x}}', 1, ', :ended-early-s 40'), form('a/c2', 'pass')) }), [])
  await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1, known: new Map([['a/c1', '{:event {:kind :x}}']]), emit: (e) => seen.push(e) })
  const [c1, c2] = resultsOf(seen)
  assert.equal(c1.knownFailure, true)
  assert.equal(c1.endedEarlyS, 40)
  assert.equal('knownFailure' in c2, false)
})
test('runPool: a failed case that is a known failure is not rerun, is marked :known-failure true and counted as skipped', async () => {
  const log = []
  const run = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail'), form('a/c2', 'fail')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 2, known: new Map([['a/c1', '']]) })
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

test('unitsWithCases: a fixture file with no listed case (none in the phase, or all unchanged) gets no unit; no listing keeps all', () => {
  const listed = parseListed('a/x  nil\nc/z  nil\n')
  assert.deepEqual(unitsWithCases(['a', 'b', 'c'], listed), ['a', 'c'])
  assert.deepEqual(unitsWithCases(['a', 'b'], null), ['a', 'b'])
})

test('noCasesSelected: a listing that leaves no unit is an empty selection; no listing is not', () => {
  assert.equal(noCasesSelected([], parseListed('')), true)
  assert.equal(noCasesSelected(['a'], parseListed('a/x  nil\n')), false)
  assert.equal(noCasesSelected([], null), false)
})
test('runPool: no unit ran is exit 2 (no cases selected), like the single-body runner', async () => {
  const r = await runPool({ units: [], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner((f) => ok(f), []) })
  assert.equal(r.code, 2)
})

// ---- server down: a connection error is a setup fault, not a case result
const refused = (id) => form(id, 'error', 1, ', :evidence "connect ECONNREFUSED 127.0.0.1:25575"')
const fakeServer = (downUntilMs) => { // a clock the fake sleep advances, and a probe that refuses until downUntilMs
  const clock = { t: 0 }
  return { clock, now: () => clock.t, sleep: async (ms) => { clock.t += ms }, probe: async () => clock.t >= downUntilMs }
}
test('runPool: cases that hit a refused connection wait for the server and rerun without using a retry or showing as errors', async () => {
  const srv = fakeServer(40000)
  const log = []
  const runUnit = fakeRunner((file) => (log.length === 1 ? { code: 1, text: vec(refused(`${file}/c1`), refused(`${file}/c2`)) } : ok(file)), log)
  const seen = []
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, retries: 1, total: 2, emit: (e) => seen.push(e), ...srv })
  assert.equal(r.code, 0)
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status), ['pass', 'pass'])
  assert.ok(srv.clock.t >= 40000)
  assert.equal(resultsOf(seen).length, 2)
  assert.equal(progressOf(seen).at(-1).retries, 0)
})
test('runPool: a unit that exits with no results while the server is down is held and rerun whole once the server is back', async () => {
  const srv = fakeServer(20000)
  const log = []
  const runUnit = fakeRunner((file) => (log.length === 1 ? { code: 1, text: null } : ok(file)), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, ...srv })
  assert.equal(r.code, 0)
  assert.equal(log.length, 2)
})
test('runPool: no new unit starts while the server is down', async () => {
  const srv = fakeServer(30000)
  const starts = []
  const runUnit = async ({ file }) => { starts.push([file, srv.clock.t]); return starts.length === 1 ? { code: 1, text: vec(refused(`${file}/c1`)) } : ok(file) }
  await runPool({ units: ['a', 'b', 'c'], workers: workerSpecs(1, 19, 'P', 0), runUnit, ...srv })
  assert.ok(starts.slice(1).every(([, t]) => t >= 30000))
})
test('runPool: a mixed unit keeps its good results and reruns only the refused case', async () => {
  const srv = fakeServer(5000)
  const log = []
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const runUnit = fakeRunner((file, match) => (log.length === 1 ? { code: 1, text: vec(form('a/c1', 'pass'), refused('a/c2')) } : { code: 0, text: vec(form(match, 'pass')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, total: 2, ...srv })
  assert.equal(r.code, 0)
  assert.deepEqual(log.map((l) => l.match), [null, 'a/c2'])
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status).sort(), ['pass', 'pass'])
})
test('runPool: past the wait cap the pool stops as server unreachable (exit 3), recording no errors', async () => {
  const srv = fakeServer(Infinity)
  const r = await runPool({ units: ['a', 'b'], workers: workerSpecs(1, 19, 'P', 0), runUnit: fakeRunner((file) => ({ code: 1, text: vec(refused(`${file}/c1`)) }), []), serverWaitMs: 60000, ...srv })
  assert.equal(r.code, 3)
  assert.equal(r.unreachable, true)
  assert.deepEqual(splitForms(r.text), [])
  assert.ok(srv.clock.t >= 60000)
})
test('parsePoolArgs: --server-wait-s sets the cap in seconds (default 300)', () => {
  assert.equal(parsePoolArgs(['d']).serverWaitS, 300)
  assert.equal(parsePoolArgs(['d', '--server-wait-s', '90']).serverWaitS, 90)
})

// ---- review findings of the fault handling (card 6bfe1115)
test('runPool: a connection fault that persists while the probe says the server is up is rerun at most twice, then settles as an error', async () => {
  const log = []
  const runUnit = fakeRunner((file) => ({ code: 1, text: vec(refused(`${file}/c1`)) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: async (u) => { if (log.length > 40) throw new Error('pool loops'); return runUnit(u) }, retries: 0 })
  assert.equal(r.code, 1)
  assert.equal(log.length, 3)
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status), ['error'])
})
test('runPool: only the error, why or evidence field makes a connection fault, not other text of the form', async () => {
  const log = []
  const note = form('a/c1', 'error', 1, ', :log "ECONNRESET in the server log"')
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: vec(note) }), log), retries: 0 })
  assert.equal(log.length, 1)
  assert.equal(r.code, 1)
})
test('runPool: after the server is back the units wait the settle time before they start', async () => {
  const srv = fakeServer(40000)
  const starts = []
  const runUnit = async ({ file }) => { starts.push(srv.clock.t); return starts.length === 1 ? { code: 1, text: vec(refused(`${file}/c1`)) } : ok(file) }
  await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, settleMs: 7000, ...srv })
  assert.ok(starts[1] >= 47000)
})
test('serverProbe: true only when every port accepts a connection', async () => {
  const srv = net.createServer().listen(0, '127.0.0.1')
  await new Promise((r) => srv.on('listening', r))
  const open = srv.address().port
  const closed = net.createServer().listen(0, '127.0.0.1')
  await new Promise((r) => closed.on('listening', r))
  const shut = closed.address().port
  await new Promise((r) => closed.close(r))
  assert.equal(await serverProbe([open])(), true)
  assert.equal(await serverProbe([open, shut])(), false)
  srv.close()
})
test('runPool: a case rerun after a fault that fails with its known failed check is not rerun again', async () => {
  const log = []
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const runUnit = fakeRunner((file, match) => (log.length === 1 ? { code: 1, text: vec(form('a/c1', 'pass'), refused('a/c2')) } : { code: 1, text: vec(form(match, 'fail')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, known: new Map([['a/c2', '']]), retries: 1 })
  assert.equal(log.length, 2)
  assert.equal(r.skipped, 1)
  assert.match(r.text, /:known-failure true/)
})
test('runPool: on an unreachable stop a failure still waiting for its retry is returned as pending (inconclusive), not as a final failure', async () => {
  const srv = fakeServer(Infinity)
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const runUnit = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail'), refused('a/c2')) }), [])
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, retries: 1, serverWaitMs: 10000, ...srv })
  assert.equal(r.code, 3)
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status), ['inconclusive'])
  assert.equal(r.pending.length, 1)
})
test('runPool: on an unreachable stop the pending failure is written to the results as inconclusive with its first failure, and emitted', async () => {
  const srv = fakeServer(Infinity)
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const seen = []
  const runUnit = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail'), refused('a/c2')) }), [])
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, retries: 1, serverWaitMs: 10000, emit: (e) => seen.push(e), ...srv })
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status), ['inconclusive'])
  assert.match(r.text, /:first-failure/)
  assert.equal(resultsOf(seen).length, 1)
})
test('runPool: the settle wait also runs when the first probe after a fault already passes', async () => {
  const srv = fakeServer(0)
  const starts = []
  const runUnit = async ({ file }) => { starts.push(srv.clock.t); return starts.length === 1 ? { code: 1, text: vec(refused(`${file}/c1`)) } : ok(file) }
  await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, settleMs: 7000, ...srv })
  assert.ok(starts[1] >= 7000)
})
test('parsePoolArgs: --server-settle-s sets the settle wait in seconds (default 10)', () => {
  assert.equal(parsePoolArgs(['d']).serverSettleS, 10)
  assert.equal(parsePoolArgs(['d', '--server-settle-s', '3']).serverSettleS, 3)
})
test('runPool: in a mixed unit the cases the faulted child never reached are rerun, not recorded as errors', async () => {
  const log = []
  const listed = new Map([['a', ['a/c1', 'a/c2', 'a/c3']]])
  const runUnit = fakeRunner((file, match) => (log.length === 1 ? { code: 1, text: vec(form('a/c1', 'pass'), refused('a/c2')) } : { code: 0, text: vec(form(match, 'pass')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, total: 3 })
  assert.equal(r.code, 0)
  assert.deepEqual(log.map((l) => l.match).sort(), ['a/c2', 'a/c3', null])
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status), ['pass', 'pass', 'pass'])
})

test('parsePoolArgs: --match-id is repeatable and --match-ids FILE adds more; none of them pass through', () => {
  const p = parsePoolArgs(['dir', '--match-id', 'a/x', '--phase', 'day', '--match-id', 'b/y', '--match-ids', 'ids.txt'])
  assert.deepEqual(p.matchIds, ['a/x', 'b/y'])
  assert.equal(p.matchIdsFile, 'ids.txt')
  assert.deepEqual(p.passthrough, ['--phase', 'day'])
})
test('matchIdsFrom: one id per line (blank lines and # comments skipped); a results vector gives its non-pass, non-flaky ids once', () => {
  assert.deepEqual(matchIdsFrom('a/x\n\n # note\n b/y \n'), ['a/x', 'b/y'])
  const text = mergeText([form('a/p', 'pass'), form('a/f', 'fail'), form('a/f', 'fail'), form('b/e', 'error'), form('c/k', 'flaky')])
  assert.deepEqual(matchIdsFrom(text), ['a/f', 'b/e'])
})
test('listArgs: every match id is handed to --list so only the matched cases are counted', () => {
  const p = parsePoolArgs(['dir', '--match-id', 'a/x', '--match-id', 'b/y', '--phase', 'day'])
  assert.deepEqual(listArgs(p), ['dir', '--phase', 'day', '--match-id', 'a/x', '--match-id', 'b/y', '--list'])
})

// ---- night batches
const classes = (...pairs) => pairs.map(([id, cls]) => ({ id, cls }))
test('parseNightClasses: the --list --night-classes lines as {id, cls}; blank lines skipped', () => {
  assert.deepEqual(parseNightClasses('a/x  natural\n\nb/y  solo\nc/z  skip\n'), classes(['a/x', 'natural'], ['b/y', 'solo'], ['c/z', 'skip']))
})
test('durationsById: the elapsed seconds of each id in the earlier results (the last one wins)', () => {
  const m = durationsById([vec(form('a/1', 'pass', 5), form('a/2', 'pass', 9)), vec(form('a/1', 'pass', 7))])
  assert.deepEqual([...m], [['a/1', 7], ['a/2', 9]])
})
test('nightBatches: natural cases pack longest first into batches of at most the bodies; a skip batch; every solo case alone, last', () => {
  const cs = classes(['a/1', 'natural'], ['a/2', 'natural'], ['a/3', 'natural'], ['b/1', 'solo'], ['c/1', 'skip'], ['b/2', 'solo'])
  const dur = new Map([['a/1', 10], ['a/2', 30], ['a/3', 20]])
  const b = nightBatches(cs, dur, { bodies: 2, budgetS: 300, defaultS: 60 })
  assert.deepEqual(b, [{ kind: 'natural', ids: ['a/2', 'a/3'] }, { kind: 'natural', ids: ['a/1'] }, { kind: 'skip', ids: ['c/1'] }, { kind: 'solo', ids: ['b/1'] }, { kind: 'solo', ids: ['b/2'] }])
})
test('nightBatches: a natural case longer than the night budget runs alone; unseen cases count as defaultS', () => {
  const cs = classes(['a/long', 'natural'], ['a/new', 'natural'], ['a/s', 'natural'])
  const b = nightBatches(cs, new Map([['a/long', 400], ['a/s', 5]]), { bodies: 3, budgetS: 300, defaultS: 60 })
  assert.deepEqual(b, [{ kind: 'long', ids: ['a/long'] }, { kind: 'natural', ids: ['a/new', 'a/s'] }])
})
test('nightBatches: a batch is never larger than the bodies, and skip cases never mix with natural ones', () => {
  const cs = classes(...Array.from({ length: 7 }, (_, i) => [`n/${i}`, 'natural']), ...Array.from({ length: 3 }, (_, i) => [`k/${i}`, 'skip']))
  const b = nightBatches(cs, new Map(), { bodies: 3, budgetS: 300, defaultS: 10 })
  assert.deepEqual(b.map((x) => [x.kind, x.ids.length]), [['natural', 3], ['natural', 3], ['natural', 1], ['skip', 3]])
})
test('parsePoolArgs: --night-batches is the pool\'s own and not passed on', () => {
  const p = parsePoolArgs(['d', '--phase', 'night-exclusive', '--night-batches', '--allow-time'])
  assert.equal(p.nightBatches, true)
  assert.deepEqual(p.passthrough, ['--phase', 'night-exclusive', '--allow-time'])
  assert.equal(parsePoolArgs(['d']).nightBatches, false)
})
const batchRunner = (log, fn = (id) => ok(id.split('/')[0])) => async (unit) => { log.push(unit); await sleepy(); return fn(unit.id) }
test('runNightBatches: batches run one after the other, the cases of one batch together, each case with its batch id on its own body', async () => {
  const log = []
  let live = 0, peak = 0
  const run = async (unit) => { live++; peak = Math.max(peak, live); log.push(unit); await sleepy(); live--; return { code: 0, text: vec(form(unit.id, 'pass')) } }
  const batches = [{ kind: 'natural', ids: ['a/1', 'a/2'] }, { kind: 'solo', ids: ['b/1'] }]
  const r = await runNightBatches({ batches, workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(r.code, 0)
  assert.equal(peak, 2)
  assert.deepEqual(log.map((u) => u.id), ['a/1', 'a/2', 'b/1'])
  assert.equal(log[0].batch, log[1].batch); assert.notEqual(log[0].batch, log[2].batch)
  assert.notEqual(log[0].worker.body, log[1].worker.body)
  assert.equal(splitForms(r.text).length, 3)
})
test('runNightBatches: a failed case reruns alone in its own batch after the others; a pass is flaky', async () => {
  const log = []
  let n = 0
  const run = async (unit) => { log.push(unit); n++; return { code: 0, text: vec(form(unit.id, unit.id === 'a/2' && n < 3 ? 'fail' : 'pass')) } }
  const r = await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/1', 'a/2'] }, { kind: 'solo', ids: ['b/1'] }], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1 })
  assert.deepEqual(log.map((u) => u.id), ['a/1', 'a/2', 'b/1', 'a/2'])
  assert.notEqual(log[3].batch, log[1].batch)
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => [s.id, s.status]), [['a/1', 'pass'], ['a/2', 'flaky'], ['b/1', 'pass']])
  assert.equal(r.code, 0)
})
test('runNightBatches: a case with no result is an error; a failure twice stays failed; code 1', async () => {
  const run = async (unit) => (unit.id === 'a/1' ? { code: 3, text: null } : { code: 1, text: vec(form(unit.id, 'fail')) })
  const r = await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/1', 'a/2'] }], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1 })
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => [s.id, s.status]), [['a/1', 'error'], ['a/2', 'fail']])
  assert.equal(r.code, 1)
})
test('runNightBatches: events carry the fixed total and one result per case', async () => {
  const ev = []
  await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/1', 'a/2'] }], workers: workerSpecs(2, 19, 'P', 0), runUnit: batchRunner([], (id) => ({ code: 0, text: vec(form(id, 'pass')) })), total: 2, emit: (e) => ev.push(e) })
  assert.deepEqual(ev.filter((e) => e.event === 'result').map((e) => e.name), ['a/1#1', 'a/2#1'])
  assert.deepEqual(ev.filter((e) => e.event === 'progress').map((e) => [e.done, e.total]), [[1, 2], [2, 2]])
})

test('nightBatches: cases that set the same time group into barrier batches (before the solo ones); a batch carries its barrier size', () => {
  const cs = classes(['a/1', 'barrier:1000'], ['a/2', 'barrier:1000'], ['a/3', 'barrier:1000'], ['b/1', 'barrier:2000'], ['c/1', 'solo'])
  const b = nightBatches(cs, new Map(), { bodies: 2, budgetS: 300, defaultS: 60 })
  assert.deepEqual(b, [{ kind: 'barrier', ids: ['a/1', 'a/2'], barrier: 2 }, { kind: 'barrier', ids: ['a/3'], barrier: 1 }, { kind: 'barrier', ids: ['b/1'], barrier: 1 }, { kind: 'solo', ids: ['c/1'] }])
})
test('runNightBatches: a barrier batch hands its size to each case; a rerun has none; onBatchEnd gets every batch id', async () => {
  const log = []
  const ended = []
  const run = async (unit) => { log.push(unit); return { code: 0, text: vec(form(unit.id, unit.id === 'a/2' && log.length < 3 ? 'fail' : 'pass')) } }
  await runNightBatches({ batches: [{ kind: 'barrier', ids: ['a/1', 'a/2'], barrier: 2 }], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1, onBatchEnd: async (b) => { ended.push(b) } })
  assert.deepEqual(log.map((u) => u.barrier), [2, 2, undefined])
  assert.equal(ended.length, 2)
})

test('nightBatchEndArgs: the batch end gets the time-change flags of the run, nothing else', () => {
  assert.deepEqual(nightBatchEndArgs(['--phase', 'night-exclusive', '--allow-time', '--time-log', 'f.log', '--tag', 'x', '--card', 'c1'], 'B', 'n1'),
    ['--night-batch-end', 'n1', '--body', 'B', '--allow-time', '--time-log', 'f.log', '--card', 'c1'])
})

test('sleepRuleRestoreArgs: the restore gets the time-change flags of the run; dead restores only a crashed run\'s value', () => {
  const pass = ['--phase', 'night-exclusive', '--allow-time', '--time-log', 'f.log', '--tag', 'x', '--card', 'c1']
  assert.deepEqual(sleepRuleRestoreArgs(pass, 'B', false), ['--sleep-rule-restore', '--body', 'B', '--allow-time', '--time-log', 'f.log', '--card', 'c1'])
  assert.equal(sleepRuleRestoreArgs(pass, 'B', true)[0], '--sleep-rule-restore-dead')
})
