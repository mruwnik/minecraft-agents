// Why JavaScript: node --test file for tools/world-test-pool.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import net from 'node:net'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { splitForms, scanForms, failSig, summarize, unitOrder, parsePoolArgs, workerSpecs, runPool, mergeText, poolCap, createReaper, countListed, listArgs, knownFailures, parseListed, unitsWithCases, noCasesSelected, serverProbe, matchIdsFrom, parseNightClasses, nightBatchEndArgs, sleepRuleRestoreArgs, nightBatches, durationsById, runNightBatches, waitForBodies, PRECHECK_EXIT, BOOT_EXIT, traceDirOf, durationFiles, main, parseMspt, gateLoad, SWEEP_MIN_BODIES, markSweep, skipKnown, withoutUnskipped, childMatchArgs } from './world-test-pool.mjs'
import { pidFileBody } from './res-slot.mjs'

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
  assert.equal(failSig('{:status :fail, :expects [{:status :fail, :not-judged true, :expect {:event {:kind :a}}} {:status :fail, :expect {:event {:kind :b}}}]}'), '{:event {:kind :b}}', 'a not-judged expectation is not the failing check')
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

test('runPool: every unit runs once, results merged, exit 0', { timeout: 10000 }, async () => {
  const log = []
  const r = await runPool({ units: ['a', 'b', 'c'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner((f) => ok(f), log) })
  assert.equal(r.code, 0)
  assert.equal(splitForms(r.text).length, 6)
  assert.deepEqual(log.map((l) => l.file).sort(), ['a', 'b', 'c'])
  assert.equal(new Set(log.map((l) => l.body)).size, 2)
})
test('runPool: never more than the workers run at once', { timeout: 10000 }, async () => {
  let now = 0, peak = 0
  const run = async ({ file }) => { now++; peak = Math.max(peak, now); await sleepy(); now--; return ok(file) }
  await runPool({ units: ['a', 'b', 'c', 'd', 'e'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(peak, 2)
})
test('runPool: a failed case reruns once on a different body, by --match; a pass is :flaky, others kept', { timeout: 10000 }, async () => {
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
test('runPool: a case failing twice stays failed, exit 1, and is not rerun a third time', { timeout: 10000 }, async () => {
  const log = []
  const run = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(log.length, 2)
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => s.status), ['fail'])
  assert.equal(r.code, 1)
})
test('runPool: with one body the rerun uses it (no other to pick)', { timeout: 10000 }, async () => {
  const log = []
  const run = fakeRunner((f, match) => match ? { code: 0, text: vec(form('a/c1', 'pass')) } : { code: 1, text: vec(form('a/c1', 'fail')) }, log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: run })
  assert.deepEqual(log.map((l) => l.body), ['PA', 'PA'])
  assert.equal(r.code, 0)
})
test('runPool: a unit that dies with no results is rerun whole once', { timeout: 10000 }, async () => {
  const log = []
  const run = fakeRunner((file, match) => log.length === 1 ? { code: 3, text: null } : ok(file), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  assert.equal(log.length, 2)
  assert.equal(log[1].match, null)
  assert.equal(r.code, 0)
  assert.equal(splitForms(r.text).length, 2)
})
test('runPool: a unit that keeps dying is reported as an :error result, exit 1', { timeout: 10000 }, async () => {
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: async () => ({ code: 3, text: null }) })
  const s = splitForms(r.text).map(summarize)
  assert.deepEqual(s.map((x) => [x.id, x.status]), [['a', 'error']])
  assert.equal(r.code, 1)
})
test('runPool: inconclusive cases are not rerun and fail the exit code', { timeout: 10000 }, async () => {
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
test('runPool: --retry-failed N reruns a failing case exactly N times, no more', { timeout: 10000 }, async () => {
  const log = []
  const run = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 3 })
  assert.equal(log.length, 4)
  assert.equal(r.code, 1)
})
test('runPool: --retry-failed 0 reruns nothing', { timeout: 10000 }, async () => {
  const log = []
  await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail')) }), log), retries: 0 })
  assert.equal(log.length, 1)
})
test('runPool: an :error case is not rerun (the single-body runner retries failures only)', { timeout: 10000 }, async () => {
  const log = []
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'error')) }), log) })
  assert.equal(log.length, 1)
  assert.equal(r.code, 1)
})
test('runPool: --repeat results of one id are all kept, and a rerun replaces only its own run', { timeout: 10000 }, async () => {
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
test('runPool: a rerun asks for the exact case id (and one run), not a substring', { timeout: 10000 }, async () => {
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
test('runPool: waits (polling) while the load average is above the core count, before starting another child', { timeout: 10000 }, async () => {
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
test('runPool: a load-gate wait is logged once it ends, with the body, the load, the limit and the seconds waited', { timeout: 10000 }, async () => {
  const loads = [20, 20, 3, 20, 3, 3, 3, 3, 3, 3]
  let polls = 0, t = 0
  const logs = []
  await runPool({
    units: ['a', 'b'], workers: workerSpecs(2, 19, 'P', 0),
    runUnit: async ({ file }) => { await new Promise((r) => setTimeout(r, 20)); return ok(file) },
    load: () => { polls++; return loads[Math.min(polls - 1, loads.length - 1)] }, cores: 8,
    sleep: async (ms) => { t += ms }, now: () => t, log: (m) => logs.push(m),
  })
  assert.equal(logs.length, 1)
  assert.match(logs[0], /P\w* waited \d+ s for the load to drop to 8 or less \(load 20, now 3\)/)
})
test('runPool: a high load never blocks when nothing is running (no livelock)', { timeout: 10000 }, async () => {
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
test('runPool events: the total is the one given from the start, in every progress line', { timeout: 10000 }, async () => {
  const run = fakeRunner((f) => ({ code: 0, text: vec(form(`${f}/1`, 'pass'), form(`${f}/2`, 'pass')) }), [])
  const seen = await eventsOf(['a', 'b'], run, 4)
  assert.deepEqual(progressOf(seen).map((e) => [e.done, e.total, e.retries]), [[1, 4, 0], [2, 4, 0], [3, 4, 0], [4, 4, 0]])
  assert.equal(resultsOf(seen).length, 4)
})
test('runPool events: a case failing and failing again is one failed result, the reruns are counted as retries', { timeout: 10000 }, async () => {
  const run = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail'), form('a/c2', 'pass')) }), [])
  const seen = await eventsOf(['a'], run, 2, 2)
  const results = resultsOf(seen)
  assert.deepEqual(results.map((e) => [e.name, e.outcome]).sort(), [['a/c1#1', 'failed'], ['a/c2#1', 'passed']])
  const last = progressOf(seen).at(-1)
  assert.deepEqual([last.done, last.total, last.retries], [2, 2, 2])
})
test('runPool events: a failure that passes on the rerun is reported once, as flaky, not as a failure', { timeout: 10000 }, async () => {
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
test('runPool: a failure with a different first failing check than last time is rerun, not a known failure', { timeout: 10000 }, async () => {
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
test('runPool: a failed case that is a known failure is not rerun, is marked :known-failure true and counted as skipped', { timeout: 10000 }, async () => {
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
test('runPool: a unit that dies twice settles one error per listed case (and run), so done reaches the total', { timeout: 10000 }, async () => {
  const listed = new Map([['a', ['a/c1', 'a/c2', 'a/c3']]])
  const seen = []
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: null }), []), listed, repeat: 2, total: 6, emit: (e) => seen.push(e) })
  assert.deepEqual(splitForms(r.text).map((f) => { const s = summarize(f); return [s.id, s.run, s.status] }).sort(),
    [['a/c1', 1, 'error'], ['a/c1', 2, 'error'], ['a/c2', 1, 'error'], ['a/c2', 2, 'error'], ['a/c3', 1, 'error'], ['a/c3', 2, 'error']])
  assert.equal(progressOf(seen).at(-1).done, 6)
  assert.equal(resultsOf(seen).length, 6)
})
test('runPool: listed cases missing from a unit that ended with partial results settle as errors', { timeout: 10000 }, async () => {
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
test('runPool: no unit ran is exit 2 (no cases selected), like the single-body runner', { timeout: 10000 }, async () => {
  const r = await runPool({ units: [], workers: workerSpecs(2, 19, 'P', 0), runUnit: fakeRunner((f) => ok(f), []) })
  assert.equal(r.code, 2)
})

// ---- server down: a connection error is a setup fault, not a case result
const refused = (id) => form(id, 'error', 1, ', :evidence "connect ECONNREFUSED 127.0.0.1:25575"')
const fakeServer = (downUntilMs) => { // a clock the fake sleep advances, and a probe that refuses until downUntilMs
  const clock = { t: 0 }
  return { clock, now: () => clock.t, sleep: async (ms) => { clock.t += ms }, probe: async () => clock.t >= downUntilMs }
}
test('runPool: cases that hit a refused connection wait for the server and rerun without using a retry or showing as errors', { timeout: 10000 }, async () => {
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
test('runPool: a unit that exits with no results while the server is down is held and rerun whole once the server is back', { timeout: 10000 }, async () => {
  const srv = fakeServer(20000)
  const log = []
  const runUnit = fakeRunner((file) => (log.length === 1 ? { code: 1, text: null } : ok(file)), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, ...srv })
  assert.equal(r.code, 0)
  assert.equal(log.length, 2)
})
test('runPool: no new unit starts while the server is down', { timeout: 10000 }, async () => {
  const srv = fakeServer(30000)
  const starts = []
  const runUnit = async ({ file }) => { starts.push([file, srv.clock.t]); return starts.length === 1 ? { code: 1, text: vec(refused(`${file}/c1`)) } : ok(file) }
  await runPool({ units: ['a', 'b', 'c'], workers: workerSpecs(1, 19, 'P', 0), runUnit, ...srv })
  assert.ok(starts.slice(1).every(([, t]) => t >= 30000))
})
test('runPool: a mixed unit keeps its good results and reruns only the refused case', { timeout: 10000 }, async () => {
  const srv = fakeServer(5000)
  const log = []
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const runUnit = fakeRunner((file, match) => (log.length === 1 ? { code: 1, text: vec(form('a/c1', 'pass'), refused('a/c2')) } : { code: 0, text: vec(form(match, 'pass')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, total: 2, ...srv })
  assert.equal(r.code, 0)
  assert.deepEqual(log.map((l) => l.match), [null, 'a/c2'])
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status).sort(), ['pass', 'pass'])
})
test('runPool: past the wait cap the pool stops as server unreachable (exit 3), recording no errors', { timeout: 10000 }, async () => {
  const srv = fakeServer(Infinity)
  const r = await runPool({ units: ['a', 'b'], workers: workerSpecs(1, 19, 'P', 0), runUnit: fakeRunner((file) => ({ code: 1, text: vec(refused(`${file}/c1`)) }), []), serverWaitMs: 60000, ...srv })
  assert.equal(r.code, 3)
  assert.equal(r.unreachable, true)
  assert.deepEqual(splitForms(r.text), [])
  assert.ok(srv.clock.t >= 60000)
})
test('parsePoolArgs: --server-wait-s and --server-settle-s set the wait and settle seconds (defaults 300, 10)', () => {
  assert.deepEqual([parsePoolArgs(['d']).serverWaitS, parsePoolArgs(['d']).serverSettleS], [300, 10])
  const p = parsePoolArgs(['d', '--server-wait-s', '90', '--server-settle-s', '3'])
  assert.deepEqual([p.serverWaitS, p.serverSettleS], [90, 3])
})

// ---- review findings of the fault handling (card 6bfe1115)
test('runPool: a connection fault that persists while the probe says the server is up is rerun at most twice, then settles as an error', { timeout: 40000 }, async () => {
  const log = []
  const runUnit = fakeRunner((file) => ({ code: 1, text: vec(refused(`${file}/c1`)) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: async (u) => { if (log.length > 40) throw new Error('pool loops'); return runUnit(u) }, retries: 0 })
  assert.equal(r.code, 1)
  assert.equal(log.length, 3)
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status), ['error'])
})
test('runPool: only the error, why or evidence field makes a connection fault, not other text of the form', { timeout: 10000 }, async () => {
  const log = []
  const note = form('a/c1', 'error', 1, ', :log "ECONNRESET in the server log"')
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: fakeRunner(() => ({ code: 1, text: vec(note) }), log), retries: 0 })
  assert.equal(log.length, 1)
  assert.equal(r.code, 1)
})
test('runPool: after the server is back the units wait the settle time before they start', { timeout: 10000 }, async () => {
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
test('runPool: a case rerun after a fault that fails with its known failed check is not rerun again', { timeout: 40000 }, async () => {
  const log = []
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const runUnit = fakeRunner((file, match) => (log.length === 1 ? { code: 1, text: vec(form('a/c1', 'pass'), refused('a/c2')) } : { code: 1, text: vec(form(match, 'fail')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, known: new Map([['a/c2', '']]), retries: 1 })
  assert.equal(log.length, 2)
  assert.equal(r.skipped, 1)
  assert.match(r.text, /:known-failure true/)
})
test('runPool: on an unreachable stop a failure still waiting for its retry is returned as pending (inconclusive), not as a final failure', { timeout: 10000 }, async () => {
  const srv = fakeServer(Infinity)
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const runUnit = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail'), refused('a/c2')) }), [])
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, retries: 1, serverWaitMs: 10000, ...srv })
  assert.equal(r.code, 3)
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status), ['inconclusive'])
  assert.equal(r.pending.length, 1)
})
test('runPool: on an unreachable stop the pending failure is written to the results as inconclusive with its first failure, and emitted', { timeout: 10000 }, async () => {
  const srv = fakeServer(Infinity)
  const listed = new Map([['a', ['a/c1', 'a/c2']]])
  const seen = []
  const runUnit = fakeRunner(() => ({ code: 1, text: vec(form('a/c1', 'fail'), refused('a/c2')) }), [])
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, retries: 1, serverWaitMs: 10000, emit: (e) => seen.push(e), ...srv })
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).status), ['inconclusive'])
  assert.match(r.text, /:first-failure/)
  assert.equal(resultsOf(seen).length, 1)
})
test('runPool: the settle wait also runs when the first probe after a fault already passes', { timeout: 10000 }, async () => {
  const srv = fakeServer(0)
  const starts = []
  const runUnit = async ({ file }) => { starts.push(srv.clock.t); return starts.length === 1 ? { code: 1, text: vec(refused(`${file}/c1`)) } : ok(file) }
  await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit, settleMs: 7000, ...srv })
  assert.ok(starts[1] >= 7000)
})
test('runPool: in a mixed unit the cases the faulted child never reached are rerun, not recorded as errors', { timeout: 40000 }, async () => {
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
test('nightBatches: natural cases form one rolling batch, longest first, with the night budget and their durations; a skip batch; every solo case alone, last', () => {
  const cs = classes(['a/1', 'natural'], ['a/2', 'natural'], ['a/3', 'natural'], ['b/1', 'solo'], ['c/1', 'skip'], ['b/2', 'solo'])
  const dur = new Map([['a/1', 10], ['a/2', 30], ['a/3', 20]])
  const b = nightBatches(cs, dur, { bodies: 2, budgetS: 300, defaultS: 60 })
  assert.deepEqual(b, [{ kind: 'natural', ids: ['a/2', 'a/3', 'a/1'], budgetS: 300, secs: { 'a/2': 30, 'a/3': 20, 'a/1': 10 } }, { kind: 'skip', ids: ['c/1'] }, { kind: 'solo', ids: ['b/1'] }, { kind: 'solo', ids: ['b/2'] }])
})
test('nightBatches: a natural case longer than the night budget runs alone; unseen cases count as defaultS', () => {
  const cs = classes(['a/long', 'natural'], ['a/new', 'natural'], ['a/s', 'natural'])
  const b = nightBatches(cs, new Map([['a/long', 400], ['a/s', 5]]), { bodies: 3, budgetS: 300, defaultS: 60 })
  assert.deepEqual(b, [{ kind: 'long', ids: ['a/long'] }, { kind: 'natural', ids: ['a/new', 'a/s'], budgetS: 300, secs: { 'a/new': 60, 'a/s': 5 } }])
})
test('nightBatches: a skip batch is never larger than the bodies, and skip cases never mix with natural ones', () => {
  const cs = classes(...Array.from({ length: 7 }, (_, i) => [`n/${i}`, 'natural']), ...Array.from({ length: 4 }, (_, i) => [`k/${i}`, 'skip']))
  const b = nightBatches(cs, new Map(), { bodies: 3, budgetS: 300, defaultS: 10 })
  assert.deepEqual(b.map((x) => [x.kind, x.ids.length]), [['natural', 7], ['skip', 3], ['skip', 1]])
})
test('parsePoolArgs: --night-batches is the pool\'s own and not passed on', () => {
  const p = parsePoolArgs(['d', '--phase', 'night-exclusive', '--night-batches', '--allow-time'])
  assert.equal(p.nightBatches, true)
  assert.deepEqual(p.passthrough, ['--phase', 'night-exclusive', '--allow-time'])
  assert.equal(parsePoolArgs(['d']).nightBatches, false)
})
const batchRunner = (log, fn = (id) => ok(id.split('/')[0])) => async (unit) => { log.push(unit); await sleepy(); return fn(unit.id) }
test('runNightBatches: batches run one after the other, the cases of one batch together, each case with its batch id on its own body', { timeout: 10000 }, async () => {
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
test('runNightBatches: a failed case reruns in a batch of its own after the others; a pass is flaky', { timeout: 10000 }, async () => {
  const log = []
  let n = 0
  const run = async (unit) => { log.push(unit); n++; return { code: 0, text: vec(form(unit.id, unit.id === 'a/2' && n < 3 ? 'fail' : 'pass')) } }
  const r = await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/1', 'a/2'] }, { kind: 'solo', ids: ['b/1'] }], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1 })
  assert.deepEqual(log.map((u) => u.id), ['a/1', 'a/2', 'b/1', 'a/2'])
  assert.notEqual(log[3].batch, log[1].batch)
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => [s.id, s.status]), [['a/1', 'pass'], ['a/2', 'flaky'], ['b/1', 'pass']])
  assert.equal(r.code, 0)
})
test('runNightBatches: a case inconclusive for overrunning dawn reruns and keeps its first reason; other inconclusive cases do not rerun', { timeout: 10000 }, async () => {
  const log = []
  const over = ', :overran true, :first-why "boom"'
  const run = async (unit) => {
    log.push(unit.id)
    const again = log.filter((x) => x === unit.id).length > 1
    if (unit.id === 'a/1') return { code: 1, text: vec(form(unit.id, again ? 'pass' : 'inconclusive', 1, again ? '' : over)) }
    return { code: 1, text: vec(form(unit.id, 'inconclusive')) }
  }
  const r = await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/1', 'a/2'] }], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1 })
  assert.deepEqual(log, ['a/1', 'a/2', 'a/1'])
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => [s.id, s.status]), [['a/1', 'flaky'], ['a/2', 'inconclusive']])
  assert.match(r.text, /:first-failure .*:first-why "boom"/)
})
test('runNightBatches: an overrun that overruns again settles inconclusive and the event says so', { timeout: 10000 }, async () => {
  const ev = []
  const run = async (unit) => ({ code: 1, text: vec(form(unit.id, 'inconclusive', 1, ', :overran true, :first-why "boom"')) })
  const r = await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/1'] }], workers: workerSpecs(1, 19, 'P', 0), runUnit: run, retries: 2, total: 1, emit: (e) => ev.push(e) })
  assert.equal(splitForms(r.text).map(summarize)[0].status, 'inconclusive')
  assert.deepEqual(ev.filter((e) => e.event === 'result').map((e) => e.outcome), ['inconclusive'])
  assert.equal(ev.find((e) => e.event === 'progress').retries, 2)
})
test('runNightBatches: a case with no result is an error; a failure twice stays failed; code 1', { timeout: 10000 }, async () => {
  const run = async (unit) => (unit.id === 'a/1' ? { code: 3, text: null } : { code: 1, text: vec(form(unit.id, 'fail')) })
  const r = await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/1', 'a/2'] }], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1 })
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => [s.id, s.status]), [['a/1', 'error'], ['a/2', 'fail']])
  assert.equal(r.code, 1)
})
test('runNightBatches: events carry the fixed total and one result per case', { timeout: 10000 }, async () => {
  const ev = []
  await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/1', 'a/2'] }], workers: workerSpecs(2, 19, 'P', 0), runUnit: batchRunner([], (id) => ({ code: 0, text: vec(form(id, 'pass')) })), total: 2, emit: (e) => ev.push(e) })
  assert.deepEqual(ev.filter((e) => e.event === 'result').map((e) => e.name), ['a/1#1', 'a/2#1'])
  assert.deepEqual(ev.filter((e) => e.event === 'progress').map((e) => [e.done, e.total]), [[1, 2], [2, 2]])
})

test('nightBatches: cases that start alike and set the same time group into barrier batches (before the solo ones); a batch carries its barrier size and class', () => {
  const cs = classes(['a/1', 'barrier:1000'], ['a/2', 'barrier:1000'], ['a/3', 'barrier:1000'], ['b/1', 'barrier:day-11500'], ['b/2', 'barrier:11500'], ['c/1', 'solo'])
  const b = nightBatches(cs, new Map(), { bodies: 2, budgetS: 300, defaultS: 60 })
  assert.deepEqual(b, [{ kind: 'barrier', ids: ['a/1', 'a/2'], barrier: 2, key: 'barrier:1000' }, { kind: 'barrier', ids: ['a/3'], barrier: 1, key: 'barrier:1000' },
    { kind: 'barrier', ids: ['b/1'], barrier: 1, key: 'barrier:day-11500' }, { kind: 'barrier', ids: ['b/2'], barrier: 1, key: 'barrier:11500' }, { kind: 'solo', ids: ['c/1'] }])
})
test('runNightBatches: a barrier batch hands its size to each case; a rerun meets the other failures of its batch; onBatchEnd gets every batch id', { timeout: 10000 }, async () => {
  const log = []
  const ended = []
  const run = async (unit) => { log.push(unit); return { code: 0, text: vec(form(unit.id, unit.id === 'a/2' && log.length < 3 ? 'fail' : 'pass')) } }
  await runNightBatches({ batches: [{ kind: 'barrier', ids: ['a/1', 'a/2'], barrier: 2 }], workers: workerSpecs(2, 19, 'P', 0), runUnit: run, retries: 1, onBatchEnd: async (b) => { ended.push(b) } })
  assert.deepEqual(log.map((u) => u.barrier), [2, 2, 1])
  assert.equal(ended.length, 2)
})
test('runNightBatches: a natural batch keeps every body busy in one night: a freed body starts the next case while its duration fits the budget; the rest go to the next night', { timeout: 10000 }, async () => {
  const log = []
  let t = 0
  let openGate
  const gate = new Promise((r) => { openGate = r })
  const secs = { 'a/long': 90, 'a/1': 10, 'a/2': 10, 'a/3': 85 }
  const run = async (unit) => {
    log.push(unit)
    if (unit.id === 'a/long') await gate
    else { await sleepy(); t += secs[unit.id]; if (unit.id === 'a/2') openGate() }
    return { code: 0, text: vec(form(unit.id, 'pass')) }
  }
  const batches = [{ kind: 'natural', ids: ['a/long', 'a/1', 'a/2', 'a/3'], budgetS: 100, secs }]
  const r = await runNightBatches({ batches, workers: workerSpecs(2, 19, 'P', 0), runUnit: run, now: () => t * 1000 })
  assert.deepEqual(log.map((u) => u.id), ['a/long', 'a/1', 'a/2', 'a/3'])
  assert.deepEqual(log.slice(0, 3).map((u) => u.batch), Array(3).fill(log[0].batch))
  assert.notEqual(log[3].batch, log[0].batch, '20 s in, 85 s more does not fit the 100 s night')
  assert.equal(log[1].worker.body, log[2].worker.body)
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => s.id), ['a/long', 'a/1', 'a/2', 'a/3'])
})
test('runNightBatches: failed cases rerun after the others, batched by class: natural failures share one night, barrier failures meet again, solo ones alone', { timeout: 10000 }, async () => {
  const log = []
  const failFirst = new Set(['a/1', 'a/3', 'b/1', 'b/2', 'c/1', 'c/2'])
  const run = async (unit) => { log.push(unit); return { code: 0, text: vec(form(unit.id, unit.attempt === 0 && failFirst.has(unit.id) ? 'fail' : 'pass')) } }
  const batches = [{ kind: 'natural', ids: ['a/1', 'a/2', 'a/3'], budgetS: 300, secs: {} }, { kind: 'barrier', ids: ['b/1', 'b/2'], barrier: 2, key: 'barrier:1000' }, { kind: 'solo', ids: ['c/1'] }, { kind: 'solo', ids: ['c/2'] }]
  const r = await runNightBatches({ batches, workers: workerSpecs(3, 19, 'P', 0), runUnit: run, retries: 1 })
  const reruns = log.filter((u) => u.attempt === 1)
  const batchOf = (id) => reruns.find((u) => u.id === id).batch
  assert.deepEqual(reruns.map((u) => u.id).sort(), ['a/1', 'a/3', 'b/1', 'b/2', 'c/1', 'c/2'])
  assert.equal(batchOf('a/1'), batchOf('a/3'))
  assert.equal(batchOf('b/1'), batchOf('b/2'))
  assert.deepEqual(reruns.filter((u) => u.id.startsWith('b/')).map((u) => u.barrier), [2, 2])
  assert.notEqual(batchOf('c/1'), batchOf('c/2'))
  assert.equal(new Set(reruns.map((u) => u.batch)).size, 4)
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => s.status), ['flaky', 'pass', 'flaky', 'flaky', 'flaky', 'flaky', 'flaky'])
  assert.equal(r.code, 0)
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

test('parsePoolArgs: --min-bodies and --min-bodies-wait-s are pool flags (default none, 1800 s)', () => {
  assert.deepEqual([parsePoolArgs([]).minBodies, parsePoolArgs([]).minBodiesWaitS], [0, 1800])
  const p = parsePoolArgs(['--min-bodies', '8', '--min-bodies-wait-s', '60', '--bodies', '16'])
  assert.deepEqual([p.minBodies, p.minBodiesWaitS, p.passthrough], [8, 60, []])
})
test('waitForBodies: polls until the cap reaches the minimum, logging the figure every minute', async () => {
  let t = 0
  const caps = [2, 2, 3, 5, 8]
  const logs = []
  const r = await waitForBodies({ min: 8, cap: () => caps.shift() ?? 8, sleep: async (ms) => { t += ms }, now: () => t, pollMs: 30000, logMs: 60000, waitMs: 1800000, log: (m) => logs.push(m) })
  assert.deepEqual([r.ok, r.cap], [true, 8])
  assert.equal(logs.length, 2, 'one line per minute, not per poll')
  assert.match(logs[0], /2 .*8/)
})
test('waitForBodies: gives up after the wait with a clear message; a cap already high does not wait', async () => {
  let t = 0
  const r = await waitForBodies({ min: 8, cap: () => 3, sleep: async (ms) => { t += ms }, now: () => t, pollMs: 30000, logMs: 60000, waitMs: 120000, log: () => {} })
  assert.equal(r.ok, false)
  assert.match(r.message, /only 3 bodies.*8.*120 s/)
  const q = await waitForBodies({ min: 4, cap: () => 4, sleep: async () => { throw new Error('slept') }, now: () => 0, log: () => {} })
  assert.equal(q.ok, true)
})
test('runPool: a unit that exits with the precheck code stops the pass at once (exit 5), starting no unit and recording no errors', { timeout: 10000 }, async () => {
  const log = []
  const runUnit = fakeRunner(() => ({ code: PRECHECK_EXIT, text: null }), log)
  const listed = new Map([['a', ['a/c1', 'a/c2']], ['b', ['b/c1']], ['c', ['c/c1']]])
  const r = await runPool({ units: ['a', 'b', 'c'], workers: workerSpecs(1, 19, 'P', 0), runUnit, listed, total: 4 })
  assert.equal(r.code, 5)
  assert.equal(r.precheck, true)
  assert.equal(log.length, 1)
  assert.deepEqual(splitForms(r.text), [])
})
test('runNightBatches: a unit that exits with the precheck code stops the pass after that batch (exit 5), recording no errors', { timeout: 10000 }, async () => {
  const log = []
  const runUnit = async ({ id }) => { log.push(id); return { code: PRECHECK_EXIT, text: null } }
  const batches = [{ kind: 'night', ids: ['a/c1'] }, { kind: 'night', ids: ['b/c1'] }]
  const r = await runNightBatches({ batches, workers: workerSpecs(1, 19, 'P', 0), runUnit, total: 2 })
  assert.equal(r.code, 5)
  assert.deepEqual(log, ['a/c1'])
  assert.deepEqual(splitForms(r.text), [])
})

test('runPool events: a first attempt that fails or dies is announced as a progress line (attempted) before its rerun has finished', { timeout: 10000 }, async () => {
  const run = fakeRunner((f, match) => match ? { code: 0, text: vec(form('a/c1', 'pass')) } : { code: 1, text: vec(form('a/c1', 'fail'), form('a/c2', 'pass')) }, [])
  const seen = []
  await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: run, total: 2, emit: (e) => seen.push(e) })
  const first = progressOf(seen).find((e) => e.attempted !== undefined)
  assert.deepEqual([first.done, first.attempted, first.total], [1, 2, 2])
  assert.ok(seen.indexOf(first) < seen.findIndex((e) => e.outcome === 'flaky'))
})
test('runPool events: a unit that died with no results is announced as attempted while done stays 0', { timeout: 10000 }, async () => {
  const seen = []
  const run = fakeRunner((f, match) => match === null && !seen.length ? { code: 1, text: null } : { code: 0, text: vec(form('a/c1', 'pass')) }, [])
  await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: run, total: 1, emit: (e) => seen.push(e) })
  assert.deepEqual([progressOf(seen)[0].done, progressOf(seen)[0].attempted], [0, 1])
})
test('runPool: a unit that exits with the boot-death code stops the pass at once (exit 6), keeping the results it wrote', { timeout: 10000 }, async () => {
  const log = []
  const runUnit = fakeRunner((f) => ({ code: BOOT_EXIT, text: vec(form(`${f}/c1`, 'error')) }), log)
  const r = await runPool({ units: ['a', 'b', 'c'], workers: workerSpecs(1, 19, 'P', 0), runUnit, total: 3 })
  assert.equal(r.code, 6)
  assert.equal(r.bootDead, true)
  assert.equal(log.length, 1)
  assert.equal(splitForms(r.text).length, 1)
})

const bootErr = (id) => form(id, 'error', 1, ', :why "body did not start within 90 s"')
test('runPool: a body that dies at boot is retired, its cases go to the other bodies and the pass goes on', { timeout: 10000 }, async () => {
  const log = []
  const runUnit = async ({ file, worker }) => {
    log.push({ file, body: worker.body })
    return worker.body === 'PA' ? { code: BOOT_EXIT, text: vec(bootErr(`${file}/c1`)) } : { code: 0, text: vec(form(`${file}/c1`, 'pass')) }
  }
  const listed = new Map([['a', ['a/c1']], ['b', ['b/c1']], ['c', ['c/c1']]])
  const r = await runPool({ units: ['a', 'b', 'c'], workers: workerSpecs(2, 19, 'P', 0), runUnit, listed, total: 3 })
  assert.equal(r.code, 0)
  assert.equal(r.bootDead, undefined)
  assert.deepEqual(splitForms(r.text).map((f) => summarize(f).id).sort(), ['a/c1', 'b/c1', 'c/c1'])
  assert.equal(log.filter((l) => l.body === 'PA').length, 1, 'the retired body ran nothing more')
})
test('runPool: when every body died at boot the pass stops with exit 6, keeping what was recorded', { timeout: 10000 }, async () => {
  const runUnit = async ({ file }) => ({ code: BOOT_EXIT, text: vec(bootErr(`${file}/c1`)) })
  const r = await runPool({ units: ['a', 'b', 'c'], workers: workerSpecs(2, 19, 'P', 0), runUnit, total: 3 })
  assert.equal(r.code, 6)
  assert.equal(r.bootDead, true)
})
test('runPool: with one of two bodies retired at boot, a failed case on the live body still reruns on it (no stall)', { timeout: 10000 }, async () => {
  const log = []
  const runUnit = async ({ file, match, worker }) => {
    log.push({ file, match, body: worker.body })
    if (worker.body === 'PA') return { code: BOOT_EXIT, text: vec(bootErr(`${file}/c1`)) }
    return match ? { code: 0, text: vec(form('a/c1', 'pass')) } : { code: 1, text: vec(form('a/c1', 'fail')) }
  }
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit, listed: new Map([['a', ['a/c1']]]), total: 1 })
  assert.equal(r.code, 0)
  assert.equal(summarize(splitForms(r.text)[0]).status, 'flaky')
  assert.deepEqual(log.filter((l) => l.body === 'PB').map((l) => l.match), [null, 'a/c1'])
})
test('runPool: a flaky entry\'s :first-failure carries the failing run\'s :trace', { timeout: 10000 }, async () => {
  const run = fakeRunner((f, match) => match
    ? { code: 0, text: vec(form('a/c1', 'pass')) }
    : { code: 1, text: vec(form('a/c1', 'fail', 1, ', :trace "cases/a__c1__r1-a0.edn"')) }, [])
  const r = await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit: run })
  const flaky = splitForms(r.text).find((f) => summarize(f).id === 'a/c1')
  assert.match(flaky, /:first-failure \{[^]*:trace "cases\/a__c1__r1-a0.edn"[^]*\}[^]*:first-failure-body/)
  assert.doesNotMatch(flaky.replace(/:first-failure \{[^]*\}[^]*:first-failure-body/, ''), /:trace/)
})
test('runPool: a rerun of a failed case tells runUnit its attempt number (first run 0)', { timeout: 10000 }, async () => {
  const seen = []
  const runUnit = async ({ file, match, attempt }) => { seen.push([match, attempt]); return match ? { code: 0, text: vec(form('a/c1', 'pass')) } : { code: 1, text: vec(form('a/c1', 'fail')) } }
  await runPool({ units: ['a'], workers: workerSpecs(2, 19, 'P', 0), runUnit, total: 1 })
  assert.deepEqual(seen, [[null, 0], ['a/c1', 1]])
})
test('runNightBatches: a case whose body died at boot runs again on another body; the dead body is not used again', { timeout: 10000 }, async () => {
  const log = []
  const runUnit = async ({ id, worker }) => {
    log.push([id, worker.body])
    return worker.body === 'PA' && log.length === 1 ? { code: BOOT_EXIT, text: vec(bootErr(id)) } : { code: 0, text: vec(form(id, 'pass')) }
  }
  const batches = [{ kind: 'natural', ids: ['a/c1', 'b/c1'] }, { kind: 'natural', ids: ['c/c1', 'd/c1'] }]
  const r = await runNightBatches({ batches, workers: workerSpecs(2, 19, 'P', 0), runUnit, total: 4 })
  assert.equal(r.code, 0)
  assert.equal(splitForms(r.text).length, 4)
  assert.deepEqual(log.filter(([, b]) => b === 'PA').length, 1)
  assert.deepEqual(log.slice(-2).map(([id]) => id).sort(), ['c/c1', 'd/c1'])
  assert.deepEqual(log.slice(-2).map(([, b]) => b), ['PB', 'PB'], 'the dead body is skipped in later batches')
})
test('runNightBatches: with no body left the pass stops with exit 6', { timeout: 10000 }, async () => {
  const runUnit = async ({ id }) => ({ code: BOOT_EXIT, text: vec(bootErr(id)) })
  const r = await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/c1'] }, { kind: 'natural', ids: ['b/c1'] }], workers: workerSpecs(2, 19, 'P', 0), runUnit, total: 2 })
  assert.equal(r.code, 6)
  assert.equal(r.bootDead, true)
})
test('traceDirOf: cases/ next to the --results file, none without one', () => {
  assert.equal(traceDirOf({ results: '/d/sweep.edn' }), '/d/cases')
  assert.equal(traceDirOf({ results: null }), null)
})

test('durationFiles: skips missing paths, directories and non-.edn files', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'pool-dur-'))
  const edn = path.join(dir, 'a.edn')
  fs.writeFileSync(edn, '[]')
  fs.writeFileSync(path.join(dir, 'b.txt'), '[]')
  fs.mkdirSync(path.join(dir, 'd.edn'))
  assert.deepEqual(durationFiles([edn, path.join(dir, 'b.txt'), path.join(dir, 'd.edn'), dir, path.join(dir, 'gone.edn')]), [edn])
  fs.rmSync(dir, { recursive: true, force: true })
})
test('main: no fixture path is a clear usage error (exit 2)', async () => {
  const err = console.error
  const lines = []
  console.error = (m) => lines.push(m)
  try { assert.equal(await main(['--bodies', '2', '--durations', 'a.edn']), 2) } finally { console.error = err }
  assert.match(lines.join('\n'), /no fixture file or directory/)
})

test('parseMspt: reads the average tick time from a `tick query` reply, null when absent', () => {
  assert.equal(parseMspt('Target tick rate: 20.0 per second.\nAverage time per tick: 23.4ms (Target: 50.0ms)'), 23.4)
  assert.equal(parseMspt('Unknown or incomplete command'), null)
  assert.equal(parseMspt(''), null)
})
test('gateLoad: the server tick time when readable, else the load average scaled to the same limit', async () => {
  assert.equal(await gateLoad({ mspt: async () => 31, loadavg: () => 99, cores: 16, limit: 45 }), 31)
  assert.equal(await gateLoad({ mspt: async () => null, loadavg: () => 24, cores: 16, limit: 45 }), 45, 'load at 1.5x cores equals the limit')
  assert.equal(await gateLoad({ mspt: async () => { throw new Error('rcon down') }, loadavg: () => 12, cores: 16, limit: 45 }), 22.5)
})
test('runPool: an async load source gates the same way', { timeout: 10000 }, async () => {
  const loads = [90, 20, 20]
  let polls = 0
  const started = []
  const r = await runPool({
    units: ['a', 'b'], workers: workerSpecs(2, 19, 'P', 0),
    runUnit: async ({ file }) => { started.push([file, polls]); await sleepy(); return ok(file) },
    load: async () => loads[Math.min(polls++, 2)], cores: 45, sleep: async () => {},
  })
  assert.equal(r.code, 0)
  assert.ok(started[1][1] >= 2)
})
test('markSweep: a pool of SWEEP_MIN_BODIES or more bodies writes its pid to the sweep marker and the returned fn removes it', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'sweep-mark-'))
  const file = path.join(dir, 'sweep-active')
  assert.equal(markSweep(dir, SWEEP_MIN_BODIES - 1), null)
  assert.equal(fs.existsSync(file), false)
  const clear = markSweep(dir, SWEEP_MIN_BODIES)
  assert.equal(fs.readFileSync(file, 'utf8'), pidFileBody(process.pid))
  clear()
  assert.equal(fs.existsSync(file), false)
  fs.rmSync(dir, { recursive: true, force: true })
})

test('parsePoolArgs: --run-known is a pool flag and not passed to the children', () => {
  assert.equal(parsePoolArgs(['dir']).runKnown, false)
  const p = parsePoolArgs(['dir', '--run-known', '--phase', 'day'])
  assert.equal(p.runKnown, true)
  assert.deepEqual(p.passthrough, ['--phase', 'day'])
})
test('skipKnown: the ids with a known failure are not run; each gets a :skipped-known form per run carrying the previous failure', () => {
  const known = new Map([['a/c1', '{:event {:kind :x}}'], ['a/c3', '']])
  const r = skipKnown(['a/c1', 'a/c2', 'a/c3'], known, 2)
  assert.deepEqual(r.keep, ['a/c2'])
  assert.deepEqual(r.forms.map(summarize).map((s) => [s.id, s.run, s.status, s.file]), [['a/c1', 1, 'skipped-known', 'a'], ['a/c1', 2, 'skipped-known', 'a'], ['a/c3', 1, 'skipped-known', 'a'], ['a/c3', 2, 'skipped-known', 'a']])
  assert.match(r.forms[0], /:previous-failure \{:event \{:kind :x\}\}/)
})
test('knownFailures: a :skipped-known result keeps the failure it carries, so the next sweep still skips it', () => {
  const skipped = skipKnown(['a/c1', 'a/c3'], new Map([['a/c1', '{:event {:kind :x}}'], ['a/c3', '']])).forms
  assert.deepEqual([...knownFailures([vec(...skipped)])], [['a/c1', '{:event {:kind :x}}'], ['a/c3', '']])
  assert.deepEqual([...knownFailures([vec(...skipped), vec(form('a/c1', 'pass'))])], [['a/c3', '']])
})
test('runPool: preSettled results are in the text and the events, count as skipped, and no child runs them', { timeout: 10000 }, async () => {
  const log = []
  const ev = []
  const pre = skipKnown(['a/c1'], new Map([['a/c1', '']])).forms
  const run = fakeRunner(() => ({ code: 0, text: vec(form('a/c2', 'pass')) }), log)
  const r = await runPool({ units: ['a'], workers: workerSpecs(1, 19, 'P', 0), runUnit: run, preSettled: pre, total: 2, emit: (e) => ev.push(e) })
  assert.equal(r.skipped, 1)
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => [s.id, s.status]), [['a/c1', 'skipped-known'], ['a/c2', 'pass']])
  assert.deepEqual(resultsOf(ev).map((e) => [e.name, e.outcome]), [['a/c1#1', 'skipped'], ['a/c2#1', 'passed']])
  assert.deepEqual(ev.filter((e) => e.event === 'progress').map((e) => [e.done, e.total]), [[1, 2], [2, 2]])
})
test('runNightBatches: preSettled results are in the text and the events, and the code stays 1 (not passing)', { timeout: 10000 }, async () => {
  const ev = []
  const pre = skipKnown(['a/1'], new Map([['a/1', '']])).forms
  const r = await runNightBatches({ batches: [{ kind: 'natural', ids: ['a/2'] }], workers: workerSpecs(1, 19, 'P', 0), runUnit: batchRunner([], (id) => ({ code: 0, text: vec(form(id, 'pass')) })), preSettled: pre, total: 2, emit: (e) => ev.push(e) })
  assert.deepEqual(splitForms(r.text).map(summarize).map((s) => [s.id, s.status]), [['a/1', 'skipped-known'], ['a/2', 'pass']])
  assert.deepEqual(resultsOf(ev).map((e) => e.outcome), ['skipped', 'passed'])
  assert.equal(r.code, 1)
})

test('unskip: --unskip-id and --unskip-ids FILE name known failures that run anyway, without limiting the run', () => {
  const p = parsePoolArgs(['engine/fixtures/world', '--unskip-id', 'a/c1', '--unskip-ids', 'u.txt'])
  assert.deepEqual(p.unskipIds, ['a/c1'])
  assert.equal(p.unskipIdsFile, 'u.txt')
  assert.deepEqual(p.matchIds, [])
  assert.deepEqual(p.passthrough, [])
  const known = new Map([['a/c1', ''], ['a/c2', ''], ['a/c3', '']])
  const r = skipKnown(['a/c1', 'a/c2', 'a/c3'], withoutUnskipped(known, ['a/c1', 'a/c3']))
  assert.deepEqual(r.keep, ['a/c1', 'a/c3'])
  assert.deepEqual(r.forms.map(summarize).map((s) => s.id), ['a/c2'])
})

test('childMatchArgs: thousands of kept ids go in a file, so the child argv stays small', () => {
  const ids = Array.from({ length: 3000 }, (_, i) => `some-fixture-file/a-fairly-long-case-name-number-${i}`)
  const file = path.join(os.tmpdir(), `pool-ids-test-${process.pid}.txt`)
  const args = childMatchArgs(ids, file)
  const text = fs.readFileSync(file, 'utf8')
  fs.rmSync(file)
  assert.deepEqual(args, ['--match-ids', file])
  assert.deepEqual(text.split('\n').filter(Boolean), ids)
})
