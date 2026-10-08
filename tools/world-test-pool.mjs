// Why JavaScript: process orchestration for tools/world-test.mjs (spawns one-body runs, merges their EDN result files); the runner itself is cljs.
// `world-test.mjs <dir|file ...> --bodies N` runs the fixture files across up to N probe bodies and writes one merged --results file.
// A unit is one fixture file, run by an ordinary one-body world-test child (own body slot, body claim, plot block). Pool flags:
//   --bodies N        bodies to use (at most the res-slot body max minus one)
//   --max-parallel M  children at once (default min(N, body slot max minus one, bodies the memory above the res-slot floor holds at the body slot's needMb))
// A new child starts only while the 1-minute load average is at most the core count (polled; the first child always starts).
// SIGINT, SIGTERM and exit kill every child this call spawned (by pid) and delete its temp result file.
//   --body PREFIX     body names PREFIX + A, B, ... (default ProbePool); each needs a whitelist entry
//   --first-plot I    first plot of body A; body k starts at I + 20 k
//   --durations FILE...  earlier --results files (all files up to the next flag; repeatable): files run longest first; unseen files count as the median; none = file order
// Every other flag (--phase, --allow-time, --time-log, --tag, ...) goes to every child, so one call is one time phase.
// With TEST_EVENTS=1 the pool prints the @@test lines itself (children print none): a plan with the total of all listed cases up front
// (world-test --list), one result per case with its final status (a rerun that also fails is one failure), and progress lines
// {done, total, retries} (reruns are counted in retries only); a first attempt that fails or dies with a rerun queued adds a progress line with attempted (cases with a first result, done included).
//   --night-batches  (with --phase night-exclusive --allow-time) runs the cases in night batches instead of one file per body: cases that only need a night run together, at most --bodies at once,
//                     packed longest first so a batch fits one night; the case that needs the vanilla night skip (:sleep-skip) runs in its own batch; every case that sets the time itself runs alone.
//                     Each batch has one `time set 14000`, made by its first case, and the next batch waits for all of it. A failed case reruns alone in a batch of its own (--retry-failed); no --repeat above 1.
// A failed case whose last result in the --durations files was a fail on the same first failing check is a known failure: no rerun, :known-failure true in its result (and knownFailure in its event).
//   --retry-failed N the pool owns retries (children never get it): a failed case (not an error or inconclusive one, as in the single-body runner)
//                     is rerun up to N times (default 1; exactly its --match-id, one run) on a different body when the pool has one; a pass is :flaky.
//   --match-id ID    (repeatable) and --match-ids FILE (one id per line, or a --results .edn: its ids that did not pass) run exactly those cases; --list counts only them.
//   --min-bodies N   waits (polls free memory, logging the figure every minute) until the body cap reaches N; past --min-bodies-wait-s (default 1800) it stops with exit 4.
//   A child exiting 6 (its body died at boot twice in a row) retires that body: its cases go to the others; when no body is left the pass stops with exit 6, keeping the results it wrote.
//   With --results FILE each failed, errored or flaky case's trace (body events, body.log tail; capped) is written to <dir of FILE>/cases/ and named :trace in its entry (a flaky entry's :first-failure holds the failing run's).
//   A child exiting 5 (a precheck that fails for every case, e.g. a stranger near the plot grid) stops the pass at once with exit 5. At start the pool stops leftover bodies of its prefix with no live runner lease.
//   --server-wait-s S when a child hits a refused RCON/game-port connection the pool holds every unit and polls the server (up to S seconds, default 300),
//                     reruns the faulted cases without using a retry (at most twice each), waits --server-settle-s (default 10) after the server answers, and past S stops with "server unreachable" (exit 3)
import fs from 'node:fs'
import path from 'node:path'
import os from 'node:os'
import net from 'node:net'
import { spawn, spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const PLOTS_PER_BODY = 20
const LOAD_POLL_MS = 5000
export const BOOT_EXIT = 6 // a child's results end in repeated boot failures (the body dies at start): the pool stops at once, keeping them
export const PRECHECK_EXIT = 5 // a child's refusal that holds for every case (a stranger near the plot grid): the pool stops at once
const here = path.dirname(fileURLToPath(import.meta.url))
const POOL_VALUE_FLAGS = ['--min-bodies', '--min-bodies-wait-s', '--bodies', '--body', '--first-plot', '--results', '--durations', '--max-parallel', '--retry-failed', '--server-wait-s', '--server-settle-s', '--match-id', '--match-ids']
const VALUE_FLAGS = ['--tag', '--match', '--repeat', '--world', '--card', '--time-log', '--phase']
const CLOSERS = { '{': '}', '[': ']', '(': ')' }

// ---- EDN text: just enough to split a results vector into its maps and read their top-level keys
const skipSpace = (s, i) => { while (i < s.length && /[\s,]/.test(s[i])) i++; return i }
const skipString = (s, i) => { i++; while (i < s.length && s[i] !== '"') i += s[i] === '\\' ? 2 : 1; return i + 1 }
// end index of the value starting at i (i at a non-space char)
const skipValue = (s, i) => {
  if (s[i] === '"') return skipString(s, i)
  if (s[i] === '#' && s[i + 1] === '{') i++
  const close = CLOSERS[s[i]]
  if (!close) { while (i < s.length && !/[\s,}\])]/.test(s[i])) i++; return i }
  i++
  for (i = skipSpace(s, i); i < s.length && s[i] !== close; i = skipSpace(s, i)) i = skipValue(s, i)
  return i + 1
}
const entries = (formText) => {
  const out = []
  for (let i = skipSpace(formText, 1); formText[i] !== '}'; i = skipSpace(formText, i)) {
    const kEnd = skipValue(formText, i), vStart = skipSpace(formText, kEnd), vEnd = skipValue(formText, vStart)
    out.push({ key: formText.slice(i, kEnd), start: vStart, end: vEnd })
    i = vEnd
  }
  return out
}
// the complete top-level forms of a results vector; complete is false when the text is empty, not a vector or cut off (the forms before the cut are still returned)
export const scanForms = (text) => {
  const forms = []
  const start = skipSpace(text, 0)
  if (text[start] !== '[') return { forms, complete: false }
  let i = skipSpace(text, start + 1)
  while (i < text.length && text[i] !== ']') {
    const end = skipValue(text, i)
    if (end > text.length) return { forms, complete: false }
    forms.push(text.slice(i, end))
    i = skipSpace(text, end)
  }
  return { forms, complete: i < text.length }
}
export const splitForms = (text) => scanForms(text).forms
const valueOf = (formText, key) => {
  const e = entries(formText).find((x) => x.key === key)
  return e && formText.slice(e.start, e.end)
}
export const summarize = (formText) => ({
  id: JSON.parse(valueOf(formText, ':id') ?? 'null'),
  run: Number(valueOf(formText, ':run') ?? 1),
  file: JSON.parse(valueOf(formText, ':file') ?? 'null'),
  status: (valueOf(formText, ':status') ?? ':error').slice(1),
  elapsed: Number(valueOf(formText, ':elapsed-s') ?? 0),
})
// the form with its :run set to run (a rerun child runs once, so reports run 1)
const withRun = (formText, run) => {
  const e = entries(formText).find((x) => x.key === ':run')
  return e ? `${formText.slice(0, e.start)}${run}${formText.slice(e.end)}` : `${formText.slice(0, -1)} :run ${run}}`
}
const withStatus = (formText, status, extra) => {
  const e = entries(formText).find((x) => x.key === ':status')
  return `${formText.slice(0, e.start)}:${status}${formText.slice(e.end, -1)} ${extra}}`
}
// which check a failed case failed on: the :expect map of its first failed expectation, else its :why ('' when neither)
export const failSig = (formText) => {
  const list = valueOf(formText, ':expects')
  if (list?.startsWith('[')) {
    for (let i = skipSpace(list, 1); i < list.length && list[i] !== ']'; i = skipSpace(list, i)) {
      const end = skipValue(list, i)
      const item = list.slice(i, end)
      if (valueOf(item, ':status') === ':fail') return valueOf(item, ':expect') ?? ''
      i = end
    }
  }
  return valueOf(formText, ':why') ?? ''
}
const withExtra = (formText, extra) => `${formText.slice(0, -1)} ${extra}}`
const OUTCOMES = { pass: 'passed', fail: 'failed', error: 'error', skipped: 'skipped', flaky: 'flaky', inconclusive: 'inconclusive' }
const resultEvent = (formText) => {
  const s = summarize(formText)
  const why = valueOf(formText, ':why') ?? valueOf(formText, ':evidence')
  const early = valueOf(formText, ':ended-early-s')
  return { event: 'result', name: `${s.id}#${s.run}`, outcome: OUTCOMES[s.status] ?? 'failed', ...(valueOf(formText, ':known-failure') === 'true' ? { knownFailure: true } : {}), ...(early ? { endedEarlyS: Number(early) } : {}), ...(s.status !== 'pass' && why?.startsWith('"') ? { message: JSON.parse(why).slice(0, 2000) } : {}) }
}
// the cases a `world-test --list` printed (one line each), times the repeat count
export const countListed = (text, repeat = 1) => text.split('\n').filter((l) => l.trim()).length * repeat
// the cases a `world-test --list` printed, by fixture stem (the id's part before the slash), in order
export const parseListed = (text) => {
  const m = new Map()
  for (const line of text.split('\n')) {
    const id = line.trim().split(/\s+/)[0]
    if (!id) continue
    const stem = id.split('/')[0]
    m.set(stem, [...(m.get(stem) ?? []), id])
  }
  return m
}
// the units that have at least one listed case (all of them without a listing)
export const noCasesSelected = (units, listed) => listed !== null && !units.length
export const unitsWithCases = (units, listed) => (listed ? units.filter((u) => listed.has(u)) : units)
// ---- night batches
const NIGHT_BUDGET_S = 330 // a night is 13000-22000: 8000 ticks from 14000 are 400 s at 20 TPS, less the set-up of each body
const NIGHT_DEFAULT_S = 90 // for a case with no recorded duration
export const parseNightClasses = (text) => text.split('\n').map((l) => l.trim().split(/\s+/)).filter((w) => w[0]).map(([id, cls]) => ({ id, cls }))
export const durationsById = (texts) => {
  const m = new Map()
  for (const text of texts) for (const f of splitForms(text)) { const s = summarize(f); m.set(s.id, s.elapsed) }
  return m
}
const chunks = (xs, n) => Array.from({ length: Math.ceil(xs.length / n) }, (_, i) => xs.slice(i * n, i * n + n))
// the ids of the barrier cases (class `barrier:TICKS`), grouped by the time they set
const barrierGroups = (cases) => [...Map.groupBy(cases.filter((c) => c.cls.startsWith('barrier:')), (c) => c.cls).values()].map((g) => g.map((c) => c.id))
// the batches in running order: natural cases longest first, `bodies` at most at once (one that outlasts the budget alone, first); the skip cases; the cases that end the night with the same time-set (barrier: they meet at the step); every other solo case alone
export const nightBatches = (cases, durations, { bodies, budgetS = NIGHT_BUDGET_S, defaultS = NIGHT_DEFAULT_S }) => {
  const secs = (id) => durations.get(id) ?? defaultS
  const idsOf = (cls) => cases.filter((c) => c.cls === cls).map((c) => c.id)
  const natural = idsOf('natural').map((id, i) => ({ id, i })).sort((a, b) => secs(b.id) - secs(a.id) || a.i - b.i).map((x) => x.id)
  return [
    ...natural.filter((id) => secs(id) > budgetS).map((id) => ({ kind: 'long', ids: [id] })),
    ...chunks(natural.filter((id) => secs(id) <= budgetS), bodies).map((ids) => ({ kind: 'natural', ids })),
    ...chunks(idsOf('skip'), bodies).map((ids) => ({ kind: 'skip', ids })),
    ...barrierGroups(cases).flatMap((ids) => chunks(ids, bodies).map((c) => ({ kind: 'barrier', ids: c, barrier: c.length }))),
    ...idsOf('solo').map((id) => ({ kind: 'solo', ids: [id] })),
  ]
}
const emitLine = (e) => { if (process.env.TEST_EVENTS) console.log(`@@test ${JSON.stringify(e)}`) }
export const mergeText = (forms) => `[${forms.join('\n ')}]`

// ---- planning
export const unitOrder = (files, previousTexts) => {
  const secs = new Map()
  for (const text of previousTexts) {
    for (const f of splitForms(text)) {
      const s = summarize(f)
      if (s.file) secs.set(s.file, (secs.get(s.file) ?? 0) + s.elapsed)
    }
  }
  const known = [...secs.values()].sort((a, b) => a - b)
  if (!known.length) return [...files]
  const median = known[Math.floor(known.length / 2)]
  const cost = (f) => secs.get(f) ?? median
  return files.map((f, i) => ({ f, i })).sort((a, b) => cost(b.f) - cost(a.f) || a.i - b.i).map((x) => x.f)
}

export const parsePoolArgs = (args) => {
  const p = { bodies: 1, prefix: 'ProbePool', firstPlot: 0, results: null, maxParallel: null, durations: [], retries: 1, serverWaitS: 300, serverSettleS: 10, minBodies: 0, minBodiesWaitS: 1800, paths: [], passthrough: [], matchIds: [], matchIdsFile: null, nightBatches: false }
  for (let i = 0; i < args.length; i++) {
    const a = args[i]
    if (!a.startsWith('--')) { p.paths.push(a); continue }
    const v = args[i + 1]
    if (a === '--night-batches') { p.nightBatches = true; continue }
    if (!POOL_VALUE_FLAGS.includes(a)) { p.passthrough.push(a); if (VALUE_FLAGS.includes(a)) p.passthrough.push(args[++i]); continue }
    i++
    if (a === '--durations') { p.durations.push(v); while (args[i + 1] !== undefined && !args[i + 1].startsWith('--')) p.durations.push(args[++i]); continue }
    if (a === '--bodies') p.bodies = Number(v)
    else if (a === '--body') p.prefix = v
    else if (a === '--first-plot') p.firstPlot = Number(v)
    else if (a === '--results') p.results = v
    else if (a === '--match-id') p.matchIds.push(v)
    else if (a === '--match-ids') p.matchIdsFile = v
    else if (a === '--max-parallel') p.maxParallel = Number(v)
    else if (a === '--retry-failed') p.retries = Number(v)
    else if (a === '--server-wait-s') p.serverWaitS = Number(v)
    else if (a === '--min-bodies') p.minBodies = Number(v)
    else if (a === '--min-bodies-wait-s') p.minBodiesWaitS = Number(v)
    else if (a === '--server-settle-s') p.serverSettleS = Number(v)
  }
  return p
}

// ids from a --match-ids file: a results vector gives its ids that did not pass (once each), else one id per line (blank lines and # comments skipped)
export const matchIdsFrom = (text) => {
  if (text.trimStart().startsWith('[')) return [...new Set(splitForms(text).map(summarize).filter((s) => !['pass', 'flaky'].includes(s.status)).map((s) => s.id))]
  return text.split('\n').map((l) => l.trim()).filter((l) => l && !l.startsWith('#'))
}
const withMatchIds = (ids) => ids.flatMap((id) => ['--match-id', id])

const bodyLetter = (k) => (k < 26 ? String.fromCharCode(65 + k) : String(k))
export const workerSpecs = (wanted, cap, prefix, firstPlot) =>
  Array.from({ length: Math.max(1, Math.min(wanted, cap)) }, (_, k) => ({ body: prefix + bodyLetter(k), firstPlot: firstPlot + PLOTS_PER_BODY * k }))

// bodies at once: --max-parallel, else the bodies asked for, capped by the body slot max and by what the memory above the res-slot floor holds
export const poolCap = ({ bodies, maxParallel, availableMb, floorMb, needMb, bodyMax }) =>
  maxParallel ?? Math.max(1, Math.min(bodies, bodyMax, Math.floor((availableMb - floorMb) / needMb)))

// --min-bodies: polls cap() (the bodies the free memory holds) until it reaches min, logging the figure every logMs; gives up after waitMs.
// -> {ok, cap, message?}
export const waitForBodies = async ({ min, cap, sleep, now = Date.now, pollMs = 10000, logMs = 60000, waitMs = 1800000, log = console.error }) => {
  const start = now()
  let lastLog = -Infinity
  for (;;) {
    const c = cap()
    if (c >= min) return { ok: true, cap: c }
    if (now() - start >= waitMs) return { ok: false, cap: c, message: `world-test pool: only ${c} bodies fit the free memory, --min-bodies wants ${min}; gave up after ${Math.round(waitMs / 1000)} s` }
    if (now() - lastLog >= logMs) { log(`world-test pool: ${c} bodies fit the free memory, waiting for ${min} (--min-bodies)`); lastLog = now() }
    await sleep(pollMs)
  }
}

// Children alive and their temp files; reap() is idempotent and synchronous so it can run in an 'exit' handler.
export const createReaper = (rm) => {
  const live = new Set()
  return {
    add: (child, tmp) => { const e = { child, tmp }; live.add(e); return e },
    done: (e) => { live.delete(e) },
    reap: () => {
      for (const { child, tmp } of live) { child.kill('SIGTERM'); rm(tmp) }
      live.clear()
    },
  }
}

// ---- scheduling
const failed = (s) => s.status === 'fail'
// a night-batch case the runner marked :inconclusive because the night ended during it: worth a rerun alone, like a failure
const overran = (formText) => summarize(formText).status === 'inconclusive' && valueOf(formText, ':overran') === 'true'
// id -> failSig for the ids whose last recorded result (the texts in order, later ones win) is a fail: a rerun that fails on the same check tells nothing new
export const knownFailures = (previousTexts) => {
  const last = new Map()
  for (const text of previousTexts) for (const f of splitForms(text)) { const s = summarize(f); last.set(s.id, s.status === 'fail' ? failSig(f) : null) }
  return new Map([...last].filter(([, sig]) => sig !== null))
}
const errorForm = (file, why, id = file) => `{:id ${JSON.stringify(id)}, :file ${JSON.stringify(file)}, :status :error, :evidence ${JSON.stringify(why)}}`

// units: file stems; runUnit({file, match, worker}) -> promise of {code, text|null} (text = the child's results vector).
// Resolves to {text, code}: one merged results vector, code 0 when every case passed or was flaky.
// total/emit (both optional): emit gets the @@test events of the run: one result per case when its status is final (a failure still
// to be rerun is not final), and a progress {done, total, retries} after each; reruns are counted in retries, never in done or total.
// A child's connection error (RCON or the game port) is a setup fault, not a case result: probe() says whether the server answers; while it does not,
// no unit starts (polled every serverPollMs, then settleMs once it answers); the faulted cases rerun for free (at most FAULT_RERUNS times each); past serverWaitMs the pool stops: {unreachable: true, code: 3}.
const CONN_FAULT = /ECONN(REFUSED|RESET)|EPIPE/
const FAULT_RERUNS = 2 // a case that still faults after this many free reruns settles as an ordinary error
const BOOT_FAULT = /did not start within/
const bootFault = (form) => summarize(form).status === 'error' && [':error', ':why', ':evidence'].some((k) => BOOT_FAULT.test(valueOf(form, k) ?? ''))
const connFault = (form) => summarize(form).status === 'error' && [':error', ':why', ':evidence'].some((k) => CONN_FAULT.test(valueOf(form, k) ?? ''))
export const runPool = async ({ units, workers, runUnit, retries = 1, load = () => 0, cores = 1, total = null, known = new Map(), listed = null, repeat = 1, emit = () => {}, sleep = (ms) => new Promise((r) => setTimeout(r, ms)),
  probe = async () => true, now = Date.now, serverWaitMs = 300000, serverPollMs = 3000, settleMs = 10000 }) => {
  const queue = units.map((file) => ({ file, match: null, avoid: null }))
  const done = new Map() // `id#run` (or file when the unit died) -> form text, in first-seen order
  let busy = 0
  let finals = 0
  let reruns = 0
  let skipped = 0
  let wake = []
  const notify = () => { const w = wake; wake = []; w.forEach((r) => r()) }
  const take = (worker) => {
    const i = queue.findIndex((j) => j.avoid !== worker.body || workers.length === 1)
    return i < 0 ? null : queue.splice(i, 1)[0]
  }
  const key = (s) => `${s.id}#${s.run}`
  const settled = new Set()
  const redoing = new Set() // keys of cases rerun after a server fault: not missing
  let holding = null
  let unreachable = false
  let precheck = false
  let bootDead = false
  let alive = workers.length // bodies not retired for dying at boot
  let attempted = 0
  const hold = () => {
    holding ??= (async () => {
      const t0 = now()
      while (!(await probe())) {
        if (now() - t0 >= serverWaitMs) { unreachable = true; break }
        await sleep(serverPollMs)
      }
      if (!unreachable) await sleep(settleMs)
      holding = null
    })()
  }
  const settle = (k, form) => { // a case's final status
    settled.add(k)
    done.set(k, form)
    emit(resultEvent(form))
    emit({ event: 'progress', done: ++finals, total: total ?? finals, unit: 'cases', retries: reruns })
  }
  // one error per listed case and run of file that is not settled yet (a unit that died); nothing without a listing
  const settleMissing = (file, why) => {
    for (const id of listed?.get(file) ?? []) {
      for (let run = 1; run <= repeat; run++) {
        if (!settled.has(`${id}#${run}`) && !done.has(`${id}#${run}`) && !redoing.has(`${id}#${run}`)) settle(`${id}#${run}`, withRun(errorForm(file, why, id), run))
      }
    }
  }
  const rerun = (j) => { reruns++; queue.push(j) }
  const record = (job, worker, res) => {
    if (job.redo) { // one case rerun after a server fault: a fresh run of it
      const k = `${job.match}#${job.run}`
      redoing.delete(k)
      const mine = res.text ? splitForms(res.text).map((f) => withRun(f, job.run)).filter((f) => summarize(f).id === job.match) : []
      if (!mine.length) settle(k, withRun(errorForm(job.file, `the run exited ${res.code} with no result for this case`, job.match), job.run))
      mine.forEach((form) => {
        if (failed(summarize(form)) && retries > 0 && known.get(job.match) === failSig(form)) { skipped++; settle(k, withExtra(form, ':known-failure true')) }
        else if (failed(summarize(form)) && retries > 0) { done.set(k, form); rerun({ file: job.file, avoid: worker.body, retry: { body: worker.body }, match: job.match, run: job.run, attempt: 1, firstForm: form }) }
        else settle(k, form)
      })
      return
    }
    const retryJob = { file: job.file, avoid: worker.body, retry: { body: worker.body } }
    if (!res.text && job.retry) {
      const why = `the run exited ${res.code} with no results (twice)`
      if (listed?.has(job.file)) { done.delete(job.file); settleMissing(job.file, why) } else settle(job.file, errorForm(job.file, why))
      return
    }
    if (!res.text) {
      rerun({ ...retryJob, match: null })
      done.set(job.file, errorForm(job.file, `the run exited ${res.code} with no results`))
      return
    }
    const forms = splitForms(res.text).map((form) => ({ form, s: summarize(form) }))
    if (job.retry && job.match === null) { // a whole-file rerun replaces the placeholder error
      done.delete(job.file)
      forms.forEach(({ form, s }) => settle(key(s), form))
      settleMissing(job.file, `the run exited ${res.code} before this case ran`)
      return
    }
    if (job.retry) { // a case rerun (one run of one case): a pass is flaky, a failure is rerun until the retries are spent
      const k = `${job.match}#${job.run}`
      for (const { form, s } of forms.map((x) => ({ ...x, form: withRun(x.form, job.run) })).filter((x) => x.s.id === job.match)) {
        if (s.status === 'pass') settle(k, withStatus(form, 'flaky', `:first-failure ${job.firstForm} :first-failure-body ${JSON.stringify(job.retry.body)}`))
        else if (failed(s) && job.attempt < retries) rerun({ ...retryJob, match: s.id, run: job.run, attempt: job.attempt + 1, firstForm: job.firstForm })
        else settle(k, form)
      }
      return
    }
    for (const { form, s } of forms) {
      if (failed(s) && retries > 0 && known.get(s.id) === failSig(form)) { skipped++; settle(key(s), withExtra(form, ':known-failure true')) }
      else if (failed(s) && retries > 0) { done.set(key(s), form); rerun({ ...retryJob, match: s.id, run: s.run, attempt: 1, firstForm: form }) }
      else settle(key(s), form)
    }
    settleMissing(job.file, `the run exited ${res.code} before this case ran`)
  }
  const loop = async (worker) => {
    for (;;) {
      if (holding) await holding
      if (unreachable || precheck || bootDead) return
      const job = take(worker)
      if (job) {
        while (busy && load() > cores) await sleep(LOAD_POLL_MS)
        busy++
        const res = await runUnit({ file: job.file, match: job.match, exact: job.match !== null, worker, attempt: job.attempt ?? 0 })
        if (res.code === PRECHECK_EXIT && !res.text) { precheck = true; busy--; notify(); return }
        const forms = res.text ? splitForms(res.text) : []
        const faults = (job.faults ?? 0) + 1
        const retire = res.code === BOOT_EXIT && alive > 1 // this body is retired, its cases go to the others; the last body ends the pass
        if (retire) alive--
        const bad = retire ? forms.filter(bootFault) : faults <= FAULT_RERUNS ? forms.filter(connFault) : []
        if (retire || bad.length || (!res.text && !(await probe()))) {
          if (!retire) hold()
          const extra = retire ? { avoid: worker.body } : { faults }
          if (job.match !== null || job.retry || bad.length === forms.length) queue.push({ ...job, ...extra }) // the same job again, no retry used
          else {
            const redo = (id, run) => {
              redoing.add(`${id}#${run}`)
              queue.push({ file: job.file, match: id, run, redo: true, avoid: null, ...extra })
            }
            for (const f of bad) redo(summarize(f).id, summarize(f).run)
            const seen = new Set(forms.map((f) => key(summarize(f))))
            for (const id of listed?.get(job.file) ?? []) { // cases the child never reached
              for (let run = 1; run <= repeat; run++) if (!seen.has(`${id}#${run}`) && !settled.has(`${id}#${run}`) && !done.has(`${id}#${run}`) && !redoing.has(`${id}#${run}`)) redo(id, run)
            }
            record(job, worker, { ...res, text: mergeText(forms.filter((f) => !bad.includes(f))) })
          }
        } else record(job, worker, res)
        if (res.code === BOOT_EXIT && !retire) bootDead = true
        if (done.size > finals && done.size !== attempted) { // first attempts waiting for their rerun
          attempted = done.size
          emit({ event: 'progress', done: finals, total: total ?? done.size, unit: 'cases', retries: reruns, attempted })
        }
        busy--
        notify()
        if (retire) return
        continue
      }
      if (!busy && !queue.length) return
      await new Promise((r) => wake.push(r))
    }
  }
  await Promise.all(workers.map(loop))
  if (precheck) return { skipped, precheck, text: mergeText([...done].filter(([k]) => settled.has(k)).map(([, f]) => f)), code: PRECHECK_EXIT }
  const forms = [...done.values()]
  if (bootDead) return { skipped, bootDead, text: mergeText(forms), code: BOOT_EXIT }
  if (unreachable) { // a failure still waiting for its retry is a result: inconclusive, with its first failure
    const pending = [...done].filter(([k]) => !settled.has(k)).map(([, f]) => withStatus(f, 'inconclusive', `:first-failure ${f}`))
    pending.forEach((f) => emit(resultEvent(f)))
    return { skipped, unreachable, text: mergeText([...[...done].filter(([k]) => settled.has(k)).map(([, f]) => f), ...pending]), pending, code: 3 }
  }
  return { skipped, text: mergeText(forms), code: !forms.length ? 2 : forms.every((f) => ['pass', 'flaky'].includes(summarize(f).status)) ? 0 : 1 }
}

// Runs the batches one after the other (a batch's cases together, one per worker, each with the batch id in its unit); a failed case reruns alone
// (own batch, up to retries times; a pass is :flaky); a known failure is not rerun. runUnit({id, batch, worker}) -> {code, text}. -> {text, code} like runPool.
export const runNightBatches = async ({ batches, workers, runUnit, retries = 1, known = new Map(), total = null, emit = () => {}, onBatchEnd = () => {} }) => {
  const done = new Map()
  let finals = 0
  let reruns = 0
  let seq = 0
  let precheck = false
  let bootDead = false
  const dead = new Set() // bodies retired for dying at boot
  const live = () => workers.filter((w) => !dead.has(w.body))
  const settle = (id, form) => {
    done.set(id, form)
    emit(resultEvent(form))
    emit({ event: 'progress', done: ++finals, total: total ?? finals, unit: 'cases', retries: reruns })
  }
  // [id, form, bootFail] per id; the ids run together on the live bodies (in waves when there are fewer bodies than ids); a case whose body died at boot is bootFail and retires that body
  const runBatch = async (ids, barrier, attempt = 0) => {
    const batch = `${process.pid}-${seq++}`
    const out = []
    for (let at = 0; at < ids.length; at += Math.max(1, live().length)) {
      const bodies = live()
      out.push(...await Promise.all(ids.slice(at, at + bodies.length).map(async (id, k) => {
        const res = await runUnit({ id, batch, barrier, worker: bodies[k], attempt })
        if (res.code === PRECHECK_EXIT && !res.text) precheck = true
        const mine = (res.text ? splitForms(res.text) : []).find((f) => summarize(f).id === id)
        const bootFail = res.code === BOOT_EXIT && (!mine || bootFault(mine))
        if (res.code === BOOT_EXIT) dead.add(bodies[k].body)
        return [id, mine ?? errorForm(id.split('/')[0], `the run exited ${res.code} with no result for this case`, id), bootFail]
      })))
    }
    await onBatchEnd(batch)
    return out
  }
  // the cases whose body died at boot run again on the others (each such death retires a body, so this ends)
  const rerunBootFails = async (out) => {
    let cur = out
    while (!precheck && cur.some((x) => x[2])) {
      if (!live().length) { bootDead = true; return cur }
      const again = new Map((await runBatch(cur.filter((x) => x[2]).map((x) => x[0]))).map((x) => [x[0], x]))
      cur = cur.map((x) => again.get(x[0]) ?? x)
    }
    return cur
  }
  const failing = []
  for (const batch of batches) {
    if (batch.barrier && live().length < batch.ids.length) { bootDead = true; break } // the cases of a barrier meet: they need a body each
    const out = await rerunBootFails(await runBatch(batch.ids, batch.barrier))
    if (precheck) return { text: mergeText([...done.values()]), precheck, code: PRECHECK_EXIT }
    if (bootDead) break
    for (const [id, form] of out) {
      if (!(failed(summarize(form)) || overran(form)) || retries <= 0) settle(id, form)
      else if (failed(summarize(form)) && known.get(id) === failSig(form)) settle(id, withExtra(form, ':known-failure true'))
      else failing.push([id, form])
    }
  }
  if (bootDead) return { text: mergeText([...done.values()]), bootDead, code: BOOT_EXIT }
  for (const [id, first] of failing) {
    let last = first
    let flaky = false
    for (let attempt = 0; attempt < retries && !flaky; attempt++) {
      reruns++
      const [, form] = (await rerunBootFails(await runBatch([id], undefined, attempt + 1)))[0]
      if (bootDead) return { text: mergeText([...done.values()]), bootDead, code: BOOT_EXIT }
      last = form
      flaky = summarize(form).status === 'pass'
      if (!flaky && !failed(summarize(form)) && !overran(form)) break
    }
    settle(id, flaky ? withStatus(last, 'flaky', `:first-failure ${first}`) : last)
  }
  const forms = batches.flatMap((b) => b.ids).map((id) => done.get(id))
  return { text: mergeText(forms), code: !forms.length ? 2 : forms.every((f) => ['pass', 'flaky'].includes(summarize(f).status)) ? 0 : 1 }
}

// ---- the real thing
const sleep = (ms) => new Promise((r) => setTimeout(r, ms))
const fixtureFiles = (paths) => paths.flatMap((p) => (fs.statSync(p).isDirectory()
  ? fs.readdirSync(p).filter((f) => f.endsWith('.edn')).sort().map((f) => path.join(p, f))
  : [p]))
const slotConfig = () => JSON.parse(fs.readFileSync(path.join(here, 'res-slot.json'), 'utf8'))
const bodyCap = () => Math.max(1, (slotConfig().kinds.body.max ?? 1) - 1)
const availableMb = () => Number(fs.readFileSync('/proc/meminfo', 'utf8').match(/MemAvailable:\s+(\d+)/)[1]) / 1024

// One child per unit: the ordinary one-body entry point (it takes the body slot and the body claim itself). Exit 75 = busy: ask again.
const spawnChild = (script, pass, matchIds, { file, match, worker, batch, barrier, attempt = 0 }, tmpResults, reaper, traceDir) => new Promise((resolve) => {
  const argv = [script, file, ...pass, ...(traceDir ? ['--trace-dir', traceDir, '--trace-attempt', String(attempt)] : []), ...(batch ? ['--night-batch', batch] : []), ...(batch && barrier ? ['--night-barrier', String(barrier)] : []), '--body', worker.body, '--first-plot', String(worker.firstPlot), '--results', tmpResults, ...(match ? ['--match-id', match, '--repeat', '1'] : withMatchIds(matchIds))]
  // the pool reports the @@test events itself (fixed total, final status per case), so a child prints none
  const { TEST_EVENTS: _events, ...env } = process.env
  const child = spawn(process.execPath, argv, { stdio: 'inherit', env: { ...env, WORLD_TEST_POOL_PID: String(process.pid) } })
  const entry = reaper.add(child, tmpResults)
  child.on('close', (code) => { reaper.done(entry); resolve(code ?? 1) })
})

// every case of the fixture files as the children will select them (--list needs no world); null when the listing fails
// the --list call selects cases with the same paths and flags (--phase, --tag, --match, ...) the children get
export const listArgs = (p) => [...p.paths, ...p.passthrough, ...withMatchIds(p.matchIds), '--list']
export const nightListArgs = (p) => [...listArgs(p), '--night-classes']
// failed cases' traces go next to the --results file (none without one)
export const traceDirOf = (p) => (p.results ? path.join(path.resolve(path.dirname(p.results)), 'cases') : null)
const repeatOf = (p) => { const at = p.passthrough.indexOf('--repeat'); return at < 0 ? 1 : Number(p.passthrough[at + 1]) }
const listCases = (script, p) => {
  const r = spawnSync(process.execPath, [script, ...listArgs(p)], { encoding: 'utf8', maxBuffer: 16 * 1024 * 1024 })
  return r.status === 0 ? r.stdout : null
}

// the texts of the existing --durations files that hold a complete results vector; an empty or cut-off one is warned about and skipped
export const durationFiles = (files) => files.filter((f) => f.endsWith('.edn') && fs.existsSync(f) && fs.statSync(f).isFile())
const readDurations = (files) => durationFiles(files).flatMap((f) => {
  const text = fs.readFileSync(f, 'utf8')
  if (scanForms(text).complete) return [text]
  console.error(`world-test pool: --durations file ${f} is empty or truncated, skipped`)
  return []
})

// the server answers when its RCON port accepts a connection (port from server.properties next to the server, else 25575)
const serverProps = () => {
  const props = process.env.MC_SERVER_PROPERTIES
  return props && fs.existsSync(props) ? fs.readFileSync(props, 'utf8') : ''
}
const rconPort = () => Number(serverProps().match(/^rcon\.port=(\d+)/m)?.[1] ?? 25575)
const gamePort = () => Number(serverProps().match(/^server-port=(\d+)/m)?.[1] ?? 25565)
export const serverProbe = (ports) => () => Promise.all(ports.map((port) => new Promise((resolve) => {
  const sock = net.connect({ port, host: '127.0.0.1' })
  sock.setTimeout(3000)
  sock.on('connect', () => { sock.destroy(); resolve(true) })
  sock.on('timeout', () => { sock.destroy(); resolve(false) })
  sock.on('error', () => resolve(false))
}))).then((answers) => answers.every(Boolean))

// the argv that ends a night batch: the runner restores the world settings the batch changed (logged like the sets) and removes its marker
export const nightBatchEndArgs = (pass, body, batch) => {
  const keep = []
  for (let i = 0; i < pass.length; i++) {
    if (pass[i] === '--allow-time') keep.push(pass[i])
    else if (['--time-log', '--card', '--world'].includes(pass[i])) keep.push(pass[i], pass[++i])
  }
  return ['--night-batch-end', batch, '--body', body, ...keep]
}
// restores the recorded sleep gamerule (a run killed in a batch leaves it in state/); dead = only when the run that recorded it is gone
export const sleepRuleRestoreArgs = (pass, body, dead) => [dead ? '--sleep-rule-restore-dead' : '--sleep-rule-restore', ...nightBatchEndArgs(pass, body, 'x').slice(2)]
const restoreSleepRule = (script, pass, body, dead) => spawnSync(process.execPath, [script, ...sleepRuleRestoreArgs(pass, body, dead)], { stdio: 'inherit', env: { ...process.env, WORLD_TEST_POOL_PID: String(process.pid) } })
// stops the leftover bodies of this pool's prefix that no live runner leases (the runner reads its own lease files)
const stopStaleBodies = (script, prefix) => spawnSync(process.execPath, [script, '--stop-stale-bodies', prefix], { stdio: 'inherit' })
const endNightBatch = (script, pass, body, batch) => new Promise((resolve) => {
  const child = spawn(process.execPath, [script, ...nightBatchEndArgs(pass, body, batch)], { stdio: 'inherit', env: { ...process.env, WORLD_TEST_POOL_PID: String(process.pid) } })
  child.on('close', resolve)
})

// --min-bodies at the start of a pool: false (message printed) when the memory never holds that many bodies
const waitMin = async (p, capNow) => {
  if (!p.minBodies) return true
  const reachable = Math.min(p.bodies, bodyCap(), p.maxParallel ?? Infinity)
  if (reachable < p.minBodies) { console.error(`world-test pool: --min-bodies ${p.minBodies} can never be met (--bodies, --max-parallel and the body slot max allow ${reachable})`); return false }
  const r = await waitForBodies({ min: p.minBodies, cap: capNow, sleep, waitMs: p.minBodiesWaitS * 1000 })
  if (!r.ok) console.error(r.message)
  return r.ok
}

const nightMain = async (p, byStem, previous, script) => {
  const pass = p.passthrough
  const phaseAt = pass.indexOf('--phase')
  if (phaseAt < 0 || pass[phaseAt + 1] !== 'night-exclusive' || !pass.includes('--allow-time')) { console.error('world-test pool: --night-batches needs --phase night-exclusive and --allow-time'); return 2 }
  if (repeatOf(p) > 1) { console.error('world-test pool: --night-batches runs each case once (no --repeat)'); return 2 }
  const r = spawnSync(process.execPath, [script, ...nightListArgs(p)], { encoding: 'utf8', maxBuffer: 16 * 1024 * 1024 })
  const cases = r.status === 0 ? parseNightClasses(r.stdout) : []
  if (!cases.length) { console.error('world-test pool: no cases selected'); emitLine({ event: 'plan', total: 0 }); return 2 }
  const cores = os.cpus().length
  const capNow = () => poolCap({ bodies: p.bodies, maxParallel: p.maxParallel, availableMb: availableMb(), floorMb: slotConfig().floorMb, needMb: slotConfig().kinds.body.needMb, bodyMax: bodyCap() })
  if (!(await waitMin(p, capNow))) return 4
  const cap = capNow()
  const workers = workerSpecs(cap, bodyCap(), p.prefix, p.firstPlot)
  stopStaleBodies(script, p.prefix)
  const batches = nightBatches(cases, durationsById(previous), { bodies: workers.length })
  console.error(`world-test pool: ${cases.length} night cases in ${batches.length} batches on up to ${workers.length} bodies (${batches.map((b) => `${b.kind}:${b.ids.length}`).join(' ')}; ${cores} cores)`)
  const reaper = createReaper((f) => fs.rmSync(f, { force: true }))
  restoreSleepRule(script, pass, workers[0].body, true)
  process.on('exit', () => { reaper.reap(); restoreSleepRule(script, pass, workers[0].body, false) })
  for (const [sig, code] of [['SIGINT', 130], ['SIGTERM', 143]]) process.on(sig, () => { process.exit(code) })
  const runUnit = async (unit) => {
    const tmp = `${p.results ?? path.join(process.env.TMPDIR ?? '/tmp', 'world-test-pool')}.${process.pid}.${unit.worker.body}.edn`
    let code
    for (let tries = 0; tries < 20; tries++) {
      fs.rmSync(tmp, { force: true })
      code = await spawnChild(script, pass, [], { file: byStem.get(unit.id.split('/')[0]), match: unit.id, worker: unit.worker, batch: unit.batch, barrier: unit.barrier, attempt: unit.attempt }, tmp, reaper, traceDirOf(p))
      if (code !== 75) break
      await sleep(5000)
    }
    const text = fs.existsSync(tmp) ? fs.readFileSync(tmp, 'utf8') : null
    fs.rmSync(tmp, { force: true })
    return { code, text }
  }
  emitLine({ event: 'plan', total: cases.length })
  const res = await runNightBatches({ batches, workers, runUnit, retries: p.retries, total: cases.length, known: knownFailures(previous), emit: emitLine, onBatchEnd: (batch) => endNightBatch(script, pass, workers[0].body, batch) })
  if (res.precheck) console.error('world-test pool: a precheck refused every case (see the line above); pass stopped, no case recorded')
  if (res.bootDead) console.error('world-test pool: every body died at boot (see the child log, e.g. a heap OOM); pass stopped, the cases not run are not recorded')
  if (p.results) fs.writeFileSync(p.results, res.text)
  const sums = splitForms(res.text).map(summarize)
  const count = (st) => sums.filter((x) => x.status === st).length
  console.error(`world-test pool: ${sums.length} cases, ${count('pass')} pass, ${count('flaky')} flaky, ${sums.length - count('pass') - count('flaky')} not passing`)
  return res.code
}

export const main = async (args) => {
  const p = parsePoolArgs(args)
  if (!p.paths.length) { console.error('world-test pool: no fixture file or directory given (e.g. engine/fixtures/world)'); return 2 }
  if (p.matchIdsFile) p.matchIds.push(...matchIdsFrom(fs.readFileSync(p.matchIdsFile, 'utf8')))
  if (p.matchIdsFile && !p.matchIds.length) { console.error(`world-test pool: ${p.matchIdsFile} holds no ids`); return 2 }
  const byStem = new Map(fixtureFiles(p.paths).map((f) => [path.basename(f, '.edn'), f]))
  const previous = readDurations(p.durations)
  const script = path.join(here, 'world-test.mjs')
  if (p.nightBatches) return nightMain(p, byStem, previous, script)
  const listing = listCases(script, p)
  const listed = listing === null ? null : parseListed(listing)
  const units = unitsWithCases(unitOrder([...byStem.keys()], previous), listed)
  if (noCasesSelected(units, listed)) {
    console.error('world-test pool: no cases selected (none match the paths and flags, or all are unchanged under --changed-since-pass)')
    emitLine({ event: 'plan', total: 0 })
    return 2
  }
  const cores = os.cpus().length
  const capNow = () => poolCap({ bodies: p.bodies, maxParallel: p.maxParallel, availableMb: availableMb(), floorMb: slotConfig().floorMb, needMb: slotConfig().kinds.body.needMb, bodyMax: bodyCap() })
  if (!(await waitMin(p, capNow))) return 4
  const workers = workerSpecs(capNow(), bodyCap(), p.prefix, p.firstPlot)
  stopStaleBodies(script, p.prefix)
  const reaper = createReaper((f) => fs.rmSync(f, { force: true }))
  process.on('exit', reaper.reap)
  for (const [sig, code] of [['SIGINT', 130], ['SIGTERM', 143]]) process.on(sig, () => { reaper.reap(); process.exit(code) })
  console.error(`world-test pool: ${units.length} fixture files on ${workers.length} bodies (${workers.map((w) => w.body).join(' ')})`)
  const runUnit = async (unit) => {
    const tmp = `${p.results ?? path.join(process.env.TMPDIR ?? '/tmp', 'world-test-pool')}.${process.pid}.${unit.worker.body}.edn`
    let code
    for (let tries = 0; tries < 20; tries++) {
      fs.rmSync(tmp, { force: true })
      code = await spawnChild(script, p.passthrough, p.matchIds, { ...unit, file: byStem.get(unit.file) }, tmp, reaper, traceDirOf(p))
      if (code !== 75) break
      await sleep(5000)
    }
    const text = fs.existsSync(tmp) ? fs.readFileSync(tmp, 'utf8') : null
    fs.rmSync(tmp, { force: true })
    return { code, text }
  }
  const total = listing === null ? null : countListed(listing, repeatOf(p))
  if (total !== null) emitLine({ event: 'plan', total })
  const r = await runPool({ listed, repeat: repeatOf(p), units, workers, runUnit, retries: p.retries, total, known: knownFailures(previous), emit: emitLine, load: () => os.loadavg()[0], cores, probe: serverProbe([rconPort(), gamePort()]), serverWaitMs: p.serverWaitS * 1000, settleMs: p.serverSettleS * 1000 })
  if (r.precheck) console.error('world-test pool: a precheck refused every case (see the line above); pass stopped, no case recorded')
  if (r.bootDead) console.error('world-test pool: every body died at boot (see the child log, e.g. a heap OOM); pass stopped, the cases not run are not recorded')
  if (r.unreachable) console.error(`world-test pool: server unreachable (RCON port ${rconPort()} or game port ${gamePort()} closed for ${p.serverWaitS} s); stopped, the cases not run are not recorded${r.pending?.length ? `; ${r.pending.length} failures awaiting a retry are recorded as inconclusive` : ''}`)
  if (p.results) fs.writeFileSync(p.results, r.text)
  const sums = splitForms(r.text).map(summarize)
  const count = (st) => sums.filter((s) => s.status === st).length
  console.error(`world-test pool: ${sums.length} cases, ${count('pass')} pass, ${count('flaky')} flaky, ${sums.length - count('pass') - count('flaky')} not passing${r.skipped ? `; ${r.skipped} known failures not rerun (same failed check in the --durations results)` : ''}`)
  return r.code
}
