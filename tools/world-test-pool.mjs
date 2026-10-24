// Why JavaScript: process orchestration for tools/world-test.mjs (spawns one-body runs, merges their EDN result files); the runner itself is cljs.
// `world-test.mjs <dir|file ...> --bodies N` runs the fixture files across up to N probe bodies and writes one merged --results file.
// A unit is one fixture file, run by an ordinary one-body world-test child (own body slot, body claim, plot block). Pool flags:
//   --bodies N        bodies to use (at most the res-slot body max minus one)
//   --max-parallel M  children at once (default min(N, body slot max minus one, bodies the memory above the res-slot floor holds at the body slot's needMb))
// A new child starts only while the server's average tick time (RCON `tick query`) is at most MSPT_LIMIT ms (polled; the first child always starts);
// when the server does not answer, the 1-minute load average scaled to the same limit is used. A pool of SWEEP_MIN_BODIES or more bodies also marks a sweep, which caps other agents' tests and server slots (tools/res-slot).
// SIGINT, SIGTERM and exit kill every child this call spawned (by pid) and delete its temp result file.
//   --body PREFIX     body names PREFIX + A, B, ... (default ProbePool); each needs a whitelist entry
//   --first-plot I    first plot of body A; body k starts at I + 20 k
//   --durations FILE...  earlier --results files (all files up to the next flag; repeatable): files run longest first; unseen files count as the median; none = file order
// Every other flag (--phase, --allow-time, --time-log, --tag, ...) goes to every child, so one call is one time phase.
// With TEST_EVENTS=1 the pool prints the @@test lines itself (children print none): a plan with the total of all listed cases up front
// (world-test --list), one result per case with its final status (a rerun that also fails is one failure), and progress lines
// {done, total, retries} (reruns are counted in retries only); a first attempt that fails or dies with a rerun queued adds a progress line with attempted (cases with a first result, done included).
//   --night-batches  (with --phase night-exclusive --allow-time) runs the cases in night batches instead of one file per body: cases that only need a night share one rolling night
//                     (longest first, at most --bodies at once; a freed body starts the next case while its recorded duration still fits the night, the rest take the next night);
//                     the cases that need the vanilla night skip (:sleep-skip) run in their own batch; cases that start alike and set the same time once meet at that step (barrier); the rest alone.
//                     Each batch has one start time set, made by its first case, and the next batch waits for all of it. Failed cases rerun after all batches, batched the same way (--retry-failed); no --repeat above 1.
// A case whose last result in the --durations files was a fail is a known failure and is not run (--run-known runs it; so does naming it with --match-id/--match-ids, or --unskip-id / --unskip-ids FILE, which do not limit the run): its result is :status :skipped-known
// with :previous-failure (the check it failed), carried on by the next sweep. A known failure that does run and fails on the same first failing check again is not rerun, :known-failure true in its result (and knownFailure in its event).
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
import { spawn, spawnSync, execFile } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { slotDir, pidFileBody } from './res-slot.mjs'

const PLOTS_PER_BODY = 20
const LOAD_POLL_MS = 5000
const LOAD_PER_CORE = 1.5 // fallback gate: a gate at 1x cores idled bodies that mostly wait on the server; the memory floor and res-slot still bound the pool
const MSPT_LIMIT = 45 // the server's 50 ms tick budget with some headroom; case timing is correct below it
const MSPT_CACHE_MS = 3000
export const SWEEP_MIN_BODIES = 8
export const BOOT_EXIT = 6 // a child's results end in repeated boot failures (the body dies at start): the pool stops at once, keeping them
export const PRECHECK_EXIT = 5 // a child's refusal that holds for every case (a stranger near the plot grid): the pool stops at once
const here = path.dirname(fileURLToPath(import.meta.url))
const POOL_VALUE_FLAGS = ['--min-bodies', '--min-bodies-wait-s', '--bodies', '--body', '--first-plot', '--results', '--durations', '--max-parallel', '--retry-failed', '--server-wait-s', '--server-settle-s', '--match-id', '--match-ids', '--unskip-id', '--unskip-ids']
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
// which check a failed case failed on: the :expect map of its first failed expectation (not one left unjudged), else its :why ('' when neither)
export const failSig = (formText) => {
  const list = valueOf(formText, ':expects')
  if (list?.startsWith('[')) {
    for (let i = skipSpace(list, 1); i < list.length && list[i] !== ']'; i = skipSpace(list, i)) {
      const end = skipValue(list, i)
      const item = list.slice(i, end)
      if (valueOf(item, ':status') === ':fail' && valueOf(item, ':not-judged') !== 'true') return valueOf(item, ':expect') ?? ''
      i = end
    }
  }
  return valueOf(formText, ':why') ?? ''
}
const withExtra = (formText, extra) => `${formText.slice(0, -1)} ${extra}}`
const OUTCOMES = { 'skipped-known': 'skipped', pass: 'passed', fail: 'failed', error: 'error', skipped: 'skipped', flaky: 'flaky', inconclusive: 'inconclusive' }
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
// the barrier cases (class `barrier:TICKS`, `barrier:day-TICKS` for a day start), grouped by class: they start alike and set the same time
const barrierGroups = (cases) => [...Map.groupBy(cases.filter((c) => c.cls.startsWith('barrier:')), (c) => c.cls)].map(([key, g]) => ({ key, ids: g.map((c) => c.id) }))
// a rolling night of the natural ids, longest first (runNightBatches starts each while its duration still fits the budget)
const naturalBatch = (ids, secs, budgetS) => {
  const sorted = ids.map((id, i) => ({ id, i })).sort((a, b) => secs(b.id) - secs(a.id) || a.i - b.i).map((x) => x.id)
  return { kind: 'natural', ids: sorted, budgetS, secs: Object.fromEntries(sorted.map((id) => [id, secs(id)])) }
}
// the batches in running order: a natural case that outlasts the budget alone, first; the natural cases as one rolling night; the skip cases; the cases that
// start alike and set the same time once (barrier: they meet at the step); every other solo case alone
export const nightBatches = (cases, durations, { bodies, budgetS = NIGHT_BUDGET_S, defaultS = NIGHT_DEFAULT_S }) => {
  const secs = (id) => durations.get(id) ?? defaultS
  const idsOf = (cls) => cases.filter((c) => c.cls === cls).map((c) => c.id)
  const natural = idsOf('natural')
  const fit = natural.filter((id) => secs(id) <= budgetS)
  return [
    ...naturalBatch(natural.filter((id) => secs(id) > budgetS), secs, budgetS).ids.map((id) => ({ kind: 'long', ids: [id] })),
    ...(fit.length ? [naturalBatch(fit, secs, budgetS)] : []),
    ...chunks(idsOf('skip'), bodies).map((ids) => ({ kind: 'skip', ids })),
    ...barrierGroups(cases).flatMap(({ key, ids }) => chunks(ids, bodies).map((c) => ({ kind: 'barrier', ids: c, barrier: c.length, key }))),
    ...idsOf('solo').map((id) => ({ kind: 'solo', ids: [id] })),
  ]
}
// the reruns of failures ({id, batch}: the batch it failed in), batched as their first runs were: natural ones share a rolling night, the failures of one skip
// or barrier batch meet again (never more than that batch had bodies); every other one alone
export const retryBatches = (failures) => {
  const natural = failures.filter(({ batch }) => batch.kind === 'natural')
  const secs = Object.assign({}, ...natural.map(({ batch }) => batch.secs))
  const together = (kind) => [...Map.groupBy(failures.filter(({ batch }) => batch.kind === kind), ({ batch }) => batch).entries()]
    .map(([batch, g]) => ({ ...batch, ids: g.map(({ id }) => id), ...(batch.barrier ? { barrier: g.length } : {}) }))
  return [
    ...(natural.length ? [naturalBatch(natural.map(({ id }) => id), (id) => secs[id] ?? 0, natural[0].batch.budgetS)] : []),
    ...together('skip'),
    ...together('barrier'),
    ...failures.filter(({ batch }) => !['natural', 'skip', 'barrier'].includes(batch.kind)).map(({ id, batch }) => ({ kind: batch.kind, ids: [id] })),
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
  const p = { bodies: 1, prefix: 'ProbePool', firstPlot: 0, results: null, maxParallel: null, durations: [], retries: 1, serverWaitS: 300, serverSettleS: 10, minBodies: 0, minBodiesWaitS: 1800, paths: [], passthrough: [], matchIds: [], matchIdsFile: null, unskipIds: [], unskipIdsFile: null, nightBatches: false, runKnown: false }
  for (let i = 0; i < args.length; i++) {
    const a = args[i]
    if (!a.startsWith('--')) { p.paths.push(a); continue }
    const v = args[i + 1]
    if (a === '--night-batches') { p.nightBatches = true; continue }
    if (a === '--run-known') { p.runKnown = true; continue }
    if (!POOL_VALUE_FLAGS.includes(a)) { p.passthrough.push(a); if (VALUE_FLAGS.includes(a)) p.passthrough.push(args[++i]); continue }
    i++
    if (a === '--durations') { p.durations.push(v); while (args[i + 1] !== undefined && !args[i + 1].startsWith('--')) p.durations.push(args[++i]); continue }
    if (a === '--bodies') p.bodies = Number(v)
    else if (a === '--body') p.prefix = v
    else if (a === '--first-plot') p.firstPlot = Number(v)
    else if (a === '--results') p.results = v
    else if (a === '--match-id') p.matchIds.push(v)
    else if (a === '--match-ids') p.matchIdsFile = v
    else if (a === '--unskip-id') p.unskipIds.push(v)
    else if (a === '--unskip-ids') p.unskipIdsFile = v
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
  for (const text of previousTexts) {
    for (const f of splitForms(text)) {
      const s = summarize(f)
      const carried = s.status === 'skipped-known' ? valueOf(f, ':previous-failure') : null // a skipped case keeps the failure it was skipped for
      last.set(s.id, s.status === 'fail' ? failSig(f) : carried === null ? null : carried === '""' ? '' : carried)
    }
  }
  return new Map([...last].filter(([, sig]) => sig !== null))
}
// the known failures minus the unskipped ids, which run again (their fix has landed)
export const withoutUnskipped = (known, ids) => new Map([...known].filter(([id]) => !ids.includes(id)))
// the ids that have a known failure are not run; each gets a :skipped-known form per run that carries the failure -> {keep: ids to run, forms}
export const skipKnown = (ids, known, repeat = 1) => ({
  keep: ids.filter((id) => !known.has(id)),
  forms: ids.filter((id) => known.has(id)).flatMap((id) => Array.from({ length: repeat }, (_, i) =>
    `{:id ${JSON.stringify(id)}, :file ${JSON.stringify(id.split('/')[0])}, :run ${i + 1}, :status :skipped-known, :previous-status :fail, :previous-failure ${known.get(id) || '""'}}`)),
})
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
export const runPool = async ({ units, workers, runUnit, retries = 1, load = () => 0, cores = 1, total = null, known = new Map(), preSettled = [], listed = null, repeat = 1, emit = () => {}, log = () => {}, sleep = (ms) => new Promise((r) => setTimeout(r, ms)),
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
    const i = queue.findIndex((j) => j.avoid !== worker.body || alive === 1) // a job avoiding this body waits for another live one
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
  preSettled.forEach((form) => { skipped++; settle(key(summarize(form)), form) })
  const loop = async (worker) => {
    for (;;) {
      if (holding) await holding
      if (unreachable || precheck || bootDead) return
      const job = take(worker)
      if (job) {
        let first = busy ? await load() : 0
        if (first > cores) {
          const since = now()
          let l = first
          while (busy && l > cores) { await sleep(LOAD_POLL_MS); l = await load() }
          log(`world-test pool: ${worker.body} waited ${Math.round((now() - since) / 1000)} s for the load to drop to ${cores} or less (load ${first}, now ${l})`)
        }
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

// Runs the batches one after the other (a batch's cases together, one per worker, each with the batch id in its unit). A natural batch (one with budgetS)
// is a rolling night: a freed body starts the next case while its duration (secs) still fits the budget from the batch start; the cases that no longer fit
// run in the next night. Failed cases rerun after all batches, batched by retryBatches (up to retries times; a pass is :flaky); a known failure is not rerun.
// runUnit({id, batch, barrier, worker, attempt}) -> {code, text}. -> {text, code} like runPool.
export const runNightBatches = async ({ batches, workers, runUnit, retries = 1, known = new Map(), preSettled = [], total = null, emit = () => {}, onBatchEnd = () => {}, now = () => Date.now() }) => {
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
  const runOne = async (id, worker, batch, barrier, attempt) => {
    const res = await runUnit({ id, batch, barrier, worker, attempt })
    if (res.code === PRECHECK_EXIT && !res.text) precheck = true
    const mine = (res.text ? splitForms(res.text) : []).find((f) => summarize(f).id === id)
    const bootFail = res.code === BOOT_EXIT && (!mine || bootFault(mine))
    if (res.code === BOOT_EXIT) dead.add(worker.body)
    return [id, mine ?? errorForm(id.split('/')[0], `the run exited ${res.code} with no result for this case`, id), bootFail]
  }
  // -> {out: [id, form, bootFail] per case run, in batch order, left: the ids for another night}; a case whose body died at boot is bootFail and retires that body
  const runBatch = async ({ ids, barrier, budgetS, secs = {} }, attempt = 0) => {
    const batch = `${process.pid}-${seq++}`
    const out = []
    const left = []
    if (budgetS) {
      const queue = [...ids]
      const start = now()
      let begun = 0
      const fits = (id) => begun === 0 || (now() - start) / 1000 + (secs[id] ?? 0) <= budgetS
      await Promise.all(live().map(async (worker) => {
        while (queue.length && !precheck && !dead.has(worker.body)) {
          const id = queue.shift()
          if (!fits(id)) { left.push(id); continue }
          begun++
          out.push(await runOne(id, worker, batch, barrier, attempt))
        }
      }))
      left.push(...queue)
    } else {
      for (let at = 0; at < ids.length; at += Math.max(1, live().length)) {
        const bodies = live()
        out.push(...await Promise.all(ids.slice(at, at + bodies.length).map((id, k) => runOne(id, bodies[k], batch, barrier, attempt))))
      }
    }
    await onBatchEnd(batch)
    const order = (id) => ids.indexOf(id)
    return { out: out.sort((a, b) => order(a[0]) - order(b[0])), left: left.sort((a, b) => order(a) - order(b)) }
  }
  // the cases whose body died at boot run again on the others (each such death retires a body, so this ends)
  const rerunBootFails = async (out, attempt) => {
    let cur = out
    while (!precheck && cur.some((x) => x[2])) {
      if (!live().length) { bootDead = true; return cur }
      const again = new Map((await runBatch({ ids: cur.filter((x) => x[2]).map((x) => x[0]) }, attempt)).out.map((x) => [x[0], x]))
      cur = cur.map((x) => again.get(x[0]) ?? x)
    }
    return cur
  }
  // runs the batches in order (a rolling night's leftovers next), calling each(out, batch) per batch; false when the pass must stop
  const runAll = async (todo, attempt, each) => {
    const queue = [...todo]
    while (queue.length) {
      const batch = queue.shift()
      if (batch.barrier && live().length < batch.ids.length) { bootDead = true; return false } // the cases of a barrier meet: they need a body each
      const { out, left } = await runBatch(batch, attempt)
      const settled = await rerunBootFails(out, attempt)
      if (precheck || bootDead) return false
      if (left.length && !live().length) { bootDead = true; return false }
      if (left.length) queue.unshift({ ...batch, ids: left })
      each(settled, batch)
    }
    return true
  }
  const stopped = () => (precheck ? { text: mergeText([...done.values()]), precheck, code: PRECHECK_EXIT } : { text: mergeText([...done.values()]), bootDead, code: BOOT_EXIT })
  preSettled.forEach((form) => settle(summarize(form).id, form))
  let failing = []
  const first = new Map()
  const firstPass = await runAll(batches, 0, (out, batch) => {
    for (const [id, form] of out) {
      if (!(failed(summarize(form)) || overran(form)) || retries <= 0) settle(id, form)
      else if (failed(summarize(form)) && known.get(id) === failSig(form)) settle(id, withExtra(form, ':known-failure true'))
      else { first.set(id, form); failing.push({ id, batch }) }
    }
  })
  if (!firstPass) return stopped()
  for (let attempt = 1; attempt <= retries && failing.length; attempt++) {
    const again = []
    reruns += failing.length
    const ok = await runAll(retryBatches(failing), attempt, (out, batch) => {
      for (const [id, form] of out) {
        const s = summarize(form)
        if (s.status === 'pass') settle(id, withStatus(form, 'flaky', `:first-failure ${first.get(id)}`))
        else if ((failed(s) || overran(form)) && attempt < retries) again.push({ id, batch })
        else settle(id, form)
      }
    })
    if (!ok) return stopped()
    failing = again
  }
  const forms = [...preSettled.map((f) => summarize(f).id), ...batches.flatMap((b) => b.ids)].map((id) => done.get(id))
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
// the ids go to the child in a file: thousands of --match-id flags overflow the argv limit (E2BIG)
export const childMatchArgs = (ids, file) => { fs.writeFileSync(file, ids.join('\n') + '\n'); return ['--match-ids', file] }
const spawnChild = (script, pass, matchIds, { file, match, worker, batch, barrier, attempt = 0 }, tmpResults, reaper, traceDir) => new Promise((resolve) => {
  const idsFile = !match && matchIds.length ? `${tmpResults}.ids` : null
  const argv = [script, file, ...pass, ...(traceDir ? ['--trace-dir', traceDir, '--trace-attempt', String(attempt)] : []), ...(batch ? ['--night-batch', batch] : []), ...(batch && barrier ? ['--night-barrier', String(barrier)] : []), '--body', worker.body, '--first-plot', String(worker.firstPlot), '--results', tmpResults, ...(match ? ['--match-id', match, '--repeat', '1'] : (idsFile ? childMatchArgs(matchIds, idsFile) : []))]
  // the pool reports the @@test events itself (fixed total, final status per case), so a child prints none
  const { TEST_EVENTS: _events, ...env } = process.env
  const child = spawn(process.execPath, argv, { stdio: 'inherit', env: { ...env, WORLD_TEST_POOL_PID: String(process.pid) } })
  const entry = reaper.add(child, tmpResults)
  child.on('close', (code) => { reaper.done(entry); if (idsFile) fs.rmSync(idsFile, { force: true }); resolve(code ?? 1) })
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

export const parseMspt = (text) => {
  const m = /Average time per tick:\s*([\d.]+)\s*ms/.exec(text ?? '')
  return m ? Number(m[1]) : null
}
// read-only `tick query` through the existing raw RCON tool; one reading is shared by all workers for MSPT_CACHE_MS
export const rconMspt = (root = path.join(here, '..'), run = (cb) => execFile('node', [path.join(root, 'tools', 'rcon-raw.mjs'), 'tick', 'query'], { cwd: root, timeout: 10000 }, cb)) => {
  let at = 0, pending = null
  return () => {
    if (pending && Date.now() - at < MSPT_CACHE_MS) return pending
    at = Date.now()
    pending = new Promise((resolve) => run((err, out) => resolve(err ? null : parseMspt(out))))
    return pending
  }
}
// The gate's reading, in ms of tick time: the server's own, else the load average scaled so that 1.5x the cores equals the limit.
export const gateLoad = async ({ mspt, loadavg, cores, limit }) => {
  const m = await mspt().catch(() => null)
  return m ?? loadavg() / (cores * LOAD_PER_CORE) * limit
}
// <dir>/sweep-active holds this pool's pid and start time while it runs a sweep-sized pass; tools/res-slot reads it. Returns the remover, or null for a small pool.
export const markSweep = (dir, bodies) => {
  if (bodies < SWEEP_MIN_BODIES) return null
  const file = path.join(dir, 'sweep-active')
  fs.mkdirSync(dir, { recursive: true })
  fs.writeFileSync(file, pidFileBody(process.pid))
  return () => fs.rmSync(file, { force: true })
}

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
// Once before the children start: each child would else find the bundle stale and rebuild it in turn.
const refreshBuild = (script) => spawnSync(process.execPath, [script, '--refresh-build'], { stdio: 'inherit' }).status === 0
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
  if (!refreshBuild(script)) return 2
  stopStaleBodies(script, p.prefix)
  const known = withoutUnskipped(knownFailures(previous), p.unskipIds)
  const skip = p.runKnown || p.matchIds.length ? { keep: cases.map((c) => c.id), forms: [] } : skipKnown(cases.map((c) => c.id), known)
  const batches = nightBatches(cases.filter((c) => skip.keep.includes(c.id)), durationsById(previous), { bodies: workers.length })
  if (skip.forms.length) console.error(`world-test pool: ${skip.forms.length} known failures skipped (--run-known runs them)`)
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
  const res = await runNightBatches({ batches, workers, runUnit, retries: p.retries, total: cases.length, known, preSettled: skip.forms, emit: emitLine, onBatchEnd: (batch) => endNightBatch(script, pass, workers[0].body, batch) })
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
  if (p.unskipIdsFile) p.unskipIds.push(...matchIdsFrom(fs.readFileSync(p.unskipIdsFile, 'utf8')))
  if (p.matchIdsFile && !p.matchIds.length) { console.error(`world-test pool: ${p.matchIdsFile} holds no ids`); return 2 }
  const byStem = new Map(fixtureFiles(p.paths).map((f) => [path.basename(f, '.edn'), f]))
  const previous = readDurations(p.durations)
  const script = path.join(here, 'world-test.mjs')
  if (p.nightBatches) return nightMain(p, byStem, previous, script)
  const listing = listCases(script, p)
  const listed = listing === null ? null : parseListed(listing)
  const known = withoutUnskipped(knownFailures(previous), p.unskipIds)
  const skip = listed === null || p.runKnown || p.matchIds.length ? { keep: null, forms: [] } : skipKnown([...listed.values()].flat(), known, repeatOf(p))
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
  if (!refreshBuild(script)) return 2
  stopStaleBodies(script, p.prefix)
  if (p.passthrough.includes('--allow-time')) restoreSleepRule(script, p.passthrough, workers[0].body, true)
  const reaper = createReaper((f) => fs.rmSync(f, { force: true }))
  process.on('exit', reaper.reap)
  for (const [sig, code] of [['SIGINT', 130], ['SIGTERM', 143]]) process.on(sig, () => { reaper.reap(); process.exit(code) })
  if (skip.forms.length) {
    const keep = new Set(skip.keep)
    for (const [stem, ids] of listed) { const left = ids.filter((id) => keep.has(id)); if (left.length) listed.set(stem, left); else listed.delete(stem) }
    p.matchIds = skip.keep
    units.splice(0, units.length, ...unitsWithCases(units, listed))
    console.error(`world-test pool: ${skip.forms.length} known failures skipped (--run-known runs them)`)
  }
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
  const mspt = rconMspt()
  const clearSweep = markSweep(slotDir(), workers.length)
  if (clearSweep) process.on('exit', clearSweep)
  const r = await runPool({ listed, repeat: repeatOf(p), units, workers, runUnit, retries: p.retries, total, known, preSettled: skip.forms, emit: emitLine, load: () => gateLoad({ mspt, loadavg: () => os.loadavg()[0], cores, limit: MSPT_LIMIT }), cores: MSPT_LIMIT, log: (m) => console.error(m), probe: serverProbe([rconPort(), gamePort()]), serverWaitMs: p.serverWaitS * 1000, settleMs: p.serverSettleS * 1000 })
  clearSweep?.()
  if (r.precheck) console.error('world-test pool: a precheck refused every case (see the line above); pass stopped, no case recorded')
  if (r.bootDead) console.error('world-test pool: every body died at boot (see the child log, e.g. a heap OOM); pass stopped, the cases not run are not recorded')
  if (r.unreachable) console.error(`world-test pool: server unreachable (RCON port ${rconPort()} or game port ${gamePort()} closed for ${p.serverWaitS} s); stopped, the cases not run are not recorded${r.pending?.length ? `; ${r.pending.length} failures awaiting a retry are recorded as inconclusive` : ''}`)
  if (p.results) fs.writeFileSync(p.results, r.text)
  const sums = splitForms(r.text).map(summarize)
  const count = (st) => sums.filter((s) => s.status === st).length
  console.error(`world-test pool: ${sums.length} cases, ${count('pass')} pass, ${count('flaky')} flaky, ${sums.length - count('pass') - count('flaky')} not passing${r.skipped ? `; ${r.skipped} known failures not rerun or skipped (same failed check in the --durations results)` : ''}`)
  return r.code
}
