// Why JavaScript: process orchestration for tools/world-test.mjs (spawns one-body runs, merges their EDN result files); the runner itself is cljs.
// `world-test.mjs <dir|file ...> --bodies N` runs the fixture files across up to N probe bodies and writes one merged --results file.
// A unit is one fixture file, run by an ordinary one-body world-test child (own body slot, body claim, plot block). Pool flags:
//   --bodies N        bodies to use (at most the res-slot body max minus one)
//   --max-parallel M  children at once (default min(N, body slot max minus one, bodies the memory above the res-slot floor holds at the body slot's needMb))
// A new child starts only while the 1-minute load average is at most the core count (polled; the first child always starts).
// SIGINT, SIGTERM and exit kill every child this call spawned (by pid) and delete its temp result file.
//   --body PREFIX     body names PREFIX + A, B, ... (default ProbePool); each needs a whitelist entry
//   --first-plot I    first plot of body A; body k starts at I + 20 k
//   --durations FILE  earlier --results files (repeatable): files run longest first; unseen files count as the median; none = file order
// Every other flag (--phase, --allow-time, --time-log, --tag, ...) goes to every child, so one call is one time phase.
// With TEST_EVENTS=1 the pool prints the @@test lines itself (children print none): a plan with the total of all listed cases up front
// (world-test --list), one result per case with its final status (a rerun that also fails is one failure), and progress lines
// {done, total, retries} (reruns are counted in retries only).
// A failed case whose last result in the --durations files was a fail on the same first failing check is a known failure: no rerun, :known-failure true in its result (and knownFailure in its event).
//   --retry-failed N the pool owns retries (children never get it): a failed case (not an error or inconclusive one, as in the single-body runner)
//                     is rerun up to N times (default 1; exactly its --match-id, one run) on a different body when the pool has one; a pass is :flaky.
import fs from 'node:fs'
import path from 'node:path'
import os from 'node:os'
import { spawn, spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const PLOTS_PER_BODY = 20
const LOAD_POLL_MS = 5000
const here = path.dirname(fileURLToPath(import.meta.url))
const POOL_VALUE_FLAGS = ['--bodies', '--body', '--first-plot', '--results', '--durations', '--max-parallel', '--retry-failed']
const VALUE_FLAGS = ['--tag', '--match', '--repeat', '--world', '--card', '--time-log', '--phase', '--match-id']
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
const OUTCOMES = { pass: 'passed', fail: 'failed', error: 'error', skipped: 'skipped', flaky: 'flaky' }
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
  const p = { bodies: 1, prefix: 'ProbePool', firstPlot: 0, results: null, maxParallel: null, durations: [], retries: 1, paths: [], passthrough: [] }
  for (let i = 0; i < args.length; i++) {
    const a = args[i]
    if (!a.startsWith('--')) { p.paths.push(a); continue }
    const v = args[i + 1]
    if (!POOL_VALUE_FLAGS.includes(a)) { p.passthrough.push(a); if (VALUE_FLAGS.includes(a)) p.passthrough.push(args[++i]); continue }
    i++
    if (a === '--bodies') p.bodies = Number(v)
    else if (a === '--body') p.prefix = v
    else if (a === '--first-plot') p.firstPlot = Number(v)
    else if (a === '--results') p.results = v
    else if (a === '--max-parallel') p.maxParallel = Number(v)
    else if (a === '--retry-failed') p.retries = Number(v)
    else p.durations.push(v)
  }
  return p
}

const bodyLetter = (k) => (k < 26 ? String.fromCharCode(65 + k) : String(k))
export const workerSpecs = (wanted, cap, prefix, firstPlot) =>
  Array.from({ length: Math.max(1, Math.min(wanted, cap)) }, (_, k) => ({ body: prefix + bodyLetter(k), firstPlot: firstPlot + PLOTS_PER_BODY * k }))

// bodies at once: --max-parallel, else the bodies asked for, capped by the body slot max and by what the memory above the res-slot floor holds
export const poolCap = ({ bodies, maxParallel, availableMb, floorMb, needMb, bodyMax }) =>
  maxParallel ?? Math.max(1, Math.min(bodies, bodyMax, Math.floor((availableMb - floorMb) / needMb)))

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
export const runPool = async ({ units, workers, runUnit, retries = 1, load = () => 0, cores = 1, total = null, known = new Map(), listed = null, repeat = 1, emit = () => {}, sleep = (ms) => new Promise((r) => setTimeout(r, ms)) }) => {
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
        if (!settled.has(`${id}#${run}`) && !done.has(`${id}#${run}`)) settle(`${id}#${run}`, withRun(errorForm(file, why, id), run))
      }
    }
  }
  const rerun = (j) => { reruns++; queue.push(j) }
  const record = (job, worker, res) => {
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
      const job = take(worker)
      if (job) {
        while (busy && load() > cores) await sleep(LOAD_POLL_MS)
        busy++
        const res = await runUnit({ file: job.file, match: job.match, exact: job.match !== null, worker })
        record(job, worker, res)
        busy--
        notify()
        continue
      }
      if (!busy && !queue.length) return
      await new Promise((r) => wake.push(r))
    }
  }
  await Promise.all(workers.map(loop))
  const forms = [...done.values()]
  return { skipped, text: mergeText(forms), code: !forms.length ? 2 : forms.every((f) => ['pass', 'flaky'].includes(summarize(f).status)) ? 0 : 1 }
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
const spawnChild = (script, pass, { file, match, worker }, tmpResults, reaper) => new Promise((resolve) => {
  const argv = [script, file, ...pass, '--body', worker.body, '--first-plot', String(worker.firstPlot), '--results', tmpResults, ...(match ? ['--match-id', match, '--repeat', '1'] : [])]
  // the pool reports the @@test events itself (fixed total, final status per case), so a child prints none
  const { TEST_EVENTS: _events, ...env } = process.env
  const child = spawn(process.execPath, argv, { stdio: 'inherit', env })
  const entry = reaper.add(child, tmpResults)
  child.on('close', (code) => { reaper.done(entry); resolve(code ?? 1) })
})

// every case of the fixture files as the children will select them (--list needs no world); null when the listing fails
// the --list call selects cases with the same paths and flags (--phase, --tag, --match, ...) the children get
export const listArgs = (p) => [...p.paths, ...p.passthrough, '--list']
const repeatOf = (p) => { const at = p.passthrough.indexOf('--repeat'); return at < 0 ? 1 : Number(p.passthrough[at + 1]) }
const listCases = (script, p) => {
  const r = spawnSync(process.execPath, [script, ...listArgs(p)], { encoding: 'utf8', maxBuffer: 16 * 1024 * 1024 })
  return r.status === 0 ? r.stdout : null
}

// the texts of the existing --durations files that hold a complete results vector; an empty or cut-off one is warned about and skipped
const readDurations = (files) => files.filter((f) => fs.existsSync(f)).flatMap((f) => {
  const text = fs.readFileSync(f, 'utf8')
  if (scanForms(text).complete) return [text]
  console.error(`world-test pool: --durations file ${f} is empty or truncated, skipped`)
  return []
})

export const main = async (args) => {
  const p = parsePoolArgs(args)
  const byStem = new Map(fixtureFiles(p.paths).map((f) => [path.basename(f, '.edn'), f]))
  const previous = readDurations(p.durations)
  const script = path.join(here, 'world-test.mjs')
  const listing = listCases(script, p)
  const listed = listing === null ? null : parseListed(listing)
  const units = unitsWithCases(unitOrder([...byStem.keys()], previous), listed)
  if (noCasesSelected(units, listed)) {
    console.error('world-test pool: no cases selected (none match the paths and flags, or all are unchanged under --changed-since-pass)')
    emitLine({ event: 'plan', total: 0 })
    return 2
  }
  const cores = os.cpus().length
  const workers = workerSpecs(poolCap({ bodies: p.bodies, maxParallel: p.maxParallel, availableMb: availableMb(), floorMb: slotConfig().floorMb, needMb: slotConfig().kinds.body.needMb, bodyMax: bodyCap() }), bodyCap(), p.prefix, p.firstPlot)
  const reaper = createReaper((f) => fs.rmSync(f, { force: true }))
  process.on('exit', reaper.reap)
  for (const [sig, code] of [['SIGINT', 130], ['SIGTERM', 143]]) process.on(sig, () => { reaper.reap(); process.exit(code) })
  console.error(`world-test pool: ${units.length} fixture files on ${workers.length} bodies (${workers.map((w) => w.body).join(' ')})`)
  const runUnit = async (unit) => {
    const tmp = `${p.results ?? path.join(process.env.TMPDIR ?? '/tmp', 'world-test-pool')}.${process.pid}.${unit.worker.body}.edn`
    let code
    for (let tries = 0; tries < 20; tries++) {
      fs.rmSync(tmp, { force: true })
      code = await spawnChild(script, p.passthrough, { ...unit, file: byStem.get(unit.file) }, tmp, reaper)
      if (code !== 75) break
      await sleep(5000)
    }
    const text = fs.existsSync(tmp) ? fs.readFileSync(tmp, 'utf8') : null
    fs.rmSync(tmp, { force: true })
    return { code, text }
  }
  const total = listing === null ? null : countListed(listing, repeatOf(p))
  if (total !== null) emitLine({ event: 'plan', total })
  const r = await runPool({ listed, repeat: repeatOf(p), units, workers, runUnit, retries: p.retries, total, known: knownFailures(previous), emit: emitLine, load: () => os.loadavg()[0], cores })
  if (p.results) fs.writeFileSync(p.results, r.text)
  const sums = splitForms(r.text).map(summarize)
  const count = (st) => sums.filter((s) => s.status === st).length
  console.error(`world-test pool: ${sums.length} cases, ${count('pass')} pass, ${count('flaky')} flaky, ${sums.length - count('pass') - count('flaky')} not passing${r.skipped ? `; ${r.skipped} known failures not rerun (same failed check in the --durations results)` : ''}`)
  return r.code
}
