// Why JavaScript: process orchestration for tools/world-test.mjs (spawns one-body runs, merges their EDN result files); the runner itself is cljs.
// `world-test.mjs <dir|file ...> --bodies N` runs the fixture files across up to N probe bodies and writes one merged --results file.
// A unit is one fixture file, run by an ordinary one-body world-test child (own body slot, body claim, plot block). Pool flags:
//   --bodies N        bodies to use (at most the res-slot body max minus one)
//   --max-parallel M  children at once (default min(N, max(1, cores/4)): each child is a node process plus a live body)
// A new child starts only while the 1-minute load average is at most the core count (polled; skipped when nothing is running).
// SIGINT, SIGTERM and exit kill every child this call spawned (by pid) and delete its temp result file.
//   --body PREFIX     body names PREFIX + A, B, ... (default ProbePool); each needs a whitelist entry
//   --first-plot I    first plot of body A; body k starts at I + 20 k
//   --durations FILE  earlier --results files (repeatable): files run longest first; unseen files count as the median; none = file order
// Every other flag (--phase, --allow-time, --time-log, --tag, ...) goes to every child, so one call is one time phase.
//   --retry-failed N the pool owns retries (children never get it): a failed case (not an error or inconclusive one, as in the single-body runner)
//                     is rerun up to N times (default 1; exactly its --match-id, one run) on a different body when the pool has one; a pass is :flaky.
import fs from 'node:fs'
import path from 'node:path'
import os from 'node:os'
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const PLOTS_PER_BODY = 20
const LOAD_POLL_MS = 5000
const here = path.dirname(fileURLToPath(import.meta.url))
const POOL_VALUE_FLAGS = ['--bodies', '--body', '--first-plot', '--results', '--durations', '--max-parallel', '--retry-failed']
const VALUE_FLAGS = ['--tag', '--match', '--repeat', '--world', '--card', '--time-log', '--phase', '--match-id']
const CLOSERS = { '{': '}', '[': ']', '(': ')' }

// ---- EDN text: just enough to split a results vector into its maps and read their top-level keys
const skipSpace = (s, i) => { while (i < s.length && /[\s,]/.test(s[i])) i++; return i }
const skipString = (s, i) => { i++; while (s[i] !== '"') i += s[i] === '\\' ? 2 : 1; return i + 1 }
// end index of the value starting at i (i at a non-space char)
const skipValue = (s, i) => {
  if (s[i] === '"') return skipString(s, i)
  if (s[i] === '#' && s[i + 1] === '{') i++
  const close = CLOSERS[s[i]]
  if (!close) { while (i < s.length && !/[\s,}\])]/.test(s[i])) i++; return i }
  i++
  for (i = skipSpace(s, i); s[i] !== close; i = skipSpace(s, i)) i = skipValue(s, i)
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
export const splitForms = (text) => {
  const forms = []
  for (let i = skipSpace(text, 1); text[i] !== ']'; i = skipSpace(text, i)) {
    const end = skipValue(text, i)
    forms.push(text.slice(i, end))
    i = end
  }
  return forms
}
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

export const poolCap = ({ bodies, cores, maxParallel }) => maxParallel ?? Math.min(bodies, Math.max(1, Math.floor(cores / 4)))

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
const errorForm = (file, why) => `{:id ${JSON.stringify(file)}, :file ${JSON.stringify(file)}, :status :error, :evidence ${JSON.stringify(why)}}`

// units: file stems; runUnit({file, match, worker}) -> promise of {code, text|null} (text = the child's results vector).
// Resolves to {text, code}: one merged results vector, code 0 when every case passed or was flaky.
export const runPool = async ({ units, workers, runUnit, retries = 1, load = () => 0, cores = 1, sleep = (ms) => new Promise((r) => setTimeout(r, ms)) }) => {
  const queue = units.map((file) => ({ file, match: null, avoid: null }))
  const done = new Map() // `id#run` (or file when the unit died) -> form text, in first-seen order
  let busy = 0
  let wake = []
  const notify = () => { const w = wake; wake = []; w.forEach((r) => r()) }
  const take = (worker) => {
    const i = queue.findIndex((j) => j.avoid !== worker.body || workers.length === 1)
    return i < 0 ? null : queue.splice(i, 1)[0]
  }
  const key = (s) => `${s.id}#${s.run}`
  const record = (job, worker, res) => {
    const retryJob = { file: job.file, avoid: worker.body, retry: { body: worker.body } }
    if (!res.text && job.retry) { done.set(job.file, errorForm(job.file, `the run exited ${res.code} with no results (twice)`)); return }
    if (!res.text) {
      queue.push({ ...retryJob, match: null })
      done.set(job.file, errorForm(job.file, `the run exited ${res.code} with no results`))
      return
    }
    const forms = splitForms(res.text).map((form) => ({ form, s: summarize(form) }))
    if (job.retry && job.match === null) { // a whole-file rerun replaces the placeholder error
      done.delete(job.file)
      forms.forEach(({ form, s }) => done.set(key(s), form))
      return
    }
    if (job.retry) { // a case rerun (one run of one case): a pass is flaky, a failure is rerun until the retries are spent
      const k = `${job.match}#${job.run}`
      for (const { form, s } of forms.map((x) => ({ ...x, form: withRun(x.form, job.run) })).filter((x) => x.s.id === job.match)) {
        if (s.status === 'pass') done.set(k, withStatus(form, 'flaky', `:first-failure ${job.firstForm} :first-failure-body ${JSON.stringify(job.retry.body)}`))
        else if (failed(s) && job.attempt < retries) queue.push({ ...retryJob, match: s.id, run: job.run, attempt: job.attempt + 1, firstForm: job.firstForm })
        else done.set(k, form)
      }
      return
    }
    for (const { form, s } of forms) {
      done.set(key(s), form)
      if (failed(s) && retries > 0) queue.push({ ...retryJob, match: s.id, run: s.run, attempt: 1, firstForm: form })
    }
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
  return { text: mergeText(forms), code: forms.length && forms.every((f) => ['pass', 'flaky'].includes(summarize(f).status)) ? 0 : 1 }
}

// ---- the real thing
const sleep = (ms) => new Promise((r) => setTimeout(r, ms))
const fixtureFiles = (paths) => paths.flatMap((p) => (fs.statSync(p).isDirectory()
  ? fs.readdirSync(p).filter((f) => f.endsWith('.edn')).sort().map((f) => path.join(p, f))
  : [p]))
const bodyCap = () => Math.max(1, (JSON.parse(fs.readFileSync(path.join(here, 'res-slot.json'), 'utf8')).kinds.body.max ?? 1) - 1)

// One child per unit: the ordinary one-body entry point (it takes the body slot and the body claim itself). Exit 75 = busy: ask again.
const spawnChild = (script, pass, { file, match, worker }, tmpResults, reaper) => new Promise((resolve) => {
  const argv = [script, file, ...pass, '--body', worker.body, '--first-plot', String(worker.firstPlot), '--results', tmpResults, ...(match ? ['--match-id', match, '--repeat', '1'] : [])]
  const child = spawn(process.execPath, argv, { stdio: 'inherit' })
  const entry = reaper.add(child, tmpResults)
  child.on('close', (code) => { reaper.done(entry); resolve(code ?? 1) })
})

export const main = async (args) => {
  const p = parsePoolArgs(args)
  const byStem = new Map(fixtureFiles(p.paths).map((f) => [path.basename(f, '.edn'), f]))
  const previous = p.durations.filter((f) => fs.existsSync(f)).map((f) => fs.readFileSync(f, 'utf8'))
  const units = unitOrder([...byStem.keys()], previous)
  const cores = os.cpus().length
  const workers = workerSpecs(poolCap({ bodies: p.bodies, cores, maxParallel: p.maxParallel }), bodyCap(), p.prefix, p.firstPlot)
  const reaper = createReaper((f) => fs.rmSync(f, { force: true }))
  process.on('exit', reaper.reap)
  for (const [sig, code] of [['SIGINT', 130], ['SIGTERM', 143]]) process.on(sig, () => { reaper.reap(); process.exit(code) })
  console.error(`world-test pool: ${units.length} fixture files on ${workers.length} bodies (${workers.map((w) => w.body).join(' ')})`)
  const script = path.join(here, 'world-test.mjs')
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
  const r = await runPool({ units, workers, runUnit, retries: p.retries, load: () => os.loadavg()[0], cores })
  if (p.results) fs.writeFileSync(p.results, r.text)
  const sums = splitForms(r.text).map(summarize)
  const count = (st) => sums.filter((s) => s.status === st).length
  console.error(`world-test pool: ${sums.length} cases, ${count('pass')} pass, ${count('flaky')} flaky, ${sums.length - count('pass') - count('flaky')} not passing`)
  return r.code
}
