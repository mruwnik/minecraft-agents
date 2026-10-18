#!/usr/bin/env node
// Why JavaScript: a thin Node launcher (copies the compiled test bundle, runs node under res-slot and timeout(1)); no engine behaviour.
// tools/test-run.mjs <ns>...   (called by tools/test-engine after the compile; namespaces space- or comma-separated)
//  Runs a private copy of out/test.cjs + out/test/cljs-runtime (/tmp/mc-test-run-<pid>, so a concurrent compile cannot swap it) under one
//  res-slot 'tests' slot sized by the namespace count (needMb), killed when no test has finished for stallLimitS (MC_TEST_STALL_S overrides, MC_TEST_FIRST_S the 300 s allowed to the first finished test; a spinning test too, the watchdog is this parent) so a hung test frees its slot, and after WHOLE_RUN_CAP_S (MC_TEST_TIMEOUT_S overrides).
//  Only the requested -test namespaces are loaded (narrowBundle: imports and shadow.test registry cut from the source, only the imported files copied; engine.timing-test, the timing / TEST_EVENTS=1 @@test reporter, always stays); full and shard runs keep the whole bundle.
import fs from 'node:fs'
import path from 'node:path'
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const tools = path.dirname(fileURLToPath(import.meta.url))
const repo = path.resolve(tools, '..')
const engine = path.join(repo, 'engine')

// Exit status for a requested namespace or deftest missing from the bundle.
export const MISSING_RC = 3

export const parseNss = (argv) => argv.flatMap((a) => a.split(',')).filter(Boolean)

// An arg is a namespace or <namespace>/<deftest> (shadow's --test takes both); narrowing, sizing and timings go by namespace.
export const nsOf = (spec) => spec.split('/')[0]

const MUNGE = { '-': '_', '?': '_QMARK_', '!': '_BANG_', '*': '_STAR_', '>': '_GT_', '<': '_LT_', '=': '_EQ_', '+': '_PLUS_' }
const mungeName = (name) => name.replace(/[-?!*><=+]/g, (c) => MUNGE[c])

// Mean ms per namespace in a full run (411 s / 196 namespaces), used when no timings exist yet (fresh worktree).
export const DEFAULT_NS_MS = 2100

// Prior per-namespace ms summed; a namespace without a prior timing costs the mean of the known ones (DEFAULT_NS_MS with none).
export const expectedMs = (nss, ms) => {
  const known = Object.values(ms)
  const mean = known.length ? known.reduce((a, b) => a + b, 0) / known.length : DEFAULT_NS_MS
  return Math.round(nss.reduce((t, n) => t + (ms[n] ?? mean), 0))
}

// Backstop only: the stall watchdog (watchStall) ends a hung run; this ends one that keeps finishing tests for hours.
export const WHOLE_RUN_CAP_S = 3600

// Measured peak RSS (VmHWM, 2026-10-16): 464-476 MB for one namespace, 476-640 for 3-5, 640-722 for many, with the heap capped at kinds.tests.heapMb (uncapped, V8 let garbage grow it to 2.3 GB); a full shard (~45 namespaces) needs needMb. The table of needs is kinds.tests in res-slot.json (needMb = shard and cap, goldenMb).
export const needMb = (nsCount, tests) => Math.min(tests.needMb, tests.baseMb + tests.perNsMb * nsCount)

// A hung test stops the timings file growing (one line per finished test, one {"started"} line at load), so the run is killed when it stalls, not after a whole-run time.
// Floor, because a test may legitimately idle (waits, loaded machine); k x the slowest prior test of the requested namespaces when timings exist.
export const STALL_FLOOR_S = 90, STALL_K = 10
// Until the first test finishes the run is still loading (bundle, setup, a long first test under machine load), so it gets this longer limit.
export const FIRST_RESULT_S = 300
export const slowestPriorMs = (nss, lines) => {
  const want = new Set(nss)
  return lines.filter(Boolean).map((l) => { try { return JSON.parse(l) } catch { return {} } })
    .filter((r) => r.var && want.has(r.var.replace(/^#'/, '').split('/')[0]))
    .reduce((m, r) => Math.max(m, r.ms), 0)
}
export const stallLimitS = (slowestMs, floor = STALL_FLOOR_S, k = STALL_K) => Math.max(floor, Math.round(k * slowestMs / 1000))

// Polls the timings file; once the node run has started (non-empty file) and the file has not grown for limitS
// (firstS, if longer, until a test has finished), calls onStall() once. Returns stop().
export const watchStall = (file, limitS, onStall, { pollMs = 2000, now = Date.now, firstS = FIRST_RESULT_S } = {}) => {
  let size = 0, since = null, fired = false, anyTest = false
  const timer = setInterval(() => {
    let cur
    try { cur = fs.statSync(file).size } catch { return }
    if (cur !== size) { size = cur; since = now(); anyTest = anyTest || fs.readFileSync(file, 'utf8').includes('"var"'); return }
    if (since === null || fired || now() - since < (anyTest ? limitS : Math.max(limitS, firstS)) * 1000) return
    fired = true; clearInterval(timer); onStall()
  }, pollMs)
  return () => clearInterval(timer)
}

// SIGTERM stops a spinning node (no handler installed); SIGKILL after a grace period for the rest.
export const killStalled = (pid, graceMs = 5000) => {
  killTree(pid, 'SIGTERM')
  setTimeout(() => killTree(pid, 'SIGKILL'), graceMs).unref()
}

export const stallMessage = (label, limitS, lines, nss) => {
  const done = new Set(lines.filter(Boolean).map((l) => { try { return JSON.parse(l).var } catch { return null } }).filter(Boolean).map((v) => v.replace(/^#'/, '').split('/')[0]))
  const untouched = nss.filter((n) => !done.has(n))
  return `${label}: STALLED, no test finished for ${limitS} s (killed); last finished test: ${lastFinished(lines) ?? 'none'}; the stuck test is the one after it in its namespace; namespaces with no finished test: ${untouched.length ? untouched.join(' ') : 'none'}`
}

export const lastFinished = (lines) =>
  lines.filter(Boolean).map((l) => JSON.parse(l)).filter((r) => r.var).at(-1)?.var ?? null

// Private copy of the bundle; fixtures resolve as <out>/../test, npm deps through node_modules.
// With nss (targeted run) the bundle is narrowed from the source dir first and only the files it imports (+ .map) are copied.
export const isolate = (engineDir, tag, root = '/tmp', nss = null) => {
  const runDir = path.join(root, `mc-test-run-${tag}`)
  const srcRuntime = path.join(engineDir, 'out/test/cljs-runtime')
  const runtime = path.join(runDir, 'out/test/cljs-runtime')
  fs.rmSync(runDir, { recursive: true, force: true })
  fs.mkdirSync(runtime, { recursive: true })
  if (nss) narrowBundle(engineDir, nss, runDir)
  else fs.copyFileSync(path.join(engineDir, 'out/test.cjs'), path.join(runDir, 'out/test.cjs'))
  const wanted = nss
    ? [...fs.readFileSync(path.join(runDir, 'out/test.cjs'), 'utf8').matchAll(/^SHADOW_IMPORT\("(.+)"\);$/gm)].flatMap((m) => [m[1], `${m[1]}.map`])
    : fs.readdirSync(srcRuntime)
  for (const f of wanted) {
    if (fs.existsSync(path.join(runtime, f)) || !fs.existsSync(path.join(srcRuntime, f))) continue
    fs.copyFileSync(path.join(srcRuntime, f), path.join(runtime, f))
  }
  fs.symlinkSync(path.join(engineDir, 'node_modules'), path.join(runDir, 'node_modules'))
  fs.symlinkSync(path.join(engineDir, 'test'), path.join(runDir, 'test'))
  return { runDir, cleanup: () => fs.rmSync(runDir, { recursive: true, force: true }) }
}

const pidAlive = (pid) => {
  try { process.kill(pid, 0); return true } catch (e) { return e.code === 'EPERM' }
}

// Removes the run dirs of dead pids (a SIGKILLed run leaves its copy in RAM): <root>/mc-test-run-<digits> and the test process's
// <root>/engine-test-<digits>-<random> temp dirs (engine/test/engine/test_util.cljs tmp-dir); never a live pid.
export const sweepStale = (root = '/tmp', alive = pidAlive) => {
  const removed = []
  for (const name of fs.readdirSync(root)) {
    const m = /^(?:mc-test-run-(\d+)|engine-test-(\d+)-.+)$/.exec(name)
    if (!m || alive(Number(m[1] ?? m[2]))) continue
    try { fs.rmSync(path.join(root, name), { recursive: true, force: true }) } catch { continue } // not ours to delete (EACCES, EBUSY): skip it
    removed.push(name)
  }
  return removed
}

// Pids of every descendant of pid (any process group or session), read from /proc.
const descendants = (pid) => {
  const kids = new Map()
  for (const d of fs.readdirSync('/proc')) {
    if (!/^\d+$/.test(d)) continue
    try {
      const ppid = Number(/^\d+ \(.*\) \S (\d+)/.exec(fs.readFileSync(`/proc/${d}/stat`, 'utf8'))[1])
      kids.set(ppid, [...(kids.get(ppid) ?? []), Number(d)])
    } catch { /* the process ended meanwhile */ }
  }
  const out = []
  for (const queue = [pid]; queue.length;) for (const k of kids.get(queue.pop()) ?? []) { out.push(k); queue.push(k) }
  return out
}

// Signals pid and all its descendants (res-slot -> flock -> timeout -> node: timeout(1) leaves the process group, so a group kill would miss the test node).
export const killTree = (pid, sig = 'SIGTERM', self = true) => {
  for (const p of [...descendants(pid).reverse(), ...(self ? [pid] : [])]) try { process.kill(p, sig) } catch { /* already gone */ }
}

// Runs cleanup once on the proc's exit or on SIGINT/SIGTERM/SIGHUP (then exits 128+signal); returns the once-only cleanup.
export const cleanupOnExit = (proc, cleanup, onSignal = () => {}) => {
  let done = false
  const once = () => { if (!done) { done = true; cleanup() } }
  proc.on('exit', once)
  for (const [sig, n] of [['SIGINT', 2], ['SIGHUP', 1], ['SIGTERM', 15]]) proc.on(sig, () => { onSignal(sig); once(); proc.exit(128 + n) })
  return once
}

// Index just past the bracket that closes the one at s[i], skipping double-quoted strings.
const closeAt = (s, i) => {
  let depth = 0
  for (let j = i; j < s.length; j++) {
    const c = s[j]
    if (c === '"') { for (j++; s[j] !== '"'; j++) if (s[j] === '\\') j++ } else if ('([{'.includes(c)) depth++
    else if (')]}'.includes(c) && --depth === 0) return j + 1
  }
  return -1
}

// The top-level comma-separated elements of the array literal s[i..] ('[' at i).
const arrayElements = (s, i) => {
  const end = closeAt(s, i) - 1
  const out = []
  for (let j = i + 1; j < end;) {
    let k = j
    while (k < end && s[k] !== ',') k = '"([{'.includes(s[k]) ? (s[k] === '"' ? closeStr(s, k) : closeAt(s, k)) : k + 1
    out.push(s.slice(j, k)); j = k + 1
  }
  return { elements: out, end: end + 1 }
}
const closeStr = (s, i) => { let j = i + 1; for (; s[j] !== '"'; j++) if (s[j] === '\\') j++; return j + 1 }

// shadow.test.node.js passes every test namespace to reset_test_data_BANG_ as one map: PersistentHashMap.fromArrays([ns symbols],[test vars]) for many namespaces,
// PersistentArrayMap(null, n, [ns, vars, ns, vars...]) for few (a targeted compile). Keeps only the namespaces in keep; null when the form is not recognised.
const narrowRegistry = (js, keep) => {
  const m = /reset_test_data_BANG_\((cljs\.core\.PersistentHashMap\.fromArrays\(\[|new cljs\.core\.PersistentArrayMap\(null, (\d+), \[)/.exec(js)
  if (!m) return null
  const wanted = (sym) => keep.has(/"([^"]+)"/.exec(sym)[1].replaceAll('-', '_'))
  const open = m.index + m[0].length - 1
  const first = arrayElements(js, open)
  if (m[2] !== undefined) {
    if (first.elements.length !== 2 * Number(m[2])) return null
    const kept = first.elements.flatMap((el, n) => n % 2 === 0 && wanted(el) ? [el, first.elements[n + 1]] : [])
    return js.slice(0, m.index) + `reset_test_data_BANG_(new cljs.core.PersistentArrayMap(null, ${kept.length / 2}, [${kept.join(',')}]` + js.slice(first.end)
  }
  if (js.slice(first.end, first.end + 2) !== ',[') return null
  const vals = arrayElements(js, first.end + 1)
  if (first.elements.length !== vals.elements.length) return null
  const pick = (els) => els.filter((_, n) => wanted(first.elements[n]))
  return js.slice(0, open) + `[${pick(first.elements).join(',')}],[${pick(vals.elements).join(',')}]` + js.slice(vals.end)
}

// A targeted run loads only the requested -test namespaces (and the -test namespaces they require): drops the other SHADOW_IMPORT lines
// and test-registry entries (2.2 s of the 3.6 s load for all 412). Reads srcDir, writes the bundle (and the narrowed registry file) under outDir, which
// may be srcDir itself. Returns how many imports it dropped (0 = bundle unchanged).
export const narrowBundle = (srcDir, nss, outDir = srcDir) => {
  const bundle = path.join(srcDir, 'out/test.cjs')
  const outBundle = path.join(outDir, 'out/test.cjs')
  const runtime = path.join(srcDir, 'out/test/cljs-runtime')
  const unchanged = () => { if (outDir !== srcDir) fs.copyFileSync(bundle, outBundle); return 0 }
  const queue = nss.map((n) => n.replaceAll('-', '_'))
  const keep = new Set([...queue, 'engine.timing_test']) // the timing and @@test reporter hooks are not requested but must load
  while (queue.length) {
    const f = path.join(runtime, `${queue.pop()}.js`)
    if (!fs.existsSync(f)) continue
    // The dev output has no goog.require lines: a required test namespace shows as a qualified reference (engine.go_to_test.go_BANG_).
    for (const [, dep] of fs.readFileSync(f, 'utf8').matchAll(/\b((?:\w+\.)+\w*_test)(?=[.'"])/g))
      if (!keep.has(dep)) { keep.add(dep); queue.push(dep) }
  }
  const nodeJs = path.join(runtime, 'shadow.test.node.js')
  if (!fs.existsSync(nodeJs)) return unchanged()
  const narrowed = narrowRegistry(fs.readFileSync(nodeJs, 'utf8'), keep)
  if (narrowed === null) return unchanged()
  fs.mkdirSync(path.join(outDir, 'out/test/cljs-runtime'), { recursive: true })
  fs.writeFileSync(path.join(outDir, 'out/test/cljs-runtime/shadow.test.node.js'), narrowed)
  let dropped = 0
  const lines = fs.readFileSync(bundle, 'utf8').split('\n').filter((l) => {
    const m = /^SHADOW_IMPORT\("(.+_test)\.js"\);$/.exec(l)
    if (!m || keep.has(m[1])) return true
    dropped++
    return false
  })
  fs.writeFileSync(outBundle, lines.join('\n'))
  return dropped
}

// Requested specs the :test bundle does not contain (e.g. the opt-in -golden namespaces, built by --golden; a deftest that is not defined): shadow runs 0 tests and exits 0.
export const missingNss = (engineDir, specs) =>
  specs.filter((spec) => {
    const [ns, name] = spec.split('/')
    const f = path.join(engineDir, 'out/test/cljs-runtime', `${ns.replaceAll('-', '_')}.js`)
    if (!fs.existsSync(f)) return true
    if (name === undefined) return false
    return !fs.readFileSync(f, 'utf8').split('\n').some((l) => l.startsWith(`${ns.replaceAll('-', '_')}.${mungeName(name)} = `))
  })

const readText = (f) => fs.existsSync(f) ? fs.readFileSync(f, 'utf8') : ''
const readJson = (f, d) => fs.existsSync(f) ? JSON.parse(fs.readFileSync(f, 'utf8')) : d

const main = async () => {
  const specs = parseNss(process.argv.slice(2))
  const nss = [...new Set(specs.map(nsOf))]
  if (!nss.length) { console.error('usage: tools/test-run.mjs <ns|ns/deftest>...'); process.exit(2) }
  const res = readJson(path.join(tools, 'res-slot.json'), null)
  const expected = expectedMs(nss, readJson(path.join(engine, 'out/test-ns-ms.json'), {}))
  const limit = Number(process.env.MC_TEST_TIMEOUT_S) || WHOLE_RUN_CAP_S
  const stallS = Number(process.env.MC_TEST_STALL_S) || stallLimitS(slowestPriorMs(nss, (readText(path.join(engine, 'out/test-timings.jsonl'))).split('\n')))
  sweepStale()
  const { runDir, cleanup } = isolate(engine, process.pid, '/tmp', nss)
  let child = null
  cleanupOnExit(process, cleanup, (sig) => child && killTree(child.pid, sig))
  // Checked on the private copy: a concurrent targeted compile of other namespaces in this checkout can replace engine/out under us (test-engine recompiles once on MISSING_RC).
  const missing = missingNss(runDir, specs)
  if (missing.length) {
    console.error(`test-engine: not in the :test bundle (a run would pass with 0 tests): ${missing.join(' ')}${missing.some((n) => nsOf(n).endsWith('-golden')) ? ' (the -golden ones run with tools/test-engine --golden)' : ''}`)
    process.exit(MISSING_RC)
  }
  const timings = path.join(runDir, 'timings.jsonl')
  fs.writeFileSync(timings, '')
  child = spawn(path.join(tools, 'res-slot'),
    ['tests', '--need', String(needMb(nss.length, res.kinds.tests)), '--',
      'timeout', '-k', '10', String(limit), 'node', `--max-old-space-size=${res.kinds.tests.heapMb}`, path.join(runDir, 'out/test.cjs'), `--test=${specs.join(',')}`],
    { cwd: engine, stdio: 'inherit', env: { ...process.env, MC_TEST_TIMINGS: timings, NODE_PATH: path.join(repo, 'node_modules') } })
  let stalled = false
  const stop = watchStall(timings, stallS, () => { stalled = true; killStalled(child.pid) }, { firstS: Number(process.env.MC_TEST_FIRST_S) || FIRST_RESULT_S })
  const exited = await new Promise((resolve) => child.on('exit', (status) => resolve(status ?? 1)))
  stop()
  const code = stalled ? 124 : exited
  if (stalled) console.error(stallMessage('test-engine', stallS, fs.readFileSync(timings, 'utf8').split('\n'), nss))
  if (!stalled && (code === 124 || code === 137))
    console.error(`test-engine: TIMEOUT, killed after ${limit} s (prior timing ~${Math.round(expected / 1000)} s); last finished test: ${lastFinished(fs.readFileSync(timings, 'utf8').split('\n')) ?? 'none'}`)
  process.exit(code)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main()
