#!/usr/bin/env node
// Why JavaScript: a thin Node launcher (copies the compiled test bundle, runs node under res-slot and timeout(1)); no engine behaviour.
// tools/test-run.mjs <ns>...   (called by tools/test-engine after the compile; namespaces space- or comma-separated)
//  Runs a private copy of out/test.cjs + out/test/cljs-runtime (/tmp/mc-test-run-<pid>, so a concurrent compile cannot swap it) under one
//  res-slot 'tests' slot sized by the namespace count (needMb), killed after runTimeoutS (from engine/out/test-ns-ms.json) so a hung test frees its slot (env MC_TEST_TIMEOUT_S overrides).
//  Only the requested -test namespaces are loaded (narrowBundle: imports and shadow.test registry cut in the private copy; engine.timing-test, the timing / TEST_EVENTS=1 @@test reporter, always stays); full and shard runs keep the whole bundle.
import fs from 'node:fs'
import path from 'node:path'
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const tools = path.dirname(fileURLToPath(import.meta.url))
const repo = path.resolve(tools, '..')
const engine = path.join(repo, 'engine')

export const parseNss = (argv) => argv.flatMap((a) => a.split(',')).filter(Boolean)

// Prior per-namespace ms summed; a namespace without a prior timing costs the mean of the known ones.
export const expectedMs = (nss, ms) => {
  const known = Object.values(ms)
  const mean = known.length ? known.reduce((a, b) => a + b, 0) / known.length : 0
  return Math.round(nss.reduce((t, n) => t + (ms[n] ?? mean), 0))
}

// Generous: a loaded machine runs a namespace up to ~3x slower than its prior timing.
export const runTimeoutS = (expected) => Math.max(180, Math.min(1200, Math.round(60 + 5 * expected / 1000)))

// Measured peak RSS: 600-610 MB for one namespace, 750 for ten, 2.45 GB for a full shard (~45 namespaces). The table of needs is kinds.tests in res-slot.json (needMb = shard and cap, goldenMb).
export const needMb = (nsCount, tests) => Math.min(tests.needMb, tests.baseMb + tests.perNsMb * nsCount)

export const lastFinished = (lines) =>
  lines.filter(Boolean).map((l) => JSON.parse(l)).filter((r) => r.var).at(-1)?.var ?? null

// Private copy of the bundle; fixtures resolve as <out>/../test, npm deps through node_modules.
export const isolate = (engineDir, tag, root = '/tmp') => {
  const runDir = path.join(root, `mc-test-run-${tag}`)
  fs.rmSync(runDir, { recursive: true, force: true })
  fs.mkdirSync(path.join(runDir, 'out/test'), { recursive: true })
  fs.copyFileSync(path.join(engineDir, 'out/test.cjs'), path.join(runDir, 'out/test.cjs'))
  fs.cpSync(path.join(engineDir, 'out/test/cljs-runtime'), path.join(runDir, 'out/test/cljs-runtime'), { recursive: true })
  fs.symlinkSync(path.join(engineDir, 'node_modules'), path.join(runDir, 'node_modules'))
  fs.symlinkSync(path.join(engineDir, 'test'), path.join(runDir, 'test'))
  return { runDir, cleanup: () => fs.rmSync(runDir, { recursive: true, force: true }) }
}

const pidAlive = (pid) => {
  try { process.kill(pid, 0); return true } catch (e) { return e.code === 'EPERM' }
}

// Removes the run dirs of dead pids (a SIGKILLed run leaves its copy in RAM); only <root>/mc-test-run-<digits>, never a live pid.
export const sweepStale = (root = '/tmp', alive = pidAlive) => {
  const removed = []
  for (const name of fs.readdirSync(root)) {
    const m = /^mc-test-run-(\d+)$/.exec(name)
    if (!m || alive(Number(m[1]))) continue
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

// shadow.test.node.js registers every test namespace in one huge fromArrays([ns symbols],[test vars]) call; keeps only the namespaces in keep.
const narrowRegistry = (js, keep) => {
  const call = 'PersistentHashMap.fromArrays(['
  const at = js.indexOf(call)
  if (at < 0) return null
  const keysAt = at + call.length - 1
  const keys = arrayElements(js, keysAt)
  if (js.slice(keys.end, keys.end + 2) !== ',[') return null
  const vals = arrayElements(js, keys.end + 1)
  if (keys.elements.length !== vals.elements.length) return null
  const pick = (els) => els.filter((_, n) => keep.has(/"([^"]+)"/.exec(keys.elements[n])[1].replaceAll('-', '_')))
  return js.slice(0, keysAt) + `[${pick(keys.elements).join(',')}],[${pick(vals.elements).join(',')}]` + js.slice(vals.end)
}

// A targeted run loads only the requested -test namespaces (and the -test namespaces they require): drops the other SHADOW_IMPORT lines
// and test-registry entries from the private bundle (2.2 s of the 3.6 s load for all 412). Returns how many imports it dropped (0 = untouched).
export const narrowBundle = (runDir, nss) => {
  const bundle = path.join(runDir, 'out/test.cjs')
  const runtime = path.join(runDir, 'out/test/cljs-runtime')
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
  if (!fs.existsSync(nodeJs)) return 0
  const narrowed = narrowRegistry(fs.readFileSync(nodeJs, 'utf8'), keep)
  if (narrowed === null) return 0
  fs.writeFileSync(nodeJs, narrowed)
  let dropped = 0
  const lines = fs.readFileSync(bundle, 'utf8').split('\n').filter((l) => {
    const m = /^SHADOW_IMPORT\("(.+_test)\.js"\);$/.exec(l)
    if (!m || keep.has(m[1])) return true
    dropped++
    return false
  })
  fs.writeFileSync(bundle, lines.join('\n'))
  return dropped
}

// Requested namespaces the :test bundle does not contain (e.g. the opt-in -golden ones, built by --golden): shadow runs 0 tests and exits 0.
export const missingNss = (engineDir, nss) =>
  nss.filter((n) => !fs.existsSync(path.join(engineDir, 'out/test/cljs-runtime', `${n.replaceAll('-', '_')}.js`)))

const readJson = (f, d) => fs.existsSync(f) ? JSON.parse(fs.readFileSync(f, 'utf8')) : d

const main = async () => {
  const nss = parseNss(process.argv.slice(2))
  if (!nss.length) { console.error('usage: tools/test-run.mjs <ns>...'); process.exit(2) }
  const missing = missingNss(engine, nss)
  if (missing.length) {
    console.error(`test-engine: not in the :test bundle (a run would pass with 0 tests): ${missing.join(' ')}${missing.some((n) => n.endsWith('-golden')) ? ' (the -golden ones run with tools/test-engine --golden)' : ''}`)
    process.exit(2)
  }
  const res = readJson(path.join(tools, 'res-slot.json'), null)
  const expected = expectedMs(nss, readJson(path.join(engine, 'out/test-ns-ms.json'), {}))
  const limit = Number(process.env.MC_TEST_TIMEOUT_S) || runTimeoutS(expected)
  sweepStale()
  const { runDir, cleanup } = isolate(engine, process.pid)
  let child = null
  cleanupOnExit(process, cleanup, (sig) => child && killTree(child.pid, sig))
  narrowBundle(runDir, nss)
  const timings = path.join(runDir, 'timings.jsonl')
  fs.writeFileSync(timings, '')
  child = spawn(path.join(tools, 'res-slot'),
    ['tests', '--need', String(needMb(nss.length, res.kinds.tests)), '--',
      'timeout', '-k', '10', String(limit), 'node', '--max-old-space-size=4096', path.join(runDir, 'out/test.cjs'), `--test=${nss.join(',')}`],
    { cwd: engine, stdio: 'inherit', env: { ...process.env, MC_TEST_TIMINGS: timings, NODE_PATH: path.join(repo, 'node_modules') } })
  const code = await new Promise((resolve) => child.on('exit', (status) => resolve(status ?? 1)))
  if (code === 124 || code === 137)
    console.error(`test-engine: TIMEOUT, killed after ${limit} s (prior timing ~${Math.round(expected / 1000)} s); last finished test: ${lastFinished(fs.readFileSync(timings, 'utf8').split('\n')) ?? 'none'}`)
  process.exit(code)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main()
