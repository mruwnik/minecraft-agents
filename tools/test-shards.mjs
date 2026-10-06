#!/usr/bin/env node
// Why JavaScript: a thin Node launcher (spawns the compiled test runner in shards, takes a machine-wide flock slot per shard); no engine behaviour.
// Usage: tools/test-engine --full [--part i/N] [--shards N] [--slots M] [--slowest K]
//  --part i/N: runs only part i (1-based) of N: the i-th, i+N-th... namespace in sorted name order (a function of the names alone, so parts never overlap or leave a gap); timings are merged into test-ns-ms.json, the full-run timings file is left alone.
//  Splits the engine test namespaces over N node processes (default 4), balanced by the per-namespace ms of the previous run (engine/out/test-ns-ms.json).
//  At most M shard processes run at once machine-wide (default: (MemAvailable - 6 GB) / 2.95 GB (kinds.tests.needMb), 1..shardMax): each takes res-slot's 'tests' slot tests.<i> (i < M; slots above shardMax stay for targeted runs), so parallel agents cannot OOM the machine.
//  Each shard is killed after runTimeoutS of its prior timing (tools/test-run.mjs), so a hung test frees its slot; the failure names the last finished test.
//  TEST_EVENTS=1: prints the live-tests @@test lines (phase, plan, result per test, progress) as the shards produce them (engine.timing-test emits them).
//  Per-test timings: engine/out/test-timings.jsonl (one {"var","ms"} line per test, {"peak-rss-kb"} per shard); the K slowest are printed.
//  Isolation: after the compile, out/test.cjs and out/test/cljs-runtime are copied to /tmp/mc-test-run-<pid>/out (engine/test and node_modules symlinked beside it) and the shards run that copy (a concurrent compile cannot swap it); per-shard files carry the pid; a failing shard's output is kept in /tmp/mc-test-run-<pid>-shard-<i>.log (path printed).
import fs from 'node:fs'
import path from 'node:path'
import { spawn, spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { slotArgs, logRun, slotDir } from './res-slot.mjs'
import { expectedMs, runTimeoutS, lastFinished, isolate, sweepStale, cleanupOnExit, killTree } from './test-run.mjs'

const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const engine = path.join(repo, 'engine')

// The failing shard's output for stdout: its tail only (the whole text is in the kept log).
export const failureDump = (out, max = 20000) => out.length <= max ? out : `...(cut, ${out.length - max} chars before)\n${out.slice(-max)}`

const walk = (dir) => fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => e.isDirectory() ? walk(path.join(dir, e.name)) : [path.join(dir, e.name)])

export const testNamespaces = (root = path.join(engine, 'test')) =>
  walk(root).filter((f) => /\.clj[cs]$/.test(f))
    .map((f) => fs.readFileSync(f, 'utf8').match(/^\(ns\s+(?:\^\S+\s+)*([^\s()]+-test)[\s)]/m)?.[1])
    .filter(Boolean).sort()

export const parsePart = (spec) => {
  const m = /^(\d+)\/(\d+)$/.exec(spec ?? '')
  const i = m && Number(m[1]), n = m && Number(m[2])
  if (!m || n < 1 || i < 1 || i > n) throw new Error(`--part wants i/N with 1 <= i <= N, got ${spec}`)
  return { i, n }
}
export const partOf = (nss, i, n) => [...nss].sort().filter((_, k) => k % n === i - 1)

// Greedy longest-first onto the lightest shard. Unknown ns get the mean known cost (or 1).
export const splitShards = (nss, ms, n) => {
  const known = nss.map((x) => ms[x]).filter((x) => x != null)
  const mean = known.length ? known.reduce((a, b) => a + b, 0) / known.length : 1
  const cost = (x) => ms[x] ?? mean
  const shards = Array.from({ length: Math.max(1, Math.min(n, nss.length)) }, () => ({ load: 0, nss: [] }))
  for (const x of [...nss].sort((a, b) => cost(b) - cost(a) || a.localeCompare(b))) {
    const s = shards.reduce((a, b) => (b.load < a.load ? b : a))
    s.nss.push(x); s.load += cost(x)
  }
  return shards.map((s) => s.nss)
}

const parse = (lines) => lines.filter(Boolean).map((l) => JSON.parse(l))
export const slowest = (lines, k) => parse(lines).filter((r) => r.var).sort((a, b) => b.ms - a.ms).slice(0, k)
// A shard that exits 0 but logged no test (crash before the first test, empty namespace list) is a failure, not a pass.
export const shardOutcome = (code, lines) =>
  code !== 0 ? { ok: false, why: `exit ${code}` } : parse(lines).some((r) => r.var) ? { ok: true } : { ok: false, why: 'exit 0 but no test was logged' }
export const nsMs = (lines) => {
  const out = {}
  for (const r of parse(lines)) if (r.var) { const ns = r.var.replace(/^#'/, '').split('/')[0]; out[ns] = (out[ns] ?? 0) + r.ms }
  return out
}

const RES = JSON.parse(fs.readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), 'res-slot.json'), 'utf8'))
const FLOOR_MB = RES.floorMb, SHARD_MB = RES.kinds.tests.needMb // shared with tools/res-slot: floor kept for others; worst shard peak seen
export const memSlots = (availableMb, max = RES.kinds.tests.shardMax) => Math.max(1, Math.min(max, Math.floor((availableMb - FLOOR_MB) / SHARD_MB)))
const availableMb = () => Number(fs.readFileSync('/proc/meminfo', 'utf8').match(/MemAvailable:\s+(\d+)/)[1]) / 1024

// Live @@test lines from the shards (stdout is otherwise kept until the shard ends). shardNs = namespace count per shard, known before any shard starts:
// result and phase pass through; one plan (summed once every shard has reported its own); one progress over all shards' namespaces, 0 done from start().
export const eventForwarder = (write, shardNs) => {
  const partial = {}, done = {}, plans = {}
  const emit = (e) => write(`@@test ${JSON.stringify(e)}`)
  const progress = (unit) => emit({ event: 'progress', done: Object.values(done).reduce((t, d) => t + d, 0), total: shardNs.reduce((t, n) => t + n, 0), unit })
  const feedLine = (i, l) => {
    const at = l.indexOf('@@test ')
    if (at < 0) return
    let e
    try { e = JSON.parse(l.slice(at + 7)) } catch { return }
    if (e.event === 'plan') {
      plans[i] = e.total
      if (Object.keys(plans).length === shardNs.length) emit({ event: 'plan', total: Object.values(plans).reduce((t, n) => t + n, 0) })
      return
    }
    if (e.event !== 'progress') return write(l.slice(at))
    done[i] = e.done
    progress(e.unit)
  }
  return {
    start: () => progress('namespaces'),
    waiting: (i) => emit({ event: 'phase', name: `waiting for slot (shard ${i + 1}/${shardNs.length})` }),
    feed: (i, chunk) => {
      const lines = ((partial[i] ?? '') + chunk).split('\n')
      partial[i] = lines.pop()
      lines.forEach((l) => feedLine(i, l))
    },
    end: (i) => { feedLine(i, partial[i] ?? ''); partial[i] = '' },
  }
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms))

// Runs cmd under the first free slot; polls until one is free.
const runInSlot = async (slots, cmd, opts, onOut = () => {}, onWait = () => {}) => {
  const t0 = Date.now()
  for (let first = true; ; first = false) for (let i = 0; i < slots; i++) {
    const t1 = Date.now()
    const r = await new Promise((res) => {
      let out = ''
      const p = spawn('flock', slotArgs('tests', i, cmd, 99), { ...opts, stdio: ['ignore', 'pipe', 'pipe'] })
      p.stdout.on('data', (d) => { out += d; onOut(String(d)) }); p.stderr.on('data', (d) => { out += d })
      p.on('close', (code) => res({ code, out }))
    })
    if (r.code !== 99) {
      logRun({ kind: 'tests', needMb: SHARD_MB, waitedS: Math.round((t1 - t0) / 1000), ranS: Math.round((Date.now() - t1) / 1000), code: r.code, cmd: cmd.join(' ') })
      return r
    }
    if (first && i === slots - 1) onWait()
    await sleep(1000 + Math.random() * 500)
  }
}

const main = async ({ compile = () => spawnSync(path.join(repo, 'tools/compile'), ['engine', 'test'], { stdio: 'inherit' }) } = {}) => {
  const argv = process.argv.slice(2)
  const opt = (name, d) => { const i = argv.indexOf(name); return i < 0 ? d : Number(argv[i + 1]) }
  const shards = opt('--shards', 4), slots = opt('--slots', memSlots(availableMb())), top = opt('--slowest', 15)
  fs.mkdirSync(slotDir(), { recursive: true })
  const nsFile = path.join(engine, 'out/test-ns-ms.json'), timingFile = path.join(engine, 'out/test-timings.jsonl')
  const prior = fs.existsSync(nsFile) ? JSON.parse(fs.readFileSync(nsFile, 'utf8')) : {}
  const partIdx = argv.indexOf('--part')
  const part = partIdx < 0 ? null : parsePart(argv[partIdx + 1])
  const all = testNamespaces()
  const split = splitShards(part ? partOf(all, part.i, part.n) : all, prior, shards)
  const events = process.env.TEST_EVENTS === '1' ? eventForwarder((l) => console.log(l), split.map((nss) => nss.length)) : null
  if (events) console.log('@@test {"event":"phase","name":"compiling"}')
  const c = compile()
  if (c.status !== 0) process.exit(c.status ?? 1)
  sweepStale()
  const { runDir, cleanup } = isolate(engine, process.pid)
  cleanupOnExit(process, cleanup, (sig) => killTree(process.pid, sig, false))
  const t0 = Date.now()
  if (events) { console.log('@@test {"event":"phase","name":"testing"}'); events.start() }
  console.log(`test-shards: ${split.length} shards, at most ${slots} at once machine-wide`)
  const results = await Promise.all(split.map(async (nss, i) => {
    const file = `${timingFile}.${process.pid}.${i}`
    fs.writeFileSync(file, '')
    const limit = Number(process.env.MC_TEST_TIMEOUT_S) || runTimeoutS(expectedMs(nss, prior))
    const r = await runInSlot(slots, ['timeout', '-k', '10', String(limit), 'node', '--max-old-space-size=4096', path.join(runDir, 'out/test.cjs'), `--test=${nss.join(',')}`], { cwd: engine, env: { ...process.env, MC_TEST_TIMINGS: file, NODE_PATH: path.join(repo, 'node_modules') } }, events ? (chunk) => events.feed(i, chunk) : undefined, events ? () => events.waiting(i) : undefined)
    events?.end(i)
    if (r.code === 124 || r.code === 137) r.out += `\ntest-shards: TIMEOUT, shard ${i} killed after ${limit} s; last finished test: ${lastFinished(fs.readFileSync(file, 'utf8').split('\n')) ?? 'none'}\n`
    return { i, nss, file, ...r }
  }))
  let bad = 0
  const lines = []
  for (const r of results) {
    const mine = fs.readFileSync(r.file, 'utf8').split('\n'); fs.unlinkSync(r.file)
    lines.push(...mine)
    const o = shardOutcome(r.code, mine)
    if (o.ok) { console.log(`shard ${r.i}: ok (${r.nss.length} ns)`); continue }
    const log = `/tmp/mc-test-run-${process.pid}-shard-${r.i}.log`
    fs.writeFileSync(log, r.out)
    bad++; console.log(`--- shard ${r.i} FAILED (${o.why}), output kept in ${log} ---\n${failureDump(r.out)}`)
  }
  cleanup()
  if (!part) fs.writeFileSync(timingFile, lines.filter(Boolean).join('\n') + '\n')
  if (bad === 0 && parse(lines).some((r) => r.var)) fs.writeFileSync(nsFile, JSON.stringify({ ...(part ? prior : {}), ...nsMs(lines) }))
  const peaks = parse(lines).filter((r) => r['peak-rss-kb']).map((r) => r['peak-rss-kb'])
  const tests = parse(lines).filter((r) => r.var).length
  console.log(`test-shards: ${tests} tests, wall ${((Date.now() - t0) / 1000).toFixed(0)} s, shard peak RSS MB: ${peaks.map((k) => Math.round(k / 1024)).join(' ')} (sum ${Math.round(peaks.reduce((a, b) => a + b, 0) / 1024)})`)
  console.log(`slowest tests (full list: ${timingFile}):`)
  for (const r of slowest(lines, top)) console.log(`  ${String(r.ms).padStart(6)} ms  ${r.var}`)
  process.exitCode = bad ? 1 : 0
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main()

export { main }
