#!/usr/bin/env node
// Why JavaScript: a thin Node launcher (spawns the compiled test runner in shards, takes a machine-wide flock slot per shard); no engine behaviour.
// Usage: tools/test-engine --full [--shards N] [--slots M] [--slowest K]
//  Splits the engine test namespaces over N node processes (default 4), balanced by the per-namespace ms of the previous run (engine/out/test-ns-ms.json).
//  At most M shard processes run at once machine-wide (default: (MemAvailable - 6 GB) / 2.8 GB, 1..shardMax): each takes res-slot's 'tests' slot tests.<i> (i < M; slots above shardMax stay for targeted runs), so parallel agents cannot OOM the machine.
//  Each shard is killed after runTimeoutS of its prior timing (tools/test-run.mjs), so a hung test frees its slot; the failure names the last finished test.
//  Per-test timings: engine/out/test-timings.jsonl (one {"var","ms"} line per test, {"peak-rss-kb"} per shard); the K slowest are printed.
//  Isolation: after the compile, out/test.cjs and out/test/cljs-runtime are copied to /tmp/mc-test-run-<pid>/out (engine/test and node_modules symlinked beside it) and the shards run that copy (a concurrent compile cannot swap it); per-shard files carry the pid; a failing shard's output is kept in /tmp/mc-test-run-<pid>-shard-<i>.log (path printed).
import fs from 'node:fs'
import path from 'node:path'
import { spawn, spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { slotArgs, logRun, slotDir } from './res-slot.mjs'
import { expectedMs, runTimeoutS, lastFinished, isolate } from './test-run.mjs'

const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const engine = path.join(repo, 'engine')

const walk = (dir) => fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => e.isDirectory() ? walk(path.join(dir, e.name)) : [path.join(dir, e.name)])

export const testNamespaces = (root = path.join(engine, 'test')) =>
  walk(root).filter((f) => /\.clj[cs]$/.test(f))
    .map((f) => fs.readFileSync(f, 'utf8').match(/^\(ns\s+(?:\^\S+\s+)*([^\s()]+-test)[\s)]/m)?.[1])
    .filter(Boolean).sort()

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
export const nsMs = (lines) => {
  const out = {}
  for (const r of parse(lines)) if (r.var) { const ns = r.var.replace(/^#'/, '').split('/')[0]; out[ns] = (out[ns] ?? 0) + r.ms }
  return out
}

const RES = JSON.parse(fs.readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), 'res-slot.json'), 'utf8'))
const FLOOR_MB = RES.floorMb, SHARD_MB = RES.kinds.tests.needMb // shared with tools/res-slot: floor kept for others; worst shard peak seen
export const memSlots = (availableMb, max = RES.kinds.tests.shardMax) => Math.max(1, Math.min(max, Math.floor((availableMb - FLOOR_MB) / SHARD_MB)))
const availableMb = () => Number(fs.readFileSync('/proc/meminfo', 'utf8').match(/MemAvailable:\s+(\d+)/)[1]) / 1024

const sleep = (ms) => new Promise((r) => setTimeout(r, ms))

// Runs cmd under the first free slot; polls until one is free.
const runInSlot = async (slots, cmd, opts) => {
  const t0 = Date.now()
  for (;;) for (let i = 0; i < slots; i++) {
    const t1 = Date.now()
    const r = await new Promise((res) => {
      let out = ''
      const p = spawn('flock', slotArgs('tests', i, cmd, 99), { ...opts, stdio: ['ignore', 'pipe', 'pipe'] })
      p.stdout.on('data', (d) => { out += d }); p.stderr.on('data', (d) => { out += d })
      p.on('close', (code) => res({ code, out }))
    })
    if (r.code !== 99) {
      logRun({ kind: 'tests', needMb: SHARD_MB, waitedS: Math.round((t1 - t0) / 1000), ranS: Math.round((Date.now() - t1) / 1000), code: r.code, cmd: cmd.join(' ') })
      return r
    }
    await sleep(1000 + Math.random() * 500)
  }
}

const main = async () => {
  const argv = process.argv.slice(2)
  const opt = (name, d) => { const i = argv.indexOf(name); return i < 0 ? d : Number(argv[i + 1]) }
  const shards = opt('--shards', 4), slots = opt('--slots', memSlots(availableMb())), top = opt('--slowest', 15)
  fs.mkdirSync(slotDir(), { recursive: true })
  const c = spawnSync(path.join(repo, 'tools/compile'), ['engine', 'test'], { stdio: 'inherit' })
  if (c.status !== 0) process.exit(c.status ?? 1)
  const nsFile = path.join(engine, 'out/test-ns-ms.json'), timingFile = path.join(engine, 'out/test-timings.jsonl')
  const { runDir, cleanup } = isolate(engine, process.pid)
  const prior = fs.existsSync(nsFile) ? JSON.parse(fs.readFileSync(nsFile, 'utf8')) : {}
  const split = splitShards(testNamespaces(), prior, shards)
  const t0 = Date.now()
  console.log(`test-shards: ${split.length} shards, at most ${slots} at once machine-wide`)
  const results = await Promise.all(split.map(async (nss, i) => {
    const file = `${timingFile}.${process.pid}.${i}`
    fs.writeFileSync(file, '')
    const limit = runTimeoutS(expectedMs(nss, prior))
    const r = await runInSlot(slots, ['timeout', '-k', '10', String(limit), 'node', '--max-old-space-size=4096', path.join(runDir, 'out/test.cjs'), `--test=${nss.join(',')}`], { cwd: engine, env: { ...process.env, MC_TEST_TIMINGS: file, NODE_PATH: path.join(repo, 'node_modules') } })
    if (r.code === 124 || r.code === 137) r.out += `\ntest-shards: TIMEOUT, shard ${i} killed after ${limit} s; last finished test: ${lastFinished(fs.readFileSync(file, 'utf8').split('\n')) ?? 'none'}\n`
    return { i, nss, file, ...r }
  }))
  let bad = 0
  const lines = []
  for (const r of results) {
    lines.push(...fs.readFileSync(r.file, 'utf8').split('\n')); fs.unlinkSync(r.file)
    if (r.code === 0) { console.log(`shard ${r.i}: ok (${r.nss.length} ns)`); continue }
    const log = `/tmp/mc-test-run-${process.pid}-shard-${r.i}.log`
    fs.writeFileSync(log, r.out)
    bad++; console.log(`--- shard ${r.i} FAILED (exit ${r.code}), output kept in ${log} ---\n${r.out}`)
  }
  cleanup()
  fs.writeFileSync(timingFile, lines.filter(Boolean).join('\n') + '\n')
  if (bad === 0) fs.writeFileSync(nsFile, JSON.stringify(nsMs(lines)))
  const peaks = parse(lines).filter((r) => r['peak-rss-kb']).map((r) => r['peak-rss-kb'])
  const tests = parse(lines).filter((r) => r.var).length
  console.log(`test-shards: ${tests} tests, wall ${((Date.now() - t0) / 1000).toFixed(0)} s, shard peak RSS MB: ${peaks.map((k) => Math.round(k / 1024)).join(' ')} (sum ${Math.round(peaks.reduce((a, b) => a + b, 0) / 1024)})`)
  console.log(`slowest tests (full list: ${timingFile}):`)
  for (const r of slowest(lines, top)) console.log(`  ${String(r.ms).padStart(6)} ms  ${r.var}`)
  process.exit(bad ? 1 : 0)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main()
