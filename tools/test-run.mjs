#!/usr/bin/env node
// Why JavaScript: a thin Node launcher (copies the compiled test bundle, runs node under res-slot and timeout(1)); no engine behaviour.
// tools/test-run.mjs <ns>...   (called by tools/test-engine after the compile; namespaces space- or comma-separated)
//  Runs a private copy of out/test.cjs + out/test/cljs-runtime (/tmp/mc-test-run-<pid>, so a concurrent compile cannot swap it) under one
//  res-slot 'tests' slot sized by the namespace count (needMb), killed after runTimeoutS (from engine/out/test-ns-ms.json) so a hung test frees its slot (env MC_TEST_TIMEOUT_S overrides).
import fs from 'node:fs'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
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

// Measured peak RSS: 650-780 MB for one namespace, up to 2.5 GB for a full shard (~45 namespaces).
export const needMb = (nsCount, capMb) => Math.min(capMb, 900 + 60 * nsCount)

export const lastFinished = (lines) =>
  lines.filter(Boolean).map((l) => JSON.parse(l)).filter((r) => r.var).at(-1)?.var ?? null

// Private copy of the bundle; fixtures resolve as <out>/../test, npm deps through node_modules.
export const isolate = (engineDir, tag) => {
  const runDir = `/tmp/mc-test-run-${tag}`
  fs.rmSync(runDir, { recursive: true, force: true })
  fs.mkdirSync(path.join(runDir, 'out/test'), { recursive: true })
  fs.copyFileSync(path.join(engineDir, 'out/test.cjs'), path.join(runDir, 'out/test.cjs'))
  fs.cpSync(path.join(engineDir, 'out/test/cljs-runtime'), path.join(runDir, 'out/test/cljs-runtime'), { recursive: true })
  fs.symlinkSync(path.join(engineDir, 'node_modules'), path.join(runDir, 'node_modules'))
  fs.symlinkSync(path.join(engineDir, 'test'), path.join(runDir, 'test'))
  return { runDir, cleanup: () => fs.rmSync(runDir, { recursive: true, force: true }) }
}

const readJson = (f, d) => fs.existsSync(f) ? JSON.parse(fs.readFileSync(f, 'utf8')) : d

const main = () => {
  const nss = parseNss(process.argv.slice(2))
  if (!nss.length) { console.error('usage: tools/test-run.mjs <ns>...'); process.exit(2) }
  const res = readJson(path.join(tools, 'res-slot.json'), null)
  const expected = expectedMs(nss, readJson(path.join(engine, 'out/test-ns-ms.json'), {}))
  const limit = Number(process.env.MC_TEST_TIMEOUT_S) || runTimeoutS(expected)
  const { runDir, cleanup } = isolate(engine, process.pid)
  const timings = path.join(runDir, 'timings.jsonl')
  fs.writeFileSync(timings, '')
  const r = spawnSync(path.join(tools, 'res-slot'),
    ['tests', '--need', String(needMb(nss.length, res.kinds.tests.needMb)), '--',
      'timeout', '-k', '10', String(limit), 'node', '--max-old-space-size=4096', path.join(runDir, 'out/test.cjs'), `--test=${nss.join(',')}`],
    { cwd: engine, stdio: 'inherit', env: { ...process.env, MC_TEST_TIMINGS: timings, NODE_PATH: path.join(repo, 'node_modules') } })
  const code = r.status ?? 1
  if (code === 124 || code === 137)
    console.error(`test-engine: TIMEOUT, killed after ${limit} s (prior timing ~${Math.round(expected / 1000)} s); last finished test: ${lastFinished(fs.readFileSync(timings, 'utf8').split('\n')) ?? 'none'}`)
  cleanup()
  process.exit(code)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main()
