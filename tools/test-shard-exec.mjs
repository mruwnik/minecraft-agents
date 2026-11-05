#!/usr/bin/env node
// Why JavaScript: a thin Node launcher that makes the private bundle copy inside the held test slot, then runs the command on it; no engine behaviour.
// Usage: test-shard-exec.mjs <engineDir> <cmd...>  ({bundle} in cmd = the copy's out/test.cjs)
// The copy is made only once the slot is held, so shards waiting for a slot cost no disk (card 28932c99).
import { spawn, spawnSync } from 'node:child_process'
import fs from 'node:fs'
import path from 'node:path'
import { isolate, cleanupOnExit, killTree, RUN_ROOT } from './test-run.mjs'

export const isolateOrExplain = (engineDir, tag, iso = isolate, err = console.error) => {
  try { return iso(engineDir, tag) } catch (e) {
    err(`test-shard-exec: cannot copy the test bundle (${e.code ?? e.message}${e.code === 'ENOSPC' ? ': disk full, free space under ~/.cache/mc-test-run and /home' : ''})`)
    return null
  }
}

// The stamp test-shards wrote after its full compile (hash line, then bundle mtime:size). A targeted compile of the same checkout rewrites the bundle (narrowed) and removes the stamp.
export const bundleStale = (engineDir) => {
  try {
    const st = fs.statSync(path.join(engineDir, 'out/test.cjs'))
    return fs.readFileSync(path.join(engineDir, 'out/test-full.stamp'), 'utf8').split('\n')[1] !== `${st.mtimeMs}:${st.size}`
  } catch { return true }
}

// Copy while holding the bundle lock shared, so a compile (exclusive) never rewrites the bundle mid-copy.
const STALE_RC = 3
export const isolateLocked = (engineDir, lock, tag = process.pid, run = spawnSync) => {
  const r = run('flock', ['-s', lock, process.execPath, new URL(import.meta.url).pathname, '--isolate', engineDir, String(tag)], { stdio: 'inherit' })
  if (r.status === STALE_RC) return 'stale'
  if (r.status !== 0) return null
  const runDir = path.join(RUN_ROOT, `mc-test-run-${tag}`)
  return { runDir, cleanup: () => fs.rmSync(runDir, { recursive: true, force: true }) }
}

// A stale bundle (rebuilt narrower by a targeted run while this part waited for its slot) is rebuilt under the exclusive lock, then copied; a bundle that keeps changing fails loudly instead of running 0 tests.
export const isolateFresh = (engineDir, lock, copy = isolateLocked, rebuild = (l) => spawnSync('flock', ['-x', l, process.execPath, path.join(path.dirname(new URL(import.meta.url).pathname), 'test-shards.mjs'), '--ensure-bundle'], { stdio: 'inherit' }), err = console.error) => {
  for (let n = 0; n < 3; n++) {
    const iso = copy(engineDir, lock)
    if (iso !== 'stale') return iso
    err('test-shard-exec: the test bundle changed while this part waited for its slot; rebuilding it')
    if (rebuild(lock).status !== 0) return null
  }
  err('test-shard-exec: the test bundle keeps changing (another run in this checkout compiles :test); giving up')
  return null
}

const main = () => {
  if (process.argv[2] === '--isolate') {
    if (bundleStale(process.argv[3])) return process.exit(STALE_RC)
    return process.exit(isolateOrExplain(process.argv[3], process.argv[4]) ? 0 : 1)
  }
  const [engineDir, ...cmd] = process.argv.slice(2)
  const lock = process.env.MC_TEST_BUNDLE_LOCK
  const iso = lock ? isolateFresh(engineDir, lock) : isolateOrExplain(engineDir, process.pid)
  if (!iso) return process.exit(1)
  let child = null
  cleanupOnExit(process, iso.cleanup, (sig) => child && killTree(child.pid, sig))
  child = spawn(cmd[0], cmd.slice(1).map((a) => a.replace('{bundle}', path.join(iso.runDir, 'out/test.cjs'))), { stdio: 'inherit' })
  child.on('exit', (code, sig) => process.exit(code ?? (sig ? 128 : 1)))
}

if (process.argv[1] === new URL(import.meta.url).pathname) main()
