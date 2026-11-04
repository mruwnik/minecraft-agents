#!/usr/bin/env node
// Why JavaScript: a thin Node launcher that makes the private bundle copy inside the held test slot, then runs the command on it; no engine behaviour.
// Usage: test-shard-exec.mjs <engineDir> <cmd...>  ({bundle} in cmd = the copy's out/test.cjs)
// The copy is made only once the slot is held, so shards waiting for a slot cost no disk (card 28932c99).
import { spawn } from 'node:child_process'
import path from 'node:path'
import { isolate, cleanupOnExit, killTree } from './test-run.mjs'

export const isolateOrExplain = (engineDir, tag, iso = isolate, err = console.error) => {
  try { return iso(engineDir, tag) } catch (e) {
    err(`test-shard-exec: cannot copy the test bundle (${e.code ?? e.message}${e.code === 'ENOSPC' ? ': disk full, free space under ~/.cache/mc-test-run and /home' : ''})`)
    return null
  }
}

const main = () => {
  const [engineDir, ...cmd] = process.argv.slice(2)
  const iso = isolateOrExplain(engineDir, process.pid)
  if (!iso) return process.exit(1)
  let child = null
  cleanupOnExit(process, iso.cleanup, (sig) => child && killTree(child.pid, sig))
  child = spawn(cmd[0], cmd.slice(1).map((a) => a.replace('{bundle}', path.join(iso.runDir, 'out/test.cjs'))), { stdio: 'inherit' })
  child.on('exit', (code, sig) => process.exit(code ?? (sig ? 128 : 1)))
}

if (process.argv[1] === new URL(import.meta.url).pathname) main()
