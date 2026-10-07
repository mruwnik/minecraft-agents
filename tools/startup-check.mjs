#!/usr/bin/env node
// Why JavaScript: thin Node launcher that spawns the generated tools and times them; the tools it measures are the compiled cljs ones.
// Startup report for the generated agent tools (AGENTS.md: about 500 ms). A report, not a gate: always exits 0.
//   node tools/startup-check.mjs [--runs N] [--budget MS] [command...]
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const repo = fileURLToPath(new URL('../', import.meta.url))
const argv = process.argv.slice(2)
const option = (name, fallback) => {
  const i = argv.indexOf(name)
  if (i < 0) return fallback
  return Number(argv.splice(i, 2)[1])
}
const runs = option('--runs', 5)
const budget = option('--budget', 500)
const commands = argv.length ? argv : ['observe', 'jobs', 'triggers', 'say', 'entities', 'drive', 'world', 'map', 'plans', 'blueprints', 'world-changes', 'time', 'snapshot']

const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'startup-check-'))
try {
  const workspace = path.join(dir, 'workspace')
  const made = spawnSync(process.execPath, [path.join(repo, 'engine/tools/workspace.mjs'), workspace, '--body', 'B', '--world', 'w', '--worlds', path.join(dir, 'worlds')], { encoding: 'utf8', cwd: dir })
  if (made.status !== 0) {
    console.log('could not generate a workspace:', made.stderr)
  } else {
    const median = xs => xs.sort((a, b) => a - b)[Math.floor(xs.length / 2)]
    console.log(`startup of ./bin/<tool> --help, median of ${runs} (budget ${budget} ms)`)
    for (const command of commands) {
      const samples = []
      let failed = false
      for (let i = 0; i < runs; i++) {
        const start = performance.now()
        const result = spawnSync(path.join(workspace, 'bin', command), ['--help'], { cwd: '/', encoding: 'utf8' })
        samples.push(performance.now() - start)
        failed ||= result.status !== 0
      }
      const m = median(samples)
      const flag = failed ? 'FAILED TO RUN' : m > budget ? 'OVER BUDGET' : ''
      console.log(`${command.padEnd(14)} ${m.toFixed(0).padStart(5)} ms  (${samples[0].toFixed(0)}-${samples.at(-1).toFixed(0)})  ${flag}`.trimEnd())
    }
  }
} finally {
  fs.rmSync(dir, { recursive: true, force: true })
}
