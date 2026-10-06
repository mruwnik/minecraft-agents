#!/usr/bin/env node
// Why JavaScript: thin entry point; the runner is cljs (dashboard/src/world_test/runner.cljs), run from the
// ahead-of-time compiled bundle dashboard/out/world-test.cjs (build it with: tools/compile dashboard world-test).
//   node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--repeat N] [--body NAME] [--world W] [--first-plot I] [--card ID] [--results FILE] [--list] [--allow-time --time-log F]
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'
import { spawnSync } from 'node:child_process'

const bundle = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
// One body slot for the whole run: a run keeps one probe body at a time (restarted per case), so it re-executes itself under tools/res-slot body.
const args = process.argv.slice(2)
if (!process.env.WORLD_TEST_SLOT_HELD && !args.includes('--list')) {
  const r = spawnSync(path.join(import.meta.dirname, 'res-slot'), ['body', '--', process.execPath, process.argv[1], ...args], { stdio: 'inherit', env: { ...process.env, WORLD_TEST_SLOT_HELD: '1' } })
  process.exit(r.status ?? 1)
}
process.env.WORLD_TEST_REPO = path.join(import.meta.dirname, '..')
process.exitCode = await createRequire(import.meta.url)(bundle).main(args)
