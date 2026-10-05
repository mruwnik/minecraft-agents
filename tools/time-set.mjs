#!/usr/bin/env node
// Why JavaScript: thin entry point; the lock and the command are cljs (dashboard/src/world_test/runner.cljs), run from the world-test bundle.
// A manual `time set` for live testers, under the same phase-shared time lock (/tmp/mc-time-lock/) the world-test runner holds for time-dependent cases
// (waits, saying who holds it, while holders of the other phase exist; dead holders are reclaimed).
//   node tools/time-set.mjs <ticks|day|noon|night|midnight>
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

const bundle = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
process.env.WORLD_TEST_REPO = path.join(import.meta.dirname, '..')
process.exitCode = await createRequire(import.meta.url)(bundle).timeSet(process.argv.slice(2))
