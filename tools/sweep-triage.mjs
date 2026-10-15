#!/usr/bin/env node
// Why JavaScript: thin entry point; the triage is cljs (dashboard/src/world_test/triage.cljs), run from the world-test bundle.
// Sorts a sweep's results into known / new / flaky failures and tells which live cards to tick, send to pending or hold. `node tools/sweep-triage.mjs --help`.
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

const bundle = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
process.exitCode = createRequire(import.meta.url)(bundle).triage(process.argv.slice(2))
