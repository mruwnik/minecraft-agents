#!/usr/bin/env node
// Why JavaScript: thin entry point; the runner is cljs (dashboard/src/world_test/runner.cljs), run from the
// ahead-of-time compiled bundle dashboard/out/world-test.cjs (build it with: tools/compile dashboard world-test).
//   node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--repeat N] [--body NAME] [--world W] [--first-plot I] [--card ID] [--results FILE] [--list] [--allow-time --time-log F]
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

const bundle = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
process.env.WORLD_TEST_REPO = path.join(import.meta.dirname, '..')
process.exitCode = await createRequire(import.meta.url)(bundle).main(process.argv.slice(2))
