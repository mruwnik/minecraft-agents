#!/usr/bin/env node
// Why JavaScript: thin entry point; the trial tool is cljs (dashboard/src/world_test/kit_trial.cljs), run from the world-test bundle.
// Repeatable fresh-body iron-kit trials: `node tools/kit-trial.mjs` prints the usage.
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

const bundle = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
process.env.WORLD_TEST_REPO = path.join(import.meta.dirname, '..')
process.exitCode = await createRequire(import.meta.url)(bundle).kitTrial(process.argv.slice(2))
