#!/usr/bin/env node
// Why JavaScript: thin entry point; the sweep driver is cljs (dashboard/src/world_test/sweep.cljs), run from the world-test bundle.
// One world sweep: passes day, night, night-exclusive of tools/world-test.mjs, then time-set day, one summary EDN. `node tools/world-sweep.mjs --help` for the flags.
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

const bundle = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
process.env.WORLD_TEST_REPO = path.join(import.meta.dirname, '..')
process.exitCode = await createRequire(import.meta.url)(bundle).sweep(process.argv.slice(2))
