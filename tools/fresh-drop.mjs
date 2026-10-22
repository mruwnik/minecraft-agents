#!/usr/bin/env node
// Why JavaScript: thin entry point; the soak tool is cljs (dashboard/src/world_test/fresh_drop.cljs), run from the world-test bundle.
// Fresh-drop soak: drop a body, report time to base/farm/kit (`node tools/fresh-drop.mjs` prints the usage).
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

const bundle = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
process.env.WORLD_TEST_REPO = path.join(import.meta.dirname, '..')
process.exitCode = await createRequire(import.meta.url)(bundle).freshDrop(process.argv.slice(2))
