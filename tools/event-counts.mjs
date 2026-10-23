#!/usr/bin/env node
// Why JavaScript: thin entry point; the counting tool is cljs (dashboard/src/world_test/event_counts.cljs), run from the world-test bundle.
// Recounts the measured combat priors from the trial bodies' event logs (`node tools/event-counts.mjs` prints the usage).
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

const root = process.env.EVENT_COUNTS_ROOT ?? path.join(import.meta.dirname, '..')
const bundle = path.join(root, 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
const newest = (dir) => fs.readdirSync(dir, { recursive: true, withFileTypes: true })
  .filter((e) => e.isFile() && e.name.endsWith('.cljs'))
  .reduce((t, e) => Math.max(t, fs.statSync(path.join(e.parentPath, e.name)).mtimeMs), 0)
const sources = Math.max(newest(path.join(root, 'dashboard', 'src', 'world_test')), fs.statSync(path.join(root, 'dashboard', 'shadow-cljs.edn')).mtimeMs)
if (fs.statSync(bundle).mtimeMs < sources) {
  console.error(`the world-test bundle is older than its sources (${bundle}): it would count with old code; rebuild it with: tools/compile dashboard world-test`)
  process.exit(2)
}
process.env.WORLD_TEST_REPO = fs.realpathSync(process.env.WORLD_TEST_REPO ?? path.join(import.meta.dirname, '..'))
process.exitCode = await createRequire(import.meta.url)(bundle).eventCounts(process.argv.slice(2))
