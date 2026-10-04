// The rules that keep the planner fast, checked on the compiled file (see the ns docstring of planner_tuned.cljs): no truthiness
// check in the search. The one allowed is in the cold count-steps. Build first: cd engine && npx shadow-cljs compile planner-bench
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const COMPILED = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../out/planner-bench/cljs-runtime/engine.path.planner_tuned.js')

test('the compiled planner has at most one cljs.core.truth_', () => {
  const source = fs.readFileSync(COMPILED, 'utf8')
  assert.ok((source.match(/truth_/g) ?? []).length <= 1)
})
