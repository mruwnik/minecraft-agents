// The rules that keep the planner fast, checked on the compiled files (engine.path.planner-tuned and engine.path.planner.*; see the ns docstring of planner_tuned.cljs): no truthiness
// check in the search. The one allowed is in the cold count-steps. Build first: tools/compile engine planner-bench
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const RUNTIME = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../out/planner-bench/cljs-runtime')
const COMPILED = fs.readdirSync(RUNTIME).filter((f) => /^engine\.path\.planner(_tuned|\.\w+)\.js$/.test(f))

test('the compiled planner has at most one cljs.core.truth_', () => {
  const source = COMPILED.map((f) => fs.readFileSync(path.join(RUNTIME, f), 'utf8')).join('\n')
  assert.ok((source.match(/truth_/g) ?? []).length <= 1)
})
