// Why JavaScript: the bench harness (bench-lang/) is Node tooling around the compiled cljs build.
// The go-to bench loads and runs one tiny course (no timing checks). Build first: tools/compile engine goto-bench
import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createRequire } from 'node:module'
import { loadCourses } from './courses.mjs'
import { runCourse } from './goto.mjs'

const require = createRequire(import.meta.url)
const build = require(path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../out/goto-bench.cjs'))

test('the go-to bench loads its courses from its own build and runs one of them', async () => {
  const courses = loadCourses(undefined, build)
  assert.ok(courses.length > 0)
  const row = await runCourse(build, build.defaultStateTable(), courses.find(c => c.group === 'course'), 1)
  assert.equal(typeof row.answer, 'string')
  assert.ok(row.rounds.length > 0)
})
