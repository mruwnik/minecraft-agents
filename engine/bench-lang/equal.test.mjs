// The ClojureScript port must return what the JS planner returns, on every course of the benchmark set, in both builds:
// a speed number for a port that fails here does not count.
//   cd engine && PLANNER_BENCH_DIR=<frozen dir> node --test bench-lang/equal.test.mjs
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { loadCourses } from './courses.mjs'
import { loadVersions } from './versions.mjs'

const courses = loadCourses()
const [js, ...ports] = loadVersions()
const expected = courses.map(c => js.plan(c.snapshot, c.query))

test('the benchmark set: 160 frozen world queries and 144 courses', () => {
  assert.deepEqual(Object.groupBy(courses, c => c.group).world.length, 160)
  assert.deepEqual(Object.groupBy(courses, c => c.group).course.length, 144)
})

test('the JS results cover found, partial and none, so agreement means something', () => {
  assert.deepEqual([...new Set(expected.map(r => r.status))].sort(), ['found', 'none', 'partial'])
  assert.ok(expected.filter(r => r.status === 'found').length > 200)
})

test('the ports under test: the dev and the advanced build', () => {
  assert.deepEqual(ports.map(p => p.name), ['cljs-tuned-dev', 'cljs-tuned-adv'])
})

for (const port of ports) {
  test(`${port.name} returns what the JS planner returns on all ${courses.length} courses`, () => {
    courses.forEach((c, k) => assert.deepStrictEqual(port.view(port.plan(c.snapshot, c.query)), js.view(expected[k]), c.id))
  })
}
