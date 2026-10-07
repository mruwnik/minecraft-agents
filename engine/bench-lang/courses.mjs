// The benchmark set of the language comparison: the frozen world queries (bench.mjs freeze) and the live tester's courses.
// The frozen world is read from PLANNER_BENCH_DIR (default: engine/test/fixtures/pathfinding/claude-1 under the repo root).
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createRequire } from 'node:module'
import { createSnapshot, loadRecordedWorld } from '../js/path/snapshot.mjs'

const require = createRequire(import.meta.url)
const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..')

export const benchDir = () => process.env.PLANNER_BENCH_DIR ?? path.join(ROOT, 'engine/test/fixtures/pathfinding/claude-1')

const worldCourses = dir => {
  const queries = path.join(dir, 'queries.json')
  if (!fs.existsSync(queries)) throw new Error(`no frozen bench at ${dir}: set PLANNER_BENCH_DIR`)
  const snapshot = createSnapshot({})
  loadRecordedWorld(snapshot, path.join(dir, 'chunks'))
  return JSON.parse(fs.readFileSync(queries, 'utf8'))
    .map(({ id, from, goal }) => ({ id, group: 'world', snapshot, query: { from, goal } }))
}

// the live tester's courses come from the cljs fixtures (engine.bench-courses in the planner-bench build); run from engine/
const laneCourses = () => {
  const build = require(path.join(ROOT, 'engine/out/planner-bench.cjs'))
  return build.courseNames().map(name => {
    const { snapshot, from, goal } = build.courseSnapshot(name)
    return { id: `course-${name}`, group: 'course', snapshot, query: { from, goal } }
  })
}

// [{ id, group: 'world' | 'course', snapshot, query: { from, goal } }]
export const loadCourses = (dir = benchDir()) => [...worldCourses(dir), ...laneCourses()]
