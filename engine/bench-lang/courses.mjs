// The benchmark set of the language comparison: the frozen world queries (bench.mjs freeze) and the live tester's courses.
// The frozen world is read from PLANNER_BENCH_DIR (default: state/bench/pathfinding/claude-1 under the repo root).
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createSnapshot, loadRecordedWorld } from '../js/path/snapshot.mjs'
import { courseNames, courseSnapshot } from '../js/path/courses.mjs'

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..')

export const benchDir = () => process.env.PLANNER_BENCH_DIR ?? path.join(ROOT, 'state/bench/pathfinding/claude-1')

const worldCourses = dir => {
  const queries = path.join(dir, 'queries.json')
  if (!fs.existsSync(queries)) throw new Error(`no frozen bench at ${dir}: set PLANNER_BENCH_DIR`)
  const snapshot = createSnapshot({})
  loadRecordedWorld(snapshot, path.join(dir, 'chunks'))
  return JSON.parse(fs.readFileSync(queries, 'utf8'))
    .map(({ id, from, goal }) => ({ id, group: 'world', snapshot, query: { from, goal } }))
}

const laneCourses = () => courseNames().map(name => {
  const { snapshot, from, goal } = courseSnapshot(name)
  return { id: `course-${name}`, group: 'course', snapshot, query: { from, goal } }
})

// [{ id, group: 'world' | 'course', snapshot, query: { from, goal } }]
export const loadCourses = (dir = benchDir()) => [...worldCourses(dir), ...laneCourses()]
