// Why JavaScript: bench harness over binary chunk files; the planning it times is the ClojureScript one
// (out/goto-bench.cjs, `npx shadow-cljs compile goto-bench`).
// The go-to bench: go-to's rounds of planning over the planner bench's courses (bench-lang/courses.mjs), the body moved
// to the end of each walked plan. Per course: the answer (arrived, blocked, or the no-walk reason), the rounds, and each
// round's planning ms and planner searches. The summary gives max and p95 of the round ms and of the single searches.
//   cd engine && node bench-lang/goto.mjs [--build out/goto-bench.cjs] [--rounds 3] [--out results.json]
// --rounds runs every course that many times and keeps each course's fastest run (machine noise only adds time).
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'
import { fileURLToPath, pathToFileURL } from 'node:url'
import { defaultStateTable } from '../js/path/blocks.mjs'
import * as space from '../js/path/space.mjs'
import { loadCourses } from './courses.mjs'

const require = createRequire(import.meta.url)
const HERE = path.dirname(fileURLToPath(import.meta.url))

const flagValue = (argv, flag) => argv.includes(flag) ? argv[argv.indexOf(flag) + 1] : undefined

export const quantile = (xs, q) => {
  if (xs.length === 0) return 0
  const sorted = [...xs].sort((a, b) => a - b)
  return sorted[Math.min(sorted.length - 1, Math.ceil(q * sorted.length) - 1)]
}

// one course's fastest run: [{answer, rounds: [{ms, searches, status}]}] -> the run whose slowest round is least
const fastest = runs => runs.reduce((a, b) => (Math.max(...b.rounds.map(r => r.ms), 0) < Math.max(...a.rounds.map(r => r.ms), 0) ? b : a))

export function summarize (rows) {
  const rounds = rows.flatMap(r => r.rounds.map(x => x.ms))
  const searches = rows.flatMap(r => r.rounds.flatMap(x => x.searches))
  return {
    courses: rows.length,
    rounds: rounds.length,
    roundMax: Math.max(...rounds),
    roundP95: quantile(rounds, 0.95),
    roundsOver100: rounds.filter(ms => ms > 100).length,
    searches: searches.length,
    searchMax: Math.max(...searches),
    searchP95: quantile(searches, 0.95),
    searchesOver100: searches.filter(ms => ms > 100).length,
    answers: rows.reduce((acc, r) => ({ ...acc, [r.answer]: (acc[r.answer] ?? 0) + 1 }), {})
  }
}

async function main (argv) {
  const build = require(path.resolve(flagValue(argv, '--build') ?? path.join(HERE, '../out/goto-bench.cjs')))
  const repeat = Number(flagValue(argv, '--rounds') ?? 3)
  const out = flagValue(argv, '--out')
  const table = defaultStateTable()
  const rows = []
  for (const c of loadCourses()) {
    const pw = { snapshot: c.snapshot, table, space }
    const { from, goal } = c.query
    const start = { x: from.x, y: from.y, z: from.z, px: from.px ?? from.x + 0.5, pz: from.pz ?? from.z + 0.5 }
    const runs = []
    for (let k = 0; k < repeat; k++) runs.push(await build.simulate(pw, start, goal, goal.range ?? 0)) // (a promise or not)
    const run = fastest(runs)
    rows.push({ id: c.id, answer: run.answer, rounds: run.rounds.map(r => ({ ms: +r.ms.toFixed(2), searches: r.searches.map(s => +s.toFixed(2)), status: r.status })) })
  }
  const summary = summarize(rows)
  console.log(JSON.stringify(summary))
  rows.filter(r => r.rounds.some(x => x.ms > 100)).forEach(r => console.log(JSON.stringify(r)))
  if (out) fs.writeFileSync(out, JSON.stringify({ summary, rows }))
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) await main(process.argv.slice(2))
