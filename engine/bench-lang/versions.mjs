// The planner versions the bench times: the ClojureScript planner from the dev build (compiled the way the engine
// ships: `shadow-cljs compile`) and from the :advanced build (`shadow-cljs release`).
//   cd engine && npx shadow-cljs compile planner-bench && npx shadow-cljs release planner-bench-release
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'
import { fileURLToPath } from 'node:url'
import { defaultStateTable } from '../js/path/blocks.mjs'
import * as space from '../js/path/space.mjs'

const require = createRequire(import.meta.url)
const OUT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../out')
const BUILDS = { dev: 'planner-bench.cjs', adv: 'planner-bench-release.cjs' }

const loadBuild = file => {
  const full = path.join(OUT, file)
  if (!fs.existsSync(full)) throw new Error(`missing ${full}: build it first (see the header of versions.mjs)`)
  return require(full)
}

// what two planners must agree on: everything but the timings
export const view = ({ status, reason, expanded, path, oneWay, stats: { maskMs, ...stats } }) => ({ status, reason, expanded, path, oneWay, stats })

// [{ name, plan(snapshot, query, extraOptions), createSearch(snapshot, query, extraOptions), view(result) }]: the views of two
// versions' results must be deeply equal
export function loadVersions () {
  const table = defaultStateTable() // one table for every version: the snapshot's section flags are cached per table
  const options = { table, space }
  const cljs = Object.entries(BUILDS).map(([mode, file]) => {
    const build = loadBuild(file)
    return {
      name: `cljs-tuned-${mode}`,
      plan: (snapshot, query, extra) => build.planTuned(snapshot, query, { ...options, ...extra }),
      createSearch: (snapshot, query, extra) => build.createSearchTuned(snapshot, query, { ...options, ...extra }),
      view
    }
  })
  return cljs
}
