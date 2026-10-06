// Why JavaScript: node --test guard over the repo's build config (tools/*.test.mjs is the tools suite's runner).
// One shadow-cljs server per checkout hosts the engine and dashboard builds (dashboard/shadow-cljs.edn).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const edn = fs.readFileSync(path.join(repo, 'dashboard/shadow-cljs.edn'), 'utf8')

const build = (id) => {
  const m = edn.match(new RegExp(`\\n(?: \\{|  ):${id} \\{[\\s\\S]*?(?=\\n  ;;|\\n  :[a-z-]+ \\{|$)`))
  assert.ok(m, `build :${id} is in dashboard/shadow-cljs.edn`)
  return m[0]
}
const regexp = (id) => {
  const m = build(id).match(/:ns-regexp "((?:[^"\\]|\\.)*)"/)
  assert.ok(m, `build :${id} has an :ns-regexp`)
  return new RegExp(JSON.parse(`"${m[1]}"`))
}
const testNamespaces = (dir) => {
  const out = []
  const walk = (d) => fs.readdirSync(d, { withFileTypes: true }).forEach((e) => {
    const p = path.join(d, e.name)
    if (e.isDirectory()) return walk(p)
    const m = /\.clj[cs]?$/.test(e.name) && fs.readFileSync(p, 'utf8').match(/^\(ns ([\w.-]+-test)\b/m)
    if (m) out.push(m[1])
  })
  walk(path.join(repo, dir))
  return out
}

test('the engine project has no shadow-cljs.edn of its own', () => {
  assert.equal(fs.existsSync(path.join(repo, 'engine/shadow-cljs.edn')), false)
})

test('engine builds output under ../engine/out', () => {
  for (const id of ['body', 'test', 'test-golden', 'planner-record', 'planner-bench', 'search-bench', 'goto-bench', 'planner-bench-release'])
    assert.match(build(id), /:output-to "\.\.\/engine\/out\//, id)
})

test('engine sources come before engine tests on the source path', () => {
  const paths = edn.match(/:source-paths \[([^\]]*)\]/)[1]
  assert.ok(paths.indexOf('"../engine/src"') >= 0)
  assert.ok(paths.indexOf('"../engine/src"') < paths.indexOf('"../engine/test"'))
})

test('the :test build takes exactly the engine test namespaces, :dashboard-test the dashboard ones', () => {
  const engine = regexp('test'), dash = regexp('dashboard-test')
  const e = testNamespaces('engine/test'), d = testNamespaces('dashboard/test')
  assert.ok(e.length > 100 && d.length > 50)
  assert.deepEqual(e.filter((n) => !engine.test(n)), [])
  assert.deepEqual(e.filter((n) => dash.test(n)), [])
  assert.deepEqual(d.filter((n) => !dash.test(n)), [])
  assert.deepEqual(d.filter((n) => engine.test(n)), [])
})
