// Why JavaScript: node --test file for tools/changed-tests.mjs (a Node tool, like the other tools/*.mjs).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { selectTests, fanout, suiteArgs, rangeOf, parseGreen, formatGreen } from './changed-tests.mjs'

const ns = (name, requires = '', extra = '') => `(ns ${name}\n  "doc (:require [fake.x])"\n  (:require ${requires}) ; [commented.out]\n  )\n${extra}`
// Fake tree: a -> b -> c (src chain); fake is shared test infra; jobs.k only named by a quoted symbol.
const tree = () => ({
  'engine/src/lib/c.cljs': ns('lib.c'),
  'engine/src/lib/b_two.cljs': ns('lib.b-two', '[lib.c :as c]'),
  'engine/src/lib/a.cljs': ns('lib.a', '[lib.b-two :as b]\n [clojure.string :as s]'),
  'engine/src/lib/solo.cljs': ns('lib.solo'),
  'engine/src/jobs/k/leaf.cljs': ns('jobs.k.leaf'),
  'engine/test/engine/a_test.cljs': ns('engine.a-test', '[lib.a]'),
  'engine/test/engine/b_test.cljs': ns('engine.b-test', '[lib.b-two]'),
  'engine/test/engine/solo_test.cljs': ns('engine.solo-test', '[lib.solo]'),
  'engine/test/engine/job_test.cljs': ns('engine.job-test', '[engine.fake :as fake]', "(submit! 'jobs.k.leaf {})"),
  'engine/test/engine/fixture_test.cljs': ns('engine.fixture-test', '', '(load "world/room-one.edn")'),
  'engine/test/engine/registry_test.cljs': ns('engine.registry-test'),
  'engine/test/engine/job_fuzz_test.cljs': ns('engine.job-fuzz-test'),
  'engine/test/engine/kit_contract_test.cljs': ns('engine.kit-contract-test'),
  'engine/test/engine/planner_goals_golden.cljs': ns('engine.planner-goals-golden', '[lib.solo]'),
  'engine/src/triggers/hungry.cljs': ns('triggers.hungry'),
  'engine/src/jobs/hooks.edn': '{}',
  'engine/test/engine/fake.cljs': ns('engine.fake'),
  'engine/test/engine/fake_test.cljs': ns('engine.fake-test', '[engine.fake]'),
  'engine/fixtures/world/room-one.edn': '{}',
  'engine/js/m.mjs': 'export const m = 1',
  'engine/js/m.test.mjs': "import { m } from './m.mjs'",
  'engine/js/sub/n.mjs': "import { m } from '../m.mjs'",
  'engine/js/sub/n.test.mjs': "import { n } from './n.mjs'",
  'engine/js/orphan.mjs': 'export {}',
  'tools/lib/u.mjs': 'export const u = 1',
  'tools/t.mjs': "import { u } from './lib/u.mjs'",
  'tools/t.test.mjs': "import './t.mjs'",
  'tools/lib/u.test.mjs': "import './u.mjs'",
})
const sel = (changed, files = tree()) => selectTests(files, changed)

test('a changed src ns selects the tests requiring it, transitively', () => {
  assert.deepEqual(sel(['engine/src/lib/c.cljs']).cljs, ['engine.a-test', 'engine.b-test'])
})
test('a leaf selects only its dependents', () => {
  assert.deepEqual(sel(['engine/src/lib/a.cljs']).cljs, ['engine.a-test'])
  assert.deepEqual(sel(['engine/src/lib/solo.cljs']).cljs, ['engine.solo-test'])
})
test('ns names in docstrings and comments are not requires', () => {
  assert.deepEqual(sel(['engine/src/lib/solo.cljs']).cljs.includes('engine.fake-test'), false)
})
test('a test file that names a job by quoted symbol is selected', () => {
  assert.ok(sel(['engine/src/jobs/k/leaf.cljs']).cljs.includes('engine.job-test'))
})
test('a changed test ns selects itself', () => {
  assert.deepEqual(sel(['engine/test/engine/solo_test.cljs']).cljs, ['engine.solo-test'])
})
test('shared test infra selects everything that requires it', () => {
  assert.deepEqual(sel(['engine/test/engine/fake.cljs']).cljs, ['engine.fake-test', 'engine.job-test'])
})
test('a changed fixture selects tests naming it', () => {
  assert.deepEqual(sel(['engine/fixtures/world/room-one.edn']).cljs, ['engine.fixture-test'])
})
test('a changed fixture seeds test files only, not source files that mention its name, and selects the fixtures suite', () => {
  const files = tree(); files['engine/src/lib/solo.cljs'] += '\n; room-one'
  const r = sel(['engine/fixtures/world/room-one.edn'], files)
  assert.deepEqual(r.cljs, ['engine.fixture-test'])
  assert.deepEqual(suiteArgs(r).fixtures, ['engine/fixtures/world/room-one.edn'])
  assert.deepEqual(sel(['engine/src/lib/c.cljs']).fixtures, [])
})
test('a deleted source file is mapped by its path', () => {
  const files = tree(); delete files['engine/src/lib/b_two.cljs']
  assert.deepEqual(sel(['engine/src/lib/b_two.cljs'], files).cljs, ['engine.a-test', 'engine.b-test'])
})
test('build config changes run the full suite', () => {
  for (const f of ['dashboard/shadow-cljs.edn', 'engine/package.json', 'engine/deps.edn'])
    assert.ok(sel([f]).full, f)
  assert.equal(sel(['engine/src/lib/a.cljs']).full, null)
})
test('changed js selects its test and tests of its importers', () => {
  assert.deepEqual(sel(['engine/js/m.mjs']).js, ['engine/js/m.test.mjs', 'engine/js/sub/n.test.mjs'])
  assert.deepEqual(sel(['engine/js/sub/n.mjs']).js, ['engine/js/sub/n.test.mjs'])
  assert.deepEqual(sel(['engine/js/m.test.mjs']).js, ['engine/js/m.test.mjs'])
  assert.deepEqual(sel(['engine/js/orphan.mjs']).js, [])
})
test('unrelated paths select nothing', () => {
  const r = sel(['README.md', 'dashboard/src/x.cljs'])
  assert.deepEqual([r.cljs, r.js, r.full], [[], [], null])
  assert.deepEqual(r.suites, ['dashboard'])
})

test('changed tools files select the tools tests of them and of their importers', () => {
  assert.deepEqual(sel(['tools/t.mjs']).tools, ['tools/t.test.mjs'])
  assert.deepEqual(sel(['tools/lib/u.mjs']).tools, ['tools/lib/u.test.mjs', 'tools/t.test.mjs'])
  assert.deepEqual(sel(['tools/t.test.mjs']).tools, ['tools/t.test.mjs'])
  assert.deepEqual(sel(['engine/js/m.mjs']).tools, [])
})
test('fanout lists each changed file with the tests it alone selects, widest first', () => {
  assert.deepEqual(fanout(tree(), ['engine/src/lib/solo.cljs', 'engine/src/lib/c.cljs']),
    [{ file: 'engine/src/lib/c.cljs', tests: 2 }, { file: 'engine/src/lib/solo.cljs', tests: 1 }])
})
test('suiteArgs gives the run_tests args per suite', () => {
  const r = sel(['engine/src/lib/c.cljs', 'engine/js/m.mjs', 'tools/t.mjs'])
  assert.deepEqual(suiteArgs(r), {
    engine: ['engine.a-test', 'engine.b-test'],
    'engine-js': ['engine/js/m.test.mjs', 'engine/js/sub/n.test.mjs'],
    tools: ['tools/t.test.mjs'],
  })
})
test('a range a..b diffs committed files only, a bare rev includes the working tree', () => {
  assert.deepEqual(rangeOf('abc..def'), { from: 'abc', to: 'def' })
  assert.deepEqual(rangeOf('abc'), { from: 'abc', to: null })
})

test('a job or trigger file also selects the namespaces that see every job, and the fuzz suite', () => {
  const always = ['engine.job-fuzz-test', 'engine.kit-contract-test', 'engine.registry-test']
  assert.deepEqual(sel(['engine/src/jobs/k/leaf.cljs']).cljs, ['engine.job-fuzz-test', 'engine.job-test', 'engine.kit-contract-test', 'engine.registry-test'])
  assert.deepEqual(sel(['engine/src/triggers/hungry.cljs']).cljs, always)
  assert.deepEqual(sel(['engine/src/jobs/hooks.edn']).cljs, always)
  assert.deepEqual(sel(['engine/src/triggers/hungry.cljs']).suites, ['fuzz'])
  assert.deepEqual(sel(['engine/src/lib/solo.cljs']).cljs.includes('engine.registry-test'), false)
})
test('a changed dashboard file selects the dashboard suite; node_modules and out do not', () => {
  assert.deepEqual(sel(['dashboard/src/x.cljs']).suites, ['dashboard'])
  assert.deepEqual(sel(['dashboard/out/x.js', 'dashboard/node_modules/y/z.js']).suites, [])
})
test('a golden namespace reached through the require graph selects the golden suite', () => {
  assert.deepEqual(sel(['engine/src/lib/solo.cljs']).suites, ['golden'])
  assert.deepEqual(sel(['engine/src/lib/c.cljs']).suites, [])
})
test('suiteArgs lists whole-suite selections with no args', () => {
  assert.deepEqual(suiteArgs(sel(['dashboard/src/x.cljs', 'engine/src/lib/c.cljs'])), {
    engine: ['engine.a-test', 'engine.b-test'], dashboard: [],
  })
})

test('the last-green record is "<rev> <iso time>" and reads back', () => {
  const text = formatGreen('abc1234', new Date('2026-10-16T12:00:00Z'))
  assert.equal(text, 'abc1234 2026-10-16T12:00:00.000Z\n')
  assert.deepEqual(parseGreen(text), { rev: 'abc1234', time: '2026-10-16T12:00:00.000Z' })
  assert.equal(parseGreen(''), null)
  assert.equal(parseGreen('not a rev\n'), null)
})
