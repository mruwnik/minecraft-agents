// The widened gate for the ClojureScript port (see equal.test.mjs for the benchmark set):
//  (i) every query the js/path/planner*.test.mjs files make is also planned by the port, by running those files with
//      compare-hook.mjs: each `plan` / `createSearch` of theirs is compared with both builds, deepStrictEqual;
//  (ii) createSearch stepped in slices of 100 expansions gives the same step sequence and result as plan;
//  (iii) non-default options: weight, maxNodes (reason 'budget'), a goal in an unloaded column ('goal-unloaded'), costs, the box,
//       the goal flood.
//   cd engine && node --test bench-lang/fixtures-equal.test.mjs     (needs both builds, see versions.mjs)
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { courseSnapshot } from '../js/path/courses.mjs'
import { plan as planJs } from '../js/path/planner.mjs'
import { MOVE } from '../js/path/planner.mjs'
import { loadVersions } from './versions.mjs'

const here = path.dirname(fileURLToPath(import.meta.url))
const pathDir = path.resolve(here, '../js/path')
const hook = path.join(here, 'compare-hook.mjs')

// ---- (i) the planner test files, comparing every query ----

const plannerTests = fs.readdirSync(pathDir).filter(f => /^planner.*\.test\.mjs$/.test(f)).sort()
// (a child of `node --test` would otherwise report to its parent in a binary format instead of printing)
const { NODE_TEST_CONTEXT, ...plainEnv } = process.env
const runCompared = file => {
  const log = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'compare-')), 'log')
  const run = spawnSync(process.execPath, ['--import', hook, path.join(pathDir, file)], { env: { ...plainEnv, COMPARE_LOG: log }, encoding: 'utf8', maxBuffer: 1 << 26 })
  const lines = fs.existsSync(log) ? fs.readFileSync(log, 'utf8').split('\n').filter(Boolean) : []
  fs.rmSync(path.dirname(log), { recursive: true, force: true }) // read: never left behind in /tmp
  return { run, lines, pass: Number(/^ℹ pass (\d+)/m.exec(run.stdout)?.[1]), fail: Number(/^ℹ fail (\d+)/m.exec(run.stdout)?.[1]) }
}

const compared = Object.fromEntries(plannerTests.map(file => [file, runCompared(file)]))

test('the planner test files are the ones this gate runs', () => {
  assert.deepEqual(plannerTests, ['planner-climb.test.mjs', 'planner-courses.test.mjs', 'planner-doors.test.mjs', 'planner-partial.test.mjs', 'planner-water.test.mjs', 'planner.test.mjs'])
})

for (const file of plannerTests) {
  test(`${file}: every query agrees between the JS planner and both builds, and the file still passes`, () => {
    const { run, pass, fail } = compared[file]
    assert.equal(run.status, 0, run.stdout.split('\n').filter(l => /not ok|differs|expected|actual/.test(l)).slice(0, 20).join('\n') + run.stderr.slice(0, 2000))
    assert.equal(fail, 0)
    assert.ok(pass > 0)
  })
}

const allLines = Object.values(compared).flatMap(c => c.lines)

test('at least 60 queries were compared', () => {
  assert.ok(allLines.length >= 60, `${allLines.length} compared`)
})

test('the compared queries use every move code', () => {
  const used = new Set(allLines.flatMap(l => l.split(' ')[1].split(',')).filter(Boolean).map(Number))
  assert.deepEqual([...Object.values(MOVE)].filter(code => !used.has(code)), [])
})

test('the compared queries end in found, partial and none', () => {
  const statuses = new Set(allLines.map(l => l.split('/')[0]))
  assert.deepEqual([...statuses].sort(), ['found', 'none', 'partial'])
})

// ---- (ii) and (iii): the same query under several settings ----

const [js, ...ports] = loadVersions()
const near = (x, y, z, range = 1) => ({ kind: 'near', x, y, z, range })

const samples = [
  'door-closed', 'iron-button', 'plate-door', 'gate-airlock', 'ladder-up', 'ladder-down', 'lad-trap-closed', 'lad-gap', 'bubble-up',
  'magma-down', 'water20-up', 'waterfall-down', 'drop8-water', 'lake20', 'lake20-wade', 'cocoa-a2-both-feethead', 'fence-diag',
  'top-slabs', 'lava-walkway', 'dripleaf', 'portal', 'rand50-we', 'tunnel-corner', 'stairs-slabs', 'drop12', 'gap4', 'iron-door'
]
const queryOf = name => {
  const { snapshot, from, goal } = courseSnapshot(name)
  return { snapshot, query: { from, goal } }
}
const cases = samples.map(name => [name, queryOf(name)])

// [what, options]
const optionSets = [
  ['weight 1.2', { weight: 1.2 }],
  ['weight 2, riskWeight 0.5', { weight: 2, riskWeight: 0.5 }],
  ['maxNodes 40: budget', { maxNodes: 40 }],
  ['maxNodes 1500: one growth step', { maxNodes: 1500 }],
  ['maxDrop 1', { maxDrop: 1 }],
  ['maxDrop 6', { maxDrop: 6 }],
  ['a tight box', { margin: 3, yMargin: 2 }],
  ['goalFlood 0', { goalFlood: 0 }],
  ['the flood after 5 expansions', { floodAfter: 5 }],
  ['a small flood budget', { floodAfter: 5, goalFlood: 30 }],
  ['dear opening and climbing', { costs: { open: 6, openRedstone: 7, openPlate: 2, climbUp: 1, climbDown: 0.9, jumpClimb: 2 } }],
  ['water costs', { costs: { swimH: 0.2, swimUp: 0.1, swimDown: 0.15, exit: 2, current: 1, bubbleUp: 0.5, bubbleDown: 0.4 } }],
  ['little air', { costs: { airSupply: 8, airLimit: 3 } }],
  ['shallow water drops', { costs: { maxWaterDrop: 2, besideMagmaColumn: 3, dripleaf: 1, dripleafRisk: 2 } }]
]

const viewsOf = (version, snapshot, query, options) => version.view(version.plan(snapshot, query, options))

for (const [what, options] of optionSets) {
  test(`options, ${what}: both builds return what the JS planner returns on ${samples.length} courses`, () => {
    for (const [name, { snapshot, query }] of cases) {
      const expected = viewsOf(js, snapshot, query, options)
      for (const port of ports) assert.deepStrictEqual(viewsOf(port, snapshot, query, options), expected, `${port.name} ${name}`)
    }
  })
}

test('options: maxNodes 40 ends in reason budget on some courses, the box and the flood change some answers', () => {
  const reasonsOf = options => new Set(cases.map(([, { snapshot, query }]) => js.plan(snapshot, query, options).reason))
  assert.ok(reasonsOf({ maxNodes: 40 }).has('budget'))
  assert.ok(reasonsOf({ floodAfter: 5 }).has('goal-enclosed'))
})

// goals in a column nobody loaded, and goals that ignore y
const unloaded = ({ snapshot, query }, dx) => ({ snapshot, query: { ...query, goal: { ...query.goal, x: query.goal.x + dx } } })
const planar = ({ snapshot, query }) => ({ snapshot, query: { ...query, goal: { ...query.goal, kind: 'xz' } } })
const goalCases = [
  ...cases.map(([name, c]) => [`${name}: goal 400 blocks east, unloaded`, unloaded(c, 400)]),
  ...cases.map(([name, c]) => [`${name}: goal 48 blocks east (the box and the far end)`, unloaded(c, 48)]),
  ...cases.map(([name, c]) => [`${name}: an xz goal`, planar(c)])
]
for (const [what, { snapshot, query }] of goalCases) {
  test(`goals: ${what}`, () => {
    const expected = viewsOf(js, snapshot, query, undefined)
    for (const port of ports) assert.deepStrictEqual(viewsOf(port, snapshot, query, undefined), expected, port.name)
  })
}

test('goals: a goal in an unloaded column ends in reason goal-unloaded on some courses', () => {
  const reasons = new Set(cases.map(([, c]) => unloaded(c, 400)).map(({ snapshot, query }) => js.plan(snapshot, query).reason))
  assert.ok(reasons.has('goal-unloaded'))
})

test('goals: a start that is not standable and a goal that is not standable', () => {
  const { snapshot, query } = queryOf('full-open')
  const inStone = { snapshot, query: { ...query, from: { ...query.from, y: query.from.y - 3 } } }
  const buried = { snapshot, query: { ...query, goal: near(query.goal.x, query.goal.y - 3, query.goal.z, 0) } }
  const verdicts = [inStone, buried].map(({ snapshot: sn, query: q }) => {
    const expected = viewsOf(js, sn, q, undefined)
    ports.forEach(port => assert.deepStrictEqual(viewsOf(port, sn, q, undefined), expected, port.name))
    return expected.reason
  })
  assert.deepEqual(verdicts, ['start-not-standable', 'goal-not-standable'])
})

// ---- (ii) createSearch in slices ----

const slices = [100, 1, 7]
const sliceRun = (version, snapshot, query, options, slice) => {
  const search = version.createSearch(snapshot, query, options)
  const steps = []
  do steps.push(search.step(slice)); while (!steps.at(-1))
  return { steps, result: version.view(search.result()) }
}

for (const slice of slices) {
  test(`createSearch in slices of ${slice}: same step results and the same answer as plan, JS and both builds`, () => {
    for (const [name, { snapshot, query }] of cases.slice(0, slice === 1 ? 6 : cases.length)) {
      const options = { floodAfter: 5 }
      const whole = viewsOf(js, snapshot, query, options)
      const sliced = [js, ...ports].map(version => sliceRun(version, snapshot, query, options, slice))
      sliced.forEach(({ result }, k) => assert.deepStrictEqual(result, whole, `${[js, ...ports][k].name} ${name}`))
      sliced.forEach(({ steps }, k) => assert.deepStrictEqual(steps, sliced[0].steps, `${[js, ...ports][k].name} ${name}: the slices end together`))
    }
  })
}

test('createSearch: result() asked before the search is done ends it with reason budget, the same in all three', () => {
  const { snapshot, query } = queryOf('rand50-we')
  const answers = [js, ...ports].map(version => {
    const search = version.createSearch(snapshot, query)
    search.step(20)
    return version.view(search.result())
  })
  assert.deepEqual(answers.map(a => a.reason), ['budget', 'budget', 'budget'])
  answers.forEach(a => assert.deepStrictEqual(a, answers[0]))
})

test('planJs is the JS planner the versions wrap', () => {
  const { snapshot, query } = queryOf('door-closed')
  assert.deepEqual(planJs(snapshot, query).status, 'found')
})
