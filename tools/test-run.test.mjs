// Why JavaScript: node --test file for tools/test-run.mjs (a Node launcher).
import fs from 'node:fs'
import path from 'node:path'
import { EventEmitter } from 'node:events'
import { spawn } from 'node:child_process'
import test from 'node:test'
import assert from 'node:assert/strict'
import { UNKNOWN_NS_MS, parseNss, nsOf, expectedMs, stallLimitS, slowestPriorMs, watchStall, stallMessage, needMb, lastFinished, isolate, narrowBundle, missingNss, sweepStale, cleanupOnExit, killTree } from './test-run.mjs'

test('parseNss: space- and comma-separated namespaces, blanks dropped', () => {
  assert.deepEqual(parseNss(['engine.a-test,engine.b-test', 'engine.c-test', '']), ['engine.a-test', 'engine.b-test', 'engine.c-test'])
})

test('expectedMs: known ns summed, unknown ones cost the mean of the known', () => {
  assert.equal(expectedMs(['a', 'b'], { a: 100, b: 300 }), 400)
  assert.equal(expectedMs(['a', 'x'], { a: 100, b: 300 }), 300)
  assert.equal(expectedMs(['x'], {}), 2100)
})

test('stallLimitS: the floor, or 10x the slowest prior test', () => {
  assert.equal(stallLimitS(0), 90)
  assert.equal(stallLimitS(2000), 90)
  assert.equal(stallLimitS(30000), 300)
})

test('slowestPriorMs: slowest var of the requested namespaces only', () => {
  const lines = ['{"var":"#\'a-test/x","ms":40}', '{"var":"#\'a-test/y","ms":900}', '{"var":"#\'b-test/z","ms":5000}', 'junk', '']
  assert.equal(slowestPriorMs(['a-test'], lines), 900)
  assert.equal(slowestPriorMs(['c-test'], lines), UNKNOWN_NS_MS, 'no timing: assume a slow one')
  assert.equal(slowestPriorMs(['a-test', 'c-test'], lines), UNKNOWN_NS_MS)
})

test('watchStall: fires once when the file stops growing, not before the run has started', async (t) => {
  const f = `/tmp/test-run-stall-${process.pid}`
  fs.writeFileSync(f, '')
  t.after(() => fs.rmSync(f, { force: true }))
  let fired = 0, clock = 0
  const stop = watchStall(f, 10, () => fired++, { pollMs: 5, now: () => clock, firstS: 0 })
  const tick = (ms) => new Promise((r) => setTimeout(r, ms))
  clock = 100000; await tick(30)
  assert.equal(fired, 0, 'an empty file means node has not started')
  fs.appendFileSync(f, 'x\n'); await tick(30)
  clock += 5000; await tick(30)
  assert.equal(fired, 0, 'inside the limit')
  fs.appendFileSync(f, 'y\n'); await tick(30)
  clock += 9000; await tick(30)
  assert.equal(fired, 0, 'progress resets the clock')
  clock += 2000; await tick(30)
  assert.equal(fired, 1)
  clock += 20000; await tick(30)
  assert.equal(fired, 1)
  stop()
})

test('watchStall: before any test has finished the longer first-result limit applies', async (t) => {
  const f = `/tmp/test-run-first-${process.pid}`
  fs.writeFileSync(f, '{"started":1}\n')
  t.after(() => fs.rmSync(f, { force: true }))
  let fired = 0, clock = 0
  const stop = watchStall(f, 10, () => fired++, { pollMs: 5, now: () => clock, firstS: 100 })
  const tick = (ms) => new Promise((r) => setTimeout(r, ms))
  await tick(30)
  clock += 50000; await tick(30)
  assert.equal(fired, 0, 'past the stall limit but inside the first-result limit')
  fs.appendFileSync(f, '{"var":"#\'a-test/x","ms":4}\n'); await tick(30)
  clock += 11000; await tick(30)
  assert.equal(fired, 1, 'after a test finished the normal limit applies')
  stop()
})

test('watchStall kills a CPU-spinning node that finished nothing (the parent is not blocked)', async (t) => {
  const f = `/tmp/test-run-spin-${process.pid}`
  fs.writeFileSync(f, '{"started":1}\n')
  t.after(() => fs.rmSync(f, { force: true }))
  const child = spawn('node', ['-e', 'for (;;);'], { stdio: 'ignore' })
  const exit = new Promise((r) => child.on('exit', (c, sig) => r(sig)))
  const stop = watchStall(f, 0.2, () => killTree(child.pid), { pollMs: 50, firstS: 0 })
  assert.equal(await exit, 'SIGTERM')
  stop()
})

test('stallMessage names the last finished test and the namespaces with no finished test', () => {
  const lines = ['{"started":1}', '{"var":"#\'a-test/x","ms":4}', '']
  const m = stallMessage('test-engine', 90, lines, ['a-test', 'b-test'])
  assert.match(m, /STALLED, no test finished for 90 s/)
  assert.match(m, /last finished test: #'a-test\/x/)
  assert.match(m, /no finished test: b-test$/)
})

test('needMb: measured peak plus margin per run, growing with the namespace count, capped at the shard need', () => {
  const tests = { needMb: 2950, baseMb: 700, perNsMb: 40 }
  assert.equal(needMb(1, tests), 740)
  assert.equal(needMb(10, tests), 1100)
  assert.equal(needMb(45, tests), 2500)
  assert.equal(needMb(200, tests), 2950)
})

test('res-slot.json holds the one table of tests needs: shard 1700, heap 1024, golden 940, floor 4096', () => {
  const res = JSON.parse(fs.readFileSync(path.join(path.dirname(new URL(import.meta.url).pathname), 'res-slot.json'), 'utf8'))
  assert.deepEqual([res.kinds.tests.needMb, res.kinds.tests.goldenMb, res.floorMb], [1700, 940, 4096])
})

test('res-slot.json: the node heap cap leaves room under the shard need, and shards fit the slot count', () => {
  const t = JSON.parse(fs.readFileSync(path.join(path.dirname(new URL(import.meta.url).pathname), 'res-slot.json'), 'utf8')).kinds.tests
  assert.ok(t.heapMb > 0 && t.heapMb < t.needMb, JSON.stringify(t))
  assert.ok(t.shardMax <= t.max - 2, JSON.stringify(t))
})

test('res-slot.json: a server slot needs 2000 MB (a shadow-cljs server JVM)', () => {
  const res = JSON.parse(fs.readFileSync(path.join(path.dirname(new URL(import.meta.url).pathname), 'res-slot.json'), 'utf8'))
  assert.equal(res.kinds.server.needMb, 2000)
})

test('lastFinished: the last test var in a timing file, or none', () => {
  const lines = ['{"var":"#\'engine.a-test/one","ms":3}', '{"var":"#\'engine.a-test/two","ms":5}', '']
  assert.equal(lastFinished(lines), "#'engine.a-test/two")
  assert.equal(lastFinished(['{"peak-rss-kb":1}']), null)
  assert.equal(lastFinished([]), null)
})

test('isolate: copies the bundle and runtime, links test and node_modules, cleanup removes it', (t) => {
  const src = fs.mkdtempSync('/tmp/test-run-src-')
  t.after(() => fs.rmSync(src, { recursive: true, force: true }))
  fs.mkdirSync(`${src}/out/test/cljs-runtime`, { recursive: true })
  fs.mkdirSync(`${src}/test`); fs.mkdirSync(`${src}/node_modules`)
  fs.writeFileSync(`${src}/out/test.cjs`, 'bundle')
  fs.writeFileSync(`${src}/out/test/cljs-runtime/a.js`, 'a')
  const { runDir, cleanup } = isolate(src, 'unit')
  fs.writeFileSync(`${src}/out/test.cjs`, 'swapped by another compile')
  assert.equal(fs.readFileSync(`${runDir}/out/test.cjs`, 'utf8'), 'bundle')
  assert.equal(fs.readFileSync(`${runDir}/out/test/cljs-runtime/a.js`, 'utf8'), 'a')
  assert.equal(fs.realpathSync(`${runDir}/test`), fs.realpathSync(`${src}/test`))
  assert.equal(fs.realpathSync(`${runDir}/node_modules`), fs.realpathSync(`${src}/node_modules`))
  cleanup()
  assert.equal(fs.existsSync(runDir), false)
})

const ns = (n) => `new cljs.core.Symbol(null,"${n.replaceAll('_', '-')}","x",1,null)`
const testData = (nss) => `return shadow.test.env.reset_test_data_BANG_(cljs.core.PersistentHashMap.fromArrays([${nss.map(ns).join(',')}],[${nss.map((n) => `cljs.core.with_meta(new cljs.core.PersistentArrayMap(null, 1, ["a]},[\\"b", [(${n}.f)]], null), null)`).join(',')}]));`

test('narrowBundle: drops unrequested -test namespaces from the imports and the test registry, keeps requested ones, helpers and required test namespaces', (t) => {
  const dir = fs.mkdtempSync('/tmp/test-run-narrow-')
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  fs.mkdirSync(`${dir}/out/test/cljs-runtime`, { recursive: true })
  const imp = (f) => `SHADOW_IMPORT("${f}.js");`
  fs.writeFileSync(`${dir}/out/test.cjs`, ['engine.test_util', 'engine.a_test', 'engine.b_test', 'engine.c_test', 'shadow.test.node'].map(imp).join('\n') + '\n})();\n')
  fs.writeFileSync(`${dir}/out/test/cljs-runtime/engine.a_test.js`, "goog.require('engine.c_test');")
  const nodeJs = `${dir}/out/test/cljs-runtime/shadow.test.node.js`
  fs.writeFileSync(nodeJs, `var x = 1;\n${testData(['engine.a_test', 'engine.b_test', 'engine.c_test'])}\nvar y = 2;\n`)
  assert.equal(narrowBundle(dir, ['engine.a-test']), 1)
  assert.deepEqual(fs.readFileSync(`${dir}/out/test.cjs`, 'utf8').split('\n').filter(Boolean),
    ['engine.test_util', 'engine.a_test', 'engine.c_test', 'shadow.test.node'].map(imp).concat('})();'))
  assert.equal(fs.readFileSync(nodeJs, 'utf8'), `var x = 1;\n${testData(['engine.a_test', 'engine.c_test'])}\nvar y = 2;\n`)
})

test('narrowBundle: keeps a test namespace the requested one only references (no goog.require line in the dev output)', (t) => {
  const dir = fs.mkdtempSync('/tmp/test-run-narrow-')
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  fs.mkdirSync(`${dir}/out/test/cljs-runtime`, { recursive: true })
  const imp = (f) => `SHADOW_IMPORT("${f}.js");`
  fs.writeFileSync(`${dir}/out/test.cjs`, ['engine.a_test', 'engine.b_test', 'engine.c_test', 'shadow.test.node'].map(imp).join('\n') + '\n})();\n')
  fs.writeFileSync(`${dir}/out/test/cljs-runtime/engine.a_test.js`, 'goog.provide("engine.a_test");\nawait engine.c_test.go_BANG_(1);')
  fs.writeFileSync(`${dir}/out/test/cljs-runtime/shadow.test.node.js`, `${testData(['engine.a_test', 'engine.b_test', 'engine.c_test'])}\n`)
  assert.equal(narrowBundle(dir, ['engine.a-test']), 1)
  assert.deepEqual(fs.readFileSync(`${dir}/out/test.cjs`, 'utf8').split('\n').filter(Boolean),
    ['engine.a_test', 'engine.c_test', 'shadow.test.node'].map(imp).concat('})();'))
})

test('narrowBundle: narrows the PersistentArrayMap registry a few-namespace compile emits', (t) => {
  const dir = fs.mkdtempSync('/tmp/test-run-narrow-')
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  fs.mkdirSync(`${dir}/out/test/cljs-runtime`, { recursive: true })
  const imp = (f) => `SHADOW_IMPORT("${f}.js");`
  fs.writeFileSync(`${dir}/out/test.cjs`, ['engine.a_test', 'engine.b_test', 'shadow.test.node'].map(imp).join('\n') + '\n})();\n')
  fs.writeFileSync(`${dir}/out/test/cljs-runtime/engine.a_test.js`, 'goog.provide("engine.a_test");')
  const nodeJs = `${dir}/out/test/cljs-runtime/shadow.test.node.js`
  const reg = (nss) => `return shadow.test.env.reset_test_data_BANG_(new cljs.core.PersistentArrayMap(null, ${nss.length}, [${nss.map((n) => `${ns(n)},new cljs.core.PersistentArrayMap(null, 1, ["v,]",[(${n}.f)]])`).join(',')}]));`
  fs.writeFileSync(nodeJs, `var x = 1;\n${reg(['engine.a_test', 'engine.b_test'])}\nvar y = 2;\n`)
  assert.equal(narrowBundle(dir, ['engine.a-test']), 1)
  assert.equal(fs.readFileSync(nodeJs, 'utf8'), `var x = 1;\n${reg(['engine.a_test'])}\nvar y = 2;\n`)
})

test('narrowBundle: an unrecognised registry leaves the bundle untouched', (t) => {
  const dir = fs.mkdtempSync('/tmp/test-run-narrow-')
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  fs.mkdirSync(`${dir}/out/test/cljs-runtime`, { recursive: true })
  fs.writeFileSync(`${dir}/out/test.cjs`, 'SHADOW_IMPORT("engine.b_test.js");\n')
  fs.writeFileSync(`${dir}/out/test/cljs-runtime/shadow.test.node.js`, 'something else')
  assert.equal(narrowBundle(dir, ['engine.a-test']), 0)
  assert.equal(fs.readFileSync(`${dir}/out/test.cjs`, 'utf8'), 'SHADOW_IMPORT("engine.b_test.js");\n')
})

test('isolate with namespaces: narrows from the source and copies only the imported files and their maps', (t) => {
  const src = fs.mkdtempSync('/tmp/test-run-src-')
  t.after(() => fs.rmSync(src, { recursive: true, force: true }))
  const rt = `${src}/out/test/cljs-runtime`
  fs.mkdirSync(rt, { recursive: true }); fs.mkdirSync(`${src}/test`); fs.mkdirSync(`${src}/node_modules`)
  const imp = (f) => `SHADOW_IMPORT("${f}.js");`
  const bundle = ['goog.base', 'engine.a_test', 'engine.b_test', 'shadow.test.node'].map(imp).join('\n') + '\n})();\n'
  fs.writeFileSync(`${src}/out/test.cjs`, bundle)
  for (const f of ['goog.base', 'engine.a_test', 'engine.b_test']) fs.writeFileSync(`${rt}/${f}.js`, f)
  fs.writeFileSync(`${rt}/engine.a_test.js.map`, 'map'); fs.writeFileSync(`${rt}/engine.b_test.js.map`, 'map')
  const registry = `${testData(['engine.a_test', 'engine.b_test'])}\n`
  fs.writeFileSync(`${rt}/shadow.test.node.js`, registry)
  const { runDir, cleanup } = isolate(src, 'unit-narrow', '/tmp', ['engine.a-test'])
  t.after(cleanup)
  assert.deepEqual(fs.readdirSync(`${runDir}/out/test/cljs-runtime`).sort(), ['engine.a_test.js', 'engine.a_test.js.map', 'goog.base.js', 'shadow.test.node.js'])
  assert.equal(fs.readFileSync(`${runDir}/out/test/cljs-runtime/shadow.test.node.js`, 'utf8'), `${testData(['engine.a_test'])}\n`)
  assert.equal(fs.readFileSync(`${src}/out/test.cjs`, 'utf8'), bundle)
  assert.equal(fs.readFileSync(`${rt}/shadow.test.node.js`, 'utf8'), registry)
})

test('missingNss: requested namespaces absent from the compiled :test bundle (a run of them would pass with 0 tests)', (t) => {
  const src = fs.mkdtempSync('/tmp/test-run-missing-')
  t.after(() => fs.rmSync(src, { recursive: true, force: true }))
  fs.mkdirSync(`${src}/out/test/cljs-runtime`, { recursive: true })
  fs.writeFileSync(`${src}/out/test/cljs-runtime/engine.go_to_test.js`, '')
  assert.deepEqual(missingNss(src, ['engine.go-to-test', 'engine.planner-courses-golden', 'engine.nope']), ['engine.planner-courses-golden', 'engine.nope'])
  assert.deepEqual(missingNss(src, ['engine.go-to-test']), [])
})

test('narrowBundle: always keeps engine.timing_test (the per-test timing and @@test reporter hooks)', (t) => {
  const dir = fs.mkdtempSync('/tmp/test-run-narrow-')
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  fs.mkdirSync(`${dir}/out/test/cljs-runtime`, { recursive: true })
  const imp = (f) => `SHADOW_IMPORT("${f}.js");`
  fs.writeFileSync(`${dir}/out/test.cjs`, ['engine.a_test', 'engine.b_test', 'engine.timing_test', 'shadow.test.node'].map(imp).join('\n') + '\n})();\n')
  fs.writeFileSync(`${dir}/out/test/cljs-runtime/shadow.test.node.js`, `${testData(['engine.a_test', 'engine.b_test'])}\n`)
  assert.equal(narrowBundle(dir, ['engine.a-test']), 1)
  assert.deepEqual(fs.readFileSync(`${dir}/out/test.cjs`, 'utf8').split('\n').filter(Boolean),
    ['engine.a_test', 'engine.timing_test', 'shadow.test.node'].map(imp).concat('})();'))
})

test('sweepStale: removes only mc-test-run-<digits> dirs of dead pids from the given root', (t) => {
  const root = fs.mkdtempSync('/tmp/test-run-sweep-')
  t.after(() => fs.rmSync(root, { recursive: true, force: true }))
  for (const d of ['mc-test-run-111', 'mc-test-run-222', 'mc-test-run-abc', 'mc-test-run-333-x', 'other-444', 'mc-test-run', 'engine-test-111-aB3', 'engine-test-222-xYz', 'engine-test-Ab12Cd']) fs.mkdirSync(`${root}/${d}/out`, { recursive: true })
  fs.writeFileSync(`${root}/mc-test-run-999`, 'a file of that name')
  for (const f of ['mc-test-run-111-shard-3.log', 'mc-test-run-222-shard-1.log', 'mc-test-run-111-shard-x.log']) fs.writeFileSync(`${root}/${f}`, 'log')
  const removed = sweepStale(root, (pid) => pid === 222)
  assert.deepEqual(removed.sort(), ['engine-test-111-aB3', 'mc-test-run-111', 'mc-test-run-111-shard-3.log', 'mc-test-run-999'])
  assert.deepEqual(fs.readdirSync(root).sort(), ['engine-test-222-xYz', 'engine-test-Ab12Cd', 'mc-test-run', 'mc-test-run-111-shard-x.log', 'mc-test-run-222', 'mc-test-run-222-shard-1.log', 'mc-test-run-333-x', 'mc-test-run-abc', 'other-444'])
})

test('sweepStale: a really dead pid is swept, the own pid is kept', (t) => {
  const root = fs.mkdtempSync('/tmp/test-run-sweep-')
  t.after(() => fs.rmSync(root, { recursive: true, force: true }))
  fs.mkdirSync(`${root}/mc-test-run-${process.pid}`)
  fs.mkdirSync(`${root}/mc-test-run-4194303`)
  sweepStale(root)
  assert.deepEqual(fs.readdirSync(root), [`mc-test-run-${process.pid}`])
})

test('cleanupOnExit: cleans once on exit, and on SIGTERM / SIGINT it stops the child, cleans and exits 128+signal', () => {
  for (const [sig, code] of [['SIGTERM', 143], ['SIGINT', 130]]) {
    const proc = Object.assign(new EventEmitter(), { exit: (c) => { proc.exited = c } })
    const calls = []
    cleanupOnExit(proc, () => calls.push('clean'), (s) => calls.push(`kill ${s}`))
    proc.emit(sig)
    proc.emit('exit')
    assert.deepEqual(calls, [`kill ${sig}`, 'clean'])
    assert.equal(proc.exited, code)
  }
  const proc = new EventEmitter()
  const calls = []
  cleanupOnExit(proc, () => calls.push('clean'))
  proc.emit('exit')
  assert.deepEqual(calls, ['clean'])
})

const alive = (pid) => { try { process.kill(pid, 0); return true } catch { return false } }
const sleepMs = (ms) => new Promise((r) => setTimeout(r, ms))

test('killTree: stops the child and its descendants, also those in another process group (timeout(1) makes its own)', async () => {
  const dir = fs.mkdtempSync('/tmp/test-run-tree-')
  const pidFile = `${dir}/pids`
  // sh -> setsid sh (own group) -> sleep: the grandchild is out of the child's process group.
  const child = spawn('sh', ['-c', `setsid sh -c 'sleep 300 & echo $! > ${pidFile}; wait' & wait`], { stdio: 'ignore' })
  const exited = new Promise((r) => child.on('exit', r))
  for (let i = 0; i < 100 && !fs.existsSync(pidFile); i++) await sleepMs(50)
  await sleepMs(100)
  const leaf = Number(fs.readFileSync(pidFile, 'utf8'))
  assert.ok(alive(leaf))
  killTree(child.pid, 'SIGTERM')
  await exited
  for (let i = 0; i < 100 && alive(leaf); i++) await sleepMs(50)
  fs.rmSync(dir, { recursive: true, force: true })
  assert.equal(alive(leaf), false)
})

test('cleanupOnExit: SIGHUP is handled like SIGTERM (exit 129)', () => {
  const proc = Object.assign(new EventEmitter(), { exit: (c) => { proc.exited = c } })
  const calls = []
  cleanupOnExit(proc, () => calls.push('clean'), (s) => calls.push(`kill ${s}`))
  proc.emit('SIGHUP')
  assert.deepEqual(calls, ['kill SIGHUP', 'clean'])
  assert.equal(proc.exited, 129)
})

test('sweepStale: a dir that cannot be removed is skipped, the others still go', (t) => {
  const root = fs.mkdtempSync('/tmp/test-run-sweep-')
  t.after(() => { fs.chmodSync(`${root}/mc-test-run-111/sub`, 0o755); fs.rmSync(root, { recursive: true, force: true }) })
  fs.mkdirSync(`${root}/mc-test-run-111/sub/x`, { recursive: true })
  fs.mkdirSync(`${root}/mc-test-run-222`)
  fs.chmodSync(`${root}/mc-test-run-111/sub`, 0o555)
  const removed = sweepStale(root, () => false)
  assert.ok(removed.includes('mc-test-run-222'))
  assert.ok(!removed.includes('mc-test-run-111'))
})

test('nsOf: a namespace/deftest spec runs in its namespace', () => {
  assert.deepEqual(['engine.a-test', 'engine.a-test/some-test?'].map(nsOf), ['engine.a-test', 'engine.a-test'])
})

test('missingNss: a namespace/deftest spec is missing when the deftest is not defined in the bundle (shadow would run 0 tests)', (t) => {
  const src = fs.mkdtempSync('/tmp/test-run-missing-var-')
  t.after(() => fs.rmSync(src, { recursive: true, force: true }))
  fs.mkdirSync(`${src}/out/test/cljs-runtime`, { recursive: true })
  fs.writeFileSync(`${src}/out/test/cljs-runtime/engine.a_test.js`, 'engine.a_test.go_to_works = (function (){})\nengine.a_test.ready_QMARK__is_set = 1\n')
  assert.deepEqual(missingNss(src, ['engine.a-test/go-to-works', 'engine.a-test/ready?-is-set', 'engine.a-test/nope', 'engine.b-test/x']), ['engine.a-test/nope', 'engine.b-test/x'])
})

test('res-slot.json needs stay above the measured VmHWM: 476 MB for one namespace, 640 for five, 722 for many', () => {
  const t = JSON.parse(fs.readFileSync(path.join(path.dirname(new URL(import.meta.url).pathname), 'res-slot.json'), 'utf8')).kinds.tests
  assert.ok(needMb(1, t) >= 476 && needMb(5, t) >= 640 && needMb(30, t) >= 722, JSON.stringify(t))
  assert.ok(needMb(1, t) < 700, JSON.stringify(t))
})
