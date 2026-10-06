// Why JavaScript: node --test file for tools/test-run.mjs (a Node launcher).
import fs from 'node:fs'
import path from 'node:path'
import { EventEmitter } from 'node:events'
import { spawn } from 'node:child_process'
import test from 'node:test'
import assert from 'node:assert/strict'
import { parseNss, expectedMs, runTimeoutS, needMb, lastFinished, isolate, narrowBundle, missingNss, sweepStale, cleanupOnExit, killTree } from './test-run.mjs'

test('parseNss: space- and comma-separated namespaces, blanks dropped', () => {
  assert.deepEqual(parseNss(['engine.a-test,engine.b-test', 'engine.c-test', '']), ['engine.a-test', 'engine.b-test', 'engine.c-test'])
})

test('expectedMs: known ns summed, unknown ones cost the mean of the known', () => {
  assert.equal(expectedMs(['a', 'b'], { a: 100, b: 300 }), 400)
  assert.equal(expectedMs(['a', 'x'], { a: 100, b: 300 }), 300)
  assert.equal(expectedMs(['x'], {}), 2100)
})

test('expectedMs without prior timings: a 52-namespace shard gets more than the 180 s floor', () => {
  const nss = Array.from({ length: 52 }, (_, i) => `n${i}`)
  assert.ok(runTimeoutS(expectedMs(nss, {})) > 500)
})

test('runTimeoutS: 60 s plus 5x the expected time, between 180 s and 1200 s', () => {
  assert.equal(runTimeoutS(0), 180)
  assert.equal(runTimeoutS(57251), 346)
  assert.equal(runTimeoutS(1e7), 1200)
})

test('needMb: measured peak plus margin per run, growing with the namespace count, capped at the shard need', () => {
  const tests = { needMb: 2950, baseMb: 700, perNsMb: 40 }
  assert.equal(needMb(1, tests), 740)
  assert.equal(needMb(10, tests), 1100)
  assert.equal(needMb(45, tests), 2500)
  assert.equal(needMb(200, tests), 2950)
})

test('res-slot.json holds the one table of tests needs: shard 2950, golden 940, floor untouched', () => {
  const res = JSON.parse(fs.readFileSync(path.join(path.dirname(new URL(import.meta.url).pathname), 'res-slot.json'), 'utf8'))
  assert.deepEqual([res.kinds.tests.needMb, res.kinds.tests.goldenMb, res.floorMb], [2950, 940, 6144])
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

test('narrowBundle: an unrecognised registry leaves the bundle untouched', (t) => {
  const dir = fs.mkdtempSync('/tmp/test-run-narrow-')
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  fs.mkdirSync(`${dir}/out/test/cljs-runtime`, { recursive: true })
  fs.writeFileSync(`${dir}/out/test.cjs`, 'SHADOW_IMPORT("engine.b_test.js");\n')
  fs.writeFileSync(`${dir}/out/test/cljs-runtime/shadow.test.node.js`, 'something else')
  assert.equal(narrowBundle(dir, ['engine.a-test']), 0)
  assert.equal(fs.readFileSync(`${dir}/out/test.cjs`, 'utf8'), 'SHADOW_IMPORT("engine.b_test.js");\n')
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
  for (const d of ['mc-test-run-111', 'mc-test-run-222', 'mc-test-run-abc', 'mc-test-run-333-x', 'other-444', 'mc-test-run']) fs.mkdirSync(`${root}/${d}/out`, { recursive: true })
  fs.writeFileSync(`${root}/mc-test-run-999`, 'a file of that name')
  const removed = sweepStale(root, (pid) => pid === 222)
  assert.deepEqual(removed.sort(), ['mc-test-run-111', 'mc-test-run-999'])
  assert.deepEqual(fs.readdirSync(root).sort(), ['mc-test-run', 'mc-test-run-222', 'mc-test-run-333-x', 'mc-test-run-abc', 'other-444'])
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
