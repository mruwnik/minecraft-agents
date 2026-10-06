// Why JavaScript: node --test file for tools/test-run.mjs (a Node launcher).
import fs from 'node:fs'
import test from 'node:test'
import assert from 'node:assert/strict'
import { parseNss, expectedMs, runTimeoutS, needMb, lastFinished, isolate, narrowBundle } from './test-run.mjs'

test('parseNss: space- and comma-separated namespaces, blanks dropped', () => {
  assert.deepEqual(parseNss(['engine.a-test,engine.b-test', 'engine.c-test', '']), ['engine.a-test', 'engine.b-test', 'engine.c-test'])
})

test('expectedMs: known ns summed, unknown ones cost the mean of the known', () => {
  assert.equal(expectedMs(['a', 'b'], { a: 100, b: 300 }), 400)
  assert.equal(expectedMs(['a', 'x'], { a: 100, b: 300 }), 300)
  assert.equal(expectedMs(['x'], {}), 0)
})

test('runTimeoutS: 60 s plus 5x the expected time, between 180 s and 1200 s', () => {
  assert.equal(runTimeoutS(0), 180)
  assert.equal(runTimeoutS(57251), 346)
  assert.equal(runTimeoutS(1e7), 1200)
})

test('needMb: ~1 GB for one namespace, growing with the count, capped at the shard reservation', () => {
  assert.equal(needMb(1, 2800), 960)
  assert.equal(needMb(10, 2800), 1500)
  assert.equal(needMb(45, 2800), 2800)
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

test('narrowBundle: an unrecognised registry leaves the bundle untouched', (t) => {
  const dir = fs.mkdtempSync('/tmp/test-run-narrow-')
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  fs.mkdirSync(`${dir}/out/test/cljs-runtime`, { recursive: true })
  fs.writeFileSync(`${dir}/out/test.cjs`, 'SHADOW_IMPORT("engine.b_test.js");\n')
  fs.writeFileSync(`${dir}/out/test/cljs-runtime/shadow.test.node.js`, 'something else')
  assert.equal(narrowBundle(dir, ['engine.a-test']), 0)
  assert.equal(fs.readFileSync(`${dir}/out/test.cjs`, 'utf8'), 'SHADOW_IMPORT("engine.b_test.js");\n')
})
