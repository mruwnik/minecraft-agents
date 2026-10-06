// Why JavaScript: node --test file for tools/test-run.mjs (a Node launcher).
import fs from 'node:fs'
import test from 'node:test'
import assert from 'node:assert/strict'
import { parseNss, expectedMs, runTimeoutS, needMb, lastFinished, isolate } from './test-run.mjs'

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
