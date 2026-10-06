// Why JavaScript: node --test file for tools/test-shards.mjs (a Node launcher).
import fs from 'node:fs'
import path from 'node:path'
import test from 'node:test'
import assert from 'node:assert/strict'
import { splitShards, testNamespaces, slowest, memSlots } from './test-shards.mjs'

test('splitShards: every namespace exactly once, loads balanced by prior timing', () => {
  const nss = ['a-test', 'b-test', 'c-test', 'd-test', 'e-test']
  const ms = { 'a-test': 100, 'b-test': 60, 'c-test': 50, 'd-test': 40, 'e-test': 10 }
  const shards = splitShards(nss, ms, 2)
  assert.deepEqual(shards.flat().sort(), nss)
  const load = shards.map((s) => s.reduce((t, n) => t + ms[n], 0))
  assert.ok(Math.abs(load[0] - load[1]) <= 20, String(load))
})

test('splitShards: unknown timings still split evenly, never empty shards beyond namespace count', () => {
  const shards = splitShards(['a-test', 'b-test', 'c-test'], {}, 5)
  assert.equal(shards.length, 3)
  assert.deepEqual(shards.flat().sort(), ['a-test', 'b-test', 'c-test'])
})

test('testNamespaces finds -test ns forms under engine/test', () => {
  const nss = testNamespaces()
  assert.ok(nss.includes('engine.go-to-test'))
  assert.ok(nss.every((n) => n.endsWith('-test')))
})

test('slowest sorts timing lines descending', () => {
  const lines = ['{"var":"x/a","ms":5}', '{"var":"x/b","ms":50}', '{"peak-rss-kb":1}']
  assert.deepEqual(slowest(lines, 1), [{ var: 'x/b', ms: 50 }])
})

test('memSlots: (available - 6 GB floor) / shard peak, at least 1, at most 3', () => {
  assert.equal(memSlots(5900), 1)
  assert.equal(memSlots(12000), 2)
  assert.equal(memSlots(30000), 3)
})

test('testNamespaces covers every file under engine/test that contains deftest', () => {
  const root = path.join(path.dirname(new URL(import.meta.url).pathname), '..', 'engine', 'test')
  const walk = (d) => fs.readdirSync(d, { withFileTypes: true }).flatMap((e) => e.isDirectory() ? walk(path.join(d, e.name)) : [path.join(d, e.name)])
  const nss = testNamespaces()
  const missing = walk(root).filter((f) => /\(deftest\s/.test(fs.readFileSync(f, 'utf8')))
    .filter((f) => !nss.includes(fs.readFileSync(f, 'utf8').match(/^\(ns\s+(?:\^\S+\s+)*([^\s()]+)[\s)]/m)?.[1]))
  assert.deepEqual(missing, [])
})
