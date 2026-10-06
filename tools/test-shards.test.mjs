// Why JavaScript: node --test file for tools/test-shards.mjs (a Node launcher).
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
