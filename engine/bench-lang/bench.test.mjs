import { test } from 'node:test'
import assert from 'node:assert/strict'
import { quantile, rotated, summarize, parseArgs } from './bench.mjs'

const quantiles = [
  [[1, 2, 3, 4], 0.5, 2.5],
  [[3, 1, 2], 0.5, 2],
  [[10], 0.95, 10],
  [Array.from({ length: 101 }, (_, k) => k + 1), 0.95, 96],
  [[1, 2], 0.95, 1.95]
]
for (const [xs, q, expected] of quantiles) {
  test(`quantile ${q} of ${xs.length} values is ${expected}`, () => {
    assert.ok(Math.abs(quantile(xs, q) - expected) < 1e-12)
  })
}

const rotations = [
  [0, ['a', 'b', 'c']],
  [1, ['b', 'c', 'a']],
  [2, ['c', 'a', 'b']],
  [3, ['a', 'b', 'c']],
  [7, ['b', 'c', 'a']]
]
for (const [k, expected] of rotations) {
  test(`rotated by ${k}: every version takes every place in turn`, () => {
    assert.deepEqual(rotated(['a', 'b', 'c'], k), expected)
  })
}

const run = {
  versions: ['js', 'other'],
  courses: [{ id: 'w', group: 'world', expanded: 100 }, { id: 'c', group: 'course', expanded: 300 }],
  // samples[version][course]: ms of each timed round
  samples: [[[1, 1, 1], [3, 3, 3]], [[2, 2, 2], [3, 3, 3]]]
}

test('summarize: pooled median and p95, sum of per-course medians, nodes per second, ratios to JS', () => {
  const { all } = summarize(run)
  assert.deepEqual(all.map(v => v.name), ['js', 'other'])
  assert.deepEqual(all.map(v => v.medianMs), [2, 2.5])
  assert.deepEqual(all.map(v => v.p95Ms), [3, 3])
  assert.deepEqual(all.map(v => v.sumMs), [4, 5])
  assert.deepEqual(all.map(v => Math.round(v.nodesPerSec)), [100000, 80000])
  assert.deepEqual(all.map(v => v.ratio), [1, 1.25])
  assert.ok(Math.abs(all[1].geomeanRatio - Math.SQRT2) < 1e-12)
})

test('summarize: one table per group', () => {
  const { world, course } = summarize(run)
  assert.deepEqual(world.map(v => v.ratio), [1, 2])
  assert.deepEqual(course.map(v => v.ratio), [1, 1])
  assert.deepEqual(world.map(v => v.plans), [3, 3])
})

test('parseArgs: defaults and overrides', () => {
  assert.deepEqual(parseArgs([]), { rounds: 30, warmup: 5, out: undefined, versions: undefined, maxLoad: Infinity })
  assert.deepEqual(parseArgs(['--rounds', '3', '--warmup', '1', '--out', 'x.json', '--versions', 'js,cljs-tuned-dev', '--max-load', '8']),
    { rounds: 3, warmup: 1, out: 'x.json', versions: ['js', 'cljs-tuned-dev'], maxLoad: 8 })
})
