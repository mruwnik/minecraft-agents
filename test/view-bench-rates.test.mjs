// Why JavaScript: tests the pure helpers of the JavaScript chromium bench (tools/view-web-bench.mjs).
import assert from 'node:assert/strict'
import test from 'node:test'
import { cardRates, meanSceneFps, dashboardLoadError } from '../tools/view/bench-rates.mjs'

const sample = scenes => ({ stats: { scenes: Object.fromEntries(Object.entries(scenes).map(([a, renders]) => [a, { renders, fps: renders }])) } })

test('cardRates handles a scene that appears mid-run', () => {
  const samples = [sample({ a: 0 }), sample({ a: 6 }), sample({ a: 12, b: 100 }), sample({ a: 18, b: 106 })]
  const { perCard, seconds } = cardRates(samples)
  assert.equal(seconds, 3)
  assert.equal(perCard.a.run, 6)
  assert.equal(perCard.b.run, 6)
})

test('cardRates reports a worst window only for scenes with enough samples', () => {
  const samples = Array.from({ length: 8 }, (_, i) => sample({ a: i * 5, ...(i >= 6 ? { b: i } : {}) }))
  const { perCard, minWindow } = cardRates(samples)
  assert.equal(perCard.a.worstWindow, 5)
  assert.equal(perCard.b.worstWindow, null)
  assert.equal(minWindow, 5)
})

test('cardRates leaves out a scene seen in one sample only', () => {
  const { perCard } = cardRates([sample({ a: 0 }), sample({ a: 5 }), sample({ a: 10, b: 1 })])
  assert.deepEqual(Object.keys(perCard), ['a'])
})

test('cardRates skips a counter reset instead of reporting a negative rate', () => {
  const { perCard, minRun } = cardRates([sample({ a: 100 }), sample({ a: 106 }), sample({ a: 2 }), sample({ a: 8 })])
  assert.equal(perCard.a.run, 6)
  assert.equal(minRun, 6)
})

test('meanSceneFps averages over the samples that have the scene', () => {
  assert.equal(meanSceneFps([sample({ a: 4 }), sample({ a: 6, b: 10 })], 'b'), 10)
  assert.equal(meanSceneFps([sample({ a: 4 }), sample({ a: 6 })], 'a'), 5)
})

test('dashboardLoadError names a refused connection in one line', () => {
  const line = dashboardLoadError('http://localhost:3798/', 'net::ERR_CONNECTION_REFUSED')
  assert.equal(line, 'cannot load http://localhost:3798/: net::ERR_CONNECTION_REFUSED')
})
