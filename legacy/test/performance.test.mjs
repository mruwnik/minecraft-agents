import test from 'node:test'
import assert from 'node:assert/strict'
import { createSlowScanReporter, timedScan } from '../src/performance.mjs'

test('slow scans report a bug without changing results or throwing at one second', () => {
  const events = []
  const report = createSlowScanReporter({ emit: (...args) => events.push(args) })
  const result = { positions: [{ x: 1, y: 64, z: 2 }] }
  const clock = [0, 1205]
  assert.equal(timedScan(report, 'find_blocks', () => result, { block: 'poppy', range: 24 }, () => clock.shift()), result)
  assert.equal(events[0][0], 'performance_bug')
  assert.equal(events[0][1].elapsed_ms, 1205)
  assert.equal(events[0][1].block, 'poppy')
  report('fast', 1000)
  assert.equal(events.length, 1)
})

test('repeated slow scans are rate limited by operation without suppressing other operations', () => {
  let now = 0
  const events = []
  const report = createSlowScanReporter({ emit: (...args) => events.push(args), now: () => now })
  assert.equal(report('path_to', 1200).reported, true)
  assert.equal(report('path_to', 1500).reported, false)
  assert.equal(report('path_to', 1600).reported, false)
  report('find_blocks', 1100)
  assert.equal(events.length, 2)
  now = 60000
  report('path_to', 1400)
  assert.equal(events[2][1].repeated, 2)
})

test('reporting failures do not lose scan results or replace original errors', () => {
  const report = createSlowScanReporter({ emit: () => { throw new Error('log unavailable') } })
  let clock = [0, 2000]
  assert.equal(timedScan(report, 'scan', () => 42, {}, () => clock.shift()), 42)
  const error = new Error('unknown block')
  clock = [0, 2000]
  assert.throws(() => timedScan(report, 'other', () => { throw error }, {}, () => clock.shift()), e => e === error)
})
