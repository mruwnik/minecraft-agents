// Why JavaScript: node --test file for tools/world-test-slots.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import { slotKinds, slotArgv } from './world-test-slots.mjs'

test('slotKinds: a run holds the body slot only', () => {
  assert.deepEqual(slotKinds(['a.edn', '--tag', 'x']), ['body'])
})
test('slotKinds: --allow-time takes the time slot first, then the body slot', () => {
  assert.deepEqual(slotKinds(['a.edn', '--allow-time', '--time-log', 'f']), ['time', 'body'])
})
test('slotArgv: nests res-slot commands outermost first and ends with the runner', () => {
  assert.deepEqual(slotArgv(['--allow-time'], '/rs', 'node', 'wt.mjs'),
    ['/rs', 'time', '--', '/rs', 'body', '--', 'node', 'wt.mjs', '--allow-time'])
})
test('slotArgv: without --allow-time only the body slot wraps it', () => {
  assert.deepEqual(slotArgv(['x.edn'], '/rs', 'node', 'wt.mjs'), ['/rs', 'body', '--', 'node', 'wt.mjs', 'x.edn'])
})
