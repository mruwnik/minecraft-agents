import test from 'node:test'
import assert from 'node:assert/strict'
import { mayRunBesideOwner, resultIsDeath, resultLostItems, recordOutcome, severeFailure, holdReason } from '../src/job-policy.mjs'

const tables = {
  quick: Object.fromEntries(['state', 'chat', 'whisper', 'inventory', 'look', 'eat', 'control', 'follow', 'reflexes'].map(name => [name, () => {}])),
  long: { 'farm.harvest': () => {}, run: () => {} }
}

test('chat and exact read-only observations can run beside a body owner', () => {
  for (const name of ['chat', 'whisper', 'state', 'inventory', 'look']) assert.equal(mayRunBesideOwner(name, tables), true, name)
})

test('physical quick actions and every long/composite action go through the owner scheduler', () => {
  for (const name of ['eat', 'control', 'follow', 'reflexes', 'farm.harvest', 'run']) assert.equal(mayRunBesideOwner(name, tables), false, name)
})

test('allowlisted name without an actual quick handler is not treated as a concurrent action', () => {
  assert.equal(mayRunBesideOwner('block_at', { quick: {}, long: {} }), false)
})

// ---------------------------------------------------------------- hold policy
test('resultIsDeath matches only the cancellation text a death actually produces', () => {
  assert.equal(resultIsDeath({ error: 'cancelled: died at 10,65,20 (slain by Zombie)' }), true)
  assert.equal(resultIsDeath({ error: 'cancelled: died at 10,65,20' }), true)
  assert.equal(resultIsDeath({ error: 'no path to the goal' }), false)
  assert.equal(resultIsDeath({ error: 'cancelled: stop' }), false)
  assert.equal(resultIsDeath({}), false)
})

test('resultLostItems is true only for a non-empty count map, regardless of ok=', () => {
  assert.equal(resultLostItems({ ok: true, lost: { iron_pickaxe: 1 } }), true)
  assert.equal(resultLostItems({ ok: false, lost: { bread: 1 } }), true)
  assert.equal(resultLostItems({ ok: true, lost: {} }), false)
  assert.equal(resultLostItems({ ok: true }), false)
  assert.equal(resultLostItems({ ok: true, lost: '2 still lying on the ground' }), false)
})

test('recordOutcome counts consecutive failures per job name and resets on that name\'s own success', () => {
  const streaks = new Map()
  assert.equal(recordOutcome(streaks, 'goto', true), 1)
  assert.equal(recordOutcome(streaks, 'craft', true), 1) // a different name keeps its own count
  assert.equal(recordOutcome(streaks, 'goto', true), 2)
  assert.equal(recordOutcome(streaks, 'goto', false), 0) // goto's own success clears it
  assert.equal(recordOutcome(streaks, 'goto', true), 1) // starts over
  assert.equal(recordOutcome(streaks, 'craft', true), 2) // craft's count was untouched by goto
})

test('severeFailure: only death, lost items, restorationPending, or a same-name failure streak of 2+ hold the queue', () => {
  const base = { status: 'failed', streak: 1, cleanupPending: false }
  assert.equal(severeFailure({ ...base, result: { ok: false, error: 'no path to the goal' } }), false)
  assert.equal(severeFailure({ ...base, result: { ok: false, error: 'no bed within 32 blocks' } }), false)
  assert.equal(severeFailure({ ...base, result: { ok: false, error: 'untillable: stone' } }), false)
  assert.equal(severeFailure({ ...base, streak: 2, result: { ok: false, error: 'no path to the goal' } }), true)
  assert.equal(severeFailure({ ...base, result: { ok: false, error: 'cancelled: died at 1,2,3' } }), true)
  assert.equal(severeFailure({ status: 'completed', streak: 0, cleanupPending: false, result: { ok: true, lost: { bucket: 1 } } }), true)
  assert.equal(severeFailure({ status: 'cancelled', streak: 0, cleanupPending: false, result: { ok: false, cancelled: true } }), false)
  assert.equal(severeFailure({ status: 'failed', streak: 1, cleanupPending: true, result: { ok: false, restorationPending: 'gate open' } }), true)
})

test('holdReason names which condition fired', () => {
  assert.match(holdReason({ id: 5, status: 'failed', result: { error: 'cancelled: died at 1,2,3' }, cleanupPending: false, streak: 1 }), /job 5 failed: the body died/)
  assert.match(holdReason({ id: 6, status: 'completed', result: { lost: { bucket: 1, iron_axe: 1 } }, cleanupPending: false, streak: 0 }), /job 6 completed: lost bucket, iron_axe/)
  assert.match(holdReason({ id: 7, status: 'failed', result: { error: 'no path' }, cleanupPending: false, streak: 2 }), /job 7 failed twice running/)
  assert.match(holdReason({ id: 8, status: 'failed', result: { restorationPending: 'gate open' }, cleanupPending: true, streak: 1 }), /^job 8 failed; inspect/)
})
