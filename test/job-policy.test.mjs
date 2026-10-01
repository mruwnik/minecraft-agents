import test from 'node:test'
import assert from 'node:assert/strict'
import { mayRunBesideOwner } from '../src/job-policy.mjs'

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
