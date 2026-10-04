import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { requestFor, specFor, post } from './jobs.mjs'
import { writeEDN } from './observe-lib.mjs'
test('native expressions preserve lists/symbols/keyword arguments through EDN transport', () => {
  const r = requestFor(['--world', 'w', 'Bob', 'submit', '(jobs.movement.go-to {:pos {:x -1 :y 64 :z 2}})', '--request-id', 'move-home'])
  assert.equal(r.request['request-id'], 'move-home')
  assert.match(writeEDN(r.request), /\(jobs.movement.go-to \{:pos \{:x -1/)
  assert.equal(requestFor(['--world', 'w', 'Bob', 'interrupt', '(repeat (jobs.movement.look-around))']).request.op.key, 'interrupt')
})
test('list/show/cancel/retry use fixed endpoints and validate arguments', () => {
  assert.equal(requestFor(['--world', 'w', 'Bob']).path, '/jobs?limit=8&offset=0')
  assert.equal(requestFor(['--world', 'w', 'Bob', 'list', '--limit', '3', '--offset', '4']).path, '/jobs?limit=3&offset=4')
  assert.equal(requestFor(['--world', 'w', 'Bob', 'show', 'j4']).path, '/job?id=j4')
  assert.equal(requestFor(['--world', 'w', 'Bob', 'cancel', 'j4']).request.id, 'j4')
  assert.equal(requestFor(['--world', 'w', 'Bob', 'retry', 'j4']).request.op.key, 'retry')
  assert.ok(requestFor(['--world', 'w', '../Bob']).error)
  assert.ok(requestFor(['--world', 'w', 'Bob', 'cancel', 'invalid']).error)
  assert.ok(requestFor(['--world', 'w', 'Bob', 'list', '--request-id', 'bad']).error)
  assert.ok(requestFor(['--world', 'w', 'Bob', 'list', '--limit', '99']).error)
})
test('a body is addressed in its world: --world is required and names the socket folder', () => {
  assert.match(requestFor(['Bob']).error, /missing --world <world>/)
  assert.match(requestFor(['Bob', '--world', '../x']).error, /world/)
  assert.equal(requestFor(['Bob', '--world', 'w', '--state', '/s']).socketPath, '/s/worlds/w/agents/Bob/engine/events.sock')
})
test('malformed/multiple/oversized native job forms fail locally without evaluation', () => {
  for (const source of ['{:job :foo}', '(jobs.one) (jobs.two)', '()']) assert.throws(() => specFor(source))
  assert.throws(() => specFor('x'.repeat(13000)))
  assert.ok(requestFor(['--world', 'w', 'Bob', 'submit', '{:job "custom"}']).error)
})

test('job mutations have a total deadline even when no response arrives', async () => {
  class Pending extends EventEmitter { end () {} destroy (error) { this.destroyed = true; this.emit('error', error) } }
  let pending
  await assert.rejects(post('/unused', {op: {key: 'cancel'}, id: 'j1'}, {timeoutMs: 5, requestImpl: () => {pending = new Pending(); return pending}}), {code: 'ETIMEDOUT'})
  assert.equal(pending.destroyed, true)
})
