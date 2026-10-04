import { test } from 'node:test'
import assert from 'node:assert/strict'
import { requestFor } from './world.mjs'

test('builds a bounded move-to submission with a stable request id field', () => {
  const request = requestFor(['Probe', 'submit', 'move-to', '1', '64', '-2', '--timeout-s', '4', '--who', 'claude'])
  assert.equal(request.error, undefined)
  assert.equal(request.body.op.__keyword, 'submit')
  assert.equal(request.body.action.__keyword, 'move-to')
  assert.match(request.body['request-id'], /^[0-9a-f-]{36}$/)
  assert.deepEqual(request.body.args, { pos: { x: 1, y: 64, z: -2 }, timeoutS: 4 })
})

test('allows callers to retry an uncertain submission with the same request id', () => {
  const request = requestFor(['Probe', 'submit', 'dig', '1', '64', '2', '--request-id', 'retry-1'])
  assert.equal(request.body['request-id'], 'retry-1')
  assert.match(requestFor(['../etc', 'inventory']).error, /body name/)
  assert.match(requestFor(['Probe', 'status', 'op-1', '--request-id', 'ignored']).error, /only valid for submit/)
  assert.match(requestFor(['Probe', 'submit', 'dig', '1', '64', '2', '--range', '-1']).error, /not valid for dig/)
  assert.match(requestFor(['Probe', 'inventory', '--item', 'bread']).error, /only valid with submit/)
})

test('maps status, cancellation, inventory and world interactions to fixed EDN operations', () => {
  assert.equal(requestFor(['Probe', 'status', 'op-1']).body.op.__keyword, 'status')
  assert.equal(requestFor(['Probe', 'cancel', 'op-1']).body.op.__keyword, 'cancel')
  assert.equal(requestFor(['Probe', 'inventory']).body.op.__keyword, 'inventory')
  const place = requestFor(['Probe', 'submit', 'place', '1', '64', '2', 'oak_planks'])
  assert.equal(place.body.action.__keyword, 'place')
  assert.deepEqual(place.body.args, { pos: { x: 1, y: 64, z: 2 }, item: 'oak_planks' })
  assert.equal(requestFor(['Probe', 'submit', 'interact', '17']).body.args.id, 17)
})

test('rejects malformed commands instead of forwarding arbitrary primitive calls', () => {
  assert.match(requestFor(['Probe', 'submit', 'delete-file']).error, /unknown action/)
  assert.match(requestFor(['Probe', 'submit', 'dig', '1', '64']).error, /needs x y z/)
  assert.match(requestFor(['Probe', 'submit', 'place', '1', '64', '2']).error, /needs x y z item/)
  assert.match(requestFor(['Probe', 'status']).error, /request-id/)
})
