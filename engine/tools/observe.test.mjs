import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import path from 'node:path'
import { get, legacyEngineNotice, requestFor, unsupportedObserveRoute } from './observe.mjs'

test('observe distinguishes an older engine missing the projection routes', () => {
  assert.equal(unsupportedObserveRoute({
    status: 404,
    contentType: 'application/edn; charset=utf-8',
    text: '{:ok false, :reason :not-found}'
  }), true)
  assert.equal(unsupportedObserveRoute({
    status: 404,
    contentType: 'application/edn',
    text: '{:ok false, :reason :job-not-found}'
  }), false)
  assert.equal(unsupportedObserveRoute({
    status: 404,
    contentType: 'application/edn',
    text: '{:extra {:reason :not-found}}'
  }), false)
  assert.equal(unsupportedObserveRoute({
    status: 404,
    contentType: 'text/plain',
    text: 'not found'
  }), false)
})

test('legacy observe guidance preserves a nondefault state root', () => {
  const request = requestFor(['ProbeBody', '--state', '/tmp/custom state'])
  const notice = legacyEngineNotice(request)
  assert.match(notice, /:reason :observe-unavailable/)
  assert.match(notice, /:action :restart-with-current-build/)
  assert.match(notice, /:fallback \{:op :status :raw true :state "\/tmp\/custom state"\}/)
})

test('observe defaults to bounded status and targets the engine event socket', () => {
  const request = requestFor(['ProbeBody', '--state', '/tmp/state'])
  assert.deepEqual(request, {
    agent: 'ProbeBody',
    state: path.resolve('/tmp/state'),
    socketPath: path.join('/tmp/state', 'agents', 'ProbeBody', 'engine', 'events.sock'),
    path: '/status'
  })
})

test('observe supports detail and on-demand capability lookups', () => {
  assert.equal(requestFor(['ProbeBody', 'status', '--limit', '4']).path, '/status?limit=4')
  assert.equal(requestFor(['ProbeBody', 'job', 'j12']).path, '/job?id=j12')
  assert.equal(requestFor(['ProbeBody', 'catalog', 'job', 'jobs.movement.go-to']).path,
    '/catalog?kind=job&name=jobs.movement.go-to')
  assert.equal(requestFor(['ProbeBody', 'catalog', 'trigger', 'health-low']).path,
    '/catalog?kind=trigger&name=health-low')
  assert.equal(requestFor(['ProbeBody', 'catalog', 'jobs', 'jobs.movement.', '--limit', '5', '--offset', '10']).path,
    '/catalog?kind=jobs&prefix=jobs.movement.&limit=5&offset=10')
  assert.equal(requestFor(['ProbeBody', 'catalog', 'triggers']).path,
    '/catalog?kind=triggers&prefix=&limit=20&offset=0')
})

test('observe rejects malformed names and limits before connecting', () => {
  assert.match(requestFor(['bad/name']).error, /body name/)
  assert.match(requestFor(['ProbeBody', 'status', '--limit', '100']).error, /1 to 32/)
  assert.match(requestFor(['ProbeBody', 'status', '--offset', '1']).error, /only valid for catalog lists/)
  assert.match(requestFor(['ProbeBody', 'job', 'j12', '--offset', '1']).error, /only valid for status and catalog lists/)
  assert.match(requestFor(['ProbeBody', 'catalog', 'job', 'jobs.x.y', '--limit', '2']).error, /does not accept/)
  assert.match(requestFor(['ProbeBody', 'catalog', 'job', '(eval foo)']).error, /exact jobs namespace/)
  assert.match(requestFor(['ProbeBody', 'job', 'j12', 'extra']).error, /one job ID/)
})

test('observe socket reads have a finite total deadline', async () => {
  class HangingRequest extends EventEmitter {
    end () {}
    destroy () { this.destroyed = true }
  }
  let request
  await assert.rejects(
    get('/unused', '/status', {
      timeoutMs: 5,
      requestImpl: (_options, _callback) => { request = new HangingRequest(); return request }
    }),
    { code: 'ETIMEDOUT' }
  )
  assert.equal(request.destroyed, true)
})

test('observe refuses oversized responses', async () => {
  class FakeResponse extends EventEmitter {
    statusCode = 200
    headers = { 'content-type': 'application/edn' }
    setEncoding () {}
    destroy () {}
  }
  class FakeRequest extends EventEmitter {
    end () {
      queueMicrotask(() => {
        this.callback(new FakeResponse())
        this.response.emit('data', '12345')
      })
    }
    destroy () { this.destroyed = true }
  }
  let request
  await assert.rejects(get('/unused', '/snapshot', {
    maxBytes: 4,
    requestImpl: (_options, callback) => {
      request = new FakeRequest()
      request.callback = callback
      const original = request.callback
      request.callback = response => { request.response = response; original(response) }
      return request
    }
  }), { code: 'ERESPONSETOOLARGE' })
  assert.equal(request.destroyed, true)
})

test('observe reports aborted responses as transport errors', async () => {
  class AbortedResponse extends EventEmitter {
    statusCode = 200
    headers = { 'content-type': 'application/edn' }
    setEncoding () {}
    destroy () {}
  }
  class AbortedRequest extends EventEmitter {
    end () { queueMicrotask(() => { this.callback(new AbortedResponse()); this.response.emit('aborted') }) }
    destroy () {}
  }
  await assert.rejects(get('/unused', '/status', {
    requestImpl: (_options, callback) => {
      const request = new AbortedRequest()
      request.callback = response => { request.response = response; callback(response) }
      return request
    }
  }), { code: 'ECONNRESET' })
})
