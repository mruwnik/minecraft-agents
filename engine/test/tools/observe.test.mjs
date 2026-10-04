import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import fs from 'node:fs'
import os from 'node:os'
import http from 'node:http'
import path from 'node:path'
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { get, legacyEngineNotice, requestFor, unsupportedObserveRoute } from '../../tools/observe.mjs'
import { readEDN } from '../../tools/observe-lib.mjs'

const cli = fileURLToPath(new URL('../../tools/observe.mjs', import.meta.url))

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
  const request = requestFor(['--world', 'w', 'ProbeBody', '--state', '/tmp/custom state'])
  const notice = legacyEngineNotice(request)
  assert.match(notice, /:reason :observe-unavailable/)
  assert.match(notice, /:action :restart-with-current-build/)
  assert.match(notice, /:fallback \{:op :status :raw true :world "w" :state "\/tmp\/custom state"\}/)
  assert.doesNotMatch(legacyEngineNotice(requestFor(['--world', 'w', 'ProbeBody', 'inventory'])), /:fallback/)
})

test('observe defaults to bounded status and targets the engine event socket', () => {
  const request = requestFor(['--world', 'w', 'ProbeBody', '--state', '/tmp/state'])
  assert.deepEqual(request, {
    agent: 'ProbeBody',
    world: 'w',
    state: path.resolve('/tmp/state'),
    socketPath: path.join('/tmp/state', 'worlds', 'w', 'agents', 'ProbeBody', 'engine', 'events.sock'),
    path: '/status'
  })
})

test('observe without --world is an error naming the flag', () => {
  assert.match(requestFor(['ProbeBody']).error, /missing --world <world>/)
  assert.match(requestFor(['ProbeBody', '--world', 'a/b']).error, /world/)
})

test('observe supports detail and on-demand capability lookups', () => {
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'status', '--limit', '4']).path, '/status?limit=4')
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'job', 'j12']).path, '/job?id=j12')
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'catalog', 'job', 'jobs.movement.go-to']).path,
    '/catalog?kind=job&name=jobs.movement.go-to')
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'catalog', 'trigger', 'health-low']).path,
    '/catalog?kind=trigger&name=health-low')
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'catalog', 'jobs', 'jobs.movement.', '--limit', '5', '--offset', '10']).path,
    '/catalog?kind=jobs&prefix=jobs.movement.&limit=5&offset=10')
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'catalog', 'triggers']).path,
    '/catalog?kind=triggers&prefix=&limit=20&offset=0')
})

test('inventory and equipment are read-only observe modes with explicit raw or slot detail', () => {
  assert.deepEqual(requestFor(['--world', 'w', 'ProbeBody', 'inventory', '--state', '/tmp/state']), {
    agent: 'ProbeBody', world: 'w', state: path.resolve('/tmp/state'),
    inventoryMode: 'inventory', slots: false, raw: false,
    socketPath: path.join('/tmp/state', 'worlds', 'w', 'agents', 'ProbeBody', 'engine', 'events.sock'),
    path: '/inventory'
  })
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'inventory', '--slots']).slots, true)
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'equipment', '--raw']).inventoryMode, 'equipment')
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'equipment', '--raw']).raw, true)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'equipment', '--slots']).error, /only valid for inventory/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'inventory', 'bread']).error, /takes no positional arguments/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'inventory', '--raw', '--slots']).error, /redundant with --raw/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'inventory', '--limit', '2']).error, /not valid for inventory/)
  assert.equal(requestFor(['--world', 'w', 'ProbeBody', 'status']).inventoryMode, undefined)
})

test('inventory and equipment CLI emits aggregate, raw and optional slot views from the read socket', async t => {
  const state = fs.mkdtempSync(path.join(os.tmpdir(), 'observe-inventory-'))
  const socket = path.join(state, 'worlds', 'w', 'agents', 'ProbeBody', 'engine', 'events.sock')
  fs.mkdirSync(path.dirname(socket), { recursive: true })
  const server = http.createServer((_req, res) => {
    res.writeHead(200, { 'content-type': 'application/edn' })
    res.end('{:ok true :inventory [{:name "bread" :count 5 :slot 9} {:name "iron_pickaxe" :count 1 :slot 37}] :equipment {:head {:name "iron_helmet" :count 1 :durability 140}}}')
  })
  await new Promise((resolve, reject) => { server.once('error', reject); server.listen(socket, resolve) })
  t.after(async () => {
    await new Promise(resolve => server.close(resolve))
    fs.rmSync(state, { recursive: true, force: true })
  })
  const run = args => new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [cli, 'ProbeBody', '--world', 'w', '--state', state, ...args])
    let stdout = '', stderr = ''
    child.stdout.on('data', chunk => { stdout += chunk })
    child.stderr.on('data', chunk => { stderr += chunk })
    child.once('error', reject)
    child.once('close', code => resolve({ code, stdout, stderr }))
  })
  const summary = await run(['inventory'])
  assert.equal(summary.code, 0, summary.stderr)
  assert.deepEqual(readEDN(summary.stdout), {
    'total-items': 6, kinds: 2, counts: { bread: 5, iron_pickaxe: 1 },
    equipment: { head: { name: 'iron_helmet', count: 1, durability: 140 } }
  })
  const slots = await run(['inventory', '--slots'])
  assert.deepEqual(readEDN(slots.stdout).slots, [
    { name: 'bread', count: 5, slot: 9 }, { name: 'iron_pickaxe', count: 1, slot: 37 }
  ])
  const equipment = await run(['equipment'])
  assert.deepEqual(readEDN(equipment.stdout), { equipment: { head: { name: 'iron_helmet', count: 1, durability: 140 } } })
  const rawEquipment = await run(['equipment', '--raw'])
  assert.deepEqual(readEDN(rawEquipment.stdout), { equipment: { head: { name: 'iron_helmet', count: 1, durability: 140 } } })
  const raw = await run(['inventory', '--raw'])
  assert.deepEqual(readEDN(raw.stdout).inventory[0], { name: 'bread', count: 5, slot: 9 })
  assert.deepEqual(readEDN(raw.stdout).equipment.head, { name: 'iron_helmet', count: 1, durability: 140 })
})

test('observe rejects malformed names and limits before connecting', () => {
  assert.match(requestFor(['--world', 'w', 'bad/name']).error, /body name/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'status', '--limit', '100']).error, /1 to 32/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'status', '--offset', '1']).error, /only valid for catalog lists/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'job', 'j12', '--offset', '1']).error, /only valid for status and catalog lists/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'catalog', 'job', 'jobs.x.y', '--limit', '2']).error, /does not accept/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'catalog', 'job', '(eval foo)']).error, /exact jobs namespace/)
  assert.match(requestFor(['--world', 'w', 'ProbeBody', 'job', 'j12', 'extra']).error, /one job ID/)
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
