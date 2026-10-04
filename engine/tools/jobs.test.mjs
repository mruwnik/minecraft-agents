import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import http from 'node:http'
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { requestFor, specFor, post } from './jobs.mjs'
import { readEDN, writeEDN } from './observe-lib.mjs'
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

test('cancel-all has no job argument; hold is a native submission option', () => {
  const cancel = requestFor(['Bob', '--world', 'w', 'cancel-all', '--request-id', 'all'])
  assert.equal(cancel.request.op.key, 'cancel-all')
  assert.equal(cancel.request['request-id'], 'all')
  assert.equal('id' in cancel.request, false)
  assert.equal('spec' in cancel.request, false)
  assert.equal(cancel.mutating, true)
  const held = requestFor(['Bob', '--world', 'w', 'submit', '(jobs.time.wait-for-day)', '--hold'])
  assert.equal(held.request['hold?'], true)
  assert.match(writeEDN(held.request), /:hold\? true/)
  assert.equal('hold?' in requestFor(['Bob', '--world', 'w', 'submit', '(jobs.time.wait-for-day)']).request, false)
  for (const args of [['cancel-all','j1'], ['cancel-all','--hold'], ['list','--hold'], ['show','j1','--hold'], ['retry','j1','--hold']]) assert.ok(requestFor(['Bob','--world','w',...args]).error)
})

test('compiled CLI sends hold and cancel-all through the existing generation-aware EDN API', async t => {
  const state = fs.mkdtempSync(path.join(os.tmpdir(), 'jobs-cli-'))
  const dir = path.join(state, 'worlds', 'w', 'agents', 'Bob', 'engine')
  fs.mkdirSync(dir, { recursive: true })
  const socket = path.join(dir, 'events.sock'), seen = []
  const server = http.createServer((req, res) => {
    res.setHeader('content-type', 'application/edn')
    if (req.url === '/snapshot') { res.end('{:generation-id "generation"}'); return }
    assert.equal(req.method, 'POST')
    assert.equal(req.url, '/jobs')
    assert.equal(req.headers['content-type'], 'application/edn')
    let text = ''
    req.on('data', chunk => { text += chunk })
    req.on('end', () => { seen.push(readEDN(text)); res.end('{:ok true}') })
  })
  await new Promise((resolve, reject) => { server.once('error', reject); server.listen(socket, resolve) })
  t.after(async () => { await new Promise(resolve => server.close(resolve)); fs.rmSync(state, {recursive:true,force:true}) })
  const run = args => new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [fileURLToPath(new URL('./jobs.mjs', import.meta.url)), 'Bob', '--world', 'w', '--state', state, ...args])
    let out = '', err = ''
    child.stdout.on('data', chunk => { out += chunk })
    child.stderr.on('data', chunk => { err += chunk })
    child.once('error', reject)
    child.once('close', code => { try { assert.equal(code, 0, out+err); assert.deepEqual(readEDN(out), {ok:true}); resolve() } catch(error) { reject(error) } })
  })
  await run(['submit', '(seq (jobs.time.wait-for-day) (jobs.movement.go-to {:pos {:x -1 :y 64 :z 2}}))', '--hold', '--request-id', 'held'])
  await run(['cancel-all', '--request-id', 'clear'])
  assert.equal(seen.length, 2)
  assert.equal(seen[0]['hold?'], true)
  assert.equal(seen[0].spec.list[0].sym, 'seq')
  assert.equal(seen[0].spec.list[2].list[1].map[0][0].key, 'pos')
  assert.equal(seen[0].spec.list[2].list[1].map[0][1].map[0][1], -1)
  assert.equal(seen[1].op.key, 'cancel-all')
  assert.equal('id' in seen[1], false)
  for (const request of seen) assert.equal(request['generation-id'], 'generation')
  assert.equal(readEDN(fs.readFileSync(path.join(state, 'commands', 'Bob', 'jobs', 'clear.edn'),'utf8'))['generation-id'], 'generation')
})
