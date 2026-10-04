// Why JavaScript: tests the engine/tools launchers, which stay JS; a JS test is the honest check of a JS entry point (process, argv, exit code).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import http from 'node:http'
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { readEDN } from './edn.mjs'

test('say CLI sends a single chat request to the correct body and world', async t => {
  const state = fs.mkdtempSync(path.join(os.tmpdir(), 'say-cli-'))
  const dir = path.join(state, 'worlds', 'w', 'agents', 'Bob', 'engine')
  fs.mkdirSync(dir, { recursive: true })
  const socket = path.join(dir, 'events.sock'), seen = []
  const server = http.createServer((req, res) => {
    assert.equal(req.method, 'POST')
    assert.equal(req.url, '/chat')
    assert.equal(req.headers['content-type'], 'application/edn')
    res.setHeader('content-type', 'application/edn')
    let text = ''
    req.on('data', chunk => { text += chunk })
    req.on('end', () => { seen.push(readEDN(text)); res.end('{:ok true :body "Bob" :result {:status "sent" :parts 1}}') })
  })
  await new Promise((resolve, reject) => { server.once('error', reject); server.listen(socket, resolve) })
  t.after(async () => { await new Promise(resolve => server.close(resolve)); fs.rmSync(state, {recursive:true,force:true}) })
  const child = spawn(process.execPath, [fileURLToPath(new URL('../../tools/say.mjs', import.meta.url)), 'Bob', '--world', 'w', '--state', state, '--to', 'Steve', 'hello there'])
  let out = '', err = ''
  child.stdout.on('data', chunk => { out += chunk })
  child.stderr.on('data', chunk => { err += chunk })
  const code = await new Promise((resolve, reject) => { child.once('error', reject); child.once('close', resolve) })
  assert.equal(code, 0, out + err)
  assert.deepEqual(seen, [{ message: 'hello there', to: 'Steve' }])
  assert.equal(readEDN(out).result.status, 'sent')
})
