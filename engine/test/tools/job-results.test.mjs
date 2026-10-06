// JavaScript tests exercise the public Node/process/socket boundaries of the CLJS tool (the advanced-compiled exports
// and the observe.mjs launcher over a real unix socket); the wait-loop cases live in agent-tools.observe-test.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawn } from 'node:child_process'
import http from 'node:http'
import { fileURLToPath } from 'node:url'
import tools from '../../tools/agent-tools-loader.mjs'
import { readEDN, writeEDN, keyword as k } from './edn.mjs'

const event = (seq, kind, data = {}) => ({ seq, 'generation-id': 'g', source: k('job'), kind: k(kind), context: { 'job-id': 'j4', chain: ['j4'] }, data })
const events = [event(1, 'queued'), event(2, 'search.done', { found: [{ what: 'stone', pos: [5, 70, 3] }], coverage: { scans: 1 } }), event(3, 'completed')]
function api (source = events, oversizedAt = Infinity) {
  const queries = []
  const get = async (_socket, endpoint, opts = {}) => {
    queries.push(endpoint)
    let value
    if (endpoint === '/snapshot') value = { body: 'Probe', 'generation-id': 'g', outstanding: {}, cursor: { 'stream-id': 's', seq: source.at(-1)?.seq ?? 0 } }
    else {
      const q = new URL(endpoint, 'http://local').searchParams
      const after = Number(q.get('after')); const limit = Number(q.get('limit'))
      if (limit > oversizedAt) throw Object.assign(new Error('cap'), { code: 'ERESPONSETOOLARGE' })
      value = { 'stream-id': 's', 'latest-seq': source.at(-1)?.seq ?? 0, 'gap?': false, events: source.filter(e => e.seq > after).slice(0, limit) }
    }
    const text = writeEDN(value)
    if (Buffer.byteLength(text) > opts.maxBytes) throw Object.assign(new Error('cap'), { code: 'ERESPONSETOOLARGE' })
    return { status: 200, contentType: 'application/edn', text }
  }
  return { get, queries }
}

test('result read exposes search coordinates without engine changes', async () => {
  const f = api()
  const result = await tools.jobResultRead('/unused', 'j4', f.get)
  assert.equal(result.status.key, 'completed')
  assert.deepEqual(result.events[0].data.found[0].pos, [5, 70, 3])
})

test('rich recent history uses multiple finite capped pages and safely reduces oversized read pages', async () => {
  const many = Array.from({ length: 1200 }, (_, n) => event(n + 1, 'memory_written', { debug: 'x'.repeat(1000) }))
  many[1198] = { ...events[1], seq: 1199 }; many[1199] = { ...events[2], seq: 1200 }
  const f = api(many, 32)
  const result = await tools.jobResultRead('/unused', 'j4', f.get)
  assert.equal(result.status.key, 'completed')
  assert.equal(result.history.key, 'partial')
  assert.deepEqual(result.events[0].data.found[0].pos, [5, 70, 3])
  assert.ok(f.queries.length > 3)
  assert.ok(f.queries.slice(1).every(x => Number(new URL(x, 'http://local').searchParams.get('limit')) <= 128))
  // Finite: one snapshot, a few halving retries (128 -> 32), then at most one read per capped page of the whole history.
  const pageCap = 32; const halvings = 2
  assert.ok(f.queries.length <= 1 + halvings + Math.ceil(many.length / pageCap), `${f.queries.length} queries`)
})

test('CLI completed job fallback and explicit result return the same domain outcome', async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'job-result-cli-'))
  const engineDir = path.join(dir, 'worlds', 'w', 'agents', 'Probe', 'engine')
  fs.mkdirSync(engineDir, { recursive: true })
  const socket = path.join(engineDir, 'events.sock')
  const f = api()
  const server = http.createServer(async (req, res) => {
    res.setHeader('content-type', 'application/edn')
    if (req.url.startsWith('/job?')) { res.statusCode = 404; res.end('{:ok false :reason :job-not-found}'); return }
    res.end((await f.get(socket, req.url)).text)
  })
  await new Promise(resolve => server.listen(socket, resolve))
  try {
    for (const operation of ['job', 'result']) {
      const run = await new Promise(resolve => {
        // Async child permits this process to serve its real unix-socket requests.
        const child = spawn(process.execPath, [fileURLToPath(new URL('../../tools/observe.mjs', import.meta.url)), 'Probe', '--world', 'w', '--worlds', path.join(dir, 'worlds'), operation, 'j4'])
        let stdout = ''; let stderr = ''
        child.stdout.on('data', c => { stdout += c }); child.stderr.on('data', c => { stderr += c })
        child.on('close', code => resolve({ code, stdout, stderr }))
      })
      assert.equal(run.code, 0, run.stderr + run.stdout)
      assert.deepEqual(readEDN(run.stdout).events[0].data.found[0].pos, [5, 70, 3])
    }
  } finally { await new Promise(resolve => server.close(resolve)); fs.rmSync(dir, { recursive: true }) }
})

test('optimized compatibility conversion preserves external EDN symbol wrappers', () => {
  const result = tools.jobResultProject('j4', 'g', [event(1, 'queued'), event(2, 'search.done', { target: { sym: 'stone' } }), event(3, 'completed')], false)
  assert.deepEqual(result.events[0].data.target, { sym: 'stone' })
})
