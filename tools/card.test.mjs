// Why JavaScript: node --test file for the bash tools/card; runs it against a fake differ HTTP API.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createServer } from 'node:http'
import { spawn } from 'node:child_process'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const CARD = join(dirname(fileURLToPath(import.meta.url)), 'card')

// Runs `tools/card <args>` against a fake server answering every request with `reply`.
async function runCard(args, reply, status = 200) {
  const requests = []
  const server = createServer((req, res) => {
    let body = ''
    req.on('data', (c) => { body += c })
    req.on('end', () => {
      requests.push({ method: req.method, url: req.url, body: body && JSON.parse(body) })
      res.statusCode = status
      res.setHeader('Content-Type', 'application/json')
      res.end(JSON.stringify(reply))
    })
  })
  await new Promise((r) => server.listen(0, '127.0.0.1', r))
  const url = `http://127.0.0.1:${server.address().port}`
  const child = spawn('bash', [CARD, ...args], { env: { ...process.env, DIFFER_URL: url, CARD_REPO: '/repo/x' } })
  let out = '', err = ''
  child.stdout.on('data', (c) => { out += c })
  child.stderr.on('data', (c) => { err += c })
  const code = await new Promise((r) => child.on('close', r))
  server.close()
  return { code, out, err, requests }
}

const created = { task: { id: 'abcd1234-0000-0000-0000-000000000000', status: 'pending', title: 'T' } }

test('create posts the board tasks route with title, description, priority, tags and default checklist', async () => {
  const r = await runCard(['create', 'Bug: x breaks', '-d', 'it breaks at 1,2,3', '-p', '2', '-t', 'bug,farm'], created)
  assert.equal(r.code, 0, r.err)
  assert.equal(r.requests.length, 1)
  const call = r.requests[0]
  assert.equal(call.method, 'POST')
  assert.equal(call.url, '/api/boards/%2Frepo%2Fx/tasks')
  assert.deepEqual(call.body, {
    title: 'Bug: x breaks', description: 'it breaks at 1,2,3',
    priority: 2, tags: ['bug', 'farm'], checklist: ['unit', 'integration', 'review', 'live'],
  })
  assert.match(r.out, /^abcd1234-0000-0000-0000-000000000000 pending T/)
})

test('create defaults: priority 1, no tags, title from all words', async () => {
  const r = await runCard(['create', 'Just', 'a', 'title'], created)
  assert.equal(r.code, 0, r.err)
  const a = r.requests[0].body
  assert.equal(a.title, 'Just a title')
  assert.equal(a.priority, 1)
  assert.deepEqual(a.tags, [])
})

test('create without a title fails and sends nothing', async () => {
  const r = await runCard(['create', '-p', '2'], created)
  assert.notEqual(r.code, 0)
  assert.match(r.err, /usage: create/)
  assert.equal(r.requests.length, 0)
})

test('create reports an error from the board', async () => {
  const r = await runCard(['create', 'T'], { error: 'boom' }, 400)
  assert.equal(r.out, '')
  assert.match(r.err, /request failed/)
})

test('create rejects a non-numeric priority', async () => {
  const r = await runCard(['create', 'T', '-p', 'high'], created)
  assert.notEqual(r.code, 0)
  assert.equal(r.requests.length, 0)
})
