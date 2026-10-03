import test from 'node:test'
import assert from 'node:assert/strict'
import { chatSendResponse, MAX_BODY_BYTES } from './chatroute.mjs'

const ok = async () => 'ok'

const cases = [
  ['text only', JSON.stringify({ text: 'hi' }), 200, { ok: true, command: 'tellraw @a {"text":"<Dan> hi"}' }],
  ['target', JSON.stringify({ text: 'hi', target: 'Aviendha' }), 200, { ok: true, command: 'tellraw Aviendha {"text":"<Dan> hi"}' }],
  ['from is ignored', JSON.stringify({ text: 'hi', from: 'Root' }), 200, { ok: true, command: 'tellraw @a {"text":"<Dan> hi"}' }],
  ['not json', 'nope', 400, { error: 'body must be JSON' }],
  ['not an object', '[1]', 400, { error: 'body must be a JSON object' }],
  ['no text', JSON.stringify({}), 400, { error: 'text must be a non-empty string' }],
  ['blank text', JSON.stringify({ text: '  ' }), 400, { error: 'text must be a non-empty string' }],
  ['text type', JSON.stringify({ text: 5 }), 400, { error: 'text must be a non-empty string' }],
  ['bad target', JSON.stringify({ text: 'x', target: '@e' }), 400, { error: 'target must be @a or a player name' }],
  ['too big', null, 413, { error: `body exceeds ${MAX_BODY_BYTES} bytes` }]
]

for (const [name, body, status, json] of cases) {
  test(`chatSendResponse ${name}`, async () => {
    assert.deepEqual(await chatSendResponse(body, { run: ok }), { status, json })
  })
}

test('chatSendResponse runs exactly the returned command', async () => {
  const seen = []
  await chatSendResponse(JSON.stringify({ text: 'a' }), { run: async c => { seen.push(c) } })
  assert.deepEqual(seen, ['tellraw @a {"text":"<Dan> a"}'])
})

test('chatSendResponse does not run on bad input', async () => {
  const seen = []
  await chatSendResponse(JSON.stringify({ text: '' }), { run: async c => { seen.push(c) } })
  assert.deepEqual(seen, [])
})

test('chatSendResponse maps an RCON failure to 502 without the stack', async () => {
  const run = async () => { throw new Error('connect ECONNREFUSED') }
  assert.deepEqual(await chatSendResponse(JSON.stringify({ text: 'a' }), { run }), { status: 502, json: { error: 'RCON failed: connect ECONNREFUSED' } })
})
