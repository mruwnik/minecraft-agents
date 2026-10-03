import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { fileURLToPath } from 'node:url'
import { createHandler } from '../tools/view/web/decode-worker.mjs'
import { columnFormat } from '../tools/view/web-format.mjs'

const fixture = fileURLToPath(new URL('./fixtures/view-columns/128.117.bin', import.meta.url))
const table = { type: 'table', format: columnFormat('26.1'), materialOf: new Uint16Array(40000).fill(1) }

test('a job posts the result with its buffers and the elapsed ms in the transfer list', async () => {
  const posted = []
  const handle = createHandler((message, transfer) => posted.push({ message, transfer }))
  await handle({ data: table })
  await handle({ data: { type: 'job', id: 7, key: 'k', bytes: new Uint8Array(fs.readFileSync(fixture)) } })
  const [{ message, transfer }] = posted
  assert.equal(message.id, 7)
  assert.equal(typeof message.ms, 'number')
  assert.deepEqual(transfer, [message.result.mats.buffer, message.result.flags.buffer, message.result.light.buffer])
})

test('a corrupt job posts an error with the same id and nothing to transfer', async () => {
  const posted = []
  const handle = createHandler((message, transfer) => posted.push({ message, transfer }))
  await handle({ data: table })
  await handle({ data: { type: 'job', id: 8, key: 'k', bytes: new Uint8Array([1, 2, 3]) } })
  assert.equal(posted[0].message.id, 8)
  assert.equal(typeof posted[0].message.error, 'string')
  assert.equal(posted[0].transfer, undefined)
})
