// Why JavaScript: tests seen-file.mjs, which stays JS: binary file format.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { loadSeen, saveSeen } from './seen-file.mjs'

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), 'seen-file-test-'))
const ids = fill => new Uint16Array(4096).fill(fill)

test('sections round-trip through the file in order, with their dimension and last-seen time', async () => {
  const dir = tmp()
  const file = path.join(dir, 'engine', 'seen.bin')
  await saveSeen(file, {
    version: '26.1',
    sections: [
      { dim: 'overworld', cx: -3, sy: -4, cz: 7, seen: 1000, base: 1, ids: ids(5), times: new Uint16Array(4096) },
      { dim: 'the_nether', cx: 1, sy: 2, cz: -1, seen: 2000.5, base: 2, ids: ids(9), times: new Uint16Array(4096) }
    ]
  })
  const back = loadSeen(file)
  const shape = back.sections.map(({ dim, cx, sy, cz, seen, ids }) => [dim, cx, sy, cz, seen, ids.length, ids[0], ids[4095]])
  fs.rmSync(dir, { recursive: true })
  assert.deepEqual([back.version, shape], ['26.1', [['overworld', -3, -4, 7, 1000, 4096, 5, 5], ['the_nether', 1, 2, -1, 2000.5, 4096, 9, 9]]])
})

test('each cell\'s seen time and the section base round-trip', async () => {
  const dir = tmp()
  const file = path.join(dir, 'seen.bin')
  const times = new Uint16Array(4096)
  times[7] = 5000
  await saveSeen(file, { version: '26.1', sections: [{ dim: 'overworld', cx: 0, sy: 0, cz: 0, seen: 5, base: 1234.5, ids: ids(1), times }] })
  const [back] = loadSeen(file).sections
  fs.rmSync(dir, { recursive: true })
  assert.deepEqual([back.base, back.times.length, back.times[7], back.times[8]], [1234.5, 4096, 5000, 0])
})

test('a missing file loads as null', () => {
  assert.equal(loadSeen(path.join(os.tmpdir(), 'no-such-dir-seen', 'seen.bin')), null)
})

test('a damaged file loads as null', () => {
  const dir = tmp()
  const file = path.join(dir, 'seen.bin')
  fs.writeFileSync(file, 'not deflated')
  const back = loadSeen(file)
  fs.rmSync(dir, { recursive: true })
  assert.equal(back, null)
})

test('a format 2 file (minutes in a byte) loads with its times in seconds', () => {
  const dir = tmp()
  const file = path.join(dir, 'seen.bin')
  const header = Buffer.from(JSON.stringify({ v: 2, version: '26.1', dims: ['overworld'], sections: [[0, 1, 2, 3, 9, 100]] }))
  const body = Buffer.alloc(8192 + 4096)
  body[8192 + 5] = 7
  const out = Buffer.alloc(4 + header.length)
  out.writeUInt32LE(header.length, 0)
  header.copy(out, 4)
  fs.writeFileSync(file, zlib.deflateSync(Buffer.concat([out, body])))
  const [back] = loadSeen(file).sections
  fs.rmSync(dir, { recursive: true })
  assert.deepEqual([back.base, back.times[5], back.times[6]], [100, 420, 0])
})
