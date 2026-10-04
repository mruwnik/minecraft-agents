// Why JavaScript: tests seen-file.mjs, which stays JS: binary file format.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { loadSeen, saveSeen } from './seen-file.mjs'

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), 'seen-file-test-'))
const ids = fill => new Uint16Array(4096).fill(fill)

test('sections round-trip through the file in order, with their dimension and last-seen time', async () => {
  const dir = tmp()
  const file = path.join(dir, 'engine', 'seen.bin')
  await saveSeen(file, {
    version: '26.1',
    sections: [
      { dim: 'overworld', cx: -3, sy: -4, cz: 7, seen: 1000, ids: ids(5) },
      { dim: 'the_nether', cx: 1, sy: 2, cz: -1, seen: 2000.5, ids: ids(9) }
    ]
  })
  const back = loadSeen(file)
  const shape = back.sections.map(({ dim, cx, sy, cz, seen, ids }) => [dim, cx, sy, cz, seen, ids.length, ids[0], ids[4095]])
  fs.rmSync(dir, { recursive: true })
  assert.deepEqual([back.version, shape], ['26.1', [['overworld', -3, -4, 7, 1000, 4096, 5, 5], ['the_nether', 1, 2, -1, 2000.5, 4096, 9, 9]]])
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
