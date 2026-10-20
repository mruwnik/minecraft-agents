// Why JavaScript: tests table-cache.mjs, a Node file-I/O boundary module.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { decode, encode, tableCached } from './table-cache.mjs'

const sample = () => ({
  top: Uint8Array.from([0, 16, 8]),
  openState: Uint32Array.from([0, 70000, 5]),
  boxes: Float32Array.from([0, 0.5, 1]),
  nameIds: new Map([['stone', [1, 1]], ['water', [80, 95]]])
})

test('a table survives encode and decode with its types and maps', () => {
  assert.deepEqual(decode(encode(sample())), sample())
})

test('the table is built once per build file and read back after that', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'tablecache-'))
  const file = path.join(dir, 'bundle.cjs')
  fs.writeFileSync(file, 'a')
  let builds = 0
  const build = () => { builds++; return sample() }
  try {
    assert.deepEqual(tableCached('t', file, build, dir), sample())
    assert.deepEqual(tableCached('t', file, build, dir), sample())
    assert.equal(builds, 1)
    fs.writeFileSync(file, 'longer bundle')
    tableCached('t', file, build, dir)
    assert.equal(builds, 2)
    assert.equal(fs.readdirSync(dir).filter(f => f.endsWith('.bin')).length, 1)
  } finally { fs.rmSync(dir, { recursive: true, force: true }) }
})
