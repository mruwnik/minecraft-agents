// The watcher's side of the view files: a chunk column dumped by a body is wrapped in the v1 file format, and
// decodeColumnFile / loadColumn must give back the same blocks.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { makeChunkClass, decodeColumnFile, loadColumn, columnCache } from '../tools/view/columns.mjs'

const VERSION = '1.21.4'
const Chunk = makeChunkClass(VERSION)

// what the body-side dumper writes (docs: file format v1)
const wrap = (column, extra = {}) => {
  const sections = column.dump()
  const biomes = Buffer.from([1, 2, 3])
  const light = Buffer.from([9, 9])
  const header = Buffer.from(JSON.stringify({
    v: 1, x: 3, z: -2, t: 1, body: 'Bob', mcVersion: VERSION, minY: column.minY, worldHeight: column.worldHeight,
    parts: [{ name: 'sections', len: sections.length }, { name: 'biomes', len: biomes.length }, { name: 'light', len: light.length, meta: { a: 1 } }],
    ...extra
  }))
  const n = Buffer.alloc(4)
  n.writeUInt32LE(header.length)
  return zlib.deflateSync(Buffer.concat([n, header, sections, biomes, light]))
}

const stone = Chunk.registry.blocksByName.stone.defaultState
const dirt = Chunk.registry.blocksByName.dirt.defaultState

const synthetic = () => {
  const column = new Chunk({ minY: -64, worldHeight: 384 })
  column.setBlockStateId({ x: 1, y: 70, z: 2 }, stone)
  column.setBlockStateId({ x: 15, y: -64, z: 15 }, dirt)
  return column
}

test('decodeColumnFile splits header and parts, parsing past biomes and light', () => {
  const column = synthetic()
  const { header, parts } = decodeColumnFile(wrap(column))
  assert.equal(header.x, 3)
  assert.equal(header.mcVersion, VERSION)
  assert.deepEqual(Array.from(parts.biomes), [1, 2, 3])
  assert.deepEqual(Array.from(parts.light), [9, 9])
  assert.deepEqual(Buffer.compare(parts.sections, column.dump()), 0)
})

test('decodeColumnFile rejects an unknown format version', () => {
  assert.throws(() => decodeColumnFile(wrap(synthetic(), { v: 2 })), /version/)
})

test('loadColumn restores the blocks of a dumped column', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-columns-'))
  const file = path.join(dir, '3.-2.bin')
  fs.writeFileSync(file, wrap(synthetic()))
  const column = loadColumn(file, Chunk)
  assert.equal(column.getBlockStateId({ x: 1, y: 70, z: 2 }), stone)
  assert.equal(column.getBlockStateId({ x: 15, y: -64, z: 15 }), dirt)
  assert.equal(column.getBlockStateId({ x: 0, y: 0, z: 0 }), 0)
})

test('the cache reloads a column only when its file changed', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-columns-'))
  const file = path.join(dir, '3.-2.bin')
  fs.writeFileSync(file, wrap(synthetic()))
  const cache = columnCache(Chunk)
  const first = cache.get(file)
  assert.equal(cache.get(file), first)
  const changed = synthetic()
  changed.setBlockStateId({ x: 1, y: 70, z: 2 }, dirt)
  fs.writeFileSync(file, wrap(changed))
  fs.utimesSync(file, new Date(), new Date(Date.now() + 5000))
  const second = cache.get(file)
  assert.notEqual(second, first)
  assert.equal(second.getBlockStateId({ x: 1, y: 70, z: 2 }), dirt)
})

test('the cache answers null for a missing file', () => {
  assert.equal(columnCache(Chunk).get('/nonexistent/1.1.bin'), null)
})
