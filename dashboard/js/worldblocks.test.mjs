import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createWorldBlocks } from './worldblocks.mjs'

const realState = path.join(import.meta.dirname, '../../state')
const hasRealColumn = fs.existsSync(path.join(realState, 'worlds', 'claude', 'chunks', '1.-6.bin'))

test('no chunk directory: every block is unknown (null)', () => {
  const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'worldblocks-'))
  const { blockAt, close } = createWorldBlocks({ stateDir, world: 'w' })
  assert.equal(blockAt(0, 64, 0), null)
  assert.equal(blockAt(-17, 64, 300), null)
  close()
})

test('a dumped column answers with block names, a neighbour that was never dumped with null', { skip: !hasRealColumn }, () => {
  const { blockAt, close } = createWorldBlocks({ stateDir: realState, world: 'claude' })
  const names = [-64, 0, 63, 64, 65, 66, 70, 80].map(y => blockAt(16, y, -96))
  assert.ok(names.every(n => typeof n === 'string'))
  assert.ok(names.some(n => n !== 'air'))
  assert.equal(blockAt(9_999_999, 64, 9_999_999), null)
  close()
})

test('worldtiles: a missing column is null, a dumped one has 256 cells with a palette, heights and water depth', { skip: !hasRealColumn }, async () => {
  const { createWorldTiles, encodeTile } = await import('./worldblocks.mjs')
  const tiles = createWorldTiles({ stateDir: realState, world: 'claude' })
  const skip = ['air', 'cave_air', 'short_grass']
  assert.equal(tiles.column(9_999_999, 9_999_999, skip), null)
  const col = tiles.column(1, -6, skip)
  assert.equal(col.top.length, 256)
  assert.equal(col.y.length, 256)
  assert.equal(col.depth.length, 256)
  assert.ok(col.palette.length > 0)
  assert.ok([...col.top].every(i => i < col.palette.length))
  assert.ok(!col.palette.includes('short_grass'))
  const png = encodeTile(16, 16, new Uint8Array(16 * 16 * 4))
  assert.deepEqual([...png.subarray(1, 4)], [80, 78, 71])
  tiles.close()
})
