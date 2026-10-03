import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { Vec3 } from 'vec3'
import prismarineBlock from 'prismarine-block'
import { makeChunkClass } from '../../tools/view/columns.mjs'
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

// a world dir holding one dumped column (chunk 0.0) with a door, a waterlogged slab and plain water
const MC_VERSION = '26.1'
function stateDirWithColumn () {
  const Chunk = makeChunkClass(MC_VERSION)
  const Block = prismarineBlock(Chunk.registry)
  const column = new Chunk({ minY: -64, worldHeight: 384 })
  const put = (x, y, z, name, props) => column.setBlockStateId(new Vec3(x, y, z), Block.fromProperties(name, props, 0).stateId)
  put(1, 64, 2, 'oak_door', { facing: 'east', half: 'lower' })
  put(3, 64, 2, 'oak_slab', { type: 'bottom', waterlogged: true })
  put(4, 64, 2, 'water', {})
  put(5, 64, 2, 'stone', {})
  const sections = column.dump()
  const header = Buffer.from(JSON.stringify({ v: 1, x: 0, z: 0, t: 0, body: 'b', mcVersion: MC_VERSION, minY: -64, worldHeight: 384, parts: [{ name: 'sections', len: sections.length }] }))
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'worldblocks-'))
  fs.mkdirSync(path.join(stateDir, 'worlds', 'w', 'chunks'), { recursive: true })
  fs.writeFileSync(path.join(stateDir, 'worlds', 'w', 'chunks', '0.0.bin'), zlib.deflateSync(Buffer.concat([length, header, sections])))
  return stateDir
}

test('a dumped block answers with its name and its state, air with an empty state, an undumped chunk with null', () => {
  const { blockAt, close } = createWorldBlocks({ stateDir: stateDirWithColumn(), world: 'w' })
  assert.equal(blockAt(1, 64, 2).name, 'oak_door')
  assert.equal(blockAt(1, 64, 2).state.facing, 'east')
  assert.equal(blockAt(1, 64, 2).state.half, 'lower')
  assert.equal(blockAt(3, 64, 2).name, 'oak_slab')
  assert.equal(blockAt(3, 64, 2).state.waterlogged, true)
  assert.equal(blockAt(4, 64, 2).name, 'water')
  assert.equal(blockAt(5, 64, 2).name, 'stone')
  assert.equal(blockAt(0, 70, 0).name, 'air')
  assert.equal(blockAt(-1, 64, 0), null)
  close()
})

test('a dumped column answers with block names, a neighbour that was never dumped with null', { skip: !hasRealColumn }, () => {
  const { blockAt, close } = createWorldBlocks({ stateDir: realState, world: 'claude' })
  const names = [-64, 0, 63, 64, 65, 66, 70, 80].map(y => blockAt(16, y, -96)?.name)
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
