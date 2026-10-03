// The scan side of the block-issues log: counting flagged blocks and unknown state ids per column file.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import zlib from 'node:zlib'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { columnFormat } from '../tools/view/web-format.mjs'
import { scanColumn, createTotals } from '../tools/view/block-scan.mjs'

const VERSION = '1.21.4'
const Chunk = makeChunkClass(VERSION)
const format = columnFormat(VERSION)

const columnFile = ({ cx, cz, body, blocks }) => {
  const column = new Chunk({ minY: -64, worldHeight: 384 })
  blocks.forEach(([x, y, z, id]) => column.setBlockStateId({ x, y, z }, id))
  const sections = column.dump()
  const header = Buffer.from(JSON.stringify({ v: 1, x: cx, z: cz, t: 1, body, mcVersion: VERSION, minY: -64, worldHeight: 384, parts: [{ name: 'sections', len: sections.length }] }))
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  return zlib.deflateSync(Buffer.concat([length, header, sections]))
}

// ids 1 and 2 are flagged ('a', 'b'), 5 belongs to no block, ids from 10 are past the table
const flagged = { names: ['a', 'b'], lookup: Uint16Array.from([0, 1, 2, 0, 0, 0, 0, 0, 0, 0]), known: Uint8Array.from([1, 1, 1, 1, 1, 0, 1, 1, 1, 1]), stateCount: 10 }
const scan = (cx, cz, body, blocks) => scanColumn(columnFile({ cx, cz, body, blocks }), { format, flagged })

test('scanColumn counts flagged names with the first position, and unknown ids with up to five positions', () => {
  const blocks = [[1, 70, 2, 1], [2, 70, 2, 1], [3, 70, 2, 2], ...[0, 1, 2, 3, 4, 5, 6].map(x => [x, 80, 0, 12])]
  const { agent, counts, unknown } = scan(1, -1, 'Ann', blocks)
  assert.equal(agent, 'Ann')
  assert.deepEqual([...counts].map(([name, c]) => [name, c.count, c.first]), [['a', 2, { x: 17, y: 70, z: -14 }], ['b', 1, { x: 19, y: 70, z: -14 }]])
  assert.equal(unknown.get(12).count, 7)
  assert.equal(unknown.get(12).firsts.length, 5)
})

test('an id in no block counts as unknown even inside the table range', () => {
  assert.deepEqual([...scan(0, 0, 'Ann', [[0, 70, 0, 5]]).unknown].map(([id, u]) => [id, u.count]), [[5, 1]])
})

test('totals merge unknown ids into one record per contiguous range, replace a rewritten column, and keep five positions', () => {
  const totals = createTotals('w')
  totals.set('0.0.bin', scan(0, 0, 'Ann', [[0, 70, 0, 11], [1, 70, 0, 12], [2, 70, 0, 12], [3, 70, 0, 20]]))
  totals.set('1.0.bin', scan(1, 0, 'Bob', [[0, 70, 0, 13], [1, 70, 0, 5]]))
  const seen = totals.seen()
  assert.deepEqual([...seen].filter(([name]) => name.startsWith('state:')).map(([name, v]) => [name, v.count]), [['state:5-5', 1], ['state:11-13', 4], ['state:20-20', 1]])
  assert.deepEqual(seen.get('state:11-13').first, { world: 'w', x: 0, y: 70, z: 0, agent: 'Ann' })
  assert.deepEqual(seen.get('state:11-13').firsts.map(p => p.agent), ['Ann', 'Ann', 'Ann', 'Bob'])
  totals.set('0.0.bin', scan(0, 0, 'Ann', [[0, 70, 0, 12]]))
  assert.deepEqual([...totals.seen()].map(([name, v]) => [name, v.count]), [['state:5-5', 1], ['state:12-13', 2]])
})
