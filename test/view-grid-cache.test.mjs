// The incremental grid cache: one chunk-aligned, full-height grid that is patched when a column object is replaced.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { buildGrid, gridCache } from '../tools/view/grid.mjs'

const Chunk = makeChunkClass('1.21.4')
const stone = Chunk.registry.blocksByName.stone.defaultState
const dirt = Chunk.registry.blocksByName.dirt.defaultState

const world = (blocks) => {
  const columns = new Map()
  for (const [x, y, z, id] of blocks) {
    const key = `${x >> 4}.${z >> 4}`
    if (!columns.has(key)) columns.set(key, new Chunk({ minY: -64, worldHeight: 384 }))
    columns.get(key).setBlockStateId({ x: x & 15, y, z: z & 15 }, id)
  }
  return { columns, column: (cx, cz) => columns.get(`${cx}.${cz}`) ?? null }
}

const cell = ({ origin, size, data }, x, y, z) => data[((y - origin.y) * size.z + (z - origin.z)) * size.x + (x - origin.x)]

for (const [eye, across] of [
  [{ x: 4.5, y: 71.6, z: 4.5 }, 32],
  [{ x: -3.2, y: 10, z: -17.9 }, 20],
  [{ x: 100.5, y: 300, z: -100.5 }, 8],
  [{ x: 0, y: -70, z: 0 }, 16]
]) {
  test(`geometry is chunk-aligned and full height for eye ${eye.x},${eye.y},${eye.z} across ${across}`, () => {
    const blank = new Chunk({ minY: -64, worldHeight: 384 })
    const grid = gridCache().get({ column: () => blank, eye, across })
    const [ex, ez] = [Math.floor(eye.x), Math.floor(eye.z)]
    const cxLo = (ex - across) >> 4
    const czLo = (ez - across) >> 4
    assert.equal(grid.origin.x, cxLo * 16)
    assert.equal(grid.origin.z, czLo * 16)
    assert.equal(grid.size.x, (((ex + across) >> 4) - cxLo + 1) * 16)
    assert.equal(grid.size.z, (((ez + across) >> 4) - czLo + 1) * 16)
    assert.equal(grid.origin.y, Math.min(-64, Math.floor(eye.y) - 1))
    assert.equal(grid.origin.y + grid.size.y - 1, Math.max(319, Math.floor(eye.y) + 1))
  })
}

test('geometry for the common eye is -64..319', () => {
  const { column } = world([[5, 70, 5, stone]])
  const grid = gridCache().get({ column, eye: { x: 4.5, y: 71.6, z: 4.5 }, across: 32 })
  assert.deepEqual([grid.origin.y, grid.size.y], [-64, 384])
})

test('all columns null falls back to eye-centred vertical range', () => {
  const grid = gridCache().get({ column: () => null, eye: { x: 0.5, y: 10, z: 0.5 }, across: 8 })
  assert.deepEqual([grid.origin.y, grid.size.y], [-38, 97])
  assert.equal(grid.top, -Infinity)
})

test('cells hold known blocks and match buildGrid inside its box', () => {
  const { column } = world([[5, 70, 5, stone], [-3, 64, 20, dirt], [5, 200, 5, stone]])
  const eye = { x: 4.5, y: 71.62, z: 4.5 }
  const grid = gridCache().get({ column, eye, across: 32 })
  assert.equal(cell(grid, 5, 70, 5), stone)
  assert.equal(cell(grid, -3, 64, 20), dirt)
  assert.equal(cell(grid, 5, 200, 5), stone)
  const ref = buildGrid({ column, eye, across: 32, up: 48 })
  const mismatches = []
  for (let y = ref.origin.y; y < ref.origin.y + ref.size.y; y++) {
    for (let z = ref.origin.z; z < ref.origin.z + ref.size.z; z++) {
      for (let x = ref.origin.x; x < ref.origin.x + ref.size.x; x++) {
        if (cell(grid, x, y, z) !== cell(ref, x, y, z)) mismatches.push([x, y, z])
      }
    }
  }
  assert.deepEqual(mismatches, [])
})

test('moving the eye inside the same chunk range reuses the grid', () => {
  const { column } = world([[5, 70, 5, stone]])
  const cache = gridCache()
  const a = cache.get({ column, eye: { x: 4.5, y: 71.6, z: 4.5 }, across: 32 })
  const b = cache.get({ column, eye: { x: 5.5, y: 72.1, z: 4.9 }, across: 32 })
  assert.ok(b === a)
  assert.equal(b.data, a.data)
  assert.deepEqual(cache.stats(), { rebuilds: 1, patches: 0 })
})

test('a replaced column is patched in place', () => {
  const before = world([[5, 70, 5, stone]])
  const after = world([[6, 71, 5, stone]])
  let current = before.column
  const cache = gridCache()
  const eye = { x: 4.5, y: 71.6, z: 4.5 }
  const a = cache.get({ column: (cx, cz) => current(cx, cz), eye, across: 32 })
  assert.equal(cell(a, 5, 70, 5), stone)
  current = after.column
  const b = cache.get({ column: (cx, cz) => current(cx, cz), eye, across: 32 })
  assert.ok(b === a)
  assert.equal(cell(b, 5, 70, 5), 0)
  assert.equal(cell(b, 6, 71, 5), stone)
  assert.deepEqual(cache.stats(), { rebuilds: 1, patches: 1 })
})

test('a column replaced with null is cleared', () => {
  const w = world([[5, 70, 5, stone], [20, 70, 5, dirt]])
  let current = w.column
  const cache = gridCache()
  const eye = { x: 4.5, y: 71.6, z: 4.5 }
  const a = cache.get({ column: (cx, cz) => current(cx, cz), eye, across: 32 })
  current = (cx, cz) => (cx === 0 && cz === 0 ? null : w.column(cx, cz))
  const b = cache.get({ column: (cx, cz) => current(cx, cz), eye, across: 32 })
  assert.ok(b === a)
  assert.equal(cell(b, 5, 70, 5), 0)
  assert.equal(cell(b, 20, 70, 5), dirt)
  assert.equal(cache.stats().patches, 1)
})

test('crossing a chunk boundary that changes the range rebuilds', () => {
  const { column } = world([[5, 70, 5, stone]])
  const cache = gridCache()
  const a = cache.get({ column, eye: { x: 4.5, y: 71.6, z: 4.5 }, across: 32 })
  const b = cache.get({ column, eye: { x: 20.5, y: 71.6, z: 4.5 }, across: 32 })
  assert.notEqual(b.data, a.data)
  assert.equal(cell(b, 5, 70, 5), stone)
  assert.deepEqual(cache.stats(), { rebuilds: 2, patches: 0 })
})

test('top is the highest block and a patch raising a block raises it', () => {
  const before = world([[5, 70, 5, stone], [-3, 64, 20, dirt]])
  const after = world([[5, 120, 5, stone]])
  let current = before.column
  const cache = gridCache()
  const eye = { x: 4.5, y: 71.6, z: 4.5 }
  const a = cache.get({ column: (cx, cz) => current(cx, cz), eye, across: 32 })
  assert.equal(a.top, 70)
  current = after.column
  const b = cache.get({ column: (cx, cz) => current(cx, cz), eye, across: 32 })
  assert.equal(b.top, 120)
})
