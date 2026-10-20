// Why JavaScript: tests sight-kernel.mjs.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { castRays, cellLight, lazySections } from './sight-kernel.mjs'

// a world of cells given by `at(x, y, z)`: state id (1 blocks sight, 0 air, -1 unloaded) and light (sky << 4 | block)
const rig = (at, lightOf = () => 0xF0, { near = 0 } = {}) => {
  const marks = new Map()
  const records = []
  const ctx = {
    raw: { stateAt: at, lightAt: lightOf },
    sections: lazySections(),
    sight: Uint8Array.from([0, 1]),
    visible: Uint8Array.from({ length: 256 }, (_, l) => l >= 16 ? 1 : 0),
    base: 4,
    radius: 8,
    near,
    marksOf: (cx, cy, cz) => {
      const k = `${cx},${cy},${cz}`
      if (!marks.has(k)) marks.set(k, new Uint32Array(4096))
      return marks.get(k)
    },
    record: (x, y, z, id) => records.push([x, y, z, id]),
    markKind: id => id === 1 ? 2 : 1,
    thin: () => false
  }
  return { records, cast: () => castRays(ctx, 0.5, 64.5, 0.5, Float64Array.from([1, 0, 0]), 1) }
}

test('a ray records the cells it enters up to the first blocking cell, which is recorded too', () => {
  const { records, cast } = rig(x => x === 3 ? 1 : 0)
  assert.equal(cast(), 4)
  assert.deepEqual(records.map(r => [r[0], r[3]]), [[0, 0], [1, 0], [2, 0], [3, 1]])
})

test('a cell an earlier ray recorded is not recorded again, and still stops the ray', () => {
  const { records, cast } = rig(x => x === 3 ? 1 : 0)
  cast()
  assert.equal(cast(), 4)
  assert.equal(records.length, 4)
})

test('a cell too dark to make out is not recorded and the ray goes on; a blocking cell takes the light of the cell before it', () => {
  const { records, cast } = rig(x => x === 3 ? 1 : 0, x => x === 2 ? 0 : 0xF0)
  cast()
  assert.deepEqual(records.map(r => r[0]), [0, 1, 3])
})

test('an unloaded cell ends the ray unrecorded', () => {
  const { records, cast } = rig(x => x === 2 ? -1 : 0)
  assert.equal(cast(), 2)
  assert.deepEqual(records.map(r => r[0]), [0, 1])
})

test('a dark cell within near is recorded', () => {
  const { records, cast } = rig(() => 0, () => 0, { near: 2.4 })
  cast()
  assert.deepEqual(records.map(r => r[0]), [0, 1, 2])
})

// cellLight: the light before the cell seen from the eye at (0.5, 64.5, 0.5); -1 when no point of the cell is reachable
const lineCtx = (at, lightOf = () => 0xF0, withSections = false) => {
  const raw = { stateAt: at, lightAt: lightOf }
  const src = withSections
    ? { sectionRange: () => ({ minY: 0, sections: 32 }),
        sectionStates: (cx, s, cz) => Uint16Array.from({ length: 4096 }, (_, i) => {
          const id = at((cx << 4) | (i & 15), (s << 4) | (i >> 8), (cz << 4) | ((i >> 4) & 15))
          return id < 0 ? 0xFFFF : id
        }) }
    : null
  return { raw, sight: Uint8Array.from([0, 1]), src, blocks: () => true }
}

for (const withSections of [false, true]) {
  const tag = withSections ? 'section arrays' : 'stateAt'
  test(`cellLight (${tag}): the light of the cell before the target along a clear line`, () => {
    const ctx = lineCtx(() => 0, (x) => x === 2 ? 0x35 : 0xF0, withSections)
    assert.equal(cellLight(ctx, 0.5, 64.5, 0.5, 3, 64, 0), 0x35)
  })

  test(`cellLight (${tag}): a blocking cell on every line to the cell gives -1`, () => {
    const ctx = lineCtx((x) => x === 1 ? 1 : 0, undefined, withSections)
    assert.equal(cellLight(ctx, 0.5, 64.5, 0.5, 3, 64, 0), -1)
  })

  test(`cellLight (${tag}): a face point reaches a cell whose centre line is blocked`, () => {
    const ctx = lineCtx((x, y, z) => x === 2 && y === 64 && z === 0 ? 1 : 0, undefined, withSections)
    ctx.blocks = (id, x, y, z, ox, oy, oz, tx, ty) => ty < 64.5
    assert.notEqual(cellLight(ctx, 0.5, 64.5, 0.5, 4, 64, 0), -1)
  })

  test(`cellLight (${tag}): an unloaded cell on the way gives -1`, () => {
    const ctx = lineCtx((x) => x === 2 ? -1 : 0, undefined, withSections)
    assert.equal(cellLight(ctx, 0.5, 64.5, 0.5, 4, 64, 0), -1)
  })
}
