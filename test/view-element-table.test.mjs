// The element table's binary layout: what packs unpacks to the same elements, repeats share one record, ids index the lists.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { packElementTable, unpackList, ELEMENT_TEXELS, TABLE_WIDTH } from '../tools/view/element-table.mjs'

const face = (layer, extra = {}) => ({ layer, uv: [0, 0, 16, 16], rotation: 0, tint: 'none', cullface: null, ...extra })
const slab = { from: [0, 0, 0], to: [16, 8, 16], rotation: null, shade: true, faces: { up: face(3), down: face(3, { cullface: 'down' }), north: face(4, { uv: [0, 8, 16, 16] }) } }
const cross = { from: [0.75, 0, 8], to: [15.25, 16, 8], rotation: { origin: [8, 8, 8], axis: 'y', angle: -45, rescale: true }, shade: false, faces: { north: face(0, { tint: 'grass', rotation: 270 }), south: face(0, { tint: 'constant', tintIndex: 17, rotation: 90, cullface: 'west' }) } }

const cases = [
  ['a slab', [slab]],
  ['a rotated, rescaled, unshaded cross', [cross]],
  ['two elements', [slab, cross]],
  ['no elements', []]
]
for (const [name, list] of cases) {
  test(`packs and unpacks ${name}`, () => {
    const packed = packElementTable([list])
    assert.deepEqual(unpackList(packed, packed.ranges[0]), list)
  })
}

test('lists share element records and are addressed by offset and count', () => {
  const packed = packElementTable([[slab, cross], [cross], [slab]])
  assert.deepEqual(packed.ranges, [[0, 2], [2, 1], [3, 1]])
  assert.equal(packed.elementCount, 2)
  assert.equal(packed.idCount, 4)
  assert.deepEqual(unpackList(packed, packed.ranges[1]), [cross])
  assert.deepEqual(unpackList(packed, packed.ranges[2]), [slab])
})

test('the data is whole rows of TABLE_WIDTH RGBA texels, lists first', () => {
  const packed = packElementTable([[slab, cross]])
  assert.equal(packed.data.length, packed.rows * TABLE_WIDTH * 4)
  assert.equal(packed.listTexels, 1)
  assert.deepEqual([...packed.data.subarray(0, 4)], [0, 1, 0, 0])
  assert.deepEqual([...packed.data.subarray(packed.listTexels * 4, packed.listTexels * 4 + 4)], [0, 0, 0, 0])
  assert.equal(ELEMENT_TEXELS, 15)
})
