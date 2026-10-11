// Why JavaScript: tests the JS collision clip (clip-tolerance.mjs) on prismarine-physics' boxes.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import './prim-base.mjs'

const AABB = createRequire(import.meta.url)('prismarine-physics/lib/aabb')
const ULP = 2 ** -48 // one rounding step at 16
const block = () => new AABB(16, 16, 16, 17, 17, 17)
// a box over the block's middle (16.2..16.8 each way) but for the ranges given by axis
const body = ranges => {
  const b = { x: [16.2, 16.8], y: [16.2, 16.8], z: [16.2, 16.8], ...ranges }
  return new AABB(b.x[0], b.y[0], b.z[0], b.x[1], b.y[1], b.z[1])
}
const clip = (axis, box, move) => block()[`computeOffset${axis.toUpperCase()}`](box, move)

for (const [axis, across] of [['x', 'y'], ['y', 'z'], ['z', 'x']]) {
  test(`a box a rounding step into a face is held at it (${axis})`, () => {
    assert.equal(clip(axis, body({ [axis]: [15.4, 16 + ULP] }), 0.02), -ULP)
    assert.equal(clip(axis, body({ [axis]: [17 - ULP, 17.6] }), -0.02), ULP)
  })

  test(`a box clear of a face moves up to it (${axis})`, () => {
    assert.equal(clip(axis, body({ [axis]: [15.25, 15.75] }), 0.125), 0.125)
    assert.equal(clip(axis, body({ [axis]: [15.375, 15.875] }), 0.5), 0.125)
    assert.equal(clip(axis, body({ [axis]: [17.125, 17.625] }), -0.5), -0.125)
  })

  test(`a box a rounding step into a block beside its way passes it (${axis})`, () => {
    assert.equal(clip(axis, body({ [axis]: [15.375, 15.875], [across]: [15.4, 16 + ULP] }), 0.5), 0.5)
    assert.equal(clip(axis, body({ [axis]: [15.375, 15.875], [across]: [17 - ULP, 17.6] }), 0.5), 0.5)
  })
}
