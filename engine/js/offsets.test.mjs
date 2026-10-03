import { test } from 'node:test'
import assert from 'node:assert/strict'
import { blockOffset, bambooBox, OFFSET_MAX } from './offsets.mjs'
import { stalkShape } from '../../src/navigation/bamboo.mjs'

const positions = [[0, 0], [1, 0], [0, 1], [5, 7], [-1, -1], [-3, 8], [12, -9], [100, 200], [-250, 77], [3, 3],
  [-17, -42], [999, -999], [31, 64], [-8, 0], [0, -8], [64, 64], [-64, -64], [1234, 5678], [-1234, 5678], [7, -7]]

test('bambooBox equals the server-true stalkShape', () => {
  positions.forEach(([x, z]) => {
    const got = bambooBox(x, z)
    const want = stalkShape(x, z)
    got.forEach((v, i) => assert.ok(Math.abs(v - want[i]) < 1e-12, `${x},${z} #${i}`))
  })
})

test('blockOffset stays within the max and is deterministic', () => {
  positions.forEach(([x, z]) => {
    const a = blockOffset(x, z, 0.125)
    assert.deepEqual(a, blockOffset(x, z, 0.125))
    assert.ok(Math.abs(a.dx) <= 0.125 && Math.abs(a.dz) <= 0.125)
  })
})

test('OFFSET_MAX lists bamboo and dripstone', () => {
  assert.deepEqual(OFFSET_MAX, { bamboo: 0.25, pointed_dripstone: 0.125 })
})
