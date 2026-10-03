import { test } from 'node:test'
import assert from 'node:assert/strict'
import { fixtureSnapshot } from './fixture.mjs'
import { defaultStateTable } from './blocks.mjs'
import { boxesNear, freeMask, regions, segmentFree } from './space.mjs'
import { bambooBox } from '../offsets.mjs'

const table = defaultStateTable()
const W = 0.31
const N = 17

// free centre positions of the cell at (x, z) with feet at y, as [px, pz] pairs
const freeAt = (snapshot, x, y, z, lo = y, hi = y + 1.8) => {
  const mask = freeMask(boxesNear(snapshot, table, x, y, z, lo, hi), x, z)
  return Array.from(mask, (v, k) => v ? [x + (k % N) / 16, z + Math.floor(k / N) / 16] : null).filter(Boolean)
}

test('an empty cell is free everywhere', () => {
  const snapshot = fixtureSnapshot({ fill: [[-2, 0, -2, 2, 0, 2, 'stone']] })
  assert.equal(freeAt(snapshot, 0, 1, 0).length, N * N)
})

test('stone to the east blocks positions flush with or beyond 1 - W', () => {
  const snapshot = fixtureSnapshot({ fill: [[-2, 0, -2, 2, 0, 2, 'stone'], [1, 1, -1, 1, 3, 1, 'stone']] })
  const free = freeAt(snapshot, 0, 1, 0)
  assert.equal(free.length, 12 * N) // px = 0..0.6875 stay free
  assert.deepEqual([Math.min(...free.map(p => p[0])), Math.max(...free.map(p => p[0]))], [0, 0.6875])
})

test('a floor box touching the feet and a ceiling above the head do not intrude', () => {
  const snapshot = fixtureSnapshot({ fill: [[-2, 0, -2, 2, 0, 2, 'stone'], [-2, 3, -2, 2, 3, 2, 'stone']] })
  assert.equal(freeAt(snapshot, 0, 1, 0, 1, 2.8).length, N * N)
})

test('an unloaded neighbour counts as a full box', () => {
  const snapshot = fixtureSnapshot({ blocks: [[0, 0, 0, 'stone']] })
  const free = freeAt(snapshot, 15, 1, 5)
  assert.equal(Math.max(...free.map(p => p[0])), 15.6875)
})

test('water has no boxes', () => {
  const snapshot = fixtureSnapshot({ fill: [[-2, 1, -2, 2, 2, 2, 'water']] })
  assert.equal(boxesNear(snapshot, table, 0, 1, 0, 1, 2.8).length, 0)
})

test('regions: one component for an open grid, centre-most point', () => {
  const mask = new Uint8Array(N * N).fill(1)
  assert.deepEqual(regions(mask), [{ size: N * N, px: 8, pz: 8 }])
})

test('regions: splits into 4-connected components; diagonal does not join', () => {
  const mask = new Uint8Array(N * N)
  mask[0] = 1 // (0,0)
  mask[1 * N + 1] = 1 // (1,1) diagonal
  mask[16 * N + 16] = 1
  mask[16 * N + 15] = 1
  assert.deepEqual(regions(mask).sort((a, b) => a.size - b.size || a.px - b.px), [
    { size: 1, px: 0, pz: 0 }, { size: 1, px: 1, pz: 1 }, { size: 2, px: 15, pz: 16 }
  ])
})

test('regions: ties go to the lowest j then i', () => {
  const mask = new Uint8Array(N * N)
  ;[[8, 7], [9, 7], [9, 8], [9, 9], [8, 9]].forEach(([i, j]) => { mask[j * N + i] = 1 })
  assert.deepEqual(regions(mask), [{ size: 5, px: 8, pz: 7 }])
})

// cocoa between two jungle logs 2 apart (gap cells x = 1, 2), pods in the row z = 0. Pod extents in x from the log face:
// age 0 0.3125, age 1 0.4375, age 2 0.5625.
const gapWorld = (age, sides, podY) => fixtureSnapshot({
  fill: [[0, 0, -1, 3, 0, 1, 'stone'], [0, 1, -1, 0, 3, 1, 'jungle_log'], [3, 1, -1, 3, 3, 1, 'jungle_log']],
  blocks: [
    ...(sides.includes('left') ? [[1, podY, 0, 'cocoa', { age, facing: 'west' }]] : []),
    ...(sides.includes('right') ? [[2, podY, 0, 'cocoa', { age, facing: 'east' }]] : [])
  ]
})

const range = (from, to) => Array.from({ length: Math.round((to - from) * 16) + 1 }, (_, i) => from + i / 16)
const gapFree = snapshot => [...freeAt(snapshot, 1, 1, 0), ...freeAt(snapshot, 2, 1, 0)]
const uniqueSorted = values => [...new Set(values)].sort((a, b) => a - b)

// [age, sides, min px, max px] on the 1/16 grid: the free centre range along x
const gapCases = [0, 1, 2].flatMap(age => {
  const left = [1.625, 1.75, 1.875][age]
  const right = [2.375, 2.25, 2.125][age]
  return [['both', left, right], ['left', left, 2.6875], ['right', 1.3125, right]]
    .flatMap(([side, min, max]) => [1, 2].map(podY => [age, side === 'both' ? ['left', 'right'] : [side], min, max, podY]))
})

// narrow pods (age 0, 1) leave a sliver at the cell's z edges where the body slides past them, so look at the middle rows
// pz in 0.25..0.75, which every pod's z extent overlaps
const middle = free => free.filter(([, pz]) => pz % 1 >= 0.25 && pz % 1 <= 0.75)

for (const [age, sides, min, max, podY] of gapCases) {
  test(`cocoa gap: age ${age}, pods ${sides}, pod row y=${podY}`, () => {
    const free = middle(gapFree(gapWorld(age, sides, podY)))
    assert.deepEqual(uniqueSorted(free.map(p => p[0])), range(min, max))
    assert.equal(new Set(free.map(p => p.join())).size, range(min, max).length * 9)
  })
}

test('cocoa age 2 both sides: free positions lie within 0.127 of the shared boundary', () => {
  const free = gapFree(gapWorld(2, ['left', 'right'], 1))
  free.forEach(([px]) => assert.ok(Math.abs(px - 2) <= 0.127))
  assert.ok(regions(freeMask(boxesNear(gapWorld(2, ['left', 'right'], 1), table, 1, 1, 0, 1, 2.8), 1, 0)).length >= 1)
})

test('cocoa pod above the head is ignored', () => {
  const free = gapFree(gapWorld(2, ['left', 'right'], 3))
  assert.deepEqual(uniqueSorted(free.map(p => p[0])), range(1.3125, 2.6875))
})

test('a one-wide gap between logs with an age 2 pod has no free position', () => {
  const snapshot = fixtureSnapshot({
    fill: [[0, 0, -1, 2, 0, 1, 'stone'], [0, 1, -1, 0, 3, 1, 'jungle_log'], [2, 1, -1, 2, 3, 1, 'jungle_log']],
    blocks: [[1, 1, 0, 'cocoa', { age: 2, facing: 'west' }]]
  })
  assert.deepEqual(freeAt(snapshot, 1, 1, 0), [])
})

const bambooWorld = fixtureSnapshot({
  fill: [[0, 0, 0, 2, 0, 2, 'stone'], [0, 1, 0, 2, 3, 2, 'bamboo']]
})

const bruteForce = () => {
  const cells = [0, 1, 2].flatMap(x => [0, 1, 2].map(z => ({ x, z, box: bambooBox(x, z) })))
  return Array.from({ length: N * N }, (_, k) => {
    const px = 1 + (k % N) / 16
    const pz = 1 + Math.floor(k / N) / 16
    return cells.some(({ x, z, box }) => px + W > x + box[0] - 1e-4 && px - W < x + box[3] + 1e-4 &&
      pz + W > z + box[2] - 1e-4 && pz - W < z + box[5] + 1e-4) ? 0 : 1
  })
}

test('bamboo grove: free space matches a brute-force check against bambooBox', () => {
  const mask = freeMask(boxesNear(bambooWorld, table, 1, 1, 1, 1, 2.8), 1, 1)
  const expected = bruteForce()
  assert.deepEqual(Array.from(mask), expected)
  const count = expected.reduce((a, b) => a + b, 0)
  assert.ok(count > 0 && count < N * N)
})

// segments in the age 2 / both sides gap: [from, to, expected]
const segmentCases = [
  [{ x: 2, z: -0.5 }, { x: 2, z: 1.5 }, true],
  [{ x: 1.9, z: 0.5 }, { x: 2.1, z: 0.5 }, true],
  [{ x: 1.2, z: 0.5 }, { x: 1.9, z: 0.5 }, false],
  [{ x: 2, z: 0.5 }, { x: 2.6, z: 0.5 }, false],
  [{ x: 2, z: 0.5 }, { x: 2, z: 0.5 }, true]
]

for (const [from, to, expected] of segmentCases) {
  test(`segmentFree ${JSON.stringify(from)} -> ${JSON.stringify(to)}`, () => {
    assert.equal(segmentFree(gapWorld(2, ['left', 'right'], 1), table, from, to, 1, 2.8), expected)
  })
}
