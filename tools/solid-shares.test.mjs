// Why JavaScript: node --test file for tools/solid-shares.mjs (a Node tool over recorded chunk columns).
import test from 'node:test'
import assert from 'node:assert/strict'
import { betaOf, columnCounts, fitWindows, reserved, solidStates } from './solid-shares.mjs'

const grid = (counts) => {
  const columns = new Map()
  for (let cx = -2; cx <= 2; cx++) {
    for (let cz = -2; cz <= 2; cz++) {
      const [wall, open] = counts(cx, cz).map(c => Int32Array.from(c))
      columns.set(`${cx},${cz}`, { wall, open })
    }
  }
  return columns
}

test('a Beta by moments has the mean and variance it was fitted to', () => {
  const { a, b } = betaOf(0.8, 0.01, 1e6)
  const n = a + b
  assert.ok(Math.abs(a / n - 0.8) < 1e-12)
  assert.ok(Math.abs(a * b / (n * n * (n + 1)) - 0.01) < 1e-12)
  assert.deepEqual(betaOf(0, 0, 9), { a: 0, b: 9 }, 'a share known exactly is worth the window')
})

test('only windows with all 9 columns and an open block count, each its share of walls among the walls and open blocks', () => {
  const fits = fitWindows(grid(() => [[0, 1024, 0], [0, 1024, 3072]]), -64)
  assert.deepEqual(fits.map(f => [f.y, f.n]), [[-64, 9], [-48, 9], [-32, 9]])
  assert.deepEqual(fits.map(f => (f.a / (f.a + f.b)).toFixed(3)), ['0.500', '0.200', '0.200'])
  assert.deepEqual(fitWindows(grid(() => [[0], [0]]), -64), [], 'all rock: sight meets nothing')
})

test('a wall is a solid block with an open one beside it in the column', () => {
  const solid = new Uint8Array(2 * 4096).fill(1)
  solid[4096 + 5 * 256 + 5 * 16 + 5] = 0
  solid[0] = 0
  const { wall, open } = columnCounts(solid)
  assert.deepEqual([[...wall], [...open]], [[3, 6], [1, 1]], 'a corner cell has 3 sides in the column, a middle one 6')
})

test('solid is the engine rule by block kind, and the fixture plot grid is left out', () => {
  assert.deepEqual([...solidStates([0, 1, 2, 3, 4, 5, 6])], [0, 1, 0, 0, 0, 0, 1], 'open, solid, water, lava, climb, openable, narrow')
  assert.deepEqual([[1246, 1246], [1245, 1300], [1300, 1245]].map(([cx, cz]) => reserved(cx, cz)), [true, false, false])
})
