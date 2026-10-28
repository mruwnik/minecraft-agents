// Why JavaScript: node --test file for tools/solid-shares.mjs (a Node tool over recorded chunk columns).
import test from 'node:test'
import assert from 'node:assert/strict'
import { betaOf, columnCounts, fitReach, fitWindows, reserved, shrunkSlope, solidStates } from './solid-shares.mjs'

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

test('reach is the slope of the share at one height on the share at another, 0 where the source never varies', () => {
  // the open blocks of the two lowest sections vary together from column to column; the top two sections are alike in all
  const { slope, n } = fitReach(grid((cx, cz) => { const o = 1 + ((cx + 2) * 5 + cz + 2) % 4; return [[1, 1, 1, 1, 1], [o, o, 1, 1, 1]] }))
  assert.deepEqual(slope.map((r, s) => r[s]), [1, 1, 1, 1, 1])
  assert.ok(slope[0][1] > 0.5, 'the lowest two sections tied')
  assert.deepEqual(slope.slice(3).map(r => r.filter(v => v !== 1)), [[0, 0, 0, 0], [0, 0, 0, 0]], 'a share that never varies says nothing')
  assert.equal(n[0][1], 9, 'the windows with all 9 columns recorded')
})

test('a slope is shrunk toward 0 by its sampling variance and cut to [0, 1]', () => {
  const sums = pairs => pairs.reduce((e, [x, y]) => ({ n: e.n + 1, x: e.x + x, y: e.y + y, xy: e.xy + x * y, xx: e.xx + x * x, yy: e.yy + y * y }),
    { n: 0, x: 0, y: 0, xy: 0, xx: 0, yy: 0 })
  const tied = Array.from({ length: 100 }, (_, i) => [i / 100, i / 100])
  assert.ok(shrunkSlope(sums(tied)) > 0.99, 'many samples of a real tie keep it')
  const faint = Array.from({ length: 100 }, (_, i) => [0.5 + (i % 2) * 1e-6, (i % 2) * 0.3 + (i % 3) * 0.2])
  assert.ok(shrunkSlope(sums(faint)) < 0.01, 'a source that barely varies says next to nothing, however steep its least-squares slope')
  assert.equal(shrunkSlope(sums(tied.map(([x, y]) => [x, 1 - y]))), 0, 'never negative')
  assert.equal(shrunkSlope(sums(tied.map(([x, y]) => [x / 4, y]))), 1, 'never past 1')
})
