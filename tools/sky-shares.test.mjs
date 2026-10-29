// Why JavaScript: node --test file for tools/sky-shares.mjs (a Node tool over recorded chunk columns).
import test from 'node:test'
import assert from 'node:assert/strict'
import { betaOf, columnSky, fitWindows, reserved, solidStates } from './sky-shares.mjs'

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

test('each level counts the open blocks at that sky light or more against the other open ones; solid ones not at all', () => {
  const solid = new Uint8Array(4096)
  const sky = new Uint8Array(4096)
  sky[0] = 15
  sky[1] = 8
  solid[2] = 1
  sky[2] = 15
  const levels = columnSky(solid, sky)
  assert.equal(levels.length, 15)
  assert.deepEqual([1, 8, 9, 15].map(l => [levels[l - 1].wall[0], levels[l - 1].open[0]]), [[2, 4093], [2, 4093], [1, 4094], [1, 4094]])
})

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

test('solid is the engine rule by block kind, and the fixture plot grid is left out', () => {
  assert.deepEqual([...solidStates([0, 1, 2, 3, 4, 5, 6])], [0, 1, 0, 0, 0, 0, 1], 'open, solid, water, lava, climb, openable, narrow')
  assert.deepEqual([[1246, 1246], [1245, 1300], [1300, 1245]].map(([cx, cz]) => reserved(cx, cz)), [true, false, false])
})
