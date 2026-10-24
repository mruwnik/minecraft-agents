// Why JavaScript: node --test file for tools/solid-shares.mjs (a Node tool over recorded chunk columns).
import test from 'node:test'
import assert from 'node:assert/strict'
import { betaOf, fitWindows } from './solid-shares.mjs'

const grid = (counts) => {
  const columns = new Map()
  for (let cx = -2; cx <= 2; cx++) for (let cz = -2; cz <= 2; cz++) columns.set(`${cx},${cz}`, Int32Array.from(counts(cx, cz)))
  return columns
}

test('a Beta by moments has the mean and variance it was fitted to', () => {
  const { a, b } = betaOf(0.8, 0.01, 1e6)
  const n = a + b
  assert.ok(Math.abs(a / n - 0.8) < 1e-12)
  assert.ok(Math.abs(a * b / (n * n * (n + 1)) - 0.01) < 1e-12)
  assert.deepEqual(betaOf(0, 0, 9), { a: 0, b: 9 }, 'a share known exactly is worth the window')
})

test('only windows with all 9 columns count, each its share of solid blocks over the sections it holds', () => {
  const fits = fitWindows(grid(() => [4096, 2048, 0]), -64)
  assert.deepEqual(fits.map(f => [f.y, f.n]), [[-64, 9], [-48, 9], [-32, 9]])
  assert.deepEqual(fits.map(f => (f.a / (f.a + f.b)).toFixed(3)), ['0.750', '0.500', '0.250'])
})
