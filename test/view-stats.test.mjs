import test from 'node:test'
import assert from 'node:assert/strict'
import { regionStats, luminance } from '../tools/view/stats.mjs'

const image = (width, height, pixel) => ({ width, height, rgba: Uint8Array.from({ length: width * height * 4 }, (_, i) => pixel(Math.floor(i / 4) % width, Math.floor(i / 4 / width))[i % 4]) })
const flat = image(4, 4, () => [10, 20, 30, 255])
const halves = image(4, 4, x => (x < 2 ? [0, 0, 0, 255] : [100, 100, 100, 255]))

const cases = [
  ['flat image', flat, { x0: 0, y0: 0, x1: 3, y1: 3 }, { mean: [10, 20, 30], std: 0 }],
  ['two halves', halves, { x0: 0, y0: 0, x1: 3, y1: 3 }, { mean: [50, 50, 50], std: 50 }],
  ['sub region', halves, { x0: 2, y0: 1, x1: 3, y1: 2 }, { mean: [100, 100, 100], std: 0 }]
]

test('regionStats mean and std', () => {
  for (const [name, img, region, want] of cases) {
    const s = regionStats(img, region)
    assert.deepEqual(s.mean, want.mean, name)
    assert.ok(Math.abs(s.std - want.std) < 1e-9, name)
  }
})

test('regionStats fraction counts matching pixels', () => {
  const s = regionStats(halves, { x0: 0, y0: 0, x1: 3, y1: 3 })
  assert.equal(s.fraction(([r]) => r > 50), 0.5)
  assert.equal(s.fraction(() => true), 1)
})

test('luminance weights green most', () => {
  assert.ok(luminance([0, 255, 0]) > luminance([255, 0, 0]))
  assert.ok(Math.abs(luminance([255, 255, 255]) - 255) < 1e-9)
})
