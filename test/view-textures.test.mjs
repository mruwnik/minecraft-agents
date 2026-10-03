// The browser's texture array: mip chains with alpha-coverage preservation, and the level-major byte layout.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { decodePng } from '../src/vision/renderer.mjs'
import { mipChain, textureSet } from '../tools/view/textures.mjs'

const textureDir = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'textures')
const SIZES = [16, 8, 4, 2, 1]
const texture = pixel => {
  const rgba = new Uint8Array(16 * 16 * 4)
  for (let i = 0; i < 256; i++) rgba.set(pixel(i % 16, Math.floor(i / 16)), i * 4)
  return rgba
}
const coverage = level => {
  let n = 0
  for (let i = 3; i < level.length; i += 4) if (level[i] >= 128) n++
  return n / (level.length / 4)
}
const texel = (bytes, level, layer, layers, i) => {
  const before = SIZES.slice(0, level).reduce((s, n) => s + layers * n * n * 4, 0)
  return bytes.subarray(before + layer * SIZES[level] ** 2 * 4 + i * 4, before + layer * SIZES[level] ** 2 * 4 + i * 4 + 4)
}

test('mip level sizes', () => {
  assert.deepEqual(mipChain(texture(() => [1, 2, 3, 255])).map(l => l.length), SIZES.map(n => n * n * 4))
})

test('an opaque uniform texture stays uniform at every level', () => {
  for (const level of mipChain(texture(() => [10, 20, 30, 255]))) {
    for (let i = 0; i < level.length; i += 4) assert.deepEqual([...level.subarray(i, i + 4)], [10, 20, 30, 255])
  }
})

test('transparent texels carry the mean colour, not black', () => {
  const chain = mipChain(texture((x, y) => (x + y) % 2 ? [200, 100, 50, 255] : [0, 0, 0, 0]))
  assert.deepEqual([...chain[0].subarray(0, 4)], [0, 0, 0, 0].map((v, i) => i < 3 ? [200, 100, 50][i] : 0))
  assert.deepEqual([...chain[4].subarray(0, 3)], [200, 100, 50])
})

test('a fully transparent 2x2 block gets the texture mean colour', () => {
  const chain = mipChain(texture((x, y) => x < 8 && y < 8 ? [255, 0, 0, 255] : [0, 0, 0, 0]))
  assert.deepEqual([...chain[1].subarray((7 * 8 + 7) * 4, (7 * 8 + 7) * 4 + 4)], [255, 0, 0, 0])
})

const patterns = {
  scattered: (x, y) => ((x * 7 + y * 11 + x * y) % 3 < 1 ? 1 : 0),
  leafy: (x, y) => ((x * 3 + y * 5 + (x ^ y)) % 7 < 5 ? 1 : 0),
  sparse: (x, y) => (x % 4 === 0 && y % 4 === 0 ? 1 : 0),
  blob: (x, y) => (x > 3 && x < 12 && y > 3 && y < 12 ? 1 : 0)
}
for (const [name, on] of Object.entries(patterns)) {
  test(`binary alpha (${name}) keeps coverage near level 0 at levels 1..2`, () => {
    const chain = mipChain(texture((x, y) => [90, 90, 90, on(x, y) ? 255 : 0]))
    for (const level of [1, 2]) assert.ok(Math.abs(coverage(chain[level]) - coverage(chain[0])) <= 0.13, `${name} level ${level}`)
  })
}

test('non-binary alpha is not rescaled', () => {
  const chain = mipChain(texture(() => [10, 10, 10, 100]))
  for (const level of chain) assert.equal(level[3], 100)
})

test('textureSet byte length and level-major layout', () => {
  const names = ['stone', 'dirt']
  const set = textureSet(textureDir, names)
  assert.deepEqual([set.size, set.levels, set.layers], [16, 5, 2])
  assert.equal(set.bytes.length, SIZES.reduce((s, n) => s + 2 * n * n * 4, 0))
  const dirt = decodePng(fs.readFileSync(path.join(textureDir, 'dirt.png'))).rgba
  assert.deepEqual([...set.bytes.subarray(1024, 1028)], [...dirt.subarray(0, 4)])
  assert.deepEqual([...texel(set.bytes, 0, 1, 2, 5)], [...dirt.subarray(20, 24)])
})

test('tint is multiplied into level 0', () => {
  const [r, g, b] = texel(textureSet(textureDir, ['grass_block_top']).bytes, 4, 0, 1, 0)
  assert.ok(g > r && g > b)
})

test('an animated texture uses its first frame only', () => {
  const strip = decodePng(fs.readFileSync(path.join(textureDir, 'water_still.png')))
  assert.ok(strip.height > 16)
  const [r, g, b] = texel(textureSet(textureDir, ['water_still']).bytes, 0, 0, 1, 0)
  const tint = [63, 118, 228]
  assert.deepEqual([r, g, b], [0, 1, 2].map(c => Math.round(strip.rgba[c] * tint[c] / 255)))
})

test('a 32 wide texture is box-downsampled to 16x16', () => {
  const wide = fs.readdirSync(textureDir).filter(f => f.endsWith('.png')).find(f => decodePng(fs.readFileSync(path.join(textureDir, f))).width === 32)
  const set = textureSet(textureDir, [wide.replace(/\.png$/, '')])
  assert.equal(set.bytes.length, SIZES.reduce((s, n) => s + n * n * 4, 0))
})
