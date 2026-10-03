import { test } from 'node:test'
import assert from 'node:assert/strict'
import { materialInfo, materialPixels, textureLevels, KINDS } from '../tools/view/web/gl.mjs'

const stone = { name: 'stone', kind: 'cube', tex: [3, 4, -1], box: [0, 0, 0, 16, 16, 16], flags: 1, emit: 0, top: [1, 2, 3, 255], side: [4, 5, 6, 255], bottom: [7, 8, 9, 255] }
const torch = { name: 'torch', kind: 'cross', tex: [-1, 0, -1], box: [6, 0, 6, 10, 10, 10], flags: 17, emit: 14, top: [0, 0, 0, 0], side: [10.4, 20.6, 30, 99], bottom: [0, 0, 0, 0] }

const dispenser = { name: 'dispenser', kind: 'cube', tex: [-1, -1, -1], tex6: [5, 0, 0, 3, 8, 1], rot6: [0, 0, 1, 2, 0, 0], box: [0, 0, 0, 16, 16, 16], flags: 128, emit: 0 }
const fence = { name: 'oak_fence', kind: 'model', tex: [-1, -1, -1], elemOffset: 41622, elemCount: 3, flags: 0, emit: 0 }

const infoCases = [
  ['textured cube', [stone], [4, 5, 0, KINDS.cube, 1, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],
  ['cross with layer 0 side', [torch], [0, 1, 0, KINDS.cross, 17, 14, 0, 0, 6, 0, 6, 0, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],
  ['six-face cube: layer + 1, quarter turns in the high bits', [dispenser], [0, 0, 0, KINDS.cube, 128, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 0, 6, 1, 4097, 8196, 9, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],
  ['model: element offset and count in the last texel', [fence], [0, 0, 0, KINDS.model, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 41622, 3, 0, 0, 0, 0, 0, 0, 0, 0]],
  ['tints per face: group + 8 * constant index', [{ ...dispenser, tint6: [2, 0, 0, 0, 5 + 8 * 17, 1] }], [0, 0, 0, KINDS.cube, 128, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 0, 6, 1, 4097, 8196, 9, 2, 0, 0, 2, 0, 0, 0, 5 + 8 * 17, 1, 0, 0]],
]
for (const [name, materials, expected] of infoCases) {
  test(`materialInfo: ${name}`, () => assert.deepEqual([...materialInfo(materials)], expected))
}

test('materialInfo: eight texels of four u16 per material', () => assert.equal(materialInfo([stone, torch]).length, 64))

test('materialPixels: faces rounded, kind in the fourth texel red', () => {
  const px = materialPixels([torch])
  assert.deepEqual([...px.slice(4, 8)], [10, 21, 30, 99])
  assert.equal(px[12], KINDS.cross)
})

test('kinds match the table kinds', () => assert.deepEqual(KINDS, { cube: 0, box: 1, cross: 2, water: 3, lava: 4, model: 5 }))

const sizes = [[2, 4, 3], [3, 16, 5], [1, 8, 4]]
for (const [layers, size, levels] of sizes) {
  test(`textureLevels: ${layers} layers of ${size}, ${levels} levels`, () => {
    const bytes = new Uint8Array(Array.from({ length: levels }, (_, l) => (size >> l) ** 2 * 4 * layers).reduce((a, b) => a + b))
    const got = textureLevels(bytes, layers, size, levels)
    assert.deepEqual(got.map(g => g.length), Array.from({ length: levels }, (_, l) => (size >> l) ** 2 * 4 * layers))
    assert.deepEqual(got.map(g => g.byteOffset), got.map((_, l) => Array.from({ length: l }, (_, k) => (size >> k) ** 2 * 4 * layers).reduce((a, b) => a + b, 0)))
  })
}
