import test from 'node:test'
import assert from 'node:assert/strict'
import { stalkShape } from '../src/navigation/bamboo.mjs'

const close = (a, b) => Math.abs(a - b) < 1e-9

// minecraft-data's single fixed bamboo shape is the offset at the origin, where the server's seed is 0: an
// independent check that the formula (not just a transcription of minecraft-data) lands on the known block-local box
test('stalkShape: at the origin matches minecraft-data\'s fixed shape', () => {
  assert.deepEqual(stalkShape(0, 0), [0.15625, 0, 0.15625, 0.34375, 1, 0.34375])
})

for (const [name, x, z, expected] of [
  ['8,-43', 8, -43, [0.18958333333333333, 0, 0.45625, 0.3770833333333333, 1, 0.64375]],
  ['9,-43', 9, -43, [0.4895833333333333, 0, 0.18958333333333333, 0.6770833333333333, 1, 0.3770833333333333]]
]) {
  test(`stalkShape: ${name} pins to the formula's own offset`, () => {
    const shape = stalkShape(x, z)
    assert.ok(shape.every((v, i) => close(v, expected[i])), `${shape} !~ ${expected}`)
  })
}

// the 2026-10-01 incident: the server put a 0.6-wide body here, inside what looked (to the client's one fixed shape)
// like a dense grove; under the server's real per-cell offsets the body's box clears every stalk around it
test('stalkShape: the incident spot overlaps none of the 9 stalks around it', () => {
  const half = 0.3
  const bodyX = [8.692235292704533 - half, 8.692235292704533 + half]
  const bodyZ = [-42.5 - half, -42.5 + half]
  const overlaps = (x, z) => {
    const [minX, , minZ, maxX, , maxZ] = stalkShape(x, z)
    return minX + x < bodyX[1] && maxX + x > bodyX[0] && minZ + z < bodyZ[1] && maxZ + z > bodyZ[0]
  }
  const cells = [7, 8, 9].flatMap(x => [-44, -43, -42].map(z => [x, z]))
  assert.equal(cells.filter(([x, z]) => overlaps(x, z)).length, 0)
})
