import test from 'node:test'
import assert from 'node:assert/strict'
import vec3 from 'vec3'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { serverShapes, withServerShapes, wrapBlockAt } from './offset-shapes.mjs'
import { bambooBox, blockOffset } from './offsets.mjs'

const { Vec3 } = vec3
const registry = prismarineRegistry('26.1')
const Block = prismarineBlock(registry)

const blockNamed = (name, x, y, z) => {
  const block = Block.fromStateId(registry.blocksByName[name].defaultState, 0)
  block.position = new Vec3(x, y, z)
  return block
}
const close = (a, b, eps = 1e-9) => assert.ok(Math.abs(a - b) < eps, `${a} vs ${b}`)
const closeBox = (a, b) => b.forEach((v, i) => close(a[i], v))

for (const [x, z] of [[0, 0], [5, 9], [-3, 7], [-100, -250], [4087, 2907]]) {
  test(`bamboo at (${x}, ${z}) is re-centred on the vanilla offset`, () => {
    const [box] = serverShapes(blockNamed('bamboo', x, 64, z))
    closeBox(box, bambooBox(x, z))
  })
}

test('live wedge: stalk of cell (4087, 2907) spans z 2907.190..2907.377', () => {
  const [box] = serverShapes(blockNamed('bamboo', 4087, 64, 2907))
  close(2907 + box[2], 2907.190, 0.001)
  close(2907 + box[5], 2907.377, 0.001)
})

test('live wedge: stalk of cell (4092, 2907) ends at x 4092.844', () => {
  const [box] = serverShapes(blockNamed('bamboo', 4092, 64, 2907))
  close(4092 + box[3], 4092.844, 0.001)
})

test('the registry shared shapes array is not mutated', () => {
  const block = blockNamed('bamboo', 5, 64, 9)
  const shared = block.shapes
  const before = JSON.stringify(shared)
  withServerShapes(block)
  assert.equal(JSON.stringify(shared), before)
  assert.notEqual(block.shapes, shared)
})

test('stone is unchanged', () => {
  const block = blockNamed('stone', 5, 64, 9)
  assert.equal(serverShapes(block), block.shapes)
  assert.equal(withServerShapes(block).shapes, block.shapes)
})

test('pointed_dripstone is re-centred with max 0.125', () => {
  const block = blockNamed('pointed_dripstone', 5, 64, 9)
  const { dx, dz } = blockOffset(5, 9, 0.125)
  const shapes = serverShapes(block)
  shapes.forEach((s, i) => {
    const old = block.shapes[i]
    close((s[0] + s[3]) / 2, 0.5 + dx)
    close((s[2] + s[5]) / 2, 0.5 + dz)
    close(s[3] - s[0], old[3] - old[0])
    assert.equal(s[1], old[1])
    assert.equal(s[4], old[4])
  })
})

test('wrapBlockAt wraps once and passes null through', () => {
  let calls = 0
  const bot = { blockAt: pos => { calls++; return pos.x === 99 ? null : blockNamed('bamboo', pos.x, pos.y, pos.z) } }
  wrapBlockAt(bot)
  const wrapped = bot.blockAt
  wrapBlockAt(bot)
  assert.equal(bot.blockAt, wrapped)
  closeBox(bot.blockAt(new Vec3(5, 64, 9), false).shapes[0], bambooBox(5, 9))
  assert.equal(calls, 1)
  assert.equal(bot.blockAt(new Vec3(99, 64, 0)), null)
})
