// Why JavaScript: tests offset-shapes.mjs, which stays JS: Mineflayer boundary; patches bot.blockAt for the physics engine.
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
  const bot = { registry, blockAt: pos => { calls++; return pos.x === 99 ? null : blockNamed('bamboo', pos.x, pos.y, pos.z) } }
  wrapBlockAt(bot)
  const wrapped = bot.blockAt
  wrapBlockAt(bot)
  assert.equal(bot.blockAt, wrapped)
  closeBox(bot.blockAt(new Vec3(5, 64, 9), false).shapes[0], bambooBox(5, 9))
  assert.equal(calls, 1)
  assert.equal(bot.blockAt(new Vec3(99, 64, 0)), null)
})

test('wrapBlockAt returns ordinary blocks untouched and re-centres bamboo', () => {
  const stone = blockNamed('stone', 5, 64, 9)
  const shapes = stone.shapes
  const bot = {
    registry,
    blockAt: pos => pos.y === 64 ? stone : blockNamed('bamboo', pos.x, pos.y, pos.z)
  }
  wrapBlockAt(bot)
  const got = bot.blockAt(new Vec3(5, 64, 9))
  assert.equal(got, stone)
  assert.equal(got.shapes, shapes)
  closeBox(bot.blockAt(new Vec3(5, 65, 9)).shapes[0], bambooBox(5, 9))
})

test('live: pointed_dripstone tip (up) at (1037, 101, -2992) spans x 1037.4375..1037.8125, z -2991.8125..-2991.4375', () => {
  // live, a body walking west along z -2991.625 was held by the server at x 1038.12-1038.13 (= 1037.8125 + 0.31)
  const block = Block.fromProperties('pointed_dripstone', { thickness: 'tip', vertical_direction: 'up', waterlogged: false }, 0)
  block.position = new Vec3(1037, 101, -2992)
  const [box] = serverShapes(block)
  close(1037 + box[0], 1037.4375, 0.001)
  close(1037 + box[3], 1037.8125, 0.001)
  close(-2992 + box[2], -2991.8125, 0.001)
  close(-2992 + box[5], -2991.4375, 0.001)
})

test('live: bamboo at (1012, 101, -2992) spans x 1012.190..1012.377, z -2991.477..-2991.290', () => {
  // live, a body walking east along z -2991.30 was held at x 1011.88-1011.89 (= 1012.19 - 0.31)
  const [box] = serverShapes(blockNamed('bamboo', 1012, 101, -2992))
  close(1012 + box[0], 1012.190, 0.002)
  close(1012 + box[3], 1012.377, 0.002)
  close(-2992 + box[2], -2991.477, 0.002)
  close(-2992 + box[5], -2991.290, 0.002)
})
