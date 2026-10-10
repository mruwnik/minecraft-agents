// Why JavaScript: node tests of gaze.mjs, the JS adapter's look guard.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import vec3 from 'vec3'
import { stares, avoidEnderStare } from './gaze.mjs'

const { Vec3 } = vec3
const enderman = (x, y, z) => ({ name: 'enderman', position: new Vec3(x, y, z) })

// a body at the origin (eyes 1.62 up) looking along -z (yaw 0), an enderman 10 blocks ahead
const fakeBot = (entities, blocks = {}) => {
  const looks = []
  const bot = {
    entity: { position: new Vec3(0, 64, 0), height: 1.62 },
    entities,
    blockAt: v => blocks[`${v.x},${v.y},${v.z}`] ?? null,
    look: async (yaw, pitch, force) => { looks.push({ yaw, pitch, force }) }
  }
  return { bot, looks }
}

test('a look at the enderman\'s eyes is a stare, one at its feet is not', () => {
  const eye = new Vec3(0, 65.62, 0)
  const target = new Vec3(0, 66.55, -10)
  assert.equal(stares(eye, 0, Math.atan2(0.93, 10), target), true)
  assert.equal(stares(eye, 0, Math.atan2(-1.6, 10), target), false)
  assert.equal(stares(eye, Math.PI, 0, target), false)
})

test('a look that would meet a seen enderman\'s eyes is moved off them, yaw kept', async () => {
  const { bot, looks } = fakeBot({ 1: enderman(0, 64, -10) })
  avoidEnderStare(bot)
  await bot.look(0, Math.atan2(0.93, 10), true)
  assert.equal(looks.length, 1)
  assert.equal(looks[0].yaw, 0)
  assert.equal(stares(new Vec3(0, 65.62, 0), looks[0].yaw, looks[0].pitch, new Vec3(0, 66.55, -10)), false)
  assert.equal(looks[0].force, true)
})

test('a look is left alone with no enderman, one out of sight, or one not in the cone', async () => {
  const wall = {}
  for (let y = 64; y < 67; y++) wall[`0,${y},-5`] = { name: 'stone', boundingBox: 'block' }
  for (const [entities, blocks] of [[{}, {}], [{ 1: enderman(0, 64, -10) }, wall], [{ 1: enderman(10, 64, 0) }, {}], [{ 1: { name: 'zombie', position: new Vec3(0, 64, -10) } }, {}]]) {
    const { bot, looks } = fakeBot(entities, blocks)
    avoidEnderStare(bot)
    await bot.look(0, Math.atan2(0.93, 10), true)
    assert.equal(looks[0].pitch, Math.atan2(0.93, 10))
  }
})
