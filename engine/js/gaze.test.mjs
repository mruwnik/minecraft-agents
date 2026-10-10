// Why JavaScript: node tests of gaze.mjs, the JS adapter's look guard.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import { stares, avoidEnderStare, safePitch } from './gaze.mjs'

const { Vec3 } = vec3
const enderman = (x, y, z) => ({ name: 'enderman', position: new Vec3(x, y, z) })

// a body at the origin (eyes 1.62 up) looking along -z (yaw 0), an enderman 10 blocks ahead
const fakeBot = (entities, blocks = {}) => {
  const looks = []
  const bot = Object.assign(new EventEmitter(), {
    entity: { position: new Vec3(0, 64, 0), height: 1.62 },
    entities,
    blockAt: v => blocks[`${v.x},${v.y},${v.z}`] ?? null,
    look: async (yaw, pitch, force) => { looks.push({ yaw, pitch, force }) }
  })
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

test('the eye is at the entity\'s eyeHeight, not its box height', () => {
  const { bot } = fakeBot({ 1: enderman(0, 64, -10) })
  bot.entity.height = 1.8
  bot.entity.eyeHeight = 1.62
  const eye = new Vec3(0, 65.62, 0)
  const pitch = Math.atan2(0.93, 10)
  assert.equal(stares(eye, 0, pitch, new Vec3(0, 66.55, -10)), true)
  assert.notEqual(safePitch(bot, 0, pitch), pitch)
})

test('an enderman that comes into view while the head is still is looked away from on the next tick', async () => {
  const { bot, looks } = fakeBot({})
  bot.entity.yaw = 0
  bot.entity.pitch = Math.atan2(0.93, 10)
  avoidEnderStare(bot)
  bot.emit('physicsTick')
  assert.equal(looks.length, 0, 'no enderman, no correction')
  bot.entities[1] = enderman(0, 64, -10)
  bot.emit('physicsTick')
  assert.equal(looks.length, 1)
  assert.equal(stares(new Vec3(0, 65.62, 0), 0, looks[0].pitch, new Vec3(0, 66.55, -10)), false)
})
