// Why JavaScript: tests steer.mjs, which stays JS: Mineflayer boundary; applies controls and reports pose on the bot.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import vec3 from 'vec3'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { createPrimitivesFromBot } from './primitives.mjs'
import { stubBot } from './stub-bot.mjs'

const { Vec3 } = vec3
const registry = prismarineRegistry('26.1')
const ChunkColumn = prismarineChunk(registry)
const SCALE = 0.01
const later = () => new Promise(resolve => setImmediate(resolve))
const decideOf = fn => Object.defineProperty({}, 'decide', { value: fn, enumerable: false })

const rig = (spec) => {
  const bot = stubBot(spec)
  bot.entity.velocity = new Vec3(0, -0.1, 0)
  bot.entity.yaw = 0.5
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  bot.base = bot.listenerCount('physicsTick') // primitives' own watchdog listener
  p.setOwner('t1')
  return { bot, p }
}
// starts a steer and waits until its tick listener is attached
const start = async (p, bot, decide, extra = {}) => {
  const result = p.steer('t1', Object.assign(decideOf(decide), extra))
  while (bot.listenerCount('physicsTick') === bot.base) await later()
  return { result }
}
const walking = { controls: { forward: true, sprint: true }, yaw: 1.5, pitch: 0.25 }
const pressed = bot => Object.values(bot.controlState).filter(Boolean)

test('decide gets a pose of the body each tick and its controls and look are applied', async () => {
  const { bot, p } = rig({ pos: [1.5, 64, 2.5] })
  const poses = []
  const { result } = await start(p, bot, pose => { poses.push(pose); return poses.length > 1 ? { done: {} } : walking })
  bot.emit('physicsTick')
  assert.equal(poses.length, 1)
  assert.deepEqual({ ...poses[0], t: 0 }, { x: 1.5, y: 64, z: 2.5, vx: 0, vy: -0.1, vz: 0, onGround: true, onClimbable: false, onScaffolding: false, inWater: undefined, inLava: undefined, collided: undefined, yaw: 0.5, t: 0 })
  assert.deepEqual(bot.controlState, { forward: true, sprint: true })
  assert.deepEqual([bot.entity.yaw, bot.entity.pitch], [1.5, 0.25])
  bot.emit('physicsTick')
  await result
})

test('the pose says climbable when the feet are in a ladder', async () => {
  const { bot, p } = rig({ pos: [0.5, 64, 0.5], blocks: { '0,64,0': 'ladder' } })
  const poses = []
  await start(p, bot, pose => { poses.push(pose); return { done: {} } })
  bot.emit('physicsTick')
  assert.equal(poses[0].onClimbable, true)
})

// sneak is descend in scaffolding (prismarine-physics), so a sneaking walk lets go of sneak in it and on it
const scaffoldingCases = [
  ['feet in scaffolding', '0,64,0', true],
  ['standing on scaffolding', '0,63,0', true],
  ['scaffolding two below the feet', '0,62,0', false]
]
for (const [name, at, expected] of scaffoldingCases) {
  test(`the pose's onScaffolding: ${name}`, async () => {
    const { bot, p } = rig({ pos: [0.5, 64, 0.5], blocks: { [at]: 'scaffolding' } })
    const poses = []
    await start(p, bot, pose => { poses.push(pose); return { done: {} } })
    bot.emit('physicsTick')
    assert.equal(poses[0].onScaffolding, expected)
  })
}

// vanilla climbs an open trapdoor over a ladder of its own facing, and so does the client once tools/patch-deps.mjs has run
const hatchCases = [
  ['an open trapdoor over a ladder of its facing', 'ladder', { open: true, facing: 'south' }, true],
  ['an open trapdoor over a ladder of another facing', 'ladder', { open: true, facing: 'north' }, false],
  ['a shut trapdoor over a ladder of its facing', 'ladder', { open: false, facing: 'south' }, false],
  ['an open trapdoor over stone', 'stone', { open: true, facing: 'south' }, false]
]
for (const [what, below, trapdoor, climbable] of hatchCases) {
  test(`the pose with the feet in ${what}: climbable ${climbable}`, async () => {
    const { bot, p } = rig({ pos: [0.5, 65, 0.5], blocks: { '0,64,0': below, '0,65,0': 'oak_trapdoor' }, props: { '0,64,0': { facing: 'south' }, '0,65,0': trapdoor } })
    const poses = []
    await start(p, bot, pose => { poses.push(pose); return { done: {} } })
    bot.emit('physicsTick')
    assert.equal(poses[0].onClimbable, climbable)
  })
}

test('done resolves with the result and the tick count and releases the controls', async () => {
  const { bot, p } = rig()
  let n = 0
  const { result } = await start(p, bot, () => (++n < 3 ? walking : { done: { status: 'arrived', at: [1, 2, 3] } }))
  bot.emit('physicsTick')
  assert.deepEqual(bot.controlState, { forward: true, sprint: true })
  bot.emit('physicsTick')
  bot.emit('physicsTick')
  assert.deepEqual(await result, { status: 'done', result: { status: 'arrived', at: [1, 2, 3] }, ticks: 3 })
  assert.deepEqual(pressed(bot), [])
  assert.equal(bot.listenerCount('physicsTick'), bot.base)
})

test('a cut mid-walk rejects cut and no control stays held', async () => {
  const { bot, p } = rig()
  const { result } = await start(p, bot, () => walking)
  bot.emit('physicsTick')
  assert.equal(pressed(bot).length, 2)
  p.setOwner('other')
  await assert.rejects(result, { code: 'cut' })
  assert.deepEqual(pressed(bot), [])
  assert.equal(bot.listenerCount('physicsTick'), bot.base)
})

test('a cut mid-jump releases jump, sprint and forward', async () => {
  const { bot, p } = rig()
  const { result } = await start(p, bot, () => ({ controls: { forward: true, sprint: true, jump: true }, yaw: 1.5 }))
  bot.emit('physicsTick')
  assert.equal(pressed(bot).length, 3)
  p.setOwner('other')
  await assert.rejects(result, { code: 'cut' })
  assert.deepEqual(pressed(bot), [])
  assert.equal(bot.listenerCount('physicsTick'), bot.base)
})

test('a timeout resolves timeout with the pose and releases the controls', async () => {
  const { bot, p } = rig({ pos: [4.5, 64, 0.5] })
  const { result } = await start(p, bot, () => walking, { timeoutS: 1 })
  bot.emit('physicsTick')
  const r = await result
  assert.equal(r.status, 'timeout')
  assert.equal(r.pose.x, 4.5)
  assert.deepEqual(pressed(bot), [])
  assert.equal(bot.listenerCount('physicsTick'), bot.base)
})

test('a decide that throws resolves failed and releases the controls', async () => {
  const { bot, p } = rig()
  let n = 0
  const { result } = await start(p, bot, () => { if (++n === 2) throw new Error('boom'); return walking })
  bot.emit('physicsTick')
  bot.emit('physicsTick')
  assert.deepEqual(await result, { status: 'failed', reason: 'Error: boom' })
  assert.deepEqual(pressed(bot), [])
  assert.equal(bot.listenerCount('physicsTick'), bot.base)
})

test('a reason longer than 200 characters is cut', async () => {
  const { bot, p } = rig()
  const { result } = await start(p, bot, () => { throw new Error('x'.repeat(500)) })
  bot.emit('physicsTick')
  assert.equal((await result).reason.length, 200)
})

test('controls without a yaw leave the look alone', async () => {
  const { bot, p } = rig()
  await start(p, bot, () => ({ controls: { jump: true } }))
  bot.emit('physicsTick')
  assert.equal(bot.entity.yaw, 0.5)
  assert.equal(bot.calls.filter(c => c.name === 'look').length, 0)
})

const badArgsCases = [
  ['no decide', {}],
  ['decide not a function', { decide: 3 }],
  ['timeoutS a string', { decide: () => ({}), timeoutS: 'x' }],
  ['timeoutS zero', { decide: () => ({}), timeoutS: 0 }],
  ['timeoutS over 120', { decide: () => ({}), timeoutS: 121 }]
]
badArgsCases.forEach(([name, a]) => test(`bad args: ${name}`, async () => {
  const { p } = rig()
  await assert.rejects(p.steer('t1', a), { code: 'bad-args' })
}))

test('pathWorld gives the planner a snapshot over the bot world', () => {
  const column = new ChunkColumn()
  column.setBlockStateId(new Vec3(3, 70, 4), registry.blocksByName.stone.defaultState)
  const { bot, p } = rig()
  bot.world = { getColumn: (cx, cz) => (cx === 0 && cz === 0 ? column : undefined) }
  bot.game = { minY: -64, height: 384 }
  const { snapshot } = p.pathWorld()
  assert.equal(snapshot.stateAt(3, 70, 4), registry.blocksByName.stone.defaultState)
  assert.equal(snapshot.hasColumn(5, 5), false)
})

test('pathWorld without a bot world is null', () => {
  assert.equal(rig().p.pathWorld(), null)
})
