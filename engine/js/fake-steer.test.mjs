import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake, CutError } from './fake.mjs'
import { stateId } from './path/fixture.mjs'
import { UNLOADED } from './path/snapshot.mjs'

const at = (x, y, z) => ({ x, y, z })
const AIR = 0
const EAST = -Math.PI / 2 // mineflayer yaw: dx = -sin(yaw)
const decideOf = fn => Object.defineProperty({}, 'decide', { value: fn, enumerable: false })
const floor = (y = 63, x0 = -2, x1 = 12) => Object.fromEntries(Array.from({ length: x1 - x0 + 1 }, (_, i) => [`${x0 + i},${y},0`, 'stone']))
const rig = (blocks = {}, pos = at(0, 64, 0)) => {
  const p = createFake({ self: { pos }, blocks: { ...floor(), ...blocks } })
  p.setOwner('t')
  return p
}
const later = () => new Promise(resolve => setImmediate(resolve))
// aims east at the target x until within 0.3 of it
const walkTo = (targetX, { jump = false, onPose = () => {} } = {}) => pose => {
  onPose(pose)
  if (Math.abs(pose.x - targetX) < 0.3) return { done: { status: 'arrived' } }
  return { controls: { forward: true, jump }, yaw: EAST }
}

test('walking five blocks along +x on a floor arrives in a plausible number of ticks', async () => {
  const p = rig()
  const r = await p.steer('t', decideOf(walkTo(5.5)))
  assert.equal(r.status, 'done')
  assert.deepEqual(r.result, { status: 'arrived' })
  assert.ok(r.ticks >= 20 && r.ticks <= 40, `ticks ${r.ticks}`)
  assert.equal(p.world.state.self.pos.x, 5)
})

test('a sprinting walk takes fewer ticks than a plain one', async () => {
  const sprint = pose => (Math.abs(pose.x - 5.5) < 0.3 ? { done: {} } : { controls: { forward: true, sprint: true }, yaw: EAST })
  const fast = await rig().steer('t', decideOf(sprint))
  const slow = await rig().steer('t', decideOf(walkTo(5.5)))
  assert.ok(fast.ticks < slow.ticks)
})

test('the first pose is where the body stands, on the ground, not climbing', async () => {
  const poses = []
  await rig().steer('t', decideOf(pose => { poses.push(pose); return { done: {} } }))
  assert.deepEqual({ ...poses[0], t: 0 }, { x: 0.5, y: 64, z: 0.5, vy: 0, onGround: true, onClimbable: false, inWater: false, collided: false, yaw: 0, t: 0 })
})

test('a one block step up needs jump: without it the body is collided and does not rise', async () => {
  const p = rig({ '3,64,0': 'stone' })
  const poses = []
  const r = await p.steer('t', decideOf(walkTo(5.5, { onPose: pose => poses.push(pose) })), { timeoutS: 1 })
  assert.equal(r.status, 'timeout')
  assert.equal(poses.at(-1).collided, true)
  assert.equal(poses.at(-1).y, 64)
  assert.ok(poses.at(-1).x < 3)
})

test('a one block step up with jump rises onto the block and stays up', async () => {
  const p = rig(floor(64, 3, 12))
  const r = await p.steer('t', decideOf(walkTo(5.5, { jump: true })))
  assert.equal(r.status, 'done')
  assert.equal(p.world.state.self.pos.y, 65)
})

test('a two block wall is never climbed', async () => {
  const p = rig({ '3,64,0': 'stone', '3,65,0': 'stone' })
  const r = await p.steer('t', decideOf(walkTo(5.5, { jump: true })), { timeoutS: 1 })
  assert.equal(r.status, 'timeout')
  assert.equal(p.world.state.self.pos.y, 64)
})

test('a drop lands at the lower floor at once', async () => {
  const blocks = Object.fromEntries([3, 4, 5, 6].flatMap(x => [[`${x},63,0`, 'air'], [`${x},62,0`, 'stone']]))
  const p = rig(blocks)
  const r = await p.steer('t', decideOf(walkTo(5.5)))
  assert.equal(r.status, 'done')
  assert.equal(p.world.state.self.pos.y, 63)
})

test('a ladder climbs with jump and does not climb without it', async () => {
  const ladder = Object.fromEntries([64, 65, 66].map(y => [`1,${y},0`, 'ladder']))
  const wall = Object.fromEntries([64, 65, 66, 67].map(y => [`2,${y},0`, 'stone']))
  const p = rig({ ...ladder, ...wall }, at(1, 64, 0))
  const idle = await p.steer('t', decideOf(pose => ({ done: { y: pose.y, climbing: pose.onClimbable } })))
  assert.deepEqual(idle.result, { y: 64, climbing: true })
  const r = await p.steer('t', decideOf(pose => (pose.y >= 66.9 ? { done: { y: pose.y } } : { controls: { jump: true } })))
  assert.equal(r.status, 'done')
  assert.ok(r.result.y >= 66.9)
})

test('on a ladder without jump the body climbs down to the floor', async () => {
  const ladder = Object.fromEntries([64, 65, 66].map(y => [`1,${y},0`, 'ladder']))
  const p = rig(ladder, at(1, 66, 0))
  const r = await p.steer('t', decideOf(pose => (pose.y <= 64 ? { done: { y: pose.y } } : { controls: {} })))
  assert.deepEqual(r.result, { y: 64 })
})

test('a solid cell above the head stops a climb', async () => {
  const ladder = Object.fromEntries([64, 65, 66].map(y => [`1,${y},0`, 'ladder']))
  const p = rig({ ...ladder, '1,68,0': 'stone' }, at(1, 64, 0))
  const r = await p.steer('t', decideOf(pose => ({ controls: { jump: true } })), { timeoutS: 1 })
  assert.equal(r.status, 'timeout')
  assert.ok(r.pose.y < 67)
})

test('a timeout resolves with the pose and clears the controls', async () => {
  const p = rig()
  const r = await p.steer('t', decideOf(() => ({ controls: { forward: true }, yaw: 0 })), { timeoutS: 0.05 })
  assert.equal(r.status, 'timeout')
  assert.equal(typeof r.pose.x, 'number')
  assert.deepEqual(p.world.state.controls, {})
})

test('a decide that throws resolves failed', async () => {
  const r = await rig().steer('t', decideOf(() => { throw new Error('boom') }))
  assert.deepEqual(r, { status: 'failed', reason: 'Error: boom' })
})

test('a cut mid-walk rejects with the cut error and the controls are cleared', async () => {
  const p = rig()
  const result = p.steer('t', decideOf(() => ({ controls: { forward: true }, yaw: EAST })))
  await later()
  await later()
  assert.equal(p.world.state.controls.forward, true)
  p.setOwner('other')
  await assert.rejects(result, CutError)
  assert.deepEqual(p.world.state.controls, {})
})

test('a stale token is cut at once', async () => {
  await assert.rejects(rig().steer('old', decideOf(() => ({ done: {} }))), CutError)
})

test('bad args are bad-args', async () => {
  await assert.rejects(rig().steer('t', {}), { code: 'bad-args' })
})

test('pathWorld reads a placed block as its state id and air elsewhere in the column', () => {
  const { snapshot, table, space } = rig({ '3,64,0': 'oak_planks' }).pathWorld()
  assert.equal(snapshot.stateAt(3, 64, 0), stateId('oak_planks'))
  assert.equal(snapshot.stateAt(4, 64, 0), AIR)
  assert.ok(table)
  assert.equal(typeof space, 'object')
})

test('pathWorld leaves untouched columns unloaded', () => {
  const { snapshot } = rig().pathWorld()
  assert.equal(snapshot.stateAt(1000, 64, 1000), UNLOADED)
  assert.equal(snapshot.hasColumn(62, 62), false)
})
