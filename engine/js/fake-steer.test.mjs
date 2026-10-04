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

// ---- what the fake moveTo did around a walk, now done by steer: leads, food, targets the old walker gave up on, rails

const cowAt = (id, x, extra = {}) => ({ id, name: 'cow', kind: 'passive', pos: at(x, 64, 0), ...extra })
const cowOf = (p, id) => p.world.state.entities.find(e => e.id === id)
const walked = async p => p.steer('t', decideOf(walkTo(5.5)))

test('a led animal is dragged behind the body when the walk is over', async () => {
  const p = createFake({ self: { pos: at(0, 64, 0) }, blocks: floor(), entities: [cowAt(1, -1, { leashedToMe: true })] })
  p.setOwner('t')
  await walked(p)
  assert.equal(Math.round(cowOf(p, 1).pos.x), 3)
})

test('a lead that snaps breaks on the walk and drops as an item', async () => {
  const p = createFake({ self: { pos: at(0, 64, 0) }, blocks: floor(), entities: [cowAt(1, -1, { leashedToMe: true, snaps: true })] })
  p.setOwner('t')
  await walked(p)
  assert.equal(cowOf(p, 1).leashedToMe, false)
  assert.ok(p.world.state.entities.some(e => e.name === 'item'))
})

test('an animal drawn by the food in hand follows when the walk is over', async () => {
  const p = createFake({ self: { pos: at(0, 64, 0) }, blocks: floor(), entities: [cowAt(1, 9)], inventory: [{ name: 'wheat', count: 1 }] })
  p.setOwner('t')
  await p.equip('t', { item: 'wheat' })
  await walked(p)
  assert.ok(cowOf(p, 1).pos.x < 9)
})

test('a cut walk drags nothing', async () => {
  const p = createFake({ self: { pos: at(0, 64, 0) }, blocks: floor(), entities: [cowAt(1, -1, { leashedToMe: true })] })
  p.setOwner('t')
  const result = p.steer('t', decideOf(() => ({ controls: { forward: true }, yaw: EAST })))
  await later()
  p.setOwner('other')
  await assert.rejects(result, CutError)
  assert.equal(cowOf(p, 1).pos.x, -1)
})

test('cells near an unreachable or no-path target are stone to the planner, the body\'s own cells are not', () => {
  const p = createFake({ self: { pos: at(0, 64, 0) }, blocks: floor(63, -8, 12), unreachable: ['8,64,0'], noPath: ['0,64,6'] })
  const { snapshot } = p.pathWorld()
  const stone = stateId('stone')
  assert.deepEqual([[8, 64, 0], [5, 66, 3], [11, 62, 0], [0, 64, 6]].map(c => snapshot.stateAt(...c)), [stone, stone, stone, stone])
  assert.deepEqual([[0, 64, 0], [0, 65, 0], [12, 64, 0]].map(c => snapshot.stateAt(...c)), [AIR, AIR, AIR])
})

test('a body walks along rails', async () => {
  const rails = Object.fromEntries([1, 2, 3, 4, 5].map(x => [`${x},64,0`, 'powered_rail']))
  const p = rig(rails)
  const r = await p.steer('t', decideOf(walkTo(5.5)))
  assert.equal(r.status, 'done')
  assert.equal(p.world.state.self.pos.x, 5)
})

// ---- doors, gates and trapdoors: open ones let the body through, shut ones are walls

const gateAt = (open, extra = {}) => ({ blocks: { '3,64,0': 'oak_fence_gate' }, states: { '3,64,0': { open, facing: 'east' } }, ...extra })
const doorRig = ({ blocks, states }) => {
  const p = createFake({ self: { pos: at(0, 64, 0) }, blocks: { ...floor(), ...blocks }, states })
  p.setOwner('t')
  return p
}

test('a body walks through an open gate and stops at a shut one', async () => {
  const through = await doorRig(gateAt(true)).steer('t', decideOf(walkTo(5.5)))
  const shut = await doorRig(gateAt(false)).steer('t', decideOf(walkTo(5.5)), { timeoutS: 1 })
  assert.deepEqual([through.status, shut.status], ['done', 'timeout'])
  assert.ok(shut.pose.x < 3)
})

test('a body walks through the open halves of a door', async () => {
  const blocks = { '3,64,0': 'oak_door', '3,65,0': 'oak_door' }
  const states = open => ({ '3,64,0': { half: 'lower', open }, '3,65,0': { half: 'upper', open } })
  const through = await doorRig({ blocks, states: states(true) }).steer('t', decideOf(walkTo(5.5)))
  const shut = await doorRig({ blocks, states: states(false) }).steer('t', decideOf(walkTo(5.5)), { timeoutS: 1 })
  assert.deepEqual([through.status, shut.status], ['done', 'timeout'])
})

test('a body climbs a ladder up through an open trapdoor and not through a shut one', async () => {
  const hatch = open => ({
    blocks: { '3,64,0': 'ladder', '3,65,0': 'ladder', '3,66,0': 'oak_trapdoor', '2,66,0': 'stone', '4,66,0': 'stone', '3,63,1': 'stone' },
    states: { '3,66,0': { open } }
  })
  const climb = pose => (pose.y >= 67 ? { done: { status: 'arrived' } } : { controls: { forward: false, jump: true }, yaw: 0 })
  const start = { x: 3, y: 64, z: 0 }
  const run = async open => {
    const p = createFake({ self: { pos: start }, blocks: { ...floor(), ...hatch(open).blocks }, states: hatch(open).states })
    p.setOwner('t')
    return p.steer('t', decideOf(climb), { timeoutS: 2 })
  }
  assert.deepEqual([(await run(true)).status, (await run(false)).status], ['done', 'timeout'])
})

test('pathWorld reads the open state of a gate', () => {
  const [open, shut] = [true, false].map(o => doorRig(gateAt(o)).pathWorld().snapshot.stateAt(3, 64, 0))
  assert.equal(open, stateId('oak_fence_gate', { open: true, facing: 'east' }))
  assert.equal(shut, stateId('oak_fence_gate', { open: false, facing: 'east' }))
})
