import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'
import { FAKE_BODY_ID } from './fake-vehicle.mjs'

const at = (x, y, z) => ({ x, y, z })
const boat = (extra = {}) => ({ id: 9, name: 'oak_boat', uuid: 'u-9', kind: 'other', pos: at(1, 64, 0), ...extra })
const cow = (extra = {}) => ({ id: 7, name: 'cow', uuid: 'u-7', kind: 'passive', pos: at(2, 64, 0), ...extra })

const rig = spec => {
  const p = createFake(spec)
  p.setOwner('t')
  return p
}

test('self().vehicle names the vehicle from the spec, null on foot', () => {
  assert.equal(rig({}).self().vehicle, null)
  assert.deepEqual(rig({ self: { vehicle: 9 }, entities: [boat()] }).self().vehicle, { id: 9, uuid: 'u-9', name: 'oak_boat' })
})

test('entities carry passengers and vehicle as the spec gives them', () => {
  const p = rig({ entities: [boat({ passengers: [7] }), cow({ vehicle: 9 })] })
  const found = id => p.entities({}).find(e => e.id === id)
  assert.deepEqual(found(9).passengers, [7])
  assert.equal(found(7).vehicle, 9)
})

test('mount refuses', async t => {
  const rows = [
    ['already-mounted', { self: { vehicle: 9 }, entities: [boat()] }, { status: 'already-mounted', vehicle: { id: 9, uuid: 'u-9', name: 'oak_boat' } }],
    ['gone', {}, { status: 'gone' }],
    ['not-mountable', { entities: [boat({ name: 'cow' })] }, { status: 'not-mountable' }],
    ['occupied', { entities: [boat({ passengers: [7, 8] })] }, { status: 'occupied' }],
    ['out-of-reach', { entities: [boat({ pos: at(5, 64, 0) })] }, { status: 'out-of-reach' }],
    ['timeout', { entities: [boat()], mountFails: true }, { status: 'timeout' }]
  ]
  for (const [label, spec, expected] of rows) {
    await t.test(label, async () => {
      const p = rig(spec)
      const before = p.self().vehicle
      assert.deepEqual(await p.mount('t', { id: 9 }), expected)
      assert.deepEqual(p.self().vehicle, before)
    })
  }
})

test('mount seats the body: on the vehicle, listed as a passenger, hand emptied', async () => {
  const p = rig({ self: { held: 'lead' }, inventory: [{ name: 'lead', count: 1 }], entities: [boat()] })
  assert.deepEqual(await p.mount('t', { id: 9 }), { status: 'mounted', vehicle: { id: 9, uuid: 'u-9', name: 'oak_boat' } })
  assert.deepEqual(p.self().pos, at(1, 64, 0))
  assert.equal(p.self().held, null)
  assert.deepEqual(p.entities({}).find(e => e.id === 9).passengers, [FAKE_BODY_ID])
})

test('dismount', async t => {
  await t.test('not-mounted', async () => assert.deepEqual(await rig({}).dismount('t', {}), { status: 'not-mounted' }))
  await t.test('timeout keeps the body aboard', async () => {
    const p = rig({ self: { vehicle: 9 }, entities: [boat()], dismountFails: true })
    assert.deepEqual(await p.dismount('t', {}), { status: 'timeout', mounted: true })
    assert.equal(p.self().vehicle.id, 9)
  })
  await t.test('lands at dismountAt and records the yaw', async () => {
    const p = rig({ self: { vehicle: 9 }, entities: [boat({ passengers: [FAKE_BODY_ID], dismountAt: at(1, 65, 3) })], blocks: { '1,64,3': 'stone' } })
    assert.deepEqual(await p.dismount('t', { yaw: 0 }), { status: 'dismounted', pos: at(1, 65, 3) })
    assert.equal(p.self().vehicle, null)
    assert.deepEqual(p.self().pos, at(1, 65, 3))
    assert.equal('passengers' in p.entities({}).find(e => e.id === 9), false)
    assert.equal(p.world.state.dismountYaw, 0)
  })
  await t.test('without dismountAt lands one block +x of the vehicle; a water cell is in water', async () => {
    const p = rig({ self: { vehicle: 9 }, entities: [boat()], blocks: { '2,64,0': 'water' } })
    assert.deepEqual(await p.dismount('t', {}), { status: 'dismounted', pos: at(2, 64, 0) })
    assert.equal(p.self().inWater, true)
  })
})

test('moveTo and steer answer mounted while aboard', async t => {
  const rows = [
    ['moveTo', { pos: at(10, 64, 0) }],
    ['steer', Object.defineProperty({ timeoutS: 1 }, 'decide', { value: () => ({ done: true }), enumerable: false })] // the fake clones args
  ]
  for (const [name, args] of rows) {
    await t.test(name, async () => {
      const p = rig({ self: { vehicle: 9 }, entities: [boat()] })
      assert.deepEqual(await p[name]('t', args), { status: 'mounted' })
      assert.deepEqual(p.self().pos, at(0, 64, 0))
    })
  }
})
