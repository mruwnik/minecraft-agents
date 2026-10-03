import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = (x, y, z) => ({ x, y, z })

const tick = () => new Promise(resolve => setTimeout(resolve, 1))

const owned = (spec = {}) => {
  const p = createFake(spec)
  p.setOwner('t1')
  return p
}

test('self reports position, vitals, time and inventory', () => {
  const p = createFake({ self: { pos: at(1, 64, 2), health: 15, food: 9, username: 'F' }, time: 13000, inventory: [{ name: 'bread', count: 2 }] })
  const s = p.self()
  assert.deepEqual(s.pos, at(1, 64, 2))
  assert.equal(s.health, 15)
  assert.equal(s.food, 9)
  assert.equal(s.isDay, false)
  assert.equal(s.username, 'F')
  assert.deepEqual(s.inventory.map(i => [i.name, i.count]), [['bread', 2]])
})


for (const [time, isDay] of [[1000, true], [12000, true], [13000, false], [23000, false], [23500, true]]) {
  test(`isDay at ${time} is ${isDay}`, () => {
    assert.equal(createFake({ time }).self().isDay, isDay)
  })
}

test('entities are filtered by radius and kind and sorted by distance', () => {
  const p = createFake({
    entities: [
      { id: 1, name: 'zombie', kind: 'hostile', pos: at(10, 64, 0) },
      { id: 2, name: 'cow', kind: 'passive', pos: at(2, 64, 0) },
      { id: 3, name: 'skeleton', kind: 'hostile', pos: at(4, 64, 0) },
      { id: 4, name: 'creeper', kind: 'hostile', pos: at(40, 64, 0) }
    ]
  })
  assert.deepEqual(p.entities({ radius: 16, kind: 'hostile' }).map(e => e.id), [3, 1])
  assert.deepEqual(p.entities({ radius: 16 }).map(e => e.id), [2, 3, 1])
  assert.deepEqual(p.entities({ names: ['cow'] }).map(e => e.id), [2])
  assert.equal(p.entities({ radius: 16, max: 1 }).length, 1)
})

test('blocks match by names or predicate, sorted by distance', () => {
  const p = createFake({ blocks: { '5,64,0': 'oak_log', '2,64,0': 'birch_log', '1,63,0': 'dirt', '50,64,0': 'oak_log' } })
  assert.deepEqual(p.blocks({ names: ['oak_log', 'birch_log'] }).map(b => b.name), ['birch_log', 'oak_log'])
  assert.deepEqual(p.blocks({ match: n => n.endsWith('_log'), radius: 100 }).map(b => b.pos.x), [2, 5, 50])
  assert.equal(p.blockAt(at(1, 63, 0)).name, 'dirt')
  assert.equal(p.blockAt(at(9, 9, 9)).name, 'air')
})

test('blockAt returns null for a cell in an unloaded chunk', () => {
  const p = createFake({ blocks: { '1,64,0': 'dirt' }, unloaded: ['1,64,0', '2,64,0'] })
  assert.equal(p.blockAt(at(1, 64, 0)), null)
  assert.equal(p.blockAt(at(2, 64, 0)), null)
  assert.equal(p.blockAt(at(3, 64, 0)).name, 'air')
})

test('acting with a stale token rejects with cut', async () => {
  const p = owned()
  await assert.rejects(p.moveTo('other', { pos: at(1, 64, 0) }), { code: 'cut' })
})

test('changing the owner cuts a held call', async () => {
  const p = owned()
  p.world.hold('moveTo')
  const walk = p.moveTo('t1', { pos: at(3, 64, 0) })
  p.setOwner('t2')
  await assert.rejects(walk, { code: 'cut' })
  assert.deepEqual(p.self().pos, at(0, 64, 0))
})

test('a released hold runs the default implementation, or returns the given result', async () => {
  const p = owned()
  const release = p.world.hold('moveTo')
  const walk = p.moveTo('t1', { pos: at(3, 64, 0) })
  release()
  assert.equal((await walk).status, 'arrived')
  const release2 = p.world.hold('dig')
  const dig = p.dig('t1', { pos: at(1, 64, 0) })
  release2({ status: 'cannot' })
  assert.equal((await dig).status, 'cannot')
})

test('moveTo arrives, goes partial past maxDistance, and is blocked when unreachable', async () => {
  const p = owned({ unreachable: ['9,64,9'] })
  assert.equal((await p.moveTo('t1', { pos: at(3, 64, 4) })).status, 'arrived')
  assert.deepEqual(p.self().pos, at(3, 64, 4))
  const far = await p.moveTo('t1', { pos: at(103, 64, 4), maxDistance: 10 })
  assert.equal(far.status, 'partial')
  assert.deepEqual(far.pos, at(13, 64, 4))
  assert.equal((await p.moveTo('t1', { pos: at(9, 64, 9) })).status, 'blocked')
})

test('dig removes the block and drops an item entity', async () => {
  const p = owned({ blocks: { '1,64,0': 'stone', '20,64,0': 'stone' }, drops: { stone: 'cobblestone' } })
  const r = await p.dig('t1', { pos: at(1, 64, 0) })
  assert.equal(r.status, 'dug')
  assert.equal(r.block, 'stone')
  assert.equal(r.drops[0].name, 'cobblestone')
  assert.equal(p.blockAt(at(1, 64, 0)).name, 'air')
  assert.equal(p.entities({ kind: 'item' })[0].item.name, 'cobblestone')
  assert.equal((await p.dig('t1', { pos: at(1, 64, 0) })).status, 'missing')
  assert.equal((await p.dig('t1', { pos: at(20, 64, 0) })).status, 'unreachable')
})

test('collect moves a dropped item into the inventory', async () => {
  const p = owned({ blocks: { '1,64,0': 'oak_log' } })
  const { drops } = await p.dig('t1', { pos: at(1, 64, 0) })
  const r = await p.collect('t1', { id: drops[0].id })
  assert.equal(r.status, 'collected')
  assert.deepEqual(r.gained, [{ name: 'oak_log', count: 1 }])
  assert.equal(p.self().inventory.find(i => i.name === 'oak_log').count, 1)
  assert.equal((await p.collect('t1', { id: drops[0].id })).status, 'gone')
})

test('place uses an inventory item on an empty cell', async () => {
  const p = owned({ inventory: [{ name: 'dirt', count: 1 }], blocks: { '1,64,0': 'stone' } })
  assert.equal((await p.place('t1', { pos: at(1, 64, 0), item: 'dirt' })).status, 'occupied')
  assert.equal((await p.place('t1', { pos: at(2, 64, 0), item: 'dirt' })).status, 'placed')
  assert.equal(p.blockAt(at(2, 64, 0)).name, 'dirt')
  assert.equal((await p.place('t1', { pos: at(3, 64, 0), item: 'dirt' })).status, 'no-item')
})

test('containers can be inspected and transferred to and from', async () => {
  const p = owned({ inventory: [{ name: 'oak_log', count: 5 }], containers: { '1,64,1': [{ name: 'cobblestone', count: 10 }] } })
  const dep = await p.transfer('t1', { pos: at(1, 64, 1), direction: 'deposit', item: 'oak_log', count: 3 })
  assert.deepEqual([dep.status, dep.moved], ['ok', 3])
  const wd = await p.transfer('t1', { pos: at(1, 64, 1), direction: 'withdraw', item: 'cobblestone', count: 20 })
  assert.deepEqual([wd.status, wd.moved], ['ok', 10])
  const seen = await p.inspectContainer('t1', { pos: at(1, 64, 1) })
  assert.deepEqual(seen.items.map(i => [i.name, i.count]), [['oak_log', 3]])
  assert.equal((await p.inspectContainer('t1', { pos: at(30, 64, 1) })).status, 'missing')
})

test('equip and eat use the inventory', async () => {
  const p = owned({ self: { food: 10 }, inventory: [{ name: 'bread', count: 1 }, { name: 'iron_axe', count: 1 }] })
  assert.equal((await p.equip('t1', { item: 'iron_axe' })).status, 'equipped')
  assert.equal(p.self().held, 'iron_axe')
  assert.equal((await p.equip('t1', { item: 'diamond' })).status, 'no-item')
  const ate = await p.eat('t1', {})
  assert.deepEqual([ate.status, ate.item, ate.food], ['ate', 'bread', 15])
  assert.equal((await p.eat('t1', {})).status, 'no-food')
})

test('attack hits then kills', async () => {
  const p = owned({ entities: [{ id: 5, name: 'zombie', kind: 'hostile', pos: at(2, 64, 0), health: 6 }] })
  assert.equal((await p.attack('t1', { id: 5 })).status, 'hit')
  assert.equal((await p.attack('t1', { id: 5 })).status, 'killed')
  assert.equal((await p.attack('t1', { id: 5 })).status, 'gone')
})

test('sleep works at night on a bed and makes it morning', async () => {
  const p = owned({ time: 1000, blocks: { '1,64,0': 'red_bed' } })
  assert.equal((await p.sleep('t1', { pos: at(1, 64, 0) })).status, 'not-night')
  p.world.setTime(14000)
  assert.equal((await p.sleep('t1', { pos: at(2, 64, 0) })).status, 'missing')
  assert.equal((await p.sleep('t1', { pos: at(1, 64, 0) })).status, 'sleeping')
  assert.equal(p.self().isDay, true)
})

test('calls are logged and overrides replace an implementation', async () => {
  const p = owned()
  p.world.override('look', async (token, args, impl) => ({ status: 'odd', viaDefault: (await impl(token, args)).status }))
  const r = await p.look('t1', { yaw: 0, pitch: 0 })
  assert.deepEqual(r, { status: 'odd', viaDefault: 'ok' })
  assert.deepEqual(p.world.calls.map(c => [c.name, c.token]), [['look', 't1']])
})

test('body events reach listeners until unsubscribed', () => {
  const p = createFake()
  const seen = []
  const off = p.onBodyEvent(e => seen.push(e.kind))
  p.world.emit({ kind: 'hurt', health: 5 })
  off()
  p.world.emit({ kind: 'chat' })
  assert.deepEqual(seen, ['hurt'])
})

test('die emits died with the position, inventory and experience, then drops the inventory there', () => {
  const p = createFake({
    self: { pos: { x: 3, y: 64, z: 1 }, experience: { level: 4, points: 60, progress: 0.5 } },
    inventory: [{ name: 'bread', count: 2 }]
  })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  p.world.die()
  assert.deepEqual(seen, [{
    kind: 'died',
    pos: { x: 3, y: 64, z: 1 },
    inventory: [{ name: 'bread', count: 2, slot: 0 }],
    experience: { level: 4, points: 60 }
  }])
  assert.deepEqual(p.self().inventory, [])
  assert.deepEqual(p.self().experience, { level: 0, points: 0, progress: 0 })
  assert.deepEqual(p.entities({ kind: 'item' }).map(e => [e.item.name, e.item.count, e.pos]), [['bread', 2, { x: 3, y: 64, z: 1 }]])
})

test('self carries the survival fields with healthy defaults', () => {
  const s = createFake().self()
  assert.deepEqual([s.oxygen, s.onFire, s.inWater, s.inLava, s.isSleeping, s.foodSaturation, s.dimension], [20, false, false, false, false, 5, 'overworld'])
  assert.deepEqual(s.experience, { level: 0, points: 0, progress: 0 })
})

test('self survival fields come from the spec', () => {
  const s = createFake({ self: { oxygen: 3, onFire: true, inLava: true, dimension: 'the_nether', experience: { level: 2, points: 20, progress: 0.5 } } }).self()
  assert.deepEqual([s.oxygen, s.onFire, s.inLava, s.dimension, s.experience.level], [3, true, true, 'the_nether', 2])
})

test('player entities default to awake with a username; mobs get neither', () => {
  const p = createFake({ entities: [{ id: 1, name: 'Dan', kind: 'player', pos: at(1, 64, 0) }, { id: 2, name: 'creeper', kind: 'hostile', creeper: true, pos: at(2, 64, 0) }, { id: 3, name: 'Sue', kind: 'player', sleeping: true, pos: at(3, 64, 0) }] })
  const [dan, creeper, sue] = p.entities({})
  assert.deepEqual([dan.username, dan.sleeping, sue.sleeping, creeper.creeper, 'sleeping' in creeper], ['Dan', false, true, true, false])
})

test('a creeper entity carries creeper: true without being told; other hostiles do not', () => {
  const p = createFake({ entities: [{ id: 1, name: 'creeper', kind: 'hostile', pos: at(1, 64, 0) }, { id: 2, name: 'zombie', kind: 'hostile', pos: at(2, 64, 0) }] })
  const [creeper, zombie] = p.entities({})
  assert.deepEqual([creeper.creeper, 'creeper' in zombie], [true, false])
})

test('offline flips the flag, emits offline and online, and resolves ok after the wait', async () => {
  const p = owned({ offlineScale: 0.001 })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  const pending = p.offline('t1', { ms: 20000 })
  await tick()
  assert.equal(p.world.state.offline, true)
  assert.deepEqual(await pending, { status: 'ok', ms: 20000 })
  assert.equal(p.world.state.offline, false)
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'online'])
})

for (const [ms, expected] of [[undefined, 300000], [99999999, 600000]]) {
  test(`fake offline with ms ${ms} reports ${expected}`, async () => {
    const p = owned({ offlineScale: 0 })
    assert.equal((await p.offline('t1', ms === undefined ? {} : { ms })).ms, expected)
  })
}

test('fake offline with a stale token rejects with cut', async () => {
  await assert.rejects(createFake().offline('nope', { ms: 1 }), e => e.code === 'cut')
})

test('a cut during the fake wait still comes back online and resolves cut', async () => {
  const p = owned({ offlineScale: 0.001 })
  const pending = p.offline('t1', { ms: 20000 })
  await tick()
  p.setOwner('t2')
  assert.deepEqual(await pending, { status: 'cut' })
  assert.equal(p.world.state.offline, false)
})

test('blocks and blockAt report a crop age that dig clears', async () => {
  const p = owned({ blocks: { '1,64,0': 'wheat', '2,64,0': 'dirt' }, ages: { '1,64,0': 7 } })
  assert.deepEqual(p.blocks({ names: ['wheat'] }).map(b => b.age), [7])
  assert.equal(p.blockAt(at(1, 64, 0)).age, 7)
  assert.equal('age' in p.blockAt(at(2, 64, 0)), false)
  await p.dig('t1', { pos: at(1, 64, 0) })
  assert.equal('age' in p.blockAt(at(1, 64, 0)), false)
})

test('a killed animal drops its listed items', async () => {
  const p = owned({ entities: [{ id: 5, name: 'cow', kind: 'passive', pos: at(1, 64, 0), health: 5, drops: [{ name: 'beef', count: 2 }] }] })
  assert.equal((await p.attack('t1', { id: 5 })).status, 'killed')
  assert.deepEqual(p.entities({ kind: 'item' }).map(e => [e.item.name, e.item.count]), [['beef', 2]])
})

test('moveTo can be told to fail with noPath: blocked, reason noPath, the body stays', async () => {
  const p = owned({ noPath: ['9,64,9'] })
  const r = await p.moveTo('t1', { pos: at(9, 64, 9) })
  assert.deepEqual([r.status, r.reason, r.pos], ['blocked', 'noPath', at(0, 64, 0)])
})

const sea = { self: { pos: at(0, 60, 0), oxygen: 3, inWater: true }, blocks: { '0,60,0': 'water', '0,61,0': 'water', '0,62,0': 'water', '0,63,0': 'water' } }

test('swim surfaces: the body rises to the top water cell and oxygen refills', async () => {
  const p = owned(sea)
  const r = await p.swim('t1', { ms: 3000 })
  assert.deepEqual(r, { status: 'surfaced', oxygen: { before: 3, after: 20 } })
  assert.deepEqual(p.self().pos, at(0, 63, 0))
  assert.equal(p.self().oxygen, 20)
})

test('swim on dry land reports surfaced and changes nothing', async () => {
  const p = owned({ self: { oxygen: 20 } })
  assert.deepEqual(await p.swim('t1'), { status: 'surfaced', oxygen: { before: 20, after: 20 } })
  assert.deepEqual(p.self().pos, at(0, 64, 0))
})

test('swim can be told to fail: timeout, nothing moves', async () => {
  const p = owned({ ...sea, swimFails: true })
  const r = await p.swim('t1')
  assert.deepEqual(r, { status: 'timeout', oxygen: { before: 3, after: 3 } })
  assert.deepEqual(p.self().pos, at(0, 60, 0))
})

test('swim with a stale token rejects with cut', async () => {
  await assert.rejects(owned(sea).swim('other'), { code: 'cut' })
})
