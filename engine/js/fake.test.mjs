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

test('attack reports hurt, and an invulnerable entity takes no damage', async () => {
  const p = owned({ entities: [
    { id: 5, name: 'zombie', kind: 'hostile', pos: at(2, 64, 0), health: 6 },
    { id: 6, name: 'zombie', kind: 'hostile', pos: at(2, 64, 1), invulnerable: true }
  ] })
  assert.deepEqual(await p.attack('t1', { id: 6 }), { status: 'hit', health: 20, hurt: false })
  assert.deepEqual(await p.attack('t1', { id: 5 }), { status: 'hit', health: 1, hurt: true })
  assert.deepEqual(await p.attack('t1', { id: 5 }), { status: 'killed', health: 0, hurt: true })
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

test('self effects default to none and come from the spec', () => {
  assert.deepEqual(createFake().self().effects, [])
  const effects = [{ name: 'fire_resistance', amplifier: 0, duration: 600 }]
  assert.deepEqual(createFake({ self: { effects } }).self().effects, effects)
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

test('place with a water bucket pours water and swaps in an empty bucket; bucket scoops it back', async () => {
  const p = owned({ blocks: { '1,63,0': 'dirt' }, inventory: [{ name: 'water_bucket', count: 1 }] })
  assert.equal((await p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' })).status, 'placed')
  assert.equal(p.blockAt(at(1, 64, 0)).name, 'water')
  assert.deepEqual(p.self().inventory.map(i => i.name), ['bucket'])
  assert.equal((await p.place('t1', { pos: at(1, 64, 0), item: 'bucket' })).status, 'placed')
  assert.equal(p.blockAt(at(1, 64, 0)).name, 'air')
  assert.deepEqual(p.self().inventory.map(i => i.name), ['water_bucket'])
  assert.equal((await p.place('t1', { pos: at(1, 64, 0), item: 'bucket' })).status, 'missing')
})

const fakeWall = Object.fromEntries([2, 3].flatMap(x => [64, 65, 66].map(y => [`${x},${y},0`, 'stone'])))
const fakeZombie = { id: 9, name: 'zombie', kind: 'hostile', pos: at(5, 64, 0) }

test('entities marks a hostile behind a wall not visible, and visible once the wall is gone', () => {
  const walled = createFake({ blocks: fakeWall, entities: [fakeZombie] })
  assert.equal(walled.entities({ kind: 'hostile' })[0].visible, false)
  Object.keys(fakeWall).forEach(k => walled.world.state.blocks.delete(k))
  assert.equal(walled.entities({ kind: 'hostile' })[0].visible, true)
})

test('only hostiles carry visible, and a spec can force it', () => {
  const p = createFake({ blocks: fakeWall, entities: [{ id: 1, name: 'cow', kind: 'passive', pos: at(5, 64, 0) }, { ...fakeZombie, visible: true }] })
  assert.equal('visible' in p.entities({ kind: 'passive' })[0], false)
  assert.equal(p.entities({ kind: 'hostile' })[0].visible, true)
})

test('wait returns ok at once and a held wait is cut by an owner change', async () => {
  const p = owned()
  assert.deepEqual(await p.wait('t1', { ms: 2000 }), { status: 'ok' })
  p.world.hold('wait')
  const call = p.wait('t1', { ms: 2000 })
  p.setOwner('t2')
  await assert.rejects(call, err => err.code === 'cut')
})

test('while offline the fake reports it, and sensing says offline instead of stale values', async () => {
  const p = owned({ offlineScale: 0.001, entities: [{ id: 1, name: 'zombie', kind: 'hostile', pos: at(2, 64, 0) }], blocks: { '1,64,0': 'stone' } })
  assert.equal(p.isOffline(), false)
  const pending = p.offline('t1', { ms: 20000 })
  await tick()
  assert.equal(p.isOffline(), true)
  assert.deepEqual(p.self(), { status: 'offline' })
  assert.deepEqual(p.entities({}), [])
  assert.deepEqual(p.blocks({}), [])
  assert.equal(p.blockAt(at(1, 64, 0)), null)
  await pending
  assert.equal(p.isOffline(), false)
  assert.equal(p.self().username, 'Fake')
  assert.equal(p.entities({}).length, 1)
})

test('a cut ends the fake wait early and the body is back before the offline call resolves', async () => {
  const p = owned({ offlineScale: 1 })
  const seen = []
  p.onBodyEvent(e => seen.push(e.kind))
  const t0 = Date.now()
  const pending = p.offline('t1', { ms: 600000 })
  await tick()
  p.setOwner('t2')
  assert.equal(p.isOffline(), true)
  assert.deepEqual(await pending, { status: 'cut' })
  assert.ok(Date.now() - t0 < 1000)
  assert.equal(p.isOffline(), false)
  assert.deepEqual(seen, ['offline', 'online'])
})

for (const name of ['fire', 'soul_fire', 'short_grass', 'tall_grass', 'grass', 'snow']) {
  test(`fake place treats a ${name} cell as free and replaces it, for a block and for a water bucket`, async () => {
    const p = owned({ inventory: [{ name: 'dirt', count: 1 }, { name: 'water_bucket', count: 1 }], blocks: { '1,64,0': name, '2,64,0': name } })
    assert.equal((await p.place('t1', { pos: at(1, 64, 0), item: 'dirt' })).status, 'placed')
    assert.equal(p.blockAt(at(1, 64, 0)).name, 'dirt')
    assert.equal((await p.place('t1', { pos: at(2, 64, 0), item: 'water_bucket' })).status, 'placed')
    assert.equal(p.blockAt(at(2, 64, 0)).name, 'water')
  })
}

test('swim toward moves the body onto a standable target within 6 blocks and lands', async () => {
  const p = owned({ ...sea, blocks: { ...sea.blocks, '3,64,0': 'stone' } })
  const r = await p.swim('t1', { ms: 3000, toward: at(3, 65, 0) })
  assert.equal(r.status, 'landed')
  assert.deepEqual(p.self().pos, at(3, 65, 0))
})

test('swim toward a target that is far, in water or not given leaves the body to surface only', async () => {
  const far = owned({ ...sea, blocks: { ...sea.blocks, '9,64,0': 'stone' } })
  assert.equal((await far.swim('t1', { ms: 3000, toward: at(9, 65, 0) })).status, 'timeout')
  assert.deepEqual(far.self().pos, at(0, 63, 0))
  const wet = owned({ ...sea, blocks: { ...sea.blocks, '3,64,0': 'water' } })
  assert.equal((await wet.swim('t1', { ms: 3000, toward: at(3, 64, 0) })).status, 'timeout')
})

test('fake jumpPlace raises the body one block per placement and consumes the items', async () => {
  const p = owned({ inventory: [{ name: 'dirt', count: 3 }], blocks: { '0,63,0': 'stone' } })
  assert.deepEqual(await p.jumpPlace('t1', { item: 'dirt', count: 2 }), { status: 'done', placed: 2 })
  assert.deepEqual(p.self().pos, at(0, 66, 0))
  assert.equal(p.blockAt(at(0, 64, 0)).name, 'dirt')
  assert.equal(p.blockAt(at(0, 65, 0)).name, 'dirt')
  assert.equal(p.self().inventory.find(i => i.name === 'dirt').count, 1)
})

test('fake jumpPlace stops early with a reason: no item, no headroom, nothing solid below', async () => {
  const none = owned({ blocks: { '0,63,0': 'stone' } })
  assert.deepEqual(await none.jumpPlace('t1', { item: 'dirt' }), { status: 'failed', placed: 0, reason: 'no-item' })
  const low = owned({ inventory: [{ name: 'dirt', count: 3 }], blocks: { '0,63,0': 'stone', '0,66,0': 'stone' } })
  assert.deepEqual(await low.jumpPlace('t1', { item: 'dirt', count: 3 }), { status: 'failed', placed: 0, reason: 'no-headroom' })
  const air = owned({ inventory: [{ name: 'dirt', count: 3 }] })
  assert.deepEqual(await air.jumpPlace('t1', { item: 'dirt' }), { status: 'failed', placed: 0, reason: 'no-support' })
  const short = owned({ inventory: [{ name: 'dirt', count: 1 }], blocks: { '0,63,0': 'stone' } })
  assert.deepEqual(await short.jumpPlace('t1', { item: 'dirt', count: 3 }), { status: 'partial', placed: 1, reason: 'no-item' })
})

test('fake jumpPlace stops under a ceiling that appears as the body rises', async () => {
  const p = owned({ inventory: [{ name: 'dirt', count: 5 }], blocks: { '0,63,0': 'stone', '0,67,0': 'stone' } })
  assert.deepEqual(await p.jumpPlace('t1', { item: 'dirt', count: 5 }), { status: 'partial', placed: 1, reason: 'no-headroom' })
  assert.deepEqual(p.self().pos, at(0, 65, 0))
})

test('fake toss takes the items from every stack and drops one item entity three blocks along x', async () => {
  const p = owned({ self: { pos: at(1, 64, 2) }, inventory: [{ name: 'dirt', count: 40 }, { name: 'bread', count: 2 }, { name: 'dirt', count: 30 }] })
  assert.deepEqual(await p.toss('t1', { item: 'dirt' }), { status: 'tossed', count: 70 })
  assert.deepEqual(p.self().inventory.map(i => i.name), ['bread'])
  const [drop] = p.entities({ kind: 'item' })
  assert.deepEqual([drop.name, drop.item, drop.pos], ['item', { name: 'dirt', count: 70 }, at(4, 64, 2)])
  assert.deepEqual(p.world.calls.map(c => [c.name, c.args]), [['toss', { item: 'dirt' }]])
})

test('fake toss honours a smaller count, clamps a larger one and reports no-item for nothing carried', async () => {
  const p = owned({ inventory: [{ name: 'dirt', count: 5 }] })
  assert.deepEqual(await p.toss('t1', { item: 'dirt', count: 2 }), { status: 'tossed', count: 2 })
  assert.deepEqual(await p.toss('t1', { item: 'dirt', count: 9 }), { status: 'tossed', count: 3 })
  assert.deepEqual(await p.toss('t1', { item: 'dirt' }), { status: 'no-item', count: 0 })
  assert.equal(p.entities({ kind: 'item' }).length, 2)
})

test('fake collect emits one picked-up event per gained item', async () => {
  const p = owned({ entities: [{ id: 5, name: 'item', kind: 'item', pos: at(1, 64, 0), item: { name: 'oak_log', count: 3 } }] })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  await p.collect('t1', { id: 5 })
  assert.deepEqual(seen, [{ kind: 'picked-up', item: 'oak_log', count: 3 }])
})

test('fake toss with a slot throws that whole stack only; no-item for an empty slot or another item', async () => {
  const p = owned({ inventory: [{ name: 'dirt', count: 40 }, { name: 'bread', count: 2 }, { name: 'dirt', count: 6 }] })
  assert.deepEqual(await p.toss('t1', { item: 'dirt', slot: 2 }), { status: 'tossed', count: 6 })
  assert.deepEqual(p.self().inventory.map(i => [i.name, i.count]), [['dirt', 40], ['bread', 2]])
  assert.deepEqual(p.entities({ kind: 'item' })[0].item, { name: 'dirt', count: 6 })
  assert.deepEqual(await p.toss('t1', { item: 'dirt', slot: 1 }), { status: 'no-item', count: 0 })
  assert.deepEqual(await p.toss('t1', { item: 'dirt', slot: 9 }), { status: 'no-item', count: 0 })
})

test('the fake is never settling unless told', () => {
  const p = createFake()
  assert.deepEqual([p.isSettling(), p.self().settling], [false, false])
})

test('world.settle sets the settling flag that isSettling and self report', () => {
  const p = createFake()
  p.world.settle(true)
  assert.deepEqual([p.isSettling(), p.self().settling], [true, true])
  p.world.settle(false)
  assert.deepEqual([p.isSettling(), p.self().settling], [false, false])
})

for (const [settles, expected] of [[true, true], [false, false]]) {
  test(`with settles: ${settles} the body is settling after the offline return: ${expected}`, async () => {
    const p = owned({ settles, offlineScale: 0.001 })
    const pending = p.offline('t1', { ms: 20000 })
    await tick()
    assert.equal(p.isSettling(), false, 'offline is its own state')
    await pending
    assert.equal(p.isSettling(), expected)
  })

  test(`with settles: ${settles} world.respawn emits respawned and settling is ${expected}`, () => {
    const p = createFake({ settles, self: { pos: at(3, 64, 4), dimension: 'overworld' } })
    const seen = []
    p.onBodyEvent(e => seen.push(e))
    p.world.respawn()
    assert.deepEqual(seen, [{ kind: 'respawned', pos: at(3, 64, 4), dimension: 'overworld' }])
    assert.equal(p.isSettling(), expected)
  })
}

test('drive records controls and look; a stale token throws cut', () => {
  const p = owned()
  p.drive('t1', { controls: { forward: true, jump: true } })
  p.drive('t1', { controls: { jump: false }, look: { yaw: 90, pitch: 10 } })
  assert.deepEqual(p.world.state.controls, { forward: true, jump: false })
  assert.deepEqual([p.world.state.yaw, p.world.state.pitch], [90, 10])
  p.drive('t1', { look: { dyaw: 300 } })
  assert.equal(p.world.state.yaw, 30)
  assert.throws(() => p.drive('old', { controls: { back: true } }), { code: 'cut' })
  assert.equal(p.world.state.controls.back, undefined)
})

for (const [label, clear] of [['stopDriving', p => p.stopDriving()], ['an owner change', p => p.setOwner('t2')], ['a null owner', p => p.setOwner(null)]]) {
  test(`fake drive: ${label} clears the controls`, () => {
    const p = owned()
    p.drive('t1', { controls: { forward: true } })
    clear(p)
    assert.deepEqual(p.world.state.controls, {})
  })
}

test('fakes do not share nested default self objects', () => {
  const first = createFake()
  first.world.state.self.pos.x = 99
  first.world.state.self.experience.level = 7
  first.world.state.self.effects.push('speed')
  const second = createFake()
  assert.equal(second.world.state.self.pos.x, 0)
  assert.equal(second.world.state.self.experience.level, 0)
  assert.deepEqual(second.world.state.self.effects, [])
})

test('a successful sleep puts the body in bed; an acting call then leaves it first', async () => {
  const p = owned({ time: 14000, skipNight: false, blocks: { '1,64,0': 'red_bed' } })
  assert.equal(p.self().isSleeping, false)
  assert.equal((await p.sleep('t1', { pos: at(1, 64, 0) })).status, 'sleeping')
  assert.equal(p.self().isSleeping, true)
  await p.look('t1', { yaw: 0, pitch: 0 })
  assert.equal(p.self().isSleeping, false)
})
