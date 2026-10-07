// Why JavaScript: tests primitives.mjs, which stays JS: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
import { test, mock } from 'node:test'
import assert from 'node:assert/strict'
import { createPrimitives, createPrimitivesFromBot, mcToMineflayerLook, mineflayerToMcLook } from './primitives.mjs'
import { EventEmitter } from 'node:events'
import { stubBot, names, Vec3 } from './stub-bot.mjs'
import registryFor from 'prismarine-registry'
import { fixDigMaterials } from './dig-materials.mjs'

// the calls a bot received, minus the blockAt reads waitForWorld makes
const acted = bot => names(bot).filter(n => n !== 'blockAt')

const at = (x, y, z) => ({ x, y, z })
const SCALE = 0.01 // every time bound shrinks 100 times: 20 s becomes 200 ms

const rig = (spec, timeScale = SCALE) => {
  const bot = stubBot(spec)
  const p = createPrimitivesFromBot(bot, { timeScale })
  p.setOwner('t1')
  return { bot, p }
}

// Each case: a world where the primitive reaches the bot call named by `hangs`, the args, the cleanup call a cut or
// timeout must make (null when none is needed), and the status a timeout resolves with (null when it cannot time out).
const world = {
  blocks: { '2,64,0': 'oak_log', '3,64,0': 'chest', '2,64,1': 'red_bed', '0,63,5': 'stone', '0,64,2': 'furnace', '1,64,2': 'enchanting_table' },
  items: [{ name: 'bread', count: 2, slot: 36 }, { name: 'cobblestone', count: 4, slot: 37 }],
  entities: { 7: { id: 7, name: 'item', type: 'object', position: at(5, 64, 0), getDroppedItem: () => ({ name: 'stick', count: 1 }) }, 8: { id: 8, name: 'zombie', type: 'hostile', position: at(1, 64, 0), height: 1.9, health: 20 } },
  containers: { '3,64,0': [{ name: 'cobblestone', count: 5, slot: 0 }] }
}
const acting = [
  { name: 'moveTo', args: { pos: at(30, 64, 0) }, hang: 'goto', cleanup: 'setGoal', timeout: 'blocked' },
  { name: 'dig', args: { pos: at(2, 64, 0) }, hang: 'dig', cleanup: 'stopDigging', timeout: 'timeout' },
  { name: 'place', args: { pos: at(1, 64, 0), item: 'cobblestone' }, hang: 'placeBlock', cleanup: null, timeout: 'timeout' },
  { name: 'collect', args: { id: 7 }, hang: 'goto', cleanup: 'setGoal', timeout: 'timeout' },
  { name: 'inspectContainer', args: { pos: at(3, 64, 0) }, hang: 'openContainer', cleanup: null, timeout: 'timeout' },
  { name: 'transfer', args: { pos: at(3, 64, 0), direction: 'deposit', item: 'cobblestone', count: 2 }, hang: 'deposit', cleanup: 'closeWindow', timeout: 'timeout' },
  { name: 'furnace', args: { pos: at(0, 64, 2), op: 'read' }, hang: 'openFurnace', cleanup: null, timeout: 'timeout' },
  { name: 'enchant', args: { pos: at(1, 64, 2), op: 'offers', item: 'bread' }, hang: 'openEnchantmentTable', cleanup: null, timeout: 'failed' },
  { name: 'equip', args: { item: 'bread' }, hang: 'equip', cleanup: null, timeout: 'timeout' },
  { name: 'toss', args: { item: 'cobblestone' }, hang: 'toss', cleanup: null, timeout: 'timeout' },
  { name: 'eat', args: { item: 'bread' }, hang: 'consume', cleanup: 'deactivateItem', timeout: 'timeout' },
  { name: 'attack', args: { id: 8 }, hang: 'attack', cleanup: null, timeout: 'timeout' },
  { name: 'interact', args: { id: 8, item: 'bread' }, hang: 'equip', cleanup: null, timeout: 'timeout' },
  { name: 'unequip', args: {}, hang: 'unequip', cleanup: null, timeout: 'timeout', over: { held: { name: 'bread', count: 2 } } },
  { name: 'sleep', args: { pos: at(2, 64, 1) }, hang: 'sleep', cleanup: 'write', timeout: 'timeout', over: { entities: {} } },
  { name: 'look', args: { pos: at(1, 64, 1) }, hang: 'lookAt', cleanup: null, timeout: 'timeout' },
  { name: 'useOn', args: { pos: at(2, 64, 0) }, hang: 'activateBlock', cleanup: null, timeout: 'timeout' },
  { name: 'swim', args: { ms: 3000 }, hang: 'setControlState', cleanup: 'setControlState', timeout: 'timeout', over: { blocks: { ...world.blocks, '0,65,0': 'water' } } }
]
const hanging = c => ({ ...world, ...c.over, hang: [c.hang] })
const cutError = err => err.code === 'cut' && err.cut === true

// Clock-free tests: node:test mock timers replace setTimeout/Date, so no amount of machine load can make a scaled wait
// expire early. `flushIO` lets every pending microtask run; `driveClock` then advances the mock clock `stepMs` at a
// time until `promise` settles (a call that never settles fails after `maxSteps` instead of hanging).
const mockClock = t => t.mock.timers.enable({ apis: ['setTimeout', 'setInterval', 'Date'] })
const flushIO = () => new Promise(resolve => setImmediate(resolve))
// ticks 1 ms at a time until `reached()` holds (the call is parked on its hanging bot call)
const driveUntil = async (t, reached, maxSteps = 20000) => {
  for (let i = 0; i < maxSteps; i++) {
    await flushIO()
    if (reached()) return
    t.mock.timers.tick(1)
  }
  throw new Error('condition never reached')
}
const driveClock = async (t, promise, stepMs = 1, maxSteps = 20000) => {
  let done = false
  const tracked = promise.then(v => { done = true; return v }, e => { done = true; throw e })
  tracked.catch(() => {})
  for (let i = 0; i < maxSteps && !done; i++) {
    await flushIO()
    if (!done) t.mock.timers.tick(stepMs)
  }
  return tracked
}

test('isOwner follows setOwner and null means nobody', () => {
  const { p } = rig(world)
  assert.equal(p.isOwner('t1'), true)
  assert.equal(p.isOwner('t2'), false)
  p.setOwner(null)
  assert.equal(p.isOwner('t1'), false)
  assert.equal(p.isOwner(null), false)
})

for (const c of acting) {
  test(`${c.name}: a stale token rejects with cut and never touches the bot`, async () => {
    const { bot, p } = rig(hanging(c))
    await assert.rejects(p[c.name]('old', c.args), cutError)
    assert.deepEqual(names(bot), [])
  })

  test(`${c.name}: a null token rejects with cut`, async () => {
    const { p } = rig(hanging(c))
    await assert.rejects(p[c.name](null, c.args), cutError)
  })

  test(`${c.name}: setOwner during the call rejects with cut and runs the cleanup`, async t => {
    mockClock(t)
    const { bot, p } = rig(hanging(c))
    const call = p[c.name]('t1', c.args)
    call.catch(() => {})
    await driveUntil(t, () => names(bot).includes(c.hang))
    assert.ok(names(bot).includes(c.hang), `reached ${c.hang}`)
    p.setOwner('t2')
    await assert.rejects(call, cutError)
    const before = names(bot).length
    t.mock.timers.tick(60000)
    await flushIO()
    assert.equal(names(bot).length, before, 'nothing reaches the bot after the cut')
  })

  test(`${c.name}: a bot call that never returns hits the time bound`, async () => {
    const { bot, p } = rig(hanging(c))
    const result = await p[c.name]('t1', c.args)
    assert.equal(result.status, c.timeout)
  })
}

const withCleanup = acting.filter(c => c.cleanup)
for (const c of withCleanup) {
  test(`${c.name}: setOwner during the call runs the cleanup`, async t => {
    mockClock(t)
    const { bot, p } = rig(hanging(c))
    const call = p[c.name]('t1', c.args)
    call.catch(() => {})
    await driveUntil(t, () => names(bot).includes(c.hang))
    p.setOwner('t2')
    await assert.rejects(call, cutError)
    assert.ok(names(bot).includes(c.cleanup), `called ${c.cleanup}`)
  })

  test(`${c.name}: hitting the time bound runs the cleanup`, async () => {
    const { bot, p } = rig(hanging(c))
    await p[c.name]('t1', c.args)
    assert.ok(names(bot).includes(c.cleanup), `called ${c.cleanup}`)
  })
}

test('setOwner to the same token does not cut', async () => {
  const { p } = rig({ ...world, hang: ['dig'] })
  const call = p.dig('t1', { pos: at(2, 64, 0) })
  p.setOwner('t1')
  const result = await call
  assert.equal(result.status, 'timeout')
})

test('dig of air says what is there and what to do', async () => {
  const { p } = rig(world)
  const r = await p.dig('t1', { pos: at(9, 64, 9) })
  assert.equal(r.status, 'missing')
  assert.match(r.reason, /nothing to dig at 9 64 9 \(air\)/)
})

test('plain results are plain objects', async () => {
  const { p } = rig(world)
  const r = await p.look('t1', { yaw: 1, pitch: 0 })
  assert.deepEqual(r, { status: 'ok' })
})

const statuses = [
  ['dig', { pos: at(9, 64, 9) }, {}, 'missing'],
  ['dig', { pos: at(0, 63, 5) }, { pos: [0, 64, 0] }, 'unreachable'],
  ['dig', { pos: at(2, 64, 0) }, { blocks: { '2,64,0': 'bedrock' } }, 'cannot'],
  ['dig', { pos: at(2, 64, 0) }, {}, 'dug'],
  ['place', { pos: at(2, 64, 0), item: 'cobblestone' }, {}, 'occupied'],
  ['place', { pos: at(1, 64, 0), item: 'dirt' }, {}, 'no-item'],
  ['place', { pos: at(1, 70, 0), item: 'cobblestone' }, {}, 'no-support'],
  ['place', { pos: at(2, 65, 0), item: 'cobblestone' }, {}, 'placed'],
  ['equip', { item: 'sword' }, {}, 'no-item'],
  ['equip', { item: 'bread' }, {}, 'equipped'],
  ['toss', { item: 'sword' }, {}, 'no-item'],
  ['toss', { item: 'cobblestone', count: 0 }, {}, 'no-item'],
  ['toss', { item: 'cobblestone' }, {}, 'tossed'],
  ['eat', { item: 'bread' }, { items: [] }, 'no-food'],
  ['eat', { item: 'bread' }, { food: 20 }, 'full'],
  ['eat', { item: 'bread' }, {}, 'ate'],
  ['attack', { id: 99 }, {}, 'gone'],
  ['attack', { id: 8 }, { entities: { 8: { id: 8, name: 'zombie', type: 'hostile', position: at(9, 64, 0), height: 1.9 } } }, 'out-of-reach'],
  ['sleep', { pos: at(9, 64, 9) }, {}, 'missing'],
  ['sleep', { pos: at(2, 64, 1), notNight: true }, {}, 'not-night'],
  ['sleep', { pos: at(2, 64, 1), monstersNear: true }, {}, 'monsters-near'],
  ['sleep', { pos: at(2, 64, 1), }, { entities: {} }, 'sleeping'],
  ['inspectContainer', { pos: at(9, 64, 9) }, {}, 'missing'],
  ['inspectContainer', { pos: at(3, 64, 0) }, {}, 'ok'],
  ['transfer', { pos: at(3, 64, 0), direction: 'withdraw', item: 'diamond', count: 1 }, {}, 'no-item'],
  ['transfer', { pos: at(3, 64, 0), direction: 'deposit', item: 'cobblestone', count: 2 }, {}, 'ok'],
  ['collect', { id: 99 }, {}, 'gone'],
  ['moveTo', { pos: at(0, 64, 0) }, {}, 'arrived']
]
for (const [name, args, over, status] of statuses) {
  test(`${name} ${JSON.stringify(args)} resolves ${status}`, async () => {
    const { p } = rig({ ...world, ...over })
    const result = await p[name]('t1', args)
    assert.equal(result.status, status)
  })
}

test('inspectContainer reports the size and the free slots', async () => {
  const { p } = rig(world)
  const result = await p.inspectContainer('t1', { pos: at(3, 64, 0) })
  assert.equal(result.size, 27)
  assert.equal(result.free, 26)
})

test(`attack ${JSON.stringify({ id: 8 })} resolves hit`, async t => {
  mockClock(t)
  const { p } = rig({ ...world })
  const result = await driveClock(t, p.attack('t1', { id: 8 }))
  assert.equal(result.status, 'hit')
})

// attack's `hurt`: the server's entityHurt for the target after the swing (mineflayer never sets health on others)
const ownWorld = () => ({ ...world, entities: { ...world.entities } }) // tests here delete entities
const hurtCases = [
  ['an entityHurt for the target', bot => bot.emit('entityHurt', bot.entities[8]), { status: 'hit', hurt: true }],
  ['no entityHurt', () => {}, { status: 'hit', hurt: false }],
  ['an entityHurt for another entity', bot => bot.emit('entityHurt', { id: 99 }), { status: 'hit', hurt: false }],
  ['an entityDead for the target', bot => bot.emit('entityDead', bot.entities[8]), { status: 'killed', hurt: true }],
  ['an entityDead for another entity', bot => bot.emit('entityDead', { id: 99 }), { status: 'hit', hurt: false }],
  ['the target removed', bot => { delete bot.entities[8] }, { status: 'killed', hurt: true }]
]
for (const [label, onSwing, expected] of hurtCases) {
  test(`attack with ${label} reports hurt ${expected.hurt} and drops its listener`, async t => {
    mockClock(t)
    const { bot, p } = rig(ownWorld())
    bot.attack = async () => { onSwing(bot) }
    const result = await driveClock(t, p.attack('t1', { id: 8 }))
    assert.equal(result.status, expected.status)
    assert.equal(result.hurt, expected.hurt)
    assert.equal(bot.listenerCount('entityHurt'), 0)
    assert.equal(bot.listenerCount('entityDead'), 0)
  })
}

test('attack resolves early when entityHurt arrives after a delay', async () => {
  const { bot, p } = rig(ownWorld())
  bot.attack = async () => { setTimeout(() => bot.emit('entityHurt', bot.entities[8]), 20 * SCALE) }
  assert.deepEqual(await p.attack('t1', { id: 8 }), { status: 'hit', health: 20, hurt: true })
  assert.equal(bot.listenerCount('entityHurt'), 0)
})

test('attack resolves well under the 300 ms wait when entityHurt arrives, at timeScale 1', async () => {
  const bot = stubBot(ownWorld())
  const p = createPrimitivesFromBot(bot)
  p.setOwner('t1')
  bot.attack = async () => { bot.emit('entityHurt', bot.entities[8]) }
  const started = Date.now()
  assert.equal((await p.attack('t1', { id: 8 })).hurt, true)
})

test('attack resolves well under the 300 ms wait when entityDead arrives, at timeScale 1', async () => {
  const bot = stubBot(ownWorld())
  const p = createPrimitivesFromBot(bot)
  p.setOwner('t1')
  bot.attack = async () => { setTimeout(() => bot.emit('entityDead', bot.entities[8]), 10) }
  const started = Date.now()
  assert.equal((await p.attack('t1', { id: 8 })).status, 'killed')
  assert.equal(bot.listenerCount('entityDead'), 0)
})

test('attack drops its listener when the swing throws', async () => {
  const { bot, p } = rig(ownWorld())
  bot.attack = async () => { throw new Error('boom') }
  assert.equal((await p.attack('t1', { id: 8 })).status, 'failed')
  assert.equal(bot.listenerCount('entityHurt'), 0)
  assert.equal(bot.listenerCount('entityDead'), 0)
})

const badArgs = [['moveTo', {}], ['moveTo', { pos: at(NaN, 64, 0) }], ['moveTo', { pos: at(1, 64, 0), range: -2 }], ['moveTo', { pos: at(1, 64, 0), timeoutS: 0 }], ['moveTo', { pos: at(1, 64, 0), timeoutS: 'soon' }], ['moveTo', { pos: at(1, 64, 0), maxDistance: -1 }], ['dig', {}], ['place', { pos: at(1, 1, 1) }], ['transfer', { pos: at(1, 1, 1), direction: 'sideways', item: 'x', count: 1 }], ['look', {}], ['collect', {}], ['attack', {}], ['toss', {}], ['toss', { item: 7 }], ['furnace', {}], ['furnace', { pos: at(0, 64, 2), op: 'stir' }], ['furnace', { pos: at(0, 64, 2), op: 'load' }], ['furnace', { pos: at(0, 64, 2), op: 'load', input: { item: 'coal', count: 0 } }], ['enchant', {}], ['enchant', { pos: at(1, 64, 2), op: 'stir', item: 'bread' }], ['enchant', { pos: at(1, 64, 2), op: 'offers' }], ['enchant', { pos: at(1, 64, 2), op: 'enchant', item: 'bread', choice: 3 }]]
for (const [name, args] of badArgs) {
  test(`${name} with bad args rejects with bad-args`, async () => {
    const { p } = rig(world)
    await assert.rejects(p[name]('t1', args), err => err.code === 'bad-args')
  })
}

const badTrades = [{}, { villager: '' }, { villager: 'v-1' }, { villager: 'v-1', op: 'sell' }, { villager: 'v-1', op: 'buy' }, { villager: 'v-1', op: 'buy', offer: -1 }, { villager: 'v-1', op: 'buy', offer: 0, times: 0 }, { villager: 'v-1', op: 'buy', offer: 0, times: 1.5 }]
for (const args of badTrades) {
  test(`trade with ${JSON.stringify(args)} rejects with bad-args`, async () => {
    const { p } = rig(world)
    await assert.rejects(p.trade('t1', args), err => err.code === 'bad-args')
  })
}

test('trade with an unknown villager uuid resolves gone', async () => {
  const { p } = rig(world)
  assert.deepEqual(await p.trade('t1', { villager: 'nobody', op: 'offers' }), { status: 'gone' })
})

test('self reports the body in the contract shape', () => {
  const { p } = rig(world)
  const s = p.self()
  assert.deepEqual(Object.keys(s).sort(), ['chunkLoaded', 'dimension', 'effects', 'equipment', 'experience', 'food', 'foodSaturation', 'health', 'held', 'inLava', 'inWater', 'inventory', 'isSleeping', 'onFire', 'onGround', 'oxygen', 'players', 'pos', 'rainState', 'settling', 'thunderState', 'timeOfDay', 'username', 'vehicle'])
  assert.equal(s.timeOfDay, 15000)
  assert.deepEqual(s.inventory[0], { name: 'bread', count: 2, slot: 36 })
})

test('self lists the other players in the player list (the tab list), not the body itself', () => {
  const { bot, p } = rig(world)
  bot.players = { [bot.username]: {}, Steve: {}, Alex: {} }
  assert.deepEqual(p.self().players, ['Steve', 'Alex'])
  delete bot.players
  assert.deepEqual(p.self().players, [])
})

test('self reports the raw rain and thunder levels (engine.senses derives raining and thundering)', () => {
  const { bot, p } = rig(world)
  Object.assign(bot, { rainState: 0.5, thunderState: 0.25 })
  assert.deepEqual([p.self().rainState, p.self().thunderState], [0.5, 0.25])
  Object.assign(bot, { rainState: undefined, thunderState: undefined })
  assert.deepEqual([p.self().rainState, p.self().thunderState], [0, 0])
})

test('entities carry baby and uuid for a mob', () => {
  const calf = { id: 9, name: 'cow', type: 'passive', uuid: 'u-9', position: at(2, 64, 0), height: 1.4, metadata: { 8: true } }
  const bot = stubBot({ ...world, entities: { ...world.entities, 9: calf } })
  bot.registry.entitiesByName = { cow: { metadataKeys: ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'baby'] } }
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  const cow = p.entities({}).find(e => e.id === 9)
  assert.equal(cow.baby, true)
  assert.equal(cow.uuid, 'u-9')
  assert.equal('baby' in p.entities({}).find(e => e.id === 8), false)
})

test('entities name a villager profession from its villager_data', () => {
  const villager = (id, data) => ({ id, name: 'villager', type: 'passive', position: at(2, 64, 0), height: 1.95, ...(data && { metadata: { 9: data } }) })
  const bot = stubBot({ ...world, entities: { ...world.entities, 12: villager(12, { villagerProfession: 5, level: 2 }), 13: villager(13, { villagerProfession: 'minecraft:librarian' }), 14: villager(14), 15: villager(15, { villagerProfession: 0 }) } })
  bot.registry.entitiesByName = { villager: { metadataKeys: ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'baby', 'villager_data'] } }
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  const found = id => p.entities({}).find(e => e.id === id)
  assert.deepEqual([12, 13, 14, 15].map(id => found(id).profession), ['farmer', 'librarian', 'unemployed', 'unemployed'])
  assert.equal('profession' in found(8), false)
})

test('entities say whether a mob is on a lead and whether the body holds it', () => {
  const cow = id => ({ id, name: 'cow', type: 'passive', position: at(2, 64, 0), height: 1.4 })
  const bot = stubBot({ ...world, entities: { ...world.entities, 9: cow(9), 10: cow(10), 11: cow(11) } })
  bot.entity.id = 4
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  bot._client.emit('attach_entity', { entityId: 9, vehicleId: bot.entity.id })
  bot._client.emit('attach_entity', { entityId: 10, vehicleId: bot.entity.id + 50 })
  const found = id => p.entities({}).find(e => e.id === id)
  assert.deepEqual([found(9).leashed, found(9).leashedToMe], [true, true])
  assert.deepEqual([found(10).leashed, found(10).leashedToMe], [true, false])
  assert.equal('leashed' in found(11), false)
  bot._client.emit('attach_entity', { entityId: 9, vehicleId: 0 })
  assert.equal('leashed' in found(9), false)
})

test('entities filters by kind and sorts by distance', () => {
  const { p } = rig(world)
  const all = p.entities({})
  assert.deepEqual(all.map(e => e.id), [8, 7])
  assert.deepEqual(p.entities({ kind: 'item' }).map(e => e.item), [{ name: 'stick', count: 1 }])
  assert.deepEqual(p.entities({ kind: 'hostile', radius: 0.5 }), [])
})

const dropEntity = (extra) => ({ 9: { id: 9, name: 'item', type: 'object', position: at(2, 64, 0), ...extra } })
const stack = { itemCount: 3, itemId: 1032, components: [] }
const unreadable = () => { throw new TypeError("Cannot read properties of undefined (reading 'present')") }

// Each case: the entity fields of a dropped item (as mineflayer 4.x leaves them) and the item the entities primitive reports.
const dropShapes = [
  ['an Item from getDroppedItem', { getDroppedItem: () => ({ name: 'stick', count: 2 }) }, { name: 'stick', count: 2 }],
  ['a prismarine item with only a type', { getDroppedItem: () => ({ type: 1032, count: 2 }) }, { name: 'egg', count: 2 }],
  ['a metadata slot {itemId, itemCount}', { getDroppedItem: () => null, metadata: [null, null, null, null, null, null, null, null, stack] }, { name: 'egg', count: 3 }],
  ['a metadata slot with present: true', { getDroppedItem: () => null, metadata: { 8: { present: true, ...stack } } }, { name: 'egg', count: 3 }],
  ['a metadata slot without a count', { metadata: [{ itemId: 1032 }] }, { name: 'egg', count: 1 }],
  ['a pre-1.13 slot with blockId', { metadata: [{ blockId: 1032, itemCount: 4 }] }, { name: 'egg', count: 4 }],
  ['getDroppedItem throwing on missing metadata, slot found by scan', { getDroppedItem: unreadable, metadata: [stack] }, { name: 'egg', count: 3 }],
  ['an id the registry lacks', { metadata: [{ itemId: 99999, itemCount: 2 }] }, { name: 'unknown', count: 2 }],
  ['an empty slot (present: false)', { getDroppedItem: () => null, metadata: [{ present: false, itemId: 1032, itemCount: 1 }] }, null],
  ['a zero count', { getDroppedItem: () => null, metadata: [{ itemId: 1032, itemCount: 0 }] }, null],
  ['no metadata at all', { getDroppedItem: unreadable, metadata: [] }, null]
]

for (const [label, extra, expected] of dropShapes) {
  test(`entities reports the dropped item from ${label}`, () => {
    const bot = stubBot({ entities: dropEntity(extra) })
    bot.registry.items = { 1032: { name: 'egg' } }
    const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
    assert.deepEqual(p.entities({ kind: 'item' }).map(e => e.item), [expected])
  })
}

test('the primitives have no raw block scan (jobs read perception seenBlocks; blockAt is the one cell lookup)', () => {
  const { p } = rig(world)
  assert.equal(p.blocks, undefined)
})

test('blockAt gives a name and a plain position', () => {
  const { p } = rig(world)
  assert.deepEqual(p.blockAt(at(2, 64, 0)), { name: 'oak_log', pos: at(2, 64, 0), fullCube: true })
})

test('body events: health drop is hurt, death and respawn are forwarded, unsubscribe works', () => {
  const { bot, p } = rig(world)
  const seen = []
  const off = p.onBodyEvent(e => seen.push(e))
  bot.health = 14
  bot.emit('health')
  bot.emit('death')
  bot.emit('respawn')
  bot.emit('spawn')
  bot.emit('chat', 'Ann', 'hi')
  off()
  bot.emit('wake')
  assert.deepEqual(seen.map(e => e.kind), ['hurt', 'died', 'spawned', 'respawned', 'chat'])
  assert.equal(seen[0].health, 14)
  assert.deepEqual(seen[4], { kind: 'chat', from: 'Ann', message: 'hi' })
})

test('body events: another player joining or leaving is reported, the body itself is not', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.emit('playerJoined', { username: 'Ann', uuid: 'u1' })
  bot.emit('playerJoined', { username: 'Stub' })
  bot.emit('playerLeft', { username: 'Ann' })
  bot.emit('playerLeft', { username: 'Stub' })
  assert.deepEqual(seen, [{ kind: 'player-joined', player: 'Ann' }, { kind: 'player-left', player: 'Ann' }])
})

test('body events: weather-levels carries the raw levels when they change (engine.senses decides what flips)', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  const set = (rainState, thunderState) => { Object.assign(bot, { rainState, thunderState }); bot.emit('weatherUpdate') }
  set(0.1, 0)
  set(0.1, 0)
  set(0.6, 1)
  assert.deepEqual(seen.filter(e => e.kind.startsWith('weather')), [
    { kind: 'weather-levels', rain: 0.1, thunder: 0 },
    { kind: 'weather-levels', rain: 0.6, thunder: 1 }])
})

const sleepBar = (key, ...counts) => ({ translate: key, with: counts.map(n => ({ text: String(n) })), toString: () => key })

test('body events: the action bar sleep count is a sleep-status event; other action bar lines are not', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.emit('actionBar', sleepBar('sleep.players_sleeping', 1, 7))
  bot.emit('actionBar', sleepBar('sleep.skipping_night'))
  bot.emit('actionBar', sleepBar('item.minecraft.bread'))
  bot.emit('actionBar', sleepBar('sleep.players_sleeping', 0, 7))
  assert.deepEqual(seen, [{ kind: 'sleep-status', sleeping: 1, needed: 7 }, { kind: 'sleep-status', skipping: true },
    { kind: 'sleep-status', sleeping: 0, needed: 7 }])
})

test('close quits the bot', async () => {
  const { bot, p } = rig(world)
  await p.close()
  assert.deepEqual(names(bot), ['quit'])
})

// ---- survival sensing ----

const withBot = (patch, spec = world) => {
  const bot = stubBot(spec)
  patch(bot)
  return createPrimitivesFromBot(bot, { timeScale: SCALE })
}
test('body events: a whisper from another player is recorded, one from the body itself is not', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.emit('whisper', 'Ann', 'psst')
  bot.emit('whisper', 'Stub', 'echo')
  assert.deepEqual(seen, [{ kind: 'whisper', from: 'Ann', message: 'psst' }])
})

const keysFor = metadataKeys => ({ entitiesByName: { player: { metadataKeys } } })

test('self().onGround follows entity.onGround', () => {
  const { bot, p } = rig(world)
  const states = [true, false].map(onGround => { bot.entity.onGround = onGround; return p.self().onGround })
  assert.deepEqual(states, [true, false])
})

test('self reports active effects by snake_case registry name, empty when none', () => {
  assert.deepEqual(withBot(() => {}).self().effects, [])
  const effects = [{ id: 11, name: 'FireResistance', amplifier: 0, duration: 600 }, { id: 1, name: 'Speed', amplifier: 1, duration: 40 }]
  assert.deepEqual(withBot(() => {}, { ...world, effects }).self().effects, [
    { name: 'speed', amplifier: 1, duration: 40 },
    { name: 'fire_resistance', amplifier: 0, duration: 600 }
  ])
})

test('self reports vitals from the bot', () => {
  const p = withBot(bot => Object.assign(bot, { health: 7, food: 3, foodSaturation: 1.5, oxygenLevel: 11, isSleeping: true, game: { dimension: 'the_nether' }, experience: { level: 4, points: 90, progress: 0.25 } }))
  const s = p.self()
  assert.deepEqual([s.health, s.food, s.foodSaturation, s.oxygen, s.isSleeping, s.dimension], [7, 3, 1.5, 11, true, 'the_nether'])
  assert.deepEqual(s.experience, { level: 4, points: 90, progress: 0.25 })
})

test('self has sane values on a bot that never sent them', () => {
  const s = createPrimitivesFromBot(stubBot(world), { timeScale: SCALE }).self()
  assert.deepEqual([s.oxygen, s.isSleeping, s.onFire, s.inWater, s.inLava], [20, false, false, false, false])
  assert.deepEqual(s.experience, { level: 0, points: 0, progress: 0 })
})

// onFire reads bit 0x01 of the shared-flags metadata byte; its index comes from the registry when it knows it.
const fireCases = [
  ['flag bit set', [1], undefined, true],
  ['fire and crouching', [3], undefined, true],
  ['crouching only', [2], undefined, false],
  ['no flags', [0], undefined, false],
  ['no metadata', undefined, undefined, false],
  ['index from the registry', [0, 0, 1], ['x', 'y', 'shared_flags'], true],
  ['registry index holds no fire', [1, 0, 0], ['x', 'y', 'shared_flags'], false]
]
for (const [label, metadata, keys, expected] of fireCases) {
  test(`self.onFire with ${label} is ${expected}`, () => {
    const p = withBot(bot => {
      bot.entity.metadata = metadata
      bot.entity.name = 'player'
      if (keys) bot.registry = { ...bot.registry, ...keysFor(keys) }
    })
    assert.equal(p.self().onFire, expected)
  })
}

// inWater and inLava prefer the physics flags, and read the block at the feet when physics has not run
const liquidCases = [
  ['isInWater set', { isInWater: true }, {}, [true, false]],
  ['isInLava set', { isInLava: true }, {}, [false, true]],
  ['flags false despite a water block', { isInWater: false, isInLava: false }, { '0,64,0': 'water' }, [false, false]],
  ['no flags, water at the feet', {}, { '0,64,0': 'water' }, [true, false]],
  ['no flags, lava at the feet', {}, { '0,64,0': 'lava' }, [false, true]],
  ['no flags, stone at the feet', {}, { '0,64,0': 'stone' }, [false, false]]
]
for (const [label, entityFlags, blocks, expected] of liquidCases) {
  test(`self.inWater and inLava with ${label}`, () => {
    const p = withBot(bot => Object.assign(bot.entity, entityFlags), { ...world, blocks })
    const s = p.self()
    assert.deepEqual([s.inWater, s.inLava], expected)
  })
}

test('entities tells lying players from standing ones and gives usernames', () => {
  const player = (id, pose) => ({ id, type: 'player', name: 'player', username: `P${id}`, position: at(id, 64, 0), metadata: pose === undefined ? [] : [0, 0, 0, 0, 0, 0, pose] })
  const p = withBot(bot => { bot.entities = { 1: player(1, 2), 2: player(2, 0), 3: player(3) } }, { ...world, blocks: {} })
  const found = p.entities({ kind: 'player' })
  assert.deepEqual(found.map(e => [e.username, e.lyingDown]), [['P1', true], ['P2', false], ['P3', false]])
})

test('a player lying in a bed reports lyingDown and the low height the sight line aims at', () => {
  const sleeper = { id: 5, type: 'player', name: 'player', username: 'P5', position: at(5, 64.6875, 0), height: 0.2, metadata: [0, 0, 0, 0, 0, 0, 2] }
  const p = withBot(bot => { bot.entities = { 5: sleeper } }, { ...world, blocks: { '4,64,0': 'red_bed', '5,64,0': 'red_bed' } })
  assert.deepEqual(p.entities({ kind: 'player' }).map(e => [e.lyingDown, e.height]), [[true, 0.2]])
})

test('entities finds the pose index through the registry', () => {
  const p = withBot(bot => {
    bot.registry = { ...bot.registry, ...keysFor(['shared_flags', 'pose']) }
    bot.entities = { 1: { id: 1, type: 'player', name: 'player', username: 'A', position: at(1, 64, 0), metadata: [0, 2] } }
  })
  assert.equal(p.entities({})[0].lyingDown, true)
})

test('entities does not put lyingDown or username on mobs, and gives no sight fields', () => {
  const [zombie] = rig(world).p.entities({ kind: 'hostile' })
  assert.deepEqual(Object.keys(zombie).sort(), ['distance', 'height', 'id', 'kind', 'name', 'pos'])
})

const mobCases = [
  ['a creeper typed hostile', { name: 'creeper', type: 'hostile' }, 'hostile', true],
  ['a zombie typed hostile', { name: 'zombie', type: 'hostile' }, 'hostile', false],
  ['a creeper with only a hostile kind', { name: 'creeper', kind: 'Hostile mobs' }, 'hostile', true],
  ['a cow', { name: 'cow', type: 'animal', kind: 'Passive mobs' }, 'passive', false]
]
for (const [label, fields, kind, creeper] of mobCases) {
  test(`entities classifies ${label}`, () => {
    const p = withBot(bot => { bot.entities = { 5: { id: 5, position: at(3, 64, 0), ...fields } } }, { ...world, blocks: {} })
    const [e] = p.entities({})
    assert.deepEqual([e.kind, e.name, Boolean(e.creeper)], [kind, fields.name, creeper])
  })
}

// ---- death and respawn events ----

test('the died event carries the death position, the inventory and the experience at that moment', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.entity.position = new Vec3(4, 70, 2)
  bot.experience = { level: 5, points: 120, progress: 0.3 }
  bot.game = { dimension: 'the_nether' }
  bot.emit('death')
  bot.entity.position = new Vec3(0, 64, 0)
  bot.experience = { level: 0, points: 0, progress: 0 }
  assert.deepEqual(seen, [{
    kind: 'died',
    pos: at(4, 70, 2),
    dimension: 'the_nether',
    inventory: [{ name: 'bread', count: 2, slot: 36 }, { name: 'cobblestone', count: 4, slot: 37 }],
    experience: { level: 5, points: 120 }
  }])
})

const emptiedAtDeath = (snapshotOn) => {
  const items = []
for (const [block, cause] of [['lava', 'lava'], ['fire', 'fire'], ['soul_fire', 'fire']]) {
  test(`the died event names the cause ${cause} when the body dies standing in ${block}`, () => {
    const { bot, p } = rig({ ...world, blocks: { ...world.blocks, '4,70,2': block } })
    const seen = []
    p.onBodyEvent(e => seen.push(e))
    bot.entity.position = new Vec3(4, 70, 2)
    bot.emit('death')
    assert.equal(seen[0].cause, cause)
  })
}

test('the died event names the cause void below the world', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.entity.position = new Vec3(4, -70, 2)
  bot.emit('death')
  assert.equal(seen[0].cause, 'void')
})

test('the hurt event carries the cause of the damage the same way', () => {
  const { bot, p } = rig({ ...world, blocks: { ...world.blocks, '0,64,0': 'lava' } })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.health = 20
  bot.emit('health')
  bot.health = 12
  bot.emit('health')
  assert.equal(seen.find(e => e.kind === 'hurt').cause, 'lava')
})

  const { bot, p } = rig({ ...world, items })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  items.push({ name: 'bread', count: 2, slot: 36 }, { name: 'iron_sword', count: 1, slot: 37 }) // picked up after the connect
  bot.health = 20
  for (let i = 0; i < 20; i++) bot.emit(snapshotOn)
  items.length = 0 // the server clears the slots before the death event is read
  bot.health = 0
  bot.emit('death')
  return seen.find(e => e.kind === 'died')
}

for (const event of ['health', 'physicsTick']) {
  test(`the died event keeps the inventory of the last ${event} snapshot when the slots are already empty`, () => {
    assert.deepEqual(emptiedAtDeath(event).inventory, [{ name: 'bread', count: 2, slot: 36 }, { name: 'iron_sword', count: 1, slot: 37 }])
  })
}

test('a health event at zero health does not overwrite the snapshot with the emptied inventory', () => {
  const items = []
  const { bot, p } = rig({ ...world, items })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  items.push({ name: 'bread', count: 2, slot: 36 })
  bot.emit('health')
  items.length = 0
  bot.health = 0
  bot.emit('health')
  bot.emit('death')
  assert.deepEqual(seen.find(e => e.kind === 'died').inventory, [{ name: 'bread', count: 2, slot: 36 }])
})

test('a death with the live inventory intact uses it, not the snapshot', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.emit('death')
  assert.equal(seen[0].inventory.length, 2)
})

test('the died event reports zero experience when the bot has none yet', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.emit('death')
  assert.deepEqual(seen[0].experience, { level: 0, points: 0 })
})

test('the respawned event waits for the spawn and carries the new position and dimension', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.game = { dimension: 'overworld' }
  bot.emit('respawn')
  assert.deepEqual(seen, [])
  bot.entity.position = new Vec3(100, 65, -3)
  bot.emit('spawn')
  assert.deepEqual(seen, [{ kind: 'spawned' }, { kind: 'respawned', pos: at(100, 65, -3), dimension: 'overworld' }])
  bot.emit('spawn')
  assert.deepEqual(seen.map(e => e.kind), ['spawned', 'respawned', 'spawned'])
})

// ---- offline ----

const MIN = 60000
const online = async (extra = {}) => {
  const bots = []
  const opts = { host: 'h', port: 1, username: 'u' }
  const seenOpts = []
  const connect = extra.connect ?? (async o => { seenOpts.push(o); const b = stubBot(world); bots.push(b); return b })
  const p = await createPrimitives(opts, { connect, timeScale: extra.timeScale ?? 0.0001 })
  p.setOwner('t1')
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  return { p, bots, seen, seenOpts, opts }
}

test('offline quits, waits, reconnects with the same params and rebinds every primitive', async () => {
  const { p, bots, seen, seenOpts, opts } = await online()
  const result = await p.offline('t1', { ms: 1000 })
  assert.deepEqual(result, { status: 'ok', ms: 1000 })
  assert.equal(bots.length, 2)
  assert.deepEqual(seenOpts, [opts, opts])
  assert.deepEqual(acted(bots[0]), ['quit'])
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'online'])
  bots[1].health = 7
  assert.equal(p.self().health, 7)
  await p.look('t1', { yaw: 0, pitch: 0 })
  assert.deepEqual(acted(bots[1]), ['look'])
  assert.deepEqual(acted(bots[0]), ['quit'])
})

test('offline reports its ms in the events and the online event carries the position', async () => {
  const { p, seen } = await online()
  await p.offline('t1', { ms: 1000 })
  assert.deepEqual(seen, [{ kind: 'offline', ms: 1000 }, { kind: 'online', pos: at(0, 64, 0) }])
})

test('a sleep count the server sends while the rejoining bot loads its world is reported after the online event', async () => {
  const bots = []
  const connect = async () => { const b = stubBot({ ...world, unloaded: bots.length > 0 }); bots.push(b); return b }
  const { p, seen } = await online({ connect })
  const away = p.offline('t1', { ms: 1000 })
  while (bots.length < 2) await new Promise(resolve => setTimeout(resolve, 1))
  bots[1].emit('actionBar', sleepBar('sleep.players_sleeping', 1, 2))
  bots[1].loadWorld()
  await away
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'online', 'sleep-status'])
  assert.deepEqual(seen[2], { kind: 'sleep-status', sleeping: 1, needed: 2 })
})

test('events of the new bot reach listeners and the old bot goes quiet', async () => {
  const { p, bots, seen } = await online()
  await p.offline('t1', { ms: 1000 })
  bots[0].emit('end', 'quit')
  bots[0].emit('chat', 'Ann', 'ghost')
  bots[1].emit('chat', 'Ann', 'hi')
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'online', 'chat'])
})

const clampCases = [[undefined, 5 * MIN], [null, 5 * MIN], [100 * MIN, 10 * MIN], [0, 0], [12.7, 12]]
for (const [ms, expected] of clampCases) {
  test(`offline with ms ${ms} waits ${expected}`, async () => {
    const { p } = await online({ timeScale: 1e-6 })
    assert.equal((await p.offline('t1', ms === undefined ? {} : { ms })).ms, expected)
  })
}

for (const ms of ['soon', -5, NaN, Infinity]) {
  test(`offline with ms ${ms} rejects with bad-args and stays online`, async () => {
    const { p, bots } = await online()
    await assert.rejects(p.offline('t1', { ms }), err => err.code === 'bad-args')
    assert.equal(bots.length, 1)
    assert.deepEqual(acted(bots[0]), [])
  })
}

test('offline with a stale token rejects with cut and never quits', async () => {
  const { p, bots } = await online()
  await assert.rejects(p.offline('old', { ms: 1000 }), err => err.code === 'cut')
  assert.deepEqual(acted(bots[0]), [])
})

test('offline from createPrimitivesFromBot is unsupported and leaves the bot alone', async () => {
  const { bot, p } = rig(world)
  assert.deepEqual(await p.offline('t1', { ms: 1000 }), { status: 'unsupported' })
  assert.deepEqual(names(bot), [])
})

test('a cut during the wait still reconnects, then resolves cut', async () => {
  const { p, bots, seen } = await online()
  const pending = p.offline('t1', { ms: 10 * MIN })
  p.setOwner('t2')
  assert.deepEqual(await pending, { status: 'cut' })
  assert.equal(bots.length, 2)
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'online'])
  assert.equal(p.self().username, 'Stub')
})

test('close during the wait cancels the reconnect', async () => {
  const { p, bots, seen } = await online()
  const pending = p.offline('t1', { ms: 10 * MIN })
  await p.close()
  assert.deepEqual(await pending, { status: 'closed' })
  assert.equal(bots.length, 1)
  assert.deepEqual(seen.map(e => e.kind), ['offline'])
})

test('close while the reconnect is in flight quits the bot it produces', async () => {
  const bots = []
  let release
  const gate = new Promise(resolve => { release = resolve })
  let calls = 0
  const connect = async () => {
    calls += 1
    if (calls > 1) await gate
    const b = stubBot(world)
    bots.push(b)
    return b
  }
  const { p, seen } = await online({ connect })
  const pending = p.offline('t1', { ms: 1 })
  while (calls < 2) await new Promise(resolve => setTimeout(resolve, 1))
  await p.close()
  release()
  assert.deepEqual(await pending, { status: 'closed' })
  assert.deepEqual(acted(bots[1]), ['quit'])
  assert.deepEqual(seen.map(e => e.kind), ['offline'])
})

test('a failed reconnect is retried', async () => {
  let calls = 0
  const bots = []
  const connect = async () => {
    calls += 1
    if (calls === 2) throw new Error('refused')
    const b = stubBot(world)
    bots.push(b)
    return b
  }
  const { p, seen } = await online({ connect })
  assert.deepEqual(await p.offline('t1', { ms: 1000 }), { status: 'ok', ms: 1000 })
  assert.equal(calls, 3)
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'online'])
})

test('offline: a fresh bot that ends while its world loads is not adopted; the next try is', async () => {
  const bots = []
  const connect = async () => {
    const b = stubBot(world)
    if (bots.length === 1) { // its chunk never arrives and the connection ends meanwhile
      b.blockAt = () => null
      setTimeout(() => b.emit('end', 'kicked while loading'), 20)
    }
    bots.push(b)
    return b
  }
  const { p, seen } = await online({ connect })
  assert.deepEqual(await p.offline('t1', { ms: 1000 }), { status: 'ok', ms: 1000 })
  assert.deepEqual([bots.length, seen.map(e => e.kind)], [3, ['offline', 'online']])
  assert.equal(p.isOffline(), false)
  await p.close()
})

test('a fresh bot that is not bound yet survives a late socket error (write EPIPE) while its world loads and after it ended', async () => {
  const bots = []
  const connect = async () => {
    const b = stubBot(world)
    if (bots.length === 1) {
      b.blockAt = () => null
      setTimeout(() => b.emit('error', new Error('write EPIPE')), 10)
      setTimeout(() => b.emit('end', 'kicked while loading'), 20)
      setTimeout(() => b.emit('error', new Error('write EPIPE')), 40)
    }
    bots.push(b)
    return b
  }
  const { p, seen } = await online({ connect })
  assert.deepEqual(await p.offline('t1', { ms: 1000 }), { status: 'ok', ms: 1000 })
  await new Promise(resolve => setTimeout(resolve, 60))
  assert.equal(bots.length, 3)
  await p.close()
})

test('while offline lastKnown has what self read just before leaving; online it is null', async () => {
  let calls = 0
  const first = stubBot(world)
  const connect = async () => { calls += 1; if (calls > 1 && calls <= 5) throw new Error('refused'); return calls === 1 ? first : stubBot(world) }
  const { p, seen } = await online({ connect })
  assert.equal(p.lastKnown(), null)
  first.health = 11
  await assert.rejects(p.offline('t1', { ms: 1000 }), /refused/)
  assert.equal(p.isOffline(), true)
  assert.equal(p.lastKnown().health, 11)
  assert.deepEqual(p.lastKnown().pos, at(0, 64, 0))
  assert.deepEqual(Object.keys(p.lastKnown()).sort(), ['equipment', 'food', 'health', 'inventory', 'pos'])
  assert.deepEqual(p.self(), { status: 'offline' })
  await untilSeen(seen, 'online')
  assert.equal(p.lastKnown(), null)
})

test('a reconnect that keeps failing rejects, reports the body disconnected, then the body keeps trying by itself', async () => {
  let calls = 0
  const connect = async () => {
    calls += 1
    if (calls > 1 && calls <= 5) throw new Error('refused')
    return stubBot(world)
  }
  const { p, seen } = await online({ connect })
  await assert.rejects(p.offline('t1', { ms: 1000 }), /refused/)
  assert.deepEqual(seen.slice(0, 2).map(e => e.kind), ['offline', 'disconnected'])
  assert.equal(p.isOffline(), true)
  await untilSeen(seen, 'online')
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'disconnected', 'reconnect-failed', 'online'])
  assert.equal(p.isOffline(), false)
})

test('blockAt carries the age state of a crop, and only then', () => {
  const { p } = rig({ blocks: { '1,64,0': 'carrots', '2,64,0': 'dirt' }, props: { '1,64,0': { age: 7 } } })
  assert.equal(p.blockAt(at(1, 64, 0)).age, 7)
  assert.equal('age' in p.blockAt(at(2, 64, 0)), false)
})

// moveTo: goto.js resolves when the pathfinder reports noPath with an empty path, so a resolve proves nothing.
const noPathRig = (spec) => {
  const { bot, p } = rig({ ...world, ...spec })
  bot.pathfinder.goto = () => { bot.emit('path_update', { status: 'noPath', path: [] }); return Promise.resolve() }
  return p
}

test('moveTo: a goto that resolves on noPath with the body unmoved is blocked, not arrived', async () => {
  const result = await noPathRig({}).moveTo('t1', { pos: at(30, 64, 0) })
  assert.equal(result.status, 'blocked')
  assert.equal(result.reason, 'noPath')
  assert.deepEqual(result.pos, at(0, 64, 0))
})

// goto.js rejects with an Error named after the planner's verdict when the best-effort path is not empty.
const rejectNamed = (name, spec = {}) => {
  const { bot, p } = rig({ ...world, ...spec })
  bot.pathfinder.goto = () => Promise.reject(Object.assign(new Error(name), { name }))
  return p
}

test('moveTo: a goto rejected with NoPath, the body unmoved, is blocked noPath', async () => {
  const result = await rejectNamed('NoPath').moveTo('t1', { pos: at(30, 64, 0) })
  assert.equal(result.status, 'blocked')
  assert.equal(result.reason, 'noPath')
})

test('moveTo: a goto rejected with Timeout is blocked planTimeout', async () => {
  const result = await rejectNamed('Timeout').moveTo('t1', { pos: at(30, 64, 0) })
  assert.equal(result.status, 'blocked')
  assert.equal(result.reason, 'planTimeout')
})

test('moveTo: a goto rejected with another error is blocked with no reason', async () => {
  const result = await rejectNamed('Boom').moveTo('t1', { pos: at(30, 64, 0) })
  assert.equal(result.status, 'blocked')
  assert.equal('reason' in result, false)
})

test('collect: a goto rejected with NoPath is unreachable', async () => {
  const result = await rejectNamed('NoPath').collect('t1', { id: 7 })
  assert.equal(result.status, 'unreachable')
})

// A dropped item is picked up from about 1.4 blocks horizontally; a walk to within one cell of it can stop 1.5 away.
const dropRig = (itemAt, onGoto) => {
  const { bot, p } = rig({ ...world, pos: [0.5, 64, 0.5], entities: { 7: { id: 7, name: 'item', type: 'object', position: new Vec3(...itemAt), getDroppedItem: () => ({ name: 'stick', count: 1 }) } } })
  const goals = []
  bot.pathfinder.goto = goal => { goals.push(goal); onGoto(bot, goal, goals.length); return Promise.resolve() }
  return { bot, p, goals }
}
const stepTo = (bot, goal) => { bot.entity.position = new Vec3(goal.x + 0.5, 64, goal.z + 0.5) }
const pickUp = bot => { delete bot.entities[7]; bot.inventory.items().push({ name: 'stick', count: 1, slot: 38 }) }

test('collect: an item still 1.5 away after the walk is approached once more, to its own cell', async () => {
  const { bot, p, goals } = dropRig([1.99, 64, 0.5], (b, goal, n) => {
    if (n < 2) return
    stepTo(b, goal)
    pickUp(b)
  })
  const result = await p.collect('t1', { id: 7 })
  assert.equal(goals.length, 2)
  assert.equal(goals[1].rangeSq, 0)
  assert.equal(result.status, 'collected')
  assert.deepEqual(result.gained, [{ name: 'stick', count: 1 }])
})

test('collect: an item that slid away while the body waited is followed', async () => {
  const { bot, p, goals } = dropRig([2.9, 64, 0.5], (b, goal, n) => {
    stepTo(b, goal)
    if (n === 1) setTimeout(() => { b.entities[7].position = new Vec3(6.5, 64, 0.5) }, 5)
    if (n === 2) pickUp(b)
  })
  const result = await p.collect('t1', { id: 7 })
  assert.equal(goals[1].x, 6)
  assert.equal(result.status, 'collected')
})

test('collect: an item in reach that is never picked up ends unreachable with a reason', async () => {
  const { p } = dropRig([1.2, 64, 0.5], () => {})
  const result = await p.collect('t1', { id: 7 })
  assert.equal(result.status, 'unreachable')
  assert.equal(result.reason, 'not-picked-up')
})

test('collect: an item the body cannot get within reach of after the re-approaches ends unreachable out-of-reach', async () => {
  const { p, goals } = dropRig([4.5, 64, 0.5], () => {})
  const result = await p.collect('t1', { id: 7 })
  assert.equal(result.status, 'unreachable')
  assert.equal(result.reason, 'out-of-reach')
  assert.equal(goals.length, 3)
})

test('moveTo: a goto that resolves with the body moved but short of the goal is partial', async () => {
  const { bot, p } = rig(world)
  bot.pathfinder.goto = () => { bot.entity.position = new Vec3(10, 64, 0); return Promise.resolve() }
  const result = await p.moveTo('t1', { pos: at(30, 64, 0) })
  assert.equal(result.status, 'partial')
})

// A body on a block lower than a cube (farmland) is planned from the cell above, so that cell is where it stands.
const onFarmland = (onGround) => {
  const { bot, p } = rig({ ...world, blocks: { '0,100,0': 'farmland' }, pos: [0, 100.9375, 0] })
  bot.entity.onGround = onGround
  bot.pathfinder.goto = () => { bot.emit('path_update', { status: 'success', path: [] }); return Promise.resolve() }
  return p
}

test('moveTo: standing on farmland, on the ground, the cell above counts: arrived', async () => {
  assert.equal((await onFarmland(true).moveTo('t1', { pos: at(1, 101, 0) })).status, 'arrived')
})

test('moveTo: standing on farmland but airborne, the floored cell counts: not arrived', async () => {
  assert.notEqual((await onFarmland(false).moveTo('t1', { pos: at(1, 101, 0) })).status, 'arrived')
})

test('moveTo: a goto that resolves within range is arrived', async () => {
  const { bot, p } = rig(world)
  bot.pathfinder.goto = () => { bot.entity.position = new Vec3(29.5, 64, 0.5); return Promise.resolve() }
  assert.equal((await p.moveTo('t1', { pos: at(30, 64, 0) })).status, 'arrived')
})

// moveTo: a capped hop stays on the surface (its goal needs a column open to the sky), and a walk with no progress ends early.
const hopGoal = async blocks => {
  const { bot, p } = rig({ ...world, blocks: { ...world.blocks, ...blocks }, hang: ['goto'] })
  await p.moveTo('t1', { pos: at(200, 64, 0), timeoutS: 1 })
  return bot.calls.find(c => c.name === 'goto').args[0]
}

test('moveTo: the hop toward a far target is not ended by a node under a roof', async () => {
  assert.equal((await hopGoal({ '64,69,0': 'stone' })).isEnd(new Vec3(64, 64, 0)), false)
})

test('moveTo: the hop toward a far target ends at a node under a leaf canopy only', async () => {
  assert.equal((await hopGoal({ '64,69,0': 'oak_leaves' })).isEnd(new Vec3(64, 64, 0)), true)
})

test('moveTo: the hop toward a far target ends at a node in range under open sky', async () => {
  assert.equal((await hopGoal({})).isEnd(new Vec3(64, 64, 0)), true)
})

test('moveTo: the hop toward a far target is not ended by a node out of range', async () => {
  assert.equal((await hopGoal({})).isEnd(new Vec3(30, 64, 0)), false)
})

test('moveTo: the hop is not ended by a node with an unloaded cell above it', async () => {
  const { bot, p } = rig({ ...world, hang: ['goto'] })
  await p.moveTo('t1', { pos: at(200, 64, 0), timeoutS: 1 })
  const read = bot.blockAt
  bot.blockAt = v => v.y === 100 ? null : read(v)
  assert.equal(bot.calls.find(c => c.name === 'goto').args[0].isEnd(new Vec3(64, 64, 0)), false)
})

test('moveTo: a capped hop that finds no path from under a roof is retried toward the XZ point, marked hop xz', async () => {
  const { bot, p } = rig({ ...world, blocks: { ...world.blocks, '0,70,0': 'stone' } })
  bot.pathfinder.goto = goal => { bot.calls.push({ name: 'goto', args: [goal] }); return bot.calls.filter(c => c.name === 'goto').length === 1 ? Promise.reject(Object.assign(new Error('x'), { name: 'NoPath' })) : new Promise(() => {}) }
  const result = await p.moveTo('t1', { pos: at(200, 64, 0), timeoutS: 1 })
  const gotos = bot.calls.filter(c => c.name === 'goto')
  assert.equal(gotos.length, 2)
  assert.equal(gotos[1].args[0].constructor.name, 'GoalNearXZ')
  assert.equal(result.hop, 'xz')
})

test('moveTo: a capped hop that finds no path under open sky is not retried', async () => {
  const { bot, p } = rig(world)
  bot.pathfinder.goto = goal => { bot.calls.push({ name: 'goto', args: [goal] }); return Promise.reject(Object.assign(new Error('x'), { name: 'NoPath' })) }
  const result = await p.moveTo('t1', { pos: at(200, 64, 0), timeoutS: 1 })
  assert.equal(bot.calls.filter(c => c.name === 'goto').length, 1)
  assert.equal(result.status, 'blocked')
  assert.equal(result.reason, 'noPath')
  assert.equal('hop' in result, false)
})

// Timers and Date are virtual for the stall cases: the pump advances them while the call is pending, so machine load cannot stretch a measured time.
const virtually = async body => {
  mock.timers.enable({ apis: ['setTimeout', 'setInterval', 'Date'] })
  try {
    let settled = false
    const done = body().finally(() => { settled = true })
    done.catch(() => {})
    while (!settled) {
      await new Promise(resolve => setImmediate(resolve))
      mock.timers.tick(1)
    }
    return await done
  } finally {
    mock.timers.reset()
  }
}

// walks only count time from the pathfinder's first path_update of the goto
const update = bot => bot.emit('path_update', { status: 'success', path: [] })

test('moveTo: a body that does not move at all after a path_update ends stalled in about STILL_S, well before STALL_S', () => virtually(async () => {
  const { bot, p } = rig({ ...world, hang: ['goto'] })
  setTimeout(() => update(bot), 1)
  const started = Date.now()
  const result = await p.moveTo('t1', { pos: at(30, 64, 0), timeoutS: 60 })
  const took = Date.now() - started
  assert.equal(result.reason, 'stalled')
  assert.ok(took >= 3.5 * 1000 * SCALE && took < 6 * 1000 * SCALE, `took ${took}`)
}))

test('moveTo: a still body in water is not cut at STILL_S, only at STALL_S', () => virtually(async () => {
  const { bot, p } = rig({ ...world, hang: ['goto'] })
  bot.entity.isInWater = true
  setTimeout(() => update(bot), 1)
  const started = Date.now()
  const result = await p.moveTo('t1', { pos: at(30, 64, 0), timeoutS: 60 })
  assert.equal(result.reason, 'stalled')
  assert.ok(Date.now() - started >= 7.5 * 1000 * SCALE)
}))

test('moveTo: a still body on a ladder is not cut at STILL_S, only at STALL_S', () => virtually(async () => {
  const { bot, p } = rig({ ...world, blocks: { ...world.blocks, '0,64,0': 'ladder' }, hang: ['goto'] })
  setTimeout(() => update(bot), 1)
  const started = Date.now()
  const result = await p.moveTo('t1', { pos: at(30, 64, 0), timeoutS: 60 })
  assert.equal(result.reason, 'stalled')
  assert.ok(Date.now() - started >= 7.5 * 1000 * SCALE)
}))

test('moveTo: a still body is not cut before the first path_update, and the cut follows it', () => virtually(async () => {
  const { bot, p } = rig({ ...world, hang: ['goto'] })
  const started = Date.now()
  let cutAt = null
  setTimeout(() => update(bot), 20 * 1000 * SCALE)
  const result = await p.moveTo('t1', { pos: at(30, 64, 0), timeoutS: 60 }).then(r => { cutAt = Date.now() - started; return r })
  assert.equal(result.reason, 'stalled')
  assert.ok(cutAt >= 23.5 * 1000 * SCALE, `cut at ${cutAt}`)
}))

test('moveTo: a walk whose body never moves ends stalled well before its timeout', () => virtually(async () => {
  const { bot, p } = rig({ ...world, hang: ['goto'] })
  setTimeout(() => update(bot), 1)
  const started = Date.now()
  const result = await p.moveTo('t1', { pos: at(30, 64, 0), timeoutS: 60 })
  assert.equal(result.status, 'blocked')
  assert.equal(result.reason, 'stalled')
  assert.ok(Date.now() - started < 60 * 1000 * SCALE / 2)
}))

test('moveTo: a body that keeps moving is not cut by the stall rule and ends on the timeout', () => virtually(async () => {
  const { bot, p } = rig({ ...world, hang: ['goto'] })
  const mover = setInterval(() => { bot.entity.position = new Vec3(bot.entity.position.x + 0.1, 64, 0) }, 1).unref()
  const result = await p.moveTo('t1', { pos: at(60, 64, 0), timeoutS: 20 })
  clearInterval(mover)
  assert.equal(result.reason, 'timeout')
}))

test('moveTo: a body that moved three blocks and then stopped is partial, stalled', () => virtually(async () => {
  const { bot, p } = rig({ ...world, hang: ['goto'] })
  setTimeout(() => update(bot), 1)
  setTimeout(() => { bot.entity.position = new Vec3(3, 64, 0) }, 5)
  const result = await p.moveTo('t1', { pos: at(30, 64, 0), timeoutS: 60 })
  assert.equal(result.status, 'partial')
  assert.equal(result.reason, 'stalled')
}))

// swim
const waterAbove = { ...world, blocks: { ...world.blocks, '0,65,0': 'water', '0,64,0': 'water' }, oxygen: 4 }
const controls = bot => bot.calls.filter(c => c.name === 'setControlState').map(c => c.args)

test('swim: holds jump until the head is out of the water, then releases and reports surfaced with oxygen', async () => {
  const blocks = { '0,65,0': 'water', '0,64,0': 'water' }
  const { bot, p } = rig({ blocks, oxygen: 4 })
  setTimeout(() => { delete blocks['0,65,0']; bot.oxygenLevel = 20 }, 5)
  const result = await p.swim('t1', { ms: 3000 })
  assert.equal(result.status, 'surfaced')
  assert.deepEqual(result.oxygen, { before: 4, after: 20 })
  assert.deepEqual(controls(bot), [['jump', true], ['jump', false]])
})

test('swim: already surfaced returns at once without pressing jump', async () => {
  const { bot, p } = rig(world)
  assert.deepEqual(await p.swim('t1'), { status: 'surfaced', oxygen: { before: 20, after: 20 } })
  assert.deepEqual(controls(bot), [])
})

test('swim: a head that stays under water ends at the bound as timeout and releases jump', async () => {
  const { bot, p } = rig(waterAbove)
  const result = await p.swim('t1', { ms: 3000 })
  assert.equal(result.status, 'timeout')
  assert.deepEqual(result.oxygen, { before: 4, after: 4 })
  assert.deepEqual(controls(bot).at(-1), ['jump', false])
})

test('swim: ms is capped at 10 s', async () => {
  const { p } = rig(waterAbove)
  await p.swim('t1', { ms: 600000 })
})

test('swim: a cut releases jump', async () => {
  const { bot, p } = rig(waterAbove)
  const call = p.swim('t1', { ms: 3000 })
  await new Promise(r => setTimeout(r, 5))
  p.setOwner('t2')
  await assert.rejects(call, cutError)
  assert.deepEqual(controls(bot).at(-1), ['jump', false])
})

test('swim with bad ms rejects with bad-args', async () => {
  const { p } = rig(world)
  await assert.rejects(p.swim('t1', { ms: -1 }), err => err.code === 'bad-args')
})

// ---- unplanned disconnects ----

const connectOnce = (bots, failAfterFirst = false) => async () => {
  if (failAfterFirst && bots.length > 0) throw new Error('refused')
  const b = stubBot(world)
  bots.push(b)
  return b
}

test('an error on the bot is reported as a body event and never thrown', async () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  assert.doesNotThrow(() => bot.emit('error', new Error('boom')))
  assert.deepEqual(seen, [{ kind: 'error', reason: 'boom' }])
})

// polls until an event of `kind` arrived `count` times; the reconnect runs on its own, no call drives it
const untilSeen = async (seen, kind, count = 1) => {
  for (let i = 0; i < 2000 && seen.filter(e => e.kind === kind).length < count; i++) await new Promise(resolve => setTimeout(resolve, 1))
  return seen.filter(e => e.kind === kind)
}
const failing = (bots, fails) => async () => {
  if (bots.length > 0 && fails.n-- > 0) throw new Error('refused')
  const b = stubBot(world)
  bots.push(b)
  return b
}

for (const [event, reason] of [['end', 'socket closed'], ['kicked', 'bye']]) {
  test(`an idle body reconnects by itself after ${event}, with no action`, async () => {
    const bots = []
    const { p, seen } = await online({ connect: connectOnce(bots) })
    bots[0].emit(event, reason)
    await untilSeen(seen, 'online')
    assert.deepEqual(seen.map(e => e.kind), ['disconnected', 'online'])
    assert.equal(bots.length, 2)
    assert.equal(p.isOffline(), false)
    assert.equal(p.self().username, 'Stub')
  })
}

test('the reconnect backs off, doubling up to a minute, and keeps trying until it gets back', async () => {
  const bots = []
  const { p, seen } = await online({ connect: failing(bots, { n: 8 }) })
  bots[0].emit('kicked', 'bye')
  await untilSeen(seen, 'online')
  const failed = seen.filter(e => e.kind === 'reconnect-failed')
  assert.deepEqual(failed.map(e => [e.attempt, e.retryMs, e.reason]),
    [[1, 1000, 'refused'], [2, 2000, 'refused'], [3, 4000, 'refused'], [4, 8000, 'refused'], [5, 16000, 'refused'],
      [6, 32000, 'refused'], [7, 60000, 'refused'], [8, 60000, 'refused']])
  assert.equal(bots.length, 2)
  assert.equal(p.isOffline(), false)
})

test('close stops the reconnect loop', async () => {
  const bots = []
  const fails = { n: Infinity }
  const { p, seen } = await online({ connect: failing(bots, fails) })
  bots[0].emit('end', 'gone')
  await untilSeen(seen, 'reconnect-failed')
  await p.close()
  const after = seen.length
  await new Promise(resolve => setTimeout(resolve, 30))
  assert.equal(seen.length, after)
})

test('a deliberate offline keeps its own timer: the end of the quit connection does not bring the body back early', async () => {
  const bots = []
  const { p } = await online({ connect: connectOnce(bots), timeScale: 1 })
  const pending = p.offline('t1', { ms: 10 * MIN })
  bots[0].emit('end', 'quit')
  bots[0].emit('kicked', 'bye')
  await new Promise(resolve => setTimeout(resolve, 30))
  assert.equal(bots.length, 1)
  assert.equal(p.isOffline(), true)
  await p.close()
  assert.deepEqual(await pending, { status: 'closed' })
})

test('while the connection is down every primitive answers offline, never success, and nothing reaches the bot', async () => {
  const bots = []
  const { p } = await online({ connect: failing(bots, { n: Infinity }) })
  bots[0].emit('kicked', 'bye')
  assert.equal(p.isOffline(), true)
  assert.deepEqual(p.self(), { status: 'offline' })
  assert.deepEqual(p.entities({}), [])
  assert.equal(p.blockAt(at(2, 64, 0)), null)
  assert.deepEqual(p.drive('t1', { look: { yaw: 90 } }), { status: 'offline' })
  assert.deepEqual(await p.look('t1', { pos: at(1, 64, 1) }), { status: 'offline' })
  assert.deepEqual(await p.dig('t1', { pos: at(2, 64, 0) }), { status: 'offline' })
  assert.deepEqual(await p.offline('t1', { ms: 1000 }), { status: 'offline' })
  assert.deepEqual(acted(bots[0]), [])
  await p.close()
})

test('move-to never reports arrived while the connection is down, even at the target', async () => {
  const bots = []
  const { p } = await online({ connect: failing(bots, { n: Infinity }) })
  const here = { ...bots[0].entity.position }
  bots[0].emit('end', 'gone')
  assert.deepEqual(await p.moveTo('t1', { pos: here, range: 2 }), { status: 'offline' })
  await p.close()
})

test('the raw world has no eye from the end of the connection until the fresh bot is adopted', async () => {
  const bots = []
  const { p, seen } = await online({ connect: connectOnce(bots), timeScale: 1 })
  const before = p.rawWorld.eye()
  bots[0].emit('end', 'socket closed')
  const during = p.rawWorld.eye()
  await untilSeen(seen, 'online')
  assert.deepEqual([before !== null, during, p.rawWorld.eye() !== null], [true, null, true])
})

test('after the reconnect acting calls run on the new bot', async () => {
  const bots = []
  const { p, seen } = await online({ connect: connectOnce(bots), timeScale: 1 })
  bots[0].emit('end', 'socket closed')
  await untilSeen(seen, 'online')
  assert.deepEqual(await p.look('t1', { pos: at(1, 64, 1) }), { status: 'ok' })
  assert.equal(names(bots[1]).includes('lookAt'), true)
  assert.equal(names(bots[0]).includes('lookAt'), false)
})

test('a stale token still rejects with cut while the bot is down', async () => {
  const bots = []
test('a call in flight when the connection drops is cut at once, not left to its time bound', async () => {
  const bots = []
  const { p } = await online({ connect: failing(bots, { n: Infinity }), timeScale: 1 })
  const flying = p.steer('t1', { decide: () => ({ controls: {} }), timeoutS: 30 })
  await new Promise(resolve => setTimeout(resolve, 5))
  bots[0].emit('end', 'gone')
  await assert.rejects(flying, err => err.code === 'cut')
  await p.close()
})

test('a fresh bot kicked while its world loads is not adopted: the body keeps reconnecting', async () => {
  const bots = []
  const connect = async () => {
    const b = stubBot(world)
    if (bots.length === 1) b.blockAt = () => null // its chunk never arrives
    bots.push(b)
    return b
  }
  const { p, seen } = await online({ connect, timeScale: 1 })
  bots[0].emit('end', 'first')
  for (let i = 0; i < 1000 && bots.length < 2; i++) await new Promise(resolve => setTimeout(resolve, 1))
  await new Promise(resolve => setTimeout(resolve, 30)) // the world wait is under way
  bots[1].emit('end', 'second')
  await untilSeen(seen, 'online')
  assert.deepEqual([bots.length, seen.filter(e => e.kind === 'online').length], [3, 1])
  await p.close()
})

  const { p } = await online({ connect: connectOnce(bots, true) })
  bots[0].emit('end', 'gone')
  await assert.rejects(p.look('old', { pos: at(1, 64, 1) }), err => err.code === 'cut')
  await p.close()
})

test('without a connection to remake, a down body answers offline', async () => {
  const { bot, p } = rig(world)
  bot.emit('end', 'gone')
  assert.deepEqual(await p.look('t1', { pos: at(1, 64, 1) }), { status: 'offline' })
  assert.deepEqual(p.self(), { status: 'offline' })
})

// ---- mineflayer rejections are statuses ----

const rejecting = [
  { name: 'dig', args: { pos: at(2, 64, 0) }, call: 'dig', message: 'Digging aborted' },
  { name: 'place', args: { pos: at(1, 64, 0), item: 'cobblestone' }, call: 'placeBlock', message: 'No block has been placed' },
  { name: 'equip', args: { item: 'bread' }, call: 'equip', message: 'cannot equip' },
  { name: 'toss', args: { item: 'cobblestone' }, call: 'toss', message: 'cannot toss' },
  { name: 'eat', args: { item: 'bread' }, call: 'consume', message: 'Consuming cancelled due to calling bot.consume() again' },
  { name: 'attack', args: { id: 8 }, call: 'attack', message: 'invalid entity' },
  { name: 'look', args: { pos: at(1, 64, 1) }, call: 'lookAt', message: 'look failed' },
  { name: 'transfer', args: { pos: at(3, 64, 0), direction: 'deposit', item: 'cobblestone', count: 2 }, call: 'deposit', message: 'Server rejected transaction' },
  { name: 'sleep', args: { pos: at(2, 64, 1) }, call: 'sleep', message: 'it is not night', over: { entities: {} } },
  { name: 'inspectContainer', args: { pos: at(3, 64, 0) }, call: 'openContainer', message: 'chest is blocked' }
]
for (const c of rejecting) {
  test(`${c.name} resolves failed with the reason when mineflayer rejects`, async () => {
    const { p } = rig({ ...world, ...c.over, reject: { [c.call]: c.message } })
    assert.deepEqual(await p[c.name]('t1', c.args), { status: 'failed', reason: c.message })
  })
}

test('a rejection does not hide a cut: the token check still wins', async () => {
  const { p } = rig({ ...world, reject: { dig: 'Digging aborted' } })
  await assert.rejects(p.dig('old', { pos: at(2, 64, 0) }), cutError)
})

// ---- buckets ----

const bucketRig = ({ blocks, onActivate, item }) => {
  const all = { '0,63,0': 'stone', '1,63,0': 'stone', ...blocks } // the stub reads this object live, so onActivate edits the world
  return rig({ blocks: all, items: [{ name: item, count: 1, slot: 36 }], onActivate: () => onActivate(all) })
}

test('place with a water bucket looks at the support and activates the item instead of placing a block', async () => {
  const { bot, p } = bucketRig({ blocks: {}, item: 'water_bucket', onActivate: blocks => { blocks['1,64,0'] = 'water' } })
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' }), { status: 'placed', block: 'water' })
  assert.deepEqual(names(bot).filter(n => ['equip', 'lookAt', 'activateItem', 'placeBlock'].includes(n)), ['equip', 'lookAt', 'activateItem'])
})

test('a bucket that changes nothing resolves failed unchanged within the bound', async () => {
  const { p } = bucketRig({ blocks: {}, item: 'water_bucket', onActivate: () => {} })
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' }), { status: 'failed', reason: 'unchanged' })
})

test('an empty bucket scoops the water at the target cell', async () => {
  const { bot, p } = bucketRig({ blocks: { '1,64,0': 'water' }, item: 'bucket', onActivate: blocks => { delete blocks['1,64,0'] } })
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'bucket' }), { status: 'placed', block: 'bucket' })
  assert.equal(names(bot).includes('activateItem'), true)
})

test('scooping where there is no liquid is missing, pouring into a solid is occupied', async () => {
  const noop = () => {}
  assert.equal((await bucketRig({ blocks: {}, item: 'bucket', onActivate: noop }).p.place('t1', { pos: at(1, 64, 0), item: 'bucket' })).status, 'missing')
  assert.equal((await bucketRig({ blocks: { '1,64,0': 'stone' }, item: 'water_bucket', onActivate: noop }).p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' })).status, 'occupied')
})

test('a bucket not carried is no-item', async () => {
  const { p } = bucketRig({ blocks: {}, item: 'bread', onActivate: () => {} })
  assert.equal((await p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' })).status, 'no-item')
})

test('wait resolves ok after its time, scaled by timeScale', async () => {
  const { p } = rig(world)
  assert.deepEqual(await p.wait('t1', { ms: 1000 }), { status: 'ok' })
})

test('a cut during a wait rejects at once', async () => {
  const { p } = rig(world)
  const call = p.wait('t1', { ms: 10000 })
  p.setOwner('t2')
  await assert.rejects(call, err => err.code === 'cut')
})

test('a wait with a stale token rejects with cut', async () => {
  const { p } = rig(world)
  await assert.rejects(p.wait('old', { ms: 10 }), err => err.code === 'cut')
})

test('a wait is clamped to 10 s and to at least 0', async () => {
  const bot = stubBot(world)
  const p = createPrimitivesFromBot(bot, { timeScale: 0.0001 })
  p.setOwner('t1')
  assert.deepEqual(await p.wait('t1', { ms: 1e9 }), { status: 'ok' })
  assert.deepEqual(await p.wait('t1', { ms: -5 }), { status: 'ok' })
})

// ---- offline as body state ----

test('while offline isOffline is true and sensing answers offline instead of stale values', async () => {
  const { p } = await online()
  assert.equal(p.isOffline(), false)
  const pending = p.offline('t1', { ms: 10 * MIN })
  assert.equal(p.isOffline(), true)
  assert.deepEqual(p.self(), { status: 'offline' })
  assert.deepEqual(p.entities({}), [])
  assert.equal(p.blockAt(at(2, 64, 0)), null)
  p.setOwner('t2')
  await pending
  assert.equal(p.isOffline(), false)
  assert.equal(p.self().username, 'Stub')
})

test('after a cut the new owner acts on the reconnected bot, never on the quit one', async () => {
  const { p, bots } = await online()
  const pending = p.offline('t1', { ms: 10 * MIN })
  p.setOwner('t2')
  assert.equal(p.isOffline(), true)
  await p.look('t2', { yaw: 0, pitch: 0 })
  assert.deepEqual(await pending, { status: 'cut' })
  assert.deepEqual(acted(bots[0]), ['quit'])
  assert.deepEqual(acted(bots[1]), ['look'])
})

test('a call whose owner is cut while waiting for the reconnect rejects with cut', async () => {
  const { p, bots } = await online()
  const pending = p.offline('t1', { ms: 10 * MIN })
  p.setOwner('t2')
  const look = p.look('t2', { yaw: 0, pitch: 0 })
  p.setOwner('t3')
  await assert.rejects(look, err => err.code === 'cut')
  await pending
  assert.deepEqual(acted(bots[1]), [])
})

// ---- replaceable cells: the game replaces them, so place treats them as free ----

const replaceable = ['fire', 'soul_fire', 'short_grass', 'tall_grass', 'grass', 'snow', 'leaf_litter', 'fern', 'large_fern', 'dead_bush', 'vine', 'glow_lichen', 'hanging_roots']

for (const name of replaceable) {
  test(`place puts a block into a ${name} cell`, async () => {
    const { p } = rig({ blocks: { '1,63,0': 'stone', '1,64,0': name }, items: [{ name: 'cobblestone', count: 1, slot: 36 }] })
    const { status, block } = await p.place('t1', { pos: at(1, 64, 0), item: 'cobblestone' })
    assert.deepEqual({ status, block }, { status: 'placed', block: 'cobblestone' })
  })

  test(`a water bucket pours onto a ${name} cell`, async () => {
    const { p } = bucketRig({ blocks: { '1,64,0': name }, item: 'water_bucket', onActivate: blocks => { blocks['1,64,0'] = 'water' } })
    assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' }), { status: 'placed', block: 'water' })
  })
}

test('place into a cell of a solid block, or a plant that is not replaceable, stays occupied', async () => {
  for (const name of ['stone', 'oak_sapling']) {
test('occupied names the block that holds the cell', async () => {
  const { p } = rig({ blocks: { '1,63,0': 'stone', '1,64,0': 'oak_sapling' }, items: [{ name: 'cobblestone', count: 1, slot: 36 }] })
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'cobblestone' }), { status: 'occupied', block: 'oak_sapling' })
})

    const { p } = rig({ blocks: { '1,63,0': 'stone', '1,64,0': name }, items: [{ name: 'cobblestone', count: 1, slot: 36 }] })
    assert.equal((await p.place('t1', { pos: at(1, 64, 0), item: 'cobblestone' })).status, 'occupied')
  }
})

// ---- the pathfinder goal never outlives a walk ----

const goalCleared = bot => bot.calls.some(c => c.name === 'setGoal' && c.args[0] === null)

for (const [label, spec] of [['arrives', {}], ['is refused', { reject: { goto: 'No path' } }]]) {
  test(`moveTo clears the pathfinder goal when the walk ${label}`, async () => {
    const { bot, p } = rig({ ...world, ...spec })
    await p.moveTo('t1', { pos: at(3, 64, 0) })
    assert.equal(goalCleared(bot), true)
  })
}

test('moveTo clears the pathfinder goal on a timeout and on a cut', async () => {
  const timed = rig({ ...world, hang: ['goto'] })
  await timed.p.moveTo('t1', { pos: at(30, 64, 0), timeoutS: 1 })
  assert.equal(goalCleared(timed.bot), true)
  const cut = rig({ ...world, hang: ['goto'] })
  const pending = cut.p.moveTo('t1', { pos: at(30, 64, 0) })
  cut.p.setOwner('t2')
  await assert.rejects(pending, cutError)
  assert.equal(goalCleared(cut.bot), true)
})

for (const event of ['death', 'respawn']) {
  test(`a ${event} event clears the pathfinder goal and the controls`, () => {
    const { bot } = rig(world)
    bot.emit(event)
    assert.equal(goalCleared(bot), true)
    assert.ok(names(bot).includes('clearControlStates'))
  })
}

// swim toward a target: climb out onto a rim the pathfinder cannot reach

const rim = at(3, 65, 0)
const pool = () => ({ '0,63,0': 'stone', '0,64,0': 'water', '0,65,0': 'water' })

test('swim toward: looks at the target, holds jump and forward until the feet stand on solid ground, then releases both', async () => {
  const blocks = pool()
  const { bot, p } = rig({ blocks })
  setTimeout(() => { delete blocks['0,64,0'] }, 5)
  const result = await p.swim('t1', { ms: 3000, toward: rim })
  assert.equal(result.status, 'landed')
  assert.deepEqual(names(bot).filter(n => n === 'lookAt'), ['lookAt'])
  assert.deepEqual(controls(bot), [['jump', true], ['forward', true], ['jump', false], ['forward', false]])
})

test('swim toward: clear of the water within a block of the target but with no footing is not landed', async () => {
  const { bot, p } = rig({ blocks: pool() })
  setTimeout(() => { bot.entity.position = new Vec3(3.2, 65, 0.3) }, 5)
  assert.equal((await p.swim('t1', { ms: 3000, toward: rim })).status, 'timeout')
})

test('swim toward: already standing on solid ground lands at once without pressing anything', async () => {
  const { bot, p } = rig({ blocks: { '0,63,0': 'stone' } })
  assert.equal((await p.swim('t1', { ms: 3000, toward: rim })).status, 'landed')
  assert.deepEqual(controls(bot), [])
})

test('swim toward: stuck in the water it times out and releases both controls', async () => {
  const { bot, p } = rig({ blocks: pool() })
  assert.equal((await p.swim('t1', { ms: 3000, toward: rim })).status, 'timeout')
  assert.deepEqual(controls(bot).slice(-2), [['jump', false], ['forward', false]])
})

test('swim toward: a cut releases both controls', async () => {
  const { bot, p } = rig({ blocks: pool() })
  const call = p.swim('t1', { ms: 3000, toward: rim })
  await new Promise(resolve => setTimeout(resolve, 5))
  p.setOwner('t2')
  await assert.rejects(call, cutError)
  assert.deepEqual(controls(bot).slice(-2), [['jump', false], ['forward', false]])
})

test('swim toward with a bad target rejects with bad-args', async () => {
  await assert.rejects(rig(world).p.swim('t1', { toward: { x: 1 } }), err => err.code === 'bad-args')
})

// ---- a bucket use counts when the inventory changed even if the block update is late ----

test('a scoop succeeds when the held bucket became a water_bucket though the cell still reads water', async () => {
  const bucket = { name: 'bucket', count: 1, slot: 36 }
  const items = [bucket]
  const { p } = rig({ blocks: { '0,63,0': 'stone', '1,64,0': 'water' }, items, onActivate: () => { items.splice(0, 1, { name: 'water_bucket', count: 1, slot: 36 }) } })
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'bucket' }), { status: 'placed', block: 'bucket' })
})

test('a scoop succeeds when the cell turns to air shortly after the activation', async () => {
  const blocks = { '0,63,0': 'stone', '1,64,0': 'water' }
  const { p } = rig({ blocks, items: [{ name: 'bucket', count: 1, slot: 36 }], onActivate: () => { setTimeout(() => { delete blocks['1,64,0'] }, 5) } })
  assert.equal((await p.place('t1', { pos: at(1, 64, 0), item: 'bucket' })).status, 'placed')
})

test('a pour succeeds when the water_bucket became a bucket though the cell still reads air', async () => {
  const items = [{ name: 'water_bucket', count: 1, slot: 36 }]
  const { p } = rig({ blocks: { '0,63,0': 'stone', '1,63,0': 'stone' }, items, onActivate: () => { items.splice(0, 1, { name: 'bucket', count: 1, slot: 36 }) } })
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' }), { status: 'placed', block: 'water' })
})

test('a scoop where neither the cell nor the inventory changes still fails unchanged', async () => {
  const { p } = rig({ blocks: { '1,64,0': 'water' }, items: [{ name: 'bucket', count: 1, slot: 36 }], onActivate: () => {} })
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'bucket' }), { status: 'failed', reason: 'unchanged' })
})

// ---- view dump hooks ----

test('the view attaches to every bot, detaches while offline and on close, then stops', async () => {
  const calls = []
  const view = { attach: b => calls.push(['attach', b]), detach: () => calls.push(['detach']), stop: () => calls.push(['stop']) }
  const first = stubBot(world)
  const second = stubBot(world)
  const p = createPrimitivesFromBot(first, { timeScale: 0.0001, reconnect: async () => second, view })
  p.setOwner('t1')
  await p.offline('t1', { ms: 1000 })
  await p.close()
  assert.deepEqual(calls, [['attach', first], ['detach'], ['attach', second], ['detach'], ['stop']])
})

// Every bound body gets half-width 0.31 (mineflayer's 0.3 has the server reject every move of a body flush against a block face).

test('dig, place, jumpPlace and useOn mark their cell on the view once they settle, whatever the outcome', async () => {
  const marks = []
  const view = { attach: () => {}, detach: () => {}, stop: () => {}, markCell: (x, y, z) => marks.push([x, y, z]) }
  const bot = stubBot(world)
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE, view })
  p.setOwner('t1')
  await p.dig('t1', { pos: at(2, 64, 0) })
  await p.place('t1', { pos: at(1, 64, 0), item: 'cobblestone' }).catch(() => {})
  await p.jumpPlace('t1', { pos: at(1, 64, 0), item: 'cobblestone' }).catch(() => {})
  await p.useOn('t1', { pos: at(2, 64, 1) }).catch(() => {})
  await p.dig('t1', { pos: at(9, 64, 9) }).catch(() => {})
  assert.deepEqual(marks, [[2, 64, 0], [1, 64, 0], [1, 64, 0], [2, 64, 1], [9, 64, 9]])
  await p.close()
})

test('a bound bot has half-width 0.31, the initial one and a reconnected one', async () => {
  const first = stubBot(world)
  const second = stubBot(world)
  const p = createPrimitivesFromBot(first, { timeScale: SCALE, reconnect: async () => second })
  assert.equal(first.physics.playerHalfWidth, 0.31)
  p.setOwner('t1')
  await p.offline('t1', { ms: 1 })
  assert.equal(second.physics.playerHalfWidth, 0.31)
  await p.close()
})

test('a bound bot reads blocks through the server-shape wrapper, once per reconnect', async () => {
  const first = stubBot(world)
  first.registry = { ...first.registry, blocksByName: { bamboo: { id: 1 }, pointed_dripstone: { id: 2 } } }
  const original = first.blockAt
  createPrimitivesFromBot(first, { timeScale: SCALE })
  assert.notEqual(first.blockAt, original)
  assert.equal(first.blockAt(new Vec3(2, 64, 0)).name, 'oak_log')
})

test('swim toward leaves the half-width alone', async () => {
  const blocks = pool()
  const { bot, p } = rig({ blocks })
  setTimeout(() => { delete blocks['0,64,0'] }, 5)
  await p.swim('t1', { ms: 3000, toward: rim })
  assert.equal(bot.physics.playerHalfWidth, 0.31)
})

test('swim toward is not landed while still in the water next to the target', async () => {
  const { p } = rig({ blocks: { '1,63,0': 'stone', '1,64,0': 'water' }, pos: [1.2, 64, 0] })
  assert.equal((await p.swim('t1', { ms: 3000, toward: at(1, 64, 0) })).status, 'timeout')
})

// moveTo: a 1-deep hole. The pathfinder stalls against the ledge (path_reset 'stuck'); the body must centre, jump, then
// press forward. The tiny simulation: jump raises y 0.25 per tick up to start+1.25; forward moves x 0.2 per tick but only
// once y >= start+1 (the ledge is at x >= 1); the ground is 63 in the hole and 64 on the ledge.
const HOLE_SCALE = 0.05
const START_Y = 63
const holeRig = ({ pathNode = at(1.5, 64, 0.5), raises = true, startX = 0.5, resolveAfter = Infinity } = {}) => {
  const bot = stubBot({ pos: [startX, START_Y, 0.5] })
  const live = () => bot.entity.position
  const p = createPrimitivesFromBot(bot, { timeScale: HOLE_SCALE })
  p.setOwner('t1')
  const gotos = []
  const sequence = []
  bot.pathfinder.goto = () => {
    gotos.push(1)
    bot.emit('path_update', { status: 'success', path: [pathNode] })
    setTimeout(() => bot.emit('path_reset', 'stuck'), 0)
    return gotos.length > resolveAfter ? Promise.resolve() : new Promise(() => {})
  }
  const record = bot.setControlState
  const jumpAt = []
  bot.setControlState = (control, state) => { sequence.push(`${control}:${state}`); if (control === 'jump' && state) jumpAt.push(live().x); return record(control, state) }
  // a server correction (forcedMove) replaces the position object: do it once, as the step-up starts
  const setGoal = bot.pathfinder.setGoal
  let replaced = false
  bot.pathfinder.setGoal = goal => { if (!replaced) { replaced = true; bot.entity.position = new Vec3(live().x, live().y, live().z) } return setGoal(goal) }
  const simulate = setInterval(() => {
    const pos = live()
    const cs = bot.controlState
    const ground = pos.x >= 1 ? START_Y + 1 : START_Y
    if (cs.forward && (pos.y >= START_Y + 1 || pos.x >= 1 || !cs.jump)) pos.x += cs.sneak ? 0.03 : 0.2
    if (raises && cs.jump && pos.x < 1 && pos.y < START_Y + 1.25) pos.y += 0.25
    else if (pos.y > ground) pos.y = Math.max(ground, pos.y - 0.25)
    if (pos.x >= 1) pos.y = Math.max(pos.y, ground)
    bot.entity.onGround = Number.isInteger(pos.y)
    bot.emit('physicsTick')
  }, 1).unref()
  return { bot, p, gotos, sequence, jumpAt, stop: () => clearInterval(simulate) }
}
const jumpsHeld = seq => seq.filter(s => s === 'jump:true').length

test('moveTo: stalled in a 1-deep hole, the walk centres, jumps, then presses forward, and re-issues the goto to arrive', async () => {
  const { bot, p, gotos, sequence, stop } = holeRig({ startX: 0.3, resolveAfter: 1 })
  const result = await p.moveTo('t1', { pos: at(1, 64, 0), range: 0 })
  stop()
  assert.equal(result.status, 'arrived')
  assert.ok(sequence.indexOf('jump:true') < sequence.indexOf('forward:true', sequence.indexOf('jump:true')))
  assert.equal(gotos.length, 2)
  assert.deepEqual(bot.controlState, {})
})

test('moveTo: centring sneaks and stops within 0.1 of the cell centre, never flush against a wall, before the jump', async () => {
  const { p, sequence, jumpAt, stop } = holeRig({ startX: 0.3, resolveAfter: 1 })
  await p.moveTo('t1', { pos: at(1, 64, 0), range: 0 })
  stop()
  assert.ok(Math.abs(jumpAt[0] - 0.5) < 0.1)
  assert.ok(sequence.indexOf('sneak:true') < sequence.indexOf('jump:true'))
  assert.ok(sequence.indexOf('sneak:false') < sequence.indexOf('jump:true'))
})

test('moveTo: a path whose first node is two blocks up gets no step-up and ends blocked at the bound', async () => {
  const { p, sequence, stop } = holeRig({ pathNode: at(1.5, 65, 0.5) })
  const result = await p.moveTo('t1', { pos: at(1, 64, 0), range: 0 })
  stop()
  assert.equal(result.status, 'blocked')
  assert.equal(jumpsHeld(sequence), 0)
})

test('moveTo: a path whose first node is not adjacent gets no step-up', async () => {
  const { p, sequence, stop } = holeRig({ pathNode: at(2.5, 64, 0.5) })
  const result = await p.moveTo('t1', { pos: at(1, 64, 0), range: 0 })
  stop()
  assert.equal(result.status, 'blocked')
  assert.equal(jumpsHeld(sequence), 0)
})

test('moveTo: a step-up that never raises the body is tried at most twice', async () => {
  const { p, sequence, stop } = holeRig({ raises: false })
  const result = await p.moveTo('t1', { pos: at(1, 64, 0), range: 0, timeoutS: 40 })
  stop()
  assert.equal(result.status, 'blocked')
  assert.equal(jumpsHeld(sequence), 2)
})

test('moveTo: a cut during the step-up clears the control states and rejects with cut', async () => {
  const { bot, p, stop } = holeRig({ raises: false })
  const call = p.moveTo('t1', { pos: at(1, 64, 0), range: 0 })
  await new Promise(resolve => bot.on('physicsTick', () => bot.controlState.jump && resolve()))
  p.setOwner('t2')
  await assert.rejects(call, err => err.code === 'cut')
  stop()
  assert.deepEqual(bot.controlState, {})
})

test('moveTo: a goto that teleports the body into a pit away from the goal and resolves is not arrived', async () => {
  const { bot, p } = rig(world)
  bot.pathfinder.goto = () => { bot.entity.position = new Vec3(20.5, 54, 0.5); return Promise.resolve() }
  const result = await p.moveTo('t1', { pos: at(30, 64, 0) })
  assert.notEqual(result.status, 'arrived')
  assert.deepEqual(result.pos, at(20.5, 54, 0.5))
})

// ---- look resolves only once the server has the rotation (the next physics tick sends it) ----

// real-sized fallback (100 ms) so a tick the test emits by hand comes well before it
const slowRig = spec => {
  const bot = stubBot(spec)
  const p = createPrimitivesFromBot(bot, { timeScale: 1 })
  p.setOwner('t1')
  return { bot, p }
}

test('look resolves only after the bot emits a physics tick', async () => {
  const { bot, p } = slowRig(world)
  const events = []
  const call = p.look('t1', { yaw: 1, pitch: 0 }).then(() => events.push('resolved'))
  await sleep(20)
  assert.deepEqual(events, [])
  events.push('tick')
  bot.emit('physicsTick')
  await call
  assert.deepEqual(events, ['tick', 'resolved'])
})

test('look to a rotation already applied client-side but not yet sent still waits for a physics tick', async () => {
  const { bot, p } = slowRig(world)
  await bot.look(1, 0, true) // someone else's forced look, no tick since
  const events = []
  const call = p.look('t1', { yaw: 1, pitch: 0 }).then(() => events.push('resolved'))
  await sleep(20)
  assert.deepEqual(events, [])
  bot.emit('physicsTick')
  await call
  assert.deepEqual(events, ['resolved'])
})

test('look resolves after the fallback when no physics tick comes', async () => {
  const { p } = rig(world)
  assert.deepEqual(await p.look('t1', { pos: at(1, 64, 1) }), { status: 'ok' })
})

test('a bucket is used only after a physics tick has sent the look at the support', async () => {
  const all = { '0,63,0': 'stone', '1,63,0': 'stone' }
  const { bot, p } = slowRig({ blocks: all, items: [{ name: 'water_bucket', count: 1, slot: 36 }], onActivate: () => { all['1,64,0'] = 'water' } })
  const events = []
  const looked = new Promise(resolve => {
    const lookAt = bot.lookAt
    bot.lookAt = (...args) => { resolve(); return lookAt(...args) }
  })
  bot.on('physicsTick', () => events.push('tick'))
  const activate = bot.activateItem
  bot.activateItem = (...args) => { events.push('activate'); return activate(...args) }
  const result = p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' })
  await looked
  await sleep(20)
  assert.deepEqual(events, [])
  bot.emit('physicsTick')
  await result
  assert.deepEqual(events, ['tick', 'activate'])
})

// ---- jumpPlace: pillar up by jumping and placing the block under the feet ----

// A stub body that rises when jump goes on, lets placeBlock fill the cell it left, and lands one block higher when
// jump goes off: the three things the real physics and server do. `rise` false leaves the body on the ground.
const pillarRig = ({ blocks = {}, items = [{ name: 'dirt', count: 3, slot: 36 }], rise = true, hang = [], pos = [0.5, 64, 0.5], pinned = false } = {}) => {
  const all = { '0,63,0': 'stone', ...blocks }
  const { bot, p } = rig({ blocks: all, items, hang, pos })
  const startOf = () => Math.floor(bot.entity.position.y)
  let start = null
  const press = bot.setControlState
  bot.setControlState = (control, on) => {
    const result = press(control, on)
    if (control === 'forward' && on && !pinned) bot.entity.position = new Vec3(Math.floor(bot.entity.position.x) + 0.5, bot.entity.position.y, Math.floor(bot.entity.position.z) + 0.5)
    if (control !== 'jump') return result
    if (on && rise) {
      start = startOf()
      bot.entity.onGround = false
      setTimeout(() => { bot.entity.position = new Vec3(bot.entity.position.x, start + 1.1, bot.entity.position.z) }, 2)
    }
    if (!on && start !== null) {
      setTimeout(() => { bot.entity.position = new Vec3(bot.entity.position.x, start + 1, bot.entity.position.z); bot.entity.onGround = true }, 2)
    }
    return result
  }
  const place = bot._placeBlockWithOptions
  bot._placeBlockWithOptions = (ref, face, options) => {
    all[`${ref.position.x + face.x},${ref.position.y + face.y},${ref.position.z + face.z}`] = items[0].name
    items[0].count -= 1
    if (items[0].count === 0) items.shift()
    return place(ref, face, options)
  }
  return { bot, p, all, items }
}

test('jumpPlace raises the body one block per repetition: look down, jump, place under the feet, release, land', async t => {
  mockClock(t)
  const { bot, p, all, items } = pillarRig()
  assert.deepEqual(await driveClock(t, p.jumpPlace('t1', { item: 'dirt', count: 2 })), { status: 'done', placed: 2 })
  assert.equal(bot.entity.position.y, 66)
  assert.deepEqual([all['0,64,0'], all['0,65,0']], ['dirt', 'dirt'])
  assert.equal(items[0].count, 1)
  const placeCalls = bot.calls.filter(c => c.name === '_placeBlockWithOptions').map(c => [c.args[0].position.y, c.args[1].y])
  assert.deepEqual(placeCalls, [[63, 1], [64, 1]], 'against the block under the start cell, on its top face')
  assert.deepEqual(bot.calls.filter(c => c.name === 'look').map(c => c.args[1]), [-Math.PI / 2, -Math.PI / 2])
  assert.deepEqual(controls(bot), [['jump', true], ['jump', false], ['jump', true], ['jump', false]])
})

test('jumpPlace centres an off-centre body in its cell, sneaking, before it jumps', () => virtually(async () => {
  const { bot, p } = pillarRig({ pos: [0.35, 64, 0.5] })
  const ticker = setInterval(() => bot.emit('physicsTick'), 2)
  try {
    assert.deepEqual(await p.jumpPlace('t1', { item: 'dirt' }), { status: 'done', placed: 1 })
  } finally {
    clearInterval(ticker)
  }
  const order = controls(bot).map(c => c.join(':'))
  assert.ok(order.indexOf('sneak:true') >= 0 && order.indexOf('sneak:true') < order.indexOf('jump:true'), `controls: ${order}`)
  assert.ok(order.indexOf('sneak:false') < order.indexOf('jump:true'), 'sneak released before the jump')
}))

test('jumpPlace places with forceLook ignore, never through placeBlock (its unforced look delays the packet ~1 s)', () => virtually(async () => {
  const { bot, p } = pillarRig()
  await p.jumpPlace('t1', { item: 'dirt', count: 2 })
  assert.deepEqual(bot.calls.filter(c => c.name === '_placeBlockWithOptions').map(c => c.args[2]), [{ swingArm: 'right', forceLook: 'ignore' }, { swingArm: 'right', forceLook: 'ignore' }])
  assert.deepEqual(names(bot).filter(n => n === 'placeBlock'), [])
}))

test('jumpPlace ignores a failure to centre: a body that cannot be centred still jumps and places', () => virtually(async () => {
  const { bot, p } = pillarRig({ pos: [0.35, 64, 0.5], pinned: true })
  const ticker = setInterval(() => bot.emit('physicsTick'), 2)
  try {
    assert.deepEqual(await p.jumpPlace('t1', { item: 'dirt' }), { status: 'done', placed: 1 })
  } finally {
    clearInterval(ticker)
  }
  const order = controls(bot).map(c => c.join(':'))
  assert.equal(order.includes('jump:true'), true)
  assert.equal(order.lastIndexOf('forward:false') > order.lastIndexOf('forward:true'), true)
}))

test('jumpPlace ends not-raised when the body cannot be centred and cannot rise', () => virtually(async () => {
  const { bot, p } = pillarRig({ pos: [0.35, 64, 0.5], pinned: true, rise: false })
  const ticker = setInterval(() => bot.emit('physicsTick'), 2)
  try {
    assert.deepEqual(await p.jumpPlace('t1', { item: 'dirt' }), { status: 'failed', placed: 0, reason: 'not-raised' })
  } finally {
    clearInterval(ticker)
  }
}))

test('jumpPlace waits for a body inside the tolerance to stop moving before it jumps', async () => {
  const { bot, p } = pillarRig({ pos: [0.5, 64, 0.5] })
  bot.entity.velocity = new Vec3(-0.014, 0, 0)
  setTimeout(() => { bot.entity.velocity = new Vec3(0, 0, 0) }, 5)
  const startedAt = Date.now()
  await p.jumpPlace('t1', { item: 'dirt' })
  const jumpedAt = bot.calls.findIndex(c => c.name === 'setControlState' && c.args[0] === 'jump' && c.args[1])
  assert.ok(jumpedAt > bot.calls.findIndex(c => c.name === 'setControlState' && c.args[0] === 'sneak' && c.args[1]))
  assert.ok(Date.now() - startedAt >= 5)
})

test('jumpPlace does not move a body that is already centred', async () => {
  const { bot, p } = pillarRig({ pos: [0.5, 64, 0.5] })
  await p.jumpPlace('t1', { item: 'dirt' })
  assert.deepEqual(controls(bot).filter(c => c[0] !== 'jump'), [])
})

test('jumpPlace without the item fails at once, without pressing anything', async () => {
  const { bot, p } = pillarRig({ items: [] })
  assert.deepEqual(await p.jumpPlace('t1', { item: 'dirt' }), { status: 'failed', placed: 0, reason: 'no-item' })
  assert.deepEqual(controls(bot), [])
})

test('jumpPlace runs out of items part way and reports partial', () => virtually(async () => {
  const { p } = pillarRig({ items: [{ name: 'dirt', count: 2, slot: 36 }] })
  assert.deepEqual(await p.jumpPlace('t1', { item: 'dirt', count: 4 }), { status: 'partial', placed: 2, reason: 'no-item' })
}))

test('jumpPlace refuses a pit whose ceiling is within two blocks of the feet, and a start with nothing solid under it', async () => {
  const low = pillarRig({ blocks: { '0,66,0': 'stone' } })
  assert.deepEqual(await low.p.jumpPlace('t1', { item: 'dirt' }), { status: 'failed', placed: 0, reason: 'no-headroom' })
  assert.deepEqual(controls(low.bot), [])
  const air = pillarRig({ blocks: { '0,63,0': undefined } })
  assert.deepEqual(await air.p.jumpPlace('t1', { item: 'dirt' }), { status: 'failed', placed: 0, reason: 'no-support' })
})

test('jumpPlace that never leaves the ground reports not-raised and releases jump', async () => {
  const { bot, p, all } = pillarRig({ rise: false })
  assert.deepEqual(await p.jumpPlace('t1', { item: 'dirt' }), { status: 'failed', placed: 0, reason: 'not-raised' })
  assert.deepEqual(controls(bot), [['jump', true], ['jump', false]])
  assert.equal(all['0,64,0'], undefined)
})

test('jumpPlace reports place-failed when the server refuses the block, and releases jump', async () => {
  const { bot, p } = pillarRig()
  bot._placeBlockWithOptions = () => Promise.reject(new Error('blockUpdate did not fire'))
  const result = await p.jumpPlace('t1', { item: 'dirt' })
  assert.deepEqual([result.status, result.placed, result.reason], ['failed', 0, 'place-failed: blockUpdate did not fire'])
  assert.deepEqual(controls(bot).at(-1), ['jump', false])
})

test('jumpPlace: a cut mid-jump releases jump and rejects with cut', async () => {
  const { bot, p } = pillarRig({ hang: ['_placeBlockWithOptions'] })
  const call = p.jumpPlace('t1', { item: 'dirt' })
  await new Promise(resolve => setTimeout(resolve, 15))
  p.setOwner('t2')
  await assert.rejects(call, cutError)
  assert.deepEqual(controls(bot).at(-1), ['jump', false])
})

test('jumpPlace caps count at 8 and rejects bad arguments', async () => {
  const { p } = pillarRig({ items: [{ name: 'dirt', count: 20, slot: 36 }] })
  const result = await p.jumpPlace('t1', { item: 'dirt', count: 100 })
  assert.deepEqual([result.status, result.placed], ['done', 8])
  for (const bad of [{}, { item: 5 }, { item: 'dirt', count: 0 }, { item: 'dirt', count: 1.5 }]) {
    await assert.rejects(p.jumpPlace('t1', bad), err => err.code === 'bad-args')
  }
})

test('jumpPlace with a stale token rejects with cut before touching the bot', async () => {
  const { bot, p } = pillarRig()
  await assert.rejects(p.jumpPlace('old', { item: 'dirt' }), cutError)
  assert.deepEqual(controls(bot), [])
})

// ---- toss: throw carried items in the direction the body looks ----

const tossCalls = bot => bot.calls.filter(c => c.name === 'toss').map(c => c.args)
const twoStacks = { ...world, items: [{ name: 'cobblestone', count: 40, slot: 36 }, { name: 'cobblestone', count: 30, slot: 37 }, { name: 'bread', count: 2, slot: 38 }] }

test('toss without count throws every carried stack of the item, across slots', async () => {
  const { bot, p } = rig(twoStacks)
  assert.deepEqual(await p.toss('t1', { item: 'cobblestone' }), { status: 'tossed', count: 70 })
  assert.deepEqual(tossCalls(bot), [[2, null, 70]])
})

test('toss clamps count to what is carried and honours a smaller count', async () => {
  const big = rig(twoStacks)
  assert.deepEqual(await big.p.toss('t1', { item: 'cobblestone', count: 500 }), { status: 'tossed', count: 70 })
  const small = rig(twoStacks)
  assert.deepEqual(await small.p.toss('t1', { item: 'bread', count: 1 }), { status: 'tossed', count: 1 })
  assert.deepEqual(tossCalls(small.bot), [[1, null, 1]])
})

test('toss of an item not carried never reaches the bot', async () => {
  const { bot, p } = rig(world)
  assert.deepEqual(await p.toss('t1', { item: 'diamond' }), { status: 'no-item', count: 0 })
  assert.deepEqual(names(bot), [])
})

test('toss does not look anywhere itself', async () => {
  const { bot, p } = rig(world)
  await p.toss('t1', { item: 'bread' })
  assert.deepEqual(names(bot).filter(n => n === 'lookAt' || n === 'look'), [])
})

test('toss with watchS reports who picked the drop up, by uuid, and ends once all of it is taken', async () => {
  const { bot, p } = rig(world)
  const done = p.toss('t1', { item: 'bread', count: 2, watchS: 1.5 })
  setTimeout(() => bot.emit('playerCollect', { uuid: 'v1' }, { getDroppedItem: () => ({ name: 'bread', count: 2 }) }), 5)
  assert.deepEqual(await done, { status: 'tossed', count: 2, takenBy: { v1: 2 } })
})

test('toss with watchS counts only the item tossed and gives an empty receipt when nobody takes it', async () => {
  const { bot, p } = rig(world, 0.01)
  const done = p.toss('t1', { item: 'bread', count: 1, watchS: 0.5 })
  setTimeout(() => bot.emit('playerCollect', { uuid: 'v1' }, { getDroppedItem: () => ({ name: 'stick', count: 1 }) }), 1)
  assert.deepEqual(await done, { status: 'tossed', count: 1, takenBy: {} })
})

test('toss with watchS waits past the 2 s pickup delay: a pickup at 2.5 s is reported, and watchS is capped at 5', async () => {
  const { bot, p } = rig(world, 0.01)
  const done = p.toss('t1', { item: 'bread', count: 1, watchS: 4 })
  setTimeout(() => bot.emit('playerCollect', { uuid: 'v1' }, { getDroppedItem: () => ({ name: 'bread', count: 1 }) }), 25)
  assert.deepEqual(await done, { status: 'tossed', count: 1, takenBy: { v1: 1 } })
  const late = rig(world, 0.01)
  const t0 = Date.now()
  await late.p.toss('t1', { item: 'bread', count: 1, watchS: 60 })
  assert.ok(Date.now() - t0 < 500, 'watchS 60 ends at the 5 s cap (50 ms at this time scale)')
})

test('toss without watchS has no receipt', async () => {
  const { p } = rig(world)
  assert.deepEqual(await p.toss('t1', { item: 'bread', count: 1 }), { status: 'tossed', count: 1 })
})

// ---- the picked-up body event ----

const pickup = (bot, collector, dropped) => bot.emit('playerCollect', collector, { getDroppedItem: () => dropped })

test('picked-up: the body collecting an item entity is reported with the item name and count', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  pickup(bot, bot.entity, { name: 'stick', count: 3 })
  assert.deepEqual(seen, [{ kind: 'picked-up', item: 'stick', count: 3 }])
})

test('picked-up: another collector, or an entity whose item cannot be read, is not reported', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  pickup(bot, { id: 5 }, { name: 'stick', count: 3 })
  pickup(bot, bot.entity, null)
  bot.emit('playerCollect', bot.entity, {})
  assert.deepEqual(seen, [])
})

test('picked-up: unsubscribing and a replaced bot go quiet', () => {
  const { bot, p } = rig(world)
  const seen = []
  const off = p.onBodyEvent(e => seen.push(e))
  off()
  pickup(bot, bot.entity, { name: 'stick', count: 3 })
  assert.deepEqual(seen, [])
})

test('toss with a slot throws exactly that stack whole, whatever the count says', async () => {
  const { bot, p } = rig(twoStacks)
  assert.deepEqual(await p.toss('t1', { item: 'cobblestone', slot: 37, count: 5 }), { status: 'tossed', count: 30 })
  assert.deepEqual(bot.calls.filter(c => c.name === 'tossStack').map(c => c.args[0].slot), [37])
  assert.deepEqual(tossCalls(bot), [])
})

test('toss with a slot that is empty or holds another item is no-item and never reaches the bot', async () => {
  const { bot, p } = rig(twoStacks)
  assert.deepEqual(await p.toss('t1', { item: 'cobblestone', slot: 38 }), { status: 'no-item', count: 0 })
  assert.deepEqual(await p.toss('t1', { item: 'cobblestone', slot: 5 }), { status: 'no-item', count: 0 })
  assert.deepEqual(names(bot), [])
})

test('toss with a non-numeric slot rejects with bad-args', async () => {
  const { p } = rig(twoStacks)
  await assert.rejects(p.toss('t1', { item: 'cobblestone', slot: 'x' }), err => err.code === 'bad-args')
})

// ---- waiting for the world after spawn ----

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))
const WORLD_OPTS = { host: 'h', port: 1, username: 'u' }

test('createPrimitives does not return until the column under the body is loaded', async () => {
  const bot = stubBot({ ...world, unloaded: true })
  let done = false
  const pending = createPrimitives(WORLD_OPTS, { connect: async () => bot, timeScale: SCALE, worldTimeoutMs: 5000 }).then(p => { done = true; return p })
  await sleep(150)
  assert.equal(done, false)
  bot.loadWorld()
  const p = await pending
  assert.equal(done, true)
  assert.equal(p.blockAt({ x: 2, y: 64, z: 0 }).name, 'oak_log')
})

test('createPrimitives: a socket error while the world loads does not throw on the bot, and a kick fails the start', async () => {
  const bot = stubBot({ ...world, unloaded: true })
  const pending = createPrimitives(WORLD_OPTS, { connect: async () => bot, timeScale: SCALE, worldTimeoutMs: 5000 })
  const result = assert.rejects(pending, /connection dropped while the world loaded/)
  await sleep(50)
  assert.doesNotThrow(() => bot.emit('error', new Error('write EPIPE')))
  bot.emit('kicked', 'bye')
  await result
})

test('createPrimitives: the world-wait error guard is gone once the body is bound (only its own handler stays)', async () => {
  const bot = stubBot(world)
  const before = bot.listenerCount('error')
  await createPrimitives(WORLD_OPTS, { connect: async () => bot, timeScale: SCALE, worldTimeoutMs: 5000 })
  assert.equal(bot.listenerCount('error'), before + 1)
})

test('createPrimitives refuses to start, before it connects, when a required dependency patch is missing', async () => {
  let connected = false
  const connect = async () => { connected = true; return stubBot(world) }
  await assert.rejects(createPrimitives(WORLD_OPTS, { connect, readFile: () => null }), /trapdoor over a ladder.*patch-deps/s)
  assert.equal(connected, false)
})

test('createPrimitives starts with the repo as it is: the required patches are applied', async () => {
  const p = await createPrimitives(WORLD_OPTS, { connect: async () => stubBot(world), timeScale: 0.0001, worldTimeoutMs: 5000 })
  assert.ok(p)
})

test('a reconnected bot is not adopted until its world is loaded', async () => {
  const bots = []
  const connect = async () => { const b = stubBot({ ...world, unloaded: bots.length > 0 }); bots.push(b); return b }
  const p = await createPrimitives(WORLD_OPTS, { connect, timeScale: 0.0001, worldTimeoutMs: 5000 })
  p.setOwner('t1')
  const pending = p.offline('t1', { ms: 1 })
  await sleep(150)
  assert.equal(bots.length, 2)
  assert.equal(p.isOffline(), true)
  bots[1].loadWorld()
  assert.deepEqual(await pending, { status: 'ok', ms: 1 })
  assert.equal(p.isOffline(), false)
})

test('a world that never loads resolves anyway and emits world-not-loaded to listeners added later', async () => {
  const bot = stubBot({ ...world, unloaded: true })
  const p = await createPrimitives(WORLD_OPTS, { connect: async () => bot, timeScale: SCALE, worldTimeoutMs: 100 })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  assert.deepEqual(seen, [{ kind: 'world-not-loaded', ms: 100 }])
})

test('a reconnect into a world that never loads adopts anyway and emits world-not-loaded before online', async () => {
  const bots = []
  const connect = async () => { const b = stubBot({ ...world, unloaded: bots.length > 0 }); bots.push(b); return b }
  const p = await createPrimitives(WORLD_OPTS, { connect, timeScale: 0.0001, worldTimeoutMs: 100 })
  p.setOwner('t1')
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  await p.offline('t1', { ms: 1 })
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'world-not-loaded', 'online'])
  assert.equal(seen[1].ms, 100)
})

test('self reports chunkLoaded from the column under the body', () => {
  const { bot, p } = rig({ unloaded: true })
  assert.equal(p.self().chunkLoaded, false)
  bot.loadWorld()
  assert.equal(p.self().chunkLoaded, true)
})

// Physics-stall watchdog: mineflayer emits no physicsTick while the column under the body is unloaded.
const stallRig = (spec = {}) => {
  const bot = stubBot(spec)
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  return { bot, p, seen }
}
const STALL_WAIT = 80 // well past 2 s * SCALE and a few watchdog periods
const stalls = seen => seen.filter(e => e.kind === 'physics-stalled')

test('an unloaded column with no physics tick emits physics-stalled once, clears the goal and releases the controls', async () => {
  const { bot, p, seen } = stallRig({ unloaded: true })
  await sleep(STALL_WAIT * 2)
  assert.equal(stalls(seen).length, 1)
  assert.deepEqual(stalls(seen)[0].pos, { x: 0, y: 64, z: 0 })
  assert.ok(stalls(seen)[0].ms >= 2000 * SCALE)
  assert.deepEqual(bot.calls.filter(c => c.name === 'setGoal').map(c => c.args), [[null]])
  assert.equal(names(bot).filter(n => n === 'clearControlStates').length, 1)
  await p.close()
})

for (const [label, ticking, unloaded] of [['a loaded column without ticks', false, false], ['ticks arriving over an unloaded column', true, true]]) {
  test(`${label} emits nothing`, async () => {
    const { bot, p, seen } = stallRig({ unloaded })
    const timer = setInterval(() => bot.emit('physicsTick'), ticking ? 2 : 1e6)
    await sleep(STALL_WAIT)
    clearInterval(timer)
    assert.equal(stalls(seen).length, 0)
    await p.close()
  })
}

test('a physics tick re-arms the watchdog: a second stall emits a second event', async () => {
  const { bot, p, seen } = stallRig({ unloaded: true })
  await sleep(STALL_WAIT)
  bot.emit('physicsTick')
  await sleep(STALL_WAIT)
  assert.equal(stalls(seen).length, 2)
  await p.close()
})

test('a reconnected bot starts with a fresh last-tick time', async () => {
  const first = stubBot({})
  const second = stubBot({})
  const p = createPrimitivesFromBot(first, { timeScale: SCALE, reconnect: async () => second })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  p.setOwner('t1')
  await sleep(STALL_WAIT)
  await p.offline('t1', { ms: 1 })
  second.unloadWorld()
  await sleep(5)
  assert.equal(stalls(seen).length, 0)
  await sleep(STALL_WAIT)
  assert.equal(stalls(seen).length, 1)
  await p.close()
})

test('close stops the watchdog', async () => {
  const { p, seen } = stallRig({ unloaded: true })
  await p.close()
  await sleep(STALL_WAIT * 2)
  assert.equal(stalls(seen).length, 0)
})

// ---- settling ----

const SETTLE = 60 // ms, timeScale 1
const settleRig = (spec = {}) => {
  const bot = stubBot({ ...world, ...spec })
  const p = createPrimitivesFromBot(bot, { settleMs: SETTLE })
  return { bot, p }
}
const settled = () => sleep(SETTLE + 40)

test('a freshly bound body is settling until settleMs have passed', async () => {
  const { p } = settleRig()
  assert.equal(p.isSettling(), true)
  await settled()
  assert.equal(p.isSettling(), false)
})

test('self().settling mirrors isSettling', async () => {
  const { p } = settleRig()
  assert.equal(p.self().settling, true)
  await settled()
  assert.equal(p.self().settling, false)
})

test('settleMs scales with timeScale', async () => {
  const p = createPrimitivesFromBot(stubBot(world), { timeScale: 0.001, settleMs: 100000 })
  await sleep(150)
  assert.equal(p.isSettling(), false)
})

test('a reconnect settles again, and offline itself is not settling', async () => {
  const bots = []
  const connect = async () => { const b = stubBot(world); bots.push(b); return b }
  const p = await createPrimitives(WORLD_OPTS, { connect, timeScale: 0.0001, settleMs: 1e6 })
  p.setOwner('t1')
  await sleep(150)
  assert.equal(p.isSettling(), false)
  const pending = p.offline('t1', { ms: 1 })
  assert.equal(p.isSettling(), false)
  await pending
  assert.equal(p.isSettling(), true)
  await sleep(150)
  assert.equal(p.isSettling(), false)
})

test('a respawn settles again once the spawn arrives', async () => {
  const { bot, p } = settleRig()
  await settled()
  bot.emit('respawn')
  assert.equal(p.isSettling(), false)
  bot.emit('spawn')
  assert.equal(p.isSettling(), true)
  await settled()
  assert.equal(p.isSettling(), false)
})

test('an unloaded column is settling, and its loading starts a fresh period', async () => {
  const { bot, p } = settleRig({ unloaded: true })
  await settled()
  assert.equal(p.isSettling(), true)
  bot.loadWorld()
  assert.equal(p.isSettling(), true)
  await settled()
  assert.equal(p.isSettling(), false)
  bot.unloadWorld()
  assert.equal(p.isSettling(), true)
  bot.loadWorld()
  assert.equal(p.isSettling(), true)
})

for (const [label, to, settling] of [['a teleport', at(100, 64, 0), true], ['a server correction', at(1, 64, 0), false], ['exactly 16 blocks', at(16, 64, 0), false]]) {
  test(`forcedMove: ${label} ${settling ? 'restarts' : 'does not restart'} settling`, async () => {
    const { bot, p } = settleRig()
    await settled()
    bot.entity.position = new Vec3(to.x, to.y, to.z)
    bot.emit('forcedMove')
    assert.equal(p.isSettling(), settling)
  })
}

test('forcedMove measures from the last position the body was known at', async () => {
  const { bot, p } = settleRig()
  await settled()
  for (const x of [10, 20, 30]) {
    bot.entity.position = new Vec3(x, 64, 0)
    bot.emit('forcedMove')
  }
  assert.equal(p.isSettling(), false)
})

// ---- drive (manual takeover) ----

const near = (a, b) => Math.abs(a - b) < 1e-9

for (const [label, mc, mf] of [
  ['south', { yaw: 0, pitch: 0 }, { yaw: Math.PI, pitch: 0 }],
  ['west, looking down', { yaw: 90, pitch: 90 }, { yaw: Math.PI / 2, pitch: -Math.PI / 2 }],
  ['north, looking up', { yaw: 180, pitch: -90 }, { yaw: 0, pitch: Math.PI / 2 }]
]) {
  test(`look conversion: ${label}`, () => {
    const to = mcToMineflayerLook(mc)
    assert.ok(near(to.yaw, mf.yaw) && near(to.pitch, mf.pitch))
    const back = mineflayerToMcLook(mf)
    assert.ok(near(back.yaw, mc.yaw) && near(back.pitch, mc.pitch))
  })
}

for (const [label, mc, want] of [
  ['pitch clamps high', { yaw: 10, pitch: 120 }, { yaw: 10, pitch: 90 }],
  ['pitch clamps low', { yaw: 10, pitch: -120 }, { yaw: 10, pitch: -90 }],
  ['yaw wraps over 360', { yaw: 370, pitch: 0 }, { yaw: 10, pitch: 0 }],
  ['yaw wraps below 0', { yaw: -90, pitch: 0 }, { yaw: 270, pitch: 0 }]
]) {
  test(`look conversion round trip normalises: ${label}`, () => {
    const back = mineflayerToMcLook(mcToMineflayerLook(mc))
    assert.ok(near(back.yaw, want.yaw) && near(back.pitch, want.pitch))
  })
}

test('drive with the owner token sets control states and looks, and returns pos and degrees', () => {
  const { bot, p } = rig({})
  const res = p.drive('t1', { controls: { forward: true, sprint: true }, look: { yaw: 90, pitch: 10 } })
  assert.deepEqual(bot.controlState, { forward: true, sprint: true })
  assert.ok(near(bot.entity.yaw, Math.PI / 2) && near(bot.entity.pitch, -10 * Math.PI / 180))
  assert.ok(near(res.yaw, 90) && near(res.pitch, 10))
  assert.deepEqual(res.pos, p.self().pos)
})

test('drive returns yaw and pitch rounded to 2 decimals, a look of 0 0 exactly 0', () => {
  const { p } = rig({})
  const res = p.drive('t1', { look: { yaw: 0, pitch: 0 } })
  assert.equal(res.yaw, 0)
  assert.equal(res.pitch, 0)
  const odd = p.drive('t1', { look: { yaw: 123.456789, pitch: -12.3456 } })
  assert.equal(odd.yaw, 123.46)
  assert.equal(odd.pitch, -12.35)
})

test('drive with a relative look adds to the current degrees', () => {
  const { p } = rig({})
  p.drive('t1', { look: { yaw: 350, pitch: 80 } })
  const res = p.drive('t1', { look: { dyaw: 20, dpitch: 30 } })
  assert.ok(near(res.yaw, 10) && near(res.pitch, 90))
})

test('drive keeps the current value for a missing look key', () => {
  const { p } = rig({})
  p.drive('t1', { look: { yaw: 45, pitch: 20 } })
  const res = p.drive('t1', { look: { pitch: -5 } })
  assert.ok(near(res.yaw, 45) && near(res.pitch, -5))
})

test('drive with a stale token throws cut and sets nothing', () => {
  const { bot, p } = rig({})
  assert.throws(() => p.drive('old', { controls: { forward: true }, look: { yaw: 90, pitch: 0 } }), { code: 'cut' })
  assert.deepEqual(bot.controlState, {})
  assert.ok(!names(bot).includes('look'))
})

for (const next of ['t2', null]) {
  test(`setOwner(${next}) after drive clears the control states`, () => {
    const { bot, p } = rig({})
    p.drive('t1', { controls: { forward: true } })
    p.drive('t1', { controls: { jump: true } })
    p.setOwner(next)
    assert.deepEqual(bot.controlState, {})
    assert.equal(names(bot).filter(n => n === 'clearControlStates').length, 1)
  })
}

test('stopDriving clears the control states without a token', () => {
  const { bot, p } = rig({})
  p.drive('t1', { controls: { forward: true } })
  p.stopDriving()
  assert.deepEqual(bot.controlState, {})
})

// Sleeping bodies: mineflayer's wake() sends a wrong id here, so leave_bed is sent by name.
const leaveBeds = bot => bot.calls.filter(c => c.name === 'write' && c.args[1].actionId === 'leave_bed')

test('a cut during sleep writes leave_bed, not just wake', async () => {
  const { bot, p } = rig({ blocks: world.blocks, timeOfDay: 15000, hang: ['sleep'] })
  const call = p.sleep('t1', { pos: at(2, 64, 1) })
  call.catch(() => {})
  await sleep(20)
  p.setOwner('t2')
  await assert.rejects(call, /cut/)
  assert.equal(leaveBeds(bot).length, 1)
})

test('moveTo on a sleeping body leaves the bed first, then succeeds', async () => {
  const { bot, p } = rig({ sleeping: true })
  const r = await p.moveTo('t1', { pos: at(1, 64, 0) })
  assert.notEqual(r.status, 'disconnected')
  assert.equal(leaveBeds(bot).length, 1)
  assert.equal(bot.isSleeping, false)
  assert.ok(acted(bot).indexOf('write') < acted(bot).indexOf('goto'))
})

test('dig on a sleeping body leaves the bed first, then digs', async () => {
  const { bot, p } = rig({ sleeping: true, blocks: world.blocks })
  await p.dig('t1', { pos: at(2, 64, 0) })
  assert.equal(leaveBeds(bot).length, 1)
  assert.ok(acted(bot).indexOf('write') < acted(bot).indexOf('dig'))
})

test('a body that never wakes within the bound: the act proceeds and fails as the stub fails', async () => {
  const { bot, p } = rig({ sleeping: true, blocks: world.blocks })
  bot._client.write = (name, data) => { bot.calls.push({ name: 'write', args: [name, data] }) }
  const r = await p.dig('t1', { pos: at(2, 64, 0) }).then(v => v, e => e)
  assert.equal(leaveBeds(bot).length, 1)
  assert.ok(acted(bot).includes('dig'))
  assert.deepEqual(r, { status: 'failed', reason: 'asleep' })
})

test('drive on a sleeping body writes leave_bed', () => {
  const { bot, p } = rig({ sleeping: true })
  p.drive('t1', { controls: { forward: true } })
  assert.equal(leaveBeds(bot).length, 1)
})

test('craft with bad args rejects with bad-args', async () => {
  const { p } = rig()
  const bad = [{}, { item: '' }, { item: 'stick', count: 0 }, { item: 'stick', count: 1.5 }, { item: 'stick', table: { x: 1 } }]
  for (const args of bad) await assert.rejects(p.craft('t1', args), err => err.code === 'bad-args', JSON.stringify(args))
})

test('craft and chat with a stale token reject with cut', async () => {
  const { p } = rig()
  await assert.rejects(p.craft('old', { item: 'stick' }), cutError)
  await assert.rejects(p.chat('old', { message: 'hi' }), cutError)
})

test('craft through the wrapper crafts and chat through the wrapper sends', async () => {
  const items = [{ name: 'oak_log', count: 1, type: 7, slot: 36 }]
  // chat listens 10 ms inside a 30 ms bound at SCALE: a tenth-scale rig keeps that margin on a loaded machine
  const { bot, p } = rig({ items }, 0.1)
  const reg = { 7: { id: 7, name: 'oak_log' }, 3: { id: 3, name: 'oak_planks', stackSize: 64 } }
  const r = { requiresTable: false, result: { id: 3, count: 4 }, delta: [{ id: 7, count: -1 }, { id: 3, count: 4 }] }
  Object.assign(bot, {
    registry: { items: reg, itemsByName: { oak_planks: reg[3] } },
    recipesAll: () => [r],
    recipesFor: () => [r],
    craft: async () => { items[0].count = 0; items.push({ name: 'oak_planks', count: 4, type: 3, slot: 37 }) },
    chat: () => {},
    players: {}
  })
  bot.inventory.emptySlotCount = () => 10
  assert.deepEqual(await p.craft('t1', { item: 'oak_planks' }), { status: 'crafted', item: 'oak_planks', made: 4, used: { oak_log: 1 } })
  const sent = []
  bot.chat = m => sent.push(m)
  assert.deepEqual(await p.chat('t1', { message: 'hello' }), { status: 'sent', parts: 1 })
  assert.deepEqual(sent, ['hello'])
})

test('chat through the wrapper fails on what engine.chat would have refused and sends nothing', async () => {
  const { bot, p } = rig()
  const sent = []
  Object.assign(bot, { chat: m => sent.push(m), whisper: (...a) => sent.push(a), players: { Steve: {} } })
  for (const args of [{ message: '/op me' }, { message: '/op me', to: 'Steve' }, { message: 'a\n/op me' }, {}, { message: undefined, to: 'Steve' }]) {
    const r = await p.chat('t1', args)
    assert.equal(r.status, 'failed', JSON.stringify(args))
    assert.match(r.reason, /refusing/, JSON.stringify(args))
  }
  assert.deepEqual(sent, [])
})

test('chat through the wrapper resolves gone for names not in the player list, prototype names included', async () => {
  const { bot, p } = rig()
  const sent = []
  Object.assign(bot, { chat: m => sent.push(m), whisper: (...a) => sent.push(a), players: { Steve: {} } })
  for (const to of ['@a', 'Name extra', 'ab', 'constructor']) assert.deepEqual(await p.chat('t1', { message: 'hi', to }), { status: 'gone', to }, to)
  assert.deepEqual(sent, [])
})

test('body events: chat from the body itself is not reported, chat from others is', () => {
  const { bot, p } = rig()
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.emit('chat', 'Stub', 'my own line')
  bot.emit('chat', 'Ann', 'hi')
  assert.deepEqual(seen, [{ kind: 'chat', from: 'Ann', message: 'hi' }])
})

// transfer waits for the window's slot updates to stop before it closes the window, and measures what moved
const SETTLE_SCALE = 0.1 // quiet 15 ms, cap 150 ms, bound 500 ms
const transferRig = ({ onClick, moveCap, items = [] }) => {
  const bot = stubBot({ items, containers: { '3,64,0': [{ name: 'bread', count: 32, slot: 0 }] }, blocks: { '3,64,0': 'chest' }, onClick, moveCap })
  const p = createPrimitivesFromBot(bot, { timeScale: SETTLE_SCALE })
  p.setOwner('t1')
  const closedAt = []
  const close = bot.closeWindow
  bot.closeWindow = (...args) => { closedAt.push(Date.now()); return close(...args) }
  return { bot, p, closedAt }
}
const withdraw12 = { pos: at(3, 64, 0), direction: 'withdraw', item: 'bread', count: 12 }
const emitEvery = (win, ms, times, stamps) => {
  for (let i = 1; i <= times; i++) setTimeout(() => { stamps.push(Date.now()); win.emit('updateSlot', 0) }, ms * i)
}

test('transfer does not close the window until the slot updates stop', () => virtually(async () => {
  const stamps = []
  const { p, closedAt } = transferRig({ onClick: win => emitEvery(win, 5, 4, stamps) })
  const result = await p.transfer('t1', withdraw12)
  assert.equal(stamps.length, 4)
  assert.equal(closedAt.length, 2)
  assert.ok(closedAt[0] - stamps.at(-1) >= 12, `closed ${closedAt[0] - stamps.at(-1)} ms after the last update`)
  assert.deepEqual(result, { status: 'ok', moved: 12 })
}))

test('transfer measures moved from the container: a click that moves 5 of 12 reports 5', async () => {
  const { p } = transferRig({ onClick: () => {}, moveCap: 5 })
  assert.deepEqual(await p.transfer('t1', withdraw12), { status: 'ok', moved: 5 })
})

test('transfer measures a deposit from the container too', async () => {
  const { p } = transferRig({ onClick: () => {}, moveCap: 3, items: [{ name: 'bread', count: 10, slot: 36 }] })
  assert.deepEqual(await p.transfer('t1', { ...withdraw12, direction: 'deposit' }), { status: 'ok', moved: 3 })
})

test('transfer closes at the cap when updates never stop, within the 5 s bound', () => virtually(async () => {
  const { p, closedAt } = transferRig({ onClick: win => { const t = setInterval(() => win.emit('updateSlot', 0), 5); setTimeout(() => clearInterval(t), 400) } })
  const start = Date.now()
  const result = await p.transfer('t1', withdraw12)
  assert.equal(result.status, 'ok')
  assert.equal(closedAt.length, 2)
  assert.ok(closedAt[0] - start >= 140 && closedAt[0] - start < 400, `closed at ${closedAt[0] - start} ms`)
}))

test('a cut during the settle wait rejects with cut and still closes the window', async () => {
  const { p, bot } = transferRig({ onClick: win => { const t = setInterval(() => win.emit('updateSlot', 0), 5); setTimeout(() => clearInterval(t), 400) } })
  const call = p.transfer('t1', withdraw12)
  await new Promise(resolve => setTimeout(resolve, 40))
  p.setOwner('t2')
  await assert.rejects(call, cutError)
  assert.ok(names(bot).includes('closeWindow'))
})

const openCount = bot => names(bot).filter(n => n === 'openContainer').length
const staleView = win => { win.containerItems = () => [{ name: 'bread', count: 32, slot: 0 }] }

test('transfer opens the container twice when it clicked, and once when there was nothing to move', async () => {
  const { p, bot } = transferRig({ onClick: () => {} })
  await p.transfer('t1', withdraw12)
  assert.equal(openCount(bot), 2)
  await p.transfer('t1', { ...withdraw12, item: 'cobblestone' })
  assert.equal(openCount(bot), 3)
})

test('transfer measures moved from the reopened window, not the one that was clicked', async () => {
  const { p } = transferRig({ onClick: staleView })
  assert.deepEqual(await p.transfer('t1', withdraw12), { status: 'ok', moved: 12 })
})

test('a cut during the reopen rejects with cut and closes the second window', async () => {
  const { p, bot } = transferRig({ onClick: () => {} })
  const open = bot.openContainer
  let opened = 0
  bot.openContainer = (...args) => { opened++; if (opened === 2) p.setOwner('t2'); return open(...args) }
  await assert.rejects(p.transfer('t1', withdraw12), cutError)
  assert.ok(names(bot).filter(n => n === 'closeWindow').length >= 2)
  assert.equal(openCount(bot), 2)
})

test('furnace reads a furnace within reach and reports a refusal as data when it is not', async () => {
  const { p } = rig({ blocks: { '0,64,2': 'furnace', '0,64,3': 'furnace', '9,64,0': 'furnace', '1,64,0': 'dirt' } })
  assert.deepEqual(await p.furnace('t1', { pos: at(0, 64, 2), op: 'read' }), { status: 'ok', kind: 'furnace', input: null, fuel: null, output: null, lit: false, burn: null, cook: null })
  assert.deepEqual(await p.furnace('t1', { pos: at(9, 64, 0), op: 'read' }), { status: 'unreachable', reason: 'too-far', distance: 9.58 })
  assert.deepEqual(await p.furnace('t1', { pos: at(1, 64, 0), op: 'read' }), { status: 'cannot', reason: 'not-a-furnace', block: 'dirt' })
  assert.deepEqual(await p.furnace('t1', { pos: at(5, 64, 5), op: 'read' }), { status: 'missing', block: 'air' })
})

test('blockAt marks a block fullCube only when its collision shape fills the cell', () => {
  const { p } = rig({ blocks: { '1,64,0': 'stone', '2,64,0': 'farmland', '3,64,0': 'wheat', '4,64,0': 'oak_slab' },
    shapes: { '2,64,0': [[0, 0, 0, 1, 0.9375, 1]], '3,64,0': [], '4,64,0': [[0, 0, 0, 1, 0.5, 1]] } })
  assert.equal(p.blockAt(at(1, 64, 0)).fullCube, true)
  for (const x of [2, 3, 4]) assert.equal('fullCube' in p.blockAt(at(x, 64, 0)), false)
  assert.equal('fullCube' in p.blockAt(at(9, 64, 0)), false)
})

test('enchant refuses a block that is not a table and one out of reach as data, without opening a window', async () => {
  const { bot, p } = rig({ blocks: { '1,64,2': 'enchanting_table', '9,64,0': 'enchanting_table', '1,64,0': 'dirt' }, items: [{ name: 'bread', count: 1, slot: 36 }] })
  assert.deepEqual(await p.enchant('t1', { pos: at(1, 64, 0), op: 'offers', item: 'bread' }), { status: 'cannot', reason: 'not-a-table' })
  assert.deepEqual(await p.enchant('t1', { pos: at(9, 64, 0), op: 'offers', item: 'bread' }), { status: 'unreachable', reason: 'too-far', distance: 9.58 })
  assert.deepEqual(await p.enchant('t1', { pos: at(5, 64, 5), op: 'offers', item: 'bread' }), { status: 'missing' })
  assert.deepEqual(await p.enchant('t1', { pos: at(1, 64, 2), op: 'offers', item: 'cake' }), { status: 'no-item', item: 'cake' })
  assert.ok(!names(bot).includes('openEnchantmentTable'))
})

const iron = (slot, durabilityUsed = 0) => ({ name: 'iron_helmet', count: 1, slot, durabilityUsed })
const noGear = { head: null, torso: null, legs: null, feet: null, offHand: null, mainHand: null }

test('self().equipment lists the armour slots, the off-hand and the main hand, durability left where the item has one', () => {
  const worn = [{ name: 'iron_helmet', count: 1, slot: 5, durabilityUsed: 15 }, { name: 'iron_boots', count: 1, slot: 8 }, { name: 'shield', count: 1, slot: 45, durabilityUsed: 36 }, { name: 'totem_of_undying', count: 1, slot: 6 }]
  const p = withBot(bot => { bot.heldItem = { name: 'bread', count: 3, slot: 36 } }, { ...world, worn })
  assert.deepEqual(p.self().equipment, {
    head: { name: 'iron_helmet', count: 1, durability: 150 },
    torso: { name: 'totem_of_undying', count: 1 },
    legs: null,
    feet: { name: 'iron_boots', count: 1, durability: 195 },
    offHand: { name: 'shield', count: 1, durability: 300 },
    mainHand: { name: 'bread', count: 3 }
  })
})

test('self().equipment carries the enchantments of a worn piece as [{name, level}] and omits them on plain gear', () => {
  const worn = [{ name: 'iron_chestplate', count: 1, slot: 6, enchants: [{ name: 'protection', lvl: 3 }, { name: 'unbreaking', lvl: 1 }] }, { name: 'iron_helmet', count: 1, slot: 5, enchants: [] }]
  const eq = withBot(() => {}, { ...world, worn }).self().equipment
  assert.deepEqual(eq.torso, { name: 'iron_chestplate', count: 1, enchants: [{ name: 'protection', level: 3 }, { name: 'unbreaking', level: 1 }] })
  assert.deepEqual(eq.head, { name: 'iron_helmet', count: 1, durability: 165 })
})

test('self().equipment is empty slots when nothing is worn, and worn items are not in the carried list', () => {
  const bare = withBot(() => {}, { ...world, items: [] })
  assert.deepEqual(bare.self().equipment, noGear)
  const dressed = withBot(() => {}, { ...world, items: [], worn: [iron(5)] })
  assert.deepEqual(dressed.self().inventory, [])
  assert.equal(dressed.self().equipment.head.name, 'iron_helmet')
})


test('blockAt marks a block fullCube only when its collision shape fills the cell', () => {
  const { p } = rig({ blocks: { '1,64,0': 'stone', '2,64,0': 'farmland', '3,64,0': 'wheat', '4,64,0': 'oak_slab' },
    shapes: { '2,64,0': [[0, 0, 0, 1, 0.9375, 1]], '3,64,0': [], '4,64,0': [[0, 0, 0, 1, 0.5, 1]] } })
  assert.equal(p.blockAt(at(1, 64, 0)).fullCube, true)
  for (const x of [2, 3, 4]) assert.equal('fullCube' in p.blockAt(at(x, 64, 0)), false)
  assert.equal('fullCube' in p.blockAt(at(9, 64, 0)), false)
})
// ---- place with a click: the face, cursor, look and sneak the caller chose, then the block read back ----

const clickRig = (extra = {}) => {
  const blocks = { '2,64,0': 'stone', '1,63,0': 'stone', ...extra.blocks }
  const props = {}
  const spec = { blocks, props, items: [{ name: 'oak_stairs', count: 2, slot: 36 }], ...extra.spec }
  const bot = stubBot(spec)
  const place = bot._placeBlockWithOptions
  bot._placeBlockWithOptions = (ref, face, options) => {
    const p = ref.position.plus(face)
    blocks[`${p.x},${p.y},${p.z}`] = 'oak_stairs'
    props[`${p.x},${p.y},${p.z}`] = { facing: 'east', half: 'top' }
    return place(ref, face, options)
  }
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  p.setOwner('t1')
  return { bot, p }
}
const stairClick = { against: at(2, 64, 0), cursor: at(0, 0.75, 0.5), yaw: 1.5 * Math.PI, pitch: 0, sneak: true }

test('place with a click sneaks, holds the look, clicks that face at that cursor without looking again, and reads the block back', async () => {
  const { bot, p } = clickRig()
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'oak_stairs', click: stairClick }),
    { status: 'placed', block: 'oak_stairs', placed: { name: 'oak_stairs', properties: { facing: 'east', half: 'top' } } })
  const steps = bot.calls.filter(c => ['setControlState', 'look', 'lookAt', '_placeBlockWithOptions'].includes(c.name))
  assert.deepEqual(steps.map(c => c.name), ['setControlState', 'look', '_placeBlockWithOptions', 'setControlState'])
  assert.deepEqual(steps[0].args, ['sneak', true])
  assert.deepEqual(steps[1].args, [1.5 * Math.PI, 0, true])
  const [ref, face, options] = steps[2].args
  assert.deepEqual([ref.position.x, ref.position.y, ref.position.z, face.x, face.y, face.z], [2, 64, 0, -1, 0, 0])
  assert.deepEqual([options.forceLook, options.delta.x, options.delta.y, options.delta.z], ['ignore', 0, 0.75, 0.5])
  assert.deepEqual(steps[3].args, ['sneak', false])
})

test('place with a click and no look looks at the cursor point; with only a pitch it keeps the yaw', async () => {
  const { bot, p } = clickRig()
  await p.place('t1', { pos: at(1, 64, 0), item: 'oak_stairs', click: { against: at(2, 64, 0), cursor: at(0, 0.5, 0.5) } })
  const look = bot.calls.find(c => c.name === 'lookAt')
  assert.deepEqual([look.args[0].x, look.args[0].y, look.args[0].z, look.args[1]], [2, 64.5, 0.5, true])
  assert.ok(!bot.calls.some(c => c.name === 'setControlState'))
  const down = clickRig()
  down.bot.entity.yaw = 0.25
  await down.p.place('t1', { pos: at(1, 64, 0), item: 'oak_stairs', click: { against: at(1, 63, 0), cursor: at(0.5, 1, 0.5), pitch: -Math.PI / 2 } })
  assert.deepEqual(down.bot.calls.find(c => c.name === 'look').args, [0.25, -Math.PI / 2, true])
})

test('place with a click on air or a fluid has no support and equips nothing', async () => {
  for (const name of [undefined, 'water']) {
    const { bot, p } = clickRig({ blocks: name ? { '1,65,0': name } : {} })
    assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'oak_stairs', click: { against: at(1, 65, 0), cursor: at(0.5, 0, 0.5) } }), { status: 'no-support' })
    assert.ok(!names(bot).includes('equip'))
  }
})

test('place refuses a click that is not on a neighbour or has a cursor off the block', async () => {
  for (const click of [{ against: at(3, 64, 0), cursor: at(0, 0.5, 0.5) }, { against: at(2, 65, 0), cursor: at(0, 0.5, 0.5) }, { against: at(2, 64, 0), cursor: at(0, 1.5, 0.5) }, { against: at(2, 64, 0) }]) {
    const { p } = clickRig()
    await assert.rejects(p.place('t1', { pos: at(1, 64, 0), item: 'oak_stairs', click }), err => err.code === 'bad-args')
  }
})

test('a cut while a sneaking click is out releases sneak', async () => {
  const { bot, p } = clickRig({ spec: { hang: ['_placeBlockWithOptions'] } })
  const call = p.place('t1', { pos: at(1, 64, 0), item: 'oak_stairs', click: stairClick })
  await new Promise(resolve => setTimeout(resolve, 20))
  p.setOwner('t2')
  await assert.rejects(call, err => err.code === 'cut')
  assert.equal(bot.controlState.sneak, false)
})

test('a plain place reads the block back too', async () => {
  const blocks = { '1,63,0': 'stone' }
  const props = {}
  const bot = stubBot({ blocks, props, items: [{ name: 'oak_log', count: 1, slot: 36 }] })
  const place = bot.placeBlock
  bot.placeBlock = (ref, face) => { blocks['1,64,0'] = 'oak_log'; props['1,64,0'] = { axis: 'y' }; return place(ref, face) }
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  p.setOwner('t1')
  assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'oak_log' }), { status: 'placed', block: 'oak_log', placed: { name: 'oak_log', properties: { axis: 'y' } } })
})

// ---- place against an interactable block: a click on a door, chest or table opens it instead of placing ----

// the reference block and the sneak state at each placeBlock, read off the call log
const placeTrace = bot => bot.calls.filter(c => ['placeBlock', 'setControlState'].includes(c.name))
  .map(c => c.name === 'placeBlock' ? ['place', c.args[0].name] : [c.args[0], c.args[1]])

const interactables = ['oak_door', 'iron_door', 'oak_trapdoor', 'oak_fence_gate', 'chest', 'trapped_chest', 'ender_chest', 'barrel', 'crafting_table', 'furnace', 'blast_furnace', 'smoker', 'anvil', 'red_bed', 'stone_button', 'lever', 'note_block', 'enchanting_table', 'brewing_stand', 'hopper', 'dispenser', 'dropper', 'shulker_box', 'red_shulker_box']

for (const name of interactables) {
  test(`place with only a ${name} to click sneaks for the click and releases after`, async () => {
    const { bot, p } = rig({ blocks: { '5,69,5': name }, items: [{ name: 'cobblestone', count: 2, slot: 36 }], pos: [5.5, 70, 5.5] })
    assert.equal((await p.place('t1', { pos: at(5, 70, 5), item: 'cobblestone' })).status, 'placed')
    assert.deepEqual(placeTrace(bot), [['sneak', true], ['place', name], ['sneak', false]])
  })

  test(`place prefers a plain wall to a ${name}, without sneaking`, async () => {
    const { bot, p } = rig({ blocks: { '5,69,5': name, '6,70,5': 'stone' }, items: [{ name: 'cobblestone', count: 2, slot: 36 }], pos: [5.5, 70, 5.5] })
    assert.equal((await p.place('t1', { pos: at(5, 70, 5), item: 'cobblestone' })).status, 'placed')
    assert.deepEqual(placeTrace(bot), [['place', 'stone']])
  })
}


// dig reports the drops the dig produced, not a pile that already lay there
const pile = { name: 'item', type: 'object', position: at(2, 64, 0), getDroppedItem: () => ({ name: 'raw_iron', count: 1 }) }
const fresh = { name: 'item', type: 'object', position: at(2, 64, 0), getDroppedItem: () => ({ name: 'cobblestone', count: 1 }) }

const digWithDrop = async (entities, spawn) => {
  const { bot, p } = rig({ ...world, blocks: { '2,64,0': 'stone' }, entities })
  const original = bot.dig
  bot.dig = async (...args) => {
    const r = await original(...args)
    for (const [id, e] of Object.entries(spawn)) bot.entities[id] = e
    return r
  }
  return p.dig('t1', { pos: at(2, 64, 0) })
}

test('dig lists only the drops that appeared with it, not items already lying there', async () => {
  const r = await digWithDrop({ 21: { id: 21, ...pile } }, { 22: { id: 22, ...fresh } })
  assert.deepEqual(r.drops.map(d => [d.id, d.name]), [[22, 'cobblestone']])
})

test('dig reports no drops when the cell only had an old pile and nothing new came', async () => {
  const r = await digWithDrop({ 21: { id: 21, ...pile } }, {})
  assert.equal(r.status, 'dug')
  assert.deepEqual(r.drops, [])
})

test('dig counts a drop that merged into an old stack as new', async () => {
  const stack = { count: 1 }
  const merging = { id: 21, ...pile, getDroppedItem: () => ({ name: 'cobblestone', count: stack.count }) }
  const { bot, p } = rig({ ...world, blocks: { '2,64,0': 'stone' }, entities: { 21: merging } })
  const original = bot.dig
  bot.dig = async (...args) => { const r = await original(...args); stack.count = 2; return r }
  const r = await p.dig('t1', { pos: at(2, 64, 0) })
  assert.deepEqual(r.drops.map(d => [d.id, d.name, d.count]), [[21, 'cobblestone', 1]])
})
test('place releases sneak when the placement throws', async () => {
  const { bot, p } = rig({ blocks: { '5,69,5': 'oak_door' }, items: [{ name: 'cobblestone', count: 2, slot: 36 }], pos: [5.5, 70, 5.5], hang: [] })
  bot.placeBlock = () => Promise.reject(new Error('refused'))
  assert.equal((await p.place('t1', { pos: at(5, 70, 5), item: 'cobblestone' })).status, 'failed')
  assert.equal(bot.controlState.sneak, false)
})

// dig's time bound is the expected dig time (bot.digTime with the held tool) plus a margin, not a fixed 10 s
test('a dig that takes longer than the old fixed bound is not cut off when digTime says it takes that long', async () => {
  const { bot, p } = rig(world)
  bot.digTime = () => 12000
  bot.dig = () => new Promise(resolve => setTimeout(resolve, 12000 * SCALE))
  const r = await p.dig('t1', { pos: at(2, 64, 0) })
  assert.equal(r.status, 'dug')
})

test('a dig that overruns its expected time by far still hits the bound', async () => {
  const { bot, p } = rig({ ...world, hang: ['dig'] })
  bot.digTime = () => 12000
  const r = await p.dig('t1', { pos: at(2, 64, 0) })
  assert.equal(r.status, 'timeout')
})

// digTime(pos, item): the expected dig time in ms with the named tool (the held one when item is omitted or held)
test('digTime is the held tool time without an item, and the named item time for another tool', () => {
  const { bot, p } = rig(world)
  bot.digTime = () => 12000
  bot.registry.itemsByName = { diamond_pickaxe: { id: 7 } }
  const real = bot.blockAt
  bot.blockAt = pos => { const b = real(pos); return b && { ...b, digTime: type => (type === 7 ? 9400 : 99999) } }
  assert.equal(p.digTime(at(2, 64, 0)), 12000)
  assert.equal(p.digTime(at(2, 64, 0), 'diamond_pickaxe'), 9400)
})

test('digTime is 0 for air and a block that cannot be dug', () => {
  const { bot, p } = rig(world)
  bot.digTime = () => 12000
  assert.equal(p.digTime(at(0, 70, 0)), 0)
})

// harvestTools names the items minecraft-data lists as able to harvest a block; null when any tool (or the hand) does
test('harvestTools lists the harvesting item names, null when the block lists none', () => {
  const { bot, p } = rig(world)
  bot.registry.items = { 1: { name: 'stone_pickaxe' }, 2: { name: 'iron_pickaxe' } }
  bot.registry.blocksByName = { iron_ore: { harvestTools: { 1: true, 2: true } }, dirt: {} }
  assert.deepEqual(p.harvestTools('iron_ore'), ['stone_pickaxe', 'iron_pickaxe'])
  assert.equal(p.harvestTools('dirt'), null)
  assert.equal(p.harvestTools('no_such_block'), null)
})

// clearTime(block, item): the dig time in ms of a block kind with the named item, the bare hand when omitted
test('clearTime is the time of the named item or the hand, Infinity for an undiggable block, 0 for an unknown name', () => {
  const { bot, p } = rig(world)
  bot.registry = registryFor('1.21.8')
  fixDigMaterials(bot.registry)
  assert.equal(p.clearTime('snow_block'), 1000)
  assert.equal(p.clearTime('snow_block', 'wooden_shovel'), 150)
  assert.equal(p.clearTime('cobweb'), 20000)
  assert.equal(p.clearTime('cobweb', 'shears'), 400)
  assert.equal(p.clearTime('bedrock'), Infinity)
  assert.equal(p.clearTime('no_such_block'), 0)
})

const hurtRig = () => {
  const bot = stubBot(world)
  bot._client = new EventEmitter()
  bot.damageTypeNames = ['fall', 'mob_attack']
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.health = 20
  bot.emit('health')
  return { bot, seen }
}

test('the hurt event carries the amount and the attacker of the damage packet', () => {
  const { bot, seen } = hurtRig()
  bot._client.emit('damage_event', { entityId: bot.entity.id, sourceTypeId: 1, sourceCauseId: 9, sourceDirectId: 9 })
  bot.health = 17
  bot.emit('health')
  const hurt = seen.find(e => e.kind === 'hurt')
  assert.equal(hurt.amount, 3)
  assert.deepEqual(hurt.attacker, { id: 8, name: 'zombie' })
  assert.equal(hurt.damageType, 'mob_attack')
})

test('the hurt event carries the damage type when nothing is responsible, and nothing stale', () => {
  const { bot, seen } = hurtRig()
  bot._client.emit('damage_event', { entityId: bot.entity.id, sourceTypeId: 0, sourceCauseId: 0, sourceDirectId: 0 })
  bot.health = 14
  bot.emit('health')
  bot.health = 12
  bot.emit('health')
  const [fall, second] = seen.filter(e => e.kind === 'hurt')
  assert.equal(fall.damageType, 'fall')
  assert.equal('attacker' in fall, false)
  assert.equal('damageType' in second, false)
})

test('a damage packet for another entity is not the cause', () => {
  const { bot, seen } = hurtRig()
  bot._client.emit('damage_event', { entityId: 999, sourceTypeId: 0, sourceCauseId: 0, sourceDirectId: 0 })
  bot.health = 14
  bot.emit('health')
  assert.equal('damageType' in seen.find(e => e.kind === 'hurt'), false)
})

// a time bound that passes after part of the work happened reports what the inventory shows, so callers re-check the world
test('transfer: a timeout after the items moved reports the inventory change', async () => {
  const items = [{ name: 'cobblestone', count: 4, slot: 37 }]
  const bot = stubBot({ ...world, items, onClick: () => { items[0].count -= 2 } })
  const open = bot.openContainer
  let opens = 0
  bot.openContainer = (...args) => ++opens > 1 ? new Promise(() => {}) : open(...args) // the second open (the re-check) never answers
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  p.setOwner('t1')
  const result = await p.transfer('t1', { pos: at(3, 64, 0), direction: 'deposit', item: 'cobblestone', count: 2 })
  assert.deepEqual(result, { status: 'timeout', inventoryChange: { cobblestone: -2 } })
})

test('a timeout with nothing changed stays a bare timeout', async () => {
  const { p } = rig(hanging(acting.find(c => c.name === 'transfer')))
  assert.deepEqual(await p.transfer('t1', { pos: at(3, 64, 0), direction: 'deposit', item: 'cobblestone', count: 2 }), { status: 'timeout' })
})

test('entities with ids filter before the max cap', () => {
  const crowd = Object.fromEntries(Array.from({ length: 5 }, (_, i) => [100 + i, { id: 100 + i, name: 'cow', type: 'passive', position: at(1 + i * 0.1, 64, 0), height: 1.4 }]))
  const far = { id: 500, name: 'cow', type: 'passive', position: at(9, 64, 0), height: 1.4 }
  const bot = stubBot({ ...world, entities: { ...world.entities, ...crowd, 500: far } })
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  assert.equal(p.entities({ max: 3 }).some(e => e.id === 500), false)
  assert.deepEqual(p.entities({ max: 3, ids: [500] }).map(e => e.id), [500])
})
