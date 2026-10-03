import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createPrimitives, createPrimitivesFromBot } from './primitives.mjs'
import { stubBot, names, Vec3 } from './stub-bot.mjs'

const at = (x, y, z) => ({ x, y, z })
const SCALE = 0.01 // every time bound shrinks 100 times: 20 s becomes 200 ms

const rig = (spec) => {
  const bot = stubBot(spec)
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  p.setOwner('t1')
  return { bot, p }
}

// Each case: a world where the primitive reaches the bot call named by `hangs`, the args, the cleanup call a cut or
// timeout must make (null when none is needed), and the status a timeout resolves with (null when it cannot time out).
const world = {
  blocks: { '2,64,0': 'oak_log', '3,64,0': 'chest', '2,64,1': 'red_bed', '0,63,5': 'stone' },
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
  { name: 'equip', args: { item: 'bread' }, hang: 'equip', cleanup: null, timeout: 'timeout' },
  { name: 'eat', args: {}, hang: 'consume', cleanup: 'deactivateItem', timeout: 'timeout' },
  { name: 'attack', args: { id: 8 }, hang: 'attack', cleanup: null, timeout: 'timeout' },
  { name: 'sleep', args: { pos: at(2, 64, 1) }, hang: 'sleep', cleanup: 'wake', timeout: 'timeout', over: { entities: {} } },
  { name: 'look', args: { pos: at(1, 64, 1) }, hang: 'lookAt', cleanup: null, timeout: 'timeout' }
]
const hanging = c => ({ ...world, ...c.over, hang: [c.hang] })
const cutError = err => err.code === 'cut' && err.cut === true

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

  test(`${c.name}: setOwner during the call rejects with cut and runs the cleanup`, async () => {
    const { bot, p } = rig(hanging(c))
    const call = p[c.name]('t1', c.args)
    await new Promise(r => setTimeout(r, 5))
    assert.ok(names(bot).includes(c.hang), `reached ${c.hang}`)
    p.setOwner('t2')
    await assert.rejects(call, cutError)
    if (c.cleanup) assert.ok(names(bot).includes(c.cleanup), `called ${c.cleanup}`)
    const before = names(bot).length
    await new Promise(r => setTimeout(r, 30))
    assert.equal(names(bot).length, before, 'nothing reaches the bot after the cut')
  })

  test(`${c.name}: a bot call that never returns hits the time bound`, async () => {
    const { bot, p } = rig(hanging(c))
    const result = await p[c.name]('t1', c.args)
    assert.equal(result.status, c.timeout)
    if (c.cleanup) assert.ok(names(bot).includes(c.cleanup), `called ${c.cleanup}`)
  })
}

test('setOwner to the same token does not cut', async () => {
  const { p } = rig({ ...world, hang: ['dig'] })
  const call = p.dig('t1', { pos: at(2, 64, 0) })
  p.setOwner('t1')
  const result = await call
  assert.equal(result.status, 'timeout')
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
  ['eat', {}, { items: [] }, 'no-food'],
  ['eat', {}, { food: 20 }, 'full'],
  ['eat', {}, {}, 'ate'],
  ['attack', { id: 99 }, {}, 'gone'],
  ['attack', { id: 8 }, { entities: { 8: { id: 8, name: 'zombie', type: 'hostile', position: at(9, 64, 0), height: 1.9 } } }, 'out-of-reach'],
  ['attack', { id: 8 }, {}, 'hit'],
  ['sleep', { pos: at(9, 64, 9) }, {}, 'missing'],
  ['sleep', { pos: at(2, 64, 1) }, { timeOfDay: 1000 }, 'not-night'],
  ['sleep', { pos: at(2, 64, 1) }, { entities: { 8: { id: 8, name: 'zombie', type: 'hostile', position: at(2, 64, 0) } } }, 'monsters-near'],
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

const badArgs = [['moveTo', {}], ['dig', {}], ['place', { pos: at(1, 1, 1) }], ['transfer', { pos: at(1, 1, 1), direction: 'sideways', item: 'x', count: 1 }], ['look', {}], ['collect', {}], ['attack', {}]]
for (const [name, args] of badArgs) {
  test(`${name} with bad args rejects with bad-args`, async () => {
    const { p } = rig(world)
    await assert.rejects(p[name]('t1', args), err => err.code === 'bad-args')
  })
}

test('self reports the body in the contract shape', () => {
  const { p } = rig(world)
  const s = p.self()
  assert.deepEqual(Object.keys(s).sort(), ['dimension', 'experience', 'food', 'foodSaturation', 'health', 'held', 'inLava', 'inWater', 'inventory', 'isDay', 'isSleeping', 'onFire', 'oxygen', 'pos', 'timeOfDay', 'username'])
  assert.equal(s.isDay, false)
  assert.deepEqual(s.inventory[0], { name: 'bread', count: 2, slot: 36 })
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

test('blocks matches by name, predicate and max, sorted by distance', () => {
  const { p } = rig(world)
  assert.deepEqual(p.blocks({ names: ['oak_log'] }).map(b => b.name), ['oak_log'])
  assert.deepEqual(p.blocks({ match: n => n.endsWith('_bed') }).map(b => b.name), ['red_bed'])
  assert.equal(p.blocks({ max: 2 }).length, 2)
  assert.equal(p.blocks({ names: ['oak_log'] })[0].pos.x, 2)
})

test('blockAt gives a name and a plain position', () => {
  const { p } = rig(world)
  assert.deepEqual(p.blockAt(at(2, 64, 0)), { name: 'oak_log', pos: at(2, 64, 0) })
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
  bot.emit('chat', 'Dan', 'hi')
  off()
  bot.emit('wake')
  assert.deepEqual(seen.map(e => e.kind), ['hurt', 'died', 'spawned', 'respawned', 'chat'])
  assert.equal(seen[0].health, 14)
  assert.deepEqual(seen[4], { kind: 'chat', from: 'Dan', message: 'hi' })
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
const keysFor = metadataKeys => ({ entitiesByName: { player: { metadataKeys } } })

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

test('entities tells sleeping players from standing ones and gives usernames', () => {
  const player = (id, pose) => ({ id, type: 'player', name: 'player', username: `P${id}`, position: at(id, 64, 0), metadata: pose === undefined ? [] : [0, 0, 0, 0, 0, 0, pose] })
  const p = withBot(bot => { bot.entities = { 1: player(1, 2), 2: player(2, 0), 3: player(3) } })
  const found = p.entities({ kind: 'player' })
  assert.deepEqual(found.map(e => [e.username, e.sleeping]), [['P1', true], ['P2', false], ['P3', false]])
})

test('entities finds the pose index through the registry', () => {
  const p = withBot(bot => {
    bot.registry = { ...bot.registry, ...keysFor(['shared_flags', 'pose']) }
    bot.entities = { 1: { id: 1, type: 'player', name: 'player', username: 'A', position: at(1, 64, 0), metadata: [0, 2] } }
  })
  assert.equal(p.entities({})[0].sleeping, true)
})

test('entities does not put sleeping or username on mobs', () => {
  const [zombie] = rig(world).p.entities({ kind: 'hostile' })
  assert.deepEqual(Object.keys(zombie).sort(), ['distance', 'id', 'kind', 'name', 'pos'])
})

const mobCases = [
  ['a creeper typed hostile', { name: 'creeper', type: 'hostile' }, 'hostile', true],
  ['a zombie typed hostile', { name: 'zombie', type: 'hostile' }, 'hostile', false],
  ['a creeper with only a hostile kind', { name: 'creeper', kind: 'Hostile mobs' }, 'hostile', true],
  ['a cow', { name: 'cow', type: 'animal', kind: 'Passive mobs' }, 'passive', false]
]
for (const [label, fields, kind, creeper] of mobCases) {
  test(`entities classifies ${label}`, () => {
    const p = withBot(bot => { bot.entities = { 5: { id: 5, position: at(3, 64, 0), ...fields } } })
    const [e] = p.entities({})
    assert.deepEqual([e.kind, e.name, Boolean(e.creeper)], [kind, fields.name, creeper])
  })
}

// ---- death and respawn events ----

test('the died event carries the death position and the inventory at that moment', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.entity.position = new Vec3(4, 70, 2)
  bot.emit('death')
  bot.entity.position = new Vec3(0, 64, 0)
  assert.deepEqual(seen, [{ kind: 'died', pos: at(4, 70, 2), inventory: [{ name: 'bread', count: 2, slot: 36 }, { name: 'cobblestone', count: 4, slot: 37 }] }])
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
  assert.deepEqual(names(bots[0]), ['quit'])
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'online'])
  bots[1].health = 7
  assert.equal(p.self().health, 7)
  await p.look('t1', { yaw: 0, pitch: 0 })
  assert.deepEqual(names(bots[1]).filter(n => n !== 'blockAt'), ['look'])
  assert.deepEqual(names(bots[0]), ['quit'])
})

test('offline reports its ms in the events and the online event carries the position', async () => {
  const { p, seen } = await online()
  await p.offline('t1', { ms: 1000 })
  assert.deepEqual(seen, [{ kind: 'offline', ms: 1000 }, { kind: 'online', pos: at(0, 64, 0) }])
})

test('events of the new bot reach listeners and the old bot goes quiet', async () => {
  const { p, bots, seen } = await online()
  await p.offline('t1', { ms: 1000 })
  bots[0].emit('end', 'quit')
  bots[0].emit('chat', 'Dan', 'ghost')
  bots[1].emit('chat', 'Dan', 'hi')
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
    assert.deepEqual(names(bots[0]), [])
  })
}

test('offline with a stale token rejects with cut and never quits', async () => {
  const { p, bots } = await online()
  await assert.rejects(p.offline('old', { ms: 1000 }), err => err.code === 'cut')
  assert.deepEqual(names(bots[0]), [])
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
  assert.deepEqual(names(bots[1]), ['quit'])
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

test('a reconnect that keeps failing rejects and reports the body disconnected', async () => {
  let calls = 0
  const connect = async () => {
    calls += 1
    if (calls > 1) throw new Error('refused')
    return stubBot(world)
  }
  const { p, seen } = await online({ connect })
  await assert.rejects(p.offline('t1', { ms: 1000 }), /refused/)
  assert.deepEqual(seen.map(e => e.kind), ['offline', 'disconnected'])
})

test('blocks and blockAt carry the age state of a crop, and only then', () => {
  const { p } = rig({ blocks: { '1,64,0': 'carrots', '2,64,0': 'dirt' }, props: { '1,64,0': { age: 7 } } })
  assert.deepEqual(p.blocks({ names: ['carrots'] }).map(b => b.age), [7])
  assert.equal(p.blockAt(at(1, 64, 0)).age, 7)
  assert.equal('age' in p.blockAt(at(2, 64, 0)), false)
})
