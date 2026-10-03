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
  { name: 'look', args: { pos: at(1, 64, 1) }, hang: 'lookAt', cleanup: null, timeout: 'timeout' },
  { name: 'swim', args: { ms: 3000 }, hang: 'setControlState', cleanup: 'setControlState', timeout: 'timeout', over: { blocks: { ...world.blocks, '0,65,0': 'water' } } }
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
  assert.deepEqual(Object.keys(s).sort(), ['dimension', 'effects', 'experience', 'food', 'foodSaturation', 'health', 'held', 'inLava', 'inWater', 'inventory', 'isDay', 'isSleeping', 'onFire', 'oxygen', 'pos', 'timeOfDay', 'username'])
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
  assert.deepEqual(Object.keys(zombie).sort(), ['distance', 'id', 'kind', 'name', 'pos', 'visible'])
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

test('the died event carries the death position, the inventory and the experience at that moment', () => {
  const { bot, p } = rig(world)
  const seen = []
  p.onBodyEvent(e => seen.push(e))
  bot.entity.position = new Vec3(4, 70, 2)
  bot.experience = { level: 5, points: 120, progress: 0.3 }
  bot.emit('death')
  bot.entity.position = new Vec3(0, 64, 0)
  bot.experience = { level: 0, points: 0, progress: 0 }
  assert.deepEqual(seen, [{
    kind: 'died',
    pos: at(4, 70, 2),
    inventory: [{ name: 'bread', count: 2, slot: 36 }, { name: 'cobblestone', count: 4, slot: 37 }],
    experience: { level: 5, points: 120 }
  }])
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

test('moveTo: a goto that resolves with the body moved but short of the goal is partial', async () => {
  const { bot, p } = rig(world)
  bot.pathfinder.goto = () => { bot.entity.position = new Vec3(10, 64, 0); return Promise.resolve() }
  const result = await p.moveTo('t1', { pos: at(30, 64, 0) })
  assert.equal(result.status, 'partial')
})

test('moveTo: a goto that resolves within range is arrived', async () => {
  const { bot, p } = rig(world)
  bot.pathfinder.goto = () => { bot.entity.position = new Vec3(29.5, 64, 0.5); return Promise.resolve() }
  assert.equal((await p.moveTo('t1', { pos: at(30, 64, 0) })).status, 'arrived')
})

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
  const started = Date.now()
  await p.swim('t1', { ms: 600000 })
  assert.ok(Date.now() - started < 10000 * SCALE + 200)
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

test('after the connection ends an acting call reconnects first and then runs on the new bot', async () => {
  const bots = []
  const { p, seen } = await online({ connect: connectOnce(bots) })
  bots[0].emit('end', 'socket closed')
  assert.deepEqual(await p.look('t1', { pos: at(1, 64, 1) }), { status: 'ok' })
  assert.deepEqual(seen.map(e => e.kind), ['disconnected', 'online'])
  assert.equal(names(bots[1]).includes('lookAt'), true)
  assert.equal(names(bots[0]).includes('lookAt'), false)
})

test('after a kick an acting call resolves disconnected when every reconnect try fails', async () => {
  const bots = []
  const { p, seen } = await online({ connect: connectOnce(bots, true) })
  bots[0].emit('kicked', 'bye')
  assert.deepEqual(await p.look('t1', { pos: at(1, 64, 1) }), { status: 'disconnected' })
  assert.deepEqual(seen.map(e => e.kind), ['disconnected', 'reconnect-failed'])
  assert.equal(seen[1].reason, 'refused')
  assert.deepEqual(names(bots[0]).filter(n => n === 'lookAt'), [])
})

test('a stale token still rejects with cut while the bot is down', async () => {
  const bots = []
  const { p } = await online({ connect: connectOnce(bots, true) })
  bots[0].emit('end', 'gone')
  await assert.rejects(p.look('old', { pos: at(1, 64, 1) }), err => err.code === 'cut')
})

test('without a connection to remake, a down body resolves acting calls disconnected', async () => {
  const { bot, p } = rig(world)
  bot.emit('end', 'gone')
  assert.deepEqual(await p.look('t1', { pos: at(1, 64, 1) }), { status: 'disconnected' })
})

// ---- mineflayer rejections are statuses ----

const rejecting = [
  { name: 'dig', args: { pos: at(2, 64, 0) }, call: 'dig', message: 'Digging aborted' },
  { name: 'place', args: { pos: at(1, 64, 0), item: 'cobblestone' }, call: 'placeBlock', message: 'No block has been placed' },
  { name: 'equip', args: { item: 'bread' }, call: 'equip', message: 'cannot equip' },
  { name: 'eat', args: {}, call: 'consume', message: 'Consuming cancelled due to calling bot.consume() again' },
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

// ---- line of sight: entities reports `visible` for hostiles ----

const wall = Object.fromEntries([2, 3].flatMap(x => [64, 65, 66].map(y => [`${x},${y},0`, 'stone'])))
const zombieAt5 = { 9: { id: 9, name: 'zombie', type: 'hostile', position: at(5, 64, 0), height: 1.9, health: 20 } }

test('a hostile behind a two-thick wall is not visible, and is once the wall is gone', () => {
  const behind = rig({ blocks: wall, entities: zombieAt5 }).p.entities({ kind: 'hostile' })
  assert.equal(behind.length, 1)
  assert.equal(behind[0].visible, false)
  const open = rig({ blocks: {}, entities: zombieAt5 }).p.entities({ kind: 'hostile' })
  assert.equal(open[0].visible, true)
})

test('a hostile seen across a diagonal is blocked by a pillar on the line', () => {
  const pillar = Object.fromEntries([64, 65, 66].map(y => ['2,' + y + ',2', 'stone']))
  const e = { 9: { id: 9, name: 'zombie', type: 'hostile', position: at(4, 64, 4), height: 1.9 } }
  assert.equal(rig({ blocks: pillar, entities: e }).p.entities({ kind: 'hostile' })[0].visible, false)
})

test('glass and unloaded cells do not block sight', () => {
  const glass = Object.fromEntries([64, 65].map(y => ['2,' + y + ',0', 'glass']))
  assert.equal(rig({ blocks: glass, entities: zombieAt5 }).p.entities({ kind: 'hostile' })[0].visible, true)
})

test('only hostiles carry visible', () => {
  const { p } = rig({ blocks: wall, entities: { ...zombieAt5, 7: { id: 7, name: 'cow', type: 'passive', position: at(5, 64, 1) } } })
  assert.equal('visible' in p.entities({ kind: 'passive' })[0], false)
})

test('wait resolves ok after its time, scaled by timeScale', async () => {
  const { p } = rig(world)
  const t0 = Date.now()
  assert.deepEqual(await p.wait('t1', { ms: 1000 }), { status: 'ok' })
  assert.ok(Date.now() - t0 < 500)
})

test('a cut during a wait rejects at once', async () => {
  const { p } = rig(world)
  const t0 = Date.now()
  const call = p.wait('t1', { ms: 10000 })
  p.setOwner('t2')
  await assert.rejects(call, err => err.code === 'cut')
  assert.ok(Date.now() - t0 < 50)
})

test('a wait with a stale token rejects with cut', async () => {
  const { p } = rig(world)
  await assert.rejects(p.wait('old', { ms: 10 }), err => err.code === 'cut')
})

test('a wait is clamped to 10 s and to at least 0', async () => {
  const bot = stubBot(world)
  const p = createPrimitivesFromBot(bot, { timeScale: 0.0001 })
  p.setOwner('t1')
  const t0 = Date.now()
  assert.deepEqual(await p.wait('t1', { ms: 1e9 }), { status: 'ok' })
  assert.deepEqual(await p.wait('t1', { ms: -5 }), { status: 'ok' })
  assert.ok(Date.now() - t0 < 500)
})

// ---- offline as body state ----

test('while offline isOffline is true and sensing answers offline instead of stale values', async () => {
  const { p } = await online()
  assert.equal(p.isOffline(), false)
  const pending = p.offline('t1', { ms: 10 * MIN })
  assert.equal(p.isOffline(), true)
  assert.deepEqual(p.self(), { status: 'offline' })
  assert.deepEqual(p.entities({}), [])
  assert.deepEqual(p.blocks({}), [])
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
  assert.deepEqual(names(bots[0]), ['quit'])
  assert.deepEqual(names(bots[1]).filter(n => n !== 'blockAt'), ['look'])
})

test('a call whose owner is cut while waiting for the reconnect rejects with cut', async () => {
  const { p, bots } = await online()
  const pending = p.offline('t1', { ms: 10 * MIN })
  p.setOwner('t2')
  const look = p.look('t2', { yaw: 0, pitch: 0 })
  p.setOwner('t3')
  await assert.rejects(look, err => err.code === 'cut')
  await pending
  assert.deepEqual(names(bots[1]).filter(n => n !== 'blockAt'), [])
})

// ---- replaceable cells: the game replaces them, so place treats them as free ----

const replaceable = ['fire', 'soul_fire', 'short_grass', 'tall_grass', 'grass', 'snow']

for (const name of replaceable) {
  test(`place puts a block into a ${name} cell`, async () => {
    const { p } = rig({ blocks: { '1,63,0': 'stone', '1,64,0': name }, items: [{ name: 'cobblestone', count: 1, slot: 36 }] })
    assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'cobblestone' }), { status: 'placed', block: 'cobblestone' })
  })

  test(`a water bucket pours onto a ${name} cell`, async () => {
    const { p } = bucketRig({ blocks: { '1,64,0': name }, item: 'water_bucket', onActivate: blocks => { blocks['1,64,0'] = 'water' } })
    assert.deepEqual(await p.place('t1', { pos: at(1, 64, 0), item: 'water_bucket' }), { status: 'placed', block: 'water' })
  })
}

test('place into a cell of a solid block, or a plant that is not replaceable, stays occupied', async () => {
  for (const name of ['stone', 'oak_sapling']) {
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

test('swim toward: within a block of the target counts as landed', async () => {
  const { bot, p } = rig({ blocks: pool() })
  setTimeout(() => { bot.entity.position = new Vec3(3.2, 65, 0.3) }, 5)
  assert.equal((await p.swim('t1', { ms: 3000, toward: rim })).status, 'landed')
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
