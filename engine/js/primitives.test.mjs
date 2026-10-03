import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createPrimitivesFromBot } from './primitives.mjs'
import { stubBot, names } from './stub-bot.mjs'

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
  assert.deepEqual(Object.keys(s).sort(), ['food', 'health', 'held', 'inventory', 'isDay', 'pos', 'timeOfDay', 'username'])
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
  bot.emit('chat', 'Dan', 'hi')
  off()
  bot.emit('wake')
  assert.deepEqual(seen.map(e => e.kind), ['hurt', 'died', 'respawned', 'chat'])
  assert.equal(seen[0].health, 14)
  assert.deepEqual(seen[3], { kind: 'chat', from: 'Dan', message: 'hi' })
})

test('close quits the bot', async () => {
  const { bot, p } = rig(world)
  await p.close()
  assert.deepEqual(names(bot), ['quit'])
})
