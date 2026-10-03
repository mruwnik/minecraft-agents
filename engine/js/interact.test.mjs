import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import { interactWith, mobFields, refusal } from './interact.mjs'

const REACH = 3.5
const ctx = { alive: () => {}, onAbort: () => {} }
const opts = { timeScale: 0.02, reach: REACH }

const registry = {
  entitiesByName: {
    sheep: { metadataKeys: ['shared_flags', 'air', 'name', 'name_visible', 'silent', 'no_gravity', 'pose', 'frozen', 'baby', 'wool'] },
    cow: { metadataKeys: ['shared_flags', 'air', 'name', 'name_visible', 'silent', 'no_gravity', 'pose', 'frozen', 'baby'] },
    zombie: { metadataKeys: ['shared_flags', 'air'] }
  }
}
const BABY = 8
const WOOL = 9

const entity = (name, extra = {}) => ({ id: 5, name, position: vec3(1, 64, 0), height: 1.4, ...extra })

// a hand-made bot; `onUse(bot, target)` scripts what the use does to the world
const makeBot = ({ target = entity('sheep'), items = [], held = null, onUse = () => {}, onSneak = () => {}, at = vec3(0, 64, 0) } = {}) => {
  const bot = new EventEmitter()
  const calls = []
  const controls = []
  return Object.assign(bot, {
    calls,
    controls,
    registry,
    entities: target ? { [target.id]: target } : {},
    entity: { id: 4, position: at, height: 1.62 },
    heldItem: held,
    inventory: { items: () => items },
    _client: new EventEmitter(),
    setControlState: (name, on) => { controls.push([name, on]); name === 'sneak' && on && onSneak(bot) },
    equip: async item => { calls.push('equip'); bot.heldItem = item },
    unequip: async () => { calls.push('unequip'); bot.heldItem = null },
    lookAt: async () => { calls.push('lookAt') },
    closeWindow: w => { calls.push('closeWindow'); bot.currentWindow = null },
    dismount: () => { calls.push('dismount') },
    useOn: t => { calls.push('useOn'); onUse(bot, t) }
  })
}
const wheat = () => ({ name: 'wheat', count: 2 })
const shears = () => ({ name: 'shears', count: 1, durabilityUsed: 0 })

test('mobFields', async t => {
  const rows = [
    ['adult cow without metadata', 'cow', {}, { baby: false }],
    ['baby cow', 'cow', { metadata: { [BABY]: true } }, { baby: true }],
    ['sheared sheep', 'sheep', { metadata: { [WOOL]: 0x10 } }, { baby: false, sheared: true }],
    ['coloured sheep', 'sheep', { metadata: { [WOOL]: 0x0e } }, { baby: false, sheared: false }],
    ['zombie has no baby key', 'zombie', {}, {}],
    ['uuid passes through', 'zombie', { uuid: 'abc-123' }, { uuid: 'abc-123' }]
  ]
  for (const [label, name, extra, expected] of rows) {
    await t.test(label, () => assert.deepEqual(mobFields({ registry }, entity(name, extra)), expected))
  }
})

test('mobFields falls back to wool index 18 without a registry', () => {
  const e = entity('sheep', { metadata: { 18: 0x10 } })
  assert.deepEqual(mobFields({}, e), { sheared: true })
})

const scenarios = [
  { label: 'gone', bot: { target: null }, a: { id: 5 }, expect: { status: 'gone' } },
  { label: 'no-item', bot: {}, a: { id: 5, item: 'wheat' }, expect: { status: 'no-item' } },
  { label: 'out-of-reach', bot: { target: entity('cow', { position: vec3(9, 64, 0) }), items: [wheat()] }, a: { id: 5, item: 'wheat' }, expect: { status: 'out-of-reach' } },
  {
    label: 'feed consumes one wheat',
    bot: { target: entity('cow'), items: [wheat()], onUse: bot => { bot.inventory.items = () => [{ name: 'wheat', count: 1 }] } },
    a: { id: 5, item: 'wheat' },
    expect: { status: 'used', consumed: 1, worn: 0, love: false, leash: null, changed: {} }
  },
  {
    label: 'love status',
    bot: { target: entity('cow'), items: [wheat()], onUse: bot => bot._client.emit('entity_status', { entityId: 5, entityStatus: 18 }) },
    a: { id: 5, item: 'wheat' },
    expect: { status: 'used', consumed: 0, worn: 0, love: true, leash: null, changed: {} }
  },
  {
    label: 'love status of another entity is ignored',
    bot: { target: entity('cow'), items: [wheat()], onUse: bot => bot._client.emit('entity_status', { entityId: 6, entityStatus: 18 }) },
    a: { id: 5, item: 'wheat' },
    expect: { status: 'no-effect', consumed: 0, worn: 0, love: false, leash: null, changed: {} }
  },
  {
    label: 'shears wear and the wool bit is set',
    bot: {
      target: entity('sheep'),
      items: [shears()],
      onUse: (bot, t) => { t.metadata = { [WOOL]: 0x10 }; bot.heldItem.durabilityUsed = 1 }
    },
    a: { id: 5, item: 'shears' },
    expect: { status: 'used', consumed: 0, worn: 1, love: false, leash: null, changed: { sheared: [false, true] } }
  },
  {
    label: 'lead attaches',
    bot: { target: entity('cow'), items: [{ name: 'lead', count: 1 }], onUse: bot => bot._client.emit('attach_entity', { entityId: 5, vehicleId: 7 }) },
    a: { id: 5, item: 'lead' },
    expect: { status: 'used', consumed: 0, worn: 0, love: false, leash: 'attached', changed: {} }
  },
  {
    label: 'empty hand detaches the lead (holder 0)',
    bot: { target: entity('cow'), held: wheat(), onUse: bot => bot._client.emit('attach_entity', { entityId: 5, vehicleId: 0 }) },
    a: { id: 5 },
    expect: { status: 'used', consumed: 0, worn: 0, love: false, leash: 'detached', changed: {} },
    calls: ['unequip', 'lookAt', 'useOn']
  },
  {
    label: 'holder -1 is detached too',
    bot: { target: entity('cow'), held: wheat(), onUse: bot => bot._client.emit('attach_entity', { entityId: 5, vehicleId: -1 }) },
    a: { id: 5 },
    expect: { status: 'used', consumed: 0, worn: 0, love: false, leash: 'detached', changed: {} }
  },
  {
    label: 'attach of another entity is ignored',
    bot: { target: entity('cow'), items: [wheat()], onUse: bot => bot._client.emit('attach_entity', { entityId: 6, vehicleId: 7 }) },
    a: { id: 5, item: 'wheat' },
    expect: { status: 'no-effect', consumed: 0, worn: 0, love: false, leash: null, changed: {} }
  },
  {
    label: 'refused villager',
    bot: { target: entity('villager') },
    a: { id: 5 },
    expect: { status: 'cannot', reason: 'opens-window' },
    calls: []
  },
  {
    label: 'use that mounts the body',
    bot: { target: entity('pig'), onUse: bot => { bot.vehicle = { id: 9 } }, onSneak: bot => bot._client.emit('set_passengers', { entityId: 9, passengers: [] }) },
    a: { id: 5 },
    expect: { status: 'failed', reason: 'mounted', consumed: 0, worn: 0, love: false, leash: null, changed: {} },
    calls: ['unequip', 'lookAt', 'useOn']
  },
  {
    label: 'use that opens a window',
    bot: { target: entity('pig'), onUse: bot => { bot.currentWindow = { id: 1 } } },
    a: { id: 5 },
    expect: { status: 'failed', reason: 'opened-window', consumed: 0, worn: 0, love: false, leash: null, changed: {} },
    calls: ['unequip', 'lookAt', 'useOn', 'closeWindow']
  },
  {
    label: 'nothing happens',
    bot: { target: entity('cow'), items: [wheat()] },
    a: { id: 5, item: 'wheat' },
    expect: { status: 'no-effect', consumed: 0, worn: 0, love: false, leash: null, changed: {} }
  }
]

for (const { label, bot: spec, a, expect, calls } of scenarios) {
  test(`interactWith: ${label}`, async () => {
    const bot = makeBot(spec)
    const result = await interactWith(bot, ctx, a, opts)
    assert.deepEqual(result, expect)
    assert.equal(bot._client.listenerCount('attach_entity'), 0)
    assert.equal(bot._client.listenerCount('entity_status'), 0)
    calls && assert.deepEqual(bot.calls, calls)
  })
}

test('interactWith removes its listeners when the call is cut', async () => {
  const bot = makeBot({ target: entity('cow'), items: [wheat()] })
  const cut = { alive: () => { throw new Error('cut') }, onAbort: () => {} }
  await assert.rejects(interactWith(bot, cut, { id: 5, item: 'wheat' }, opts), /cut/)
  assert.equal(bot._client.listenerCount('attach_entity'), 0)
  assert.equal(bot._client.listenerCount('entity_status'), 0)
})

test('refusal', async t => {
  const rows = [
    ['villager', 'opens-window'], ['wandering_trader', 'opens-window'], ['chest_minecart', 'opens-window'],
    ['horse', 'mounts'], ['minecart', 'mounts'], ['oak_boat', 'mounts'], ['oak_chest_boat', 'mounts'], ['bamboo_raft', 'mounts'],
    ['cow', null], ['pig', null], ['sheep', null]
  ]
  for (const [name, reason] of rows) {
    await t.test(name, () => assert.equal(refusal(name), reason))
  }
})

test('a cut registers an undo that sneaks off and closes the window', async () => {
  const bot = makeBot({ target: entity('pig') })
  let undo
  await interactWith(bot, { alive: () => {}, onAbort: f => { undo = f } }, { id: 5 }, opts)
  bot.vehicle = { id: 9 }
  bot.currentWindow = { id: 1 }
  undo()
  assert.deepEqual(bot.calls.slice(-1), ['closeWindow'])
  assert.deepEqual(bot.controls, [['sneak', true]])
  await new Promise(resolve => setTimeout(resolve, 600))
  assert.deepEqual(bot.controls.at(-1), ['sneak', false])
})

const mounted = onSneak => makeBot({ target: entity('pig'), onUse: bot => { bot.vehicle = { id: 9 } }, onSneak })

test('mounting is undone by sneaking; set_passengers without the body clears the vehicle, then the sneak is released', async () => {
  const bot = mounted(b => b._client.emit('set_passengers', { entityId: 9, passengers: [] }))
  const r = await interactWith(bot, ctx, { id: 5 }, opts)
  assert.equal(r.reason, 'mounted')
  assert.equal(bot.vehicle, null)
  assert.deepEqual(bot.controls, [['sneak', true], ['sneak', false]])
  assert.equal(bot._client.listenerCount('set_passengers'), 0)
})

test('still mounted after the wait is mounted-stuck, and the sneak is still released', async () => {
  const r = await (async () => {
    const bot = mounted(() => {})
    return { bot, r: await interactWith(bot, ctx, { id: 5 }, opts) }
  })()
  assert.deepEqual([r.r.status, r.r.reason], ['failed', 'mounted-stuck'])
  assert.deepEqual(r.bot.controls.at(-1), ['sneak', false])
  assert.equal(r.bot._client.listenerCount('set_passengers'), 0)
})

test('set_passengers for another vehicle, or still carrying the body, does not clear bot.vehicle', async () => {
  const packets = [{ entityId: 8, passengers: [] }, { entityId: 9, passengers: [4] }]
  for (const p of packets) {
    const bot = mounted(b => b._client.emit('set_passengers', p))
    const r = await interactWith(bot, ctx, { id: 5 }, opts)
    assert.equal(r.reason, 'mounted-stuck')
    assert.deepEqual(bot.vehicle, { id: 9 })
  }
})

// love and the inventory update arrive as separate packets, in either order
const slowOpts = { timeScale: 0.2, reach: REACH }

test('a consumption seen before love waits for the love status', async () => {
  const items = [wheat()]
  const bot = makeBot({
    target: entity('cow'),
    items,
    held: items[0],
    onUse: bot => {
      items.length = 0
      setTimeout(() => bot._client.emit('entity_status', { entityId: 5, entityStatus: 18 }), 60 * slowOpts.timeScale)
    }
  })
  const r = await interactWith(bot, ctx, { id: 5, item: 'wheat' }, slowOpts)
  assert.equal(r.status, 'used')
  assert.equal(r.consumed, 2)
  assert.equal(r.love, true)
})

test('a consumption with no love waits out the grace and reports love false', async () => {
  const items = [wheat()]
  const bot = makeBot({ target: entity('cow'), items, held: items[0], onUse: () => { items.length = 0 } })
  const start = Date.now()
  const r = await interactWith(bot, ctx, { id: 5, item: 'wheat' }, slowOpts)
  assert.equal(r.status, 'used')
  assert.equal(r.consumed, 2)
  assert.equal(r.love, false)
  assert.ok(Date.now() - start >= 150 * slowOpts.timeScale - 2)
})
