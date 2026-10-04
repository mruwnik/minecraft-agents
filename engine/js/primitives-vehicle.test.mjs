// Why JavaScript: tests the Mineflayer adapter primitives.mjs, which stays JS (Mineflayer boundary).
// The vehicle primitives as createPrimitivesFromBot wires them: sensing fields, mount, dismount, and the walking
// primitives refusing while aboard. vehicle.test.mjs covers the rules themselves.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createPrimitivesFromBot } from './primitives.mjs'
import { stubBot, names } from './stub-bot.mjs'

const at = (x, y, z) => ({ x, y, z })
const SCALE = 0.01
const BODY = 4

const boat = { id: 9, name: 'oak_boat', uuid: 'u-9', type: 'other', position: at(1, 64, 0), height: 0.56 }
const cow = { id: 7, name: 'cow', uuid: 'u-7', type: 'passive', position: at(2, 64, 0), height: 1.4 }

const rig = (spec = {}) => {
  const bot = stubBot({ entities: { 9: { ...boat }, 7: { ...cow } }, ...spec })
  bot.entity.id = BODY
  bot.entities[BODY] = bot.entity
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE })
  p.setOwner('t1')
  return { bot, p }
}
const passengers = (bot, entityId, ids) => bot._client.emit('set_passengers', { entityId, passengers: ids })

test('self().vehicle is null on foot and names the vehicle aboard', () => {
  const { bot, p } = rig()
  assert.equal(p.self().vehicle, null)
  passengers(bot, 9, [BODY])
  assert.deepEqual(p.self().vehicle, { id: 9, uuid: 'u-9', name: 'oak_boat' })
  passengers(bot, 9, [])
  assert.equal(p.self().vehicle, null)
})

test('entities show passengers and vehicle', () => {
  const { bot, p } = rig()
  passengers(bot, 9, [7])
  const found = id => p.entities({}).find(e => e.id === id)
  assert.deepEqual(found(9).passengers, [7])
  assert.equal(found(7).vehicle, 9)
  assert.equal('vehicle' in found(9), false)
})

test('mount uses the entity and resolves once the server seats the body', async () => {
  const { bot, p } = rig()
  bot.mount = target => { bot.calls.push({ name: 'mount', args: [target.id] }); passengers(bot, target.id, [BODY]) }
  const r = await p.mount('t1', { id: 9 })
  assert.deepEqual(r, { status: 'mounted', vehicle: { id: 9, uuid: 'u-9', name: 'oak_boat' } })
  assert.ok(names(bot).includes('mount'))
})

test('mount and dismount check their args', async () => {
  const { p } = rig()
  await assert.rejects(p.mount('t1', {}), /mount needs an entity id/)
  await assert.rejects(p.dismount('t1', { yaw: 'north' }), /dismount yaw/)
})

test('dismount answers not-mounted on foot', async () => {
  const { p } = rig()
  assert.deepEqual(await p.dismount('t1', {}), { status: 'not-mounted' })
})

test('dismount sneaks off once the server drops and places the body', async () => {
  const { bot, p } = rig()
  passengers(bot, 9, [BODY])
  bot.setControlState = (control, on) => {
    bot.calls.push({ name: 'setControlState', args: [control, on] })
    if (control !== 'sneak' || !on) return
    passengers(bot, 9, [])
    bot.emit('forcedMove')
  }
  const r = await p.dismount('t1', { yaw: 180 })
  assert.equal(r.status, 'dismounted')
  assert.deepEqual(bot.calls.filter(c => c.name === 'setControlState').map(c => c.args), [['sneak', true], ['sneak', false]])
})

test('moveTo and steer refuse while aboard, without touching the bot', async t => {
  const rows = [
    ['moveTo', { pos: at(10, 64, 0) }],
    ['steer', { decide: () => ({ done: true }), timeoutS: 1 }]
  ]
  for (const [name, args] of rows) {
    await t.test(name, async () => {
      const { bot, p } = rig()
      passengers(bot, 9, [BODY])
      bot.calls.length = 0
      assert.deepEqual(await p[name]('t1', args), { status: 'mounted' })
      assert.deepEqual(names(bot).filter(n => n !== 'blockAt'), [])
    })
  }
})

test('a reconnected bot is tracked too', async () => {
  const fresh = stubBot({ entities: { 9: { ...boat } } })
  fresh.entity.id = BODY
  const bot = stubBot({ entities: { 9: { ...boat } } })
  bot.entity.id = BODY
  const p = createPrimitivesFromBot(bot, { timeScale: SCALE, reconnect: async () => fresh, worldTimeoutMs: 10 })
  p.setOwner('t1')
  await p.offline('t1', { ms: 0 })
  passengers(fresh, 9, [BODY])
  assert.deepEqual(p.self().vehicle, { id: 9, uuid: 'u-9', name: 'oak_boat' })
})
