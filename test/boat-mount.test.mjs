import { test } from 'node:test'
import assert from 'node:assert/strict'
import vec3 from 'vec3'
import { makeBoatRuntime } from '../src/body/boat.mjs'

const { Vec3 } = vec3
function fixture (update = () => {}) {
  const player = { id: 1, name: 'player', position: new Vec3(0, 63, 0) }
  const boat = { id: 2, name: 'oak_boat', position: new Vec3(0, 62.5, 1), passengers: [] }
  let waits = 0, mounts = 0, cancelled = false
  const bot = {
    entity: player, entities: { 1: player, 2: boat },
    mount: () => { mounts++ }, dismount: () => {},
    waitForTicks: () => { throw new Error('mounted physics ticks never arrive') }
  }
  const runtime = makeBoatRuntime({
    getBot: () => bot, getBoatLeashHolder: () => new Map(),
    goNear: async () => {}, pos: () => '0,63,0',
    cancelGuard: () => { if (cancelled) throw new Error('cancelled') },
    pause: async ms => { assert.equal(ms, 50); update({ bot, boat, player, waits: ++waits }) }
  })
  return { bot, boat, player, runtime, get waits () { return waits }, get mounts () { return mounts }, cancel: () => { cancelled = true } }
}

test('server-confirmed boat mount succeeds without mounted physics ticks', async () => {
  const h = fixture(({ bot, boat, player, waits }) => {
    if (waits === 2) { bot.vehicle = boat; boat.passengers = [player] }
  })
  assert.equal((await h.runtime.long.boat_mount({ id: 2 })).mounted.id, 2)
  assert.equal(h.waits, 2)
  assert.equal(h.mounts, 1)
})

test('boat mount is bounded and rejects missing server acceptance or controlling seat', async () => {
  const h = fixture()
  await assert.rejects(h.runtime.long.boat_mount({ id: 2 }), /did not accept the mount/)
  assert.equal(h.waits, 20)
  const occupied = fixture(({ bot, boat }) => { bot.vehicle = boat; boat.passengers = [{ id: 3 }] })
  await assert.rejects(occupied.runtime.long.boat_mount({ id: 2 }), /another passenger controls/)
})

test('boat passenger wait observes cancellation without sending a dismount', async () => {
  const h = fixture(() => h.cancel())
  await assert.rejects(h.runtime.long.boat_mount({ id: 2 }), /cancelled/)
  assert.equal(h.waits, 1)
})

test('raw boat dismount waits for server passenger removal on wall time', async () => {
  const h = fixture(({ bot, boat, waits }) => { if (waits === 2) { bot.vehicle = null; boat.passengers = [] } })
  h.bot.vehicle = h.boat
  h.boat.passengers = [h.player]
  assert.equal((await h.runtime.long.boat_dismount({ id: 2 })).dismounted, 2)
  assert.equal(h.waits, 2)
})
