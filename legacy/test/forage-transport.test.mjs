import test from 'node:test'
import assert from 'node:assert/strict'
import { forageTransportOptions, waterFrontiers, nearbyDryLandings, createForageTransport } from '../src/forage-transport.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
import search from '../library/forage/search.mjs'
import { createRequire } from 'node:module'

class RouteError extends Error {}
const navigation = {
  BoatRouteError: RouteError,
  boatSurface: (block, x, z) => block(x, 63, z)?.name === 'water' ? { x: x + 0.5, y: 63, z: z + 0.5 } : null,
  checkedBoatRoute: (block, from, to) => ({ from, to, distance: Math.hypot(to.x - from.x, to.z - from.z) })
}
const shore = (x, y) => ({ name: y === 63 ? x <= 0 ? 'stone' : 'water' : 'air', solid: y === 63 && x <= 0 })
function fixture ({ mounted = null, ready = true, passengers = [], landingConfirmed = true } = {}) {
  let pos = { x: 0.5, y: 64, z: 0.5 }
  const boat = { id: 7, position: { x: 1.8, y: 63.6, z: 0.5 }, yaw: Math.PI / 2, passengers, leashHolderId: null, controller: mounted === 7 ? { id: 1 } : null }
  const calls = [], reports = []
  const api = { block: shore, pos: () => pos, report: value => reports.push(value), async act (name, args) {
    calls.push({ name, args })
    if (name === 'boat_state') return { selfId: 1, mounted, goalTravel: ready, boats: [boat] }
    if (name === 'boat_drive') { mounted = 7; boat.controller = { id: 1 }; boat.position = { x: args.x, y: args.y, z: args.z }; pos = { ...boat.position, y: args.y + 1 }; return {} }
    if (name === 'boat_land') { if (landingConfirmed) { mounted = null; boat.controller = null; pos = { x: args.x + 0.5, y: args.y, z: args.z + 0.5 } } return {} }
    throw new Error(`unexpected action ${name}`)
  } }
  return { api, calls, reports, boat }
}

test('transport defaults to walking and validates explicit authorization before motion', () => {
  assert.deepEqual(forageTransportOptions({}), { transport: 'walk', boat: undefined })
  for (const args of [{ transport: 'horse' }, { transport: 'boat' }, { transport: 'auto', boat: 1.5 }, { boat: 7 }, { transport: 'boat', boat: 7, pattern: 'spiral' }]) assert.throws(() => forageTransportOptions(args))
  assert.deepEqual(forageTransportOptions({ transport: 'auto', boat: 7 }), { transport: 'auto', boat: 7 })
})
test('water frontiers use authoritative surfaces and route checks, never guess unloaded water', () => {
  let checked = 0
  const result = waterFrontiers((x, y, z) => x === 8 ? null : shore(x, y, z), { x: 2.5, y: 63, z: 0.5 }, [{ x: 8, z: 0 }, { x: 12, z: 0 }], (block, from, to) => { checked++; return navigation.checkedBoatRoute(block, from, to) }, navigation.boatSurface, RouteError)
  assert.equal(checked, 1)
  assert.deepEqual(result.routes[0].goal, { x: 12.5, y: 63, z: 0.5 })
  assert.equal(result.failures.length, 1)
  assert.throws(() => waterFrontiers(shore, { x: 2.5, y: 63, z: 0.5 }, [{ x: 12, z: 0 }], () => { throw new TypeError('bug') }, navigation.boatSurface, RouteError), /bug/)
})
test('dry landing candidates exclude deep water, hazards and unloaded headroom', () => {
  assert.ok(nearbyDryLandings(shore, { position: { x: 1.8, y: 63.6, z: 0.5 }, yaw: Math.PI / 2 }).length)
  assert.equal(nearbyDryLandings(() => null, { position: { x: 1.8, y: 63.6, z: 0.5 }, yaw: Math.PI / 2 }).length, 0)
  assert.equal(nearbyDryLandings((x, y) => ({ name: y === 63 ? 'magma_block' : 'air', solid: y === 63 }), { position: { x: 1.8, y: 63.6, z: 0.5 }, yaw: Math.PI / 2 }).length, 0)
})
test('boat exploration stays mounted across legs and has no walking or legacy mount calls', async () => {
  const f = fixture()
  const session = await createForageTransport(f.api, { transport: 'boat', boat: 7 }, navigation)
  const frontier = await session.frontiers([{ x: 12, z: 0 }])
  await session.move(frontier.routes[0].goal)
  assert.equal(session.status().mounted, 7)
  assert.equal(await session.land(), false)
  assert.ok(f.calls.every(call => ['boat_state', 'boat_drive'].includes(call.name)))
})
test('walking fallback is allowed only on foot; unavailable explicit boats fail clearly', async () => {
  const f = fixture({ ready: false })
  const session = await createForageTransport(f.api, { transport: 'auto', boat: 7 }, navigation)
  assert.equal((await session.frontiers([{ x: 12, z: 0 }])).fallback, true)
  assert.equal(session.status().mounted, null)
  await assert.rejects(createForageTransport(f.api, { transport: 'boat', boat: 7 }, navigation), /controller is unavailable/)
  await assert.rejects(createForageTransport(fixture({ mounted: 9 }).api, { transport: 'auto', boat: 7 }, navigation), /mounted vehicle 9/)
  await assert.rejects(createForageTransport(fixture({ passengers: [{ id: 2 }] }).api, { transport: 'boat', boat: 7 }, navigation), /empty/)
})
test('landing requires server-confirmed dismount and settled dry footing', async () => {
  const f = fixture({ mounted: 7 })
  const session = await createForageTransport(f.api, { transport: 'boat', boat: 7 }, navigation)
  assert.equal(await session.land(), true)
  assert.equal(session.status().mounted, null)
  assert.equal(f.calls.filter(call => call.name === 'boat_land').length, 1)
  const denied = await createForageTransport(fixture({ mounted: 7, landingConfirmed: false }).api, { transport: 'boat', boat: 7 }, navigation)
  await assert.rejects(denied.land(), /did not confirm dismount/)
})
test('safety interruption during boarding preserves an unconfirmed mounted report', async () => {
  const f = fixture()
  const original = f.api.act
  f.api.act = async (name, args) => name === 'boat_drive' ? { stopped: 'health' } : original(name, args)
  const session = await createForageTransport(f.api, { transport: 'boat', boat: 7 }, navigation)
  await assert.rejects(session.move({ x: 12.5, y: 63, z: 0.5 }), CompositeHandBack)
  assert.equal(session.status().mounted, 'unconfirmed')
  assert.equal(f.reports.at(-1).mounted, 'unconfirmed')
})

test('boat scan-only and sufficient local findings do not inspect or board vehicles', async () => {
  for (const args of [{ steps: 0 }, { steps: 20 }]) {
    const calls = []
    const api = { pos: () => ({ x: 0.5, y: 64, z: 0.5 }), checkpoint: async () => {}, report: () => {}, async act (name) {
      calls.push(name)
      return { positions: args.steps === 0 ? [] : [{ x: 2, y: 64, z: 2 }] }
    } }
    const result = await search.run(api, { block: 'cactus', transport: 'boat', boat: 7, ...args })
    assert.deepEqual(calls, ['find_blocks'])
    assert.match(result.resume, /transport=boat boat=7/)
    assert.equal(result.mounted, 'unchecked')
  }
})
test('expired initial scan budget never starts a boat trip', async () => {
  const calls = []
  const api = { pos: () => ({ x: 0.5, y: 64, z: 0.5 }), checkpoint: async () => {}, report: () => {}, async act (name) {
    calls.push(name)
    await new Promise(resolve => setTimeout(resolve, 10))
    return { positions: [] }
  } }
  const result = await search.run(api, { block: 'cactus', transport: 'boat', boat: 7, minutes: 0.00001 })
  assert.deepEqual(calls, ['find_blocks'])
  assert.equal(result.reason, 'time budget exhausted')
})
test('transport setup uses the original expedition budget before any movement', async () => {
  const calls = []
  const api = { pos: () => ({ x: 0.5, y: 64, z: 0.5 }), block: shore, checkpoint: async () => {}, report: () => {}, async act (name) {
    calls.push(name)
    if (name === 'find_blocks') return { positions: [] }
    assert.equal(name, 'boat_state')
    await new Promise(resolve => setTimeout(resolve, 20))
    return { mounted: null, goalTravel: false, boats: [] }
  } }
  const result = await search.run(api, { block: 'cactus', transport: 'auto', minutes: 0.0001 })
  assert.deepEqual(calls, ['find_blocks', 'boat_state'])
  assert.equal(result.reason, 'time budget exhausted')
  assert.equal(result.mounted, null)
})

test('fractional shore approaches use checked hull routes and reachable server landing points', async () => {
  const { boatShoreApproaches } = await import('../src/forage-transport.mjs')
  const cube = { name: 'stone', solid: true, shapes: [[0, 0, 0, 1, 1, 1]] }
  const world = (x, y, z) => y === 63 ? x >= 1 ? cube : { name: 'water', shapes: [] } : { name: 'air', shapes: [] }
  const boat = { position: { x: -1.5, y: 63.5, z: 0.5 }, yaw: -Math.PI / 2, width: 1.375, height: 0.5625 }
  let checked = 0
  const navigation = { checkedBoatRoute (blockAt, from, to) {
    checked++
    if (Math.max(from.x, to.x) + 0.6875 >= 1) throw new Error('hull meets shore')
    return { from, to }
  } }
  const candidates = boatShoreApproaches(world, boat, navigation)
  assert.ok(checked > 0)
  assert.ok(candidates.length > 0)
  assert.ok(candidates.every(c => !Number.isInteger(c.goal.x * 2)), 'approaches are not limited to block centres')
  assert.deepEqual(candidates[0].exit, { x: 1, y: 64, z: 0 })
  assert.ok(candidates[0].goal.x > 0.22 && candidates[0].goal.x < 0.23)
  assert.deepEqual(boatShoreApproaches(world, boat, { checkedBoatRoute () { throw new Error('route blocked') } }), [])
})

test('search uses real source-water geometry for exploration and a checked fractional shore return', async () => {
  const require = createRequire(import.meta.url)
  const Block = require('prismarine-block')(require('minecraft-data')('26.1'))
  const cell = name => {
    const b = Block.fromProperties(name, name === 'water' ? { level: 0 } : {}, 0)
    return { name: b.name, solid: b.boundingBox === 'block', shapes: b.shapes, properties: b.getProperties() }
  }
  const water = cell('water'), stone = cell('stone'), air = cell('air')
  const world = (x, y) => y < 63 || y === 63 && x <= 0 ? stone : y === 63 ? water : air
  const f = fixture()
  f.api.block = world
  f.boat.position.y = 63 + Math.fround(8 / 9) - 0.5625 * 0.65
  f.api.checkpoint = async () => {}
  const action = f.api.act
  let scans = 0
  f.api.act = async (name, args) => {
    if (name === 'find_blocks') return { positions: ++scans === 1 ? [] : [{ x: 8, y: 64, z: 0 }] }
    if (name === 'boat_drive') f.boat.yaw = Math.atan2(f.boat.position.x - args.x, f.boat.position.z - args.z)
    return action(name, args)
  }
  const result = await search.run(f.api, { block: 'cactus', transport: 'boat', boat: 7, heading: 'east', spacing: 4, steps: 1 })
  assert.equal(result.complete, true)
  assert.equal(scans, 2)
  assert.equal(result.mounted, null)
  assert.equal(f.calls.filter(c => c.name === 'boat_drive').length, 2, 'exploration followed by a fractional shore approach')
  assert.equal(f.calls.filter(c => c.name === 'boat_land').length, 1)
  assert.ok(!f.calls.some(c => c.name === 'goto'), 'no walking while aboard')
})
