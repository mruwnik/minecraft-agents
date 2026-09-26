import { test } from 'node:test'
import assert from 'node:assert/strict'
import board from '../library/villager/board.mjs'
import dock from '../library/villager/dock.mjs'
import ferry from '../library/villager/ferry.mjs'
import undock from '../library/villager/undock.mjs'
import routeCheck from '../library/villager/route.mjs'
import { villagerBoatRoute, villagerDockPlan } from '../src/lib.mjs'

const uuid = 'c071f7d4-8b43-4f01-9c2f-92b648d3d143'
const otherUuid = '87b3392e-ae93-4f51-bf07-2f53add88880'
const villager = (id, uuid, exact = '0,64,0', metadata = '{}') => ({ id, uuid, name: 'villager', exact, metadata })

function boardApi ({ entities = [villager(41, uuid)], states = [], boatId = 8 } = {}) {
  const calls = []
  const reports = []
  let stateIndex = 0
  const api = {
    me: () => 'test-bot',
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'entity') return { found: entities }
      if (name === 'boat_place') return { boat: { id: boatId } }
      if (name === 'boat_state') return states[Math.min(stateIndex++, states.length - 1)] ?? { boats: [] }
      throw new Error(`unexpected action ${name}`)
    },
    until: async (predicate, options) => {
      assert.equal(options.timeout, 30)
      assert.equal(options.every, 0.5)
      if (!(await predicate())) throw new Error('boarding timeout')
    },
    report: value => reports.push(value)
  }
  return { api, calls, reports }
}

test('villager.board places once and waits for the exact UUID before confirming capture', async () => {
  const { api, calls, reports } = boardApi({
    entities: [villager(41, otherUuid), villager(42, uuid, '4,64,-2')],
    states: [
      { boats: [{ id: 8, passengers: [] }] },
      { boats: [{ id: 8, passengers: [{ id: 42, uuid, name: 'villager' }] }] }
    ]
  })

  const result = await board.run(api, { uuid, item: 'oak_boat', x: 4, y: 63, z: -2, timeout: 30 })

  assert.deepEqual(result, { boat: 8, villagerUuid: uuid, boarded: true })
  assert.equal(calls.filter(c => c.name === 'boat_place').length, 1)
  assert.deepEqual(calls.find(c => c.name === 'boat_place').args, { item: 'oak_boat', x: 4, y: 63, z: -2 })
  assert.equal(calls.filter(c => c.name === 'boat_state').length, 2)
  assert.deepEqual(reports.at(-1), { boarding: 'complete' })
})

test('villager.board reuses an existing boat and refuses another UUID already aboard', async () => {
  const { api, calls } = boardApi({
    states: [{ boats: [{ id: 8, passengers: [{ id: 91, uuid: otherUuid, name: 'villager' }] }] }]
  })

  await assert.rejects(board.run(api, { uuid, boat: 8 }), /already carries somebody other than villager/)
  assert.equal(calls.some(c => c.name === 'boat_place'), false)
  assert.equal(calls.filter(c => c.name === 'boat_state').length, 1)
})

test('villager.board refuses an unobserved or mismatched entity ID before placing a boat', async () => {
  for (const target of [[], [villager(41, uuid)]]) {
    const { api, calls } = boardApi({ entities: target })
    await assert.rejects(board.run(api, { uuid, id: 42, item: 'oak_boat', x: 0, y: 63, z: 0 }), /not observed with the requested ID/)
    assert.equal(calls.some(c => c.name === 'boat_place'), false)
  }
})

test('villager.board refuses a baby before placing a boat', async () => {
  const { api, calls } = boardApi({ entities: [villager(41, uuid, '0,64,0', '{"16":true}')] })
  await assert.rejects(board.run(api, { uuid, item: 'oak_boat', x: 0, y: 63, z: 0 }), /is a baby/)
  assert.equal(calls.some(c => c.name === 'boat_place'), false)
})

test('villager.board fails if a second passenger enters during capture', async () => {
  const { api } = boardApi({
    states: [
      { boats: [{ id: 8, passengers: [] }] },
      { boats: [{ id: 8, passengers: [{ id: 41, uuid, name: 'villager' }, { id: 99, uuid: otherUuid, name: 'villager' }] }] }
    ]
  })
  await assert.rejects(board.run(api, { uuid, item: 'oak_boat', x: 0, y: 63, z: 0, timeout: 30 }), /another passenger entered/)
})

function ferryApi ({ boatUuid = uuid, leashHolderId = 12, attach = false, loseLeashAfterGoto = false, terrainAt } = {}) {
  const calls = []
  const reports = []
  let position = { x: 0, y: 64, z: 0 }
  let leash = attach ? null : leashHolderId
  let lose = false
  const api = {
    pos: () => ({ ...position }),
    block: (x, y, z) => terrainAt ? terrainAt(x, y, z) : { name: y <= 63 ? 'stone' : 'air', solid: y <= 63 },
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'state') return { inWater: false }
      if (name === 'boat_state') return {
        selfId: 12,
        mounted: null,
        boats: [{
          id: 8,
          exact: position.x + ',' + position.y + ',' + position.z,
          leashHolderId: lose ? null : leash,
          passengers: [{ id: 42, uuid: boatUuid, name: 'villager' }]
        }]
      }
      if (name === 'boat_leash') { leash = 12; return { leashed: 8 } }
      if (name === 'goto') {
        position = { x: Math.floor(args.x) + 0.5, y: args.y, z: Math.floor(args.z) + 0.5 }
        if (loseLeashAfterGoto) lose = true
        return {}
      }
      throw new Error('unexpected action ' + name)
    },
    until: async (predicate, options) => {
      assert.equal(options.timeout, 12)
      assert.equal(options.every, 0.25)
      if (!(await predicate())) throw new Error('boat did not follow')
    },
    report: value => reports.push(value)
  }
  return { api, calls, reports }
}

test('villager.ferry attaches a lead if needed and tows the same UUID in short walking steps', async () => {
  const { api, calls, reports } = ferryApi({ attach: true })
  const result = await ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0 })

  assert.equal(result.villagerUuid, uuid)
  assert.equal(result.leashHeld, true)
  assert.equal(result.secure, false)
  assert.equal(calls.filter(c => c.name === 'boat_leash').length, 1)
  const steps = calls.filter(c => c.name === 'goto')
  assert.ok(steps.length >= 2)
  assert.ok(steps.every(c => c.args.range === 0))
  assert.deepEqual(steps.at(-1).args, { x: 7.5, y: 64, z: 0, range: 0 })
  assert.equal(calls.some(c => c.name === 'boat_move' || c.name === 'boat_mount'), false)
  assert.ok(reports.at(-1).tow.includes('/'))
  assert.equal(reports.at(-1).boatAt, result.at)
})

test('villager.ferry alignment is a read-only plan check and refuses a misaligned boat before attachment or movement', async () => {
  const planned = ferryApi({ attach: true })
  const plan = await ferry.run(planned.api, { uuid, boat: 8, x: 7, y: 64, z: 0, alignZ: 1, plan: true })
  assert.equal(plan.aligned, false)
  assert.equal(planned.calls.some(c => c.name === 'boat_leash' || c.name === 'goto'), false)

  const misaligned = ferryApi({ attach: true })
  await assert.rejects(ferry.run(misaligned.api, { uuid, boat: 8, x: 7, y: 64, z: 0, alignZ: 1 }), /needs water staging.*no towing started/)
  assert.equal(misaligned.calls.some(c => c.name === 'boat_leash' || c.name === 'goto'), false, 'misalignment refuses before attaching the lead or walking')

  const aligned = ferryApi()
  await ferry.run(aligned.api, { uuid, boat: 8, x: 7, y: 64, z: 0, alignZ: 0 })
  assert.ok(aligned.calls.some(c => c.name === 'goto'), 'a boat within the requested z tolerance can proceed')
})

test('villager.ferry accepts dry arrival at the requested block cell despite fractional waypoint distance', async () => {
  const { api, calls } = ferryApi()
  const ordinaryPos = api.pos
  const ordinaryAct = api.act
  let cellPosition = null
  api.pos = () => cellPosition ?? ordinaryPos()
  api.act = async (name, args) => {
    const result = await ordinaryAct(name, args)
    if (name === 'goto' && Math.floor(args.x) === 7 && args.y === 64 && Math.floor(args.z) === 0) {
      cellPosition = { x: 7.99, y: 64.99, z: 0.99 }
    }
    return result
  }
  const result = await ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0 })
  const lastGoto = calls.filter(c => c.name === 'goto').at(-1)
  assert.ok(lastGoto)
  assert.equal(lastGoto.args.range, 0)
  assert.deepEqual([Math.floor(api.pos().x), Math.floor(api.pos().y), Math.floor(api.pos().z)], [7, 64, 0])
  assert.ok(Math.hypot(api.pos().x - lastGoto.args.x, api.pos().y - lastGoto.args.y, api.pos().z - lastGoto.args.z) > 1, 'the body can be more than one block from a fractional waypoint while inside its requested dry cell')
  assert.equal(result.at, '7.5,64,0.5', 'the boat itself still reaches the requested landing')
})

test('villager.ferry never substitutes a nearby villager for the requested UUID', async () => {
  const { api, calls } = ferryApi({ boatUuid: otherUuid, attach: true })
  await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /not in boat 8/)
  assert.equal(calls.some(c => c.name === 'goto' || c.name === 'boat_leash'), false)
})

test('villager.ferry stops at the first step if the leash breaks', async () => {
  const { api, calls } = ferryApi({ loseLeashAfterGoto: true })
  await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /leash broke during tow/)
  assert.equal(calls.filter(c => c.name === 'goto').length, 1)
  assert.equal(calls.some(c => c.name === 'boat_move'), false)
})

test('villager.ferry refuses a boat controlled by this bot', async () => {
  const { api, calls } = ferryApi()
  const state = api.act
  api.act = async (name, args) => name === 'boat_state'
    ? { selfId: 12, mounted: 8, boats: [{ id: 8, exact: '0,64,0', leashHolderId: 12, passengers: [{ id: 42, uuid, name: 'villager' }] }] }
    : state(name, args)
  await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /dismount boat 8 before towing/)
  assert.equal(calls.some(c => c.name === 'goto'), false)
})

test('villager.ferry requires the boat itself, not just the bot, to reach the named landing', async () => {
  const { api, calls } = ferryApi()
  const original = api.act
  api.act = async (name, args) => {
    if (name === 'boat_state') return {
      selfId: 12,
      mounted: null,
      boats: [{ id: 8, exact: '2,64,0', leashHolderId: 12, passengers: [{ id: 42, uuid, name: 'villager' }] }]
    }
    return original(name, args)
  }

  await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /boat did not follow|did not reach the landing/)
  assert.ok(calls.filter(c => c.name === 'goto').length > 0)
})

test('villager.ferry walks past the boat landing until the boat follows at natural leash slack', async () => {
  const calls = []
  let position = { x: 0, y: 64, z: 0 }
  let boatX = 0.5
  const api = {
    pos: () => ({ ...position }),
    block: (x, y) => ({ name: y <= 63 ? 'stone' : 'air', solid: y <= 63 }),
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'state') return { inWater: false }
      if (name === 'boat_state') return {
        selfId: 12,
        mounted: null,
        boats: [{ id: 8, exact: `${boatX},64,0`, leashHolderId: 12, passengers: [{ id: 42, uuid, name: 'villager' }] }]
      }
      if (name === 'goto') {
        position = { x: args.x + 0.5, y: args.y, z: args.z + 0.5 }
        boatX = args.x === 3 ? 0.5 : args.x === 6 ? 0.5 : args.x === 9 ? 3.5 : 7.5
        return {}
      }
      throw new Error(`unexpected action ${name}`)
    },
    until: async (predicate, options) => {
      assert.equal(options.timeout, 12)
      assert.equal(options.every, 0.25)
      if (!(await predicate())) throw new Error('boat did not follow')
    },
    report: () => {}
  }

  const result = await ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0, pullX: 13, pullY: 64, pullZ: 0 })
  assert.equal(result.at, '7.5,64,0')
  assert.equal(result.pullAt, '13,64,0')
  const xSteps = calls.filter(c => c.name === 'goto').map(c => c.args.x)
  assert.ok(xSteps.length >= 4)
  assert.equal(xSteps.at(-1), 13)
  assert.ok(xSteps.every((x, i) => i === 0 || x >= xSteps[i - 1]))
})

test('villager.ferry stops before another waypoint while the boat remains near leash snap distance', async () => {
  const calls = []
  let position = { x: 0, y: 64, z: 0 }
  const api = {
    pos: () => ({ ...position }),
    block: (x, y) => ({ name: y <= 63 ? 'stone' : 'air', solid: y <= 63 }),
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'state') return { inWater: false }
      if (name === 'boat_state') return {
        selfId: 12,
        mounted: null,
        boats: [{ id: 8, exact: '-6.5,64,0', leashHolderId: 12, passengers: [{ id: 42, uuid, name: 'villager' }] }]
      }
      if (name === 'goto') { position = { x: args.x + 0.5, y: args.y, z: args.z + 0.5 }; return {} }
      throw new Error(`unexpected action ${name}`)
    },
    until: async predicate => { if (!(await predicate())) throw new Error('boat did not follow') },
    report: () => {}
  }

  await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 16, y: 64, z: 0, pullX: 22, pullY: 64, pullZ: 0 }), /boat did not follow|fell too far behind/)
  assert.equal(calls.filter(c => c.name === 'goto').length, 1)
})

function dryCatchupApi ({ loseLeashDuringWait = false } = {}) {
  const calls = []
  let position = { x: 0, y: 64, z: 0 }
  let boatX = -6.5
  let gotoCount = 0
  let postGotoStates = 0
  let waitReads = 0
  let leash = 12
  const api = {
    pos: () => ({ ...position }),
    block: (x, y) => ({ name: y <= 63 ? 'stone' : 'air', solid: y <= 63 }),
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'state') return { inWater: false }
      if (name === 'boat_state') {
        if (gotoCount === 1) {
          postGotoStates++
          if (loseLeashDuringWait && postGotoStates >= 2) leash = null
        }
        return { selfId: 12, mounted: null, boats: [{ id: 8, exact: `${boatX},64,0`, leashHolderId: leash, passengers: [{ id: 42, uuid, name: 'villager' }] }] }
      }
      if (name === 'goto') {
        gotoCount++
        if (gotoCount > 1) throw new Error('observed next waypoint after catch-up')
        position = { x: args.x + 0.5, y: args.y, z: args.z + 0.5 }
        return {}
      }
      throw new Error(`unexpected action ${name}`)
    },
    until: async (predicate, options) => {
      assert.equal(options.timeout, 12)
      assert.equal(options.every, 0.25)
      for (let i = 0; i < 12; i++) {
        if (await predicate()) return
        waitReads++
        if (!loseLeashDuringWait && waitReads === 3) boatX = position.x - 6
      }
      throw new Error(options.what)
    },
    report: () => {}
  }
  return { api, calls, get waitReads () { return waitReads }, get gotoCount () { return gotoCount } }
}

test('villager.ferry waits for a lagging dry tow boat before issuing the next goto waypoint', async () => {
  const run = dryCatchupApi()
  await assert.rejects(ferry.run(run.api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /observed next waypoint after catch-up/)
  assert.ok(run.waitReads >= 3, 'the dry leg holds and checks boat state while the lead catches up')
  assert.equal(run.gotoCount, 2, 'the second goto starts only after the lagging boat enters the safe gap')
  const gotos = run.calls.map((c, i) => c.name === 'goto' ? i : -1).filter(i => i >= 0)
  assert.ok(run.calls.slice(gotos[0] + 1, gotos[1]).some(c => c.name === 'boat_state'), 'fresh boat observations happen between the two walking legs')
})

test('villager.ferry stops during dry catch-up if the leash breaks before the next goto', async () => {
  const run = dryCatchupApi({ loseLeashDuringWait: true })
  await assert.rejects(ferry.run(run.api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /leash broke during tow/)
  assert.equal(run.gotoCount, 1, 'no new walking step follows a leash loss during catch-up')
  assert.ok(run.waitReads <= 1)
})

function landingSettleApi (mode = 'delayed') {
  const calls = []
  let position = { x: 0, y: 64, z: 0 }
  let boatOffset = 0
  let leash = 12
  let settling = false
  let settleReads = 0
  const api = {
    pos: () => ({ ...position }),
    block: (x, y) => ({ name: y <= 63 ? 'stone' : 'air', solid: y <= 63 }),
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'state') return { inWater: false }
      if (name === 'boat_state') {
        if (settling) {
          settleReads++
          if (mode === 'leash') leash = null
          if (mode === 'delayed' && settleReads >= 3) boatOffset = 0
        }
        return { selfId: 12, mounted: null, boats: [{ id: 8, exact: `${position.x - boatOffset},${position.y},${position.z}`, leashHolderId: leash, passengers: [{ id: 42, uuid, name: 'villager' }] }] }
      }
      if (name === 'goto') {
        position = { x: Math.floor(args.x) + 0.5, y: args.y, z: Math.floor(args.z) + 0.5 }
        if (Math.floor(args.x) === 7 && Math.floor(args.z) === 0) boatOffset = 2
        return {}
      }
      throw new Error(`unexpected action ${name}`)
    },
    until: async (predicate, options) => {
      assert.equal(options.timeout, 12)
      assert.equal(options.every, 0.25)
      if (options.what.includes('stayed outside the landing')) settling = true
      for (let i = 0; i < 8; i++) if (await predicate()) return
      throw new Error(options.what)
    },
    report: () => {}
  }
  return { api, calls, get settleReads () { return settleReads } }
}

test('villager.ferry waits for a delayed boat arrival inside the final landing radius', async () => {
  const run = landingSettleApi('delayed')
  const result = await ferry.run(run.api, { uuid, boat: 8, x: 7, y: 64, z: 0 })
  assert.ok(run.settleReads >= 3, 'final arrival requires fresh boat position feedback after the last walking step')
  const [x, y, z] = result.at.split(',').map(Number)
  assert.ok(Math.hypot(x - 7.5, y - 64, z - 0.5) <= 1, 'success reports the boat inside the landing radius')
})

test('villager.ferry refuses a stationary outside-radius landing and a leash loss while settling', async () => {
  const outside = landingSettleApi('never')
  await assert.rejects(ferry.run(outside.api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /stayed outside the landing/)
  assert.ok(outside.settleReads >= 2, 'the command waits for actual boat arrival instead of trusting the operator position')

  const lost = landingSettleApi('leash')
  await assert.rejects(ferry.run(lost.api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /leash broke while settling at the landing/)
  assert.equal(lost.settleReads, 1)
})

function routeTerrain (floorYAt = () => 63, obstructed = () => false, unloaded = () => false) {
  return (x, y, z) => {
    if (unloaded(x, y, z)) return null
    if (obstructed(x, y, z)) return { name: 'stone', solid: true }
    const floorY = floorYAt(x, z)
    return y === floorY ? { name: 'stone', solid: true } : { name: 'air', solid: false }
  }
}

test('boat route accepts a monotonic descent with a turn and returns the checked hull path', () => {
  const blockAt = routeTerrain((x, z) => x + z < 3 ? 63 : x + z < 6 ? 62 : 61)
  const route = villagerBoatRoute({ from: { x: 0.5, y: 64, z: 0.5 }, to: { x: 4.5, y: 62, z: 4.5 }, blockAt, margin: 1 })
  assert.ok(route.points, JSON.stringify(route))
  assert.equal(route.points[0].x, 0.5)
  assert.equal(route.points[0].z, 0.5)
  assert.equal(route.points.at(-1).x, 4.5)
  assert.equal(route.points.at(-1).z, 4.5)
  assert.ok(route.points.some((p, i) => i > 0 && p.x !== route.points[i - 1].x))
  assert.ok(route.points.some((p, i) => i > 0 && p.z !== route.points[i - 1].z))
  assert.ok(route.points.every((p, i) => i === 0 || p.x === route.points[i - 1].x || p.z === route.points[i - 1].z), 'every hull step must be checked before turning')
  assert.ok(route.points.every((p, i) => i === 0 || p.y <= route.points[i - 1].y))
})

test('boat route shifts a quantized start through a checked connector when rounding clips the measured hull', () => {
  const blockAt = (x, y, z) => {
    if (y === 62 && x >= -132 && x <= -126 && z >= -169 && z <= -163) return { name: 'water', solid: false }
    if (y === 61 && x >= -132 && x <= -126 && z >= -169 && z <= -163) return { name: 'stone', solid: true }
    return { name: 'air', solid: false }
  }
  const route = villagerBoatRoute({
    from: { x: -131.31, y: 62.41, z: -168.31 },
    to: { x: -127.5, y: 62, z: -165.5 },
    blockAt,
    margin: 1
  })
  assert.ok(route.points, JSON.stringify(route))
  assert.deepEqual(route.points[0], { x: -131, y: 62.5, z: -168 })
  assert.ok(route.points.at(-1).x === -127.5 && route.points.at(-1).z === -165.5)
})

test('boat route rejects an uphill hull lip even when a player could walk up it', () => {
  const blockAt = routeTerrain(x => x < 2 ? 63 : 64)
  const route = villagerBoatRoute({ from: { x: 0.5, y: 64, z: 0.5 }, to: { x: 4.5, y: 65, z: 0.5 }, blockAt, margin: 1 })
  assert.equal(route.points, undefined)
  assert.match(route.error, /upward|nonascending/)
})

test('boat route rejects a one-cell slit and any unloaded terrain in the hull footprint', () => {
  const slit = villagerBoatRoute({
    from: { x: 0.5, y: 64, z: 0.5 },
    to: { x: 2.5, y: 64, z: 0.5 },
    blockAt: routeTerrain(() => 63, (_x, y, z) => y >= 64 && y <= 65 && (z === -1 || z === 1)),
    margin: 0.5
  })
  assert.equal(slit.points, undefined)
  assert.match(slit.error, /hull blocked|upward lip/)

  const unknown = villagerBoatRoute({
    from: { x: 0.5, y: 64, z: 0.5 },
    to: { x: 2.5, y: 64, z: 0.5 },
    blockAt: routeTerrain(() => 63, () => false, (x, y, z) => x === -1 && y === 64 && z === 0),
    margin: 0.5
  })
  assert.equal(unknown.points, undefined)
  assert.match(unknown.error, /unloaded/)
})

test('villager.ferry rejects the complete uphill boat route before attaching a lead or walking', async () => {
  const { api, calls } = ferryApi({ terrainAt: routeTerrain(x => x < 2 ? 63 : 64) })
  await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 7, y: 65, z: 0 }), /boat route blocked.*upward|boat route blocked.*nonascending/)
  assert.equal(calls.some(c => c.name === 'boat_leash' || c.name === 'goto'), false)
})

test('villager.ferry refuses a direct pull that cuts a corner even when the hull route can detour around it', async () => {
  const blockAt = (x, y, z) => {
    if (y <= 63) return { name: 'stone', solid: true }
    if (x === 2 && z === 0 && y === 64) return { name: 'stone', solid: true }
    return { name: 'air', solid: false }
  }
  const from = { x: 0.5, y: 64, z: 0.5 }
  const landing = { x: 7.5, y: 64, z: 0.5 }
  const planned = villagerBoatRoute({ from, to: landing, blockAt })
  assert.ok(planned.points?.length > 2, 'the full planner can route around the obstacle')
  const direct = villagerBoatRoute({ from, to: landing, direct: true, blockAt })
  assert.match(direct.error, /hull blocked|upward boat step/, 'the straight hull sweep through the same corner is blocked')

  const calls = []
  let body = { x: 0, y: 64, z: 0 }
  const boat = { ...from }
  let leash = 12
  let leads = 0
  const api = {
    pos: () => ({ ...body }),
    block: blockAt,
    inv: () => ({ lead: leads }),
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'state') return { inWater: false }
      if (name === 'boat_state') return { selfId: 12, mounted: null, boats: [{ id: 8, exact: `${boat.x},${boat.y},${boat.z}`, leashHolderId: leash, passengers: [{ id: 42, uuid, name: 'villager' }] }] }
      if (name === 'goto') { body = { x: Math.floor(args.x) + 0.5, y: args.y, z: Math.floor(args.z) + 0.5 }; return {} }
      if (name === 'boat_unleash') { leash = null; leads++; return { unleashed: true } }
      throw new Error(`unexpected action ${name}`)
    },
    until: async predicate => { if (!(await predicate())) throw new Error('boat did not follow') },
    report: () => {}
  }
  await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0 }), /actual boat pull blocked.*lead detached.*recover the dropped lead/)
  const lastGoto = calls.findLastIndex(c => c.name === 'goto')
  assert.ok(lastGoto >= 0, 'the walker may follow the surveyed detour')
  assert.equal(calls.slice(lastGoto + 1).some(c => c.name === 'goto'), false, 'the blocked direct hull pull is refused before any following walking step')
  assert.equal(calls.filter(c => c.name === 'boat_unleash').length, 1, 'release only the exact verified boat after the unsafe pull is found')
  assert.equal(leads, 1, 'the fake records recovery of the detached lead')
})

test('boat route rejects a sheer drop that would strand or break the hull', () => {
  const route = villagerBoatRoute({
    from: { x: 0.5, y: 68, z: 0.5 },
    to: { x: 5.5, y: 62, z: 0.5 },
    blockAt: routeTerrain(x => x < 2 ? 67 : 61),
    margin: 1
  })
  assert.equal(route.points, undefined)
  assert.match(route.error, /unsafe boat drop/)
})

test('villager.ferry plan mode reads the full boat route without attaching a lead or walking', async () => {
  const { api, calls } = ferryApi({ attach: true })
  const result = await ferry.run(api, { uuid, boat: 8, x: 7, y: 64, z: 0, plan: true })
  assert.equal(result.planned, true)
  assert.ok(result.route.length > 1)
  assert.equal(calls.some(c => c.name === 'boat_leash' || c.name === 'goto'), false)
})

test('villager.ferry accepts an exact two-wide gate landing center inside its named cell', async () => {
  const { api, calls } = ferryApi()
  const result = await ferry.run(api, {
    uuid, boat: 8, x: 7, y: 64, z: 0,
    centerX: 7, centerZ: 0, plan: true
  })
  assert.equal(result.planned, true)
  assert.deepEqual(result.route.at(-1), { x: 7, y: 64, z: 0 })
  assert.equal(calls.some(c => c.name === 'boat_leash' || c.name === 'goto'), false)
})

test('villager.ferry preflights the observed water float height against a water-cell landing', async () => {
  const calls = []
  const water = (x, y) => y === 62
    ? { name: 'water', solid: false }
    : y === 61
      ? { name: 'stone', solid: true }
      : { name: 'air', solid: false }
  const api = {
    pos: () => ({ x: 0.5, y: 62.52, z: 0.5 }),
    block: water,
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'boat_state') return {
        selfId: 12,
        mounted: null,
        boats: [{ id: 8, exact: '0.5,62.52,0.5', leashHolderId: null, passengers: [{ id: 42, uuid, name: 'villager' }] }]
      }
      throw new Error(`unexpected action ${name}`)
    },
    report: () => {}
  }
  const result = await ferry.run(api, { uuid, boat: 8, x: 4, y: 62, z: 0, plan: true })
  assert.equal(result.planned, true)
  assert.equal(result.route[0].y, 62.5)
  assert.equal(result.route.at(-1).y, 62.5)
  assert.equal(calls.some(c => c.name === 'boat_leash' || c.name === 'goto'), false)
})

function waterFerryApi (mode = 'progress') {
  let position = { x: 0.5, y: 62.52, z: 0.5 }
  let boatAt = { ...position }
  let leashHolderId = null
  let lostPassenger = false
  let checkFeet = false
  const calls = []
  const api = {
    pos: () => ({ ...position }),
    block: (x, y, z) => {
      if (checkFeet && x === Math.floor(position.x) && y === Math.floor(position.y - 0.5) && z === Math.floor(position.z)) {
        checkFeet = false
        const name = mode.slice('feet-'.length)
        return { name, solid: false }
      }
      if (mode === 'emerge' && position.x >= 2.5 && y === 62) return { name: 'stone', solid: true }
      if (mode === 'source-water' && x >= 6 && y === 62) return { name: 'stone', solid: true }
      return y === 62
        ? { name: 'water', solid: false }
      : y === 61
        ? { name: 'stone', solid: true }
        : { name: 'air', solid: false }
    },
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'state') {
        checkFeet = mode.startsWith('feet-')
        return { inWater: (mode !== 'emerge' || position.x < 2.5) && !mode.startsWith('feet-') }
      }
      if (name === 'boat_state') return {
        selfId: 12,
        mounted: null,
        boats: [{ id: 8, exact: `${boatAt.x},${boatAt.y},${boatAt.z}`, leashHolderId, passengers: lostPassenger ? [] : [{ id: 42, uuid, name: 'villager' }] }]
      }
      if (name === 'boat_leash') { leashHolderId = 12; return { leashed: 8 } }
      if (name === 'boat_swim') {
        if (mode !== 'stalled') {
          position = {
            x: ['emerge', 'source-water'].includes(mode) ? Math.min(args.x, position.x + 2) : args.x,
            y: 62.52,
            z: args.z
          }
          boatAt = { ...position }
        }
        if (mode === 'uuid-loss') lostPassenger = true
        if (mode === 'leash-loss') leashHolderId = null
        return { from: '0,62.52,0', to: `${position.x},${position.y},${position.z}` }
      }
      if (name === 'goto' && ['emerge', 'source-water'].includes(mode)) {
        position = { x: args.x, y: args.y, z: args.z }
        if (mode === 'emerge') boatAt = { x: args.x, y: 62.52, z: args.z }
        return {}
      }
      if (name === 'goto') throw new Error('water crossing should use boat_swim')
      throw new Error(`unexpected action ${name}`)
    },
    until: async (predicate, options) => {
      assert.equal(options.timeout, 12)
      if (!(await predicate())) throw new Error('boat did not follow')
    },
    report: () => {}
  }
  return { api, calls }
}

test('villager.ferry makes water progress with bounded swim strokes and verifies the passenger each stroke', async () => {
  const { api, calls } = waterFerryApi()
  const result = await ferry.run(api, { uuid, boat: 8, x: 4, y: 62, z: 0 })
  const strokes = calls.filter(c => c.name === 'boat_swim')
  assert.ok(strokes.length >= 2)
  assert.ok(strokes.every(c => c.args.ms === 700))
  assert.equal(calls.some(c => c.name === 'goto'), false)
  assert.ok(calls.filter(c => c.name === 'boat_state').length >= strokes.length + 2, 'inspect boat and passenger after every swim stroke')
  assert.equal(result.at, '4.5,62.52,0.5')
})

test('villager.ferry detects water and plants underfoot when state.inWater is false', async () => {
  for (const feet of ['water', 'bubble_column', 'seagrass', 'kelp']) {
    const { api, calls } = waterFerryApi(`feet-${feet}`)
    await ferry.run(api, { uuid, boat: 8, x: 4, y: 62, z: 0 })
    assert.ok(calls.some(c => c.name === 'boat_swim'), `${feet} should select boat_swim`)
  }
})

test('villager.ferry stops a water crossing after two swim strokes without progress', async () => {
  const { api, calls } = waterFerryApi('stalled')
  await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 4, y: 62, z: 0 }), /made no progress/)
  assert.equal(calls.filter(c => c.name === 'boat_swim').length, 2)
  assert.equal(calls.some(c => c.name === 'goto'), false)
})

test('villager.ferry stops a water crossing when its passenger UUID or leash is lost', async () => {
  for (const [mode, expected] of [['uuid-loss', /not in boat 8/], ['leash-loss', /leash broke during tow/]]) {
    const { api, calls } = waterFerryApi(mode)
    await assert.rejects(ferry.run(api, { uuid, boat: 8, x: 4, y: 62, z: 0 }), expected)
    assert.equal(calls.filter(c => c.name === 'boat_swim').length, 1)
    assert.equal(calls.some(c => c.name === 'goto'), false)
  }
})

test('villager.ferry switches from swimming to walking when the bot reaches dry land', async () => {
  const { api, calls } = waterFerryApi('emerge')
  await ferry.run(api, { uuid, boat: 8, x: 4, y: 62, z: 0 })
  const swims = calls.filter(c => c.name === 'boat_swim')
  const walks = calls.filter(c => c.name === 'goto')
  assert.ok(swims.length > 0)
  assert.ok(walks.length > 0, JSON.stringify(calls.map(c => [c.name, c.args])))
  assert.ok(calls.findIndex(c => c.name === 'boat_swim') < calls.findIndex(c => c.name === 'goto'))
  assert.ok(calls.filter(c => c.name === 'boat_swim').every(c => calls.indexOf(c) < calls.findIndex(d => d.name === 'goto')), 'after emerging, continue the crossing on foot')
})

test('villager.ferry walks to verified dry support even while the body still reports in water', async () => {
  const { api, calls } = waterFerryApi('source-water')
  await ferry.run(api, { uuid, boat: 8, x: 4, y: 62, z: 0, pullX: 6, pullY: 63, pullZ: 0 })
  const lastSwim = calls.findLastIndex(c => c.name === 'boat_swim')
  const firstDryWalk = calls.findIndex((c, i) => i > lastSwim && c.name === 'goto')
  assert.ok(lastSwim >= 0 && firstDryWalk > lastSwim, 'swim on the water route, then walk the verified dry column')
  assert.ok(calls.slice(0, firstDryWalk).some(c => c.name === 'state'), 'state.inWater remains true during this fixture; the dry support check selects goto')
})

test('villager.ferry follows pullVia waypoints in order after reaching the boat landing', async () => {
  const calls = []
  let position = { x: 0.5, y: 64, z: 0.5 }
  let boatAt = { ...position }
  let leashHolderId = null
  const api = {
    pos: () => ({ ...position }),
    block: (_x, y) => ({ name: y <= 63 ? 'stone' : 'air', solid: y <= 63 }),
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'state') return { inWater: false }
      if (name === 'boat_state') return {
        selfId: 12,
        mounted: null,
        boats: [{ id: 8, exact: `${boatAt.x},${boatAt.y},${boatAt.z}`, leashHolderId, passengers: [{ id: 42, uuid, name: 'villager' }] }]
      }
      if (name === 'boat_leash') { leashHolderId = 12; return { leashed: 8 } }
      if (name === 'goto') {
        position = { x: args.x, y: args.y, z: args.z }
        if (args.x <= 4.5) boatAt = { ...position }
        return {}
      }
      throw new Error(`unexpected action ${name}`)
    },
    until: async predicate => { if (!(await predicate())) throw new Error('boat did not follow') },
    report: () => {}
  }
  const result = await ferry.run(api, {
    uuid, boat: 8, x: 4, y: 64, z: 0,
    pullVia: '5:64:-1,7:64:-1', pullX: 9, pullY: 64, pullZ: -1
  })
  const waypoints = calls.filter(c => c.name === 'goto').map(c => [c.args.x, c.args.y, c.args.z])
  assert.deepEqual(waypoints.slice(-3), [[5, 64, -1], [7, 64, -1], [9, 64, -1]])
  assert.ok(calls.filter(c => c.name === 'goto').slice(-3).every(c => c.args.range === 0), 'explicit pull points must reach the exact side of a narrow shore doorway')
  assert.equal(result.at, '4.5,64,0.5')
})

test('villager.ferry selects supported dry-bank and shallow-water foot cells, but no invented floor', async () => {
  async function runHandoff ({ supported = false, unknown = false, dryBank = false }) {
    let position = { x: 0.5, y: 64, z: 0.5 }
    let boatAt = { ...position }
    let leashHolderId = null
    let inWater = false
    const calls = []
    const block = (x, y, z) => {
      if (dryBank && x >= 7 && x <= 9) {
        if (y === 63) return { name: 'grass_block', solid: true }
        if (y === 64 || y === 65) return { name: 'air', solid: false }
      }
      if (x === 7) {
        if (unknown && y <= 63) return null
        if (!dryBank && y === 62) return { name: 'water', solid: false }
        if (y === 61) return supported ? { name: 'sand', solid: true } : { name: 'air', solid: false }
        if (y === 60) return { name: 'air', solid: false }
        if (y === 63 || y === 64 || y === 65) return { name: 'air', solid: false }
      }
      return y <= 63 ? { name: 'stone', solid: true } : { name: 'air', solid: false }
    }
    const api = {
      pos: () => ({ ...position }), block,
      act: async (name, args = {}) => {
        calls.push({ name, args })
        if (name === 'state') return { inWater }
        if (name === 'boat_state') return {
          selfId: 12, mounted: null,
          boats: [{ id: 8, exact: `${boatAt.x},${boatAt.y},${boatAt.z}`, leashHolderId, passengers: [{ id: 42, uuid, name: 'villager' }] }]
        }
        if (name === 'boat_leash') { leashHolderId = 12; return { leashed: 8 } }
        if (name === 'goto') {
          position = { x: Math.floor(args.x) + 0.5, y: args.y, z: Math.floor(args.z) + 0.5 }
          if (Math.floor(args.x) <= 4) boatAt = { ...position }
          inWater = supported && Math.floor(position.x) === 7 && position.y === 62
          return {}
        }
        if (name === 'boat_swim') {
          inWater = true
          position = { x: Math.floor(args.x) + 0.5, y: 62.52, z: Math.floor(args.z) + 0.5 }
          return { to: `${position.x},${position.y},${position.z}` }
        }
        throw new Error(`unexpected action ${name}`)
      },
      until: async predicate => { if (!(await predicate())) throw new Error('boat did not follow') },
      report: () => {}
    }
    await ferry.run(api, {
      uuid, boat: 8, x: 4, y: 64, z: 0,
      pullX: 9, pullY: dryBank ? 65 : 63, pullZ: 0,
      pullVia: dryBank ? '7:65:0' : '7:63:0'
    })
    return calls
  }

  const dry = await runHandoff({ dryBank: true })
  assert.equal(dry.find(c => c.name === 'goto' && Math.floor(c.args.x) === 7).args.y, 64, 'a two-block-high dry bank uses its supported feet cell')

  const supported = await runHandoff({ supported: true })
  const shallowGoto = supported.find(c => c.name === 'goto' && Math.floor(c.args.x) === 7)
  assert.equal(shallowGoto.args.y, 62, 'the supported shallow-water column uses its sand-supported foot cell')
  assert.ok(supported.some(c => c.name === 'boat_swim' && c.args.x === 9), 'the next waypoint switches to a swim after the bot enters water')
  assert.ok(supported.findIndex(c => c.name === 'goto' && Math.floor(c.args.x) === 7) < supported.findIndex(c => c.name === 'boat_swim'))

  const unsupported = await runHandoff({ supported: false })
  assert.ok(unsupported.some(c => c.name === 'goto' && Math.floor(c.args.x) === 7 && c.args.y === 63), 'without solid support, keep the boat-height waypoint')
  assert.equal(unsupported.some(c => c.name === 'goto' && Math.floor(c.args.x) === 7 && c.args.y === 62), false, 'an unsupported water column does not create a lower walking floor')

  const unloaded = await runHandoff({ unknown: true })
  assert.ok(unloaded.some(c => c.name === 'goto' && Math.floor(c.args.x) === 7 && c.args.y === 63), 'unknown support keeps the original waypoint height')
  assert.equal(unloaded.some(c => c.name === 'goto' && Math.floor(c.args.x) === 7 && c.args.y < 63), false, 'unknown blocks cannot justify lowering the walk target')
})

test('villager.route rejects an uphill route before boat capture and accepts a clear descent with a turn', async () => {
  const calls = []
  const uphill = routeCheck.run({
    block: (x, y) => ({ name: y === (x >= 3 ? 64 : 63) ? 'stone' : 'air', solid: y === (x >= 3 ? 64 : 63) }),
    act: async (...args) => calls.push(args),
    report: () => {}
  }, { fromX: 0, fromY: 64, fromZ: 0, x: 5, y: 65, z: 0, margin: 2 })
  await assert.rejects(uphill, /boat route blocked.*upward|boat route blocked.*nonascending/)
  assert.deepEqual(calls, [], 'route planning must not capture, move, or edit anything')

  const clear = await routeCheck.run({
    block: routeTerrain((x, z) => x + z < 3 ? 63 : x + z < 6 ? 62 : 61),
    act: async (...args) => calls.push(args),
    report: () => {}
  }, { fromX: 0, fromY: 64, fromZ: 0, x: 4, y: 62, z: 4, margin: 2 })
  assert.equal(clear.clear, true)
  assert.ok(clear.route.length > 1)
  assert.equal(calls.length, 0)
})

test('villager.route normalizes water-cell goals to the boat float height in both directions', async () => {
  const water = (x, y) => y === 62
    ? { name: 'water', solid: false }
    : y === 61
      ? { name: 'stone', solid: true }
      : { name: 'air', solid: false }
  for (const [fromX, toX] of [[0, 4], [4, 0]]) {
    const plan = await routeCheck.run({ block: water, report: () => {} }, {
      fromX, fromY: 62, fromZ: 0, x: toX, y: 62, z: 0, margin: 2
    })
    assert.equal(plan.clear, true)
    assert.equal(plan.route[0].y, 62.5)
    assert.equal(plan.route.at(-1).y, 62.5)
  }
})

test('villager.route accepts a land-to-water descent and reports the water surface center', async () => {
  const terrain = (x, y) => {
    if (x <= 0 && y === 62) return { name: 'stone', solid: true }
    if (x > 0 && y === 62) return { name: 'water', solid: false }
    if (x > 0 && y === 61) return { name: 'stone', solid: true }
    return { name: 'air', solid: false }
  }
  const plan = await routeCheck.run({ block: terrain, report: () => {} }, {
    fromX: 0, fromY: 63, fromZ: 0, x: 4, y: 62, z: 0, margin: 2
  })
  assert.equal(plan.clear, true)
  assert.equal(plan.route[0].y, 63)
  assert.equal(plan.route.at(-1).y, 62.5)
  assert.ok(plan.route.every((p, i) => i === 0 || p.y <= plan.route[i - 1].y))
})

test('boat route rejects a bank landing above the current water float height at the landing', () => {
  const terrain = (x, y) => {
    if (x < 3 && y === 62) return { name: 'water', solid: false }
    if (x < 3 && y === 61) return { name: 'stone', solid: true }
    if (x >= 3 && y === 63) return { name: 'stone', solid: true }
    return { name: 'air', solid: false }
  }
  const result = villagerBoatRoute({
    from: { x: 0.5, y: 62.5, z: 0.5 },
    to: { x: 5.5, y: 64, z: 0.5 },
    blockAt: terrain,
    margin: 2
  })
  assert.equal(result.points, undefined)
  assert.match(result.error, /^landing unsupported water or ground/)
})

test('boat route checks the full destination hull before searching the route', () => {
  const terrain = (x, y) => {
    if (x === 5 && y === 63) return { name: 'stone', solid: true }
    if (y === 62) return { name: 'water', solid: false }
    if (y === 61) return { name: 'stone', solid: true }
    return { name: 'air', solid: false }
  }
  const result = villagerBoatRoute({
    from: { x: 0.5, y: 62.5, z: 0.5 },
    to: { x: 4.5, y: 62, z: 0.5 },
    blockAt: terrain,
    margin: 2
  })
  assert.equal(result.points, undefined)
  assert.match(result.error, /landing hull blocked by stone/)
  assert.equal(result.at.x, 5)
  assert.equal(result.at.y, 63)
})

const key = (x, y, z) => typeof x === 'object' ? `${x.x},${x.y},${x.z}` : `${x},${y},${z}`

function adultReachable (plan) {
  const blocked = new Set([...plan.walls, ...plan.gate].map(p => key(p.x, p.y, p.z)))
  // Model the ring with a flat exterior, then add a continuous one-block-high
  // platform around it so a villager cannot exploit a raised approach.
  const floors = new Set()
  for (let x = plan.cell.x - 5; x <= plan.cell.x + 5; x++) for (let z = plan.cell.z - 5; z <= plan.cell.z + 5; z++) floors.add(key(x, plan.cell.y - 1, z))
  for (let x = plan.cell.x - 5; x <= plan.cell.x + 5; x++) for (let z = plan.cell.z - 5; z <= plan.cell.z + 5; z++) {
    if (Math.abs(x - plan.cell.x) >= 3 || Math.abs(z - plan.cell.z) >= 3) floors.add(key(x, plan.cell.y, z))
  }
  const clearAdult = (x, feet, z) => !blocked.has(key(x, feet, z)) && !blocked.has(key(x, feet + 1, z))
  const canStand = (x, feet, z) => floors.has(key(x, feet - 1, z)) && clearAdult(x, feet, z)
  const start = { x: plan.cell.x, y: plan.cell.y, z: plan.cell.z }
  const queue = [start]
  const seen = new Set([key(start.x, start.y, start.z)])
  while (queue.length) {
    const p = queue.shift()
    for (const [dx, dz] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
      for (const dy of [0, 1, -1]) {
        const next = { x: p.x + dx, y: p.y + dy, z: p.z + dz }
        const k = key(next.x, next.y, next.z)
        if (seen.has(k) || !canStand(next.x, next.y, next.z)) continue
        seen.add(k)
        queue.push(next)
      }
    }
  }
  return seen
}

function adultWaterReachableWithServiceOpen (plan, deep = false) {
  const blocked = new Set([
    ...plan.walls,
    ...plan.gate.filter(p => key(p.x, p.y, p.z) !== key(plan.service)),
    plan.serviceFoundation
  ].map(p => key(p.x, p.y, p.z)))
  const floors = new Set()
  const water = new Set()
  const addFloor = (x, y, z) => {
    floors.add(key(x, y, z))
    blocked.add(key(x, y, z))
  }
  for (let x = plan.cell.x - 5; x <= plan.cell.x + 5; x++) for (let z = plan.cell.z - 5; z <= plan.cell.z + 5; z++) {
    addFloor(x, plan.cell.y - 2 - (deep ? 1 : 0), z) // bed at y=61 or y=60
    if (Math.abs(x - plan.cell.x) >= 3 || Math.abs(z - plan.cell.z) >= 3) {
      addFloor(x, plan.cell.y - 1, z) // flat shore at y=62
      addFloor(x, plan.cell.y, z) // one-block-raised shore at y=63
    }
  }
  for (const p of plan.interior) {
    water.add(key(p.x, plan.cell.y - 1, p.z)) // top water layer
    if (deep) water.add(key(p.x, plan.cell.y - 2, p.z)) // second water layer
  }
  if (deep) for (const p of plan.gateBase) addFloor(p.x, plan.cell.y - 2, p.z) // fill both gate beds
  addFloor(plan.cell.x, plan.cell.y, plan.cell.z) // dry landing floor inside
  const clear = (x, feet, z) => !blocked.has(key(x, feet, z)) && !blocked.has(key(x, feet + 1, z))
  const canStand = (x, feet, z) => clear(x, feet, z) && (
    floors.has(key(x, feet - 1, z)) || water.has(key(x, feet, z)) || water.has(key(x, feet - 1, z))
  )
  const queue = [plan.cell.y - 1, plan.cell.y, plan.cell.y + 1, plan.cell.y + 2].map(y => ({ x: plan.cell.x, y, z: plan.cell.z })).filter(p => canStand(p.x, p.y, p.z))
  const seen = new Set(queue.map(p => key(p.x, p.y, p.z)))
  while (queue.length) {
    const p = queue.shift()
    for (const [dx, dz] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) for (const dy of [0, 1, -1]) {
      const next = { x: p.x + dx, y: p.y + dy, z: p.z + dz }
      const k = key(next.x, next.y, next.z)
      if (seen.has(k) || !canStand(next.x, next.y, next.z)) continue
      seen.add(k)
      queue.push(next)
    }
  }
  return seen
}

test('villager dock keeps a two-block boat opening and a closed four-high adult barrier on every river side', () => {
  const cell = { x: 40, y: 64, z: -20 }
  for (const river of [{ x: 1, z: 0 }, { x: -1, z: 0 }, { x: 0, z: 1 }, { x: 0, z: -1 }]) {
    const plan = villagerDockPlan(cell, river)
    assert.equal(plan.gateBase.length, 2)
    const gateWidth = Math.hypot(plan.gateBase[0].x - plan.gateBase[1].x, plan.gateBase[0].z - plan.gateBase[1].z) + 1
    assert.ok(gateWidth >= 1.375, `two-cell opening must clear a 1.375-wide boat, got ${gateWidth}`)

    const reachable = adultReachable(plan)
    const interior = reachable.has(key(cell.x, cell.y, cell.z))
    assert.ok(interior)
    for (let x = cell.x - 5; x <= cell.x + 5; x++) for (let z = cell.z - 5; z <= cell.z + 5; z++) {
      if (Math.abs(x - cell.x) >= 3 || Math.abs(z - cell.z) >= 3) {
        assert.equal(reachable.has(key(x, cell.y, z)), false, `adult escaped to flat/raised exterior at ${x},${cell.y},${z}`)
        assert.equal(reachable.has(key(x, cell.y + 1, z)), false, `adult escaped to raised exterior at ${x},${cell.y + 1},${z}`)
      }
    }
  }
})

test('temporary water-level service slot contains adults from riverbed, water surface, and raised bank foot levels', () => {
  const cell = { x: 40, y: 63, z: -20 }
  for (const river of [{ x: 1, z: 0 }, { x: -1, z: 0 }, { x: 0, z: 1 }, { x: 0, z: -1 }]) {
    const plan = villagerDockPlan(cell, river)
    assert.equal(plan.service.y, 63)
    assert.equal(plan.serviceFoundation.y, 62)
    for (const deep of [false, true]) {
      const reachable = adultWaterReachableWithServiceOpen(plan, deep)
      for (let feet = 61; feet <= 66; feet++) {
        for (let x = cell.x - 5; x <= cell.x + 5; x++) for (let z = cell.z - 5; z <= cell.z + 5; z++) {
          if (Math.abs(x - cell.x) >= 3 || Math.abs(z - cell.z) >= 3) {
            assert.equal(reachable.has(key(x, feet, z)), false, `adult escaped from feet y=${feet} (${deep ? 'deep' : 'shallow'}) to raised/flat bank at ${x},${z}`)
          }
        }
      }
    }
  }
})

function dockApi ({ blocks = 500, cellX = 40, cellY = 64, cellZ = -20, rearGap = false, staleReleaseOutside = false, deepWater = false, deepStandUnsupported = false, dryAirOverSolid = false, siteVillagers = [], prepBoats = [], releasedVillager = true, deferredCap = null } = {}) {
  const cell = { x: cellX, y: cellY, z: cellZ }
  const river = { x: 1, z: 0 }
  const plan = villagerDockPlan(cell, river)
  const blockMap = new Map()
  const put = (p, name) => blockMap.set(key(p.x, p.y, p.z), { name, solid: !['air', 'water'].includes(name) })
  const gate = new Set(plan.gate.map(p => key(p.x, p.y, p.z)))
  for (const p of plan.ring) {
    if (!gate.has(key(p.x, p.y, p.z))) put(p, 'grass_block')
    else put(p, 'air')
    put({ ...p, y: p.y - 1 }, 'water')
    put({ ...p, y: p.y - 2 }, deepWater ? 'water' : 'stone')
    if (deepWater) put({ ...p, y: p.y - 3 }, 'stone')
    for (let n = 1; n <= 3; n++) put({ ...p, y: p.y + n }, 'air')
  }
  if (rearGap) {
    for (const y of [cell.y + 1, cell.y + 2]) blockMap.delete(key(cell.x - 2, y, cell.z))
    put({ x: cell.x - 2, y: cell.y + 2, z: cell.z - 1 }, 'cobblestone')
    put({ x: cell.x - 2, y: cell.y + 2, z: cell.z + 1 }, 'cobblestone')
  }
  for (const p of plan.interior) {
    put({ ...p, y: p.y - 1 }, 'water')
    put({ ...p, y: p.y - 2 }, deepWater ? 'water' : 'stone')
    if (deepWater) put({ ...p, y: p.y - 3 }, 'stone')
    put(p, 'air')
    put({ ...p, y: p.y + 1 }, 'air')
  }
  if (dryAirOverSolid) put({ ...plan.interior[4], y: plan.interior[4].y - 1 }, 'gravel')
  else put(plan.interior[4], 'grass_block')
  const standFloor = { ...plan.serviceStand, y: plan.serviceStand.y - 1 }
  put(standFloor, deepWater ? 'water' : 'stone')
  if (deepWater) put({ ...standFloor, y: standFloor.y - 1 }, deepStandUnsupported ? 'water' : 'stone')
  if (deepStandUnsupported) put({ ...standFloor, y: standFloor.y - 2 }, 'stone')
  put(plan.serviceStand, 'water')

  const inventory = typeof blocks === 'object' ? { ...blocks } : { cobblestone: blocks }

  const calls = []
  const reports = []
  let entityReads = 0
  let released = false
  const api = {
    me: () => 'test-bot',
    block: (x, y, z) => blockMap.get(key(x, y, z)) ?? { name: 'air', solid: false },
    inv: () => ({ ...inventory }),
    zones: () => [],
    places: () => [],
    report: value => reports.push(value),
    until: async (predicate, options) => {
      assert.equal(options.timeout, 5)
      if (!(await predicate())) throw new Error('villager was not observed inside after release')
    },
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'boat_state') return args.id === undefined
        ? { selfId: 12, mounted: null, boats: prepBoats }
        : { selfId: 12, mounted: null, boats: [{ id: 8, exact: `${cell.x + 0.5},${cellY},${cell.z + 0.5}`, leashHolderId: null, passengers: [{ id: 42, uuid, name: 'villager' }] }] }
      if (name === 'entity') {
        entityReads++
        if (entityReads === 1 || !released || !releasedVillager) return { found: siteVillagers }
        if (staleReleaseOutside && entityReads === 2) return { found: [{ ...villager(42, uuid, `${cell.x + 5.5},${cellY + 1},${cell.z + 0.5}`), vehicleId: null }] }
        return { found: [{ ...villager(42, uuid, `${cell.x + 0.5},${cellY + 1},${cell.z + 0.5}`), vehicleId: null }] }
      }
      if (name === 'goto') return {}
      if (name === 'place') { put(args, args.item); return { placed: 1 } }
      if (name === 'dig') { blockMap.delete(key(args.x, args.y, args.z)); return { dug: 1 } }
      if (name === 'boat_release') {
        const gap = [...plan.gate.filter(p => key(p.x, p.y, p.z) !== key(plan.service)), ...plan.walls.filter(p => !deferredCap || key(p) !== key(deferredCap)), plan.serviceFoundation].find(p => !api.block(p.x, p.y, p.z)?.solid)
        assert.equal(gap, undefined, 'gate and walls must be sealed before releasing the passenger')
        if (deepWater) {
          assert.ok(plan.gateBase.every(p => api.block(p.x, p.y - 2, p.z)?.solid), 'both deep gate beds must be filled before release')
          assert.equal(api.block(plan.serviceFoundation.x, plan.serviceFoundation.y, plan.serviceFoundation.z)?.solid, true, 'the service gate alone keeps its temporary sill')
          const channelSill = { ...plan.gateBase[1], y: plan.gateBase[1].y - 1 }
          assert.equal(api.block(channelSill.x, channelSill.y, channelSill.z)?.name, 'water', 'leave the second gate column open for the boat channel')
        }
        released = true
        return { released: args.passengerUuid, onFoot: { uuid: args.passengerUuid, vehicleId: null, exact: staleReleaseOutside ? `${cell.x + 5.5},${cellY + 1},${cell.z + 0.5}` : `${cell.x + 0.5},${cellY + 1},${cell.z + 0.5}` }, boatBroken: args.id }
      }
      throw new Error(`unexpected action ${name}`)
    }
  }
  return { api, calls, reports, plan, inventory, setBlock: (p, name) => put(p, name) }
}

test('villager.dock checks full block inventory before making world edits', async () => {
  const { api, calls } = dockApi({ blocks: 0 })
  await assert.rejects(dock.run(api, { uuid, boat: 8, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /dock needs .* cobblestone/)
  assert.equal(calls.some(c => ['place', 'goto', 'boat_unleash', 'boat_release'].includes(c.name)), false)
})

test('villager.dock prepare refuses insufficient materials before placing any blocks', async () => {
  const { api, calls } = dockApi({ blocks: 0 })
  await assert.rejects(dock.run(api, { prepare: true, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /dock needs \d+ blocks from cobblestone/)
  assert.equal(calls.some(c => ['place', 'goto', 'boat_release', 'boat_place'].includes(c.name)), false)
})

test('villager.dock prepare refuses occupied sites without making changes', async () => {
  const occupiedByVillager = dockApi({ siteVillagers: [villager(42, uuid, '40.5,65,-19.5')] })
  await assert.rejects(dock.run(occupiedByVillager.api, { prepare: true, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /already contains another villager/)
  assert.equal(occupiedByVillager.calls.some(c => c.name === 'place'), false)

  const occupiedByBoat = dockApi({ prepBoats: [{ id: 88, exact: '40.5,64,-19.5', passengers: [] }] })
  await assert.rejects(dock.run(occupiedByBoat.api, { prepare: true, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /already contains boat 88/)
  assert.equal(occupiedByBoat.calls.some(c => c.name === 'place'), false)
})

test('villager.dock prepare builds walls and lintel while leaving both boat gate cells and underwater channel open', async () => {
  const { api, calls, plan, reports } = dockApi()
  const result = await dock.run(api, { prepare: true, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 })
  assert.equal(result.prepared, true)
  assert.equal(result.gateOpen, true)
  for (const p of plan.walls) assert.equal(api.block(p.x, p.y, p.z).solid, true, `prepared wall missing at ${key(p)}`)
  for (const p of plan.gate) assert.equal(api.block(p.x, p.y, p.z).solid, false, `gate should remain open at ${key(p)}`)
  for (const p of plan.gateBase) assert.equal(api.block(p.x, p.y - 1, p.z).name, 'water', `boat channel should remain open below ${key(p)}`)
  assert.equal(calls.some(c => ['goto', 'boat_release', 'boat_place'].includes(c.name)), false)
  assert.equal(api.block(plan.serviceStand.x, plan.serviceStand.y - 1, plan.serviceStand.z).solid, true, 'service stand must have a firm floor')
  assert.deepEqual(reports.at(-1), { dock: 'prepared', gateOpen: true })
})

test('villager.dock accepts clear dry standing space above solid shore ground', async () => {
  const { api, plan } = dockApi({ cellY: 63, dryAirOverSolid: true })
  const dryCell = plan.interior[4]
  assert.equal(api.block(dryCell.x, dryCell.y, dryCell.z).name, 'air')
  assert.equal(api.block(dryCell.x, dryCell.y - 1, dryCell.z).name, 'gravel')
  const result = await dock.run(api, { prepare: true, x: 40, y: 63, z: -20, riverX: 1, riverZ: 0 })
  assert.equal(result.prepared, true)
})

test('villager.dock prepare allocates a mixed-material bill before edits and resumes an already built palette dock', async () => {
  const { api, calls, reports } = dockApi({ blocks: { cobbled_deepslate: 2, cherry_planks: 100 }, releasedVillager: false })
  const args = { prepare: true, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0, materials: 'cobbled_deepslate,cherry_planks' }
  const result = await dock.run(api, args)
  assert.deepEqual(result.bill, { cobbled_deepslate: 2, cherry_planks: result.need - 2 })
  assert.equal(calls.filter(c => c.name === 'place').length, result.need)
  assert.ok(calls.every(c => c.name !== 'place' || ['cobbled_deepslate', 'cherry_planks'].includes(c.args.item)))

  const placesBeforeResume = calls.filter(c => c.name === 'place').length
  const resumed = await dock.run(api, args)
  assert.equal(resumed.need, 0)
  assert.deepEqual(resumed.bill, {})
  assert.equal(calls.filter(c => c.name === 'place').length, placesBeforeResume)
  assert.deepEqual(reports.at(-1), { dock: 'prepared', gateOpen: true })
  assert.ok(calls.filter(c => c.name === 'place').every(c => ['cobbled_deepslate', 'cherry_planks'].includes(api.block(c.args.x, c.args.y, c.args.z)?.name)))
})

test('villager.dock closes the gate before releasing the exact passenger and sealing its service slot', async () => {
  const { api, calls, reports, plan } = dockApi()
  const result = await dock.run(api, { uuid, boat: 8, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 })
  assert.equal(result.villagerUuid, uuid)
  assert.equal(result.gateClosed, true)
  assert.equal(result.secure, true)
  assert.equal(calls.find(c => c.name === 'boat_release').args.passengerUuid, uuid)
  assert.ok(calls.findIndex(c => c.name === 'boat_release') > calls.findLastIndex(c => c.name === 'place' && plan.gate.some(p => key(p.x, p.y, p.z) === key(c.args.x, c.args.y, c.args.z) && key(p.x, p.y, p.z) !== key(plan.service))))
  assert.ok(calls.findIndex(c => c.name === 'place' && key(c.args.x, c.args.y, c.args.z) === key(plan.service)) > calls.findIndex(c => c.name === 'boat_release'))
  assert.equal(calls.some(c => c.name === 'place' && plan.gateBase.some(p => key(p.x, p.y - 1, p.z) === key(c.args.x, c.args.y, c.args.z) && key(c.args.x, c.args.y, c.args.z) !== key(plan.serviceFoundation))), false, 'the second gate base must remain open beneath the boat')
  assert.equal(api.block(plan.serviceFoundation.x, plan.serviceFoundation.y, plan.serviceFoundation.z).solid, true)
  assert.equal(api.block(plan.service.x, plan.service.y, plan.service.z).solid, true)
  assert.deepEqual(reports.at(-1), { dock: 'secured', villagerUuid: uuid })
})

test('villager.dock permits one deferred rear cap only for the freshly observed adult passenger', async () => {
  const cell = { x: -132, y: 63, z: -168 }
  const cap = { x: cell.x - 2, y: cell.y + 3, z: cell.z }
  const insideStand = { x: cell.x, y: cell.y + 1, z: cell.z }
  const adult = { ...villager(42, uuid, `${cell.x + 0.5},${cell.y + 1},${cell.z + 0.5}`), baby: false, adult: true }
  const run = dockApi({ cellX: cell.x, cellY: cell.y, cellZ: cell.z, siteVillagers: [adult], deferredCap: cap })
  run.setBlock({ x: cap.x, y: cap.y + 1, z: cap.z }, 'oak_planks')
  const originalAct = run.api.act
  run.api.act = async (name, args) => {
    if (name === 'boat_release') {
      for (const p of run.plan.gate) assert.equal(run.api.block(p.x, p.y, p.z).solid, true, `boat gate remains closed at ${key(p)}`)
      for (const y of [cap.y - 1, cap.y - 2, cap.y + 1]) assert.equal(run.api.block(cap.x, y, cap.z).solid, true, `adult barrier/roof is full at ${cap.x},${y},${cap.z}`)
      assert.equal(run.api.block(cap.x, cap.y, cap.z).name, 'air', 'the deferred opening is exactly one high')
      return originalAct(name, args)
    }
    return originalAct(name, args)
  }
  const result = await dock.run(run.api, {
    uuid, boat: 8, ...cell, riverX: 1, riverZ: 0,
    insideX: insideStand.x, insideY: insideStand.y, insideZ: insideStand.z,
    deferCapX: cap.x, deferCapZ: cap.z
  })
  assert.equal(result.adultOnly, true)
  assert.equal(result.fullySealed, false)
  assert.equal(result.deferredCap, key(cap))
  assert.equal(run.plan.walls.filter(p => !run.api.block(p.x, p.y, p.z).solid).length, 1)
  assert.equal(run.api.block(cap.x, cap.y, cap.z).name, 'air')
  assert.equal(run.calls.some(c => c.name === 'boat_release'), true)
})

test('villager.dock refuses a deferred adult cap for baby/unknown age and for a missing roof before edits', async () => {
  const cell = { x: -132, y: 63, z: -168 }
  const cap = { x: cell.x - 2, y: cell.y + 3, z: cell.z }
  const insideStand = { x: cell.x, y: cell.y + 1, z: cell.z }
  const args = { uuid, boat: 8, ...cell, riverX: 1, riverZ: 0, insideX: insideStand.x, insideY: insideStand.y, insideZ: insideStand.z, deferCapX: cap.x, deferCapZ: cap.z }
  for (const resident of [
    { ...villager(42, uuid, `${cell.x + 0.5},${cell.y + 1},${cell.z + 0.5}`), baby: true, adult: false },
    villager(42, uuid, `${cell.x + 0.5},${cell.y + 1},${cell.z + 0.5}`)
  ]) {
    const run = dockApi({ cellX: cell.x, cellY: cell.y, cellZ: cell.z, siteVillagers: [resident], deferredCap: cap })
    run.setBlock({ x: cap.x, y: cap.y + 1, z: cap.z }, 'oak_planks')
    await assert.rejects(dock.run(run.api, args), /exact passenger freshly observed as an adult/)
    assert.equal(run.calls.some(c => ['goto', 'place', 'dig', 'boat_release'].includes(c.name)), false)
  }
  const noRoof = dockApi({ cellX: cell.x, cellY: cell.y, cellZ: cell.z })
  await assert.rejects(dock.run(noRoof.api, args), /full roof over one upper rear wall cell/)
  assert.equal(noRoof.calls.length, 0, 'the roof must be preflighted before any boat or world action')
})

test('villager.dock repairs a two-block rear wall gap before releasing the passenger', async () => {
  const { api, calls, plan } = dockApi({ cellX: -132, cellY: 63, cellZ: -168, rearGap: true })
  const gap = [{ x: -134, y: 64, z: -168 }, { x: -134, y: 65, z: -168 }]
  for (const p of gap) assert.equal(api.block(p.x, p.y, p.z).name, 'air')
  await dock.run(api, { uuid, boat: 8, x: -132, y: 63, z: -168, riverX: 1, riverZ: 0 })
  const releaseAt = calls.findIndex(c => c.name === 'boat_release')
  for (const p of gap) {
    assert.equal(api.block(p.x, p.y, p.z).solid, true, `rear wall gap ${key(p)} must be sealed`)
    const placedAt = calls.findIndex(c => c.name === 'place' && key(c.args) === key(p))
    assert.ok(placedAt >= 0 && placedAt < releaseAt, `rear wall gap ${key(p)} must be filled before release`)
  }
  const lowerPlace = calls.findIndex(c => c.name === 'place' && key(c.args) === key(gap[0]))
  const upperPlace = calls.findIndex(c => c.name === 'place' && key(c.args) === key(gap[1]))
  assert.ok(upperPlace < lowerPlace, `fill upper before lower (upper=${upperPlace}, lower=${lowerPlace})`)
  assert.ok(plan.walls.some(p => key(p) === key(gap[0])))
})

test('villager.dock waits for a stale release position to update to the exact villager inside', async () => {
  const { api, calls, plan } = dockApi({ staleReleaseOutside: true })
  const result = await dock.run(api, { uuid, boat: 8, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 })
  assert.equal(result.secure, true)
  assert.equal(result.villagerUuid, uuid)
  assert.ok(calls.filter(c => c.name === 'entity').length >= 3, 'retry a stale outside position until the same UUID is inside')
  assert.equal(api.block(plan.service.x, plan.service.y, plan.service.z).solid, true)
})

test('villager.dock reseals the service slot before returning an unobserved passenger error', async () => {
  const { api, calls, plan } = dockApi({ releasedVillager: false })
  await assert.rejects(dock.run(api, { uuid, boat: 8, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /was not observed inside after release/)
  const releaseAt = calls.findIndex(c => c.name === 'boat_release')
  const servicePlaceAt = calls.findIndex(c => c.name === 'place' && key(c.args) === key(plan.service))
  assert.ok(servicePlaceAt > releaseAt)
  assert.equal(api.block(plan.service.x, plan.service.y, plan.service.z).solid, true)
})

test('villager.dock preflights and fills both deep gate beds before releasing a passenger', async () => {
  const preflight = dockApi({ blocks: 0, cellY: 63, deepWater: true })
  const error = await dock.run(preflight.api, { uuid, boat: 8, x: 40, y: 63, z: -20, riverX: 1, riverZ: 0 }).then(() => null, e => e)
  assert.match(error.message, /dock needs \d+ blocks from cobblestone/)
  assert.equal(preflight.calls.some(c => ['place', 'goto', 'boat_release'].includes(c.name)), false)
  const required = Number(error.message.match(/dock needs (\d+) blocks from cobblestone/)[1])

  const { api, calls, plan } = dockApi({ blocks: required, cellY: 63, deepWater: true })
  await dock.run(api, { uuid, boat: 8, x: 40, y: 63, z: -20, riverX: 1, riverZ: 0 })
  const standFloor = { ...plan.serviceStand, y: plan.serviceStand.y - 1 }
  assert.equal(api.block(standFloor.x, standFloor.y, standFloor.z).name, 'cobblestone', 'two-deep stand water must get a solid floor')
  assert.ok(calls.findIndex(c => c.name === 'place' && key(c.args.x, c.args.y, c.args.z) === key(standFloor)) < calls.findIndex(c => c.name === 'goto'), 'stand floor must be in place before approaching the service stand')
  for (const p of plan.gateBase) {
    assert.equal(api.block(p.x, p.y - 2, p.z).name, 'cobblestone')
    assert.ok(calls.some(c => c.name === 'place' && key(c.args.x, c.args.y, c.args.z) === key(p.x, p.y - 2, p.z)))
  }
  assert.equal(calls.some(c => c.name === 'place' && key(c.args.x, c.args.y, c.args.z) === key(plan.serviceFoundation)), true)
  const secondColumnUnderwater = { ...plan.gateBase[1], y: plan.gateBase[1].y - 1 }
  assert.equal(api.block(secondColumnUnderwater.x, secondColumnUnderwater.y, secondColumnUnderwater.z).name, 'water')
})

test('villager.dock prepare refuses a service stand deeper than two blocks before edits', async () => {
  const { api, calls } = dockApi({ cellY: 63, deepWater: true, deepStandUnsupported: true })
  await assert.rejects(dock.run(api, { prepare: true, x: 40, y: 63, z: -20, riverX: 1, riverZ: 0 }), /service stand is deeper than two blocks/)
  assert.equal(calls.some(c => ['place', 'goto', 'boat_release'].includes(c.name)), false)
})

function undockApi ({ leads = 1, losePassengerAfterGateDig = false, failBoatPlace = false, cell = { x: 40, y: 64, z: -20 }, dryLaunch = false } = {}) {
  const river = { x: 1, z: 0 }
  const plan = villagerDockPlan(cell, river)
  const blockMap = new Map()
  const put = (p, name) => blockMap.set(key(p.x, p.y, p.z), { name, solid: !['air', 'water'].includes(name) })
  for (const p of [...plan.walls, ...plan.gate, plan.service]) put(p, 'cobblestone')
  const launchY = cell.y - 1
  for (let x = cell.x - 2; x <= cell.x + 2; x++) {
    for (let z = cell.z - 2; z <= cell.z + 2; z++) {
      put({ x, y: launchY, z }, dryLaunch ? 'air' : 'water')
      put({ x, y: launchY - 1, z }, dryLaunch ? 'air' : 'stone')
    }
  }
  put({ ...plan.serviceStand, y: plan.serviceStand.y - 1 }, 'grass_block')
  put(plan.serviceStand, 'water')
  put(plan.serviceFoundation, 'stone')
  const calls = []
  const reports = []
  const boatPlaceSnapshot = []
  let boatId = null
  let leashHolderId = null
  let states = 0
  let lostPassenger = false
  const target = { ...villager(42, uuid, `${cell.x + 0.5},${cell.y + 1},${cell.z + 0.5}`), vehicleId: null }
  const boatState = () => ({
    selfId: 12,
    mounted: null,
    boats: boatId === null ? [] : [{
      id: boatId,
      exact: `${cell.x + 0.5},${cell.y},${cell.z + 0.5}`,
      leashHolderId,
      passengers: lostPassenger ? [] : [{ id: target.id, uuid, name: 'villager' }]
    }]
  })
  const api = {
    me: () => 'test-bot',
    pos: () => ({ x: cell.x + 0.5, y: cell.y + 1, z: cell.z + 0.5 }),
    block: (x, y, z) => blockMap.get(key(x, y, z)) ?? { name: 'air', solid: false },
    inv: () => ({ oak_boat: 1, lead: leads, cobblestone: 10 }),
    zones: () => [],
    places: () => [],
    report: value => reports.push(value),
    until: async (predicate, options) => {
      assert.equal(options.timeout, 90)
      if (!(await predicate())) throw new Error('boarding timeout')
    },
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'entity') return { found: [target] }
      if (name === 'boat_place') {
        boatPlaceSnapshot.push({
          service: api.block(plan.service.x, plan.service.y, plan.service.z).solid,
          sill: api.block(plan.serviceFoundation.x, plan.serviceFoundation.y, plan.serviceFoundation.z).solid,
          otherGate: plan.gate.filter(p => key(p.x, p.y, p.z) !== key(plan.service)).every(p => api.block(p.x, p.y, p.z).solid)
        })
        if (failBoatPlace) throw new Error('boat placement failed')
        boatId = 8
        return { boat: { id: boatId } }
      }
      if (name === 'boat_state') { states++; return boatState() }
      if (name === 'goto') return {}
      if (name === 'boat_leash') { leashHolderId = 12; return { leashed: 8 } }
      if (name === 'dig') {
        blockMap.delete(key(args.x, args.y, args.z))
        if (losePassengerAfterGateDig && plan.gate.some(p => key(p.x, p.y, p.z) === key(args.x, args.y, args.z)) && key(args.x, args.y, args.z) !== key(plan.service)) lostPassenger = true
        return { dug: 1 }
      }
      if (name === 'place') { put(args, args.item); return { placed: 1 } }
      throw new Error(`unexpected action ${name}`)
    }
  }
  return { api, calls, reports, plan, blockMap, boatPlaceSnapshot, get states () { return states } }
}

test('villager.undock refuses a missing lead before boarding or opening the service slot', async () => {
  const { api, calls, plan } = undockApi({ leads: 0 })
  await assert.rejects(undock.run(api, { uuid, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /needs one lead/)
  assert.equal(calls.some(c => ['boat_place', 'dig', 'boat_leash'].includes(c.name)), false)
  assert.equal(api.block(plan.service.x, plan.service.y, plan.service.z).solid, true)
})

test('villager.undock refuses an unsupported launch hull before opening the service slot', async () => {
  const { api, calls, plan } = undockApi({ dryLaunch: true })
  await assert.rejects(undock.run(api, { uuid, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /boat launch blocked/)
  assert.equal(calls.some(c => ['goto', 'dig', 'boat_place', 'boat_leash'].includes(c.name)), false)
  assert.equal(api.block(plan.service.x, plan.service.y, plan.service.z).solid, true)
})

test('villager.undock boards and leashes the requested UUID before opening the river gate', async () => {
  const { api, calls, plan, reports, boatPlaceSnapshot } = undockApi()
  const result = await undock.run(api, { uuid, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 })
  assert.equal(result.villagerUuid, uuid)
  assert.equal(result.passengerSeated, true)
  assert.equal(result.leashHeld, true)
  assert.equal(result.gateOpen, true)
  assert.ok(calls.some(c => c.name === 'boat_place'))
  assert.ok(calls.findIndex(c => c.name === 'dig' && key(c.args.x, c.args.y, c.args.z) === key(plan.service)) < calls.findIndex(c => c.name === 'boat_place'))
  assert.deepEqual(boatPlaceSnapshot.at(-1), { service: false, sill: true, otherGate: true }, 'only the one-high service cell opens before placing the boat')
  assert.equal(calls.find(c => c.name === 'boat_leash').args.id, result.boat)
  assert.ok(calls.findIndex(c => c.name === 'dig' && key(c.args.x, c.args.y, c.args.z) === key(plan.service)) < calls.findIndex(c => c.name === 'boat_leash'))
  const otherGateDigs = calls.filter(c => c.name === 'dig' && plan.gate.some(p => key(p.x, p.y, p.z) === key(c.args.x, c.args.y, c.args.z) && key(p.x, p.y, p.z) !== key(plan.service)))
  const firstGateDig = calls.findIndex(c => c === otherGateDigs[0])
  assert.ok(firstGateDig > calls.findIndex(c => c.name === 'boat_leash'))
  const sillDig = calls.findIndex(c => c.name === 'dig' && key(c.args.x, c.args.y, c.args.z) === key(plan.serviceFoundation))
  assert.ok(sillDig > calls.findIndex(c => c === otherGateDigs.at(-1)))
  assert.ok(plan.gate.every(p => !api.block(p.x, p.y, p.z).solid))
  assert.equal(api.block(plan.serviceFoundation.x, plan.serviceFoundation.y, plan.serviceFoundation.z).solid, false)
  assert.deepEqual(reports.at(-1), { dock: 'open', boat: result.boat, villagerUuid: uuid })
})

test('villager.undock reseals the service slot when boat placement fails', async () => {
  const { api, calls, plan, reports, boatPlaceSnapshot } = undockApi({ failBoatPlace: true })
  await assert.rejects(undock.run(api, { uuid, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /boat placement failed/)
  assert.deepEqual(boatPlaceSnapshot, [{ service: false, sill: true, otherGate: true }])
  assert.ok(calls.some(c => c.name === 'dig' && key(c.args.x, c.args.y, c.args.z) === key(plan.service)))
  assert.ok(calls.some(c => c.name === 'place' && key(c.args.x, c.args.y, c.args.z) === key(plan.service)))
  assert.equal(api.block(plan.service.x, plan.service.y, plan.service.z).solid, true)
  assert.equal(api.block(plan.serviceFoundation.x, plan.serviceFoundation.y, plan.serviceFoundation.z).solid, true)
  assert.ok(plan.gate.filter(p => key(p.x, p.y, p.z) !== key(plan.service)).every(p => api.block(p.x, p.y, p.z).solid))
  assert.equal(reports.at(-1).closurePending, '')
})

test('villager.undock passes an explicit centered launch into boat placement', async () => {
  const cell = { x: -132, y: 63, z: -169 }
  const { api, calls } = undockApi({ cell })
  await undock.run(api, { uuid, ...cell, riverX: 1, riverZ: 0, centerX: -131, centerZ: -168, aimY: 62.85 })
  const place = calls.find(c => c.name === 'boat_place')
  assert.deepEqual(place.args, { item: 'oak_boat', x: -132, y: 62, z: -169, centerX: -131, centerZ: -168, aimY: 62.85 })
})

test('villager.undock rejects aimY outside the water layer before opening the service slot', async () => {
  const cell = { x: -132, y: 63, z: -169 }
  const { api, calls, plan } = undockApi({ cell })
  await assert.rejects(undock.run(api, { uuid, ...cell, riverX: 1, riverZ: 0, centerX: -131, centerZ: -168, aimY: 62.4 }), /aimY=.*water layer/)
  assert.equal(calls.some(c => ['goto', 'dig', 'boat_place', 'boat_leash'].includes(c.name)), false)
  assert.equal(api.block(plan.service.x, plan.service.y, plan.service.z).solid, true)
})

test('villager.undock reseals every gate cell and temporary sill if the passenger disappears during opening', async () => {
  const { api, calls, plan, reports } = undockApi({ losePassengerAfterGateDig: true })
  await assert.rejects(undock.run(api, { uuid, x: 40, y: 64, z: -20, riverX: 1, riverZ: 0 }), /is not in boat/)
  assert.ok(calls.some(c => c.name === 'place' && key(c.args.x, c.args.y, c.args.z) === key(plan.service)))
  for (const p of [...plan.gate, plan.serviceFoundation]) assert.equal(api.block(p.x, p.y, p.z).solid, true)
  assert.equal(reports.at(-1).closurePending, '')
})
