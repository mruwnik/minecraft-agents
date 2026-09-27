import { test } from 'node:test'
import assert from 'node:assert/strict'
import stage from '../library/boat/stage.mjs'

const uuid = 'c071f7d4-8b43-4f01-9c2f-92b648d3d143'
const otherUuid = '87b3392e-ae93-4f51-bf07-2f53add88880'

function stageApi ({ passengerUuid = uuid, leashHolderId = 12, unknownCell, blockedHeadCell, boatPosition = { x: 0.5, y: 62.5, z: 1.5 }, bodyPosition = { x: 7.5, y: 62.5, z: 1.5 }, revokeLeashAfterState = 0, maxUntilIterations = 100 } = {}) {
  let boat = { ...boatPosition }
  const body = { ...bodyPosition }
  let swims = 0
  let springPending = false
  let springDelay = 0
  const calls = []
  const reports = []
  const same = (p, x, y, z) => p && p.x === x && p.y === y && p.z === z
  const block = (x, y, z) => {
    if (same(unknownCell, x, y, z)) return null
    if (same(blockedHeadCell, x, y, z)) return { name: 'stone', solid: true }
    if (y === 62) return { name: 'water', solid: false }
    if (y === 61) return { name: 'stone', solid: true }
    if (y === 63) return { name: 'air', solid: false }
    return y < 61 ? { name: 'stone', solid: true } : { name: 'air', solid: false }
  }
  const api = {
    pos: () => ({ ...body }), block,
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'boat_state') {
        if (revokeLeashAfterState && calls.filter(c => c.name === 'boat_state').length === revokeLeashAfterState) leashHolderId = 77
        if (springPending) {
          if (springDelay > 0) springDelay--
          else {
            boat.z = 0.2 // small delayed server spring after alignment
            springPending = false
          }
        }
        return { selfId: 12, mounted: null, boats: [{ id: 8, exact: `${boat.x},${boat.y},${boat.z}`, leashHolderId, passengers: [{ id: 42, uuid: passengerUuid, name: 'villager', width: 0.6, height: 1.95, baby: false }] }] }
      }
      if (name === 'boat_swim') {
        swims++
        boat.x += Math.sign(args.x - boat.x)
        boat.z += Math.sign(args.z - boat.z) * Math.min(1.5, Math.abs(args.z - boat.z))
        if (swims === 2) { springPending = true; springDelay = 1 }
        return { from: 'previous', to: `${boat.x},${boat.y},${boat.z}` }
      }
      if (name === 'goto') { body.x = args.x + 0.5; body.y = args.y; body.z = args.z + 0.5; return {} }
      throw new Error(`unexpected action ${name}`)
    },
    until: async (predicate, options) => {
      assert.equal(options.timeout, 90)
      assert.equal(options.every, 0.5)
      for (let i = 0; i < maxUntilIterations; i++) if (await predicate()) return
      throw new Error(options.what)
    },
    report: value => reports.push(value)
  }
  return { api, calls, reports, get swims () { return swims }, get boat () { return { ...boat } } }
}

test('boat.stage plans an observed hull route without moving or attaching anything', async () => {
  const run = stageApi()
  const result = await stage.run(run.api, { uuid, boat: 8, z: 0, minX: 0, plan: true })
  assert.equal(result.planned, true)
  assert.equal(result.aligned, false)
  assert.ok(result.route.length > 1)
  assert.deepEqual(run.calls.map(c => c.name), ['boat_state'])
})

test('boat.stage plans a supported start stance read only and moves there before alignment feedback', async () => {
  const start = { startX: 2, startY: 62, startZ: 1 }
  const planned = stageApi()
  const result = await stage.run(planned.api, { uuid, boat: 8, z: 1.5, ...start, plan: true })
  assert.deepEqual(result.start, { x: 2, y: 62, z: 1 })
  assert.deepEqual(planned.calls.map(c => c.name), ['boat_state'], 'plan validates without moving toward the stance')

  const run = stageApi()
  const finished = await stage.run(run.api, { uuid, boat: 8, z: 1.5, ...start })
  assert.equal(finished.aligned, true)
  assert.deepEqual(run.calls.filter(c => c.name === 'goto')[0].args, { x: 2, y: 62, z: 1, range: 0, into: true })
  assert.ok(run.calls.findIndex(c => c.name === 'goto') < run.calls.findIndex(c => c.name === 'boat_swim' || c.name === 'boat_state' && run.calls.indexOf(c) > 0), 'the operator reaches the shallow-water stance before alignment feedback')
  assert.equal(run.swims, 0, 'already aligned boat needs no extra tension stroke after moving to the stance')
})

test('boat.stage refuses an unsafe start stance before boat movement', async () => {
  const run = stageApi()
  await assert.rejects(stage.run(run.api, { uuid, boat: 8, z: 0, startX: 2, startY: 63, startZ: 1 }), /loaded shallow water stance/)
  assert.deepEqual(run.calls, [], 'invalid footing is refused before boat_state, goto, or swim')
})

test('boat.stage steers by actual boat feedback, removes residual lead tension, and waits for stable alignment', async () => {
  const run = stageApi()
  const result = await stage.run(run.api, { uuid, boat: 8, z: 0, minX: 0 })
  assert.equal(result.aligned, true)
  assert.ok(Math.abs(run.boat.z) <= 0.3)
  assert.ok(run.swims >= 2, 'the boat corrects its lateral offset and then reduces lead tension')
  const finalGap = Math.hypot(7.5 - run.boat.x, 1.5 - run.boat.z)
  assert.ok(finalGap <= 5.5, 'the returned aligned boat has lead slack')
  assert.ok(run.calls.filter(c => c.name === 'boat_state').length >= 8, 'success requires repeated stable boat observations after spring motion')
})

test('boat.stage refuses a missing passenger, a foreign leash, or a westward boundary crossing before swimming', async () => {
  for (const [options, args, expected] of [
    [{ passengerUuid: otherUuid }, { uuid, boat: 8, z: 0 }, /exactly one passenger|not in boat 8/],
    [{ leashHolderId: 77 }, { uuid, boat: 8, z: 0 }, /staging requires exactly the named passenger/],
    [{}, { uuid, boat: 8, z: 0, minX: 1 }, /west of the safe staging boundary/]
  ]) {
    const run = stageApi(options)
    await assert.rejects(stage.run(run.api, args), expected)
    assert.equal(run.calls.some(c => c.name === 'boat_swim'), false)
  }
})

test('boat.stage refuses unknown or obstructed leader corridor cells before applying a stroke', async () => {
  for (const options of [
    { unknownCell: { x: 7, y: 62, z: 1 } },
    { blockedHeadCell: { x: 7, y: 63, z: 1 } }
  ]) {
    const run = stageApi(options)
    await assert.rejects(stage.run(run.api, { uuid, boat: 8, z: 0 }), /leader corridor is not clear open water/)
    assert.equal(run.calls.some(c => c.name === 'boat_swim'), false)
  }
})

test('boat.stage holds at a too-close endpoint until its bounded timeout without issuing an invalid stroke', async () => {
  // The predicted lateral target is exactly the operator position; the live boat is still misaligned.
  const run = stageApi({
    boatPosition: { x: 0.5, y: 62.5, z: 7.5 },
    bodyPosition: { x: 2.5, y: 62.5, z: 0 },
    maxUntilIterations: 8
  })
  await assert.rejects(stage.run(run.api, { uuid, boat: 8, z: 0 }), /did not settle aligned/)
  assert.equal(run.swims, 0, 'the stage command holds and observes instead of sending a too-close boat_swim target')
  assert.equal(run.calls.filter(c => c.name === 'boat_state').length, 9, 'each hold iteration rechecks fresh boat position')
})

test('boat.stage rechecks the exact passenger and leash during a no-stroke endpoint hold', async () => {
  const run = stageApi({
    boatPosition: { x: 0.5, y: 62.5, z: 7.5 },
    bodyPosition: { x: 2.5, y: 62.5, z: 0 },
    revokeLeashAfterState: 2
  })
  await assert.rejects(stage.run(run.api, { uuid, boat: 8, z: 0 }), /staging requires exactly the named passenger and a lead held/)
  assert.equal(run.swims, 0)
})
