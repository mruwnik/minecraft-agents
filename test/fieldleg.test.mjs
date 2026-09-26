// One walk of a farm sweep (card 72e49b3d). A field that has grown in has no lane the pathfinder likes: the walk to
// the edge, and the walk to the cell a pour or a plant is worked from, answered "no walkable path" or "the search ran
// out of time" every few sweeps on one carrot patch, and the driver's workaround was a goto dig=true a few cells
// further in by hand, then the sweep from there. A leg now does that itself: plain first, and on a path failure once
// more with dig=true, but only when both ends of the leg lie inside the plan's footprint (its cells and the ring it is
// worked from), so the digging stays at the field, and never a plan block (the dig walk refuses everything that looks
// built: farmland, crops, slabs, fences, chests; see looksBuilt). Both failing is one stuck= line naming the cell.
import test from 'node:test'
import assert from 'node:assert/strict'
import { fakeApi } from './helpers.mjs'
import { RING, footprintOf, inFootprint, digRetryRefusal, fieldLeg, PATH_FAILURE } from '../src/fieldleg.mjs'

const cells = [{ x: 0, y: 63, z: 0 }, { x: 2, y: 63, z: 0 }, { x: 0, y: 63, z: 1 }]
const NO_PATH = 'goto: no walkable path (walks don\'t dig or bridge): look for a way round, go in shorter legs, or pass dig=true if breaking and placing blocks on the way is fine'
const TIMED_OUT = 'goto: the search ran out of time (5 s) before it found a way, which is not the same as there being none'

test('the ring round the plan a sweep stands in is the work range, whole cells', () => assert.equal(RING, 5))

test('footprintOf: the x/z box round the cells, grown by the ring', () => {
  assert.deepEqual(footprintOf(cells), { x1: -5, z1: -5, x2: 7, z2: 6 })
})

for (const [name, at, expected] of [
  ['a plan cell', { x: 0, y: 63, z: 0 }, true],
  ['the ring\'s far corner', { x: 7, y: 70, z: 6 }, true],
  ['one past the ring', { x: 8, y: 63, z: 0 }, false],
  ['a body position between cells', { x: -4.7, y: 64.2, z: 2.3 }, true],
  ['any depth counts', { x: 1, y: 12, z: 1 }, true]
]) {
  test(`inFootprint: ${name}`, () => assert.equal(inFootprint(footprintOf(cells), at), expected))
}

for (const [name, error, expected] of [
  ['no walkable path', NO_PATH, true],
  ['the search timed out', TIMED_OUT, true],
  ['the pathfinder\'s own no path', 'no path to the goal: the search found nothing to walk from here', true],
  ['nowhere to stand, judged before the search', 'nowhere to stand within 1 of 3,64,2: every cell in reach is planted (wheat:9)', true],
  ['a hurt hand-back is not a path failure', 'interrupted: hurt (health 6)', false],
  ['a stop is not a path failure', 'cancelled: stop', false]
]) {
  test(`PATH_FAILURE: ${name}`, () => assert.equal(PATH_FAILURE.test(error), expected))
}

const box = footprintOf(cells)
for (const [name, given, expected] of [
  ['a path failure with both ends in the footprint: retry', { error: NO_PATH, from: { x: 1, y: 64, z: 1 }, to: { x: 2, y: 64, z: 1 } }, null],
  ['not a path failure: no retry, no reason (the error stands as it is)', { error: 'interrupted: hurt', from: { x: 1, y: 64, z: 1 }, to: { x: 2, y: 64, z: 1 } }, undefined],
  ['the goal outside the footprint', { error: NO_PATH, from: { x: 1, y: 64, z: 1 }, to: { x: 30, y: 64, z: 1 } }, '30,64,1 is outside the plan\'s footprint'],
  ['the body outside the footprint', { error: TIMED_OUT, from: { x: 20.5, y: 64, z: -9.2 }, to: { x: 2, y: 64, z: 1 } }, 'I stand outside the plan\'s footprint, at 20,64,-10']
]) {
  test(`digRetryRefusal: ${name}`, () => assert.equal(digRetryRefusal({ ...given, box }), expected))
}

// a fake goto: plain walks fail with the error given, dig walks with the second one (or succeed when there is none)
const walker = (plain, dug = null) => ({ goto: args => { const why = args.dig === true ? dug : plain; if (why) throw new Error(why); return { pos: { x: args.x, y: args.y, z: args.z } } } })
const leg = { x: 2, y: 64, z: 1, range: 0 }

test('fieldLeg: a plain walk that arrives is the whole leg', async () => {
  const { api, calls } = fakeApi({ answers: walker(null) })
  const r = await fieldLeg(api, leg, box)
  assert.deepEqual(calls, ['goto x=2 y=64 z=1 range=0'])
  assert.equal(r.dug, undefined)
})

test('fieldLeg: a path failure inside the footprint is walked once more with dig=true, and the result says so', async () => {
  const { api, calls } = fakeApi({ answers: walker(NO_PATH) })
  const r = await fieldLeg(api, leg, box)
  assert.deepEqual(calls, ['goto x=2 y=64 z=1 range=0', 'goto x=2 y=64 z=1 range=0 dig'])
  assert.equal(r.dug, '2,64,1')
})

test('fieldLeg: both walks failing is one line naming the cell and both answers, and no third walk', async () => {
  const { api, calls } = fakeApi({ answers: walker(NO_PATH, TIMED_OUT) })
  await assert.rejects(fieldLeg(api, leg, box), { message: `2,64,1: ${NO_PATH}; with dig=true inside the plan's footprint: ${TIMED_OUT}` })
  assert.equal(calls.length, 2)
})

test('fieldLeg: a goal outside the footprint is not dug to', async () => {
  const { api, calls } = fakeApi({ answers: walker(NO_PATH) })
  await assert.rejects(fieldLeg(api, { x: 30, y: 64, z: 1, range: 2 }, box), { message: `30,64,1: ${NO_PATH} (no dig=true retry: 30,64,1 is outside the plan's footprint)` })
  assert.deepEqual(calls, ['goto x=30 y=64 z=1 range=2'])
})

test('fieldLeg: a body outside the footprint does not dig its way in', async () => {
  const { api, calls } = fakeApi({ answers: walker(NO_PATH) })
  api.pos = () => ({ x: 40, y: 64, z: 0 })
  await assert.rejects(fieldLeg(api, leg, box), { message: `2,64,1: ${NO_PATH} (no dig=true retry: I stand outside the plan's footprint, at 40,64,0)` })
  assert.equal(calls.length, 1)
})

test('fieldLeg: a failure that is not about the path is the primitive\'s own, once, as it was', async () => {
  const { api, calls } = fakeApi({ answers: walker('interrupted: hurt (health 6)') })
  await assert.rejects(fieldLeg(api, leg, box), { message: 'interrupted: hurt (health 6)' })
  assert.equal(calls.length, 1)
})

// ---------------------------------------------------------------- the sweep's own legs
// a plan at y=63: the body walks at 64 on dirt; one bed of growing wheat anchors the plan, the other is empty farmland
import { parsePlan, planCells, planBill } from '../src/lib.mjs'
import maintainFarm from '../library/farm/maintain.mjs'

const fakePlace = plan => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan, parsed: parsePlan(plan), cells: planCells({ plan, x: 0, y: 63, z: 0 }), bill: planBill(parsePlan(plan)) })
const field = () => {
  const world = {}
  for (let x = -6; x <= 8; x++) {
    for (let z = -6; z <= 6; z++) {
      world[`${x},62,${z}`] = 'dirt'
      world[`${x},63,${z}`] = 'dirt'
      world[`${x},64,${z}`] = 'air'
      world[`${x},65,${z}`] = 'air'
    }
  }
  return { ...world, '0,63,0': 'farmland', '0,64,0': 'wheat#3', '1,63,0': 'farmland' }
}
// the walk to the field's edge (range 1) arrives; the walk to the cell the plant is worked from (range 0) fails as told
const sweep = async goto => {
  const legs = args => args.range === 0 ? goto(args) : {}
  const made = fakeApi({ place: fakePlace('ww'), world: field(), items: { wheat_seeds: 5 }, answers: { 'farm.harvest': { harvested: {}, replanted: 0 }, goto: legs } })
  const summary = await maintainFarm.run(made.api, { place: 'test-field' })
  return { summary, walks: made.calls.filter(c => /^(goto|place) /.test(c)) }
}

test('farm.maintain: a walk to the cell a plant is worked from fails on the path: once more with dig=true, then the plant', async () => {
  const { summary, walks } = await sweep(walker(NO_PATH).goto)
  assert.deepEqual(walks.filter(c => c.includes('range=0')), ['goto x=1 y=64 z=-1 range=0', 'goto x=1 y=64 z=-1 range=0 dig'])
  assert.deepEqual(walks.filter(c => c.startsWith('place')), ['place item=wheat_seeds x=1 y=64 z=0'])
  assert.equal(summary.stuck, undefined)
  assert.equal(summary.dug, '1,64,-1')
})

test('farm.maintain: both walks failing is a stuck= line naming the cell, and the bed is counted bare as unreachable', async () => {
  const { summary, walks } = await sweep(walker(NO_PATH, TIMED_OUT).goto)
  assert.equal(walks.filter(c => c.includes('range=0')).length, 2)
  assert.equal(summary.stuck, `1,64,-1: ${NO_PATH}; with dig=true inside the plan's footprint: ${TIMED_OUT}`)
  assert.equal(summary.bare, '1 (unreachable:1 1,64,0)')
})
