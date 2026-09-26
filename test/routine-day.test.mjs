// The routine's day line and its place list. A step's summary is cut to 120 characters for the routine_day event, and
// on 09-26 the fields that said what went wrong (stuck=, missing=, bare=) stood behind lowSlabs= and clutter= and were
// the part cut off: a field read as maintained for a day and a half while its summary said otherwise past the cut.
// And a homestead is several plans: $places in a step is every place the routine was given, so farm.maintain can
// keep the seed the OTHER plans sow (card b22fa1c9).
import test from 'node:test'
import assert from 'node:assert/strict'
import { dayEvent, outcomeText, routinePlan } from '../src/routine.mjs'
import { routineSteps } from '../src/lib.mjs'

const long = 'x'.repeat(200)
for (const [name, outcome, expected] of [
  ['a failure is the whole line', { failed: 'no hoe' }, 'FAILED no hoe'],
  ['a short summary is left as it is', { sweeps: 1, replanted: 3 }, 'ok sweeps=1 replanted=3'],
  ['what went wrong comes first, in a fixed order, whatever order it was said in',
    { sweeps: 1, bare: '2 (untilled:2 no hoe)', replanted: 3, stuck: 'till: no hoe', missing: 'wheat_seeds:2', stopped: 'twice in a row' },
    'ok stopped=twice in a row stuck=till: no hoe missing=wheat_seeds:2 bare=2 (untilled:2 no hoe) sweeps=1 replanted=3'],
  ['a full store is what went wrong too, after the rest of them',
    { sweeps: 1, storage_full: 'wheat:40 carried', stuck: 'till: no hoe', deposited: 'wheat:64@1,64,0' },
    'ok stuck=till: no hoe storage_full=wheat:40 carried sweeps=1 deposited=wheat:64@1,64,0'],
  ['the cut falls on the rest, never on what went wrong',
    { sweeps: 1, lowSlabs: long, bare: '2 (untilled:2 no hoe)' },
    `ok bare=2 (untilled:2 no hoe) sweeps=1 lowSlabs=${'x'.repeat(120 - 'ok bare=2 (untilled:2 no hoe) sweeps=1 lowSlabs='.length)}`],
  ['what went wrong is never cut, even past the limit by itself',
    { stuck: long, sweeps: 1 },
    `ok stuck=${long}`]
]) {
  test(`outcomeText: ${name}`, () => assert.equal(outcomeText(outcome), expected))
}

test('dayEvent: a step summary keeps its failure fields inside the cut', () => {
  const event = dayEvent(1, [{ action: 'farm.maintain', place: 'f', outcome: { sweeps: 1, clutter: long, bare: '1 (no seed:1 wheat_seeds)' } }])
  assert.match(event.places.f['farm.maintain'], /^ok bare=1 \(no seed:1 wheat_seeds\) sweeps=1 clutter=x+$/)
  assert.equal(event.places.f['farm.maintain'].length, 120)
})

// ---------------------------------------------------------------- $places
const STEP = { action: 'farm.maintain', place: '$place', reserve_for: '$places' }
for (const [name, args, expected] of [
  ['one place: $places is that place', { steps: [STEP], place: 'a', places: 'a' }, [{ action: 'farm.maintain', place: 'a', reserve_for: 'a' }]],
  ['several: every place the routine runs over, whichever one this round is', { steps: [STEP], place: 'b', places: 'a,b,c' }, [{ action: 'farm.maintain', place: 'b', reserve_for: 'a,b,c' }]],
  ['no places given: the argument is dropped so the action keeps its own default', { steps: [{ action: 'farm.maintain', reserve_for: '$places' }] }, [{ action: 'farm.maintain' }]]
]) {
  test(`routineSteps: ${name}`, () => assert.deepEqual(routineSteps(args, () => null).steps, expected))
}

test('routinePlan: every round of a homestead carries the whole place list', () => {
  const { steps } = routinePlan({ steps: [STEP], place: 'a, b' }, () => null)
  assert.deepEqual(steps, [
    { action: 'farm.maintain', place: 'a', reserve_for: 'a,b' },
    { action: 'farm.maintain', place: 'b', reserve_for: 'a,b' }
  ])
})

test('the farmer routine keeps the seed of every plan of the homestead', async () => {
  const { default: fs } = await import('node:fs')
  const steps = JSON.parse(fs.readFileSync(new URL('../roles/farmer/homestead.json', import.meta.url), 'utf8'))
  const { steps: filled } = routinePlan({ steps, place: 'melon-patch,crop-field' }, () => null)
  assert.deepEqual(filled.filter(s => s.action === 'farm.maintain').map(s => s.reserve_for), ['melon-patch,crop-field', 'melon-patch,crop-field'])
})

// deposit= in a shipped routine is $store: store= on the routine fills it, and a routine given none leaves the step to
// the action's own default (the plan's chests)
for (const [name, args, expected] of [
  ['store= fills $store', { steps: [{ action: 'farm.maintain', place: '$place', deposit: '$store' }], place: 'a', store: 'barn' }, [{ action: 'farm.maintain', place: 'a', deposit: 'barn' }]],
  ['no store=: the step keeps its default', { steps: [{ action: 'farm.maintain', place: '$place', deposit: '$store' }], place: 'a' }, [{ action: 'farm.maintain', place: 'a' }]],
  ['vars= can say it too', { steps: [{ action: 'farm.maintain', place: '$place', deposit: '$store' }], place: 'a', vars: { store: '1,2,3' } }, [{ action: 'farm.maintain', place: 'a', deposit: '1,2,3' }]]
]) {
  test(`routineSteps: ${name}`, () => assert.deepEqual(routineSteps(args, () => null).steps, expected))
}

test('every shipped routine that stores its produce takes the store from store=', async () => {
  const { default: fs } = await import('node:fs')
  const roles = ['farmer/homestead', 'rancher/sheep', 'rancher/cattle', 'rancher/pigs', 'rancher/chickens', 'beekeeper/apiary']
  const stores = roles.map(role => JSON.parse(fs.readFileSync(new URL(`../roles/${role}.json`, import.meta.url), 'utf8'))
    .filter(step => 'deposit' in step).map(step => step.deposit))
  assert.deepEqual(stores, [['$store'], ['$store'], ['$store'], ['$store'], ['$store'], ['$store']])
})

// the stuck watch is told which days ended with a full store (src/stuck.mjs raises the alert after two in a row)
test('a routine day whose step reports storage_full is told to the stuck watch', async () => {
  const { default: routine } = await import('../library/routine.mjs')
  const { fakeApi } = await import('./helpers.mjs')
  const { api, progress } = fakeApi({ answers: { 'farm.maintain': { sweeps: 1, storage_full: 'wheat:40 carried' } } })
  await routine.run(api, { steps: [{ action: 'farm.maintain' }], days: 2 })
  assert.deepEqual(progress.routine.storageFull, [1, 2])
})
