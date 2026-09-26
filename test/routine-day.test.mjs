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
