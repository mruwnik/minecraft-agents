// Nightfall with no bed within 32 blocks: walk to the body's own bed when it is within bed_range (default 200) instead
// of stopping, sleep, and walk back at dawn (card bebf3a5f). The decision is pure (src/lib/sleep.mjs); library/routine.mjs
// only acts on it. No block fakes here: a bed is known from the shared map, not from the world.
import { test as nodeTest } from 'node:test'
import assert from 'node:assert/strict'
import { BED_RANGE, ownBed, nightPlan } from '../src/lib/sleep.mjs'
import { stopAdvice, stopEvent, dayEvent } from '../src/routine.mjs'
import routine from '../library/routine.mjs'
import { fakeApi } from './helpers.mjs'

const test = (name, run) => nodeTest(name, { timeout: 2000 }, run)

const FROM = { x: 0, y: 64, z: 0 }
const myBed = { name: 'tester-bed', kind: 'bed', by: 'Tester', x: 100, y: 64, z: 0 }
const farBed = { name: 'tester-far-bed', kind: 'bed', by: 'Tester', x: 0, y: 64, z: 250 }
const theirBed = { name: 'their-bed', kind: 'bed', by: 'Somebody', x: 3, y: 64, z: 0 }
const hut = { name: 'tester-hut', kind: 'base', by: 'Tester', x: 20, y: 65, z: 20, note: 'bed inside' }

test('bed_range defaults to 200 blocks', () => assert.equal(BED_RANGE, 200))

// ---------------------------------------------------------------- which bed is mine
// The client never learns its spawn bed and a body is restarted most nights, so the last bed slept in is gone with
// the process: the one source that survives is the shared map. A mark of kind=bed by the body itself is its bed;
// bed=<place> names any mark instead; where it last woke this run is the fallback within one run.
for (const [name, places, opts, expected] of [
  ['my own bed mark', [theirBed, myBed], {}, myBed],
  ['somebody else’s bed mark is not mine', [theirBed], {}, null],
  ['a base mark is not a bed, whatever its note says', [hut], {}, null],
  ['the nearest of my bed marks', [farBed, myBed], {}, myBed],
  ['bed=<place> names any mark, a base with a bed inside included', [myBed, hut], { bed: 'tester-hut' }, hut],
  ['bed=<place> nobody marked is nothing', [myBed], { bed: 'nope' }, null],
  ['where I last woke this run, when nothing is marked', [hut], { sleptAt: { x: 40, y: 64, z: 40 } }, { name: 'where you last woke', x: 40, y: 64, z: 40 }],
  ['a mark of mine beats where I last woke', [myBed], { sleptAt: { x: 40, y: 64, z: 40 } }, myBed],
  ['no map at all', undefined, {}, null]
]) {
  test(`ownBed: ${name}`, () => assert.deepEqual(ownBed(places, 'Tester', { ...opts, from: FROM }), expected))
}

// ---------------------------------------------------------------- the decision at nightfall
for (const [name, input, expected] of [
  ['a bed within 32: sleep as before, whatever the map says', { near: true, bed: farBed, from: FROM }, { do: 'sleep' }],
  ['my bed within bed_range: walk there', { near: false, bed: myBed, from: FROM }, { do: 'walk', to: myBed, distance: 100 }],
  ['my bed exactly at bed_range: still a walk', { near: false, bed: { ...myBed, x: 200 }, from: FROM }, { do: 'walk', to: { ...myBed, x: 200 }, distance: 200 }],
  ['my bed beyond bed_range: stop, saying how far', { near: false, bed: farBed, from: FROM }, { do: 'stop', why: 'tester-far-bed is 250 blocks away, beyond bed_range=200' }],
  ['a shorter bed_range', { near: false, bed: myBed, from: FROM, bedRange: 50 }, { do: 'stop', why: 'tester-bed is 100 blocks away, beyond bed_range=50' }],
  ['a longer bed_range', { near: false, bed: farBed, from: FROM, bedRange: 300 }, { do: 'walk', to: farBed, distance: 250 }],
  ['no known bed: stop', { near: false, bed: null, from: FROM }, { do: 'stop', why: 'no bed of yours on the shared map: mark yours (mark name=<you>-bed kind=bed, standing on it) or pass bed=<place>' }],
  ['distance counts height too', { near: false, bed: { ...myBed, x: 0, y: 64 + 30, z: 40 }, from: FROM }, { do: 'walk', to: { ...myBed, x: 0, y: 94, z: 40 }, distance: 50 }]
]) {
  test(`nightPlan: ${name}`, () => assert.deepEqual(nightPlan(input), expected))
}

// ---------------------------------------------------------------- the words
test('stopAdvice: the night stop names bed_range and how to mark a bed', () => {
  assert.equal(stopAdvice('night and no bed within 32 blocks'),
    'put a bed within 32 blocks of the places, or mark your own bed (mark name=<you>-bed kind=bed, standing on it) or pass bed=<place> so the routine walks to it at nightfall when it is within bed_range (default 200) blocks; or quit for the night (./mc quit, then ./mc dawn); then start the routine again')
})

test('stopEvent: a bed detail rides along when there is one, and is absent otherwise', () => {
  assert.deepEqual(Object.keys(stopEvent({ reason: 'night and no bed within 32 blocks', bed: 'x', days: 1 })), ['reason', 'step', 'place', 'advice', 'bed'])
  assert.deepEqual(Object.keys(stopEvent({ reason: 'days', days: 1 })), ['reason', 'step', 'place', 'advice'])
})

test('dayEvent: the night before the day gets one line', () => {
  assert.deepEqual(dayEvent(2, [], 'walked 100 blocks to tester-bed (12s), back at dawn (11s)'),
    { day: 2, places: {}, night: 'walked 100 blocks to tester-bed (12s), back at dawn (11s)' })
  assert.deepEqual(dayEvent(1, []), { day: 1, places: {} })
})

// ---------------------------------------------------------------- the routine acting on it
const NIGHT = 'night and no bed within 32 blocks'
const handBack = reason => Object.assign(new Error(reason), { reason })
// the runner's checkpoint as the fake sees it: hands back "night and no bed within 32" on the nth call and sleeps
// (returns) on every other, since the routine has walked to a bed by then
const nightAt = (api, ...nights) => {
  let calls = 0
  api.checkpoint = async () => {
    // An incorrect night fixture must fail promptly even if an infinite microtask loop starves the test timeout.
    if (++calls > 100) throw new Error('bedwalk fixture exceeded 100 checkpoints without stopping')
    if (nights.includes(calls)) throw handBack(NIGHT)
  }
}
const farm = { name: 'a', by: 'Tester', kind: 'farm', note: 'wheat', x: 10, y: 64, z: 10 }
const answers = { 'farm.maintain': {} }
const walks = calls => calls.filter(c => c.startsWith('goto'))
const noNotes = calls => calls.filter(c => !c.startsWith('note'))

test('routine: at nightfall with my bed within bed_range it walks there, sleeps, walks back to the first place at dawn and runs the next day', async () => {
  const { api, calls, events } = fakeApi({ places: [farm, myBed], answers })
  nightAt(api, 3) // checkpoints: one maintenance step, the day's end, then dusk
  const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a', days: 2 })
  assert.deepEqual(noNotes(calls), [
    'farm.maintain place=a reserve_for=a',
    'goto x=100 y=64 z=0 range=2', 'goto x=10 y=64 z=10 range=3',
    'farm.maintain place=a reserve_for=a'
  ])
  assert.deepEqual([summary.days, summary.bedWalks], [2, 1])
  assert.deepEqual(events.map(e => e.type), ['routine_day', 'routine_bed_walk', 'routine_bed_walk', 'routine_day', 'routine_stopped'])
  const [there, back] = events.filter(e => e.type === 'routine_bed_walk')
  assert.deepEqual([there, back].map(({ seconds, ...rest }) => rest), [
    { type: 'routine_bed_walk', leg: 'bed', bed: 'tester-bed', from: '0,64,0', to: '100,64,0', distance: 100 },
    { type: 'routine_bed_walk', leg: 'back', bed: 'tester-bed', from: '100,64,0', to: '10,64,10', distance: 91 }
  ])
  assert.deepEqual([typeof there.seconds, typeof back.seconds], ['number', 'number'])
  assert.deepEqual(events[3].night, 'walked 100 blocks to tester-bed (0s), back at dawn (0s)')
})

test('routine: with no place, dawn walks back to where nightfall found it', async () => {
  const { api, calls } = fakeApi({ places: [myBed], answers: { 'farm.compost': {} } })
  nightAt(api, 3)
  await routine.run(api, { steps: [{ action: 'farm.compost' }], days: 2 })
  assert.deepEqual(walks(calls), ['goto x=100 y=64 z=0 range=2', 'goto x=0 y=64 z=0 range=3'])
})

test('routine: bed=<place> walks to that mark, and bed_range= is honoured', async () => {
  const { api, calls } = fakeApi({ places: [farm, hut], answers })
  nightAt(api, 2)
  await routine.run(api, { name: 'farmer/homestead', place: 'a', days: 2, bed: 'tester-hut', bed_range: 40 })
  assert.deepEqual(walks(calls), ['goto x=20 y=65 z=20 range=2', 'goto x=10 y=64 z=10 range=3'])
})

test('routine: bed=<place> nobody marked is refused before day one', async () => {
  const { api, calls } = fakeApi({ places: [farm], answers })
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a', bed: 'nope' }), /no place called nope on the shared map/)
  assert.deepEqual(calls, [])
})

test('routine: my bed beyond bed_range stops the routine, the stop saying how far and the advice naming bed_range', async () => {
  const { api, calls, events } = fakeApi({ places: [farm, myBed], answers })
  nightAt(api, 2)
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a', days: 0, bed_range: 50 }), /night and no bed within 32 blocks/)
  assert.deepEqual(walks(calls), [])
  assert.deepEqual(events.at(-1), {
    type: 'routine_stopped', reason: NIGHT, step: null, place: null, advice: stopAdvice(NIGHT), bed: 'tester-bed is 100 blocks away, beyond bed_range=50'
  })
})

test('routine: no bed of mine on the map stops the routine as before, saying so', async () => {
  const { api, calls, events } = fakeApi({ places: [farm, theirBed], answers })
  nightAt(api, 2)
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a', days: 0 }), /night and no bed within 32 blocks/)
  assert.deepEqual(walks(calls), [])
  assert.equal(events.at(-1).bed, 'no bed of yours on the shared map: mark yours (mark name=<you>-bed kind=bed, standing on it) or pass bed=<place>')
})

test('routine: a walk to the bed that fails stops the routine with the night reason and the walk’s error', async () => {
  const { api, events } = fakeApi({ places: [farm, myBed], answers: { ...answers, goto: new Error('goto: no path to the goal') } })
  nightAt(api, 2)
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a', days: 0 }), /night and no bed within 32 blocks/)
  assert.deepEqual([events.at(-1).reason, events.at(-1).bed], [NIGHT, 'the walk to tester-bed failed: goto: no path to the goal'])
})

test('routine: still no bed within 32 after the walk hands back once, not a second walk', async () => {
  const { api, calls, events } = fakeApi({ places: [farm, myBed], answers })
  nightAt(api, 2, 3)
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a', days: 0 }), /night and no bed within 32 blocks/)
  assert.deepEqual(walks(calls), ['goto x=100 y=64 z=0 range=2'])
  assert.equal(events.at(-1).type, 'routine_stopped')
})

test('routine: a failed walk back at dawn is noted and the day still runs', async () => {
  let gotos = 0
  const { api, calls } = fakeApi({ places: [farm, myBed], answers: { ...answers, goto: () => { if (++gotos === 2) throw new Error('goto: no path to the goal') } } })
  nightAt(api, 2)
  const got = await routine.run(api, { name: 'farmer/homestead', place: 'a', days: 2 })
  assert.equal(got.days, 2)
  assert.deepEqual(calls.filter(c => /^note .*walk back/.test(c)), ['note the walk back to a failed (goto: no path to the goal): the steps walk to their places themselves'])
})

test('routine: where it last woke this run is the bed when nothing is marked', async () => {
  // night 1 is slept where the runner finds a bed (checkpoint returns); the routine notes where it woke. Night 2 the
  // runner finds none: the routine walks back to where it woke
  const { api, calls } = fakeApi({ places: [farm], answers })
  // pos() is first read after the first night's checkpoint returns (where it woke), then at the second nightfall
  const spots = [{ x: 55, y: 64, z: 5 }]
  api.pos = () => spots.shift() ?? { x: 0, y: 64, z: 0 }
  nightAt(api, 6)
  await routine.run(api, { name: 'farmer/homestead', place: 'a', days: 3 })
  assert.deepEqual(walks(calls), ['goto x=55 y=64 z=5 range=2', 'goto x=10 y=64 z=10 range=3'])
})

test('routine: takes bed= and bed_range= and says so in its doc', () => {
  assert.deepEqual([routine.args.bed, routine.args.bed_range], ['string', 'number'])
  assert.match(routine.doc, /bed_range/)
})
