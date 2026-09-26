// routine place=a,b,c: a farmstead is several fields, and one routine a day should cover all of them (card cd4c1262).
// The pure half lives in src/routine.mjs; library/routine.mjs only calls it. Nothing here touches src/lib.mjs.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { placeList, routinePlan, placesRefusal, stopAdvice } from '../src/routine.mjs'
import routine from '../library/routine.mjs'
import { fakeApi } from './helpers.mjs'

const ROLE_FILES = {
  'farmer/homestead': '[{"action":"farm.tidy","place":"$place"},{"action":"farm.maintain","place":"$place","deposit":true,"compost":"$compost"}]',
  'farmer/chores': '[{"action":"farm.compost"}]'
}
const readRole = name => ROLE_FILES[name] ?? null
// farm.maintain feeds the composter itself now (compost=), so the day's own routine has no separate farm.compost step;
// a $compost nobody gave in vars= is dropped, so farm.maintain falls back to the plan's own K cell
// the shipped roles/farmer/homestead.json also carries reserve_for=$places (the whole place= list), the fake here does not
const homestead = (place, compost, places) => [
  { action: 'farm.tidy', place },
  { action: 'farm.maintain', place, deposit: true, ...(compost !== undefined ? { compost } : {}), ...(places !== undefined ? { reserve_for: places } : {}) }
]

for (const [name, place, expected] of [
  ['one place', 'a', ['a']],
  ['two places', 'a,b', ['a', 'b']],
  ['three places, in the order given', 'c,a,b', ['c', 'a', 'b']],
  ['spaces round the commas', ' a , b ', ['a', 'b']],
  ['a trailing comma', 'a,b,', ['a', 'b']],
  ['no place at all', undefined, []],
  ['a list already', ['a', 'b'], ['a', 'b']]
]) {
  test(`placeList: ${name}`, () => assert.deepEqual(placeList(place), expected))
}

for (const [name, args, expected] of [
  ['one place is the routine as it was', { name: 'farmer/homestead', place: 'a' }, { places: ['a'], steps: homestead('a') }],
  ['two places run the routine twice, in order', { name: 'farmer/homestead', place: 'a,b' }, { places: ['a', 'b'], steps: [...homestead('a'), ...homestead('b')] }],
  ['three places run it three times, in the order given', { name: 'farmer/homestead', place: 'c,a,b' }, { places: ['c', 'a', 'b'], steps: [...homestead('c'), ...homestead('a'), ...homestead('b')] }],
  ['steps= with $place fills each place too', { steps: [{ action: 'farm.tidy', place: '$place' }], place: 'a,b' }, { places: ['a', 'b'], steps: [{ action: 'farm.tidy', place: 'a' }, { action: 'farm.tidy', place: 'b' }] }],
  ['no place: the routine once, as before', { steps: [{ action: 'farm.compost' }] }, { places: [], steps: [{ action: 'farm.compost' }] }],
  ['a routine that needs a place and is given none', { name: 'farmer/homestead' }, { error: 'routine name=farmer/homestead needs place=<the name of a marked farm> to work on' }],
  ['several places for a routine that works on none', { name: 'farmer/chores', place: 'a,b' }, { error: 'roles/farmer/chores.json has no $place to fill: place=a,b would run the same steps 2 times' }],
  ['a name nobody ships', { name: 'farmer/nope', place: 'a,b' }, { error: 'no routine called farmer/nope (roles/farmer/nope.json)' }],
  // any other $name in a step is filled from vars=; one nobody gave is dropped, so the step's own default holds
  ['vars= fills every other $name', { steps: [{ action: 'farm.maintain', place: '$place', compost: '$compost' }], place: 'a', vars: { compost: 'shared-composter' } }, { places: ['a'], steps: [{ action: 'farm.maintain', place: 'a', compost: 'shared-composter' }] }],
  ['a $name with no vars= for it is left out of the step', { steps: [{ action: 'farm.maintain', place: '$place', compost: '$compost' }], place: 'a' }, { places: ['a'], steps: [{ action: 'farm.maintain', place: 'a' }] }],
  ['vars= as the JSON text the command line gives', { steps: [{ action: 'farm.compost', x: '$x' }], vars: '{"x":5}' }, { places: [], steps: [{ action: 'farm.compost', x: 5 }] }],
  ['vars= that is not an object', { steps: [{ action: 'farm.compost' }], vars: '5' }, { error: 'vars= must be an object like {"compost":"shared-composter"}' }],
  // the homestead role's own $compost, shared by every plot from one vars=
  ['farmer/homestead over three plots with one shared composter', { name: 'farmer/homestead', place: 'c,a,b', vars: { compost: 'shared-composter' } }, { places: ['c', 'a', 'b'], steps: [...homestead('c', 'shared-composter'), ...homestead('a', 'shared-composter'), ...homestead('b', 'shared-composter')] }]
]) {
  test(`routinePlan: ${name}`, () => assert.deepEqual(routinePlan(args, readRole), expected))
}

const marked = [
  { name: 'a', by: 'Tester', kind: 'farm', note: 'wheat' },
  { name: 'b', by: 'Somebody', kind: 'farm', note: 'anyone welcome' },
  { name: 'c', by: 'Somebody', kind: 'farm', note: 'carrots' }
]
for (const [name, names, expected] of [
  ['no places asked for', [], null],
  ['my own', ['a'], null],
  ['my own and an inviting one', ['a', 'b'], null],
  ['one nobody marked, named', ['a', 'x'], 'no place called x on the shared map (places lists what is marked): a routine over places you name stops here rather than failing the same way once a round'],
  ['two nobody marked, both named', ['x', 'a', 'y'], 'no place called x, y on the shared map (places lists what is marked): a routine over places you name stops here rather than failing the same way once a round'],
  ['somebody else’s with a silent note, third in the list', ['a', 'b', 'c'], /^c is Somebody's ground and the note on it does not invite work/]
]) {
  test(`placesRefusal: ${name}`, () => {
    const got = placesRefusal(marked, names, 'Tester')
    return expected instanceof RegExp ? assert.match(got, expected) : assert.equal(got, expected)
  })
}

test('routine: place=a,b runs the routine over both fields, a before b', async () => {
  const { api, calls } = fakeApi({ places: marked, answers: { 'farm.tidy': {}, 'farm.maintain': {} } })
  const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a,b' })
  assert.deepEqual([calls.filter(c => !c.startsWith('note')), summary.days, summary.ran], [
    ['farm.tidy place=a', 'farm.maintain place=a deposit reserve_for=a,b',
      'farm.tidy place=b', 'farm.maintain place=b deposit reserve_for=a,b'], 1, 4])
})

test('routine: vars=compost reaches farm.maintain for every plot, one shared composter', async () => {
  const { api, calls } = fakeApi({ places: marked, answers: { 'farm.tidy': {}, 'farm.maintain': {} } })
  await routine.run(api, { name: 'farmer/homestead', place: 'a,b', vars: { compost: 'shared-composter' } })
  assert.deepEqual(calls.filter(c => !c.startsWith('note')), [
    'farm.tidy place=a', 'farm.maintain place=a deposit compost=shared-composter reserve_for=a,b',
    'farm.tidy place=b', 'farm.maintain place=b deposit compost=shared-composter reserve_for=a,b'
  ])
})

test('routine: a place nobody marked stops the round before day one, named', async () => {
  const { api, calls } = fakeApi({ places: marked })
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a,nope,b', days: 3 }), /no place called nope on the shared map/)
  assert.deepEqual(calls, [])
})

test('routine: somebody else’s silent ground anywhere in the list stops the round before day one', async () => {
  const { api, calls } = fakeApi({ places: marked })
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a,b,c', days: 3 }), /c is Somebody's ground/)
  assert.deepEqual(calls, [])
})

test('routine: dry=true prints the expanded steps and runs nothing, whoever owns the ground', async () => {
  const { api, calls } = fakeApi({ places: marked })
  const got = await routine.run(api, { name: 'farmer/homestead', place: 'a,b,c', dry: true })
  assert.deepEqual([got, calls], [{ dry: true, places: ['a', 'b', 'c'], steps: [...homestead('a', undefined, 'a,b,c'), ...homestead('b', undefined, 'a,b,c'), ...homestead('c', undefined, 'a,b,c')] }, []])
})

test('routine: dry=true still names a place nobody marked', async () => {
  const { api } = fakeApi({ places: marked })
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a,nope', dry: true }), /no place called nope/)
})

// ---------------------------------------------------------------- autopilot: days=0, routine_day and routine_stopped (autopilot card)
const handBack = reason => Object.assign(new Error(reason), { reason })
// a checkpoint that hands back (like the runner's) on its nth call
const stopAt = (api, n, error) => {
  let calls = 0
  api.checkpoint = async () => { if (++calls === n) throw error }
}
const answers = { 'farm.tidy': {}, 'farm.maintain': { harvested: { wheat: 12 } } }

test('routine: days=0 runs day after day until the runner hands back, and says why it stopped', async () => {
  const { api, events } = fakeApi({ places: marked, answers })
  stopAt(api, 6, handBack('health 6'))
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a', days: 0 }), /health 6/)
  assert.deepEqual(events.map(e => e.type), ['routine_day', 'routine_stopped'])
  assert.deepEqual(events[1], { type: 'routine_stopped', reason: 'health 6', step: 'farm.maintain place=a', place: 'a', advice: 'the body is hurt: eat to food 18 and rest until health is back, then start the routine again' })
})

test('routine: days=0 takes days off the args it shares with the runner, whose days rule would end it at once', async () => {
  const { api } = fakeApi({ places: marked, answers })
  stopAt(api, 4, handBack('spoken to (Steve: hi)'))
  const a = { name: 'farmer/homestead', place: 'a', days: 0 }
  await assert.rejects(routine.run(api, a))
  assert.equal('days' in a, false)
})

test('routine: days=2 stops after its two days with reason days, one routine_day per day', async () => {
  const { api, events } = fakeApi({ places: marked, answers })
  const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a', days: 2 })
  assert.deepEqual([summary.days, summary.ran], [2, 4])
  assert.deepEqual(events.map(e => [e.type, e.day ?? e.reason]), [['routine_day', 1], ['routine_day', 2], ['routine_stopped', 'days']])
  assert.deepEqual(events[2], { type: 'routine_stopped', reason: 'days', step: null, place: null, advice: 'the routine ran its 2 days: start it again (days=0 runs until stopped) or move on' })
})

test('routine: no days= is one day, as before', async () => {
  const { api, events } = fakeApi({ places: marked, answers })
  const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a' })
  assert.deepEqual([summary.days, events.at(-1).reason, events.at(-1).advice], [1, 'days', 'the routine ran its 1 day: start it again (days=0 runs until stopped) or move on'])
})

test('routine: routine_day says what each step reported per place, failures included', async () => {
  const { api, events } = fakeApi({ places: marked, answers: { ...answers, 'farm.maintain': new Error('farm.maintain: no composter within 24') } })
  await routine.run(api, { name: 'farmer/homestead', place: 'a,b' })
  assert.deepEqual(events[0], {
    type: 'routine_day',
    day: 1,
    places: {
      a: { 'farm.tidy': 'ok', 'farm.maintain': 'FAILED farm.maintain: no composter within 24' },
      b: { 'farm.tidy': 'ok', 'farm.maintain': 'FAILED farm.maintain: no composter within 24' }
    }
  })
})

test('routine: steps with no place report under "here"', async () => {
  const { api, events } = fakeApi({ places: marked, answers: { 'farm.compost': { composted: 3 } } })
  await routine.run(api, { steps: [{ action: 'farm.compost' }] })
  assert.deepEqual(events[0], { type: 'routine_day', day: 1, places: { here: { 'farm.compost': 'ok composted=3' } } })
})

test('routine: the stuck watch is told which day began and which steps failed on which day', async () => {
  const { api, progress } = fakeApi({ places: marked, answers: { ...answers, 'farm.maintain': new Error('farm.maintain: no composter') } })
  await routine.run(api, { name: 'farmer/homestead', place: 'a', days: 3 })
  assert.deepEqual([progress.routine.day, progress.routine.failedSteps, progress.routine.phase, typeof progress.routine.lastDayStartedAt],
    [3, [{ day: 1, step: 'farm.maintain place=a' }, { day: 2, step: 'farm.maintain place=a' }, { day: 3, step: 'farm.maintain place=a' }], 'steps', 'number'])
})

test('routine: a stop between days names no step', async () => {
  const { api, events } = fakeApi({ places: marked, answers })
  stopAt(api, 4, handBack('night and no bed within 32 blocks'))
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a', days: 0 }))
  assert.deepEqual(events.at(-1), {
    type: 'routine_stopped',
    reason: 'night and no bed within 32 blocks',
    step: null,
    place: null,
    advice: stopAdvice('night and no bed within 32 blocks'),
    bed: 'no bed of yours on the shared map: mark yours (mark name=<you>-bed kind=bed, standing on it) or pass bed=<place>'
  })
})

test('routine: takes vars= (any $name but $place in a step) and says so in its doc', () => {
  assert.equal(routine.args.vars, 'any')
  assert.match(routine.doc, /vars=/)
})

for (const [reason, advice] of [
  ['cancelled', /stopped from outside \(\.\/mc stop, or a stall or circling cancel: events type=task_cancelled last=1 says which\)/],
  ['food 4 and nothing edible carried', /fetch or grow food/],
  ['twice in a row: farm.tidy: no path to the goal', /run it by hand and read its FAIL/],
  ['twice in a row: farm.maintain: no hoe to till with', /craft another and carry a spare/],
  ['twice in a row: farm.tidy: the shears broke', /craft another and carry a spare/],
  ['twice in a row: farm.tidy: tool_broke stone_hoe', /craft another and carry a spare/],
  ['spoken to (Steve: come here)', /answer them in chat/],
  ['inventory full and no chest to deposit in', /deposit into a chest/],
  ['until', /its until= minutes are up/],
  ['something nobody foresaw', /read the error, fix what it names/]
]) {
  test(`stopAdvice: ${reason}`, () => assert.match(stopAdvice(reason, 1), advice))
}
