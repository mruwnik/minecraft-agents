// routine place=a,b,c: a farmstead is several fields, and one routine a day should cover all of them (card cd4c1262).
// The pure half lives in src/routine.mjs; library/routine.mjs only calls it. Nothing here touches src/lib.mjs.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { placeList, routinePlan, placesRefusal, stopAdvice, rekitVerdict } from '../src/routine.mjs'
import routine from '../library/routine.mjs'
import { fakeApi } from './helpers.mjs'

const ROLE_FILES = {
  'farmer/homestead': '[{"action":"kit","tools":"stone_hoe","food":12,"place":"$place"},{"action":"farm.tidy","place":"$place"},{"action":"farm.maintain","place":"$place","deposit":"$store","compost":"$compost"}]',
  'farmer/chores': '[{"action":"farm.compost"}]'
}
const readRole = name => ROLE_FILES[name] ?? null
// farm.maintain feeds the composter itself now (compost=), so the day's own routine has no separate farm.compost step;
// a $compost nobody gave in vars= is dropped, so farm.maintain falls back to the plan's own K cell
// the shipped roles/farmer/homestead.json also carries reserve_for=$places (the whole place= list) and deposit=$store
// (dropped when no store= is given, so farm.maintain's default, the plan's chests, holds); the fake here does not
const homestead = (place, compost, places) => [
  { action: 'kit', tools: 'stone_hoe', food: 12, place },
  { action: 'farm.tidy', place },
  { action: 'farm.maintain', place, ...(compost !== undefined ? { compost } : {}), ...(places !== undefined ? { reserve_for: places } : {}) }
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
    ['farm.maintain place=a reserve_for=a,b', 'farm.maintain place=b reserve_for=a,b'], 1, 2])
})

test('routine: vars=compost reaches farm.maintain for every plot, one shared composter', async () => {
  const { api, calls } = fakeApi({ places: marked, answers: { 'farm.tidy': {}, 'farm.maintain': {} } })
  await routine.run(api, { name: 'farmer/homestead', place: 'a,b', vars: { compost: 'shared-composter' } })
  assert.deepEqual(calls.filter(c => !c.startsWith('note')), [
    'farm.maintain place=a compost=shared-composter reserve_for=a,b',
    'farm.maintain place=b compost=shared-composter reserve_for=a,b'
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
  assert.deepEqual([got, calls], [{ dry: true, places: ['a', 'b', 'c'], steps: ['a', 'b', 'c'].map(place => ({ action: 'farm.maintain', place, reserve_for: 'a,b,c' })) }, []])
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
  stopAt(api, 4, handBack('health 6'))
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
  assert.deepEqual([summary.days, summary.ran], [2, 2])
  assert.deepEqual(events.map(e => [e.type, e.day ?? e.reason]), [['routine_day', 1], ['routine_day', 2], ['routine_stopped', 'days']])
  assert.deepEqual(events[2], { type: 'routine_stopped', reason: 'days', step: null, place: null, advice: 'the routine ran its 2 days: start it again (days=0 runs until stopped) or move on' })
})

test('routine: no days= is one day, as before', async () => {
  const { api, events } = fakeApi({ places: marked, answers })
  const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a' })
  assert.deepEqual([summary.days, events.at(-1).reason, events.at(-1).advice], [1, 'days', 'the routine ran its 1 day: start it again (days=0 runs until stopped) or move on'])
})

test('routine: routine_day says what each step reported per place, failures included', async () => {
  const { api, events } = fakeApi({ places: marked, answers: { ...answers, 'work': new Error('work: no composter within 24') } })
  await routine.run(api, { steps: [{ action: 'work', place: '$place' }], place: 'a,b' })
  assert.deepEqual(events[0], {
    type: 'routine_day',
    day: 1,
    places: {
      a: { 'work': 'FAILED work: no composter within 24' },
      b: { 'work': 'FAILED work: no composter within 24' }
    }
  })
})

test('routine: steps with no place report under "here"', async () => {
  const { api, events } = fakeApi({ places: marked, answers: { 'farm.compost': { composted: 3 } } })
  await routine.run(api, { steps: [{ action: 'farm.compost' }] })
  assert.deepEqual(events[0], { type: 'routine_day', day: 1, places: { here: { 'farm.compost': 'ok composted=3' } } })
})

test('routine: the stuck watch is told which day began and which steps failed on which day', async () => {
  const { api, progress } = fakeApi({ places: marked, answers: { ...answers, 'work': new Error('work: no composter') } })
  await routine.run(api, { steps: [{ action: 'work', place: '$place' }], place: 'a', days: 3 })
  assert.deepEqual([progress.routine.day, progress.routine.failedSteps, progress.routine.phase, typeof progress.routine.lastDayStartedAt],
    [3, [{ day: 1, step: 'work place=a' }, { day: 2, step: 'work place=a' }, { day: 3, step: 'work place=a' }], 'steps', 'number'])
})

test('routine: a stop between days names no step', async () => {
  const { api, events } = fakeApi({ places: marked, answers })
  stopAt(api, 2, handBack('night and no bed within 32 blocks'))
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

for (const failure of [new Error('unknown farm failure'), { stopped: 'health 4' }, { stopped: 'twice in a row: goto: no path' }]) {
  test(`routine: a fatal farm result stops before the next field (${failure.message ?? failure.stopped})`, async () => {
    const { api, calls, events } = fakeApi({ places: marked, answers: { 'farm.maintain': failure } })
    await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a,b' }), /unknown farm failure|health 4|twice in a row/)
    assert.deepEqual(calls.filter(c => c.startsWith('farm.maintain')), ['farm.maintain place=a reserve_for=a,b'])
    assert.equal(events.at(-1).type, 'routine_stopped')
  })
}

test('routine: recoverable farm attention continues to the next field', async () => {
  const { api, calls, events } = fakeApi({ places: marked, answers: { 'farm.maintain': { bare: '2 (no seed:2 wheat_seeds)' } } })
  const result = await routine.run(api, { name: 'farmer/homestead', place: 'a,b' })
  assert.equal(result.ran, 2)
  assert.equal(calls.filter(c => c.startsWith('farm.maintain')).length, 2)
  assert.match(events[0].places.a['farm.maintain'], /^incomplete bare=/)
})

for (const repeated of [false, true]) {
  test(`routine: nested farm night hand-back ${repeated ? 'is retried only once' : 'sleeps then retries the same field'}`, async () => {
    let worked = 0
    const { api, calls } = fakeApi({ places: [...marked, { name: 'bed', kind: 'bed', by: 'Tester', x: 60, y: 64, z: 0 }], answers: {
      'farm.maintain': () => ++worked === 1 || repeated ? { stopped: 'night and no bed within 32 blocks' } : { sweeps: 1 }
    } })
    const run = routine.run(api, { name: 'farmer/homestead', place: 'a', bed: 'bed' })
    if (repeated) await assert.rejects(run, /night and no bed/)
    else await run
    assert.equal(worked, 2)
    assert.equal(calls.filter(c => c.startsWith('goto ') && c.endsWith('range=2')).length, 1)
  })
}

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

// ---------------------------------------------------------------- the kit step and a tool that wears out (card 6cf481c0)
const KITTED = [{ action: 'kit', tools: 'stone_hoe', place: '$place' }, { action: 'work', place: '$place' }]
// a work whose hoe breaks under it: the fake takes the hoe out of the pockets, and fails the first time
const kittedApi = ({ items, kitLeaves, maintainFails = true }) => {
  let maintains = 0
  const made = fakeApi({
    places: marked, items,
    answers: {
      // the kit tops the hoes up to what the chest allows, and never takes one away
      kit: () => { items.stone_hoe = Math.max(items.stone_hoe ?? 0, kitLeaves); return { kit: `hoe:${items.stone_hoe} food:0` } },
      'work': () => {
        maintains++
        if (maintains > 1) return { harvested: 3 }
        items.stone_hoe = Math.max(0, (items.stone_hoe ?? 1) - 1)
        if (maintainFails) throw new Error('work: no hoe')
        return { harvested: 1 }
      }
    }
  })
  return made
}

test('routine: a step that fails as its hoe breaks gets the kit again and one more try, and the day says so', async () => {
  const { api, calls, events } = kittedApi({ items: { stone_hoe: 1 }, kitLeaves: 2 })
  const summary = await routine.run(api, { steps: KITTED, place: 'a' })
  assert.deepEqual([calls.filter(c => !c.startsWith('note')), events[0].places.a['work'], summary.failed], [
    ['kit tools=stone_hoe place=a', 'work place=a', 'kit tools=stone_hoe place=a', 'work place=a'],
    'ok rekit=hoe replaced harvested=3', undefined
  ])
})

test('routine: when the kit cannot replace the hoe the step is not tried again, and the failure says why', async () => {
  const { api, calls, events } = kittedApi({ items: { stone_hoe: 1 }, kitLeaves: 0 })
  await routine.run(api, { steps: KITTED, place: 'a' })
  assert.deepEqual([calls.filter(c => c.startsWith('work')).length, events[0].places.a['work']],
    [1, 'FAILED work: no hoe (hoe broke, no spare)'])
})

test('routine: a spare that took over still means the kit runs again, without a retry of a step that worked', async () => {
  const { api, calls, events } = kittedApi({ items: { stone_hoe: 2 }, kitLeaves: 2, maintainFails: false })
  await routine.run(api, { steps: KITTED, place: 'a' })
  assert.deepEqual([calls.filter(c => !c.startsWith('note')), events[0].places.a['work']],
    [['kit tools=stone_hoe place=a', 'work place=a', 'kit tools=stone_hoe place=a'], 'ok rekit=hoe replaced harvested=1'])
})

test('routine: a routine with no kit step watches no tools', async () => {
  const items = { stone_hoe: 1 }
  const { api, calls } = fakeApi({ places: marked, items, answers: { 'farm.maintain': () => { delete items.stone_hoe; return { harvested: 1 } } } })
  await routine.run(api, { steps: [{ action: 'farm.maintain', place: 'a' }] })
  assert.deepEqual(calls.filter(c => !c.startsWith('note')), ['farm.maintain place=a'])
})

for (const [name, lost, items, expected] of [
  ['back in the pockets', ['hoe'], { iron_hoe: 1 }, { replaced: ['hoe'], missing: [], text: 'hoe replaced' }],
  ['still gone', ['hoe'], {}, { replaced: [], missing: ['hoe'], text: 'hoe broke, no spare' }],
  ['one of each', ['hoe', 'shears'], { shears: 1 }, { replaced: ['shears'], missing: ['hoe'], text: 'shears replaced, hoe broke, no spare' }]
]) {
  test(`rekitVerdict: ${name}`, () => assert.deepEqual(rekitVerdict(lost, items), expected))
}

// ---------------------------------------------------------------- until=<EDN goal>: stop when the goal is met
const GOAL = '(>= (read :chest_count {:x 114 :y 71 :z -107 :item "bread"}) 576)'
// the store's bread after each day: the fake answers chest_count from this list, one day at a time
const breadByDay = counts => () => ({ item: 'bread', count: counts.shift() ?? 0, chest: '114,71,-107' })

test('routine: until= stops on the day the goal reads true, saying what it read', async () => {
  const { api, events } = fakeApi({ places: marked, answers: { ...answers, chest_count: breadByDay([200, 400, 600]) } })
  const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a', until: GOAL, days: 0 })
  assert.deepEqual([summary.days, events.map(e => e.type)], [3, ['routine_day', 'routine_day', 'routine_day', 'routine_stopped']])
  assert.deepEqual(events.at(-1), {
    type: 'routine_stopped', reason: 'until', step: null, place: null, advice: stopAdvice('until'),
    read: 'chest_count x=114 y=71 z=-107 item=bread: item=bread count=600 chest=114,71,-107'
  })
})

for (const [name, args, days] of [
  ['days= comes first', { days: 2 }, 2],
  ['the goal comes first', { days: 5 }, 3],
  ['no days= with a goal runs until the goal', {}, 3]
]) {
  test(`routine: until= and days=, whichever first: ${name}`, async () => {
    const { api } = fakeApi({ places: marked, answers: { ...answers, chest_count: breadByDay([0, 0, 576]) } })
    const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a', until: GOAL, ...args })
    assert.equal(summary.days, days)
  })
}

for (const [name, until, kept] of [['a goal is the routine’s own', GOAL, false], ['minutes stay the runner’s', 30, true]]) {
  test(`routine: until= on the args it shares with the runner: ${name}`, async () => {
    const { api } = fakeApi({ places: marked, answers: { ...answers, chest_count: breadByDay([576]) } })
    const a = { name: 'farmer/homestead', place: 'a', until }
    await routine.run(api, a)
    assert.equal('until' in a, kept)
  })
}

test('routine: a goal that does not parse is refused before day one', async () => {
  const { api, calls } = fakeApi({ places: marked, answers })
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a', until: '(>= (read :quit {}) 1)' }), /until=.*disallowed observation "quit"/)
  assert.deepEqual(calls, [])
})

test('routine: a goal it cannot read is noted and the days go on', async () => {
  const { api, calls } = fakeApi({ places: marked, answers: { ...answers, chest_count: new Error('chest_count: no path to the goal') } })
  const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a', until: GOAL, days: 2 })
  assert.equal(summary.days, 2)
  assert.deepEqual(calls.filter(c => c.startsWith('note until=')), Array(2).fill('note until= could not be read: chest_count: no path to the goal'))
})
