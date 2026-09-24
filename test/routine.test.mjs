// routine place=a,b,c: a farmstead is several fields, and one routine a day should cover all of them (card cd4c1262).
// The pure half lives in src/routine.mjs; library/routine.mjs only calls it. Nothing here touches src/lib.mjs.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { placeList, routinePlan, placesRefusal } from '../src/routine.mjs'
import routine from '../library/routine.mjs'
import { fakeApi } from './helpers.mjs'

const ROLE_FILES = {
  'farmer/homestead': '[{"action":"farm.tidy","place":"$place"},{"action":"farm.maintain","place":"$place","deposit":true},{"action":"farm.compost","place":"$place"}]',
  'farmer/chores': '[{"action":"farm.compost"}]'
}
const readRole = name => ROLE_FILES[name] ?? null
const homestead = place => [
  { action: 'farm.tidy', place },
  { action: 'farm.maintain', place, deposit: true },
  { action: 'farm.compost', place }
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
  ['a name nobody ships', { name: 'farmer/nope', place: 'a,b' }, { error: 'no routine called farmer/nope (roles/farmer/nope.json)' }]
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
  const { api, calls } = fakeApi({ places: marked, answers: { 'farm.tidy': {}, 'farm.maintain': {}, 'farm.compost': {} } })
  const summary = await routine.run(api, { name: 'farmer/homestead', place: 'a,b' })
  assert.deepEqual([calls.filter(c => !c.startsWith('note')), summary.days, summary.ran], [
    ['farm.tidy place=a', 'farm.maintain place=a deposit', 'farm.compost place=a',
      'farm.tidy place=b', 'farm.maintain place=b deposit', 'farm.compost place=b'], 1, 6])
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
  assert.deepEqual([got, calls], [{ dry: true, places: ['a', 'b', 'c'], steps: [...homestead('a'), ...homestead('b'), ...homestead('c')] }, []])
})

test('routine: dry=true still names a place nobody marked', async () => {
  const { api } = fakeApi({ places: marked })
  await assert.rejects(routine.run(api, { name: 'farmer/homestead', place: 'a,nope', dry: true }), /no place called nope/)
})
