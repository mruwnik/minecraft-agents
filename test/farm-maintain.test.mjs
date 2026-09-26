// What a sweep of farm.maintain leaves bare, and why it says so. The human, 09-26 22:14Z, on an all-wheat plan: "you're
// properly harvesting and replanting, but you don't seem to be maintaining the field according to the plan. There are
// some bare dirt, and most of the wheat field isn't planted". The body carried no hoe: the first till failed, its seed
// on the dirt failed, the second till failed the same way and the runner's "twice in a row" ended the sweep before one
// plant job on ready farmland had run, day after day, with nothing in the summary but stopped=. A sweep now never
// tries a till it has no hoe for, counts every planned crop cell it leaves empty, and says why.
import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, planCells, planBill } from '../src/lib.mjs'
import { bareLine, bareWhy, hasHoe, NO_HOE, seedReserve } from '../src/lib/farm.mjs'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'

const fakePlace = (plan, x = 0, y = 63, z = 0) => {
  const parsed = parsePlan(plan)
  return { name: 'test-field', kind: 'farm', x, y, z, plan, parsed, cells: planCells({ plan, x, y, z }), bill: planBill(parsed) }
}
const sweep = async ({ plan, world, items, answers = {} }) => {
  const made = fakeApi({ place: fakePlace(plan), world, items, answers: { 'farm.harvest': { harvested: {}, replanted: 0 }, ...answers } })
  const summary = await maintainFarm.run(made.api, { place: 'test-field' })
  return { summary, work: made.calls.filter(c => /^(till|place) /.test(c)) }
}
// a seed that is used up as it is planted: the pockets, not the job list, say whether there is one left
const lastSeed = { wheat_seeds: 1 }
const sowing = { place: ({ item }) => { lastSeed[item]-- } }
// every cell round a bed loaded and solid: nothing to stand on anywhere within work range of it
const walledBed = () => {
  const world = {}
  for (let x = -5; x <= 5; x++) for (let y = 58; y <= 68; y++) for (let z = -5; z <= 5; z++) world[`${x},${y},${z}`] = 'stone'
  return { ...world, '0,63,0': 'farmland', '0,64,0': 'air', '0,65,0': 'air' }
}

// ---------------------------------------------------------------- the sweep
for (const [name, given, expected] of [
  ['no hoe: the dirt bed is never tilled and the farmland beside it is still sown',
    { plan: 'www', world: { '0,63,0': 'dirt', '1,63,0': 'farmland', '1,64,0': 'wheat#3', '2,63,0': 'farmland' }, items: { wheat_seeds: 5 } },
    { work: ['place item=wheat_seeds x=2 y=64 z=0'], tilled: 0, replanted: 1, bare: `1 (untilled:1 ${NO_HOE})` }],
  ['a hoe and seed: the dirt bed is tilled and sown at once, then the empty farmland',
    { plan: 'www', world: { '0,63,0': 'dirt', '1,63,0': 'farmland', '1,64,0': 'wheat#3', '2,63,0': 'farmland' }, items: { wheat_seeds: 5, stone_hoe: 1 } },
    { work: ['till 0,63,0', 'place item=wheat_seeds x=0 y=64 z=0', 'place item=wheat_seeds x=2 y=64 z=0'], tilled: 1, replanted: 2, bare: undefined }],
  ['the seed runs out halfway: the second bed is counted, not tried',
    { plan: 'ww', world: { '0,63,0': 'farmland', '1,63,0': 'farmland' }, items: lastSeed, answers: sowing },
    { work: ['place item=wheat_seeds x=0 y=64 z=0'], tilled: 0, replanted: 1, bare: '1 (no seed:1 wheat_seeds)' }],
  ['no seed at all: every empty bed is counted under its seed',
    { plan: 'cw', world: { '0,63,0': 'farmland', '1,63,0': 'farmland' }, items: {} },
    { work: [], tilled: 0, replanted: 0, bare: '2 (no seed:2 carrot, wheat_seeds)' }],
  ['a till that fails leaves its own bed bare, with the reason, and its seed is not thrown on the dirt',
    { plan: 'ww', world: { '0,63,0': 'dirt', '1,63,0': 'farmland' }, items: { wheat_seeds: 5, stone_hoe: 1 }, answers: { till: new Error('till: stone where farmland should be') } },
    { work: ['till 0,63,0', 'place item=wheat_seeds x=1 y=64 z=0'], tilled: 0, replanted: 1, bare: '1 (untilled:1 till: stone where farmland should be)' }],
  ['a bed nothing can be stood beside is unreachable',
    { plan: 'w', world: { '0,63,0': 'farmland' }, items: { wheat_seeds: 5 }, answers: { place: new Error('place: nowhere to stand within 4 of 0,64,0') } },
    { work: ['place item=wheat_seeds x=0 y=64 z=0'], tilled: 0, replanted: 0, bare: '1 (unreachable:1 0,64,0)' }],
  ['a bed walled in on every side, all of it loaded, is unreachable before a step is taken',
    { plan: 'w', world: walledBed(), items: { wheat_seeds: 5 } },
    { work: [], tilled: 0, replanted: 0, bare: '1 (unreachable:1 0,64,0)' }],
  ['water standing on a bed is said, not sown into',
    { plan: 'w', world: { '0,63,0': 'farmland', '0,64,0': 'water' }, items: { wheat_seeds: 5 } },
    { work: [], tilled: 0, replanted: 0, bare: '1 (water:1 0,64,0)' }]
]) {
  test(`farm.maintain: ${name}`, async () => {
    const { summary, work } = await sweep(given)
    assert.deepEqual({ work, tilled: summary.tilled, replanted: summary.replanted, bare: summary.bare }, { ...expected })
  })
}

// bare= comes right after the counts: the routine keeps 120 characters of each step's summary, and a line that
// stood behind lowSlabs= and clutter= was the line nobody saw (09-26)
test('farm.maintain: bare= stands before the rest of the summary', async () => {
  const { summary } = await sweep({ plan: 'w', world: { '0,63,0': 'dirt' }, items: { wheat_seeds: 5 } })
  assert.deepEqual(Object.keys(summary).slice(0, 4), ['sweeps', 'harvested', 'replanted', 'bare'])
})

// ---------------------------------------------------------------- the pure parts
for (const [name, items, expected] of [
  ['a stone hoe', { stone_hoe: 1, wheat_seeds: 3 }, true],
  ['any hoe will do', { netherite_hoe: 1 }, true],
  ['seed alone is not a hoe', { wheat_seeds: 64 }, false],
  ['a hoe-shaped name that is not one', { hoe_stand: 1 }, false],
  ['empty pockets', {}, false]
]) {
  test(`hasHoe: ${name}`, () => assert.equal(hasHoe(items), expected))
}

for (const [name, message, expected] of [
  ['nowhere to stand', 'place: nowhere to stand within 4 of 1,64,2', 'unreachable'],
  ['no cell that sees the top', 'no cell to stand within 4.2 of 1,63,2 that sees its top: cannot see the top', 'unreachable'],
  ['no walkable path', 'place: no walkable path (walks don\'t dig or bridge)', 'unreachable'],
  ['a search that ran out of time', 'goto: no path to the goal: the search found nothing', 'unreachable'],
  ['anything else', 'placing wheat_seeds at 1,64,2 did not take', 'failed']
]) {
  test(`bareWhy: ${name}`, () => assert.equal(bareWhy(message), expected))
}

for (const [name, entries, expected] of [
  ['nothing bare', [], null],
  ['one reason', [{ why: 'untilled', note: NO_HOE }, { why: 'untilled', note: NO_HOE }], `2 (untilled:2 ${NO_HOE})`],
  ['reasons in a fixed order, notes joined once each',
    [{ why: 'no seed', note: 'wheat_seeds' }, { why: 'unreachable', note: '3,64,0' }, { why: 'untilled', note: NO_HOE }, { why: 'no seed', note: 'wheat_seeds' }, { why: 'water', note: '5,64,0' }],
    `5 (untilled:1 ${NO_HOE}; no seed:2 wheat_seeds; unreachable:1 3,64,0; water:1 5,64,0)`],
  ['a reason with no note', [{ why: 'failed' }], '1 (failed:1)']
]) {
  test(`bareLine: ${name}`, () => assert.equal(bareLine(entries), expected))
}

// ---------------------------------------------------------------- the seed reserve of a homestead (card b22fa1c9)
// The reserve was this plan's seed bill twice over, and nothing else: on a homestead of three plans the crop-field
// and cane passes composted EVERY wheat seed the body carried, since their plans sow none (Jizo, 09-26: 44-53
// wheat_seeds lost a day). reserve_for= names the other plans whose seed is kept as well
for (const [name, plans, expected] of [
  ['one plan: its seed twice over', ['ww'], { wheat_seeds: 4 }],
  ['several plans: the sum of their bills, twice over', ['ww', 'bbb', 'w'], { wheat_seeds: 6, beetroot_seeds: 6 }],
  ['structures are not seed', ['wCK~'], { wheat_seeds: 2 }],
  ['the same plan named twice counts once', ['ww', 'ww'], { wheat_seeds: 4 }]
]) {
  test(`seedReserve: ${name}`, () => assert.deepEqual(seedReserve([...new Set(plans)].map(parsePlan)), expected))
}

const homestead = () => {
  const own = fakePlace('wK')
  const other = { ...fakePlace('bb', 10, 63, 0), name: 'other' }
  const world = { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '1,63,0': 'dirt', '1,64,0': 'composter', '10,63,0': 'farmland', '11,63,0': 'farmland' }
  const made = fakeApi({ place: own, world, items: { wheat_seeds: 10, beetroot_seeds: 10 }, answers: { 'farm.harvest': { harvested: {}, replanted: 0 }, 'farm.compost': { fed: 'fed' } } })
  made.api.plan = name => ({ 'test-field': own, other })[name]
  return made
}
for (const [name, args, expected] of [
  ['without reserve_for the other plan\'s seed is compost', { place: 'test-field' }, ['farm.compost items(wheat_seeds:8 beetroot_seeds:10) x=1 y=64 z=0']],
  ['reserve_for keeps the other plan\'s seed too', { place: 'test-field', reserve_for: 'other' }, ['farm.compost items(wheat_seeds:8 beetroot_seeds:6) x=1 y=64 z=0']],
  ['the plan itself in the list changes nothing', { place: 'test-field', reserve_for: 'test-field, other' }, ['farm.compost items(wheat_seeds:8 beetroot_seeds:6) x=1 y=64 z=0']]
]) {
  test(`farm.maintain: ${name}`, async () => {
    const { api, calls } = homestead()
    await maintainFarm.run(api, args)
    assert.deepEqual(calls.filter(c => c.startsWith('farm.compost')), expected)
  })
}
