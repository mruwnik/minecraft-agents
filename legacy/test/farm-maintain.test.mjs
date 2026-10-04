// What a sweep of farm.maintain leaves bare, and why it says so. The human, 09-26 22:14Z, on an all-wheat plan: "you're
// properly harvesting and replanting, but you don't seem to be maintaining the field according to the plan. There are
// some bare dirt, and most of the wheat field isn't planted". The body carried no hoe: the first till failed, its seed
// on the dirt failed, the second till failed the same way and the runner's "twice in a row" ended the sweep before one
// plant job on ready farmland had run, day after day, with nothing in the summary but stopped=. A sweep now never
// tries a till it has no hoe for, counts every planned crop cell it leaves empty, and says why.
import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, planBill } from '../src/lib.mjs'
import { planCells } from './plan-fixture.mjs'
import { bareLine, bareWhy, hasHoe, NO_HOE, seedReserve } from '../src/lib/farm.mjs'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'
import harvestFarm from '../library/farm/harvest.mjs'

const fakePlace = (plan, x = 0, y = 63, z = 0) => {
  const parsed = parsePlan(plan)
  return { name: 'test-field', kind: 'farm', x, y, z, plan, parsed, cells: planCells({ plan, x, y, z }), bill: planBill(parsed) }
}

test('a till that leaves dirt unchanged reports the bed and continues independent sowing', async () => {
  const world = { '0,63,0': 'dirt', '1,63,0': 'farmland', '4,63,0': 'water' }
  const items = { stone_hoe: 1, wheat_seeds: 2 }
  const { api, calls, events } = fakeApi({ place: fakePlace('ww'), world, items, answers: {
    'farm.harvest': {},
    till: new Error('till: tilled nothing: 1 still dirt: nothing is on top of it (first 0,63,0)'),
    place: p => { world[`${p.x},${p.y},${p.z}`] = 'wheat#0'; items.wheat_seeds-- }
  } })
  const result = await maintainFarm.run(api, { place: 'test-field', compost: false })
  assert.equal(result.replanted, 1)
  assert.match(result.bare, /untilled:1/)
  assert.equal(world['0,63,0'], 'dirt')
  assert.equal(world['1,64,0'], 'wheat#0')
  assert.equal(calls.filter(c => c.startsWith('till ')).length, 1)
  assert.ok(events.some(e => e.type === 'farm_attention' && e.reasons.bare))
})

// A trampled bed (card trampled-retill): the sweep's job list was built before its own walk crossed the field, and
// the walk to bed0's till trampled bed1's farmland back to dirt on the way. Found dirt there, at job time, bed1 is
// retilled in place and planted - not left bare for the mock's place to refuse, the way a live field found it
test('farm.maintain retills a bed trampled back to dirt since the job list was built, and plants it', async () => {
  const world = { '0,63,0': 'dirt', '1,63,0': 'farmland', '4,63,0': 'water' }
  const items = { stone_hoe: 1, wheat_seeds: 2 }
  const { api, calls, events } = fakeApi({ place: fakePlace('ww'), world, items, answers: {
    'farm.harvest': {},
    till: p => {
      world[`${p.x},${p.y},${p.z}`] = 'farmland'
      if (p.x === 0) world['1,63,0'] = 'dirt'
    },
    place: p => {
      const ground = world[`${p.x},${p.y - 1},${p.z}`]
      if (ground !== 'farmland') throw new Error(`place: placed nothing: 1 ${ground} is already there (first ${p.x},${p.y},${p.z})`)
      world[`${p.x},${p.y},${p.z}`] = 'wheat#0'; items.wheat_seeds--
    }
  } })
  const result = await maintainFarm.run(api, { place: 'test-field', compost: false })
  assert.equal(result.tilled, 2)
  assert.equal(result.replanted, 2)
  assert.equal(world['1,63,0'], 'farmland')
  assert.equal(world['1,64,0'], 'wheat#0')
  assert.equal(calls.filter(c => c.startsWith('till ')).length, 2)
  assert.equal(result.bare, undefined)
  assert.equal(result.stuck, undefined)
  assert.deepEqual(events, [])
})

test('maintenance tills only beds it can seed, then reports the remaining shortage', async () => {
  // water within 4 of every bed in the row (0..3): none of them is the dry case this file leaves to farm-water.test.mjs
  const world = { ...Object.fromEntries([0, 1, 2, 3].map(x => [`${x},63,0`, 'dirt'])), '1,63,4': 'water' }
  const items = { stone_hoe: 1, wheat_seeds: 1, carrot: 1 }
  const { api, calls, events } = fakeApi({ place: fakePlace('wwcc'), world, items, answers: {
    'farm.harvest': {},
    till: p => { world[`${p.x},${p.y},${p.z}`] = 'farmland' },
    place: p => { world[`${p.x},${p.y},${p.z}`] = p.item === 'carrot' ? 'carrots#0' : 'wheat#0'; items[p.item]-- }
  } })
  const result = await maintainFarm.run(api, { place: 'test-field', compost: false })
  assert.equal(result.tilled, 2)
  assert.equal(result.replanted, 2)
  assert.deepEqual(calls.filter(c => c.startsWith('till ')), ['till 0,63,0', 'till 2,63,0'])
  assert.equal(world['1,63,0'], 'dirt')
  assert.equal(world['3,63,0'], 'dirt')
  assert.match(result.missing, /wheat_seeds:1/)
  assert.match(result.missing, /carrot:1/)
  assert.ok(events.some(e => e.type === 'farm_attention' && e.reasons.missing))
  // A seedless second pass should spend no hoe durability on empty beds.
  const before = calls.length
  const next = await maintainFarm.run(api, { place: 'test-field', compost: false })
  assert.equal(next.tilled, 0)
  assert.ok(!calls.slice(before).some(c => c.startsWith('till ')))
})
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

test('farm.maintain retains harvest loss and access warnings in its result', async () => {
  const warnings = { lost: 'wheat@4,64,0', unreachable: '1 ripe crop beyond a fence', stalksOutOfReach: '2 stalks too deep', inventoryFull: true }
  const { summary } = await sweep({ plan: 'w', world: { '0,63,0': 'farmland', '0,64,0': 'wheat#3' }, items: {}, answers: { 'farm.harvest': { harvested: { wheat: 1 }, ...warnings } } })
  for (const [key, value] of Object.entries(warnings)) assert.equal(summary[key], value)
})

for (const state of ['missing', 'full', 'healthy', 'disabled']) {
  test(`farm.maintain storage ${state}: reports attention without stopping or choosing another store`, async () => {
    const place = fakePlace('wC')
    const items = { wheat: 40, wheat_seeds: 2 }
    const world = { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '1,63,0': 'dirt', '1,64,0': state === 'missing' ? 'air' : 'chest' }
    const { api, events, calls } = fakeApi({ place, world, items, answers: {
      deposit: () => {
        if (state === 'full') { items.wheat = 15; throw new Error('the CHEST is full') }
        items.wheat = 0
        return {}
      }
    } })
    const summary = await maintainFarm.run(api, { place: place.name, ...(state === 'disabled' ? { deposit: false } : {}) })
    assert.equal(summary.sweeps, 1, 'storage attention must not stop maintenance')
    const attention = events.filter(e => e.type === 'farm_attention')
    assert.equal(attention.length, ['missing', 'full'].includes(state) ? 1 : 0)
    if (attention.length) {
      assert.equal(attention[0].place, place.name)
      assert.equal(attention[0].reasons.storage_full, summary.storage_full)
      assert.deepEqual(attention[0].carried, { wheat: state === 'full' ? 15 : 40 }, 'only remaining surplus, excluding reserved seed')
      assert.match(attention[0].advice, /Storage needs a decision/)
      if (state === 'missing') assert.equal(attention[0].reasons.chest_missing, summary.chest_missing)
    }
    const deposits = calls.filter(c => c.startsWith('deposit '))
    assert.equal(deposits.length, ['full', 'healthy'].includes(state) ? 1 : 0)
    assert.ok(deposits.every(c => c.endsWith('x=1 y=64 z=0')), 'storage destination stays as configured')
  })
}

test('farm.maintain reserves generic-bed seed collected during this harvest before composting', async () => {
  const place = fakePlace('*K')
  const items = {}
  const world = { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '1,63,0': 'dirt', '1,64,0': 'composter' }
  const { api, calls } = fakeApi({ place, world, items, answers: {
    'farm.harvest': () => { items.wheat_seeds = 10; return { harvested: { wheat: 1 }, replanted: 0 } },
    'farm.compost': { fed: 'wheat_seeds:8' }
  } })
  await maintainFarm.run(api, { place: place.name })
  assert.deepEqual(calls.filter(c => c.startsWith('farm.compost ')), ['farm.compost items(wheat_seeds:8) x=1 y=64 z=0'])
})

test('farm.maintain harvests, clears a melon from a wheat bed, then tills and sows both beds', async () => {
  const place = fakePlace('ww')
  const world = {}
  for (let x = -5; x <= 6; x++) for (let z = -5; z <= 5; z++) {
    world[`${x},63,${z}`] = 'dirt'
    world[`${x},64,${z}`] = 'air'
    world[`${x},65,${z}`] = 'air'
  }
  Object.assign(world, { '0,63,0': 'farmland', '0,64,0': 'wheat#7', '1,64,0': 'melon', '4,64,0': 'wheat#7', '5,63,0': 'water' })
  const items = {}
  const key = a => `${a.x},${a.y},${a.z}`
  const { api, calls } = fakeApi({ place, places: [place], world, items, answers: {
    kit: () => { items.stone_hoe = 1; return { kit: 'hoe:1 food:12' } },
    'farm.harvest': a => { assert.equal(a.replant, false); return harvestFarm.run(api, a) },
    find_blocks: a => ({ positions: String(a.block).includes('wheat') ? [{ x: 0, y: 64, z: 0 }, { x: 4, y: 64, z: 0 }] : [] }),
    dig: a => { world[key(a)] = 'air' },
    collect: () => { items.wheat_seeds = 4; return {} },
    till: a => { world[key(a)] = 'farmland' },
    place: a => { assert.equal(a.item, 'wheat_seeds'); world[key(a)] = 'wheat#0'; items.wheat_seeds-- }
  } })
  const out = await maintainFarm.run(api, { place: place.name })
  const firstCollect = calls.findIndex(c => c.startsWith('collect '))
  const tidy = calls.findIndex(c => c === 'dig 1,64,0')
  const firstPlant = calls.findIndex(c => c.startsWith('place '))
  assert.ok(firstCollect >= 0 && tidy > firstCollect && firstPlant > tidy, calls.join('\n'))
  assert.deepEqual(out.harvested, { wheat: 1 })
  assert.equal(out.cleared, '1(melon)')
  assert.equal(out.tilled, 1)
  assert.equal(out.replanted, 2)
  assert.equal(world['0,64,0'], 'wheat#0')
  assert.equal(world['1,64,0'], 'wheat#0')
  assert.equal(world['4,64,0'], 'wheat#7', 'neighboring wheat stays intact')
})

for (const replace of [true, false]) {
  test(`farm.maintain provisions its own hoe and ${replace ? 'replaces it once' : 'reports a missing replacement'} when it breaks`, async () => {
    const place = fakePlace('wwww')
    const world = { '0,63,0': 'dirt', '1,63,0': 'dirt', '2,63,0': 'dirt', '3,63,0': 'farmland', '4,63,0': 'water' }
    const items = { wheat_seeds: 10 }
    let kits = 0
    const { api, calls } = fakeApi({ place, world, items, answers: {
      kit: () => {
        kits++
        if (kits === 1 || replace) { items.stone_hoe = 1; return { kit: 'hoe:1 food:12' } }
        return { kit: 'hoe:0 food:12', kit_short: 'stone_hoe: 0 carried, 2 wanted: no cobblestone' }
      },
      till: a => { world[`${a.x},${a.y},${a.z}`] = 'farmland'; delete items.stone_hoe },
      place: a => { world[`${a.x},${a.y},${a.z}`] = 'wheat#0'; items.wheat_seeds-- }
    } })
    const out = await maintainFarm.run(api, { place: place.name })
    assert.equal(kits, 2, 'one initial kit and at most one replacement attempt per sweep')
    assert.ok(calls.indexOf('kit tools=stone_hoe food=12 place=test-field') < calls.findIndex(c => c.startsWith('farm.harvest ')))
    assert.ok(calls.some(c => c.startsWith('farm.harvest place=test-field within=')), 'harvest must stay on the named plan')
    assert.equal(out.tilled, replace ? 2 : 1)
    assert.equal(out.replanted, replace ? 3 : 2)
    assert.equal(out.rekit, replace ? 'hoe replaced' : 'hoe broke, no spare')
    assert.match(out.bare, /untilled:.*no hoe/)
    assert.equal(world['3,64,0'], 'wheat#0', 'ready farmland is still planted after running out of hoes')
    if (!replace) assert.match(out.kit_short, /no cobblestone/)
  })
}

// ---------------------------------------------------------------- the sweep
for (const [name, given, expected] of [
  ['no hoe: the dirt bed is never tilled and the farmland beside it is still sown',
    { plan: 'www', world: { '0,63,0': 'dirt', '1,63,0': 'farmland', '1,64,0': 'wheat#3', '2,63,0': 'farmland' }, items: { wheat_seeds: 5 } },
    { work: ['place item=wheat_seeds x=2 y=64 z=0'], tilled: 0, replanted: 1, bare: `1 (untilled:1 ${NO_HOE})` }],
  ['a hoe and seed: the dirt bed is tilled and sown at once, then the empty farmland',
    { plan: 'www', world: { '0,63,0': 'dirt', '1,63,0': 'farmland', '1,64,0': 'wheat#3', '2,63,0': 'farmland', '4,63,0': 'water' }, items: { wheat_seeds: 5, stone_hoe: 1 } },
    { work: ['till 0,63,0', 'place item=wheat_seeds x=0 y=64 z=0', 'place item=wheat_seeds x=2 y=64 z=0'], tilled: 1, replanted: 2, bare: undefined }],
  ['the seed runs out halfway: the second bed is counted, not tried',
    { plan: 'ww', world: { '0,63,0': 'farmland', '1,63,0': 'farmland' }, items: lastSeed, answers: sowing },
    { work: ['place item=wheat_seeds x=0 y=64 z=0'], tilled: 0, replanted: 1, bare: '1 (no seed:1 wheat_seeds)' }],
  ['no seed at all: every empty bed is counted under its seed',
    { plan: 'cw', world: { '0,63,0': 'farmland', '1,63,0': 'farmland' }, items: {} },
    { work: [], tilled: 0, replanted: 0, bare: '2 (no seed:2 carrot, wheat_seeds)' }],
  ['a till that fails leaves its own bed bare, with the reason, and its seed is not thrown on the dirt',
    { plan: 'ww', world: { '0,63,0': 'dirt', '1,63,0': 'farmland', '4,63,0': 'water' }, items: { wheat_seeds: 5, stone_hoe: 1 }, answers: { till: new Error('till: stone where farmland should be') } },
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
