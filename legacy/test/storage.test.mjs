// Where a harvest goes and what happens when it does not fit. The human, in game 09-26: "can you control where the
// harvested stuff gets put? What if that doesn't have any space?". Until now the surplus went to the plan's one C
// chest, a full chest was a stuck= line, and the body carried the harvest round for another day. deposit= now names
// the chest cell or the marked storage place, the plan's other chests take the overflow in a fixed order, and a
// harvest nothing can take is said as storage_full=, not as stuck (card 63e91e8f).
import test from 'node:test'
import assert from 'node:assert/strict'
import { chestOrder, depositLine, depositPlan, depositTarget, planChests, storageFullLine, wentIn } from '../src/lib/storage.mjs'
import { canStore, storeSurplus, storeInto } from '../src/storage.mjs'
import { parsePlan, planBill } from '../src/lib.mjs'
import { planCells } from './plan-fixture.mjs'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'
import flockMaintain from '../library/flock/maintain.mjs'
import apiaryMaintain from '../library/apiary/maintain.mjs'

const PLACES = [{ name: 'barn', kind: 'storage', x: 10.4, y: 64, z: -20.7, by: 'somebody' }]

// ---------------------------------------------------------------- what deposit= means
for (const [name, arg, expected] of [
  ['false: nothing is stored', false, null],
  ['left out: the plan\'s own chests', undefined, { kind: 'plan' }],
  ['true: the plan\'s own chests', true, { kind: 'plan' }],
  ['x,y,z: that chest first', '12,63,-80', { kind: 'cell', x: 12, y: 63, z: -80 }],
  ['a marked place: its chests, found there', 'barn', { kind: 'place', name: 'barn', x: 10, y: 64, z: -21 }],
  ['a name nobody marked is said', 'shed', { error: 'deposit=shed is neither x,y,z nor a marked place: places lists them (mark name=<name> kind=storage beside the chests)' }]
]) {
  test(`depositTarget: ${name}`, () => assert.deepEqual(depositTarget(arg, PLACES), expected))
}

const cellsOf = plan => planCells({ plan, x: 0, y: 63, z: 0 })
for (const [name, plan, expected] of [
  ['one chest, one above the ground', 'wC', [{ x: 1, y: 64, z: 0 }]],
  ['every chest, row by row', 'wCw\nCwC', [{ x: 1, y: 64, z: 0 }, { x: 0, y: 64, z: 1 }, { x: 2, y: 64, z: 1 }]],
  ['no chest', 'www', []]
]) {
  test(`planChests: ${name}`, () => assert.deepEqual(planChests(cellsOf(plan)), expected))
}

// the chests found round a storage place come back in a fixed order: nearest the mark first, then by x, z, y
test('chestOrder: nearest the mark first, ties by x then z then y', () => {
  const found = [{ x: 14, y: 64, z: -20 }, { x: 11, y: 64, z: -20 }, { x: 10, y: 64, z: -19 }, { x: 10, y: 65, z: -21 }, { x: 10, y: 64, z: -21 }]
  assert.deepEqual(chestOrder(found, { x: 10, y: 64, z: -21 }), [
    { x: 10, y: 64, z: -21 }, { x: 10, y: 65, z: -21 }, { x: 11, y: 64, z: -20 }, { x: 10, y: 64, z: -19 }, { x: 14, y: 64, z: -20 }
  ])
})

// ---------------------------------------------------------------- the plan: which chest takes what
// a chest is { x, y, z, free (empty slots), items (what is in it) }; one whose room is not known is asked for everything left
const at = (x, extra = {}) => ({ x, y: 64, z: 0, ...extra })
for (const [name, surplus, chests, expected] of [
  ['fits in one', { wheat: 100, wheat_seeds: 10 }, [at(1, { free: 27, items: {} }), at(2, { free: 27, items: {} })],
    { drops: [{ x: 1, y: 64, z: 0, items: { wheat: 100, wheat_seeds: 10 } }], left: {} }],
  ['spills into the next', { wheat: 100 }, [at(1, { free: 1, items: {} }), at(2, { free: 27, items: {} })],
    { drops: [{ x: 1, y: 64, z: 0, items: { wheat: 64 } }, { x: 2, y: 64, z: 0, items: { wheat: 36 } }], left: {} }],
  ['nothing fits', { wheat: 40 }, [at(1, { free: 0, items: { wheat: 128 } }), at(2, { free: 0, items: { dirt: 64 } })],
    { drops: [], left: { wheat: 40 } }],
  ['a part-filled stack has room without a free slot', { wheat: 40 }, [at(1, { free: 0, items: { wheat: 100 } })],
    { drops: [{ x: 1, y: 64, z: 0, items: { wheat: 28 } }], left: { wheat: 12 } }],
  ['two items share the free slots', { wheat: 64, wheat_seeds: 10 }, [at(1, { free: 1, items: {} }), at(2, { free: 27, items: {} })],
    { drops: [{ x: 1, y: 64, z: 0, items: { wheat: 64 } }, { x: 2, y: 64, z: 0, items: { wheat_seeds: 10 } }], left: {} }],
  ['eggs stack by 16', { egg: 20 }, [at(1, { free: 1, items: {} })],
    { drops: [{ x: 1, y: 64, z: 0, items: { egg: 16 } }], left: { egg: 4 } }],
  ['a chest of unknown room is asked for everything', { wheat: 100 }, [at(1), at(2)],
    { drops: [{ x: 1, y: 64, z: 0, items: { wheat: 100 } }], left: {} }],
  ['no chests at all', { wheat: 100 }, [], { drops: [], left: { wheat: 100 } }],
  ['nothing to store', {}, [at(1)], { drops: [], left: {} }]
]) {
  test(`depositPlan: ${name}`, () => assert.deepEqual(depositPlan(surplus, chests), expected))
}

// the pockets before and after say what a deposit really moved, whatever the deposit answered
for (const [name, asked, before, after, expected] of [
  ['all of it went', { wheat: 40 }, { wheat: 40 }, {}, { wheat: 40 }],
  ['a full chest took part', { wheat: 100, wheat_seeds: 10 }, { wheat: 100, wheat_seeds: 10 }, { wheat: 36, wheat_seeds: 10 }, { wheat: 64 }],
  ['a meal eaten meanwhile is not a deposit', { bread: 8 }, { bread: 8 }, { bread: 7 }, { bread: 1 }],
  ['never more than asked', { wheat: 10 }, { wheat: 40 }, {}, { wheat: 10 }]
]) {
  test(`wentIn: ${name}`, () => assert.deepEqual(wentIn(asked, before, after), expected))
}

for (const [name, drops, expected] of [
  ['one chest', [{ x: 12, y: 63, z: -80, items: { wheat: 64, wheat_seeds: 3 } }], 'wheat:64@12,63,-80 wheat_seeds:3@12,63,-80'],
  ['a spill names both chests', [{ x: 12, y: 63, z: -80, items: { wheat: 64 } }, { x: 13, y: 63, z: -80, items: { wheat: 16 } }], 'wheat:64@12,63,-80 wheat:16@13,63,-80'],
  ['nothing stored', [], null]
]) {
  test(`depositLine: ${name}`, () => assert.equal(depositLine(drops), expected))
}

for (const [name, left, expected] of [
  ['what nobody could take', { wheat: 40, wheat_seeds: 3 }, 'wheat:40 wheat_seeds:3 carried'],
  ['everything stored', {}, null]
]) {
  test(`storageFullLine: ${name}`, () => assert.equal(storageFullLine(left), expected))
}

// ---------------------------------------------------------------- storing, chest by chest
// a chest that fills up: the deposit takes what fits from the pockets, then fails the way the primitive does
const CHEST_FULL = 'the CHEST is full: what fitted went in (the - above). Put the rest in another chest, or take out what does not belong here'
const fillsUp = (items, room) => ({ items: args => {
  for (const [name, n] of Object.entries(args.items)) {
    const took = Math.min(n, room[name] ?? 0)
    items[name] -= took
    room[name] = (room[name] ?? 0) - took
  }
  return new Error(CHEST_FULL)
} })
const twoChests = cellsOf('wCwC')
const noNotes = calls => calls.filter(c => !c.startsWith('note'))

test('storeSurplus: what the first chest cannot take goes to the next, and the line says what went where', async () => {
  const items = { wheat: 80, wheat_seeds: 4 }
  const room = { wheat: 64 }
  const { api, calls } = fakeApi({ items, answers: { deposit: args => args.x === 1 ? fillsUp(items, room).items(args) : {} } })
  const stored = await storeSurplus(api, { surplus: { wheat: 80, wheat_seeds: 4 }, target: { kind: 'plan' }, cells: twoChests })
  assert.deepEqual([noNotes(calls), stored], [
    ['deposit items(wheat:80 wheat_seeds:4) x=1 y=64 z=0', 'deposit items(wheat:16 wheat_seeds:4) x=3 y=64 z=0'],
    { deposited: 'wheat:64@1,64,0 wheat:16@3,64,0 wheat_seeds:4@3,64,0' }
  ])
})

test('storeSurplus: every chest full is storage_full, not stuck', async () => {
  const items = { wheat: 40 }
  const { api, calls } = fakeApi({ items, answers: { deposit: fillsUp(items, {}).items } })
  const stored = await storeSurplus(api, { surplus: { wheat: 40 }, target: { kind: 'plan' }, cells: twoChests })
  assert.deepEqual([noNotes(calls), stored], [
    ['deposit items(wheat:40) x=1 y=64 z=0', 'deposit items(wheat:40) x=3 y=64 z=0'],
    { storage_full: 'wheat:40 carried' }
  ])
})

test('storeSurplus: any other failure of a deposit is stuck, and the chests after it are not tried', async () => {
  const { api, calls } = fakeApi({ items: { wheat: 40 }, answers: { deposit: new Error('goto: no walkable path') } })
  const stored = await storeSurplus(api, { surplus: { wheat: 40 }, target: { kind: 'plan' }, cells: twoChests })
  assert.deepEqual([noNotes(calls), stored], [['deposit items(wheat:40) x=1 y=64 z=0'], { stuck: 'goto: no walkable path' }])
})

test('storeSurplus: a chest cell comes first, then the plan\'s own chests', async () => {
  const { api, calls } = fakeApi({ items: { wheat: 40 } })
  const stored = await storeSurplus(api, { surplus: { wheat: 40 }, target: { kind: 'cell', x: 9, y: 64, z: 9 }, cells: twoChests })
  assert.deepEqual([noNotes(calls), stored], [['deposit items(wheat:40) x=9 y=64 z=9'], { deposited: 'wheat:40@9,64,9' }])
})

test('storeSurplus: a storage place is walked to and its chests found there, nearest the mark first', async () => {
  const items = { wheat: 80 }
  const room = { wheat: 64 }
  const { api, calls } = fakeApi({
    items,
    answers: {
      find_blocks: { positions: [{ x: 13, y: 64, z: -20 }, { x: 10, y: 64, z: -21 }] },
      deposit: args => args.x === 10 ? fillsUp(items, room).items(args) : {}
    }
  })
  const stored = await storeSurplus(api, { surplus: { wheat: 80 }, target: { kind: 'place', name: 'barn', x: 10, y: 64, z: -21 }, cells: twoChests })
  assert.deepEqual([noNotes(calls), stored], [
    ['goto x=10 y=64 z=-21 range=4', 'find_blocks block=chest barrel trapped_chest maxDistance=12 count=32',
      'deposit items(wheat:80) x=10 y=64 z=-21', 'deposit items(wheat:16) x=13 y=64 z=-20'],
    { deposited: 'wheat:64@10,64,-21 wheat:16@13,64,-20' }
  ])
})

test('storeSurplus: a storage place with no chest in reach is storage_full and says so', async () => {
  const { api, calls } = fakeApi({ items: { wheat: 40 }, answers: { find_blocks: { positions: [] } } })
  const stored = await storeSurplus(api, { surplus: { wheat: 40 }, target: { kind: 'place', name: 'barn', x: 10, y: 64, z: -21 }, cells: [] })
  assert.deepEqual([calls.filter(c => c.startsWith('deposit')), stored], [[], { storage_full: 'wheat:40 carried (no chest within 12 of barn)' }])
})

test('storeSurplus: the nearest chest, for a round with no plan', async () => {
  const { api, calls } = fakeApi({ items: { honeycomb: 6 }, answers: { find_blocks: { positions: [{ x: 3, y: 64, z: 3 }] } } })
  const stored = await storeSurplus(api, { surplus: { honeycomb: 6 }, target: { kind: 'nearest' }, cells: [] })
  assert.deepEqual([noNotes(calls), stored], [
    ['find_blocks block=chest barrel trapped_chest maxDistance=32 count=1', 'deposit items(honeycomb:6) x=3 y=64 z=3'],
    { deposited: 'honeycomb:6@3,64,3' }
  ])
})

test('storeSurplus: nothing to store walks nowhere', async () => {
  const { api, calls } = fakeApi({ items: {} })
  const stored = await storeSurplus(api, { surplus: {}, target: { kind: 'place', name: 'barn', x: 10, y: 64, z: -21 }, cells: [] })
  assert.deepEqual([calls, stored], [[], {}])
})

// the runner's "inventory full and no chest to deposit in" hand-back: is there anywhere to put things at all?
for (const [name, target, cells, expected] of [
  ['deposit=false', null, twoChests, false],
  ['the plan has chests', { kind: 'plan' }, twoChests, true],
  ['the plan has none', { kind: 'plan' }, cellsOf('www'), false],
  ['a named place', { kind: 'place', name: 'barn', x: 10, y: 64, z: -21 }, cellsOf('www'), true],
  ['a chest cell', { kind: 'cell', x: 1, y: 2, z: 3 }, [], true]
]) {
  test(`canStore: ${name}`, () => assert.equal(canStore(target, cells), expected))
}

// ---------------------------------------------------------------- through the composites
// (a two-wheat plan keeps 4 seed back for its own sowing: 9 carried leaves 5 to store)
const fakePlace = (plan, x = 0, y = 63, z = 0) => {
  const parsed = parsePlan(plan)
  return { name: 'test-field', kind: 'farm', x, y, z, plan, parsed, cells: planCells({ plan, x, y, z }), bill: planBill(parsed) }
}
const ripeField = { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '1,63,0': 'dirt', '1,64,0': 'chest', '2,63,0': 'farmland', '2,64,0': 'wheat#3', '3,63,0': 'dirt', '3,64,0': 'chest' }

test('farm.maintain: a full plan chest spills into the plan\'s next one and the summary says what went where', async () => {
  const items = { wheat: 80, wheat_seeds: 9 }
  const room = { wheat: 64 }
  const { api, calls } = fakeApi({
    place: fakePlace('wCwC'), world: ripeField, items,
    answers: { 'farm.harvest': { harvested: { wheat: 80 } }, deposit: args => args.x === 1 ? fillsUp(items, room).items(args) : {} }
  })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual([calls.filter(c => c.startsWith('deposit')), summary.deposited, summary.storage_full, summary.stuck], [
    ['deposit items(wheat:80 wheat_seeds:5) x=1 y=64 z=0', 'deposit items(wheat:16 wheat_seeds:5) x=3 y=64 z=0'],
    'wheat:64@1,64,0 wheat:16@3,64,0 wheat_seeds:5@3,64,0', undefined, undefined
  ])
})

test('farm.maintain: every chest full is storage_full=, the sweep is not stuck', async () => {
  const items = { wheat: 40, wheat_seeds: 9 }
  const { api } = fakeApi({
    place: fakePlace('wCwC'), world: ripeField, items,
    answers: { 'farm.harvest': { harvested: { wheat: 40 } }, deposit: fillsUp(items, {}).items }
  })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual([summary.storage_full, summary.deposited, summary.stuck], ['wheat:40 wheat_seeds:5 carried', undefined, undefined])
})

test('farm.maintain: deposit=<place> stores at the marked place instead of the plan chest', async () => {
  const { api, calls } = fakeApi({
    place: fakePlace('wC'), places: PLACES, world: ripeField, items: { wheat: 40, wheat_seeds: 9 },
    answers: { 'farm.harvest': { harvested: { wheat: 40 } }, find_blocks: { positions: [{ x: 10, y: 64, z: -21 }] } }
  })
  const summary = await maintainFarm.run(api, { place: 'test-field', deposit: 'barn' })
  assert.deepEqual([calls.filter(c => /^(goto x=10|find_blocks|deposit)/.test(c)), summary.deposited], [
    ['goto x=10 y=64 z=-21 range=4', 'find_blocks block=chest barrel trapped_chest maxDistance=12 count=32', 'deposit items(wheat:40 wheat_seeds:7) x=10 y=64 z=-21'],
    'wheat:40@10,64,-21 wheat_seeds:7@10,64,-21'
  ])
})

test('farm.maintain: deposit= a place nobody marked is refused before the sweep', async () => {
  const { api } = fakeApi({ place: fakePlace('wC'), world: ripeField, items: { wheat: 40 } })
  await assert.rejects(maintainFarm.run(api, { place: 'test-field', deposit: 'shed' }), /deposit=shed is neither x,y,z nor a marked place/)
})

const penAt = { kind: 'pen', x: 10, y: 64, z: 20 }
test('flock.maintain: deposit=x,y,z puts the wool in that chest', async () => {
  const { api, calls } = fakeApi({
    items: { wheat: 8, white_wool: 6 }, places: [{ name: 'paddock', ...penAt }],
    answers: { animals: { found: [{ mob: 'sheep', id: 1, grown: true, inMyPen: true }, { mob: 'sheep', id: 2, grown: true, inMyPen: true }] }, shear: { tried: 1 }, collect: {} }
  })
  const summary = await flockMaintain.run(api, { mob: 'sheep', place: 'paddock', size: 2, deposit: '12,64,22' })
  assert.deepEqual([calls.filter(c => c.startsWith('deposit')), summary.deposited], [['deposit items(white_wool:6) x=12 y=64 z=22'], 'white_wool:6@12,64,22'])
})

test('flock.maintain: a full chest is storage_full=, and the round goes on', async () => {
  const items = { wheat: 8, white_wool: 6 }
  const { api } = fakeApi({
    items, places: [{ name: 'paddock', ...penAt }],
    answers: { animals: { found: [{ mob: 'sheep', id: 1, grown: true, inMyPen: true }, { mob: 'sheep', id: 2, grown: true, inMyPen: true }] }, shear: { tried: 1 }, collect: {}, deposit: fillsUp(items, {}).items }
  })
  const summary = await flockMaintain.run(api, { mob: 'sheep', place: 'paddock', size: 2, deposit: '12,64,22' })
  assert.deepEqual([summary.storage_full, summary.stuck, summary.rounds], ['white_wool:6 carried', undefined, 1])
})

test('apiary.maintain: deposit=<place> stores the comb at the marked place', async () => {
  const { api, calls } = fakeApi({
    items: { shears: 1, honeycomb: 6 }, places: [{ name: 'hives', kind: 'apiary', x: 0, y: 64, z: 0 }, ...PLACES],
    answers: { 'apiary.inspect': { hives: 2, ripe: 0, beesVisible: 6, grownVisible: 6 }, find_blocks: { positions: [{ x: 10, y: 64, z: -21 }] } }
  })
  const summary = await apiaryMaintain.run(api, { place: 'hives', deposit: 'barn' })
  assert.deepEqual([calls.filter(c => c.startsWith('deposit')), summary.deposited], [['deposit items(honeycomb:6) x=10 y=64 z=-21'], 'honeycomb:6@10,64,-21'])
})

test('apiary.maintain: deposit=true is still the nearest chest', async () => {
  const { api, calls } = fakeApi({
    items: { shears: 1, honeycomb: 6 }, places: [{ name: 'hives', kind: 'apiary', x: 0, y: 64, z: 0 }],
    answers: { 'apiary.inspect': { hives: 2, ripe: 0, beesVisible: 6, grownVisible: 6 }, find_blocks: { positions: [{ x: 3, y: 64, z: 3 }] } }
  })
  const summary = await apiaryMaintain.run(api, { place: 'hives', deposit: true })
  assert.deepEqual([calls.filter(c => c.startsWith('deposit')), summary.deposited], [['deposit items(honeycomb:6) x=3 y=64 z=3'], 'honeycomb:6@3,64,3'])
})

// A chest the target names that is not there is skipped and named, never opened: Jizo's plan marks a C cell where no
// chest was ever built (farmland at -1,62,-88, nothing above it), and the deposit tried to open the air there:
// stuck="farm.maintain/deposit: containerToOpen is neither a block nor an entity" (task 12, 09-26 23:51Z). A cell that is
// not loaded is still tried: the deposit walks there, and a far chest reads as nothing from here
const MISSING = (at, what, whose = "the plan's") => `${whose} chest at ${at} is missing (${what} there): place one, or pass deposit=false`
for (const [name, world, target, expected] of [
  ['the first plan chest missing: the second takes it all, the first is named',
    { '1,64,0': 'air', '3,64,0': 'chest' }, { kind: 'plan' },
    [['deposit items(wheat:40) x=3 y=64 z=0'], { deposited: 'wheat:40@3,64,0', chest_missing: MISSING('1,64,0', 'air') }]],
  ['every plan chest missing: nothing opened, the harvest carried and both named',
    { '1,64,0': 'air', '3,64,0': 'wheat' }, { kind: 'plan' },
    [[], { storage_full: 'wheat:40 carried', chest_missing: `${MISSING('1,64,0', 'air')}; ${MISSING('3,64,0', 'wheat')}` }]],
  ['a deposit= cell with stone in it: named, and the plan\'s chests tried after it',
    { '9,64,9': 'stone', '1,64,0': 'chest' }, { kind: 'cell', x: 9, y: 64, z: 9 },
    [['deposit items(wheat:40) x=1 y=64 z=0'], { deposited: 'wheat:40@1,64,0', chest_missing: MISSING('9,64,9', 'stone', 'the deposit=') }]],
  ['a barrel or a trapped chest is a chest',
    { '1,64,0': 'barrel', '3,64,0': 'trapped_chest' }, { kind: 'plan' },
    [['deposit items(wheat:40) x=1 y=64 z=0'], { deposited: 'wheat:40@1,64,0' }]],
  ['a chest cell not loaded is still tried',
    {}, { kind: 'plan' },
    [['deposit items(wheat:40) x=1 y=64 z=0'], { deposited: 'wheat:40@1,64,0' }]]
]) {
  test(`storeSurplus: ${name}`, async () => {
    const { api, calls } = fakeApi({ items: { wheat: 40 }, world })
    const stored = await storeSurplus(api, { surplus: { wheat: 40 }, target, cells: twoChests })
    assert.deepEqual([noNotes(calls), stored], expected)
  })
}

for (const [name, summary, stored, expected] of [
  ['a missing chest is carried into the summary', {}, { chest_missing: 'x' }, { chest_missing: 'x' }],
  ['a second round keeps the first round\'s line', { chest_missing: 'x' }, {}, { chest_missing: 'x' }]
]) {
  test(`storeInto: ${name}`, () => assert.deepEqual(storeInto({ ...summary }, stored), expected))
}
