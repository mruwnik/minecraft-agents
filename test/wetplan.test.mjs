// Building a plan over water (card 013d4477): the dams that turn a pond into ground, the shore order that works every
// fill from dry footing, the reopening that never lets water back in, and the line that names the water in the way
import test from 'node:test'
import assert from 'node:assert/strict'
import { fakeApi } from './helpers.mjs'
import { parsePlan, planCells, planBill } from '../src/lib.mjs'
import { drainJobs, shoreOrder, reopenJobs, wetFooting } from '../src/wetplan.mjs'
import { buildFromPlan } from '../src/builder.mjs'

// a row of beds at y=63 from x=0 east, laid in water two deep (water at 63 and 64 over dirt at 62), air above. The land
// round the water is dry at 63 with air over it; `wet` says which columns hold water
const world = ({ x1 = -3, x2 = 8, wet }) => {
  const w = {}
  for (let x = x1; x <= x2; x++) {
    for (let z = -3; z <= 3; z++) {
      const water = wet(x, z)
      w[`${x},62,${z}`] = 'dirt'
      w[`${x},63,${z}`] = water ? 'water' : 'dirt'
      w[`${x},64,${z}`] = water ? 'water' : 'air'
      w[`${x},65,${z}`] = 'air'
      w[`${x},66,${z}`] = 'air'
    }
  }
  return w
}
// a trench exactly the plan's row: dry ground on every side of every cell
const trench = width => world({ wet: (x, z) => z === 0 && x >= 0 && x < width })
// a pond three wide with the plan's row down its middle: water beside every cell, a shore at x=-1
const pond = width => world({ wet: (x, z) => Math.abs(z) <= 1 && x >= 0 && x < width })
// water from a shore at x=-1 to beyond the plan (one shore) or to a shore at x=width (two shores)
const bay = (width, shores = 1) => world({ wet: (x, z) => x >= 0 && x < (shores === 2 ? width : 20) })
// water as far as the eye sees
const sea = () => world({ wet: () => true })

const blockOf = w => (x, y, z) => w[`${x},${y},${z}`] === undefined ? null : { name: w[`${x},${y},${z}`], properties: {}, solid: !['air', 'water'].includes(w[`${x},${y},${z}`]) }
const plan = width => planCells({ plan: 'w'.repeat(width), x: 0, y: 63, z: 0 })
const fills = width => Array.from({ length: width }, (_, x) => ({ do: 'fill', x, y: 63, z: 0, why: 'wheat where the floor should be', item: 'dirt' }))
const dams = width => Array.from({ length: width }, (_, x) => ({ do: 'fill', dam: true, x, y: 64, z: 0, why: 'water stands in the cell', item: 'dirt' }))
const order = jobs => jobs.map(j => `${j.dam ? 'dam' : j.do} ${j.x},${j.y},${j.z}`)
const dammedUp = (w, width) => Array.from({ length: width }, (_, x) => x).reduce((m, x) => ({ ...m, [`${x},63,0`]: 'dirt', [`${x},64,0`]: 'dirt' }), w)

test('drainJobs: water standing in a cell is dammed with dirt; the floor is groundJobs\' fill, not this list\'s', () => {
  assert.deepEqual(drainJobs(plan(3), blockOf(trench(3))), dams(3))
})

test('drainJobs: a dry plan has no dams', () => {
  assert.deepEqual(drainJobs(plan(3), blockOf(dammedUp(trench(3), 3))), [])
})

test('drainJobs: a channel cell holds water by design and is never dammed', () => {
  const cells = planCells({ plan: 'w~w', x: 0, y: 63, z: 0 })
  assert.deepEqual(drainJobs(cells, blockOf(trench(3))).map(j => j.x), [0, 2])
})

for (const [name, w, width, expected] of [
  ['a trench with dry ground beside every cell keeps the given order', trench(5), 5,
    ['fill 0,63,0', 'fill 1,63,0', 'fill 2,63,0', 'fill 3,63,0', 'fill 4,63,0', 'dam 0,64,0', 'dam 1,64,0', 'dam 2,64,0', 'dam 3,64,0', 'dam 4,64,0']],
  ['from one shore the arm fills what it reaches, then dams make footing for the next fills', bay(5), 5,
    ['fill 0,63,0', 'fill 1,63,0', 'fill 2,63,0', 'dam 0,64,0', 'dam 1,64,0', 'fill 3,63,0', 'dam 2,64,0', 'fill 4,63,0', 'dam 3,64,0', 'dam 4,64,0']],
  ['with a shore on both sides of five cells the given order already works', bay(5, 2), 5,
    ['fill 0,63,0', 'fill 1,63,0', 'fill 2,63,0', 'fill 3,63,0', 'fill 4,63,0', 'dam 0,64,0', 'dam 1,64,0', 'dam 2,64,0', 'dam 3,64,0', 'dam 4,64,0']],
  ['with no dry footing anywhere the given order is kept, for the build to name', sea(), 3,
    ['fill 0,63,0', 'fill 1,63,0', 'fill 2,63,0', 'dam 0,64,0', 'dam 1,64,0', 'dam 2,64,0']]
]) {
  test(`shoreOrder: ${name}`, () => assert.deepEqual(order(shoreOrder([...fills(width), ...dams(width)], blockOf(w))), expected))
}

test('shoreOrder: digs and tills have no footing rule and keep their order', () => {
  const clears = [2, 0, 1].map(x => ({ do: 'clear', x, y: 64, z: 0 }))
  assert.deepEqual(order(shoreOrder(clears, blockOf(sea()))), ['clear 2,64,0', 'clear 0,64,0', 'clear 1,64,0'])
})

test('reopenJobs: dams with nothing wet beside them are dug again, top first', () => {
  const w = { ...dammedUp(trench(3), 3), '1,65,0': 'dirt' }
  const stacked = [...dams(3), { do: 'fill', dam: true, x: 1, y: 65, z: 0, item: 'dirt' }]
  const { open, kept } = reopenJobs(stacked, blockOf(w))
  assert.deepEqual(order(open), ['clear 1,65,0', 'clear 0,64,0', 'clear 1,64,0', 'clear 2,64,0'])
  assert.deepEqual(kept, [])
})

test('reopenJobs: a dam with water from outside the plan against it stays in, named with that cell', () => {
  const { open, kept } = reopenJobs(dams(3), blockOf(dammedUp(pond(3), 3)))
  assert.deepEqual(order(open), [])
  assert.deepEqual(kept, ['0,64,0 (water at 0,64,1 would flow back in)', '1,64,0 (water at 1,64,1 would flow back in)', '2,64,0 (water at 2,64,1 would flow back in)'])
})

test('reopenJobs: another dam beside a dam is not water', () => {
  assert.equal(reopenJobs(dams(3), blockOf(dammedUp(trench(3), 3))).open.length, 3)
})

for (const [name, w, job, expected] of [
  ['a fill with nothing dry in reach names the nearest water', sea(), fills(3)[1], '1,63,0: no dry cell within 4.2 of it to stand on, water at 1,63,0: fill or drain that first'],
  ['a fill the shore reaches has footing: nothing to say', bay(3), fills(3)[1], null],
  ['a dig walks by itself: nothing to say', sea(), { do: 'clear', x: 1, y: 64, z: 0 }, null],
  ['no footing but no water in reach either (the edge of the loaded world): left to the primitive, nothing to say', {}, fills(3)[1], null]
]) {
  test(`wetFooting: ${name}`, () => assert.equal(wetFooting(job, blockOf(w)), expected))
}

// ---------------------------------------------------------------- the build
const fakePlace = width => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan: 'w'.repeat(width), parsed: parsePlan('w'.repeat(width)), cells: plan(width), bill: planBill(parsePlan('w'.repeat(width))) })
// the world changes as the build works: a placed block lands, a dug one becomes air, a tilled one farmland
const living = w => ({
  place: ({ item, x, y, z }) => { w[`${x},${y},${z}`] = item === 'wheat_seeds' ? 'wheat' : item; return { placed: 1 } },
  dig: ({ x, y, z }) => { w[`${x},${y},${z}`] = 'air'; return {} },
  till: ({ x, y, z }) => { w[`${x},${y},${z}`] = 'farmland'; return {} }
})
const build = async (w, width, extra = {}) => {
  const made = fakeApi({ place: fakePlace(width), world: w, items: { dirt: 20, wheat_seeds: 9, stone_hoe: 1 }, answers: { ...living(w), ...extra } })
  const r = await buildFromPlan(made.api, { place: 'test-field', partial: true })
  return { r, calls: made.calls.filter(c => /^(place|dig|till|goto) /.test(c)) }
}
const dirtAt = calls => calls.filter(c => c.startsWith('place item=dirt')).map(c => c.replace('place item=dirt ', ''))

test('farm.build: a trench is filled, dammed, the dams dug out again, then tilled and sown', async () => {
  const { r, calls } = await build(trench(3), 3)
  assert.deepEqual(dirtAt(calls), ['x=0 y=63 z=0', 'x=1 y=63 z=0', 'x=2 y=63 z=0', 'x=0 y=64 z=0', 'x=1 y=64 z=0', 'x=2 y=64 z=0'])
  assert.deepEqual(calls.filter(c => c.startsWith('dig')), ['dig 0,64,0', 'dig 1,64,0', 'dig 2,64,0'])
  assert.deepEqual(calls.filter(c => c.startsWith('till')), ['till 0,63,0', 'till 1,63,0', 'till 2,63,0'])
  assert.equal(r.levelled, 6)
  assert.equal(r.dammed, 3)
  assert.equal(r.planted, 3)
  assert.equal(r.kept, undefined)
})

test('farm.build: from one shore the fills and dams go in shore order', async () => {
  const { calls } = await build(bay(5), 5)
  assert.deepEqual(dirtAt(calls), ['x=0 y=63 z=0', 'x=1 y=63 z=0', 'x=2 y=63 z=0', 'x=0 y=64 z=0', 'x=1 y=64 z=0', 'x=3 y=63 z=0', 'x=2 y=64 z=0', 'x=4 y=63 z=0', 'x=3 y=64 z=0', 'x=4 y=64 z=0'])
})

test('farm.build: water from outside the plan keeps a dam in; its cell is left as it is and named on kept=', async () => {
  const { r, calls } = await build(pond(3), 3)
  assert.deepEqual(calls.filter(c => /^(dig|till)/.test(c)), [])
  assert.equal(r.dammed, 3)
  assert.equal(r.planted, undefined)
  assert.equal(r.kept, '0,64,0 (water at 0,64,1 would flow back in); 1,64,0 (water at 1,64,1 would flow back in); 2,64,0 (water at 2,64,1 would flow back in)')
})

test('farm.build: a plan with no dry cell in reach of any job is refused, naming the water in the way', async () => {
  await assert.rejects(build(sea(), 3), { message: 'test-field: 0,63,0: no dry cell within 4.2 of it to stand on, water at 0,63,0: fill or drain that first' })
})

test('farm.build: a walk to a standing cell that fails on the path is walked once more with dig=true, sparing the plan', async () => {
  const spared = []
  const goto = args => { if (args.dig === true) { spared.push(args.spare.length); return {} } if (args.range === 0) throw new Error('no walkable path'); return {} }
  const { r, calls } = await build(trench(3), 3, { goto })
  assert.ok(calls.some(c => /^goto .* dig spare=/.test(c)), calls.join('\n'))
  assert.deepEqual([...new Set(spared)], [6], 'every dig walk spares the three plan cells at y and y+1')
  assert.match(r.dug, /^-?\d+,\d+,-?\d+/)
})
