import test from 'node:test'
import assert from 'node:assert/strict'
import { PLAN_LEGEND, planCropMatches, parsePlan, planBill, planSummary, planErrors, planAnchor, fieldCensus, farmJobs, jobsBill, groundJobs, seedReserve, farmSurplus } from '../src/lib.mjs'
import { planCells } from './plan-fixture.mjs'
import { fakeApi } from './helpers.mjs'
import buildFarm from '../library/farm/build.mjs'
import maintainFarm from '../library/farm/maintain.mjs'

const field = plan => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan, parsed: parsePlan(plan), cells: planCells({ plan, x: 0, y: 63, z: 0 }), bill: planBill(parsePlan(plan)) })
const beds = names => Object.fromEntries(names.flatMap((name, x) => [[`${x},63,0`, 'farmland'], [`${x},64,0`, name], [`${x},65,0`, 'air']]))

test('generic crop plans describe farmland while specific crops remain strict', () => {
  assert.deepEqual(planErrors(parsePlan('*~')), [])
  assert.equal(planSummary(parsePlan('*wc')), '3x1 crop wheat carrot')
  assert.deepEqual(planBill(parsePlan('*wc')), { crop_seed: 1, wheat_seeds: 1, carrot: 1 })
  for (const name of ['wheat', 'carrots', 'potatoes', 'beetroots', 'melon_stem', 'attached_melon_stem', 'pumpkin_stem', 'attached_pumpkin_stem']) assert.equal(planCropMatches(PLAN_LEGEND['*'], name), true, name)
  for (const name of ['melon', 'pumpkin', 'bamboo', 'sugar_cane', 'stone', undefined]) assert.equal(planCropMatches(PLAN_LEGEND['*'], name), false, name)
  assert.equal(planCropMatches(PLAN_LEGEND.w, 'carrots'), false)
  assert.equal(planCropMatches(PLAN_LEGEND.w, 'melon'), false)
  assert.equal(planCropMatches(PLAN_LEGEND.m, 'attached_melon_stem'), true)
})

test('generic crop jobs retain mixed existing crops and allocate carried seeds without stealing specific seed', () => {
  const plan = field('******w')
  const { api } = fakeApi({ world: beds(['carrots#3', 'attached_melon_stem', 'air', 'air', 'air', 'air', 'air']) })
  const jobs = farmJobs({ cells: plan.cells, worldAt: api.block, items: { wheat_seeds: 1, carrot: 1, potato: 2 } })
  assert.deepEqual(jobs.map(j => [j.x, j.item, j.have]), [[2, 'carrot', true], [3, 'potato', true], [4, 'potato', true], [5, 'crop_seed', false], [6, 'wheat_seeds', true]])
  assert.deepEqual(jobsBill(jobs), { carrot: 1, potato: 2, crop_seed: 1, wheat_seeds: 1 })
  assert.deepEqual(groundJobs({ cells: plan.cells, worldAt: api.block, solid: name => name !== 'air' }), [])
  const census = fieldCensus(plan.cells, api.block)
  assert.deepEqual(census.crops, { carrots: 1, melon_stem: 1 })
  assert.equal(census.empty, 5)
  assert.equal(planAnchor(plan.cells, api.block).off, 0)
})

test('generic crop reserve shares a two-sowing budget across carried species after specific reserves', () => {
  const keep = seedReserve([parsePlan('**w')], { wheat_seeds: 2, carrot: 3, potato: 2 })
  assert.deepEqual(keep, { wheat_seeds: 2, carrot: 2, potato: 2 })
  assert.deepEqual(farmSurplus({ carrot: 3, potato: 2, wheat_seeds: 2 }, keep), { carrot: 1 })
  assert.deepEqual(seedReserve([parsePlan('*w')], { carrot: 1 }), { wheat_seeds: 2, carrot: 1 })
  assert.deepEqual(seedReserve([parsePlan('*')]), {})
})

for (const [name, action] of [['build', buildFarm], ['maintain', maintainFarm]]) {
  test(`farm.${name} reports a generic seed shortage without attempting a fictitious seed`, async () => {
    const plan = field('*')
    const { api, calls } = fakeApi({ place: plan, world: beds(['air']), items: { stone_hoe: 1, sugar_cane: 4 }, answers: {
      'farm.harvest': { harvested: {}, replanted: 0 }
    } })
    const result = await action.run(api, { place: plan.name, compost: false })
    assert.match(result.missing, /crop_seed:1/)
    assert.ok(!calls.some(c => c.startsWith('place ')), calls.join('\n'))
  })
  test(`farm.${name} sows a generic bed with carried carrot and retains a growing potato`, async () => {
    const plan = field('**')
    const world = beds(['air', 'potatoes#2'])
    const items = { carrot: 1, stone_hoe: 1 }
    const { api, calls } = fakeApi({ place: plan, world, items, answers: {
      'farm.harvest': { harvested: {}, replanted: 0 },
      place: a => { assert.equal(a.item, 'carrot'); world[`${a.x},${a.y},${a.z}`] = 'carrots#0'; items.carrot-- }
    } })
    const result = await action.run(api, { place: plan.name, compost: false })
    assert.equal(result.missing, undefined)
    assert.ok(calls.some(c => c === 'place item=carrot x=0 y=64 z=0'), calls.join('\n'))
    assert.equal(world['1,64,0'], 'potatoes#2')
    assert.ok(!calls.some(c => c.includes('item=crop_seed')))
  })
}
