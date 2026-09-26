// trample=true (card 68f4e331): the explicit last resort out of a crop pocket. A walk steps round crops, so a body a sweep
// left deep in its own rows can walk nowhere; with trample=true the pathfinder may step on crop cells. farm.maintain
// hands it to every walk of its own (the edge walks and the harvest), and nothing else changes when it is not asked for.
import test from 'node:test'
import assert from 'node:assert/strict'
import { fakeApi } from './helpers.mjs'
import farmMaintain from '../library/farm/maintain.mjs'
import { parsePlan, planCells, checkArgs } from '../src/lib.mjs'

const block = (name, properties = {}) => ({ name, properties, solid: name !== 'air' && name !== 'water' })
// a three by three farm, built to its plan: a covered channel, four carrots, a path, a torch post, a fence, a chest and a gate
const PLAN = { name: 'test-field', kind: 'farm', x: 100, y: 70, z: 200, plan: '~cc\n.cT\n#CG' }
const PLACE = { ...PLAN, parsed: parsePlan(PLAN.plan), cells: planCells(PLAN) }
const built = {
  '100,70,200': block('oak_slab', { waterlogged: 'true', type: 'bottom' }),
  '101,70,200': block('farmland'),
  '102,70,200': block('farmland'),
  '100,70,201': block('dirt'),
  '101,70,201': block('farmland'),
  '102,70,201': block('dirt'),
  '100,70,202': block('dirt'),
  '101,70,202': block('dirt'),
  '102,70,202': block('dirt'),
  '101,71,200': block('carrots', { age: 7 }),
  '102,71,200': block('carrots', { age: 3 }),
  '101,71,201': block('carrots', { age: 3 }),
  '102,71,201': block('oak_fence'),
  '102,72,201': block('torch'),
  '100,71,202': block('oak_fence'),
  '101,71,202': block('chest'),
  '102,71,202': block('oak_fence_gate')
}
const maintainApi = () => {
  const made = fakeApi({
    place: PLACE,
    items: { wheat_seeds: 64, carrot: 64, oak_slab: 8, water_bucket: 1 },
    answers: { 'farm.harvest': { harvested: {}, replanted: 0 }, 'farm.compost': { fed: 0 } }
  })
  made.api.block = (x, y, z) => built[`${x},${y},${z}`] ?? null
  return made
}
const walks = calls => calls.filter(c => /^(goto|farm\.harvest) /.test(c))

test('farm.maintain: trample=true rides on every walk of the sweep', async () => {
  const { api, calls } = maintainApi()
  await farmMaintain.run(api, { place: 'test-field', trample: true })
  assert.deepEqual(walks(calls), ['goto x=101 y=71 z=201 range=3 trample', 'farm.harvest within=8 trample'])
})

test('farm.maintain: without it the walks step round crops as before', async () => {
  const { api, calls } = maintainApi()
  await farmMaintain.run(api, { place: 'test-field' })
  assert.deepEqual(walks(calls), ['goto x=101 y=71 z=201 range=3', 'farm.harvest within=8'])
})

test('farm.maintain: trample= is one of its arguments, a boolean', () => {
  assert.deepEqual(
    [checkArgs('farm.maintain', farmMaintain.args, { place: 'f', trample: true }), checkArgs('farm.maintain', farmMaintain.args, { place: 'f', trample: 'yes' })],
    [null, 'farm.maintain: trample= wants a boolean, got "yes"'])
})
