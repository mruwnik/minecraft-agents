// A sweep cut short still says what it did. The runner hands the body back at a checkpoint (spoken to, low health, night
// with no bed) and the task's result is whatever the composite had reported by then: farm.maintain reported only at the
// end of a sweep, so Jizo's task 11 (09-26 23:43Z, stopped="spoken to") had used 43 seed and 3 dirt and said none of
// filled=, tilled=, replanted= or bare=. The sweep's report is now brought up to date before every checkpoint
import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, planCells, planBill } from '../src/lib.mjs'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'

const fakePlace = (plan, x = 0, y = 63, z = 0) => {
  const parsed = parsePlan(plan)
  return { name: 'test-field', kind: 'farm', x, y, z, plan, parsed, cells: planCells({ plan, x, y, z }), bill: planBill(parsed) }
}
// the runner's hand-back, thrown by the `stopAt`th checkpoint of the run (what runComposite turns into stopped=)
const stoppedAt = async ({ plan, world, items, answers = {}, stopAt }) => {
  const made = fakeApi({ place: fakePlace(plan), world, items, answers: { 'farm.harvest': { harvested: { wheat: 2 }, replanted: 1 }, collect: {}, ...answers } })
  let seen = 0
  made.api.checkpoint = async () => { if (++seen === stopAt) throw new Error('spoken to (a player)') }
  await assert.rejects(maintainFarm.run(made.api, { place: 'test-field' }), /spoken to/)
  return made.report
}
const pick = (report, keys) => Object.fromEntries(keys.map(k => [k, report[k]]))

// checkpoints of a sweep: once per cleared stray, after the anchor, after the harvest, then after every job
const BEDS = { '0,63,0': 'dirt', '1,63,0': 'farmland', '1,64,0': 'wheat#3', '2,63,0': 'farmland' }
// two strays over the beds, and a dig that takes the block out of the world
const LITTERED = { ...BEDS, '0,64,0': 'dirt', '2,64,0': 'cobblestone', '0,65,0': 'air', '2,65,0': 'air' }
const digOut = world => ({ dig: ({ x, y, z }) => { world[`${x},${y},${z}`] = 'air'; return {} } })
for (const [name, given, keys, expected] of [
  ['spoken to after the harvest: the harvest is said',
    { plan: 'www', world: BEDS, items: { wheat_seeds: 5, stone_hoe: 1 }, stopAt: 2 },
    ['harvested', 'replanted', 'tilled'], { harvested: { wheat: 2 }, replanted: 1, tilled: 0 }],
  ['spoken to after a till and a plant: both are counted',
    { plan: 'www', world: BEDS, items: { wheat_seeds: 5, stone_hoe: 1 }, stopAt: 4 },
    ['harvested', 'replanted', 'tilled'], { harvested: { wheat: 2 }, replanted: 2, tilled: 1 }],
  ['spoken to after a failed till: its bed is already bare=, with the reason',
    { plan: 'ww', world: { '0,63,0': 'dirt', '1,63,0': 'farmland' }, items: { wheat_seeds: 5, stone_hoe: 1 }, answers: { till: new Error('till: stone where farmland should be') }, stopAt: 3 },
    ['tilled', 'bare'], { tilled: 0, bare: '1 (untilled:1 till: stone where farmland should be)' }],
  ['spoken to after a hole was filled: filled= is said',
    { plan: 'ww', world: { '0,63,0': 'air', '1,63,0': 'farmland', '1,64,0': 'wheat#3' }, items: { wheat_seeds: 5, stone_hoe: 1, dirt: 4 }, stopAt: 3 },
    ['filled'], { filled: 1 }],
  ['spoken to between two strays: the one cleared is said',
    (world => ({ plan: 'www', world, items: { wheat_seeds: 5, stone_hoe: 1 }, answers: digOut(world), stopAt: 1 }))({ ...LITTERED }),
    ['cleared'], { cleared: '1(dirt)' }]
]) {
  test(`farm.maintain stopped early: ${name}`, async () => assert.deepEqual(pick(await stoppedAt(given), keys), expected))
}
