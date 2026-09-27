import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { fertilizePlanned } from '../src/farm/fertilize.mjs'
import { planCells, parsePlan } from '../src/lib/plan.mjs'
import { routineSteps } from '../src/lib.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
import maintain from '../library/farm/maintain.mjs'
import { fakeApi } from './helpers.mjs'

const plan = map => ({ name: 'field', kind: 'farm', x: 0, y: 63, z: 0, plan: map, parsed: parsePlan(map), cells: planCells({ plan: map, x: 0, y: 63, z: 0 }) })
const cropsWorld = names => Object.fromEntries(names.flatMap((name, x) => [[`${x},63,0`, 'farmland'], [`${x},64,0`, name]]))

test('fertilization touches only immature ordinary crops matching their planned beds', async () => {
  const field = plan('wcpb*wmkt.w')
  const world = cropsWorld(['wheat#0', 'carrots#1', 'potatoes#2', 'beetroots#1', 'carrots#0', 'wheat#7', 'melon_stem#0', 'pumpkin_stem#0', 'oak_sapling', 'short_grass', 'carrots#0', 'wheat#0'])
  const items = { bone_meal: 20 }
  const { api, calls } = fakeApi({ world, items, answers: { fertilize: () => { items.bone_meal--; return { used: 99 } } } })
  const result = await fertilizePlanned(api, field.cells)
  assert.deepEqual(calls, [0, 1, 2, 3, 4].map(x => `fertilize ${x},64,0`))
  assert.equal(result.bone_meal_used, 5, 'inventory consumption, not a claimed result, is counted')
  assert.equal(result.bone_meal_attempted, 5)
  assert.equal(result.fertilized, 5)
  assert.equal(result.bone_meal_attention, undefined)
})

test('each eligible bed gets at most one attempt even when no meal is consumed', async () => {
  const items = { bone_meal: 10 }
  const { api, calls } = fakeApi({ world: cropsWorld(['wheat#0', 'wheat#0', 'wheat#0']), items })
  const result = await fertilizePlanned(api, plan('www').cells)
  assert.equal(calls.length, 3)
  assert.equal(result.bone_meal_used, 0)
  assert.equal(result.bone_meal_attempted, 3)
  assert.match(result.bone_meal_attention, /did not take/)
})

test('stock shortage names the remaining eligible crop attempts', async () => {
  const items = { bone_meal: 1 }
  const { api, calls } = fakeApi({ world: cropsWorld(['wheat#0', 'wheat#0', 'wheat#0', 'wheat#0']), items, answers: { fertilize: () => { items.bone_meal-- } } })
  const result = await fertilizePlanned(api, plan('wwww').cells)
  assert.equal(calls.length, 1)
  assert.equal(result.bone_meal_used, 1)
  assert.match(result.bone_meal_attention, /short of bone meal for 3 requested crop attempts/)
})

test('no immature matching crops means no demand for bone meal', async () => {
  const { api, calls } = fakeApi({ world: cropsWorld(['wheat#7', 'carrots#0']) })
  const result = await fertilizePlanned(api, plan('ww').cells)
  assert.deepEqual(calls, [])
  assert.equal(result.bone_meal_attention, undefined)
})

const setup = (options = {}) => {
  const field = plan('w')
  const world = cropsWorld(['wheat#0'])
  const items = { stone_hoe: 2, ...(options.items ?? {}) }
  const made = fakeApi({ place: field, world, items, answers: {
    fertilize: () => { items.bone_meal--; world['0,64,0'] = 'wheat#7'; return { used: 1 } },
    'farm.harvest': () => ({ harvested: world['0,64,0'] === 'wheat#7' ? { wheat: 1 } : {}, replanted: 0 }),
    ...options.answers
  } })
  return { ...made, world, items }
}

test('default maintenance uses no bone meal; opt-in matures before harvesting', async () => {
  const off = setup({ items: { bone_meal: 1 } })
  const defaultResult = await maintain.run(off.api, { place: 'field', compost: false })
  assert.ok(!off.calls.some(c => c.startsWith('fertilize ')))
  assert.equal(defaultResult.bone_meal_used, undefined)
  const on = setup({ items: { bone_meal: 1 } })
  const result = await maintain.run(on.api, { place: 'field', compost: false, bone_meal: true })
  assert.equal(result.bone_meal_used, 1)
  assert.deepEqual(result.harvested, { wheat: 1 })
  assert.ok(on.calls.indexOf('fertilize 0,64,0') < on.calls.findIndex(c => c.startsWith('farm.harvest ')))
})

test('requested meal shortage emits farm attention and harvesting still runs', async () => {
  const made = setup()
  const result = await maintain.run(made.api, { place: 'field', compost: false, bone_meal: true })
  assert.match(result.bone_meal_attention, /short of bone meal/)
  assert.ok(made.calls.some(c => c.startsWith('farm.harvest ')))
  assert.ok(made.events.some(e => e.type === 'farm_attention' && e.reasons.bone_meal_attention))
})

test('recoverable fertilizer reach failure reports attention and harvesting continues', async () => {
  const made = setup({ items: { bone_meal: 1 }, answers: { fertilize: new Error('too far to reach') } })
  const result = await maintain.run(made.api, { place: 'field', compost: false, bone_meal: true })
  assert.match(result.bone_meal_attention, /0,64,0:.*too far to reach/)
  assert.equal(result.bone_meal_used, 0)
  assert.ok(made.calls.some(c => c.startsWith('farm.harvest ')))
})

for (const error of [new Error('cancelled'), new TypeError('unexpected crop state'), { stopped: 'hurt' }]) {
  test(`fertilization preserves hard stop ${error.message ?? error.stopped}`, async () => {
    const made = setup({ items: { bone_meal: 1 }, answers: { fertilize: error } })
    await assert.rejects(maintain.run(made.api, { place: 'field', bone_meal: true }))
    assert.ok(!made.calls.some(c => c.startsWith('farm.harvest ')))
  })
}

for (const budget of [-1, 0, 1, 0.5, NaN, Infinity, 'true']) {
  test(`invalid bone_meal=${budget} fails before any action`, async () => {
    const made = setup()
    await assert.rejects(maintain.run(made.api, { place: 'field', bone_meal: budget }), /true or false/)
    assert.deepEqual(made.calls, [])
  })
}

test('one-attempt limit resets each sweep while actual consumption accumulates in the report', async () => {
  const made = setup({ items: { bone_meal: 2 } })
  const act = made.api.act
  made.api.act = async (name, args) => { const r = await act(name, args); if (name === 'farm.harvest') made.world['0,64,0'] = 'wheat#0'; return r }
  made.api.checkpoint = async () => { if (made.report.sweeps === 2) throw new CompositeHandBack('done') }
  await assert.rejects(maintain.run(made.api, { place: 'field', compost: false, bone_meal: true, days: 2 }), /done/)
  assert.equal(made.calls.filter(c => c.startsWith('fertilize ')).length, 2)
  assert.equal(made.report.bone_meal_used, 2)
})

test('farmer routine keeps bone meal off unless explicitly supplied as a variable', () => {
  const steps = JSON.parse(fs.readFileSync(new URL('../roles/farmer/homestead.json', import.meta.url), 'utf8'))
  assert.equal(routineSteps({ steps, place: 'field' }, () => null).steps[0].bone_meal, undefined)
  assert.equal(routineSteps({ steps, place: 'field', vars: { bone_meal: true } }, () => null).steps[0].bone_meal, true)
})
