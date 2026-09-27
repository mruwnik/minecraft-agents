import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan} from '../src/lib/plan.mjs'
import { planCells } from './plan-fixture.mjs'
import { strays, clutterBlocks } from '../library/farm/shared/clutter.mjs'
import maintain from '../library/farm/maintain.mjs'
import { fakeApi } from './helpers.mjs'

const field = map => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan: map, parsed: parsePlan(map), cells: planCells({ plan: map, x: 0, y: 63, z: 0 }) })
for (const symbol of ['C', 'K', 'A', '#', 'G', 'T']) {
  test(`crop growth occupying planned ${symbol} is a tidy job`, () => {
    const { api } = fakeApi({ world: { '0,64,0': 'wheat#0' } })
    assert.deepEqual(clutterBlocks(field(symbol).cells, api.block), [{ x: 0, y: 64, z: 0, name: 'wheat' }])
  })
}

test('specific and generic crops, fruit beside stems, and planned plants are retained', () => {
  const plan = field('w*m..tF')
  const world = { '0,64,0': 'wheat#0', '1,64,0': 'carrots#2', '2,64,0': 'attached_melon_stem', '3,64,0': 'melon', '4,64,0': 'melon', '5,64,0': 'birch_sapling', '7,64,0': 'pumpkin' }
  world['6,64,0'] = 'dandelion'
  const { api } = fakeApi({ world })
  assert.deepEqual(strays(plan.cells, api.block), [])
})

test('a user container occupying another planned structure stays protected', () => {
  const { api } = fakeApi({ world: { '0,64,0': 'barrel', '1,64,0': 'chest' } })
  assert.deepEqual(strays(field('CA').cells, api.block).map(b => [b.name, b.keep]), [['barrel', "somebody's block"], ['chest', "somebody's block"]])
})

const setup = ({ zones = [], failed = null } = {}) => {
  const plan = field('Cw.')
  const world = {}
  for (let x = -3; x <= 5; x++) for (let z = -3; z <= 3; z++) for (let y = 62; y <= 66; y++) world[`${x},${y},${z}`] = y <= 63 ? 'dirt' : 'air'
  Object.assign(world, { '0,63,0': 'farmland', '0,64,0': 'wheat#0', '1,63,0': 'farmland', '1,64,0': 'wheat#2' })
  const items = { chest: 1, stone_hoe: 2 }
  const made = fakeApi({ place: plan, places: [plan], world, items, answers: {
    'farm.harvest': { harvested: {}, replanted: 0 }, zones: { zones },
    dig: p => { if (failed) throw new Error(failed); world[`${p.x},${p.y},${p.z}`] = 'air' },
    place: p => { assert.equal(p.item, 'chest'); world[`${p.x},${p.y},${p.z}`] = p.item; items.chest-- }
  } })
  return { ...made, world, items }
}

test('maintain harvests then tidies wheat from C and places the carried chest', async () => {
  const { api, calls, world, items } = setup()
  const result = await maintain.run(api, { place: 'test-field', compost: false })
  assert.equal(world['0,64,0'], 'chest')
  assert.equal(world['1,64,0'], 'wheat#2')
  assert.equal(items.chest, 0)
  assert.equal(result.built, 1)
  assert.equal(result.cleared, '1(wheat)')
  const harvest = calls.findIndex(c => c.startsWith('farm.harvest '))
  const dig = calls.indexOf('dig 0,64,0')
  const place = calls.findIndex(c => c.startsWith('place item=chest '))
  assert.ok(harvest < dig && dig < place)
  assert.deepEqual(calls.filter(c => c.startsWith('dig ')), ['dig 0,64,0'])
})

for (const blocked of ['zone', 'dig failure']) {
  test(`maintain reports ${blocked} instead of forcing the chest through the crop`, async () => {
    const zone = { name: 'neighbour-farm', x1: 0, x2: 0, y1: 63, y2: 65, z1: 0, z2: 0 }
    const made = setup(blocked === 'zone' ? { zones: [zone] } : { failed: 'too far to reach' })
    const result = await maintain.run(made.api, { place: 'test-field', compost: false })
    assert.equal(made.world['0,64,0'], 'wheat#0')
    assert.equal(made.items.chest, 1)
    assert.equal(result.built, 0)
    assert.ok(made.events.some(e => e.type === 'farm_attention'))
    if (blocked === 'zone') assert.equal(result.inZone, 'neighbour-farm:1')
    else assert.match(result.stuck, /clear wheat.*too far to reach/)
    assert.ok(!made.calls.some(c => c.startsWith('place ')))
  })
}
