// farm.harvest place= on a dense field (card f30fd998): the walk goes to the field's edge, not its centre; only the
// plan's own crops are cut; the seeds go back far end first so the body is never walled in; and what stays bare is named
import test from 'node:test'
import assert from 'node:assert/strict'
import { fakeApi } from './helpers.mjs'
import farmHarvest from '../library/farm/harvest.mjs'

const key = c => `${c.x},${c.y},${c.z}`
const range = (from, to) => Array.from({ length: to - from + 1 }, (_, i) => from + i)
// a 9x9 wheat field on flat ground: dirt at y=70 with air over it for 23 blocks round (100,200), farmland x 96..104 z 196..204
// planted with ripe wheat at y=71, and one ripe wheat just east of the field that is somebody else's
const PLAN = { name: 'dense-field', kind: 'farm', x: 96, y: 70, z: 196, plan: range(1, 9).map(() => 'wwwwwwwww').join('\n') }
const inField = c => c.x >= 96 && c.x <= 104 && c.z >= 196 && c.z <= 204
const flat = () => Object.assign({}, ...range(89, 111).flatMap(x => range(189, 211).flatMap(z => [
  { [`${x},70,${z}`]: inField({ x, z }) ? 'farmland' : 'dirt' },
  { [`${x},71,${z}`]: inField({ x, z }) || (x === 106 && z === 200) ? 'wheat#7' : 'air' },
  { [`${x},72,${z}`]: 'air' }
])))
const crops = world => Object.keys(world).filter(k => world[k] === 'wheat#7').map(k => k.split(',').map(Number)).map(([x, y, z]) => ({ x, y, z }))

// the world changes under the tools: a dig leaves air, a place leaves the seed's crop, except in the cells that stay bare
const harvestApi = ({ bare = [], items = { wheat_seeds: 200 }, from = { x: 120, y: 71, z: 200 }, world = flat() } = {}) => {
  const batches = []
  const sow = b => { if (!bare.includes(key(b))) world[key(b)] = 'wheat#0' }
  const made = fakeApi({
    world,
    items,
    places: [PLAN],
    answers: {
      find_blocks: a => ({ positions: a.block === 'bamboo,sugar_cane' || Array.isArray(a.block) && a.block.includes('bamboo') ? [] : crops(world) }),
      dig: a => { world[key(a)] = 'air'; return {} },
      place: a => { const blocks = a.blocks ?? [a]; batches.push(blocks); blocks.forEach(sow); return { placed: blocks.length } },
      collect: {}
    }
  })
  made.api.pos = () => from
  return { ...made, batches, world }
}

test('farm.harvest place=: walks to the nearest cell at the edge of the plan, never its centre, and says so', async () => {
  const { api, calls } = harvestApi()
  const out = await farmHarvest.run(api, { place: 'dense-field' })
  assert.equal(calls[0], 'goto x=108 y=71 z=200 range=1')
  assert.equal(out.standing, "standing at 108,71,200, plan's edge")
  assert.equal(calls.filter(c => c.startsWith('goto x=100')).length, 0)
})

test('farm.harvest place=: cuts only the crops over the plan, all of them, and reaches far enough from the edge', async () => {
  const { api, calls } = harvestApi()
  const out = await farmHarvest.run(api, { place: 'dense-field' })
  const digs = calls.filter(c => c.startsWith('dig'))
  assert.equal(digs.length, 81)
  assert.ok(!digs.includes('dig 106,71,200 batch=true'))
  assert.equal(out.harvested.wheat, 81)
  assert.equal(out.replanted, 81)
  assert.equal(out.notReplanted, null)
  assert.match(calls.find(c => c.startsWith('find_blocks')), /maxDistance=15\b/)
})

test('farm.harvest place=: the seeds go back far end first, the cell beside the edge last', async () => {
  const { api, batches } = harvestApi()
  await farmHarvest.run(api, { place: 'dense-field' })
  assert.equal(batches[0].length, 81)
  assert.deepEqual(batches[0][0], { item: 'wheat_seeds', x: 96, y: 71, z: 196 })
  assert.deepEqual(batches[0][80], { item: 'wheat_seeds', x: 104, y: 71, z: 200 })
})

test('farm.harvest: a cell that stays bare is retried from the edge, then named with why', async () => {
  const { api, calls } = harvestApi({ bare: ['100,71,200'] })
  const out = await farmHarvest.run(api, { place: 'dense-field' })
  assert.equal(out.replanted, 80)
  assert.equal(out.notReplanted, '1: no standing cell within reach at 100,200')
  assert.deepEqual(calls.filter(c => c.startsWith('goto')), ['goto x=108 y=71 z=200 range=1', 'goto x=108 y=71 z=200 range=1'])
  assert.ok(calls.includes('place item=wheat_seeds x=100 y=71 z=200'))
})

test('farm.harvest: a seed the pockets ran out of is named beside the walled-in cells', async () => {
  // from the south-east the edge is 108,71,205, so the last cell in plant order (the one the pockets cannot cover) is 104,204
  const { api, calls } = harvestApi({ bare: ['100,71,200'], items: { wheat_seeds: 80 }, from: { x: 130, y: 71, z: 215 } })
  const out = await farmHarvest.run(api, { place: 'dense-field' })
  assert.equal(calls[0], 'goto x=108 y=71 z=205 range=1')
  assert.equal(out.replanted, 79)
  assert.equal(out.notReplanted, '2: no standing cell within reach at 100,200; no wheat_seeds left in my pockets at 104,204')
})

test('farm.harvest place=: nothing to stand in near the plan walks to its anchor first, then gives up out loud', async () => {
  const world = flat()
  Object.keys(world).filter(k => world[k] === 'air').forEach(k => delete world[k])
  const { api, calls, events } = harvestApi({ world })
  const result = await farmHarvest.run(api, { place: 'dense-field' })
  assert.match(result.stuck, /dense-field: nowhere to stand within 4.2 of any cell of its plan/)
  assert.equal(events.filter(e => e.type === 'farm_attention').length, 1)
  assert.deepEqual(calls, ['goto x=96 y=71 z=196 range=4'])
})

test('farm.harvest without place=: works where I stand, everything found is cut, in cut order', async () => {
  const { api, calls, batches } = harvestApi()
  const out = await farmHarvest.run(api, { within: 12 })
  assert.equal(calls.filter(c => c.startsWith('goto')).length, 0)
  assert.equal(out.harvested.wheat, 82)
  assert.equal(out.standing, undefined)
  assert.deepEqual(batches[0][0], { item: 'wheat_seeds', x: 96, y: 71, z: 196 })
})

for (const named of [true, false]) {
  test(`farm.harvest ${named ? 'place= keeps neighboring stalks intact' : 'without place= can cut every nearby stalk'}`, async () => {
    const world = flat()
    const segments = [96, 106].flatMap(x => [71, 72].map(y => ({ x, y, z: 200 })))
    for (const c of segments) world[key(c)] = 'sugar_cane'
    const { api, calls } = harvestApi({ world })
    const act = api.act
    api.act = async (name, args) => {
      const answer = await act(name, args)
      return name === 'find_blocks' && String(args.block).includes('bamboo') ? { positions: segments } : answer
    }
    const out = await farmHarvest.run(api, named ? { place: PLAN.name } : { within: 24 })
    assert.equal(out.harvested.sugar_cane, named ? 1 : 2)
    assert.equal(world['96,71,200'], 'sugar_cane', 'own base stays planted')
    assert.equal(world['106,71,200'], 'sugar_cane', 'neighbor base stays planted')
    assert.equal(world['106,72,200'], named ? 'sugar_cane' : 'air')
    assert.equal(calls.some(c => c.startsWith('dig ') && c.includes('x=106 y=72 z=200')), !named)
  })
}

test('farm.harvest replant=false cuts and collects but defers every planting until maintenance tidies', async () => {
  const { api, calls, world } = harvestApi()
  const out = await farmHarvest.run(api, { place: PLAN.name, replant: false })
  assert.equal(out.harvested.wheat, 81)
  assert.equal(out.replanted, 0)
  assert.equal(out.deferred, 81)
  assert.equal(out.notReplanted, null, 'intentional deferral is not a seed or placement failure')
  assert.ok(calls.some(c => c.startsWith('collect ')))
  assert.ok(!calls.some(c => c.startsWith('place ')))
  assert.equal(world['96,71,196'], 'air')
  assert.equal(world['106,71,200'], 'wheat#7')
})
