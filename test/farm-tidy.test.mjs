// Clutter over a farm: what the plan does not account for, and the tool that clears it.
import test from 'node:test'
import assert from 'node:assert/strict'
import { fakeApi } from './helpers.mjs'
import farmFields from '../library/farm/fields.mjs'
import farmTidy from '../library/farm/tidy.mjs'
import farmMaintain from '../library/farm/maintain.mjs'
import { strays, clutterBlocks, clutterLine, clutterKinds, foreignZone } from '../library/farm/shared/clutter.mjs'
import { parsePlan, planCells, routineSteps } from '../src/lib.mjs'
import fs from 'node:fs'

const block = (name, properties = {}) => ({ name, properties, solid: name !== 'air' && name !== 'water' })
const blockAt = world => (x, y, z) => world[`${x},${y},${z}`] ?? null
const key = b => `${b.x},${b.y},${b.z}`

// a three by three farm: a covered channel, four carrots, a path, a torch post, a fence, a chest and a gate.
// The plan's y is the GROUND block, so everything it puts on a cell stands at y+1 (a torch at y+2, on its post).
const PLAN = { name: 'test-field', kind: 'farm', x: 100, y: 70, z: 200, plan: '~cc\n.cT\n#CG' }
const PLACE = { ...PLAN, parsed: parsePlan(PLAN.plan), cells: planCells(PLAN) }

const built = () => ({
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
})

// three strays the plan never asked for, one weed, one stray light and one crop that wandered
const littered = () => ({
  ...built(),
  '100,71,201': block('dirt'),
  '101,72,200': block('cobblestone'),
  '100,72,200': block('oak_log'),
  '100,71,200': block('short_grass'),
  '102,72,200': block('torch'),
  '102,72,202': block('air')
})

// ---------------------------------------------------------------- what counts as clutter
test('clutterBlocks: a farm built exactly to its plan has none', () => {
  assert.deepEqual(clutterBlocks(PLACE.cells, blockAt(built())), [])
})

test('clutterBlocks: every stray block over the footprint, at ground+1 and ground+2', () => {
  const found = clutterBlocks(PLACE.cells, blockAt(littered()))
  assert.deepEqual(found.map(b => `${b.name}@${key(b)}`), ['oak_log@100,72,200', 'cobblestone@101,72,200', 'dirt@100,71,201'])
})

for (const [why, at, name] of [
  ['a crop on its own bed', '101,71,201', 'carrots'],
  ['a chest the plan marks', '101,71,202', 'chest'],
  ['a gate of any wood', '102,71,202', 'birch_fence_gate'],
  ['a fence of any wood: Chani fenced her oak plan in birch', '100,71,202', 'birch_fence'],
  ['a post of any wood under a torch', '102,71,201', 'spruce_fence'],
  ['a channel covered with a slab of any kind', '100,71,200', 'stone_slab'],
  ['a sapling of any tree on a sapling cell', '102,71,201', 'oak_fence'],
  ['a fence post under a torch', '102,71,201', 'oak_fence'],
  ['the torch the post carries', '102,72,201', 'torch'],
  ['a slab laid over its own channel', '100,71,200', 'oak_slab'],
  ['a crop that wandered onto a path', '100,71,201', 'melon'],
  ['the cell\'s own crop standing at ground+2, as cane and bamboo do', '101,72,201', 'carrots'],
  ['weeds, which grow straight back', '100,71,201', 'tall_grass'],
  ['a flower', '100,71,201', 'dandelion'],
  ['water, which no dig may touch', '100,71,200', 'water'],
  ['a cell nobody has seen', '100,71,201', null]
]) {
  test(`clutterBlocks: ${why} is not clutter`, () => {
    const world = built()
    if (name === null) delete world[at]
    else world[at] = block(name)
    assert.deepEqual(clutterBlocks(PLACE.cells, blockAt(world)).filter(b => key(b) === at), [])
  })
}

for (const [why, at, name] of [
  ['a stray torch is somebody\'s light, not clutter', '102,72,200', 'torch'],
  ['a chest off the plan holds somebody\'s things', '100,71,201', 'chest'],
  ['a bed is somebody\'s', '100,71,201', 'red_bed']
]) {
  test(`strays: ${why}`, () => {
    const world = { ...built(), [at]: block(name) }
    const kept = strays(PLACE.cells, blockAt(world)).filter(b => key(b) === at)
    assert.deepEqual(kept.map(b => [b.name, Boolean(b.keep)]), [[name, true]])
  })
}

test('clutterLine: counts first, then the kinds, commonest first', () => {
  const found = clutterBlocks(PLACE.cells, blockAt({ ...littered(), '102,72,202': block('dirt') }))
  assert.deepEqual([found.length, clutterLine(found), clutterKinds(found)],
    [4, '4(dirt,cobblestone,oak_log)', 'dirt,cobblestone,oak_log'])
})

test('clutterLine: nothing over the field renders as nothing at all', () => {
  assert.equal(clutterLine([]), undefined)
})

// ---------------------------------------------------------------- whose ground it is
for (const [why, zone, me, expected] of [
  ['another agent\'s zone is theirs', 'chani-farm', 'Arren', 'chani-farm'],
  ['my own zone is mine', 'arren-farm', 'Arren', undefined],
  ['my own zone, whatever the case', 'Arren-Farm', 'arren', undefined],
  ['a starter zone is everybody\'s', 'starter-house', 'Arren', undefined]
]) {
  test(`foreignZone: ${why}`, () => {
    const zones = [{ name: zone, x1: 99, y1: 60, z1: 199, x2: 103, y2: 80, z2: 203 }]
    assert.equal(foreignZone(zones, me, { x: 100, y: 71, z: 201 })?.name, expected)
  })
}

test('foreignZone: a block outside every zone is nobody\'s', () => {
  const zones = [{ name: 'chani-farm', x1: 0, y1: 0, z1: 0, x2: 5, y2: 5, z2: 5 }]
  assert.equal(foreignZone(zones, 'Arren', { x: 100, y: 71, z: 201 }), undefined)
})

// ---------------------------------------------------------------- the census
const fieldsApi = (world, extra = {}) => {
  const made = fakeApi({ places: [PLAN], ...extra })
  made.api.block = blockAt(world)
  return made
}

test('farm.fields: clutter is counted on the census line it already prints', async () => {
  const { api } = fieldsApi(littered())
  const out = await farmFields.run(api, { place: 'test-field' })
  assert.match(out.text, /^test-field \d+m .*clutter=3\(cobblestone,dirt,oak_log\)$/)
})

test('farm.fields: a clean field says nothing about clutter', async () => {
  const { api } = fieldsApi(built())
  const out = await farmFields.run(api, { place: 'test-field' })
  assert.equal(/clutter/.test(out.text), false)
})

// ---------------------------------------------------------------- farm.tidy
const tidyApi = ({ world = littered(), zones = [], me = 'Arren', digs = () => null, items = {} } = {}) => {
  const made = fakeApi({
    places: [PLAN],
    items,
    answers: {
      zones: { zones },
      collect: { picked: 3 },
      dig: ({ x, y, z }) => {
        const refused = digs({ x, y, z })
        if (refused) throw new Error(refused)
        world[`${x},${y},${z}`] = block('air')
        return { dug: 'ok' }
      }
    }
  })
  made.api.block = blockAt(world)
  made.api.me = () => me
  return { ...made, world }
}

test('farm.tidy: walks to each stray block, digs it, picks the drops up and says what went', async () => {
  const { api, calls } = tidyApi()
  const out = await farmTidy.run(api, { place: 'test-field' })
  assert.deepEqual([out.cleared, out.left, out.kinds, out.picked], [3, 0, 'cobblestone,dirt,oak_log', 3])
  assert.deepEqual(calls.filter(c => c.startsWith('dig')),
    ['dig 100,72,200', 'dig 101,72,200', 'dig 100,71,201'])
})

test('farm.tidy: every dig is walked to first', async () => {
  const { api, calls } = tidyApi()
  await farmTidy.run(api, { place: 'test-field' })
  assert.deepEqual(calls.slice(0, 4), [
    'zones',
    'goto x=100 y=72 z=200 range=3',
    'dig 100,72,200',
    'goto x=101 y=72 z=200 range=3'
  ])
})

test('farm.tidy: nothing the plan accounts for, and no crop, is ever dug', async () => {
  const { api, calls } = tidyApi()
  await farmTidy.run(api, { place: 'test-field' })
  const safe = ['101,71,200', '102,71,200', '101,71,201', '102,71,201', '102,72,201', '100,71,202', '101,71,202', '102,71,202', '100,71,200', '102,72,200']
  assert.deepEqual(calls.filter(c => safe.some(at => c === `dig ${at}`)), [])
})

test('farm.tidy: a clean field is left alone and says so', async () => {
  const { api, calls } = tidyApi({ world: built() })
  const out = await farmTidy.run(api, { place: 'test-field' })
  assert.deepEqual([out.cleared, out.left, out.already, calls.filter(c => c.startsWith('dig'))], [0, 0, 'test-field has nothing over it that its plan does not ask for', []])
})

test('farm.tidy: somebody else\'s block over the field is named, not dug', async () => {
  const { api } = tidyApi({ world: { ...littered(), '102,72,202': block('chest') } })
  const out = await farmTidy.run(api, { place: 'test-field' })
  assert.deepEqual([out.cleared, out.leftAlone], [3, 'torch@102,72,200 chest@102,72,202'])
})

test('farm.tidy: a stray block inside somebody else\'s zone is refused by name', async () => {
  const zones = [{ name: 'chani-farm', x1: 99, y1: 60, z1: 199, x2: 103, y2: 80, z2: 203 }]
  const { api, calls } = tidyApi({ zones })
  await assert.rejects(farmTidy.run(api, { place: 'test-field' }), /the 3 stray blocks over test-field stand inside the protected zone chani-farm/)
  assert.deepEqual(calls.filter(c => c.startsWith('dig')), [])
})

test('farm.tidy: one block in a foreign zone is refused in the singular', async () => {
  const zones = [{ name: 'chani-farm', x1: 101, y1: 60, z1: 199, x2: 103, y2: 80, z2: 203 }]
  const world = { ...built(), '101,72,200': block('cobblestone') }
  const { api } = tidyApi({ world, zones })
  await assert.rejects(farmTidy.run(api, { place: 'test-field' }), /the 1 stray block over test-field stands inside the protected zone chani-farm/)
})

test('farm.tidy: my own zone is no obstacle', async () => {
  const zones = [{ name: 'arren-farm', x1: 99, y1: 60, z1: 199, x2: 103, y2: 80, z2: 203 }]
  const { api } = tidyApi({ zones })
  const out = await farmTidy.run(api, { place: 'test-field' })
  assert.equal(out.cleared, 3)
})

test('farm.tidy: the part of a field inside a foreign zone is skipped and the rest cleared', async () => {
  const zones = [{ name: 'chani-farm', x1: 100, y1: 60, z1: 199, x2: 100, y2: 80, z2: 203 }]
  const { api, calls } = tidyApi({ zones })
  const out = await farmTidy.run(api, { place: 'test-field' })
  assert.deepEqual([out.cleared, out.left, out.inZone, calls.filter(c => c.startsWith('dig'))],
    [1, 2, 'chani-farm:2', ['dig 101,72,200']])
})

test('farm.tidy: a block it cannot reach stops the round with the reason and the cell', async () => {
  const { api, calls } = tidyApi({ digs: ({ x }) => x === 100 ? 'no path to 100,71,201' : null })
  const out = await farmTidy.run(api, { place: 'test-field' })
  assert.deepEqual([out.cleared, out.left, calls.filter(c => c.startsWith('dig')).length], [0, 3, 1])
  assert.match(out.stopped, /100,72,200.*no path/)
})

test('farm.tidy: a dig the server dropped is counted as left, not cleared', async () => {
  const world = littered()
  const made = fakeApi({ places: [PLAN], answers: { zones: { zones: [] }, collect: { picked: 0 }, dig: () => ({ dug: 'ok' }) } })
  made.api.block = blockAt(world)
  made.api.me = () => 'Arren'
  const out = await farmTidy.run(made.api, { place: 'test-field' })
  assert.deepEqual([out.cleared, out.left], [0, 3])
})

test('farm.tidy: with no place it sweeps every plan in range', async () => {
  const { api, calls } = tidyApi()
  const out = await farmTidy.run(api, { range: 400 })
  assert.deepEqual([out.plans, out.cleared], ['test-field', 3])
  assert.equal(calls.filter(c => c.startsWith('dig')).length, 3)
})

test('farm.tidy: no plan within range is a refusal that says so', async () => {
  const { api } = tidyApi()
  await assert.rejects(farmTidy.run(api, { range: 4 }), /no farm plan within 4 blocks/)
})

// #144: clearing rubble off a field is work on the ground it stands on, so it asks the one ownership question every
// tool asks, in the same words, instead of inventing a second rule beside the zone one.
test('farm.tidy: somebody else\u2019s field with a silent note is refused before anything is read', async () => {
  const { api, calls } = tidyApi()
  api.places = () => [{ ...PLAN, by: 'Chani', note: 'carrots, 4x4' }]
  await assert.rejects(farmTidy.run(api, { place: 'test-field' }), /test-field is Chani's ground/)
  assert.deepEqual(calls, [])
})

test('farm.tidy: somebody else\u2019s field whose note invites work is swept like my own', async () => {
  const { api } = tidyApi()
  api.places = () => [{ ...PLAN, by: 'Chani', note: 'anyone welcome, harvest and tidy' }]
  const out = await farmTidy.run(api, { place: 'test-field' })
  assert.deepEqual([out.cleared, out.left], [3, 0])
})

test('farm.tidy: a place with no plan of its own is refused by name', async () => {
  const { api } = tidyApi()
  await assert.rejects(farmTidy.run(api, { place: 'nowhere' }), /no plan called nowhere/)
})

// ---------------------------------------------------------------- farm.maintain says a sweep is worth it
const maintainApi = world => {
  const made = fakeApi({
    place: PLACE,
    items: { wheat_seeds: 64, carrot: 64, oak_slab: 8, water_bucket: 1 },
    answers: { 'farm.harvest': { harvested: {}, replanted: 0 }, 'farm.compost': { fed: 0 } }
  })
  made.api.block = blockAt(world)
  return made
}

test('farm.maintain: a sweep over a littered field names the clutter and the tool for it', async () => {
  const { api } = maintainApi(littered())
  const out = await farmMaintain.run(api, { place: 'test-field' })
  assert.match(out.clutter, /^3\(cobblestone,dirt,oak_log\).*farm\.tidy place=test-field$/)
})

test('farm.maintain: a clean field says nothing about clutter', async () => {
  const { api } = maintainApi(built())
  const out = await farmMaintain.run(api, { place: 'test-field' })
  assert.equal(out.clutter, undefined)
})

test('the farmer routine clears the rubble before it works the field', () => {
  const steps = JSON.parse(fs.readFileSync(new URL('../roles/farmer/homestead.json', import.meta.url), 'utf8'))
  const read = routineSteps({ steps, place: 'test-field' }, () => null)
  assert.deepEqual([read.error, read.steps.map(s => s.action)], [undefined, ['farm.tidy', 'farm.maintain', 'farm.compost']])
})
