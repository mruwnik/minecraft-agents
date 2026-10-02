// farm.maintain waters its own channels (card 72e49b3d). The dry cells of one carrot patch were reported as
// "dry and I carry no water" sweep after sweep while an empty bucket sat in the field chest: a sweep now fetches its
// own water (the nearest still source within range, the carried bucket filled there) before it lists the jobs, and
// again whenever a pour has emptied the bucket. A channel stays dry for exactly two reasons, and the skipped= line
// says which: no bucket at all (and how to make one), or no water within range.
import test from 'node:test'
import { readFileSync } from 'node:fs'
import assert from 'node:assert/strict'
import { parsePlan, planBill } from '../src/lib.mjs'
import { planCells } from './plan-fixture.mjs'
import { NO_BUCKET, noWaterLine, waterShortfall } from '../src/build/plan.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
import { fakeApi as baseFakeApi } from './helpers.mjs'
const fakeApi = options => {
  const made = baseFakeApi(options)
  const read = made.api.block
  made.api.block = (x, y, z) => read(x, y, z) ?? { name: y <= 63 ? 'dirt' : 'air', solid: y <= 63, properties: {} }
  return made
}
const withoutShoreWalks = calls => calls.filter(c => !c.startsWith('goto '))
import maintainFarm from '../library/farm/maintain.mjs'

const NO_WATER = 'no water within 32 blocks'
test('the two reasons, word for word', () => {
  assert.equal(NO_BUCKET, 'no bucket: craft item=bucket (3 iron_ingot)')
  assert.equal(noWaterLine(32), NO_WATER)
  assert.equal(noWaterLine(10), 'no water within 10 blocks')
})

// ---------------------------------------------------------------- waterShortfall
// the pockets change as a bucket is filled or poured: the fake's items are the pockets, and the fake's answers move the water
const pockets = items => ({
  items,
  fill: () => { items.bucket--; items.water_bucket = (items.water_bucket ?? 0) + 1; return {} },
  pour: () => { items.water_bucket--; items.bucket = (items.bucket ?? 0) + 1; return {} }
})
const found = positions => ({ find_blocks: () => ({ positions }) })
const FIND = 'find_blocks block=water maxDistance=32 count=32'

for (const [name, given, expected] of [
  ['a full bucket carried: nothing to do', { items: { water_bucket: 1 }, world: {} }, { why: null, calls: [] }],
  ['no bucket at all: how to make one, and no search', { items: { iron_ingot: 3 }, world: {} }, { why: NO_BUCKET, calls: [] }],
  ['a bucket and no water anywhere near', { items: { bucket: 1 }, world: {}, positions: [] }, { why: NO_WATER, calls: [FIND] }],
  ['the nearest source is filled from, not the first the search lists',
    { items: { bucket: 1 }, world: { '9,63,0': 'water', '3,63,0': 'water' }, positions: [{ x: 9, y: 63, z: 0 }, { x: 3, y: 63, z: 0 }] },
    { why: null, calls: [FIND, 'fill 3,63,0'] }],
  ['flowing water is passed over for a still source',
    { items: { bucket: 1 }, world: { '1,63,0': 'water#3', '4,63,0': 'water' }, positions: [{ x: 1, y: 63, z: 0 }, { x: 4, y: 63, z: 0 }] },
    { why: null, calls: [FIND, 'fill 4,63,0'] }],
  ['only flowing water in range is no water',
    { items: { bucket: 1 }, world: { '1,63,0': 'water#3' }, positions: [{ x: 1, y: 63, z: 0 }] },
    { why: NO_WATER, calls: [FIND] }],
  ['a fill that fails moves on to the next source',
    { items: { bucket: 1 }, world: { '2,63,0': 'water', '6,63,0': 'water' }, positions: [{ x: 2, y: 63, z: 0 }, { x: 6, y: 63, z: 0 }], fillFails: 1 },
    { why: null, calls: [FIND, 'fill 2,63,0', 'fill 6,63,0'] }],
  ['every fill failing is no water',
    { items: { bucket: 1 }, world: { '2,63,0': 'water' }, positions: [{ x: 2, y: 63, z: 0 }], fillFails: 9 },
    { why: NO_WATER, calls: [FIND, 'fill 2,63,0'] }]
]) {
  test(`waterShortfall: ${name}`, async () => {
    const hands = pockets(given.items)
    let fails = given.fillFails ?? 0
    const fill = () => { if (fails-- > 0) throw new Error('fill: cannot see the source'); return hands.fill() }
    const { api, calls } = fakeApi({ world: given.world, items: given.items, answers: { ...found(given.positions ?? []), fill } })
    assert.equal(await waterShortfall(api), expected.why)
    assert.deepEqual(withoutShoreWalks(calls), expected.calls)
  })
}

test('waterShortfall: the range is in the reason and in the search', async () => {
  const { api, calls } = fakeApi({ items: { bucket: 1 }, answers: found([]) })
  assert.equal(await waterShortfall(api, 10), 'no water within 10 blocks')
  assert.deepEqual(calls, ['find_blocks block=water maxDistance=10 count=32'])
})

// ---------------------------------------------------------------- the sweep
// a plan at y=63: the body walks at 64 on dirt, a bed of growing wheat at 0,63,0 anchors the plan, and the channel
// cells after it are dry dirt with dirt under them to pour onto. The pond, when there is one, lies east at x=8
const fakePlace = plan => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan, parsed: parsePlan(plan), cells: planCells({ plan, x: 0, y: 63, z: 0 }), bill: planBill(parsePlan(plan)) })
const field = (plan, edits = {}) => {
  const world = {}
  for (let x = -3; x <= 10; x++) {
    for (let z = -3; z <= 3; z++) {
      world[`${x},62,${z}`] = 'dirt'
      world[`${x},63,${z}`] = 'dirt'
      world[`${x},64,${z}`] = 'air'
      world[`${x},65,${z}`] = 'air'
    }
  }
  return { ...world, '0,63,0': 'farmland', '0,64,0': 'wheat#3', ...edits }
}
const POND = { '8,63,0': 'water', '8,63,1': 'water' }
const pondFound = found([{ x: 8, y: 63, z: 1 }, { x: 8, y: 63, z: 0 }])
const sweep = async ({ plan, world, items, answers = {} }) => {
  const hands = pockets(items)
  const made = fakeApi({ place: fakePlace(plan), world, items, answers: { 'farm.harvest': { harvested: {}, replanted: 0 }, fill: hands.fill, pour: hands.pour, ...answers } })
  const summary = await maintainFarm.run(made.api, { place: 'test-field' })
  return { summary, water: made.calls.filter(c => /^(find_blocks|fill|dig|pour) /.test(c)), calls: made.calls, events: made.events }
}
const cellKey = p => `${p.x},${p.y},${p.z}`

for (const [name, given, expected] of [
  ['no channel in the plan: no search for water',
    { plan: 'w', world: field('w'), items: {} },
    { water: [], poured: 0, skipped: undefined }],
  ['a dry channel and no bucket: the cell is skipped with the bucket recipe, and no search',
    { plan: 'w~', world: field('w~'), items: {} },
    { water: [], poured: 0, skipped: `1,63,0 (${NO_BUCKET})` }],
  ['a dry channel, a bucket and no water: the cell is skipped with the range',
    { plan: 'w~', world: field('w~'), items: { bucket: 1 }, answers: found([]) },
    { water: [FIND], poured: 0, skipped: `1,63,0 (${NO_WATER})` }],
  ['a dry channel, a bucket and a pond: the bucket is filled at the pond, the cell dug open and poured',
    { plan: 'w~', world: field('w~', POND), items: { bucket: 1 }, answers: pondFound },
    { water: [FIND, 'fill 8,63,0', 'dig 1,63,0', 'pour 1,62,0'], poured: 1, skipped: undefined }],
  ['two dry channels and one bucket (the digs come first, in job order): it is filled again after the first pour empties it',
    { plan: 'w~~', world: field('w~~', POND), items: { bucket: 1 }, answers: pondFound },
    { water: [FIND, 'fill 8,63,0', 'dig 1,63,0', 'dig 2,63,0', 'pour 1,62,0', FIND, 'fill 8,63,0', 'pour 2,62,0'], poured: 2, skipped: undefined }],
  ['the pond gone after the first pour: the second cell is skipped with the range, the first was poured',
    { plan: 'w~~', world: field('w~~', POND), items: { bucket: 1 }, answers: { find_blocks: (() => { let asked = 0; return () => ({ positions: asked++ === 0 ? [{ x: 8, y: 63, z: 0 }] : [] }) })() } },
    { water: [FIND, 'fill 8,63,0', 'dig 1,63,0', 'dig 2,63,0', 'pour 1,62,0', FIND], poured: 1, skipped: `2,63,0 (${NO_WATER})` }]
]) {
  test(`farm.maintain: ${name}`, async () => {
    const { summary, water } = await sweep(given)
    assert.deepEqual(water, expected.water)
    assert.equal(summary.poured, expected.poured)
    assert.equal(summary.skipped, expected.skipped)
    assert.equal(Boolean(summary.missing?.includes('water_bucket')), false, 'water_bucket is never a missing= item: skipped= says why the channel stays dry')
  })
}

// ---------------------------------------------------------------- the till gate: no bed tilled before its water holds
test('farm.maintain: the channel is poured before the bed beside it is tilled, and the bed is sown at once', async () => {
  const items = { bucket: 1, oak_slab: 1, wheat_seeds: 1, stone_hoe: 1 }
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air', ...POND })
  const { summary, calls } = await sweep({
    plan: 'w~',
    world,
    items,
    answers: {
      ...pondFound,
      till: p => { world[cellKey(p)] = 'farmland' },
      place: p => {
        world[cellKey(p)] = p.item === 'oak_slab' ? 'oak_slab#top~' : p.item === 'wheat_seeds' ? 'wheat#0' : p.item
        items[p.item] = (items[p.item] ?? 0) - 1
      },
      pour: p => { world[cellKey({ ...p, y: p.y + 1 })] = 'water'; items.water_bucket--; items.bucket = (items.bucket ?? 0) + 1 }
    }
  })
  const work = calls.filter(c => /^(pour|till|place) /.test(c))
  const pourIdx = work.findIndex(c => c.startsWith('pour '))
  const tillIdx = work.findIndex(c => c.startsWith('till '))
  const plantIdx = work.findIndex(c => /wheat_seeds/.test(c))
  assert.ok(tillIdx > pourIdx, 'the channel is poured (and covered) before the bed is tilled')
  assert.equal(plantIdx, tillIdx + 1, 'the bed is sown at once, right after its own till')
  assert.equal(summary.farm_needs_water, undefined)
})

test('farm.maintain: a dry bed is tilled anyway when its seed is in hand, and sown at once', async () => {
  const items = { wheat_seeds: 1, stone_hoe: 1 }
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air' })
  const { summary, calls } = await sweep({
    plan: 'w~',
    world,
    items,
    answers: {
      till: p => { world[cellKey(p)] = 'farmland' },
      place: p => { world[cellKey(p)] = 'wheat#0'; items[p.item] = (items[p.item] ?? 0) - 1 }
    }
  })
  const work = calls.filter(c => /^(till|place) /.test(c))
  assert.deepEqual(work, ['till 0,63,0', 'place item=wheat_seeds x=0 y=64 z=0'])
  assert.match(summary.farm_needs_water, /0,63,0/)
  assert.equal(/dry/.test(summary.bare ?? ''), false)
})

test('farm.maintain: a checkpoint right after the dry till is skipped, so its plant runs before any hand-back', async () => {
  const items = { wheat_seeds: 1, stone_hoe: 1 }
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air' })
  const made = fakeApi({
    place: fakePlace('w~'),
    world,
    items,
    answers: {
      'farm.harvest': { harvested: {}, replanted: 0 },
      till: p => { world[cellKey(p)] = 'farmland' },
      place: p => { world[cellKey(p)] = 'wheat#0'; items[p.item] = (items[p.item] ?? 0) - 1 }
    }
  })
  // a checkpoint hands back the instant a till has happened: unfixed, that fires right after the till and before its
  // plant ever runs; fixed, the plant goes in first and only then does a checkpoint get a turn
  made.api.checkpoint = async () => { if (made.calls.some(c => c.startsWith('till '))) throw new CompositeHandBack('hurt') }
  await assert.rejects(maintainFarm.run(made.api, { place: 'test-field' }), /hurt/)
  const work = made.calls.filter(c => /^(till|place) /.test(c))
  assert.deepEqual(work, ['till 0,63,0', 'place item=wheat_seeds x=0 y=64 z=0'])
})

test('farm.maintain: a dry till that fails still gets its own checkpoint, seed in hand or not', async () => {
  const items = { wheat_seeds: 2, stone_hoe: 1 }
  const world = { '0,63,0': 'dirt', '0,64,0': 'air', '1,63,0': 'dirt', '1,64,0': 'air' }
  const made = fakeApi({
    place: fakePlace('ww'),
    world,
    items,
    answers: {
      'farm.harvest': { harvested: {}, replanted: 0 },
      till: p => new Error(`till: tilled nothing: 1 still dirt: is there a block on top of it? (first ${p.x},${p.y},${p.z})`)
    }
  })
  made.api.checkpoint = async () => { made.calls.push('checkpoint') }
  await maintainFarm.run(made.api, { place: 'test-field' })
  // a failed dry till never sows its plant (untilled, like any other failed till): it must not borrow the sown-at-once
  // skip meant for a till that actually went in, or two failing beds in a row run with no checkpoint between them at all
  const tillAt = x => made.calls.indexOf(`till ${x},63,0`)
  assert.equal(made.calls[tillAt(0) + 1], 'checkpoint')
  assert.equal(made.calls[tillAt(1) + 1], 'checkpoint')
})

test('farm.maintain: no seed means no till on the dry bed, and farm_needs_water says why and where', async () => {
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air' })
  const { summary, calls, events } = await sweep({ plan: 'w~', world, items: { stone_hoe: 1 } })
  assert.equal(calls.some(c => c.startsWith('till ')), false)
  assert.equal(world['0,63,0'], 'dirt')
  assert.match(summary.farm_needs_water, new RegExp(NO_BUCKET.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')))
  assert.match(summary.farm_needs_water, /0,63,0/)
  assert.match(summary.bare, /dry:1/)
  assert.match(summary.missing, /wheat_seeds:1/)
  assert.ok(events.some(e => e.type === 'farm_attention' && e.reasons.farm_needs_water))
})

// every cell round the bed loaded and solid (walledBed in test/farm-maintain.test.mjs): nothing to stand on
// anywhere within work range of it, dry bed left as dirt besides
test('farm.maintain: a dry bed with seed in hand is held, not tilled, when its plant has nowhere to stand', async () => {
  const world = {}
  for (let x = -5; x <= 5; x++) for (let y = 58; y <= 68; y++) for (let z = -5; z <= 5; z++) world[`${x},${y},${z}`] = 'stone'
  world['0,63,0'] = 'dirt'
  world['0,64,0'] = 'air'
  world['0,65,0'] = 'air'
  const { summary, calls } = await sweep({ plan: 'w', world, items: { wheat_seeds: 5, stone_hoe: 1 } })
  assert.equal(calls.some(c => c.startsWith('till ')), false)
  assert.match(summary.bare, /unreachable:1/)
  assert.match(summary.farm_needs_water, /0,63,0/)
})

test('farm.maintain: a later sweep that waters and tills its bed clears an earlier farm_needs_water', async () => {
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air' })
  let harvests = 0
  const made = fakeApi({
    place: fakePlace('w~'),
    world,
    items: { wheat_seeds: 1, stone_hoe: 1 },
    answers: {
      // the second sweep's own channel gains water before its job list is read, same as one a sweep poured itself
      'farm.harvest': () => {
        harvests++
        if (harvests === 2) world['1,63,0'] = 'water'
        return { harvested: {}, replanted: 0 }
      }
    }
  })
  // made.report only ever merges (Object.assign, same as the real runner): once farm_needs_water is set it would
  // never read back as cleared there. The sweep's own last report is the one summary that actually reflects the fix
  let lastReport = {}
  const report = made.api.report
  made.api.report = partial => { lastReport = partial; return report(partial) }
  made.api.checkpoint = async () => { if (made.report.sweeps === 2) throw new CompositeHandBack('done') }
  await assert.rejects(maintainFarm.run(made.api, { place: 'test-field', days: 2 }), /done/)
  assert.equal(made.report.sweeps, 2)
  assert.ok(made.calls.includes('till 0,63,0'), 'the now-hydrated bed is tilled on the second sweep')
  assert.equal(lastReport.farm_needs_water, undefined)
})

test('farm.maintain: a channel already holding water means no water work, and the bed is tilled', async () => {
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air', '1,63,0': 'oak_slab#top~' })
  const { summary, calls } = await sweep({ plan: 'w~', world, items: { wheat_seeds: 1, stone_hoe: 1 } })
  assert.equal(calls.some(c => /^(find_blocks|fill|pour|dig) /.test(c)), false)
  assert.equal(calls.filter(c => c === 'till 0,63,0').length, 1)
  assert.equal(summary.farm_needs_water, undefined)
})

test('farm.build: a dry bed is tilled anyway when its seed is in hand, and sown at once', async () => {
  const items = { wheat_seeds: 1 }
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air' })
  const made = fakeApi({ place: fakePlace('w~'), world, items, answers: {
    till: p => { world[cellKey(p)] = 'farmland' },
    place: p => { world[cellKey(p)] = 'wheat#0'; items[p.item] = (items[p.item] ?? 0) - 1 }
  } })
  const summary = await buildFarm.run(made.api, { place: 'test-field', partial: true })
  const work = made.calls.filter(c => /^(till|place) /.test(c))
  assert.deepEqual(work, ['till 0,63,0', 'place item=wheat_seeds x=0 y=64 z=0'])
  assert.match(summary.farm_needs_water, /0,63,0/)
  assert.equal(summary.unfinished, undefined)
})

test('farm.build: a checkpoint right after the dry till is skipped, so its plant runs before any hand-back', async () => {
  const items = { wheat_seeds: 1 }
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air' })
  const made = fakeApi({ place: fakePlace('w~'), world, items, answers: {
    till: p => { world[cellKey(p)] = 'farmland' },
    place: p => { world[cellKey(p)] = 'wheat#0'; items[p.item] = (items[p.item] ?? 0) - 1 }
  } })
  // a checkpoint hands back the instant a till has happened: unfixed, that fires right after the till and before its
  // plant ever runs; fixed, the plant goes in first and only then does a checkpoint get a turn
  made.api.checkpoint = async () => { if (made.calls.some(c => c.startsWith('till '))) throw new CompositeHandBack('hurt') }
  await assert.rejects(buildFarm.run(made.api, { place: 'test-field', partial: true }), /hurt/)
  const work = made.calls.filter(c => /^(till|place) /.test(c))
  assert.deepEqual(work, ['till 0,63,0', 'place item=wheat_seeds x=0 y=64 z=0'])
})

test('farm.build: a dry till that fails still gets its own checkpoint, seed in hand or not', async () => {
  const items = { wheat_seeds: 2 }
  const world = field('ww', { '0,63,0': 'dirt', '0,64,0': 'air', '1,63,0': 'dirt', '1,64,0': 'air' })
  const made = fakeApi({ place: fakePlace('ww'), world, items, answers: {
    till: p => new Error(`till: tilled nothing: 1 still dirt: is there a block on top of it? (first ${p.x},${p.y},${p.z})`)
  } })
  made.api.checkpoint = async () => { made.calls.push('checkpoint') }
  await buildFarm.run(made.api, { place: 'test-field', partial: true })
  // a failed dry till never sows its plant (dryKeys holds it, like any other failed till): it must not borrow the
  // sown-at-once skip meant for a till that actually went in, or two failing beds in a row run with no checkpoint
  const tillAt = x => made.calls.indexOf(`till ${x},63,0`)
  assert.equal(made.calls[tillAt(0) + 1], 'checkpoint')
  assert.equal(made.calls[tillAt(1) + 1], 'checkpoint')
})

// job 720 (2026-10-02): one seed, no hoe (lost in a death), grass where every bed goes, no water near. The till of
// the first bed failed for want of a hoe, a failure farm.build steps past, and its plant ran on the grass anyway:
// place refused it outright and the whole build died there
test('farm.build on the live mruwnik-farm plan: a till that fails for want of a hoe never plants its bed on grass', async () => {
  const items = { wheat_seeds: 1 }
  const made = fakeApi({ place: JSON.parse(readFileSync(new URL('./fixtures/mruwnik-farm.json', import.meta.url))), items, answers: {
    till: () => new Error('no hoe: craft item=wooden_hoe (2 planks + 2 sticks)'),
    place: p => made.api.block(p.x, p.y - 1, p.z).name === 'farmland' || p.item !== 'wheat_seeds'
      ? {}
      : new Error(`placed nothing: 1 wheat_seeds needs farmland under it, and there is ${made.api.block(p.x, p.y - 1, p.z).name}: till that block first (first ${p.x},${p.y},${p.z})`)
  } })
  made.api.block = (x, y, z) => ({ name: y < 70 ? 'dirt' : y === 70 ? 'grass_block' : 'air', solid: y <= 70, properties: {} })
  const summary = await buildFarm.run(made.api, { place: 'mruwnik-farm', partial: true })
  assert.ok(made.calls.includes('till 104,70,-106'))
  assert.equal(made.calls.some(c => c.startsWith('place item=wheat_seeds')), false)
  assert.match(summary.stuck, /no hoe/)
})

test('farm.build: no seed means no till on the dry bed, and farm_needs_water says why', async () => {
  const world = field('w~', { '0,63,0': 'dirt', '0,64,0': 'air' })
  const made = fakeApi({ place: fakePlace('w~'), world, items: {} })
  const summary = await buildFarm.run(made.api, { place: 'test-field', partial: true })
  assert.equal(made.calls.some(c => c.startsWith('till ')), false)
  assert.equal(made.calls.some(c => c.startsWith('place') && c.includes('wheat_seeds')), false)
  assert.match(summary.farm_needs_water, new RegExp(NO_BUCKET.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')))
  assert.ok(made.events.some(e => e.type === 'farm_attention' && e.reasons.farm_needs_water))
  assert.equal(summary.unfinished, undefined)
})

// Sources are removed by a bucket in the real world. Model that mutation so a
// pass cannot appear successful while it merely shuffles its own irrigation.
import buildFarm from '../library/farm/build.mjs'
for (const command of [maintainFarm, buildFarm]) {
  for (const external of [false, true]) {
    test(`${command === maintainFarm ? 'maintain' : 'build'} preserves four separated channels (${external ? 'external pond' : 'no external source'})`, async () => {
      const plan = fakePlace('w~.~.~.~')
      const world = field(plan.plan, external ? POND : {})
      const items = { water_bucket: 1, oak_slab: 4, dirt: 8 }
      const hands = pockets(items)
      const key = p => `${p.x},${p.y},${p.z}`
      const made = fakeApi({ place: plan, world, items, answers: {
        'farm.harvest': { harvested: {}, replanted: 0 },
        find_blocks: () => ({ positions: Object.entries(world).filter(([, name]) => name === 'water').map(([at]) => {
          const [x, y, z] = at.split(',').map(Number)
          return { x, y, z }
        }) }),
        dig: p => { world[key(p)] = 'air' },
        fill: p => {
          assert.equal(p.x, 8, 'only the external pond may supply water')
          // The external pond renews; planned channels would lose their water.
          return hands.fill()
        },
        pour: p => { world[key({ ...p, y: p.y + 1 })] = 'water'; return hands.pour() },
        place: p => { world[key(p)] = p.item === 'oak_slab' ? 'oak_slab#top~' : p.item; items[p.item]-- }
      } })
      const summary = await command.run(made.api, { place: plan.name, partial: true })
      assert.equal(summary.poured, external ? 4 : 1)
      assert.equal(summary.covered, external ? 4 : 1)
      assert.equal([1, 3, 5, 7].filter(x => world[`${x},63,0`] === 'oak_slab#top~').length, external ? 4 : 1)
      assert.equal(made.calls.filter(c => c.startsWith('fill ')).length, external ? 3 : 0)
      if (!external) {
        assert.ok(made.events.some(e => e.type === 'farm_attention'))
        assert.match(command === maintainFarm ? summary.skipped : summary.stuck, /preserved planned irrigation.*external source/)
      }
    })
  }
}

test('waterShortfall protects another saved farm, even when it is the closest source', async () => {
  const neighbour = { name: 'neighbour', kind: 'farm', plan: '~', x: 1, y: 63, z: 0 }
  const items = { bucket: 1 }
  const made = fakeApi({ places: [neighbour], items, world: { '1,63,0': 'water', '8,63,0': 'water' }, answers: {
    ...found([{ x: 1, y: 63, z: 0 }, { x: 8, y: 63, z: 0 }]), fill: pockets(items).fill
  } })
  assert.equal(await waterShortfall(made.api), null)
  assert.deepEqual(withoutShoreWalks(made.calls), [FIND, 'fill 8,63,0'])
})

test('waterShortfall widens a search crowded by protected irrigation to find the external pond', async () => {
  const cells = planCells({ plan: '~'.repeat(32), x: 0, y: 63, z: 0 })
  const world = Object.fromEntries([...cells, { x: 0, y: 63, z: 8 }].map(p => [`${p.x},${p.y},${p.z}`, 'water']))
  const items = { bucket: 1 }
  const made = fakeApi({ items, world, answers: {
    find_blocks: ({ count }) => ({ positions: count === 32 ? cells : [...cells, { x: 0, y: 63, z: 8 }] }),
    fill: pockets(items).fill
  } })
  assert.equal(await waterShortfall(made.api, undefined, cells), null)
  assert.deepEqual(withoutShoreWalks(made.calls), [FIND, 'find_blocks block=water maxDistance=32 count=64', 'fill 0,63,8'])
})
