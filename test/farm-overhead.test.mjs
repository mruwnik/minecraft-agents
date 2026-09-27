import test from 'node:test'
import assert from 'node:assert/strict'
import { planCells, parsePlan } from '../src/lib.mjs'
import { farmIssues } from '../src/farm/attention.mjs'
import { overheadTreeBlocks, overheadTreeLine, OVERHEAD_TREE_HEIGHT } from '../src/farm/overhead.mjs'
import maintain from '../library/farm/maintain.mjs'
import fields from '../library/farm/fields.mjs'
import tidy from '../library/farm/tidy.mjs'
import { fakeApi } from './helpers.mjs'

const field = plan => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan, parsed: parsePlan(plan), cells: planCells({ plan, x: 0, y: 63, z: 0 }) })

test('overhead scan is bounded, read-only, and restricted to crop/path columns', () => {
  const plan = field('w.tsB')
  const world = {
    '0,65,0': 'oak_log', // low clutter, outside this scan
    '0,66,0': 'oak_log',
    '0,67,0': 'stripped_oak_wood',
    '1,68,0': 'oak_leaves', // planned path
    '2,68,0': 'oak_log', // planned sapling
    '3,70,0': 'sugar_cane',
    '4,72,0': 'bamboo',
    '5,68,0': 'oak_log', // outside plan
    [`0,${63 + OVERHEAD_TREE_HEIGHT},0`]: 'oak_leaves',
    [`0,${64 + OVERHEAD_TREE_HEIGHT},0`]: 'oak_log'
  }
  const { api, calls } = fakeApi({ world })
  const read = []
  const found = overheadTreeBlocks(plan.cells, (x, y, z) => { read.push({ x, y, z }); return api.block(x, y, z) })
  assert.deepEqual(found.map(b => `${b.name}@${b.x},${b.y},${b.z}`), ['oak_log@0,66,0', 'stripped_oak_wood@0,67,0', 'oak_leaves@0,95,0', 'oak_leaves@1,68,0'])
  assert.ok(read.every(p => p.y >= 66 && p.y <= 95 && p.x !== 2 && p.x < 5))
  const line = overheadTreeLine(found)
  assert.match(line, /2 log\/wood, 2 leaves/)
  assert.match(line, /y=66\.\.95/)
  assert.match(line, /oak_log@0,66,0/)
  assert.match(line, /upper blocks were left intact/)
  assert.deepEqual(calls, [])
})

test('maintain clears a tree base, reports the remaining canopy, and continues planting without high digs', async () => {
  const plan = field('w')
  const world = { '0,63,0': 'farmland', '0,64,0': 'oak_log', '0,65,0': 'oak_log', '0,66,0': 'oak_log', '0,67,0': 'oak_leaves' }
  const items = { wheat_seeds: 2, stone_hoe: 1 }
  const { api, calls, events } = fakeApi({ place: plan, places: [plan], world, items, answers: {
    'farm.harvest': { harvested: {}, replanted: 0 },
    dig: p => { assert.ok(p.y <= 65, 'tidy never expands into high tree removal'); world[`${p.x},${p.y},${p.z}`] = 'air' },
    place: p => { world[`${p.x},${p.y},${p.z}`] = 'wheat#0'; items.wheat_seeds-- }
  } })
  const result = await maintain.run(api, { place: plan.name, compost: false })
  assert.equal(result.cleared, '2(oak_log)')
  assert.equal(result.replanted, 1)
  assert.equal(world['0,66,0'], 'oak_log')
  assert.equal(world['0,67,0'], 'oak_leaves')
  assert.match(result.overhead_tree, /1 log\/wood, 1 leaves/)
  assert.match(result.overhead_tree, /oak_log@0,66,0/)
  assert.deepEqual(calls.filter(c => c.startsWith('dig ')), ['dig 0,65,0', 'dig 0,64,0'])
  const attention = events.filter(e => e.type === 'farm_attention')
  assert.equal(attention.length, 1)
  assert.equal(attention[0].reasons.overhead_tree, result.overhead_tree)
  assert.equal(farmIssues(result).overhead_tree, result.overhead_tree, 'routine sees this as unfinished work')
})

test('fields reports overhead tree separately from low clutter without emitting repeated events', async () => {
  const plan = field('w')
  const { api, calls, events } = fakeApi({ places: [plan], world: { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '0,66,0': 'oak_log' } })
  for (let n = 0; n < 2; n++) {
    const result = await fields.run(api, { place: plan.name })
    assert.match(result.text, /overhead_tree: .*oak_log@0,66,0/)
    assert.doesNotMatch(result.text, /clutter=/)
  }
  assert.deepEqual(calls, [])
  assert.deepEqual(events, [])
})

test('standalone tidy reports upper tree even when no low blocks remain to clear', async () => {
  const plan = field('w')
  const { api, calls, events } = fakeApi({ places: [plan], world: { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '0,66,0': 'oak_log' } })
  const result = await tidy.run(api, { place: plan.name })
  assert.equal(result.cleared, 0)
  assert.equal(result.already, undefined, 'does not claim the tree is fully cleared')
  assert.match(result.overhead_tree, /oak_log@0,66,0/)
  assert.equal(events[0].type, 'farm_attention')
  assert.equal(events[0].reasons.overhead_tree, result.overhead_tree)
  assert.ok(!calls.some(c => c.startsWith('dig ')))
})

test('a low obstruction needing a missing pickaxe reports attention while upper tree detection and other sowing continue', async () => {
  const plan = field('ww')
  const world = { '0,63,0': 'farmland', '0,64,0': 'cobblestone', '0,66,0': 'oak_log', '1,63,0': 'farmland', '1,64,0': 'air' }
  const { api, calls, events } = fakeApi({ place: plan, world, items: { wheat_seeds: 2, stone_hoe: 1 }, answers: {
    'farm.harvest': { harvested: {}, replanted: 0 },
    dig: new Error('farm.maintain/dig: cobblestone needs a wooden_pickaxe or better: you carry none, craft one first'),
    place: p => { assert.equal(p.x, 1); world['1,64,0'] = 'wheat#0' }
  } })
  const result = await maintain.run(api, { place: plan.name, compost: false })
  assert.match(result.stuck, /needs a wooden_pickaxe/)
  assert.match(result.clutter, /cobblestone/)
  assert.match(result.overhead_tree, /oak_log@0,66,0/)
  assert.equal(result.replanted, 1)
  assert.equal(world['0,64,0'], 'cobblestone')
  assert.equal(world['0,66,0'], 'oak_log')
  assert.equal(calls.filter(c => c.startsWith('dig ')).length, 1)
  assert.ok(events.some(e => e.reasons?.overhead_tree))
  assert.ok(events.some(e => /wooden_pickaxe/.test(e.reasons?.stuck ?? '')))
})

test('legitimate tall crops and unloaded overhead blocks never produce a tree warning', () => {
  const plan = field('sBt')
  const world = { '0,66,0': 'sugar_cane', '0,67,0': 'sugar_cane', '1,68,0': 'bamboo', '2,70,0': 'oak_leaves' }
  const { api } = fakeApi({ world })
  assert.deepEqual(overheadTreeBlocks(plan.cells, api.block), [])
  assert.equal(overheadTreeLine([]), undefined)
  assert.equal(farmIssues({}).overhead_tree, undefined)
})
