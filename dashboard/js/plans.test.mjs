import test from 'node:test'
import assert from 'node:assert/strict'
import { compareCell, comparePlan, expectedAt, listPlans } from './plans.mjs'
import { PLAN_LEGEND } from '../../src/lib/plan.mjs'

const spec = (ch, extra = {}) => ({ ...PLAN_LEGEND[ch], ...extra })

const compareCases = [
  ['wheat crop, wheat', spec('w'), 'wheat', 'match'],
  ['wheat crop, carrots', spec('w'), 'carrots', 'wrong'],
  ['wheat crop, air', spec('w'), 'air', 'missing'],
  ['melon stem, attached stem', spec('m'), 'attached_melon_stem', 'match'],
  ['generic crop, potatoes', spec('*'), 'potatoes', 'match'],
  ['generic crop, sugar cane', spec('*'), 'sugar_cane', 'wrong'],
  ['sugar cane', spec('s'), 'sugar_cane', 'match'],
  ['water, water', spec('~'), 'water', 'match'],
  ['water, waterlogged cover slab', spec('~'), 'oak_slab', 'match'],
  ['water, air', spec('~'), 'air', 'missing'],
  ['water, dirt', spec('~'), 'dirt', 'wrong'],
  ['path, dirt_path', spec('.'), 'dirt_path', 'match'],
  ['path, grass_block', spec('.'), 'grass_block', 'match'],
  ['path, air', spec('.'), 'air', 'missing'],
  ['path, lava', spec('.'), 'lava', 'wrong'],
  ['fence, oak_fence', spec('#'), 'oak_fence', 'match'],
  ['fence, birch_fence (non-literal family)', spec('#'), 'birch_fence', 'match'],
  ['fence, literal oak_fence vs birch_fence', spec('#', { literal: true }), 'birch_fence', 'wrong'],
  ['fence, gate', spec('#'), 'oak_fence_gate', 'wrong'],
  ['fence, air', spec('#'), 'air', 'missing'],
  ['gate, oak_fence_gate', spec('G'), 'oak_fence_gate', 'match'],
  ['gate, fence', spec('G'), 'oak_fence', 'wrong'],
  ['torch post, fence', spec('T'), 'oak_fence', 'match'],
  ['chest, chest', spec('C'), 'chest', 'match'],
  ['chest, trapped_chest', spec('C'), 'trapped_chest', 'match'],
  ['composter', spec('K'), 'composter', 'match'],
  ['table', spec('A'), 'crafting_table', 'match'],
  ['flower, poppy', spec('F'), 'poppy', 'match'],
  ['flower, stone', spec('F'), 'stone', 'wrong'],
  ['sapling, spruce_sapling', spec('t'), 'spruce_sapling', 'match'],
  ['tree, grown log', { kind: 'tree', species: 'oak', item: 'oak_sapling' }, 'oak_log', 'match'],
  ['block literal', { kind: 'block', item: 'stone', literal: true }, 'stone', 'match'],
  ['block literal, other', { kind: 'block', item: 'stone', literal: true }, 'dirt', 'wrong'],
  ['air spec, air', { kind: 'air' }, 'air', 'match'],
  ['air spec, cave_air', { kind: 'air' }, 'cave_air', 'match'],
  ['air spec, stone', { kind: 'air' }, 'stone', 'wrong'],
  ['reserved, stone', { kind: 'reserved' }, 'stone', 'match'],
  ['ground farmland, farmland', { kind: 'ground', ground: 'farmland' }, 'farmland', 'match'],
  ['ground farmland, dirt', { kind: 'ground', ground: 'farmland' }, 'dirt', 'wrong'],
  ['ground farmland, air', { kind: 'ground', ground: 'farmland' }, 'air', 'missing'],
  ['no column dumped', spec('w'), null, 'unknown'],
  ['no column dumped, reserved', { kind: 'reserved' }, null, 'unknown']
]
for (const [label, s, actual, status] of compareCases) {
  test(`compareCell: ${label}`, () => assert.equal(compareCell(s, actual), status))
}

test('expectedAt describes every legend kind', () => {
  for (const ch of Object.keys(PLAN_LEGEND)) assert.equal(typeof expectedAt(spec(ch)).label, 'string')
})

const legendLayers = {
  legend: { '#': { kind: 'fence', item: 'oak_fence', ground: 'dirt', literal: false }, g: { kind: 'ground', ground: 'farmland' }, w: { kind: 'crop', crop: 'wheat', seed: 'wheat_seeds', ground: 'farmland', literal: false }, '~': { kind: 'water', ground: 'water', cover: 'oak_slab', literal: false } },
  layers: [{ y: 0, rows: ['~g~'] }, { y: 1, rows: ['#w_'] }]
}
const place = { name: 'tiny', kind: 'farm', by: 'bob', note: 'n', x: 10, y: 70, z: -5, structure: legendLayers }
const world = { '10,70,-5': 'water', '11,70,-5': 'farmland', '10,71,-5': 'air', '11,71,-5': 'carrots' }
const blockAt = (x, y, z) => world[`${x},${y},${z}`] ?? null

test('comparePlan counts and grids', () => {
  const r = comparePlan({ place, blockAt })
  assert.deepEqual([r.name, r.total, r.match, r.missing, r.wrong, r.unknown], ['tiny', 5, 2, 1, 1, 1])
  assert.equal(r.percent, 40)
  assert.deepEqual(r.layers.map(l => l.y), [0, 1])
  assert.deepEqual(r.layers[1].rows[0].map(c => [c.ch, c.status, c.actual]), [['#', 'missing', 'air'], ['w', 'wrong', 'carrots'], ['_', 'free', null]])
  assert.equal(r.layers[1].rows[0][0].expected, 'oak_fence')
})

test('comparePlan marks unconstrained cells free and does not count them', () => {
  const p = { ...place, structure: { ...legendLayers, layers: [{ y: 0, rows: ['~__'] }] } }
  const r = comparePlan({ place: p, blockAt })
  assert.deepEqual([r.total, r.match], [1, 1])
  assert.equal(r.layers[0].rows[0][1].status, 'free')
})

test('listPlans lists only places with a structure, with bounds', () => {
  const list = listPlans([{ name: 'spot', x: 1, y: 1, z: 1 }, place])
  assert.deepEqual(list, [{ name: 'tiny', kind: 'farm', by: 'bob', note: 'n', x: 10, y: 70, z: -5, bounds: { x1: 10, y1: 70, z1: -5, x2: 12, y2: 71, z2: -5 }, cells: 5, layers: 2 }])
})

test('listPlans skips a place whose structure does not parse', () => {
  assert.deepEqual(listPlans([{ name: 'bad', x: 0, y: 0, z: 0, structure: { layers: [] } }]), [])
})
