import { test } from 'node:test'
import assert from 'node:assert/strict'
import prismarineRegistry from 'prismarine-registry'
import { stateId } from './fixture.mjs'
import { buildStateTable, OPEN, SOLID, WATER, LAVA, CLIMB, OPENABLE, NARROW, HAZARD_NONE, HAZARD_AVOID, DAMAGE_STAND, DAMAGE_TOUCH, SLOW } from './blocks.mjs'

const registry = prismarineRegistry('26.1')
const table = buildStateTable(registry)

// [block, props, top, base, kind, hazard]; heights in 1/16 block
const cases = [
  ['air', {}, 0, 16, OPEN, HAZARD_NONE],
  ['stone', {}, 16, 0, SOLID, HAZARD_NONE],
  ['oak_slab', { type: 'bottom' }, 8, 0, SOLID, HAZARD_NONE],
  ['oak_slab', { type: 'top' }, 16, 8, SOLID, HAZARD_NONE],
  ['oak_slab', { type: 'bottom', waterlogged: true }, 8, 0, SOLID, HAZARD_NONE],
  ['snow', { layers: 1 }, 0, 16, OPEN, HAZARD_NONE],
  ['snow', { layers: 2 }, 2, 0, SOLID, HAZARD_NONE],
  ['snow', { layers: 8 }, 14, 0, SOLID, HAZARD_NONE],
  ['oak_fence', {}, 24, 0, NARROW, HAZARD_NONE],
  ['cobblestone_wall', {}, 24, 0, NARROW, HAZARD_NONE],
  ['oak_fence_gate', { open: false }, 24, 0, OPENABLE, HAZARD_NONE],
  ['oak_fence_gate', { open: true }, 0, 16, OPENABLE, HAZARD_NONE],
  ['oak_trapdoor', { half: 'bottom', open: false }, 3, 0, OPENABLE, HAZARD_NONE],
  ['oak_trapdoor', { half: 'bottom', open: true }, 16, 0, OPENABLE, HAZARD_NONE],
  ['glass_pane', {}, 16, 0, NARROW, HAZARD_NONE],
  ['iron_bars', {}, 16, 0, NARROW, HAZARD_NONE],
  ['bamboo', {}, 16, 0, NARROW, HAZARD_NONE],
  ['ladder', {}, 16, 0, CLIMB, HAZARD_NONE],
  ['vine', {}, 0, 16, CLIMB, HAZARD_NONE],
  ['water', {}, 0, 16, WATER, HAZARD_NONE],
  ['bubble_column', {}, 0, 16, WATER, HAZARD_NONE],
  ['seagrass', {}, 0, 16, WATER, HAZARD_NONE],
  ['oak_sign', { waterlogged: true }, 0, 16, WATER, HAZARD_NONE],
  ['oak_sign', { waterlogged: false }, 0, 16, OPEN, HAZARD_NONE],
  ['lava', {}, 0, 16, LAVA, HAZARD_AVOID],
  ['fire', {}, 0, 16, OPEN, HAZARD_AVOID],
  ['magma_block', {}, 16, 0, SOLID, DAMAGE_STAND],
  ['soul_sand', {}, 14, 0, SOLID, SLOW],
  ['honey_block', {}, 15, 0, SOLID, SLOW],
  ['campfire', { lit: true }, 7, 0, SOLID, DAMAGE_STAND],
  ['campfire', { lit: false }, 7, 0, SOLID, HAZARD_NONE],
  ['sweet_berry_bush', {}, 0, 16, OPEN, DAMAGE_TOUCH],
  ['wither_rose', {}, 0, 16, OPEN, DAMAGE_TOUCH],
  ['cactus', {}, 15, 0, SOLID, DAMAGE_TOUCH],
  ['cobweb', {}, 0, 16, OPEN, HAZARD_AVOID],
  ['powder_snow', {}, 0, 16, OPEN, HAZARD_AVOID],
  ['short_grass', {}, 0, 16, OPEN, HAZARD_NONE],
  ['white_carpet', {}, 1, 0, SOLID, HAZARD_NONE],
  ['dirt_path', {}, 15, 0, SOLID, HAZARD_NONE],
  ['farmland', {}, 15, 0, SOLID, HAZARD_NONE],
  ['mud', {}, 14, 0, SOLID, HAZARD_NONE]
]

for (const [name, props, top, base, kind, hazard] of cases) {
  test(`state table: ${name} ${JSON.stringify(props)}`, () => {
    const id = stateId(name, props)
    assert.deepEqual({ top: table.top[id], base: table.base[id], kind: table.kind[id], hazard: table.hazard[id] }, { top, base, kind, hazard })
  })
}

// walking direction that climbs a bottom straight stairs block: 1 east, 2 west, 3 south, 4 north; 0 for anything else
const stairCases = [
  ['oak_stairs', { facing: 'east', half: 'bottom', shape: 'straight' }, 1],
  ['oak_stairs', { facing: 'west', half: 'bottom', shape: 'straight' }, 2],
  ['oak_stairs', { facing: 'south', half: 'bottom', shape: 'straight' }, 3],
  ['oak_stairs', { facing: 'north', half: 'bottom', shape: 'straight' }, 4],
  ['stone_brick_stairs', { facing: 'north', half: 'bottom', shape: 'straight' }, 4],
  ['oak_stairs', { facing: 'north', half: 'top', shape: 'straight' }, 0],
  ['oak_stairs', { facing: 'north', half: 'bottom', shape: 'inner_left' }, 0],
  ['oak_stairs', { facing: 'north', half: 'bottom', shape: 'outer_right' }, 0],
  ['oak_slab', { type: 'bottom' }, 0],
  ['stone', {}, 0]
]

for (const [name, props, expected] of stairCases) {
  test(`stairUp: ${name} ${JSON.stringify(props)}`, () => {
    assert.equal(table.stairUp[stateId(name, props)], expected)
  })
}

test('state table is sized to the highest state id', () => {
  const max = registry.blocksArray.reduce((m, b) => Math.max(m, b.maxStateId), 0)
  assert.equal(table.top.length, max + 1)
})

const boxesOf = (id) => Array.from({ length: table.boxCount[id] }, (_, i) => Array.from(table.boxes.subarray((table.boxStart[id] + i) * 6, (table.boxStart[id] + i) * 6 + 6)))

// [block, props, partial, offsetMax]
const partialCases = [
  ['stone', {}, 0, 0],
  ['air', {}, 0, 0],
  ['oak_slab', { type: 'bottom' }, 0, 0],
  ['oak_slab', { type: 'top' }, 0, 0],
  ['oak_stairs', { facing: 'north', half: 'bottom', shape: 'straight' }, 0, 0],
  ['white_carpet', {}, 0, 0],
  ['oak_trapdoor', { half: 'bottom', open: false }, 0, 0],
  ['oak_trapdoor', { half: 'bottom', open: true }, 1, 0],
  ['cocoa', { age: 2 }, 1, 0],
  ['bamboo', {}, 1, 0.25],
  ['pointed_dripstone', {}, 1, 0.125],
  ['glass_pane', {}, 1, 0],
  ['iron_bars', {}, 1, 0],
  ['oak_fence', {}, 1, 0],
  ['cobblestone_wall', {}, 1, 0],
  ['ladder', {}, 1, 0],
  ['oak_door', { open: false }, 1, 0],
  ['lantern', {}, 1, 0],
  ['iron_chain', {}, 1, 0],
  ['end_rod', {}, 1, 0],
  ['lightning_rod', {}, 1, 0],
  ['candle', {}, 1, 0],
  ['flower_pot', {}, 1, 0],
  ['skeleton_skull', {}, 1, 0],
  ['oak_fence_gate', { open: true }, 0, 0]
]

for (const [name, props, partial, offsetMax] of partialCases) {
  test(`partial and offsetMax: ${name} ${JSON.stringify(props)}`, () => {
    const id = stateId(name, props)
    assert.deepEqual({ partial: table.partial[id], offsetMax: table.offsetMax[id] }, { partial, offsetMax })
  })
}

// [block, props, boxes] block-local
const boxCases = [
  ['air', {}, []],
  ['stone', {}, [[0, 0, 0, 1, 1, 1]]],
  ['oak_slab', { type: 'bottom' }, [[0, 0, 0, 1, 0.5, 1]]],
  ['cocoa', { age: 2, facing: 'west' }, [[0.0625, 0.1875, 0.25, 0.5625, 0.75, 0.75]]],
  ['bamboo', {}, [[0.40625, 0, 0.40625, 0.59375, 1, 0.59375]]],
  ['oak_fence', { north: false, south: false, east: false, west: false }, [[0.375, 0, 0.375, 0.625, 1.5, 0.625]]]
]

for (const [name, props, boxes] of boxCases) {
  test(`boxes: ${name} ${JSON.stringify(props)}`, () => {
    assert.deepEqual(boxesOf(stateId(name, props)), boxes)
  })
}

// [block, props, climb, climbName, facing, floor]: climb 1 climbable, 2 open trapdoor (climbable over a ladder of its
// facing), 3 closed trapdoor a hand opens; climbName 1 ladder, 2 vines, 3 scaffolding; floor: what a body can stand on
const climbCases = [
  ['stone', {}, 0, 0, 0, 16],
  ['oak_slab', { type: 'bottom' }, 0, 0, 0, 8],
  ['ladder', { facing: 'west' }, 1, 1, 2, 0],
  ['vine', { east: true }, 1, 2, 0, 0],
  ['twisting_vines_plant', {}, 1, 2, 0, 0],
  ['weeping_vines', {}, 1, 2, 0, 0],
  ['cave_vines', {}, 1, 2, 0, 0],
  ['scaffolding', {}, 1, 3, 0, 16],
  ['oak_trapdoor', { half: 'top', open: true, facing: 'west' }, 2, 0, 2, 16],
  ['oak_trapdoor', { half: 'top', open: false, facing: 'east' }, 3, 0, 1, 16],
  ['iron_trapdoor', { half: 'top', open: false, facing: 'east' }, 0, 0, 1, 16],
  ['iron_trapdoor', { half: 'top', open: true, facing: 'east' }, 2, 0, 1, 16]
]

for (const [name, props, climb, climbName, facing, floor] of climbCases) {
  test(`climb data: ${name} ${JSON.stringify(props)}`, () => {
    const id = stateId(name, props)
    assert.deepEqual({ climb: table.climb[id], climbName: table.climbName[id], facing: table.facing[id], floor: table.floor[id] }, { climb, climbName, facing, floor })
  })
}

test('scaffolding has no collision for a body inside it, but is a floor to stand on', () => {
  const id = stateId('scaffolding', {})
  assert.deepEqual({ top: table.top[id], boxes: boxesOf(id), kind: table.kind[id] }, { top: 0, boxes: [], kind: CLIMB })
})
