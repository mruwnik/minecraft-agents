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

test('state table is sized to the highest state id', () => {
  const max = registry.blocksArray.reduce((m, b) => Math.max(m, b.maxStateId), 0)
  assert.equal(table.top.length, max + 1)
})
