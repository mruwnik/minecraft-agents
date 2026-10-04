// Why JavaScript: tests movements.mjs, which stays JS: Mineflayer boundary; the cost policy has to run inside mineflayer-pathfinder's getNeighbors.
import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import vec3 from 'vec3'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import pf from 'mineflayer-pathfinder'
import { SafeMovements } from './movements.mjs'

const require = createRequire(import.meta.url)
const AStar = require('mineflayer-pathfinder/lib/astar')
const Move = require('mineflayer-pathfinder/lib/move')
const { Vec3 } = vec3
const { GoalBlock } = pf.goals

const registry = prismarineRegistry('26.1')
const Block = prismarineBlock(registry)
const BOUNDS = { x: [-2, 12], y: [50, 90], z: [-3, 6] }
const within = ([lo, hi], v) => v >= lo && v <= hi

const blockNamed = (name, pos) => {
  const block = Block.fromStateId(registry.blocksByName[name].defaultState, 0)
  block.position = pos
  return block
}

// cells: [x, y, z, name] entries; unspecified cells in bounds are air, outside is unloaded (null)
const fakeBot = cells => {
  const names = new Map(cells.map(([x, y, z, name]) => [`${x},${y},${z}`, name]))
  return {
    registry,
    blockAt: pos => {
      const at = new Vec3(Math.floor(pos.x), Math.floor(pos.y), Math.floor(pos.z))
      if (!within(BOUNDS.x, at.x) || !within(BOUNDS.y, at.y) || !within(BOUNDS.z, at.z)) return null
      return blockNamed(names.get(`${at.x},${at.y},${at.z}`) ?? 'air', at)
    },
    inventory: { items: () => [] },
    entities: {},
    entity: { effects: {}, position: new Vec3(0, 64, 0) },
    game: { minY: -64 },
    pathfinder: { bestHarvestTool: () => null }
  }
}

const range = (from, to) => Array.from({ length: to - from + 1 }, (_, i) => from + i)
const rect = (x0, x1, z0, z1, y, name) => range(x0, x1).flatMap(x => range(z0, z1).map(z => [x, y, z, name]))

const plan = (cells, from, to) => {
  const movements = new SafeMovements(fakeBot(cells))
  const goal = new GoalBlock(...to)
  return new AStar(new Move(...from, 0, 0), movements, goal, Infinity, Infinity, -1).compute()
}
const visits = (result, pred) => result.path.some(pred)
const jumps = (result, from) => {
  const nodes = [{ x: from[0], z: from[2] }, ...result.path]
  return nodes.slice(1).some((node, i) => Math.max(Math.abs(node.x - nodes[i].x), Math.abs(node.z - nodes[i].z)) > 1)
}

// body hazards sit at feet height, floor hazards replace the floor block
const HAZARDS = [
  ['powder_snow', 64], ['sweet_berry_bush', 64], ['wither_rose', 64], ['cobweb', 64],
  ['magma_block', 63], ['campfire', 63], ['soul_campfire', 63]
]
const START = [0, 64, 1]
const GOAL = [8, 64, 1]
const floor = (z1) => rect(-1, 9, 0, z1, 63, 'stone')

HAZARDS.forEach(([name, y]) => {
  test(`${name} in a lane with a detour is walked round`, () => {
    const lane = rect(4, 4, 0, 1, y, name)
    const result = plan([...floor(2), ...lane], START, GOAL)
    assert.equal(result.status, 'success')
    assert.ok(!visits(result, n => n.x === 4 && n.z <= 1 && (y === 64 ? n.y === 64 : true)))
  })

  test(`${name} filling the corridor is still crossed`, () => {
    const lane = rect(4, 4, 0, 2, y, name)
    const result = plan([...floor(2), ...lane], START, GOAL)
    assert.equal(result.status, 'success')
    assert.deepEqual(result.path.at(-1).toArray(), GOAL)
  })
})

test('a cobweb in a 1-wide corridor can be crossed', () => {
  const result = plan([...rect(-1, 9, 1, 1, 63, 'stone'), [4, 64, 1, 'cobweb']], START, GOAL)
  assert.equal(result.status, 'success')
})

test('soul_fire in a 1-wide corridor blocks the way like fire', () => {
  const result = plan([...rect(-1, 9, 1, 1, 63, 'stone'), [4, 64, 1, 'soul_fire']], START, GOAL)
  assert.equal(result.status, 'noPath')
})

// portals are never entered: no cost, a ban (the 1-wide corridor has no other way)
const PORTALS = ['nether_portal', 'end_portal', 'end_gateway']

PORTALS.forEach(name => {
  test(`${name} in a 1-wide corridor gives no path`, () => {
    assert.equal(plan([...rect(-1, 9, 1, 1, 63, 'stone'), [4, 64, 1, name]], START, GOAL).status, 'noPath')
  })

  test(`${name} with a way round is never entered`, () => {
    const result = plan([...floor(2), [4, 64, 1, name]], START, GOAL)
    assert.equal(result.status, 'success')
    assert.ok(!visits(result, n => n.x === 4 && n.y === 64 && n.z === 1))
  })
})

const lavaPool = rect(3, 5, 0, 2, 63, 'lava')
const poolFloor = z1 => rect(-1, 9, 0, z1, 63, 'stone')

test('a 3-wide lava pool with a way round is not jumped', () => {
  const from = [2, 64, 1]
  const result = plan([...poolFloor(3), ...lavaPool], from, [6, 64, 1])
  assert.equal(result.status, 'success')
  assert.ok(!jumps(result, from))
})

test('a 3-wide lava pool with no way round is never jumped', () => {
  const result = plan([...poolFloor(2), ...lavaPool], [2, 64, 1], [6, 64, 1])
  assert.notEqual(result.status, 'success')
})

test('a drop into water is bounded like any other drop', () => {
  assert.equal(new SafeMovements(fakeBot([])).infiniteLiquidDropdownDistance, false)
})

test('a 10-block drop into water is not taken', () => {
  const cells = [...rect(0, 1, 0, 0, 73, 'stone'), ...rect(2, 9, 0, 0, 59, 'stone'), ...rect(2, 9, 0, 0, 60, 'water'),
    ...rect(2, 9, 0, 0, 61, 'water'), ...rect(2, 9, 0, 0, 62, 'water'), ...rect(2, 9, 0, 0, 63, 'water')]
  assert.notEqual(plan(cells, [0, 74, 0], [5, 63, 0]).status, 'success')
})

test('a 3-block drop onto stone is taken', () => {
  const cells = [...rect(0, 1, 0, 0, 66, 'stone'), ...rect(2, 9, 0, 0, 63, 'stone')]
  assert.equal(plan(cells, [0, 67, 0], [5, 64, 0]).status, 'success')
})

// farmland turns to dirt when landed on from over half a block up: parkour and drops never land on it
const farmlane = rect(0, 8, 1, 1, 63, 'farmland').filter(([x]) => x !== 4)
const waterCell = [[4, 63, 1, 'water'], [4, 62, 1, 'stone']]

test('a farmland lane cut by water is crossed without a parkour landing', () => {
  const from = [0, 64, 1]
  const result = plan([...farmlane, ...waterCell], from, [8, 64, 1])
  assert.equal(result.status, 'success')
  assert.deepEqual(result.path.at(-1).toArray(), [8, 64, 1])
  assert.ok(!jumps(result, from))
})

test('a drop onto farmland is not taken when a level way round exists', () => {
  const cells = [...rect(0, 2, 0, 1, 64, 'stone'), ...rect(3, 9, 0, 0, 63, 'stone'), ...rect(3, 9, 1, 1, 63, 'farmland')]
  const result = plan(cells, [0, 65, 1], [8, 64, 1])
  assert.equal(result.status, 'success')
  assert.ok(!visits(result, n => n.x === 3 && n.y === 64 && n.z === 1))
})

test('a jump up onto farmland stays allowed', () => {
  const cells = [...rect(0, 1, 1, 1, 63, 'stone'), ...rect(2, 3, 1, 1, 64, 'farmland')]
  const result = plan(cells, [0, 64, 1], [3, 65, 1])
  assert.equal(result.status, 'success')
  assert.deepEqual(result.path.at(-1).toArray(), [3, 65, 1])
})

test('parkour over a 1-block gap onto stone is unchanged', () => {
  const from = [0, 64, 1]
  const result = plan(rect(0, 8, 1, 1, 63, 'stone').filter(([x]) => x !== 4), from, [8, 64, 1])
  assert.equal(result.status, 'success')
  assert.ok(jumps(result, from))
})
