// Where a walk can end. farm.build on jizo-melon-patch crept at 5 s a job (reported 16:52Z): every till and plant deep in the
// wheat asked the pathfinder for a cell within 3 of a farmland block whose every neighbour was planted, and a walk steps
// round crops, so no node could ever satisfy the goal. A* then searched the whole 160-block radius (15-19k nodes) and gave
// up at mineflayer-pathfinder's 5 s thinkTimeout, for every cell in turn. The goal is judged BEFORE the search: no cell to
// stand on within range is a refusal in a millisecond, with the reason, not a five-second timeout.
import test from 'node:test'
import assert from 'node:assert/strict'
import { cellsWithin, standable, noStanding, WORK_RANGE } from '../src/walk.mjs'

// the pathfinder's GoalNear counts integer node distance: dx²+dy²+dz² <= range²
for (const [range, count] of [[0, 1], [1, 7], [1.5, 19], [2, 33], [3, 123]]) {
  test(`cellsWithin: range ${range} holds ${count} cells`, () => assert.equal(cellsWithin({ x: 5, y: 62, z: -83 }, range).length, count))
}
test('cellsWithin: floors a fractional target, the way GoalNear does', () => {
  assert.deepEqual(cellsWithin({ x: 5.7, y: 62.5, z: -83.2 }, 0), [{ x: 5, y: 62, z: -84 }])
})

const AIR = { name: 'air', solid: false }
const DIRT = { name: 'dirt', solid: true }
const STONE = { name: 'stone', solid: true }
const WATER = { name: 'water', solid: false, liquid: true }
const WHEAT = { name: 'wheat', solid: false, crop: true }
const LAVA = { name: 'lava', solid: false, liquid: true }
// a column read bottom to top: [floor, feet, head]
const column = (floor, feet, head) => (x, y, z) => ({ '-1': floor, 0: feet, 1: head })[y]
for (const [name, cells, expected] of [
  ['air over dirt: yes', column(DIRT, AIR, AIR), true],
  ['a crop in the feet cell: no (a walk steps round crops)', column(DIRT, WHEAT, AIR), false],
  ['a block in the feet cell: no', column(DIRT, DIRT, AIR), false],
  ['head in a block: no', column(DIRT, AIR, DIRT), false],
  ['nothing under the feet: no', column(AIR, AIR, AIR), false],
  ['water in the feet cell: yes, the body swims', column(AIR, WATER, AIR), true],
  ['water over the head too: yes, the pathfinder walks the pond floor', column(DIRT, WATER, WATER), true],
  ['lava: never', column(DIRT, LAVA, AIR), false],
  ['a chunk that is not loaded: no', () => null, false]
]) {
  test(`standable: ${name}`, () => assert.equal(standable(cells, { x: 0, y: 0, z: 0 }), expected))
}

// a world as a map of 'x,y,z' -> cell; anything unnamed is air over dirt (dirt at and below groundY)
const world = (cells, groundY = 61) => (x, y, z) => cells[`${x},${y},${z}`] ?? (y <= groundY ? DIRT : AIR)
const farmland = (x, z, planted) => ({ [`${x},62,${z}`]: { name: 'farmland', solid: true }, ...(planted ? { [`${x},63,${z}`]: WHEAT } : {}) })
const span = n => Array.from({ length: 2 * n + 1 }, (_, i) => i - n)
// jizo-melon-patch: farmland at y=62 planted with wheat in every cell for 8 blocks around (5,-80)
const planted = world(Object.assign({}, ...span(8).flatMap(dx => span(8).map(dz => farmland(5 + dx, -80 + dz, true)))), 62)
// the same field with a covered channel (a bottom slab in the water) two rows south of (5,-80): a walkable lane
const laned = (x, y, z) => (z === -78 && y === 62) ? { name: 'oak_slab', solid: true } : (z === -78 && y === 63) ? AIR : planted(x, y, z)
const CROPS = 'a walk steps round crops, so leave a . path or a covered channel through the field, or work the rows from one'
for (const [name, at, target, range, expected] of [
  ['open ground beside the cell: nothing to say', world({}), { x: 5, y: 62, z: -80 }, 1, null],
  ['range 0 at a ground block: nothing can stand inside it', world(farmland(5, -80, false)), { x: 5, y: 62, z: -80 }, 0,
    'nowhere to stand within 0 of 5,62,-80: it is farmland, a block; aim at the cell above it (y=63) or pass range=1'],
  ['a farmland block with air over it: the cell above it will do', world(farmland(5, -80, false)), { x: 5, y: 62, z: -80 }, 1, null],
  ['deep in a planted field: every cell within 3 is wheat', planted, { x: 5, y: 62, z: -80 }, 3,
    `nowhere to stand within 3 of 5,62,-80: every cell in reach is planted (wheat:25); ${CROPS}`],
  ['the same cell from the lane two rows off, within work range', laned, { x: 5, y: 62, z: -80 }, WORK_RANGE, null],
  ['a lane four rows off is still within work range', laned, { x: 5, y: 62, z: -82 }, WORK_RANGE, null],
  ['a lane five rows off is out of reach', laned, { x: 5, y: 62, z: -83 }, WORK_RANGE,
    `nowhere to stand within 4.2 of 5,62,-83: every cell in reach is planted (wheat:49); ${CROPS}`],
  ['a cell in water with the pond floor under it: swim there', world({ '5,62,-80': WATER }), { x: 5, y: 62, z: -80 }, 0, null],
  ['under a roof: the head cell is what blocks', world({ ...farmland(5, -80, false), '5,64,-80': STONE }, 62), { x: 5, y: 62, z: -80 }, 1,
    'nowhere to stand within 1 of 5,62,-80: every cell in reach is blocked (stone:1)'],
  ['a cell in mid-air', world({}), { x: 5, y: 70, z: -80 }, 0, 'nowhere to stand within 0 of 5,70,-80: nothing to stand on there (mid-air, or inside a block)'],
  ['a cell whose chunks are not loaded', () => null, { x: 500, y: 62, z: 500 }, 2,
    'nowhere to stand within 2 of 500,62,500: that part of the world is not loaded here (unloaded:33): walk nearer first']
]) {
  test(`noStanding: ${name}`, () => assert.equal(noStanding(at, target, range), expected))
}

// the arm reaches 4.5 from the eyes; a node one up and four across from a ground cell is 4.12 away, five across is 5.1
test('WORK_RANGE: four rows of crops from a lane can be worked, five cannot', () => {
  assert.ok(Math.hypot(4, 1) <= WORK_RANGE)
  assert.ok(Math.hypot(5, 1) > WORK_RANGE)
})

// how long a search may think. mineflayer-pathfinder gives every search 5 s; a goal a few blocks off that has not been found
// after 1.5 s (5k nodes) is walled in (crops, a fence, a pocket under the field the pre-check cannot see), and the job
// should hear so then. A far goal keeps the full 5 s: a 140-block walk over hills needs it
import { thinkBudget, goalDistance, THINK_CAP_MS } from '../src/walk.mjs'
for (const [distance, ms] of [[0, 1500], [3, 1575], [20, 2000], [100, 4000], [140, 5000], [400, 5000], [null, 5000]]) {
  test(`thinkBudget: ${distance} blocks off thinks ${ms} ms`, () => assert.equal(thinkBudget(distance), ms))
}
test('THINK_CAP_MS is the plugin default', () => assert.equal(THINK_CAP_MS, 5000))
for (const [name, goal, expected] of [
  ['GoalNear and GoalBlock carry x y z', { x: 3, y: 64, z: 4 }, 5],
  ['GoalNearXZ has no y: distance across', { x: 3, z: 4 }, 5],
  ['a goal with no coordinates (following an entity): unknown', { entity: {} }, null]
]) {
  test(`goalDistance: ${name}`, () => assert.equal(goalDistance(goal, { x: 0, y: 64, z: 0 }), expected))
}
