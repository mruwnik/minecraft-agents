// Where a walk can end. farm.build on jizo-melon-patch crept at 5 s a job (reported 16:52Z): every till and plant deep in the
// wheat asked the pathfinder for a cell within 3 of a farmland block whose every neighbour was planted, and a walk steps
// round crops, so no node could ever satisfy the goal. A* then searched the whole 160-block radius (15-19k nodes) and gave
// up at mineflayer-pathfinder's 5 s thinkTimeout, for every cell in turn. The goal is judged BEFORE the search: no cell to
// stand on within range is a refusal in a millisecond, with the reason, not a five-second timeout.
import test from 'node:test'
import assert from 'node:assert/strict'
import { cellsWithin, standable, noStanding, walkRefusal, WORK_RANGE } from '../src/navigation/walk.mjs'

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
const CROPS = 'work the rows from a . path or a covered channel (a walk crosses crops only where it must, at a walking pace)'
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

// a dig walk makes its own room: ore three under the dirt has no air cell within 3, and mine.get's dig walk was refused
// there as "mid-air, or inside a block" every time (Hollis, 09-26: iron_ore at 27,59,-35 under plain dirt)
const BURIED = 'nowhere to stand within 3 of 5,57,-80: nothing to stand on there (mid-air, or inside a block)'
for (const [name, dig, expected] of [
  ['a walk that may not dig is refused at a buried cell', false, BURIED],
  ['a dig walk to a buried cell digs its way there', true, null]
]) {
  test(`walkRefusal: ${name}`, () => assert.equal(walkRefusal(world({}), { x: 5, y: 57, z: -80, range: 3 }, { dig }), expected))
}

// the arm reaches 4.5 from the eyes; a node one up and four across from a ground cell is 4.12 away, five across is 5.1
test('WORK_RANGE: four rows of crops from a lane can be worked, five cannot', () => {
  assert.ok(Math.hypot(4, 1) <= WORK_RANGE)
  assert.ok(Math.hypot(5, 1) > WORK_RANGE)
})

// how long a search may think. mineflayer-pathfinder gives every search 5 s; a goal a few blocks off that has not been found
// after 1.5 s (5k nodes) is walled in (crops, a fence, a pocket under the field the pre-check cannot see), and the job
// should hear so then. A far goal keeps the full 5 s: a 140-block walk over hills needs it
import { thinkBudget, goalDistance, THINK_CAP_MS } from '../src/navigation/walk.mjs'
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

// A goal on the floor of a pit, or in mid-air over its mouth, is walked to the pit's rim. Eight dig walks aimed beside the
// 1-wide, 3-deep test pit by spawn jumped in (card 3fe30fb4): the pathfinder's nearest node it could stand on was the pit
// floor. The chooser judges the cell the walk would land on: walled in by rises of two or more on every side, it is a
// pit, and the nearest rim cell (ties: nearest the body) is the goal instead, with a note. A goal MEANT for the pit floor
// (into=true, or the body already down there) is left alone, as is a hole one deep (a step), open ground, or a trench
// longer than the look round (LOOK), which the pathfinder is trusted with
import { rimGoal } from '../src/navigation/walk.mjs'
// the site by spawn: dirt at and below y=62, surface feet at 63; a pit dug `depth` deep in the column x,z
const hole = (x, z, depth) => Object.fromEntries(Array.from({ length: depth }, (_, i) => [`${x},${62 - i},${z}`, AIR]))
const site = (...holes) => world(Object.assign({}, ...holes), 62)
const pit = site(hole(-11, -1, 3))
// the hole at -11,-1 dug `depth` deep with farmland (planted, unless told bare) in the eight cells round it
const ringed = (depth, planted = true) => Object.assign(hole(-11, -1, depth), ...[-1, 0, 1].flatMap(dx => [-1, 0, 1].map(dz => dx || dz ? farmland(-11 + dx, -1 + dz, planted) : {})))
// a tree's canopy a few blocks off: leaves at y=67 beside the hole, the kind of block the old rim search climbed to
const canopy = { '-11,67,-2': { name: 'oak_leaves', solid: true } }
const trench = site(...[-13, -12, -11, -10, -9].map(x => hole(x, -1, 3)))
const longTrench = site(...span(10).map(dx => hole(-11 + dx, -1, 3)))
const RIM = (x, y, z) => ({ x, y, z, range: 0, note: `the goal is the floor of a pit: standing at the rim ${x},${y},${z} instead` })
const outside = { x: -5, y: 63, z: 0 }
const inside = { x: -11, y: 60, z: -1 }
for (const [name, at, goal, range, options, expected] of [
  ['the pit floor itself: the rim nearest the body', pit, { x: -11, y: 60, z: -1 }, 0, { from: outside }, RIM(-10, 63, -1)],
  ['mid-air over the mouth (the walks that jumped in): a fall from there lands on the floor', pit, { x: -11, y: 63, z: -1 }, 0, { from: outside }, RIM(-10, 63, -1)],
  ['one over the floor with range 1: the range rounds to the floor', pit, { x: -11, y: 61, z: -1 }, 1, { from: outside }, RIM(-10, 63, -1)],
  ['a fractional goal over the mouth', pit, { x: -10.6, y: 63.4, z: -0.7 }, 0, { from: outside }, RIM(-10, 63, -1)],
  ['the body on the far side: the rim on its side', pit, { x: -11, y: 60, z: -1 }, 0, { from: { x: -16, y: 63, z: -1 } }, RIM(-12, 63, -1)],
  ['the surface beside the pit: nothing to change', pit, { x: -10, y: 63, z: -1 }, 0, { from: outside }, null],
  ['the surface beside the pit with a range that reaches down: the goal cell stands', pit, { x: -10, y: 63, z: -1 }, 4, { from: outside }, null],
  ['into=true: the floor is meant', pit, { x: -11, y: 60, z: -1 }, 0, { into: true, from: outside }, null],
  ['the body already in the pit: the floor is meant', pit, { x: -11, y: 60, z: -1 }, 0, { from: inside }, null],
  ['the body in the pit, aiming at the rim: climbing out is the walk\'s business', pit, { x: -10, y: 63, z: -1 }, 0, { from: inside }, null],
  ['a hole one deep is a step, not a pit', site(hole(-11, -1, 1)), { x: -11, y: 62, z: -1 }, 0, { from: outside }, null],
  ['a hole two deep is a pit', site(hole(-11, -1, 2)), { x: -11, y: 61, z: -1 }, 0, { from: outside }, RIM(-10, 63, -1)],
  // Jizo, jizo-melon-patch, 09-26: a one-deep hole in the beds, wheat all round it, and a tree's canopy a few blocks off.
  // A crop cell is no floor cell, so the hole read as a one-cell pit, and the rim search went up the columns beside it
  // until it found the canopy: "standing at the rim 4,68,-87 instead", two walks timed out on the detour. A side one
  // high is a step out, crop or not (the walk prices it as a crop step), and a rim is the top of a wall, never a canopy
  ['a one-deep hole ringed by wheat is a step out through the crops, not a pit', site(ringed(1)), { x: -11, y: 62, z: -1 }, 0, { from: outside }, null],
  ['the same, aimed one above it (a walk to the bed), with a canopy beside: no rim up there', site(ringed(1), canopy), { x: -11, y: 63, z: -1 }, 0, { from: outside }, null],
  ['the same hole two deep, bare farmland round it: a pit, its rim the farmland', site(ringed(2, false)), { x: -11, y: 61, z: -1 }, 0, { from: outside }, RIM(-10, 63, -1)],
  ['two deep and ringed by wheat: a pit with no rim to stand on, left to the pathfinder', site(ringed(2), canopy), { x: -11, y: 61, z: -1 }, 0, { from: outside }, null],
  ['a wall top that cannot be stood on is no rim: the next side is', site(hole(-11, -1, 3), { '-10,63,-1': STONE, '-10,64,-1': STONE }), { x: -11, y: 60, z: -1 }, 0, { from: { x: -5, y: 63, z: -1 } }, RIM(-11, 63, 0)],
  ['a trench: the rim beside the goal, on the body\'s side', trench, { x: -11, y: 60, z: -1 }, 0, { from: outside }, RIM(-11, 63, 0)],
  ['a trench, aimed at its end: the end rim is as near as the sides, and nearer the body', trench, { x: -9, y: 60, z: -1 }, 0, { from: { x: -5, y: 63, z: -1 } }, RIM(-8, 63, -1)],
  ['a trench longer than the look round is left to the pathfinder', longTrench, { x: -11, y: 60, z: -1 }, 0, { from: outside }, null],
  ['open ground', site(), { x: -11, y: 63, z: -1 }, 0, { from: outside }, null],
  ['a goal inside the ground: no floor to land on (noStanding says so)', pit, { x: -10, y: 62, z: -1 }, 0, { from: outside }, null],
  ['a goal high over open ground: it lands on the surface', site(), { x: -11, y: 70, z: -1 }, 0, { from: outside }, null],
  ['a goal high over the pit: it falls to the floor', pit, { x: -11, y: 66, z: -1 }, 0, { from: outside }, RIM(-10, 63, -1)],
  ['a pit with a lid on: still a pit, the rim beside the lid', site({ ...hole(-11, -1, 3), '-11,63,-1': STONE }), { x: -11, y: 60, z: -1 }, 0, { from: outside }, RIM(-10, 63, -1)],
  ['a stone room with no way up: no rim to stand on', world({ '0,10,0': AIR, '0,11,0': AIR }, 30), { x: 0, y: 10, z: 0 }, 0, { from: outside }, null],
  ['a pit whose chunk is not loaded', () => null, { x: -11, y: 60, z: -1 }, 0, { from: outside }, null]
]) {
  test(`rimGoal: ${name}`, () => assert.deepEqual(rimGoal(at, goal, range, options), expected))
}
