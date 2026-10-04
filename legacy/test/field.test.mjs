// Where to stand at a field and in what order to put its seeds back (card f30fd998). farm.harvest place= walked to the
// plan's geometric centre: on a dense field that cell has no node a walk can end in (a walk steps round crops), so the
// search timed out. And a replant in cut order walled the body in: the last cells of a row had no cell within reach left
// to stand in once their neighbours were planted, and 14 of 108 came back notReplanted.
import test from 'node:test'
import assert from 'node:assert/strict'
import { cellOf, fieldEdge, plantOrder, fieldCrops, notReplantedLine, standingLine, bareReason } from '../src/farm/field.mjs'
import { WORK_RANGE } from '../src/navigation/walk.mjs'

// what api.block answers, read as the cell walk.mjs judges
for (const [name, block, expected] of [
  ['a chunk that is not loaded', null, null],
  ['farmland: a block, nothing else', { name: 'farmland', properties: {}, solid: true }, { name: 'farmland', solid: true, liquid: false, crop: false }],
  ['wheat: not solid, but a walk steps round it', { name: 'wheat', properties: { age: 7 }, solid: false }, { name: 'wheat', solid: false, liquid: false, crop: true }],
  ['water: something to swim in', { name: 'water', properties: {}, solid: false }, { name: 'water', solid: false, liquid: true, crop: false }],
  ['air', { name: 'air', properties: {}, solid: false }, { name: 'air', solid: false, liquid: false, crop: false }]
]) {
  test(`cellOf: ${name}`, () => assert.deepEqual(cellOf(block), expected))
}

// a world as a map of 'x,y,z' -> cell; anything unnamed is air over dirt (dirt at and below the ground)
const AIR = { name: 'air', solid: false }
const DIRT = { name: 'dirt', solid: true }
const WHEAT = { name: 'wheat', solid: false, crop: true }
const FENCE = { name: 'oak_fence', solid: true }
const world = (cells, groundY = 62) => (x, y, z) => cells[`${x},${y},${z}`] ?? (y <= groundY ? DIRT : AIR)
const span = n => Array.from({ length: 2 * n + 1 }, (_, i) => i - n)
// plan cells (ground y=62) of a square field round (5,-80), every cell planted with wheat at y=63
const field = n => span(n).flatMap(dx => span(n).map(dz => ({ x: 5 + dx, y: 62, z: -80 + dz, ch: 'w' })))
const planted = n => Object.assign({}, ...field(n).map(c => ({ [`${c.x},63,${c.z}`]: WHEAT })))
// a fence ring at y=63 just outside the field
const fenced = n => Object.assign({}, planted(n), ...span(n + 1).flatMap(d => [
  { [`${5 - n - 1},63,${-80 + d}`]: FENCE }, { [`${5 + n + 1},63,${-80 + d}`]: FENCE },
  { [`${5 + d},63,${-80 - n - 1}`]: FENCE }, { [`${5 + d},63,${-80 + n + 1}`]: FENCE }
]))
// the same field with a lane (a covered channel: a slab in the ground, nothing over it) along z=-78
const laned = n => ({ ...planted(n), ...Object.assign({}, ...span(n).map(dx => ({ [`${5 + dx},63,-78`]: AIR }))) })

for (const [name, cells, at, from, expected] of [
  ['from the east: the nearest cell east of the rows that still reaches the last row (four across)',
    field(2), world(planted(2)), { x: 20, y: 63, z: -80 }, { x: 11, y: 63, z: -80, span: 11 }],
  ['from the west, a row off: the nearest such cell on that side', field(2), world(planted(2)), { x: -10, y: 63.5, z: -79.2 }, { x: -1, y: 63, z: -79, span: 11 }],
  ['a lane through the field is nearer than its rim', field(4), world(laned(4)), { x: 5, y: 63, z: -78 }, { x: 5, y: 63, z: -78, span: 10 }],
  ['a fence ring: the rim cell is outside the fence, still within reach of the outer rows',
    field(2), world(fenced(2)), { x: 20, y: 63, z: -80 }, { x: 11, y: 63, z: -80, span: 11 }],
  ['from above: the feet cell is on the ground, not in the air', field(2), world(planted(2)), { x: 20, y: 70, z: -80 }, { x: 11, y: 63, z: -80, span: 11 }],
  ['nothing loaded: no cell to choose', field(2), () => null, { x: 20, y: 63, z: -80 }, null],
  ['no cells in the plan: nothing to choose', [], world({}), { x: 20, y: 63, z: -80 }, null]
]) {
  test(`fieldEdge: ${name}`, () => assert.deepEqual(fieldEdge(at, cells, from), expected))
}

test('fieldEdge: every candidate is within work range of some cell of the plan', () => {
  const edge = fieldEdge(world(planted(3)), field(3), { x: 5, y: 63, z: -120 })
  assert.ok(field(3).some(c => Math.hypot(c.x - edge.x, c.y + 1 - edge.y, c.z - edge.z) <= WORK_RANGE))
  assert.equal(edge.z, -80 - 3 - 4)
})

// the seeds go back far end first: the cells still bare are the ones nearer the standing cell, so there is always one to
// stand in within reach of the next, and the last one planted is the one beside where the body ends up
const cut = [[0, 0], [1, 0], [2, 0], [0, 1], [1, 1], [2, 1], [0, 2], [1, 2], [2, 2]].map(([x, z]) => ({ x, y: 63, z, seed: 'wheat_seeds' }))
for (const [name, stand, expected] of [
  ['standing west of the rows: the east column first, each column north to south',
    { x: -1, y: 63, z: 1 }, ['2,0', '2,2', '2,1', '1,0', '1,2', '1,1', '0,0', '0,2', '0,1']],
  ['standing at the north-west corner: the far corner first, the near corner last',
    { x: -1, y: 63, z: -1 }, ['2,2', '2,1', '1,2', '2,0', '0,2', '1,1', '1,0', '0,1', '0,0']],
  ['standing south of the rows: the north row first', { x: 1, y: 63, z: 3 }, ['0,0', '2,0', '1,0', '0,1', '2,1', '1,1', '0,2', '2,2', '1,2']]
]) {
  test(`plantOrder: ${name}`, () => assert.deepEqual(plantOrder(cut, stand).map(c => `${c.x},${c.z}`), expected))
}
test('plantOrder: keeps every field of a cell', () => assert.deepEqual(plantOrder(cut, { x: -1, y: 63, z: 1 })[0], { x: 2, y: 63, z: 0, seed: 'wheat_seeds' }))
test('plantOrder: with nowhere to stand from, the cut order is kept', () => assert.deepEqual(plantOrder(cut, null), cut))

// only what stands over a cell of the plan is the field's: a neighbour's rows a few blocks off are theirs
test('fieldCrops: keeps the crops over the plan and drops the rest', () => {
  const found = [{ x: 5, y: 63, z: -80 }, { x: 7, y: 63, z: -78 }, { x: 8, y: 63, z: -80 }, { x: 5, y: 63, z: -83 }]
  assert.deepEqual(fieldCrops(field(2), found), [{ x: 5, y: 63, z: -80 }, { x: 7, y: 63, z: -78 }])
})
test('fieldCrops: with no plan, everything found is kept', () => {
  const found = [{ x: 5, y: 63, z: -80 }, { x: 8, y: 63, z: -80 }]
  assert.deepEqual(fieldCrops(null, found), found)
})

// which cells stayed bare and why, in one line the driver can act on
const bare = (n, why, z = -80) => Array.from({ length: n }, (_, i) => ({ x: i, y: 63, z, why }))
for (const [name, failed, expected] of [
  ['nothing bare', [], null],
  ['one reason, a few cells', bare(2, 'no standing cell within reach'), '2: no standing cell within reach at 0,-80 1,-80'],
  ['one reason, many cells: the first three and a count', bare(14, 'no standing cell within reach'), '14: no standing cell within reach at 0,-80 1,-80 2,-80 +11 more'],
  ['two reasons', [...bare(1, 'no wheat_seeds left in my pockets'), ...bare(2, 'no standing cell within reach', -79)],
    '3: no wheat_seeds left in my pockets at 0,-80; no standing cell within reach at 0,-79 1,-79']
]) {
  test(`notReplantedLine: ${name}`, () => assert.equal(notReplantedLine(failed), expected))
}

test('standingLine: names the cell and that it is the edge', () => assert.equal(standingLine({ x: 11, y: 63, z: -80 }), "standing at 11,63,-80, plan's edge"))

// why a cut cell stayed bare: the walled-in case is named, anything else is the place itself
for (const [name, at, cell, expected] of [
  ['deep in a planted field', world(planted(4)), { x: 5, y: 63, z: -80 }, 'no standing cell within reach'],
  ['the bare cell itself is the only one in reach: a body standing in it cannot plant it', world({ ...planted(4), '5,63,-80': AIR }), { x: 5, y: 63, z: -80 }, 'no standing cell within reach'],
  ['open ground beside it', world({}), { x: 5, y: 63, z: -80 }, 'the place failed twice (out of reach, or I stood in it)']
]) {
  test(`bareReason: ${name}`, () => assert.equal(bareReason(at, cell), expected))
}
