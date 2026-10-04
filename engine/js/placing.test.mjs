import { test } from 'node:test'
import assert from 'node:assert/strict'
import { placedBlocks } from './placing.mjs'

const at = (x, y, z) => ({ x, y, z })
const PI = Math.PI
const YAW = { north: 0, west: PI / 2, south: PI, east: 1.5 * PI }
const UP = [0, 1, 0]
const DOWN = [0, -1, 0]
const pos = at(0, 64, 0)
const world = cells => p => cells[`${p.x},${p.y},${p.z}`] ?? 'air'
const floor = { '0,63,0': 'stone' }
const top = { x: 0.5, y: 1, z: 0.5 }

const place = (item, { face = UP, cursor = top, yaw = 0, pitch = 0, cells = floor } = {}) =>
  placedBlocks({ item, pos, face, cursor, yaw, pitch, blockAt: world(cells) })
const props = r => r.blocks[0].properties

const cases = [
  ['stairs face the look, the top face gives the bottom half', 'oak_stairs', { yaw: YAW.east }, { facing: 'east', half: 'bottom' }],
  ['stairs clicked on the underside are top', 'oak_stairs', { face: DOWN, yaw: YAW.west }, { facing: 'west', half: 'top' }],
  ['stairs on a side above the middle are top', 'oak_stairs', { face: [-1, 0, 0], cursor: { x: 0, y: 0.75, z: 0.5 }, yaw: YAW.north }, { facing: 'north', half: 'top' }],
  ['stairs on a side below the middle are bottom', 'oak_stairs', { face: [-1, 0, 0], cursor: { x: 0, y: 0.25, z: 0.5 }, yaw: YAW.south }, { facing: 'south', half: 'bottom' }],
  ['a slab on a side above the middle is top', 'oak_slab', { face: [1, 0, 0], cursor: { x: 1, y: 0.75, z: 0.5 } }, { type: 'top' }],
  ['a slab on the top face is bottom whatever the cursor', 'oak_slab', { cursor: { x: 0.5, y: 1, z: 0.5 } }, { type: 'bottom' }],
  ['a log takes the axis of the face, x', 'oak_log', { face: [1, 0, 0] }, { axis: 'x' }],
  ['a log takes the axis of the face, z', 'oak_log', { face: [0, 0, -1] }, { axis: 'z' }],
  ['a log takes the axis of the face, y', 'oak_log', {}, { axis: 'y' }],
  ['a gate faces the look', 'oak_fence_gate', { yaw: YAW.east }, { facing: 'east', open: false }],
  ['a chest faces the placer', 'chest', { yaw: YAW.north }, { facing: 'south', type: 'single' }],
  ['a furnace faces the placer', 'furnace', { yaw: YAW.west }, { facing: 'east' }],
  ['a trapdoor on a side faces that side, half by cursor', 'oak_trapdoor', { face: [-1, 0, 0], cursor: { x: 0, y: 0.75, z: 0.5 }, yaw: YAW.north }, { facing: 'west', half: 'top' }],
  ['a trapdoor on a top face faces the placer, bottom', 'oak_trapdoor', { yaw: YAW.east }, { facing: 'west', half: 'bottom' }],
  ['a trapdoor under a block faces the placer, top', 'oak_trapdoor', { face: DOWN, yaw: YAW.south }, { facing: 'north', half: 'top' }]
]

for (const [name, item, click, want] of cases) {
  test(`placing: ${name}`, () => {
    const got = props(place(item, click))
    for (const [k, v] of Object.entries(want)) assert.equal(got[k], v, k)
  })
}

test('placing: a slab or stair into water is waterlogged', () => {
  assert.equal(props(place('oak_slab', { cells: { ...floor, '0,64,0': 'water' } })).waterlogged, true)
  assert.equal(props(place('oak_slab')).waterlogged, false)
})

test('placing: a door is two blocks, the upper one above; no room or no floor places nothing', () => {
  const r = place('oak_door', { yaw: YAW.north })
  assert.deepEqual(r.blocks.map(b => [b.pos.y, b.properties.half, b.properties.facing]), [[64, 'lower', 'north'], [65, 'upper', 'north']])
  assert.ok(place('oak_door', { cells: { ...floor, '0,65,0': 'stone' } }).refused)
  assert.ok(place('oak_door', { face: [1, 0, 0], cells: { '-1,64,0': 'stone' } }).refused)
})

test('placing: a bed is the foot where clicked and the head along the look', () => {
  const r = place('white_bed', { yaw: YAW.south })
  assert.deepEqual(r.blocks.map(b => [b.pos, b.properties.part, b.properties.facing]), [[pos, 'foot', 'south'], [at(0, 64, 1), 'head', 'south']])
  assert.ok(place('white_bed', { yaw: YAW.south, cells: { ...floor, '0,64,1': 'stone' } }).refused)
})

test('placing: a torch looking down is a floor torch; looking level at a wall it hangs on the wall', () => {
  const cells = { ...floor, '1,64,0': 'stone' }
  assert.deepEqual(place('torch', { pitch: -PI / 2, cells }).blocks[0], { pos, name: 'torch', properties: {} })
  assert.deepEqual(place('torch', { yaw: YAW.east, pitch: -0.3, cells }).blocks[0], { pos, name: 'wall_torch', properties: { facing: 'west' } })
  assert.deepEqual(place('soul_torch', { face: [-1, 0, 0], yaw: YAW.east, cells: { '1,64,0': 'stone' } }).blocks[0].name, 'soul_wall_torch')
  assert.ok(place('torch', { yaw: YAW.east, cells: {} }).refused)
})

test('placing: a ladder faces away from the wall it is looked into', () => {
  assert.deepEqual(props(place('ladder', { face: [-1, 0, 0], yaw: YAW.east, cells: { '1,64,0': 'stone' } })), { facing: 'west', waterlogged: false })
  assert.ok(place('ladder', { yaw: YAW.east, cells: floor }).refused)
})

test('placing: any other block has no state', () => {
  assert.deepEqual(place('oak_fence', { yaw: YAW.east }), { blocks: [{ pos, name: 'oak_fence', properties: {} }] })
})
