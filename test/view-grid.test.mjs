// Filling the renderer's grid from loaded chunk columns, and turning a pose into the renderer's camera.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { buildGrid } from '../tools/view/grid.mjs'
import { cameraFromPose, entitiesFromPose } from '../tools/view/camera.mjs'
import { directionFor } from '../src/vision/renderer.mjs'

const Chunk = makeChunkClass('1.21.4')
const stone = Chunk.registry.blocksByName.stone.defaultState
const dirt = Chunk.registry.blocksByName.dirt.defaultState

const columns = new Map()
const put = (x, y, z, id) => {
  const key = `${x >> 4}.${z >> 4}`
  if (!columns.has(key)) columns.set(key, new Chunk({ minY: -64, worldHeight: 384 }))
  columns.get(key).setBlockStateId({ x: x & 15, y, z: z & 15 }, id)
}
put(5, 70, 5, stone)
put(-3, 64, 20, dirt) // another column, negative x
put(5, 200, 5, stone) // far above the box

const grid = buildGrid({ column: (cx, cz) => columns.get(`${cx}.${cz}`) ?? null, eye: { x: 4.5, y: 71.62, z: 4.5 }, across: 32, up: 16 })

const at = (x, y, z) => {
  const { origin, size, data } = grid
  return data[((y - origin.y) * size.z + (z - origin.z)) * size.x + (x - origin.x)]
}

test('a known block lands at its world cell', () => {
  assert.equal(at(5, 70, 5), stone)
  assert.equal(at(-3, 64, 20), dirt)
})

test('air and cells outside the vertical box stay empty', () => {
  assert.equal(at(6, 70, 5), 0)
  assert.equal(grid.origin.y + grid.size.y <= 200, true)
})

test('the grid is centred on the eye and records the highest block', () => {
  assert.deepEqual(grid.origin, { x: 4 - 32, y: 71 - 16, z: 4 - 32 })
  assert.deepEqual(grid.size, { x: 65, y: 33, z: 65 })
  assert.equal(grid.top, 70)
})

test('missing columns are skipped', () => {
  const empty = buildGrid({ column: () => null, eye: { x: 0.5, y: 10, z: 0.5 }, across: 8, up: 8 })
  assert.equal(empty.top, -Infinity)
})

for (const [name, yaw, pitch, expected] of [
  ['yaw 0 faces north (-z)', 0, 0, { x: 0, y: 0, z: -1 }],
  ['yaw pi/2 faces west (-x)', Math.PI / 2, 0, { x: -1, y: 0, z: 0 }],
  ['yaw pi faces south (+z)', Math.PI, 0, { x: 0, y: 0, z: 1 }],
  ['yaw 3pi/2 faces east (+x)', 3 * Math.PI / 2, 0, { x: 1, y: 0, z: 0 }],
  ['pitch pi/2 looks up', 0, Math.PI / 2, { x: 0, y: 1, z: 0 }]
]) {
  test(`camera: ${name}`, () => {
    const cam = cameraFromPose({ eye: { x: 1, y: 2, z: 3 }, yaw, pitch })
    assert.deepEqual(cam.eye, { x: 1, y: 2, z: 3 })
    const d = directionFor(cam.yaw, cam.pitch)
    for (const k of ['x', 'y', 'z']) assert.ok(Math.abs(d[k] - expected[k]) < 1e-9, `${k}: ${d[k]}`)
  })
}

test('pose entities become renderer entities with flat positions', () => {
  const [e] = entitiesFromPose({ entities: [{ id: 1, type: 'hostile', name: 'zombie', kind: 'Hostile mobs', username: undefined, pos: { x: 1, y: 2, z: 3 }, yaw: 0.5, height: 1.95, width: 0.6 }] })
  assert.deepEqual(e, { name: 'zombie', label: 'zombie', kind: 'hostile', x: 1, y: 2, z: 3, width: 0.6, height: 1.95, yaw: 0.5 })
})
