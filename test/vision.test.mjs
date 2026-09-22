import test from 'node:test'
import assert from 'node:assert/strict'
import zlib from 'node:zlib'
import { encodePng, decodePng, textureCandidates, castRay, makeGrid, render, directionFor } from '../src/vision.mjs'

// ---------------------------------------------------------------- png
test('png: encode then decode round-trips rgba pixels', () => {
  const rgba = Uint8Array.from([255, 0, 0, 255, 0, 255, 0, 128, 0, 0, 255, 0, 9, 8, 7, 6, 1, 2, 3, 4, 5, 6, 7, 8])
  const out = decodePng(encodePng(3, 2, rgba))
  assert.deepEqual([out.width, out.height, [...out.rgba]], [3, 2, [...rgba]])
})

const crcTable = Array.from({ length: 256 }, (_, n) => { let c = n; for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1; return c >>> 0 })
const crc = buf => (buf.reduce((c, b) => crcTable[(c ^ b) & 0xff] ^ (c >>> 8), 0xffffffff) ^ 0xffffffff) >>> 0
const chunk = (type, data) => {
  const body = Buffer.concat([Buffer.from(type), Buffer.from(data)])
  const out = Buffer.alloc(body.length + 8)
  out.writeUInt32BE(data.length, 0); body.copy(out, 4); out.writeUInt32BE(crc(body), body.length + 4)
  return out
}
const png = (w, h, depth, colorType, scanlines, extra = []) => Buffer.concat([
  Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]),
  chunk('IHDR', [0, 0, 0, w, 0, 0, 0, h, depth, colorType, 0, 0, 0]),
  ...extra,
  chunk('IDAT', zlib.deflateSync(Buffer.from(scanlines))),
  chunk('IEND', [])
])

const decodeCases = [
  ['4-bit palette with transparency (how Mojang ships many textures)',
    png(2, 1, 4, 3, [0, 0x01], [chunk('PLTE', [10, 20, 30, 40, 50, 60]), chunk('tRNS', [255, 0])]),
    [10, 20, 30, 255, 40, 50, 60, 0]],
  ['8-bit greyscale', png(2, 1, 8, 0, [0, 7, 200]), [7, 7, 7, 255, 200, 200, 200, 255]],
  ['greyscale + alpha', png(1, 1, 8, 4, [0, 9, 100]), [9, 9, 9, 100]],
  ['rgb', png(1, 1, 8, 2, [0, 1, 2, 3]), [1, 2, 3, 255]],
  ['sub filter', png(2, 1, 8, 2, [1, 10, 10, 10, 5, 5, 5]), [10, 10, 10, 255, 15, 15, 15, 255]],
  ['up filter', png(1, 2, 8, 0, [0, 50, 2, 25]), [50, 50, 50, 255, 75, 75, 75, 255]]
]
for (const [name, file, expected] of decodeCases) {
  test(`png decode: ${name}`, () => assert.deepEqual([...decodePng(file).rgba], expected))
}

// ---------------------------------------------------------------- textures
const textureCases = [
  ['stone', 'top', {}, 'stone'],
  ['grass_block', 'top', {}, 'grass_block_top'],
  ['grass_block', 'side', {}, 'grass_block_side'],
  ['grass_block', 'bottom', {}, 'dirt'],
  ['oak_log', 'top', {}, 'oak_log_top'],
  ['oak_log', 'side', {}, 'oak_log'],
  ['water', 'top', {}, 'water_still'],
  ['oak_stairs', 'side', {}, 'oak_planks'],
  ['stone_brick_slab', 'top', {}, 'stone_bricks'],
  ['cobblestone_wall', 'side', {}, 'cobblestone'],
  ['white_bed', 'top', {}, 'white_wool'],
  ['chest', 'side', {}, 'oak_planks'],
  ['oak_door', 'side', { half: 'upper' }, 'oak_door_top'],
  ['oak_door', 'side', { half: 'lower' }, 'oak_door_bottom'],
  ['wheat', 'side', { age: 7 }, 'wheat_stage7'],
  ['furnace', 'side', {}, 'furnace_side'],
  ['crafting_table', 'top', {}, 'crafting_table_top'],
  ['wall_torch', 'side', {}, 'torch']
]
const known = new Set(['stone', 'grass_block_top', 'grass_block_side', 'dirt', 'oak_log_top', 'oak_log', 'water_still', 'oak_planks',
  'stone_bricks', 'cobblestone', 'white_wool', 'oak_door_top', 'oak_door_bottom', 'wheat_stage7', 'furnace_side', 'furnace_top',
  'furnace_front', 'crafting_table_top', 'crafting_table_side', 'crafting_table_front', 'torch'])
for (const [block, face, props, expected] of textureCases) {
  test(`texture for ${block} ${face} ${JSON.stringify(props)}`, () =>
    assert.equal(textureCandidates(block, face, props).find(t => known.has(t)), expected))
}

// ---------------------------------------------------------------- rays
const CUBE = { kind: 'cube' }
const world = blocks => {
  const grid = makeGrid({ x: -8, y: -8, z: -8 }, { x: 17, y: 17, z: 17 })
  blocks.forEach(([x, y, z, id]) => grid.set(x, y, z, id))
  return grid
}
const info = id => [null, CUBE, { kind: 'boxes', boxes: [[0, 0, 0, 1, 0.5, 1]] }, { kind: 'cross' }][id]

test('castRay hits the near face of a cube and reports where', () => {
  const hit = castRay(world([[3, 0, 0, 1]]), info, { x: 0.5, y: 0.5, z: 0.5 }, { x: 1, y: 0, z: 0 }, 16)
  assert.deepEqual([hit.x, hit.y, hit.z, hit.face, hit.t], [3, 0, 0, 'west', 2.5])
})

test('castRay looking down lands on top', () => {
  const hit = castRay(world([[0, -3, 0, 1]]), info, { x: 0.5, y: 0.5, z: 0.5 }, { x: 0, y: -1, z: 0 }, 16)
  assert.deepEqual([hit.y, hit.face, hit.t], [-3, 'top', 2.5])
})

test('castRay returns null when nothing is in range', () => {
  assert.equal(castRay(world([[3, 0, 0, 1]]), info, { x: 0.5, y: 0.5, z: 0.5 }, { x: -1, y: 0, z: 0 }, 16), null)
})

test('castRay flies over a half-height block and hits the cube behind it', () => {
  const grid = world([[2, 0, 0, 2], [4, 0, 0, 1]])
  const hit = castRay(grid, info, { x: 0.5, y: 0.75, z: 0.5 }, { x: 1, y: 0, z: 0 }, 16)
  assert.deepEqual([hit.x, hit.face], [4, 'west'])
})

test('castRay hits a half-height block when aimed low enough', () => {
  const hit = castRay(world([[2, 0, 0, 2]]), info, { x: 0.5, y: 0.25, z: 0.5 }, { x: 1, y: 0, z: 0 }, 16)
  assert.deepEqual([hit.x, hit.face, hit.t], [2, 'west', 1.5])
})

// ---------------------------------------------------------------- camera
const close = (a, b) => assert.ok(Math.abs(a - b) < 1e-9, `${a} !~ ${b}`)
const directionCases = [
  // mineflayer convention: yaw 0 faces north (-z), yaw grows turning left (west); pitch > 0 looks up
  ['yaw 0 faces north', 0, 0, [0, 0, -1]],
  ['yaw pi/2 faces west', Math.PI / 2, 0, [-1, 0, 0]],
  ['yaw pi faces south', Math.PI, 0, [0, 0, 1]],
  ['pitch up', 0, Math.PI / 2, [0, 1, 0]]
]
for (const [name, yaw, pitch, expected] of directionCases) {
  test(`directionFor: ${name}`, () => {
    const d = directionFor(yaw, pitch)
    close(d.x, expected[0]); close(d.y, expected[1]); close(d.z, expected[2])
  })
}

// ---------------------------------------------------------------- render
const solid = (r, g, b) => ({ width: 1, height: 1, rgba: Uint8Array.from([r, g, b, 255]) })
const scene = {
  // a red wall to the north, a green floor below
  grid: world([...[-2, -1, 0, 1, 2].flatMap(x => [-1, 0, 1, 2].map(y => [x, y, -4, 1])), ...[-3, -2, -1, 0, 1, 2, 3].flatMap(x => [-3, -2, -1, 0, 1, 2, 3].map(z => [x, -2, z, 2]))]),
  info: id => [null, { kind: 'cube', name: 'wall' }, { kind: 'cube', name: 'floor' }][id],
  texture: (name) => name === 'wall' ? solid(200, 0, 0) : solid(0, 200, 0),
  eye: { x: 0.5, y: 0.5, z: 0.5 },
  timeOfDay: 6000
}
const pixel = (img, x, y) => [...img.rgba.slice((y * img.width + x) * 4, (y * img.width + x) * 4 + 3)]
const dominant = ([r, g, b]) => r > g && r > b ? 'red' : g > r && g > b ? 'green' : 'other'

test('render: looking north shows the wall in the middle and the floor at the bottom', () => {
  const img = render({ ...scene, yaw: 0, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })
  assert.deepEqual([dominant(pixel(img, 16, 16)), dominant(pixel(img, 16, 31))], ['red', 'green'])
})

test('render: looking south shows sky above the floor', () => {
  const img = render({ ...scene, yaw: Math.PI, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })
  assert.deepEqual([dominant(pixel(img, 16, 2)), dominant(pixel(img, 16, 31))], ['other', 'green'])
})

test('render: a panorama is centred on north, with south at the edges', () => {
  const img = render({ ...scene, panorama: true, width: 64, height: 16, maxDist: 12 })
  assert.deepEqual([dominant(pixel(img, 32, 8)), dominant(pixel(img, 0, 8))], ['red', 'other'])
})

test('render: entities are drawn and reported with their pixel position', () => {
  const entities = [{ name: 'sheep', x: 0.5, y: -1, z: -2.5, width: 0.9, height: 1.3 }]
  const img = render({ ...scene, entities, yaw: 0, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })
  const [seen] = img.seen
  assert.deepEqual([img.seen.length, seen.name, Math.abs(seen.px - 16) <= 1, seen.py > 16, seen.dist], [1, 'sheep', true, true, 3])
})

test('render: entities behind a wall are not reported', () => {
  const entities = [{ name: 'zombie', x: 0.5, y: 0, z: -7.5, width: 0.6, height: 1.95 }]
  const img = render({ ...scene, entities, yaw: 0, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })
  assert.deepEqual(img.seen, [])
})

const nearCases = [
  ['a wall one block away fills the view', [[-1, 0, 1].flatMap(x => [-1, 0, 1, 2].map(y => [x, y, -1, 1]))].flat(), true],
  ['a wall four blocks away does not', [], false]
]
for (const [name, extra, blocked] of nearCases) {
  test(`render: near fraction, ${name}`, () => {
    const grid = world([...extra, ...[-2, -1, 0, 1, 2].flatMap(x => [-1, 0, 1, 2].map(y => [x, y, -4, 1]))])
    const img = render({ ...scene, grid, yaw: 0, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })
    assert.equal(img.near > 0.5, blocked)
    assert.equal(img.near >= 0 && img.near <= 1, true)
  })
}
