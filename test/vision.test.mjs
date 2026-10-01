import test from 'node:test'
import assert from 'node:assert/strict'
import zlib from 'node:zlib'
import crypto from 'node:crypto'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { EventEmitter } from 'node:events'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { Vec3 } from 'vec3'
import { makeEyes, lookKey } from '../src/vision/eyes.mjs'
import { encodePng, decodePng, textureCandidates, blockIcon, castRay, makeGrid, render, directionFor } from '../src/vision/renderer.mjs'

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
  ['wall_torch', 'side', {}, 'torch'],
  ['bamboo', 'side', {}, 'bamboo_stalk'],
  ['oak_wood', 'side', {}, 'oak_log'],
  ['stripped_birch_wood', 'side', {}, 'stripped_birch_log'],
  ['crimson_hyphae', 'side', {}, 'crimson_stem'],
  ['waxed_cut_copper_stairs', 'side', {}, 'cut_copper'],
  ['infested_stone_bricks', 'side', {}, 'stone_bricks'],
  ['potted_poppy', 'cross', {}, 'poppy'],
  ['smooth_sandstone_slab', 'top', {}, 'sandstone_top'],
  ['quartz_stairs', 'side', {}, 'quartz_block_side'],
  ['snow_block', 'top', {}, 'snow'],
  ['magma_block', 'side', {}, 'magma'],
  ['water_cauldron', 'side', {}, 'cauldron_side'],
  ['sweet_berry_bush', 'cross', { age: 2 }, 'sweet_berry_bush_stage2'],
  ['campfire', 'top', {}, 'campfire_log'],
  ['redstone_wire', 'top', {}, 'redstone_dust_line0'],
  ['fire', 'cross', {}, 'fire_0'],
  ['frosted_ice', 'top', {}, 'frosted_ice_0'],
  ['dried_kelp_block', 'side', {}, 'dried_kelp_side'],
  ['ender_chest', 'side', {}, 'obsidian'],
  ['piston_head', 'side', {}, 'piston_side'],
  ['sticky_piston', 'top', {}, 'piston_top'],
  ['light_weighted_pressure_plate', 'top', {}, 'gold_block'],
  ['petrified_oak_slab', 'top', {}, 'oak_planks'],
  ['red_candle_cake', 'side', {}, 'cake_side']
]
const known = new Set(['stone', 'grass_block_top', 'grass_block_side', 'dirt', 'oak_log_top', 'oak_log', 'water_still', 'oak_planks',
  'stone_bricks', 'cobblestone', 'white_wool', 'oak_door_top', 'oak_door_bottom', 'wheat_stage7', 'furnace_side', 'furnace_top',
  'furnace_front', 'crafting_table_top', 'crafting_table_side', 'crafting_table_front', 'torch', 'bamboo_stalk', 'oak_log',
  'stripped_birch_log', 'crimson_stem', 'cut_copper', 'stone_bricks', 'poppy', 'sandstone_top', 'sandstone', 'quartz_block_side',
  'snow', 'magma', 'cauldron_side', 'sweet_berry_bush_stage2', 'campfire_log', 'redstone_dust_line0', 'fire_0', 'frosted_ice_0',
  'dried_kelp_side', 'obsidian', 'piston_side', 'piston_top', 'gold_block', 'oak_planks', 'cake_side'])
for (const [block, face, props, expected] of textureCases) {
  test(`texture for ${block} ${face} ${JSON.stringify(props)}`, () =>
    assert.equal(textureCandidates(block, face, props).find(t => known.has(t)), expected))
}

// ---------------------------------------------------------------- inventory icons
const filled = (rgba, size = 16) => ({ width: size, height: size, rgba: Uint8Array.from({ length: size * size * 4 }, (_, i) => rgba[i % 4]) })
const rgbaAt = (img, x, y) => [...img.rgba.subarray((y * img.width + x) * 4, (y * img.width + x) * 4 + 4)]
const cube = blockIcon(filled([200, 0, 0, 255]), filled([0, 200, 0, 255]), filled([0, 0, 200, 255]))
for (const [name, x, y, expected] of [
  ['the top face, fully lit', 16, 4, [200, 0, 0, 255]],
  ['the left face (the front) a little darker', 6, 18, [0, 160, 0, 255]],
  ['the right face darker still', 26, 18, [0, 0, 120, 255]],
  ['above the cube is see-through', 1, 1, [0, 0, 0, 0]],
  ['below the left face is see-through', 1, 30, [0, 0, 0, 0]]
]) {
  test(`blockIcon: ${name}`, () => assert.deepEqual([cube.width, cube.height, rgbaAt(cube, x, y)], [32, 32, expected]))
}
test('blockIcon: a transparent texel stays transparent (glass, leaves)', () =>
  assert.equal(rgbaAt(blockIcon(filled([9, 9, 9, 0]), filled([9, 9, 9, 0]), filled([9, 9, 9, 0])), 16, 4)[3], 0))

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

// `top` promises nothing stands higher; the blocks above it stand in for the cells a ray would walk through regardless
test('castRay: above the grid\'s top, a ray going up or level ends without walking on', () => {
  const grid = { ...world([[0, -2, 0, 1], [0, 4, 0, 1], [4, 0, 0, 1]]), top: -2 }
  const from = { x: 0.5, y: 0.5, z: 0.5 }
  const ray = d => castRay(grid, info, from, d, 12)
  assert.deepEqual([ray({ x: 0, y: 1, z: 0 }), ray({ x: 1, y: 0, z: 0 }), ray({ x: 0, y: -1, z: 0 }).y], [null, null, -2])
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

// The renderer's picture for this scene, pinned: a change in the pixels is a change on purpose, and updates the hash.
// Entities sit on screen, off screen and behind the eye; the floor's far rows reach the fog; a plant and a slab wear a
// texture whose texels differ and one is see-through, so where each ray lands on them shows.
const checker = { width: 2, height: 2, rgba: Uint8Array.from([200, 0, 0, 255, 0, 0, 200, 0, 0, 200, 0, 255, 200, 200, 0, 255]) }
const pinnedGrid = makeGrid(scene.grid.origin, scene.grid.size)
pinnedGrid.data.set(scene.grid.data)
pinnedGrid.set(-1, -1, -2, 3)
pinnedGrid.set(-2, -1, -3, 4)
const pinned = {
  ...scene,
  grid: pinnedGrid,
  info: id => [null, { kind: 'cube', name: 'wall' }, { kind: 'cube', name: 'floor' }, { kind: 'cross', name: 'plant' }, { kind: 'boxes', name: 'slab', boxes: [[0, 0, 0, 1, 0.5, 1]] }][id],
  texture: name => name === 'plant' || name === 'slab' ? checker : scene.texture(name),
  entities: [
    { name: 'cow', kind: 'passive', x: 0.5, y: -1, z: -2.5, width: 0.9, height: 1.4 },
    { name: 'item', x: 2.5, y: -1, z: -1.5, width: 0.35, height: 0.35 },
    { name: 'zombie', kind: 'hostile', x: 0.5, y: -1, z: 3.5, width: 0.6, height: 1.95 },
    { name: 'sheep', x: -9, y: -1, z: -2, width: 0.9, height: 1.3 }
  ]
}
const sha = img => crypto.createHash('sha256').update(img.rgba).digest('hex').slice(0, 16)
test('render: the pinned view is drawn exactly as before', () => {
  const img = render({ ...pinned, yaw: 0, pitch: 0, width: 64, height: 40, fov: 100, maxDist: 12 })
  assert.deepEqual([sha(img), img.seen.map(e => e.name)], ['edae299000cdce66', ['cow', 'item']])
})
test('render: the pinned panorama is drawn exactly as before', () => {
  const img = render({ ...pinned, panorama: true, width: 96, height: 24, maxDist: 12 })
  assert.deepEqual([sha(img), img.seen.map(e => e.name).sort()], ['d894513612815aa8', ['cow', 'item', 'sheep', 'zombie']])
})

test('render: an entity whose box is wholly behind the eye is neither drawn nor seen', () => {
  const behind = { ...scene, entities: [{ name: 'cow', x: 0.5, y: -1, z: 3.5, width: 0.9, height: 1.4 }] }
  const img = render({ ...behind, yaw: 0, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })
  assert.deepEqual(img.seen, [])
  assert.equal(sha(img), sha(render({ ...scene, yaw: 0, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })))
})

// a 64x64 view north from the scene's eye with fov 90: the pixel a point dx, dy across and up, dz north of the eye lands on
const pixelAt = (dx, dy, dz) => [Math.round((dx / dz + 1) * 32 - 0.5), Math.round((1 - dy / dz) * 32 - 0.5)]
const view64 = { yaw: 0, pitch: 0, width: 64, height: 64, fov: 90, maxDist: 12 }
const bare = render({ ...scene, ...view64 })
const covers = (img, [x, y]) => pixel(img, x, y).join() !== pixel(bare, x, y).join()
// a cow three blocks north, side on: east is yaw -pi/2, west pi/2
const sideOn = yaw => render({ ...scene, ...view64, entities: [{ name: 'cow', x: 0.5, y: -1, z: -2.5, width: 0.9, height: 1.4, yaw }] })
test('render: a cow seen side on stands on legs, with the floor showing between them', () => {
  const img = sideOn(-Math.PI / 2)
  assert.deepEqual([covers(img, pixelAt(-0.11, -1.22, 3)), covers(img, pixelAt(0.3, -1.22, 3))], [false, true])
})
for (const [name, yaw, east, west] of [['east', -Math.PI / 2, true, false], ['west', Math.PI / 2, false, true]]) {
  test(`render: a cow facing ${name} has its head out on that side`, () => {
    const img = sideOn(yaw)
    assert.deepEqual([covers(img, pixelAt(0.8, -0.2, 3)), covers(img, pixelAt(-0.8, -0.2, 3))], [east, west])
  })
}

// the mean colour of the pixels an entity changed, standing three blocks north facing the eye
const entityColour = entity => {
  const img = render({ ...scene, ...view64, entities: [{ x: 0.5, y: -1, z: -2.5, yaw: Math.PI, ...entity }] })
  const changed = []
  for (let y = 0; y < 64; y++) for (let x = 0; x < 64; x++) if (covers(img, [x, y])) changed.push(pixel(img, x, y))
  return [0, 1, 2].map(i => changed.reduce((s, c) => s + c[i], 0) / changed.length)
}
for (const [name, entity, looks] of [
  ['a creeper is green', { name: 'creeper', kind: 'hostile', width: 0.6, height: 1.7 }, ([r, g, b]) => g > r && g > b],
  ['a skeleton is pale', { name: 'skeleton', kind: 'hostile', width: 0.6, height: 1.99 }, c => Math.min(...c) > 120],
  ['a spider is dark', { name: 'spider', kind: 'hostile', width: 1.4, height: 0.9 }, c => Math.max(...c) < 90],
  ['a hostile without colours of its own is red', { name: 'breeze', kind: 'hostile', width: 0.6, height: 1.77 }, ([r, g, b]) => r > 2 * g && r > 2 * b],
  ['a player is magenta', { name: 'player', kind: 'player', width: 0.6, height: 1.8 }, ([r, g, b]) => r > g && b > g]
]) test(`render: ${name}`, () => assert.ok(looks(entityColour(entity)), String(entityColour(entity))))

test('render: a zombie facing the eye shows its face, one facing away the back of its head', () => {
  const zombie = yaw => render({ ...scene, ...view64, entities: [{ name: 'zombie', kind: 'hostile', x: 0.5, y: -1, z: -2.5, width: 0.6, height: 1.95, yaw }] })
  const head = pixelAt(0, 0.2, 2.75)
  assert.notDeepEqual(pixel(zombie(Math.PI), ...head), pixel(zombie(0), ...head))
})

test('render: seen gives each entity the box of pixels it covers, holding its centre, smaller for a chicken than a cow', () => {
  const at = entity => render({ ...scene, ...view64, entities: [{ x: 0.5, y: -1, z: -2.5, yaw: Math.PI, kind: 'passive', ...entity }] }).seen[0]
  const cow = at({ name: 'cow', width: 0.9, height: 1.4 })
  const chicken = at({ name: 'chicken', width: 0.4, height: 0.7 })
  const area = ({ box: [x1, y1, x2, y2] }) => (x2 - x1 + 1) * (y2 - y1 + 1)
  const holds = ({ px, py, box: [x1, y1, x2, y2] }) => x1 <= px && px <= x2 && y1 <= py && py <= y2
  assert.deepEqual([cow.kind, holds(cow), holds(chicken), area(chicken) < area(cow) / 3], ['passive', true, true, true])
})

// ---------------------------------------------------------------- eyes
const registry = prismarineRegistry('26.1')
const STONE = registry.blocksByName.stone.defaultState
const Chunk = prismarineChunk(registry)
// a body standing on a stone floor (y 63) looking north, with `reads` counting the chunk columns it copies
const standingBot = () => {
  const columns = new Map()
  const column = (cx, cz) => {
    const key = `${cx},${cz}`
    if (columns.has(key)) return columns.get(key)
    const c = new Chunk({ minY: -64, worldHeight: 384 })
    for (let x = 0; x < 16; x++) for (let z = 0; z < 16; z++) c.setBlockStateId(new Vec3(x, 63, z), STONE)
    columns.set(key, c)
    return c
  }
  const bot = Object.assign(new EventEmitter(), {
    registry,
    reads: 0,
    game: { minY: -64, height: 384 },
    entity: { position: new Vec3(0.5, 64, 0.5), eyeHeight: 1.62, yaw: 0, pitch: 0 },
    entities: {},
    time: { timeOfDay: 6000 },
    column
  })
  bot.world = { getColumn: (cx, cz) => { bot.reads++; return column(cx, cz) } }
  return bot
}
// a stone wall across the view, one and a half blocks north of the body at (x, 64, 0.5); without an event, as the
// chunk data changes under a body that has not been told yet
const putWall = (bot, x) => [...Array(9).keys()].flatMap(dx => [64, 65, 66, 67].map(y => new Vec3(x + dx - 4, y, -2))).forEach(p => {
  bot.column(p.x >> 4, p.z >> 4).setBlockStateId(new Vec3(p.x & 15, p.y, p.z & 15), STONE)
  bot.wallCells = [...(bot.wallCells ?? []), p]
})
const eyesFor = bot => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'eyes-'))
  return makeEyes(bot, { textureDir: dir, snapshotDir: dir })
}

const lookKeyBase = { eye: { x: 0.5, y: 65.62, z: 0.5 }, yaw: 0.1, pitch: 0, timeOfDay: 6000, entities: [{ name: 'cow', x: 3, y: 65, z: 2 }], world: '1:0', width: 320, height: 180, maxDist: 64, panorama: false, fov: 100 }
for (const [name, change, same] of [
  ['the same scene', {}, true],
  ['a step of under a sixteenth of a block', { eye: { x: 0.52, y: 65.62, z: 0.5 } }, true],
  // 0.004 from the brief lands exactly on this base yaw's rounding boundary (11 vs 12) and flips buckets by
  // floating-point luck; 0.0003 keeps the same "well under half a degree" intent without the boundary coincidence
  ['a turn of under half a degree', { yaw: 0.1 + 0.0003 }, true],
  ['a step of a block', { eye: { x: 1.5, y: 65.62, z: 0.5 } }, false],
  ['a turn', { yaw: 0.3 }, false],
  ['a hundred ticks later', { timeOfDay: 6100 }, false],
  ['an entity that moved a block', { entities: [{ name: 'cow', x: 4, y: 65, z: 2 }] }, false],
  ['a block changed', { world: '1:1' }, false],
  ['another size', { width: 480, height: 270 }, false],
  ['a panorama', { panorama: true }, false]
]) test(`lookKey: ${name} ${same ? 'draws nothing new' : 'is a new picture'}`, () => assert.equal(lookKey({ ...lookKeyBase, ...change }) === lookKey(lookKeyBase), same))

const freshCases = [
  ['a block update', bot => { putWall(bot, 0); bot.wallCells.forEach(position => bot.emit('blockUpdate', null, { position, stateId: STONE })) }],
  ['a chunk that arrives', bot => { putWall(bot, 0); bot.emit('chunkColumnLoad', new Vec3(0, 0, -16)) }],
  ['walking off', bot => { putWall(bot, 30); bot.entity.position.x = 30.5 }]
]
for (const [name, change] of freshCases) {
  test(`look: a wall put up since the last look is seen, after ${name}`, async () => {
    const bot = standingBot()
    const look = eyesFor(bot)
    const before = await look({ file: 'a.png' })
    change(bot)
    const after = await look({ file: 'b.png' })
    assert.deepEqual([before.blocked, Boolean(after.blocked)], [null, true])
  })
}

test('look: a body that has not moved does not copy the world again', async () => {
  const bot = standingBot()
  const look = eyesFor(bot)
  await look({ file: 'a.png' })
  const copied = bot.reads
  await look({ file: 'b.png' })
  assert.deepEqual([copied > 0, bot.reads], [true, copied])
})

test("look: the body's own thread runs on while the picture is drawn", async () => {
  const bot = standingBot()
  const look = eyesFor(bot)
  await look({ file: 'a.png' })
  let ticks = 0
  const timer = setInterval(() => ticks++, 1)
  // a changed scene so this look is actually drawn, not answered from the cache under test below
  bot.time.timeOfDay = 6100
  await look({ file: 'b.png' })
  clearInterval(timer)
  assert.ok(ticks > 0)
})

test('look: an unchanged scene answers the last frame without asking the worker to draw again', async () => {
  const bot = standingBot()
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'eyes-'))
  const look = makeEyes(bot, { textureDir: dir, snapshotDir: dir })
  const first = await look({ file: 'a.png' })
  const second = await look({ file: 'b.png' })
  // a cache hit still writes the file (the dashboard reads it from disk), with the bytes the worker drew the first time
  assert.deepEqual(fs.readFileSync(path.join(dir, 'a.png')), fs.readFileSync(path.join(dir, 'b.png')))
  assert.deepEqual([second.view, second.seen], [first.view, first.seen])
  // answering from the cache skips the worker round trip entirely: the draw count stays at the first, real draw
  assert.equal(look.draws, 1)
})
