// The thread pool must draw the very bytes renderBand draws in-process, frame after frame, and fail loudly, not hang.
import { test, after } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { makeGrid } from '../src/vision/renderer.mjs'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { makeBlockSource } from '../tools/view/blocks.mjs'
import { renderBand, NEAR } from '../tools/view/raycaster.mjs'
import { renderPool, poolFor, closePools, BAND } from '../tools/view/pool.mjs'

const VERSION = '1.21.4'
const Chunk = makeChunkClass(VERSION)
const TEXTURES = path.join(import.meta.dirname, '../textures')
const state = name => Chunk.registry.blocksByName[name].defaultState

const grid = (() => {
  const g = makeGrid({ x: -16, y: 50, z: -16 }, { x: 32, y: 24, z: 32 })
  for (let x = -16; x < 16; x++) for (let z = -16; z < 16; z++) g.set(x, 63, z, state('stone'))
  for (let x = -3; x <= 3; x++) for (let y = 64; y <= 66; y++) g.set(x, y, -8, state('stone'))
  g.set(5, 64, -3, state('oak_slab'))
  g.set(-5, 64, -3, state('short_grass'))
  g.set(1, 64, -5, state('oak_leaves'))
  g.top = 66
  return g
})()

const [width, height] = [48, 27]
const scene = { grid, eye: { x: 0.5, y: 65.6, z: 4.5 }, timeOfDay: 6000, width, height, maxDist: 64, yaw: 0, pitch: -0.3, fov: 70 }

const inProcess = frame => {
  const rgba = new Uint8Array(width * height * 4)
  const depth = new Float64Array(width * height)
  const { info, texture } = makeBlockSource(Chunk.registry, TEXTURES)
  const near = renderBand({ ...frame, info, texture, rgba, depth, rowStart: 0, rowEnd: height })
  return { rgba, depth, near: near / (width * height) }
}

after(() => closePools())

test('BAND is 4 rows', () => assert.equal(BAND, 4))

for (const threads of [1, 2, 3]) {
  test(`${threads} thread(s) draw what renderBand draws in-process`, () => {
    const pool = renderPool({ threads, version: VERSION, textureDir: TEXTURES })
    const out = pool.render(scene)
    const expected = inProcess(scene)
    assert.deepEqual(out.rgba, expected.rgba)
    assert.deepEqual(out.depth, expected.depth)
    assert.equal(out.near, expected.near)
    pool.close()
  })
}

test('workers are reused: two frames with different cameras are both right', () => {
  const pool = poolFor({ threads: 2, version: VERSION, textureDir: TEXTURES })
  for (const camera of [{ yaw: 0.4, pitch: -0.5 }, { yaw: 3, pitch: 0.2 }, { yaw: 0.4, pitch: -0.5 }]) {
    const frame = { ...scene, ...camera }
    const out = pool.render(frame)
    const expected = inProcess(frame)
    assert.deepEqual(out.rgba, expected.rgba)
    assert.deepEqual(out.depth, expected.depth)
  }
})

test('a size change between frames still draws right', () => {
  const pool = poolFor({ threads: 2, version: VERSION, textureDir: TEXTURES })
  const out = pool.render({ ...scene, width: 20, height: 11 })
  assert.equal(out.rgba.length, 20 * 11 * 4)
  assert.equal(out.depth.length, 20 * 11)
})

test('poolFor hands back the same pool for the same key', () => {
  assert.equal(poolFor({ threads: 2, version: VERSION, textureDir: TEXTURES }), poolFor({ threads: 2, version: VERSION, textureDir: TEXTURES }))
})

test('an eye outside the grid throws instead of hanging, and the pool then still renders', () => {
  const pool = poolFor({ threads: 2, version: VERSION, textureDir: TEXTURES })
  assert.throws(() => pool.render({ ...scene, eye: { x: 500, y: 65, z: 500 } }), /the eye is outside the grid/)
  assert.deepEqual(pool.render(scene).rgba, inProcess(scene).rgba)
})

test('near counts pixels closer than NEAR', () => {
  const pool = poolFor({ threads: 2, version: VERSION, textureDir: TEXTURES })
  const out = pool.render({ ...scene, pitch: -1.5 })
  assert.equal(out.near, out.depth.filter(t => t < NEAR).length / (width * height))
})
