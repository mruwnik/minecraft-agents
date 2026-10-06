// renderBand + drawEntities must give exactly the bytes and `seen` that render() gives.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { makeGrid, render } from '../tools/view/renderer.mjs'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { makeBlockSource } from '../tools/view/blocks.mjs'
import { renderBand, drawEntities } from '../tools/view/raycaster.mjs'

const Chunk = makeChunkClass('1.21.4')
const TEXTURES = path.join(import.meta.dirname, '../textures')
const state = name => Chunk.registry.blocksByName[name].defaultState

export const syntheticGrid = () => {
  const grid = makeGrid({ x: -16, y: 50, z: -16 }, { x: 32, y: 24, z: 32 })
  for (let x = -16; x < 16; x++) for (let z = -16; z < 16; z++) grid.set(x, 63, z, state('stone'))
  for (let x = -3; x <= 3; x++) for (let y = 64; y <= 66; y++) grid.set(x, y, -8, state('stone'))
  grid.set(5, 64, -3, state('oak_slab'))
  grid.set(-5, 64, -3, state('short_grass'))
  for (let y = 64; y <= 65; y++) grid.set(1, y, -5, state('oak_leaves'))
  for (let x = 6; x <= 9; x++) for (let z = -6; z <= -3; z++) grid.set(x, 63, z, state('water'))
  grid.top = 66
  return grid
}

const eye = { x: 0.5, y: 65.6, z: 4.5 }
const entities = [
  { name: 'zombie', kind: 'hostile', x: -2, y: 64, z: -2, width: 0.6, height: 1.95, yaw: 0.7 },
  { name: 'cow', x: 3, y: 64, z: -7.2, width: 0.9, height: 1.4, yaw: 0 },
  { name: 'item', kind: 'item', x: 0.5, y: 64, z: 0, width: 0.25, height: 0.25 },
  { name: 'creeper', kind: 'hostile', x: 0.5, y: 64, z: 10, width: 0.6, height: 1.7 },
  { name: 'pig', x: 0.5, y: 64, z: -60, width: 0.9, height: 0.9 }
]
const cameras = [
  { yaw: 0, pitch: 0, fov: 70 },
  { yaw: 0, pitch: -1.2, fov: 70 },
  { yaw: 0, pitch: 1.2, fov: 70 },
  { yaw: 2.5, pitch: -0.1, fov: 70 },
  { yaw: 0.3, pitch: -0.2, fov: 90 },
  { panorama: true }
]

const sources = {
  textured: () => makeBlockSource(Chunk.registry, TEXTURES),
  flat: () => makeBlockSource(Chunk.registry, path.join(import.meta.dirname, 'no-such-textures'))
}

const splits = {
  whole: h => [[0, h]],
  bands: h => [[0, 7], [7, 20], [20, h]],
  single: h => Array.from({ length: h }, (_, i) => [i, i + 1])
}

const [width, height, maxDist] = [48, 27, 64]

for (const [sourceName, makeSource] of Object.entries(sources)) {
  for (const [splitName, split] of Object.entries(splits)) {
    for (const timeOfDay of [6000, 18000]) {
      for (const camera of cameras) {
        test(`${sourceName} ${splitName} t=${timeOfDay} ${JSON.stringify(camera)}: bands then entities equal render()`, () => {
          const grid = syntheticGrid()
          const scene = { grid, eye, timeOfDay, width, height, maxDist, ...camera }
          const expected = render({ ...scene, ...makeSource(), entities })
          const rgba = new Uint8Array(width * height * 4)
          const depth = new Float64Array(width * height)
          const source = makeSource()
          for (const [rowStart, rowEnd] of split(height)) renderBand({ ...scene, ...source, rgba, depth, rowStart, rowEnd })
          const seen = drawEntities({ ...scene, entities, rgba, depth })
          assert.deepEqual(rgba, expected.rgba)
          assert.deepEqual(seen, expected.seen)
        })
      }
    }
  }
}

test('the scene has something to see: a zombie and a cow on screen, the far pig and the rear creeper not', () => {
  const out = render({ grid: syntheticGrid(), eye, entities, timeOfDay: 6000, width, height, maxDist, yaw: 0, pitch: 0, fov: 70, ...sources.flat() })
  assert.deepEqual(out.seen.map(s => s.name).sort(), ['cow', 'item', 'zombie'])
})

test('renderBand returns how many band pixels are terrain closer than 2', () => {
  const grid = syntheticGrid()
  const scene = { grid, eye, timeOfDay: 6000, width, height, maxDist, yaw: 0, pitch: -1.5, fov: 70, ...sources.flat() }
  const rgba = new Uint8Array(width * height * 4)
  const depth = new Float64Array(width * height)
  const all = renderBand({ ...scene, rgba, depth, rowStart: 0, rowEnd: height })
  const halves = renderBand({ ...scene, rgba, depth, rowStart: 0, rowEnd: 10 }) + renderBand({ ...scene, rgba, depth, rowStart: 10, rowEnd: height })
  assert.equal(all, depth.filter(t => t < 2).length)
  assert.equal(halves, all)
})

test('depth is the hit distance, or maxDist for sky', () => {
  const scene = { grid: syntheticGrid(), eye, timeOfDay: 6000, width, height, maxDist, yaw: 0, pitch: 1.2, fov: 70, ...sources.flat() }
  const depth = new Float64Array(width * height)
  renderBand({ ...scene, rgba: new Uint8Array(width * height * 4), depth, rowStart: 0, rowEnd: height })
  assert.equal(depth[0], maxDist)
})
