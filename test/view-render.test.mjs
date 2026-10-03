// renderView on a tiny synthetic world written to a temp state dir: a floor and a wall block in front of the camera.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { renderView, readPose } from '../tools/view/render.mjs'
import { decodePng } from '../src/vision/renderer.mjs'

const VERSION = '1.21.4'
const Chunk = makeChunkClass(VERSION)
const stone = Chunk.registry.blocksByName.stone.defaultState

const writeColumn = (dir, cx, cz, column) => {
  const sections = column.dump()
  const header = Buffer.from(JSON.stringify({ v: 1, x: cx, z: cz, t: 1, body: 'Bob', mcVersion: VERSION, minY: -64, worldHeight: 384, parts: [{ name: 'sections', len: sections.length }] }))
  const n = Buffer.alloc(4)
  n.writeUInt32LE(header.length)
  fs.mkdirSync(path.join(dir, 'worlds/w/chunks'), { recursive: true })
  fs.writeFileSync(path.join(dir, `worlds/w/chunks/${cx}.${cz}.bin`), zlib.deflateSync(Buffer.concat([n, header, sections])))
}

const stateWith = pose => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-render-'))
  const column = new Chunk({ minY: -64, worldHeight: 384 })
  for (let x = 0; x < 16; x++) for (let z = 0; z < 16; z++) column.setBlockStateId({ x, y: 63, z }, stone)
  column.setBlockStateId({ x: 8, y: 65, z: 4 }, stone) // a block ahead (north) of an eye at 8.5, 65.62, 8.5
  writeColumn(dir, 0, 0, column)
  fs.mkdirSync(path.join(dir, 'agents/Bob/view'), { recursive: true })
  fs.writeFileSync(path.join(dir, 'agents/Bob/view/pose.json'), JSON.stringify({ v: 1, t: 1, world: 'w', status: 'online', mcVersion: VERSION, pos: { x: 8.5, y: 64, z: 8.5 }, eye: { x: 8.5, y: 65.62, z: 8.5 }, yaw: 0, pitch: 0, entities: [], timeOfDay: 6000, ...pose }))
  return dir
}

const pixel = (png, x, y) => {
  const img = decodePng(png)
  const at = (y * img.width + x) * 4
  return [img.rgba[at], img.rgba[at + 1], img.rgba[at + 2]]
}

test('the centre pixel facing a wall block is not the sky; looking up it is', async () => {
  const stateDir = stateWith({})
  const wall = await renderView({ agentName: 'Bob', width: 64, height: 36, fov: 70, stateDir })
  assert.equal(wall.columns, 1)
  const sky = await renderView({ agentName: 'Bob', width: 64, height: 36, fov: 70, stateDir, override: { pitch: 1.2 } })
  assert.notDeepEqual(pixel(wall.png, 32, 18), pixel(sky.png, 32, 18))
})

test('looking down shows the floor, not the sky', async () => {
  const stateDir = stateWith({ pitch: -1.2 })
  const down = await renderView({ agentName: 'Bob', width: 64, height: 36, fov: 70, stateDir })
  const up = await renderView({ agentName: 'Bob', width: 64, height: 36, fov: 70, stateDir, override: { pitch: 1.2 } })
  assert.notDeepEqual(pixel(down.png, 32, 18), pixel(up.png, 32, 18))
})

test('readPose says what is wrong when there is no pose', () => {
  const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-render-'))
  assert.throws(() => readPose('Nobody', stateDir), /pose\.json/)
})

test('the PNG has the asked size', async () => {
  const { png } = await renderView({ agentName: 'Bob', width: 40, height: 30, fov: 70, stateDir: stateWith({}) })
  const img = decodePng(png)
  assert.deepEqual([img.width, img.height], [40, 30])
})

for (const [name, pose] of [
  ['no eye', { eye: undefined, pos: undefined }],
  ['no yaw', { yaw: undefined }],
  ['a non-numeric eye', { eye: { x: 'a', y: 1, z: 2 } }]
]) {
  test(`renderView refuses a pose with ${name}, with a clear PoseError`, () => {
    const stateDir = stateWith({ status: 'offline', ...pose })
    assert.throws(() => renderView({ agentName: 'Bob', width: 8, height: 8, stateDir }), e => e.name === 'PoseError' && /pose has no position; body never fully started/.test(e.message))
  })
}

test('an offline pose that has a position still renders', () => {
  const r = renderView({ agentName: 'Bob', width: 8, height: 8, stateDir: stateWith({ status: 'offline' }) })
  assert.equal(r.pose.status, 'offline')
})
