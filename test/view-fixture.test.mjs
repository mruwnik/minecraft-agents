import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { decodeColumnFile, restoreColumn } from '../engine/js/view.mjs'
import { cameraBasis } from '../tools/view/web/camera.mjs'
import { faceRegion } from '../tools/view/project.mjs'
import { FIXTURE, blockNameAt, lightAt, writeFixture } from '../tools/view/fixture.mjs'

const Chunk = makeChunkClass('26.1')
const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-fixture-test-'))
writeFixture(stateDir)

const load = (cx, cz) => {
  const { header, sections, light } = decodeColumnFile(fs.readFileSync(path.join(stateDir, 'worlds', 'fixture', 'chunks', `${cx}.${cz}.bin`)))
  return { header, column: restoreColumn(new Chunk({ minY: header.minY, worldHeight: header.worldHeight }), { sections, light }) }
}

const blocks = [
  ['floor', 3, 64, 3, 'stone'], ['above floor', 3, 65, 3, 'air'], ['lit stripe', 2, 66, 4, 'stone'], ['dark stripe', 7, 68, 4, 'stone'],
  ['gap', 9, 66, 4, 'air'], ['leaves', 10, 67, 4, 'oak_leaves'], ['wool', 10, 67, 3, 'red_wool'], ['diamond', 13, 65, 4, 'diamond_ore'],
  ['above wall', 2, 69, 4, 'air'], ['other chunk', -3, 64, 5, 'air'], ['below floor', 3, 63, 3, 'air']
]

test('blockNameAt describes the scene', () => {
  for (const [, x, y, z, name] of blocks) assert.equal(blockNameAt(x, y, z), name, `${x} ${y} ${z}`)
})

const lights = [
  ['open air', 3, 100, 8, { sky: 15, block: 0 }],
  ['in front of lit stripe', 2, 66, 5, { sky: 15, block: 0 }],
  ['in front of dark stripe', 6, 66, 5, { sky: 0, block: 0 }],
  ['dark stripe row 2', 8, 68, 6, { sky: 0, block: 0 }],
  ['dark pocket ends at z=7', 6, 66, 7, { sky: 15, block: 0 }],
  ['torch patch', FIXTURE.torch.x[0], 65, 9, { sky: 0, block: 14 }],
  ['next to torch patch', FIXTURE.torch.x[1] + 1, 65, 9, { sky: 15, block: 0 }],
  ['solid', 3, 64, 3, { sky: 0, block: 0 }],
  ['other chunk', -5, 70, -5, { sky: 15, block: 0 }]
]

test('lightAt describes the lighting', () => {
  for (const [, x, y, z, want] of lights) assert.deepEqual(lightAt(x, y, z), want, `${x} ${y} ${z}`)
})

test('the four columns round trip through the file format with every cell set', () => {
  for (const [cx, cz] of [[0, 0], [0, -1], [-1, 0], [-1, -1]]) {
    const { header, column } = load(cx, cz)
    assert.equal(header.mcVersion, '26.1')
    assert.equal(header.minY, -64)
    assert.equal(header.worldHeight, 384)
    const inColumn = ([, x, , z]) => Math.floor(x / 16) === cx && Math.floor(z / 16) === cz
    for (const [, x, y, z] of [...blocks, ...lights].filter(inColumn)) {
      const local = { x: x - cx * 16, y, z: z - cz * 16 }
      assert.equal(column.getSkyLight(local), lightAt(x, y, z).sky, `sky ${x} ${y} ${z}`)
      assert.equal(column.getBlockLight(local), lightAt(x, y, z).block, `block ${x} ${y} ${z}`)
      assert.equal(column.getBlock(local).name, blockNameAt(x, y, z), `block ${x} ${y} ${z}`)
    }
  }
})

test('the lowest and highest sections carry light data', () => {
  const { column } = load(-1, -1)
  for (const y of [-64, -1, 0, 319]) assert.equal(column.getSkyLight({ x: 3, y, z: 3 }), 15, `y ${y}`)
})

test('pose and hud files are written', () => {
  const pose = JSON.parse(fs.readFileSync(path.join(stateDir, 'agents', 'Fixture', 'view', 'pose.json'), 'utf8'))
  assert.equal(pose.world, 'fixture')
  assert.equal(pose.mcVersion, '26.1')
  assert.deepEqual(pose.eye, FIXTURE.eye)
  assert.equal(pose.yaw, FIXTURE.yaw)
  assert.equal(JSON.parse(fs.readFileSync(path.join(stateDir, 'agents', 'Fixture', 'view', 'hud.json'), 'utf8')).v, 1)
})

test('every region is fully on screen at 640x360 fov 70', () => {
  const basis = cameraBasis({ yaw: FIXTURE.yaw, pitch: FIXTURE.pitch, fov: 70 })
  for (const { name, face } of FIXTURE.regions) {
    const r = faceRegion(basis, FIXTURE.eye, face, 640, 360, 0)
    assert.ok(r, name)
    assert.ok(r.x0 >= 0 && r.y0 >= 0 && r.x1 < 640 && r.y1 < 360, `${name} ${JSON.stringify(r)}`)
    assert.ok(r.x1 - r.x0 >= 20 && r.y1 - r.y0 >= 10, `${name} too small ${JSON.stringify(r)}`)
  }
})
