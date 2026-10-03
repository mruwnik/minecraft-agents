import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { decodeColumnFile, restoreColumn } from '../engine/js/view.mjs'
import { cameraBasis } from '../tools/view/web/camera.mjs'
import { decodeLight } from '../tools/view/web/decode.mjs'
import { faceRegion } from '../tools/view/project.mjs'
import { FIXTURE, STATES, blockNameAt, lightAt, writeFixture } from '../tools/view/fixture.mjs'

const Chunk = makeChunkClass('26.1')
const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-fixture-test-'))
writeFixture(stateDir)

const load = (cx, cz) => {
  const { header, sections, light } = decodeColumnFile(fs.readFileSync(path.join(stateDir, 'worlds', 'fixture', 'chunks', `${cx}.${cz}.bin`)))
  return { header, light, column: restoreColumn(new Chunk({ minY: header.minY, worldHeight: header.worldHeight }), { sections, light }) }
}

// the dumped light in vanilla nibble order (prismarine's own getters are scrambled within a row, so they are not truth)
const lightCell = ({ header, light }, x, y, z) => {
  const values = decodeLight(light.buffer, light.meta, header.worldHeight >> 4)
  const ly = y - header.minY
  const v = values[(ly >> 4) * 4096 + ((ly & 15) << 8 | z << 4 | x)]
  return { sky: v >> 4, block: v & 15 }
}

const blocks = [
  ['floor', 3, 64, 3, 'stone'], ['above floor', 3, 65, 3, 'air'], ['lit stripe', 2, 66, 4, 'stone'], ['dark stripe', 7, 68, 4, 'stone'],
  ['gap', 9, 66, 4, 'air'], ['leaves', 10, 67, 4, 'oak_leaves'], ['wool', 10, 67, 3, 'red_wool'], ['diamond', 13, 65, 4, 'diamond_ore'],
  ['above wall', 2, 69, 4, 'air'], ['slab', 4, 65, 8, 'oak_slab'], ['behind the slab', 4, 65, 7, 'red_wool'], ['snow', 11, 65, 8, 'snow'], ['behind the snow', 11, 65, 7, 'gold_block'], ['above the slab', 4, 66, 8, 'air'], ['other chunk', -3, 64, 5, 'air'], ['below floor', 3, 63, 3, 'air']
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
  ['next to torch patch', FIXTURE.torch.x[1] + 1, 65, 8, { sky: 15, block: 0 }],
  ['solid', 3, 64, 3, { sky: 0, block: 0 }],
  ['leaves let light through', 10, 67, 4, { sky: 15, block: 0 }],
  ['slab cell is lit', 4, 65, 8, { sky: 15, block: 0 }],
  ['snow cell is lit', 11, 65, 8, { sky: 15, block: 0 }],
  ['glass cell is lit', 8, 69, 3, { sky: 15, block: 0 }],
  ['lime cell behind the glass', 8, 69, 1, { sky: 0, block: 0 }],
  ['other chunk', -5, 70, -5, { sky: 15, block: 0 }]
]

test('lightAt describes the lighting', () => {
  for (const [, x, y, z, want] of lights) assert.deepEqual(lightAt(x, y, z), want, `${x} ${y} ${z}`)
})

test('the four columns round trip through the file format with every cell set', () => {
  for (const [cx, cz] of [[0, 0], [0, -1], [-1, 0], [-1, -1]]) {
    const loaded = load(cx, cz)
    const { header, column } = loaded
    assert.equal(header.mcVersion, '26.1')
    assert.equal(header.minY, -64)
    assert.equal(header.worldHeight, 384)
    const inColumn = ([, x, , z]) => Math.floor(x / 16) === cx && Math.floor(z / 16) === cz
    for (const [, x, y, z] of [...blocks, ...lights].filter(inColumn)) {
      const local = { x: x - cx * 16, y, z: z - cz * 16 }
      assert.deepEqual(lightCell(loaded, local.x, y, local.z), lightAt(x, y, z), `light ${x} ${y} ${z}`)
      assert.equal(column.getBlock(local).name, blockNameAt(x, y, z), `block ${x} ${y} ${z}`)
    }
  }
})

test('the lowest and highest sections carry light data', () => {
  const loaded = load(-1, -1)
  for (const y of [-64, -1, 0, 319]) assert.equal(lightCell(loaded, 3, y, 3).sky, 15, `y ${y}`)
})

test('every cell of a whole row decodes to its intended light (vanilla nibble order)', () => {
  const loaded = load(0, 0)
  const row = Array.from({ length: 16 }, (_, x) => lightCell(loaded, x, 65, 8))
  assert.deepEqual(row, Array.from({ length: 16 }, (_, x) => lightAt(x, 65, 8)))
  assert.ok(row.some(c => c.block === 14) && row.some(c => c.block === 0))
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

test('the slab is a bottom slab and the snow has two layers', () => {
  const loaded = load(0, 0)
  assert.equal(loaded.column.getBlock({ x: 4, y: 65, z: 8 }).getProperties().type, 'bottom')
  assert.equal(loaded.column.getBlock({ x: 11, y: 65, z: 8 }).getProperties().layers, '2')
  assert.deepEqual(STATES, { oak_slab: { type: 'bottom' }, snow: { layers: 2 } })
})

const OLD_REGIONS = ['lit', 'dark', 'leaves', 'diamond', 'torch']
const overlap = (a, b) => a.x0 <= b.x1 && b.x0 <= a.x1 && a.y0 <= b.y1 && b.y0 <= a.y1

test('the slab and snow regions do not overlap the original ones or each other', () => {
  const basis = cameraBasis({ yaw: FIXTURE.yaw, pitch: FIXTURE.pitch, fov: 70 })
  const rects = Object.fromEntries(FIXTURE.regions.map(({ name, face }) => [name, faceRegion(basis, FIXTURE.eye, face, 640, 360)]))
  const added = FIXTURE.regions.map(r => r.name).filter(n => !OLD_REGIONS.includes(n) && n !== 'glass')
  assert.deepEqual(added, ['slab lower', 'slab upper', 'snow lower', 'snow upper'])
  const pairs = added.flatMap((a, i) => [...OLD_REGIONS, ...added.slice(i + 1)].map(b => [a, b]))
  for (const [a, b] of pairs) assert.ok(!overlap(rects[a], rects[b]), `${a} ${JSON.stringify(rects[a])} overlaps ${b} ${JSON.stringify(rects[b])}`)
})

test('the glass wall and its region do not overlap any other region', () => {
  const basis = cameraBasis({ yaw: FIXTURE.yaw, pitch: FIXTURE.pitch, fov: 70 })
  const rects = Object.fromEntries(FIXTURE.regions.map(({ name, face }) => [name, faceRegion(basis, FIXTURE.eye, face, 640, 360)]))
  const others = FIXTURE.regions.map(r => r.name).filter(n => n !== 'glass')
  assert.ok(FIXTURE.regions.some(r => r.name === 'glass'))
  for (const b of others) assert.ok(!overlap(rects.glass, rects[b]), `glass ${JSON.stringify(rects.glass)} overlaps ${b} ${JSON.stringify(rects[b])}`)
  // the whole glass wall (face rectangle without the inset) stays clear of the others too
  const wall = faceRegion(basis, FIXTURE.eye, { axis: 'z', at: 4, x: [7, 10], y: [69, 71] }, 640, 360)
  for (const b of others) assert.ok(!overlap(wall, rects[b]), `glass wall overlaps ${b}`)
})
