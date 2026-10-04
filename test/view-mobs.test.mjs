// Mob names and colours (tools/view/web/mobs.mjs) and the name labels drawn over them (tools/view/labels.mjs, the software
// renderers) must tell mobs apart: a colour of its own per species, a readable label that is hidden behind terrain.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { SPECIES, hasPalette, paletteFor, colorFor, labelFor, placeLabels, projectorFor, labelWidth, canSee } from '../tools/view/web/mobs.mjs'
import { cameraBasis } from '../tools/view/web/camera.mjs'
import { drawLabels } from '../tools/view/labels.mjs'
import { boxLabels, labelUniforms } from '../tools/view/web/gl.mjs'
import { makeGrid, render } from '../tools/view/renderer.mjs'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { makeBlockSource } from '../tools/view/blocks.mjs'

const dist = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1], a[2] - b[2])

test('every species has a palette of its own', () => {
  assert.deepEqual(SPECIES.filter(name => !hasPalette(name)), [])
})

const palettesApart = (a, b) => [0, 1, 2, 3].reduce((sum, i) => sum + dist(paletteFor({ name: a })[i], paletteFor({ name: b })[i]), 0)

test('no two species look alike: their four colours are at least 60 apart, summed', () => {
  const close = []
  for (let i = 0; i < SPECIES.length; i++) {
    for (let j = i + 1; j < SPECIES.length; j++) {
      if (palettesApart(SPECIES[i], SPECIES[j]) < 60) close.push(`${SPECIES[i]} ${SPECIES[j]}`)
    }
  }
  assert.deepEqual(close, [])
})

// the mobs met together most often (a village, a farm, a night in the open) differ in body colour alone, as a snapshot is small
const COMMON = 'player zombie skeleton creeper spider enderman villager cow pig sheep cat fox witch pillager blaze iron_golem horse'.split(' ')
test('the common mobs differ in body colour by at least 40', () => {
  const close = []
  for (let i = 0; i < COMMON.length; i++) {
    for (let j = i + 1; j < COMMON.length; j++) {
      if (dist(colorFor({ name: COMMON[i] }), colorFor({ name: COMMON[j] })) < 40) close.push(`${COMMON[i]} ${COMMON[j]}`)
    }
  }
  assert.deepEqual(close, [])
})

test('an unknown mob gets a stable colour of its own, a hostile one red', () => {
  assert.deepEqual(colorFor({ name: 'new_mob' }), colorFor({ name: 'new_mob' }))
  assert.notDeepEqual(colorFor({ name: 'new_mob' }), colorFor({ name: 'other_mob' }))
  assert.deepEqual(paletteFor({ name: 'new_mob', kind: 'hostile' })[0], [225, 35, 35])
})

const labels = [
  [{ name: 'zombie' }, 'zombie'],
  [{ name: 'zombie_villager' }, 'zombie villager'],
  [{ name: 'player', kind: 'player', label: 'Bob_2' }, 'Bob_2'],
  [{ name: 'player', kind: 'player', label: 'Alex' }, 'Alex'],
  [{ name: 'cow', label: 'cow' }, 'cow']
]
for (const [entity, expected] of labels) test(`labelFor ${JSON.stringify(entity)}`, () => assert.equal(labelFor(entity), expected))

const [W, H] = [160, 90]
const basis = cameraBasis({ yaw: 0, pitch: 0, fov: 70 })
const eye = { x: 0, y: 65, z: 0 }
const place = entities => placeLabels({ eye, project: projectorFor(basis, W, H), width: W, height: H, entities })
const mob = (name, x, z, extra = {}) => ({ name, x, y: 64, z, width: 0.6, height: 1.8, ...extra })

test('a mob ahead gets a label centred over its head, above it', () => {
  const [one] = place([mob('zombie', 0, -6)])
  assert.equal(one.text, 'zombie')
  assert.ok(Math.abs(one.px - W / 2) < 1)
  assert.ok(one.py < H / 2)
  assert.ok(one.depth > 4 && one.depth < 7)
})

test('a mob behind the camera, an item and a mob off the side get no label', () => {
  assert.deepEqual(place([mob('zombie', 0, 6), mob('item', 0, -4, { kind: 'item' }), mob('cow', 40, -2)]), [])
})

test('labels shrink with distance but stay readable, and the nearest come first', () => {
  const found = placeLabels({ eye, project: projectorFor(basis, 640, 360), width: 640, height: 360, entities: [mob('cow', 0.4, -40), mob('zombie', -0.4, -3), mob('pig', 0, -12)] })
  assert.deepEqual(found.map(l => l.text), ['zombie', 'pig', 'cow'])
  assert.ok(found[0].h > found[1].h && found[1].h >= found[2].h)
  assert.ok(found[2].h >= 9)
})

test('at most 16 labels', () => {
  const crowd = Array.from({ length: 30 }, (_, i) => mob('pig', (i % 5) * 0.2 - 0.4, -3 - i))
  assert.equal(place(crowd).length, 16)
})

const blank = () => ({ rgba: new Uint8Array(W * H * 4).fill(40), depth: new Float64Array(W * H).fill(64) })
const whites = ({ rgba }) => { let n = 0; for (let i = 0; i < rgba.length; i += 4) if (rgba[i] === 255 && rgba[i + 1] === 255 && rgba[i + 2] === 255) n++; return n }

test('a label draws white text on a plate, centred, and clips at the picture edge', () => {
  const picture = blank()
  const [one] = place([mob('zombie', 0, -6)])
  drawLabels({ ...picture, width: W, height: H, labels: [one] })
  assert.ok(whites(picture) > 30)
  const edge = blank()
  drawLabels({ ...edge, width: W, height: H, labels: [{ ...one, px: 0, py: 3 }] })
  assert.ok(whites(edge) > 0)
})

test('terrain nearer than the mob hides the label; terrain behind it does not', () => {
  const [one] = place([mob('zombie', 0, -6)])
  const hidden = blank()
  hidden.depth.fill(3)
  drawLabels({ ...hidden, width: W, height: H, labels: [one] })
  assert.equal(whites(hidden), 0)
  assert.ok(hidden.rgba.every(v => v === 40 || v === 255))
  const shown = blank()
  shown.depth.fill(30)
  drawLabels({ ...shown, width: W, height: H, labels: [one] })
  assert.ok(whites(shown) > 30)
})

test('half the label behind a wall draws half', () => {
  const [one] = place([mob('zombie', 0, -6)])
  const full = blank()
  drawLabels({ ...full, width: W, height: H, labels: [one] })
  const half = blank()
  for (let y = 0; y < H; y++) for (let x = 0; x < W / 2; x++) half.depth[y * W + x] = 3
  drawLabels({ ...half, width: W, height: H, labels: [one] })
  assert.ok(whites(half) < whites(full) * 0.7 && whites(half) > whites(full) * 0.3)
})

test('text is wider for longer names', () => assert.ok(labelWidth('zombie villager', 14) > labelWidth('cow', 14)))

const Chunk = makeChunkClass('1.21.4')
const state = name => Chunk.registry.blocksByName[name].defaultState
const wallWorld = () => {
  const grid = makeGrid({ x: -16, y: 50, z: -16 }, { x: 32, y: 24, z: 32 })
  for (let x = -16; x < 16; x++) for (let z = -16; z < 16; z++) grid.set(x, 63, z, state('stone'))
  for (let x = -6; x <= 6; x++) for (let y = 64; y <= 70; y++) grid.set(x, y, -5, state('stone'))
  grid.top = 70
  return grid
}
const flat = makeBlockSource(Chunk.registry, path.join(import.meta.dirname, 'no-such-textures'))
const scene = { grid: wallWorld(), eye: { x: 0.5, y: 65.6, z: 4.5 }, timeOfDay: 6000, width: 160, height: 90, maxDist: 64, yaw: 0, pitch: 0, fov: 70, ...flat }
const labelPixels = (out, out2) => { let n = 0; for (let i = 0; i < out.rgba.length; i += 4) if (out.rgba[i] !== out2.rgba[i] || out.rgba[i + 1] !== out2.rgba[i + 1]) n++; return n }

test('render: the mob in the open has a label, the one behind the wall has none (the picture is the same without the name)', () => {
  const open = { name: 'zombie', kind: 'hostile', x: 2.5, y: 64, z: 0.5, width: 0.6, height: 1.95, yaw: 0 }
  const behind = { ...open, z: -9.5 }
  const withName = render({ ...scene, entities: [open] })
  const noName = render({ ...scene, entities: [{ ...open, label: '' }] })
  assert.ok(labelPixels(withName, noName) > 20)
  const hiddenNamed = render({ ...scene, entities: [behind] })
  const hiddenPlain = render({ ...scene, entities: [{ ...behind, label: '' }] })
  assert.equal(labelPixels(hiddenNamed, hiddenPlain), 0)
})

// ---- the WebGL view: boxes in, shader uniforms out

const box = (name, x, z, extra = {}) => ({ min: [x - 0.3, 64, z - 0.3], max: [x + 0.3, 65.8, z + 0.3], color: [0.5, 0.5, 0.5], name, ...extra })

test('boxLabels: a box becomes a label over its top, the same one placeLabels gives for the mob', () => {
  const [fromBox] = boxLabels({ boxes: [box('zombie', 0, -6)], eye, basis, width: W, height: H, dist: 64 })
  const [fromMob] = place([mob('zombie', 0, -6)])
  assert.deepEqual(fromBox, fromMob)
})

test('boxLabels: a drop and a mob beyond the draw distance get none', () => {
  assert.deepEqual(boxLabels({ boxes: [box('item', 0, -4, { kind: 'item' }), box('cow', 0, -50)], eye, basis, width: W, height: H, dist: 32 }), [])
})

test('labelUniforms: pixel rects from the bottom left, the label height tall, and depths, padded to 16', () => {
  const [one] = place([mob('zombie', 0, -6)])
  const u = labelUniforms([one], H)
  assert.equal(u.count, 1)
  assert.equal(u.rects.length, 64)
  assert.equal(u.depths.length, 16)
  const [x0, y0, x1, y1] = u.rects
  assert.ok(Math.abs(y1 - y0 - one.h) < 1e-4)
  assert.ok(Math.abs((x0 + x1) / 2 - one.px) < 1e-3)
  assert.ok(Math.abs(y0 - (H - one.py)) < 1e-4)
  assert.ok(Math.abs(x1 - x0 - labelWidth('zombie', one.h)) < 1e-3)
  assert.ok(Math.abs(u.depths[0] - one.depth) < 1e-4)
  assert.equal(u.ats.length, 48)
  assert.deepEqual([...u.ats.slice(0, 3)].map(v => Math.round(v * 100) / 100), [0, 64.9, -6]) // the mob's middle, which the shader reads the light at
  assert.ok(Math.abs(u.dists[0] - one.dist) < 1e-4)
})

test('render: a mob in front of another hides that one\'s label', () => {
  const near = { name: 'slime', kind: 'hostile', x: 0.5, y: 64, z: -1.5, width: 3, height: 3.5, yaw: 0 }
  const far = { name: 'cow', x: 0.5, y: 64, z: -3.5, width: 0.9, height: 1.4, yaw: 0 }
  const picture = labelsOf => render({ ...scene, eye: { x: 0.5, y: 65.6, z: 2.5 }, entities: labelsOf })
  const both = picture([near, { ...far }])
  const farUnnamed = picture([near, { ...far, label: '' }])
  // the cow is straight behind a big slime: its label plate is behind the slime's body, so naming it changes nothing visible
  assert.equal(labelPixels(both, farUnnamed), 0)
})

// ---- no label for a mob the body could not see: lit enough (seeing 0.2, about block light 2) or within 2 blocks
test('canSee: bright enough or close', () => {
  assert.equal(canSee(0.099, 5), false)
  assert.equal(canSee(0.099, 2), true)
  assert.equal(canSee(0.2, 5), true)
  assert.equal(canSee(0.199, 2.5), false)
})

const darkScene = (light, entities) => {
  const grid = wallWorld()
  grid.light.fill(light)
  return render({ ...scene, grid, entities })
}
const open = { name: 'zombie', kind: 'hostile', x: 2.5, y: 64, z: 0.5, width: 0.6, height: 1.95, yaw: 0 }
const close = { ...open, x: 0.5, z: 2.8 }
const labelDifference = (light, e) => labelPixels(darkScene(light, [e]), darkScene(light, [{ ...e, label: '' }]))

test('render: in the dark a far mob gets no label, a near one does, and a torch-lit one does', () => {
  assert.equal(labelDifference(0, open), 0)
  assert.ok(labelDifference(0, close) > 20)
  assert.ok(labelDifference(14, open) > 20)
})
