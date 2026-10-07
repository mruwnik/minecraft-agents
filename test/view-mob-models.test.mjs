// Why JavaScript: tests for the JS view stack (graphics tables and renderers).
// The simple textured mob models (tools/view/web/mob-models.mjs): which mobs have one, that every sheet region is in the client jar, how a model
// turns to a yaw, and how the software renderers and the WebGL view's tables use them.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { MOBS, BABIES, DYES, FACES, modelFor, modelLayers, sheetsOf, yawBasis, worldBox, faceUV, layerName } from '../tools/view/web/mob-models.mjs'
import { mobFor, mobPaint } from '../tools/view/mob-draw.mjs'
import { createMobImages } from '../tools/view/mob-textures.mjs'
import { speciesColored } from '../tools/view/web/scene.mjs'
import { modelUniforms, MAX_PART_ROWS } from '../tools/view/web/gl.mjs'
import { render, makeGrid } from '../tools/view/renderer.mjs'
import { openJar, findClientJar } from '../tools/view/jar-read.mjs'
import { textureBytes } from '../tools/view/materials.mjs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const WANTED = 'zombie husk drowned player piglin zombified_piglin skeleton stray wither_skeleton creeper cow mooshroom pig sheep spider cave_spider chicken villager wandering_trader witch enderman vindicator pillager evoker illusioner zombie_villager horse donkey mule skeleton_horse zombie_horse wolf cat ocelot fox iron_golem snow_golem blaze slime magma_cube llama trader_llama rabbit goat'.split(' ')
const textureDir = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'textures')
const jarPath = findClientJar()
const skip = jarPath === null
const close = (a, b) => assert.ok(Math.abs(a - b) < 1e-9, `${a} !~ ${b}`)

test('every mob the card lists has a model of several boxes with six faces each', () => {
  for (const name of WANTED) {
    assert.ok(name in MOBS, name)
    const model = modelFor({ name, height: 1 })
    assert.ok(model.parts.length >= 3, name)
    for (const p of model.parts) assert.equal(p.layers.length, FACES.length, name)
  }
})

test('a mob without a model has none', () => {
  assert.equal(modelFor({ name: 'ghast', height: 4 }), null)
  assert.equal(modelFor({ name: 'item', height: 0.25 }), null)
})

test('the layers the models need stay few: the array with the blocks must fit a GPU\'s 2048 layers', { skip }, () => {
  const total = textureBytes('26.1', textureDir, { jarPath }).table.textures.names.length
  assert.ok(total < 2000, `${total} layers, ${modelLayers().length} of them the models'`)
})

test('a model is scaled to the entity height, so a baby is smaller', () => {
  const top = e => modelFor(e).hull[4]
  close(top({ name: 'zombie', height: 1.95 }) / top({ name: 'zombie', height: 0.975 }), 2)
  assert.ok(Math.abs(top({ name: 'zombie', height: 1.95 }) - 1.95) < 1e-9)
})

test('every sheet a model reads is in the jar, and every region lies inside its sheet', { skip }, () => {
  const jar = openJar(jarPath)
  for (const name of Object.keys(MOBS)) {
    for (const sheet of [...sheetsOf(name), ...sheetsOf(name, true)]) assert.ok(jar.has(`assets/minecraft/textures/entity/${sheet}.png`), `${name}: ${sheet}`)
  }
  for (const layer of modelLayers()) {
    const [path, region] = layer.split('#')
    const png = jar.read(`assets/minecraft/textures/${path}.png`)
    const [w, h] = [png.readUInt32BE(16), png.readUInt32BE(20)]
    const [x, y, rw, rh] = region.split(',').map(Number)
    assert.ok(x >= 0 && y >= 0 && x + rw <= w && y + rh <= h && rw > 0 && rh > 0, `${layer} in ${w}x${h}`)
  }
})

test('layer names are the block entities\' entity/<sheet>#x,y,w,h', () => {
  assert.equal(layerName('zombie/zombie', [8, 8, 8, 8]), 'entity/zombie/zombie#8,8,8,8')
  for (const layer of modelLayers()) assert.match(layer, /^entity\/[\w/]+#\d+,\d+,\d+,\d+$/)
})

test('yaw: 0 faces -z, a quarter turn left faces -x, a half turn +z (mineflayer)', () => {
  for (const [yaw, forward, right] of [[0, [0, -1], [1, 0]], [Math.PI / 2, [-1, 0], [0, -1]], [Math.PI, [0, 1], [-1, 0]]]) {
    const b = yawBasis(yaw)
    close(b.forward.x, forward[0]); close(b.forward.z, forward[1])
    close(b.right.x, right[0]); close(b.right.z, right[1])
  }
})

test('a model turned to a yaw: its front part lands on the side the mob faces', () => {
  const frontHeadAt = yaw => {
    const model = modelFor({ name: 'cow', height: 1.4, yaw })
    const head = model.parts[1].box // the head sticks out of the front
    const [mx, mz] = [(head[0] + head[3]) / 2, (head[2] + head[5]) / 2]
    return [mx * model.right.x + mz * model.forward.x, mx * model.right.z + mz * model.forward.z]
  }
  const [x0, z0] = frontHeadAt(0)
  assert.ok(z0 < -0.3 && Math.abs(x0) < 1e-9, 'facing north the head is to the north')
  const [x1, z1] = frontHeadAt(Math.PI / 2)
  assert.ok(x1 < -0.3 && Math.abs(z1) < 1e-9, 'facing west the head is to the west')
  const [x2, z2] = frontHeadAt(Math.PI)
  assert.ok(z2 > 0.3 && Math.abs(x2) < 1e-9, 'facing south the head is to the south')
})

test('the box round a turned model holds its turned parts', () => {
  const model = modelFor({ name: 'cow', height: 1.4, yaw: Math.PI / 2 })
  const { min, max } = worldBox(model, { x: 10, y: 5, z: 20 })
  assert.ok(max[0] - min[0] > max[2] - min[2], 'a cow facing west is longer along x')
  close(min[1], 5)
  close(max[1], 5 + model.hull[4])
})

test('faceUV reads each face as seen from outside', () => {
  assert.deepEqual(faceUV('south', 0.25, 0.25, 0), [0.75, 0.75])
  assert.deepEqual(faceUV('north', 0.25, 0.25, 1), [0.25, 0.75])
  assert.deepEqual(faceUV('east', 1, 0.5, 0.25), [0.25, 0.5])
  assert.deepEqual(faceUV('west', 0, 0.5, 0.25), [0.75, 0.5])
  assert.deepEqual(faceUV('top', 0.25, 1, 0.25), [0.25, 0.75])
  assert.deepEqual(faceUV('bottom', 0.25, 0, 0.25), [0.25, 0.25])
})

// ---------------------------------------------------------------- software renderers
const solid = (r, g, b) => ({ width: 16, height: 16, rgba: Uint8Array.from({ length: 16 * 16 * 4 }, (_, i) => [r, g, b, 255][i % 4]) })

test('mobFor: a mob with a model is drawn from its parts with their layers, any other from its family', () => {
  const eye = { x: 0, y: 1, z: 5 }
  const zombie = mobFor({ name: 'zombie', x: 0, y: 0, z: 0, width: 0.6, height: 1.95, yaw: 0 }, eye)
  assert.equal(zombie.parts[0].length, 9)
  assert.equal(zombie.parts[0][7].length, 6)
  const ghast = mobFor({ name: 'ghast', x: 0, y: 0, z: 0, width: 4, height: 4, yaw: 0 }, eye)
  assert.equal(ghast.parts[0].length, 7)
})

test('mobPaint: the sheet picture where a ray hits a textured part, the flat palette without pictures', () => {
  const eye = { x: 0, y: 1, z: 5 }
  const m = mobFor({ name: 'creeper', x: 0, y: 0, z: 0, width: 0.6, height: 1.7, yaw: Math.PI }, eye)
  const part = m.parts[0]
  const local = { x: 0, y: 0, z: -1 }
  const t = 4
  const withPictures = mobPaint(m, part, 'south', t, local, () => solid(10, 200, 30))
  assert.deepEqual(withPictures, [10, 200, 30])
  assert.deepEqual(mobPaint(m, part, 'south', t, local, null), m.palette[part[6] === 1 ? 3 : part[6]])
  assert.deepEqual(mobPaint(m, part, 'south', t, local, () => null), m.palette[part[6] === 1 ? 3 : part[6]])
})

test('createMobImages: a layer is the sheet region as 16x16, null when the sheet is missing', () => {
  const opened = { has: () => false, read: () => null }
  assert.equal(createMobImages(opened)('entity/zombie/zombie#0,0,8,8'), null)
})

test('createMobImages with the client jar reads a creeper face', { skip }, () => {
  const image = createMobImages()('entity/creeper/creeper#8,8,8,8')
  assert.equal(image.width, 16)
  assert.equal(image.rgba.length, 16 * 16 * 4)
  assert.ok(image.rgba.some(v => v !== 0))
})

// a floor with a mob three blocks north of the eye: the picture handed to render() is what the mob's head shows
test('render: a model mob shows the sheet picture, turning with its yaw', () => {
  const grid = makeGrid({ x: -8, y: -3, z: -8 }, { x: 16, y: 8, z: 16 })
  for (let x = -8; x < 8; x++) for (let z = -8; z < 8; z++) grid.set(x, -2, z, 1)
  const scene = { grid, info: id => [null, { kind: 'cube', name: 'floor' }][id], texture: () => solid(0, 90, 0), eye: { x: 0.5, y: 0.5, z: 0.5 }, timeOfDay: 6000, yaw: 0, pitch: 0, width: 64, height: 64, fov: 90, maxDist: 12 }
  const at = (img, x, y) => [...img.rgba.slice((y * 64 + x) * 4, (y * 64 + x) * 4 + 3)].join()
  const mob = yaw => ({ name: 'zombie', x: 0.5, y: -1, z: -2.5, width: 0.6, height: 1.95, yaw })
  const pictures = layer => layer.includes('#8,8,8,8') ? solid(250, 0, 250) : solid(0, 0, 250)
  const facing = render({ ...scene, entities: [mob(Math.PI)], mobPictures: pictures })
  const away = render({ ...scene, entities: [mob(0)], mobPictures: pictures })
  const flat = render({ ...scene, entities: [mob(Math.PI)], mobPictures: null })
  const head = [32, 29]
  assert.notEqual(at(facing, ...head), at(away, ...head))
  assert.notEqual(at(facing, ...head), at(flat, ...head))
  assert.ok(facing.seen.some(s => s.name === 'zombie'))
})

// ---------------------------------------------------------------- the WebGL view
test('speciesColored: a mob with a model carries it and a box round the turned model, others keep their box', () => {
  const [cow, ghast] = speciesColored([
    { min: [9.55, 64, 19.55], max: [10.45, 65.4, 20.45], name: 'cow', yaw: Math.PI / 2 },
    { min: [0, 0, 0], max: [4, 4, 4], name: 'ghast', yaw: 0 }
  ])
  assert.equal(cow.model.parts.length, modelFor({ name: 'cow', height: 1.4 }).parts.length)
  assert.deepEqual(cow.model.origin, [10, 64, 20])
  assert.ok(cow.max[0] - cow.min[0] > cow.max[2] - cow.min[2])
  assert.equal(ghast.model, undefined)
  assert.deepEqual(ghast.max, [4, 4, 4])
})

test('modelUniforms: rows of 16 floats a part, the layer indices from the array, rot and origin per mob', () => {
  const names = modelLayers()
  const layerIndex = new Map(names.map((n, i) => [n, i]))
  const [creeper] = speciesColored([{ min: [0, 0, 0], max: [0.6, 1.7, 0.6], name: 'creeper', yaw: 0 }])
  const { rot, org, rows } = modelUniforms([creeper, { min: [0, 0, 0], max: [1, 1, 1], name: 'ghast' }], layerIndex)
  const parts = creeper.model.parts.length
  assert.equal(rows.length, parts * 16)
  assert.deepEqual([...rot.slice(0, 4)], [1, -0, 0, parts])
  assert.deepEqual([...rot.slice(4, 8)], [0, 0, 0, 0])
  assert.deepEqual([...org.slice(0, 3)].map(v => Math.round(v * 100) / 100), [0.3, 0, 0.3])
  assert.deepEqual([...rows.slice(8, 14)], [...creeper.model.parts[0].layers.map(l => layerIndex.get(l)), 0, 0].slice(0, 6))
})

test('modelUniforms: a model whose layers are not in the array stays a plain box', () => {
  const [cow] = speciesColored([{ min: [0, 0, 0], max: [0.9, 1.4, 0.9], name: 'cow', yaw: 0 }])
  const { rot, rows } = modelUniforms([cow], new Map())
  assert.deepEqual([...rot.slice(0, 4)], [0, 0, 0, 0])
  assert.equal(rows.length, 0)
})

test('modelUniforms: 64 mobs of the biggest model fit the table', () => {
  const layerIndex = new Map(modelLayers().map((n, i) => [n, i]))
  const many = speciesColored(Array.from({ length: 64 }, () => ({ min: [0, 0, 0], max: [0.6, 1.95, 0.6], name: 'witch', yaw: 0 })))
  const { rot, rows } = modelUniforms(many, layerIndex)
  assert.ok(rows.length / 16 <= MAX_PART_ROWS)
  assert.deepEqual([rot[3] > 0, rot[4 * 63 + 3] > 0], [true, true])
})

test('a dyed sheep tints its wool body only; an undyed or other mob carries no tint', () => {
  const red = modelFor({ name: 'sheep', height: 1.3, dye: 14 })
  assert.deepEqual(red.parts.map(p => p.tint ?? null), [DYES[14], null, null, null, null, null])
  assert.ok(modelFor({ name: 'sheep', height: 1.3 }).parts.every(p => !p.tint))
  assert.ok(modelFor({ name: 'cow', height: 1.4, dye: 14 }).parts.every(p => !p.tint))
})

test('the dye table has sixteen colours, white first', () => {
  assert.equal(DYES.length, 16)
  assert.ok(DYES[0].every(v => v > 240))
})

test('a baby is half the height of an adult with a bigger head for its body', () => {
  const look = e => {
    const m = modelFor({ name: 'cow', height: 1.4, ...e })
    const head = m.parts.find(p => p.paint === 1)
    return { top: m.hull[4], headRatio: (head.box[4] - head.box[1]) / m.hull[4] }
  }
  const [adult, baby] = [look({}), look({ baby: true })]
  assert.ok(baby.top < 0.8 * adult.top && baby.top > 0.4 * adult.top)
  assert.ok(baby.headRatio > 1.2 * adult.headRatio)
})

test('a baby with its own sheet layout is drawn from the baby sheet, not the adult\'s', () => {
  for (const [name, [, sheet]] of Object.entries(BABIES)) {
    const layers = modelFor({ name, height: 1, baby: true }).parts.flatMap(p => p.layers)
    assert.ok(layers.some(l => l.startsWith(`entity/${sheet}#`)), name)
    assert.ok(layers.every(l => l.includes('_baby#')), `${name}: every region is from a baby sheet`)
  }
})

test('the baby zombie\'s head reads the 6x6x6 head net at (3, 3) of its sheet, and its size is the baby\'s own (no 1.5x head grown on the adult)', () => {
  const m = modelFor({ name: 'zombie', height: 1.95, baby: true })
  const head = m.parts.find(p => p.paint === 1)
  assert.equal(head.layers[FACES.indexOf('south')], 'entity/zombie/zombie_baby#9,9,6,6')
  close(head.box[3] - head.box[0], 6 * 1.95 / 32)
  assert.ok(m.hull[4] < 0.55 * 1.95)
})

test('a baby of a mob without a baby layout still reads the adult sheet, half height with a bigger head', () => {
  const layers = modelFor({ name: 'wolf', height: 0.85, baby: true }).parts.flatMap(p => p.layers)
  assert.ok(layers.every(l => l.startsWith('entity/wolf/wolf#')))
})

test('mobPaint: a tinted part multiplies its sheet picture by the dye', () => {
  const eye = { x: 0, y: 1, z: 5 }
  const m = mobFor({ name: 'sheep', x: 0, y: 0, z: 0, width: 0.9, height: 1.3, yaw: Math.PI, dye: 14 }, eye)
  const part = m.parts[0]
  const local = { x: 0, y: 0, z: -1 }
  assert.deepEqual(mobPaint(m, part, 'south', 4, local, () => solid(200, 100, 50)), [200 * DYES[14][0] / 255, 100 * DYES[14][1] / 255, 50 * DYES[14][2] / 255].map(Math.round))
  assert.deepEqual(mobPaint(m, m.parts[1], 'south', 4, local, () => solid(200, 100, 50)), [200, 100, 50])
})

test('modelUniforms: a tinted part packs its dye as r * 65536 + g * 256 + b in the 15th float, others 0', () => {
  const layerIndex = new Map(modelLayers().map((n, i) => [n, i]))
  const [sheep] = speciesColored([{ min: [0, 0, 0], max: [0.9, 1.3, 0.9], name: 'sheep', yaw: 0, dye: 14 }])
  const { rows } = modelUniforms([sheep], layerIndex)
  const [r, g, b] = DYES[14]
  assert.equal(rows[14], r * 65536 + g * 256 + b)
  assert.equal(rows[16 + 14], 0)
})

test('speciesColored passes baby and dye on to the model', () => {
  const [lamb] = speciesColored([{ min: [0, 0, 0], max: [0.45, 0.65, 0.45], name: 'sheep', yaw: 0, dye: 2, baby: true }])
  assert.deepEqual(lamb.model.parts[0].tint, DYES[2])
})
