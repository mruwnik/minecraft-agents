// Baking block states into model elements from the game's own blockstate and model JSON: variant and multipart selection,
// parent chains, texture variables, the blockstate x/y rotation with and without uvlock, and a run over the real 26.1 registry
// against the 1.21.8 jar.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import prismarineRegistry from 'prismarine-registry'
import { bakeState, bakeAll, classify } from '../tools/view/block-bake.mjs'
import { loadModels } from '../tools/view/block-models.mjs'
import { findClientJar, zipEntries } from '../tools/view/jar-read.mjs'

const jar = (blockstates, models) => ({ blockstates: new Map(Object.entries(blockstates)), models: new Map(Object.entries(models)) })
const face = (texture, extra = {}) => ({ texture, ...extra })
const all = texture => Object.fromEntries(['up', 'down', 'north', 'south', 'east', 'west'].map(d => [d, face(texture)]))

// ---- variant matching ----

const variantJar = jar({
  thing: {
    variants: {
      'facing=north,half=bottom': { model: 'minecraft:block/nb' },
      'facing=north,half=top': [{ model: 'block/nt', weight: 3 }, { model: 'block/other' }],
      'facing=east': { model: 'block/e' }
    }
  },
  always: { variants: { '': { model: 'block/nb' } } }
}, {
  'block/nb': { elements: [{ from: [0, 0, 0], to: [16, 16, 16], faces: all('#t') }], textures: { t: 'block/nb_tex' } },
  'block/nt': { elements: [{ from: [0, 0, 0], to: [16, 8, 16], faces: all('#t') }], textures: { t: 'block/nt_tex' } },
  'block/e': { elements: [] }
})

for (const [props, textured] of [
  [{ facing: 'north', half: 'bottom', open: false }, 'nb_tex'],
  [{ facing: 'north', half: 'top', open: true }, 'nt_tex']
]) {
  test(`variant matches a subset of the props: ${JSON.stringify(props)}`, () => {
    assert.equal(bakeState({ name: 'thing', props }, variantJar).elements[0].faces.up.texture, textured)
  })
}

test('a variant with no model elements bakes to no elements, and "" always matches', () => {
  assert.deepEqual(bakeState({ name: 'thing', props: { facing: 'east', half: 'top' } }, variantJar).elements, [])
  assert.equal(bakeState({ name: 'always', props: { anything: 1 } }, variantJar).elements.length, 1)
})

test('a name with no blockstate bakes to null', () => {
  assert.equal(bakeState({ name: 'nothing', props: {} }, variantJar), null)
})

// ---- multipart ----

const multipartJar = jar({
  fence: {
    multipart: [
      { apply: { model: 'block/post' } },
      { when: { north: 'true' }, apply: { model: 'block/side' } },
      { when: { OR: [{ east: 'true' }, { west: 'true' }] }, apply: { model: 'block/side', y: 90 } },
      { when: { AND: [{ up: 'true' }, { kind: 'a|b' }] }, apply: { model: 'block/cap' } }
    ]
  }
}, {
  'block/post': { elements: [{ from: [6, 0, 6], to: [10, 16, 10], faces: { up: face('#t') } }], textures: { t: 'block/p' } },
  'block/side': { elements: [{ from: [7, 0, 0], to: [9, 16, 7], faces: { north: face('#t', { cullface: 'north' }) } }], textures: { t: 'block/p' } },
  'block/cap': { elements: [{ from: [0, 16, 0], to: [16, 17, 16], faces: { up: face('#t') } }], textures: { t: 'block/p' } }
})

for (const [props, count] of [
  [{ north: false, east: false, west: false, up: false, kind: 'a' }, 1],
  [{ north: true, east: false, west: false, up: false, kind: 'a' }, 2],
  [{ north: false, east: false, west: true, up: false, kind: 'a' }, 2],
  [{ north: true, east: true, west: true, up: false, kind: 'a' }, 3],
  [{ north: false, east: false, west: false, up: true, kind: 'a' }, 2],
  [{ north: false, east: false, west: false, up: true, kind: 'b' }, 2],
  [{ north: false, east: false, west: false, up: true, kind: 'c' }, 1]
]) {
  test(`multipart OR / AND / pipe: ${JSON.stringify(props)} gives ${count} elements`, () => {
    assert.equal(bakeState({ name: 'fence', props }, multipartJar).elements.length, count)
  })
}

test('multipart applies each part rotation: the east part is the north side turned by y=90', () => {
  const { elements } = bakeState({ name: 'fence', props: { north: false, east: true, west: false, up: false, kind: 'a' } }, multipartJar)
  assert.deepEqual([elements[1].from, elements[1].to], [[9, 0, 7], [16, 16, 9]])
  assert.deepEqual(Object.keys(elements[1].faces), ['east'])
  assert.equal(elements[1].faces.east.cullface, 'east')
})

// ---- parent chain and texture variables ----

test('parents give elements, textures merge with the child winning, #vars resolve through hops, prefixes strip', () => {
  const models = jar({ b: { variants: { '': { model: 'block/child' } } } }, {
    'block/child': { parent: 'minecraft:block/mid', textures: { all: '#base', particle: '#base' } },
    'block/mid': { parent: 'block/grand', textures: { base: 'minecraft:block/stone', all: 'block/ignored' } },
    'block/grand': { elements: [{ from: [0, 0, 0], to: [16, 16, 16], faces: { up: face('#all'), north: face('#missing') } }] }
  })
  const { elements } = bakeState({ name: 'b', props: {} }, models)
  assert.equal(elements[0].faces.up.texture, 'stone')
  assert.equal(elements[0].faces.north.texture, null)
})

test('a texture entry may be an object with a sprite (26.1 models)', () => {
  const models = jar({ b: { variants: { '': { model: 'block/g' } } } }, {
    'block/g': { textures: { all: { force_translucent: true, sprite: 'minecraft:block/glass' }, alias: '#all' }, elements: [{ from: [0, 0, 0], to: [16, 16, 16], faces: { up: face('#all'), down: face('#alias'), north: face({ sprite: 'block/x' }) } }] }
  })
  const { faces } = bakeState({ name: 'b', props: {} }, models).elements[0]
  assert.deepEqual([faces.up.texture, faces.down.texture], ['glass', 'glass'])
})

test('a bare texture name that is a variable key resolves like #name (heavy_core in the jar)', () => {
  const models = jar({ b: { variants: { '': { model: 'block/h' } } } }, {
    'block/h': { textures: { all: 'block/heavy_core' }, elements: [{ from: [4, 0, 4], to: [12, 8, 12], faces: { up: face('all') } }] }
  })
  assert.equal(bakeState({ name: 'b', props: {} }, models).elements[0].faces.up.texture, 'heavy_core')
})

// ---- rotation ----

const stairJar = (extra = {}) => jar({ stairs: { variants: { '': { model: 'block/stairs', ...extra } } } }, {
  'block/stairs': {
    textures: { t: 'block/planks' },
    elements: [
      { from: [0, 0, 0], to: [16, 8, 16], faces: { up: face('#t'), down: face('#t', { cullface: 'down' }) } },
      { from: [8, 8, 0], to: [16, 16, 16], faces: { up: face('#t', { tintindex: 0 }), east: face('#t', { cullface: 'east' }), north: face('#t', { uv: [0, 0, 8, 8], rotation: 90 }) } }
    ]
  }
})

test('y=90 turns a stair step: from/to and face directions, cullface and tint', () => {
  const step = bakeState({ name: 'stairs', props: {} }, stairJar({ y: 90 })).elements[1]
  assert.deepEqual([step.from, step.to], [[0, 8, 8], [16, 16, 16]])
  assert.deepEqual(Object.keys(step.faces).sort(), ['east', 'south', 'up'])
  assert.equal(step.faces.south.cullface, 'south')
  assert.equal(step.faces.up.tintindex, 0)
  assert.equal(step.faces.east.tintindex, -1)
  assert.equal(step.faces.east.cullface, null)
  assert.equal(step.rotation, null)
})

for (const [turn, from, to] of [
  [{ y: 180 }, [0, 8, 0], [8, 16, 16]],
  [{ y: 270 }, [0, 8, 0], [16, 16, 8]],
  [{ x: 180 }, [8, 0, 0], [16, 8, 16]]
]) {
  test(`${JSON.stringify(turn)} puts the step at ${from}..${to}`, () => {
    const step = bakeState({ name: 'stairs', props: {} }, stairJar(turn)).elements[1]
    assert.deepEqual([step.from, step.to], [from, to])
  })
}

// ---- uv ----

const uvJar = (face, extra = {}) => jar({ u: { variants: { '': { model: 'block/u', ...extra } } } }, {
  'block/u': { textures: { t: 'block/t' }, elements: [{ from: [0, 0, 0], to: [16, 16, 8], faces: { up: face } }] }
})

test('default uv is the element projected on the face (vanilla rule)', () => {
  const models = jar({ u: { variants: { '': { model: 'block/u' } } } }, {
    'block/u': { textures: { t: 'block/t' }, elements: [{ from: [4, 0, 2], to: [12, 8, 10], faces: Object.fromEntries(['up', 'down', 'north', 'south', 'east', 'west'].map(d => [d, face('#t')])) }] }
  })
  const faces = bakeState({ name: 'u', props: {} }, models).elements[0].faces
  assert.deepEqual(Object.fromEntries(Object.entries(faces).map(([d, f]) => [d, f.uv])), {
    up: [4, 2, 12, 10], down: [4, 6, 12, 14], north: [4, 8, 12, 16], south: [4, 8, 12, 16], east: [6, 8, 14, 16], west: [2, 8, 10, 16]
  })
})

test('without uvlock a turned up face keeps its uv and turns its texture', () => {
  const up = bakeState({ name: 'u', props: {} }, uvJar(face('#t'), { y: 90 })).elements[0].faces.up
  assert.deepEqual(up.uv, [0, 0, 16, 8])
  assert.equal(up.rotation, 90)
})

test('uvlock keeps the up face aligned to the world axes', () => {
  const up = bakeState({ name: 'u', props: {} }, uvJar(face('#t'), { y: 90, uvlock: true })).elements[0].faces.up
  assert.deepEqual(up.uv, [8, 0, 16, 16])
  assert.equal(up.rotation, 0)
})

test('uvlock leaves side faces and unrotated faces alone', () => {
  const models = jar({ u: { variants: { '': { model: 'block/u', y: 90, uvlock: true } } } }, {
    'block/u': { textures: { t: 'block/t' }, elements: [{ from: [0, 0, 0], to: [16, 16, 8], faces: { north: face('#t', { uv: [1, 2, 3, 4], rotation: 180 }) } }] }
  })
  const east = bakeState({ name: 'u', props: {} }, models).elements[0].faces.east
  assert.deepEqual([east.uv, east.rotation], [[1, 2, 3, 4], 180])
})

// ---- cross, classify ----

test('a cross keeps its 45 degree element rotation, moved by the model rotation', () => {
  const models = jar({ c: { variants: { 'k=1': { model: 'block/c', x: 90 }, '': { model: 'block/c' } } } }, {
    'block/c': { textures: { t: 'block/c' }, elements: [{ from: [0.8, 0, 8], to: [15.2, 16, 8], rotation: { origin: [8, 8, 8], axis: 'y', angle: 45, rescale: true }, faces: { north: face('#t'), south: face('#t') } }] }
  })
  assert.deepEqual(bakeState({ name: 'c', props: {} }, models).elements[0].rotation, { origin: [8, 8, 8], axis: 'y', angle: 45, rescale: true })
  const turned = bakeState({ name: 'c', props: { k: 1 } }, models).elements[0]
  assert.deepEqual(turned.rotation, { origin: [8, 8, 8], axis: 'z', angle: -45, rescale: true })
  assert.equal(classify({ elements: [turned] }), 'model')
})

test('leaf_litter-like multipart: segment_amount and facing pick 1 to 4 flat elements', () => {
  const flat = n => ({ elements: [{ from: [n, 0.1, 0], to: [n + 8, 0.1, 8], faces: { up: face('#t') } }], textures: { t: 'block/leaf_litter' } })
  const part = (amounts, model, y) => ({ when: { AND: [{ facing: 'east' }, { segment_amount: amounts }] }, apply: { model, y } })
  const models = jar({ leaf_litter: { multipart: [part('1|2|3|4', 'block/s1', 90), part('2|3|4', 'block/s2', 90), part('3|4', 'block/s3', 90), part('4', 'block/s4', 90)] } },
    { 'block/s1': flat(0), 'block/s2': flat(8), 'block/s3': flat(16), 'block/s4': flat(24) })
  for (const [amount, facing, count] of [[1, 'east', 1], [2, 'east', 2], [3, 'east', 3], [4, 'east', 4], [3, 'north', 0]]) {
    assert.equal(bakeState({ name: 'leaf_litter', props: { segment_amount: amount, facing } }, models).elements.length, count)
  }
  const { elements, source } = bakeState({ name: 'leaf_litter', props: { segment_amount: 3, facing: 'east' } }, models)
  assert.ok(elements.every(e => e.from[1] === 0.1 && e.to[1] === 0.1 && e.faces.up.texture === 'leaf_litter'))
  assert.match(source, /multipart/)
})

test('a cube keeps per-face textures: dispenser front on the facing side, furnace_top on up', () => {
  const models = jar({ dispenser: { variants: { 'facing=north': { model: 'block/dispenser' }, 'facing=east': { model: 'block/dispenser', y: 90 } } } }, {
    'block/dispenser': {
      textures: { particle: 'block/furnace_top', up: 'block/furnace_top', side: 'block/furnace_side', front: 'block/dispenser_front' },
      elements: [{ from: [0, 0, 0], to: [16, 16, 16], faces: { down: face('#up'), up: face('#up'), north: face('#front'), south: face('#side'), west: face('#side'), east: face('#side') } }]
    }
  })
  for (const [facing, front] of [['north', 'north'], ['east', 'east']]) {
    const baked = bakeState({ name: 'dispenser', props: { facing } }, models)
    assert.equal(classify(baked), 'cube')
    assert.equal(baked.elements[0].faces[front].texture, 'dispenser_front')
    assert.equal(baked.elements[0].faces.up.texture, 'furnace_top')
  }
})

test('classify: empty without elements, model for anything else', () => {
  const models = jar({ chest: { variants: { '': { model: 'block/particle_only' } } }, slab: { variants: { '': { model: 'block/slab' } } } }, {
    'block/particle_only': { textures: { particle: 'block/oak_planks' } },
    'block/slab': { elements: [{ from: [0, 0, 0], to: [16, 8, 16], faces: all('#t') }], textures: { t: 'block/x' } }
  })
  assert.equal(classify(bakeState({ name: 'chest', props: {} }, models)), 'empty')
  assert.equal(classify(bakeState({ name: 'slab', props: {} }, models)), 'model')
})

// ---- bakeAll ----

test('bakeAll walks every state of a small registry and summarises', () => {
  const registry = {
    blocksByName: {
      air: { name: 'air', minStateId: 0, maxStateId: 0, states: [] },
      thing: { name: 'thing', minStateId: 1, maxStateId: 4, states: [{ name: 'facing', type: 'enum', num_values: 2, values: ['north', 'east'] }, { name: 'half', type: 'enum', num_values: 2, values: ['bottom', 'top'] }] }
    }
  }
  const { states, summary } = bakeAll(registry, variantJar)
  assert.equal(states.get(0), null)
  assert.equal(states.get(1).source.includes('facing=north,half=bottom'), true)
  assert.deepEqual(summary, { states: 5, baked: 4, withElements: 2, distinctKeys: 2, totalElements: 2, maxElements: 1, noBlockstate: 1 })
})

// ---- the real data ----

const jarPath = findClientJar()
const real = jarPath === null ? null : { ...loadModels(jarPath), jarPath }
const registry26 = prismarineRegistry('26.1')

test('real: 26.1 states bake against the 1.21.8 jar and every face texture exists', { skip: real === null }, () => {
  const { states, summary } = bakeAll(registry26, real)
  assert.ok(summary.withElements > 20000)
  const names = new Set(zipEntries(fs.readFileSync(real.jarPath)).map(e => e.name))
  const exists = texture => names.has(`assets/minecraft/textures/block/${texture}.png`) || names.has(`assets/minecraft/textures/${texture}.png`)
  const missing = new Set([...states.values()].filter(Boolean).flatMap(b => b.elements).flatMap(e => Object.values(e.faces)).map(f => f.texture).filter(t => t === null || !exists(t)))
  assert.ok(missing.size <= 40, `${missing.size} face textures missing: ${[...missing].slice(0, 10)}`)
})
