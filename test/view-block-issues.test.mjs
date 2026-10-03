// Which blocks the browser view cannot draw properly: the classifier over small hand-built registry, material and model
// fixtures, the merge with what was seen in the world, and a check against the real 26.1 table and the 1.21.8 jar.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineRegistry from 'prismarine-registry'
import { REASONS, SEVERITIES, COLORMAP_BLOCKS, classifyStatic, mergeSeen } from '../tools/view/block-issues.mjs'

const enumState = (name, values) => ({ name, type: 'enum', num_values: values.length, values })
const boolState = name => ({ name, type: 'bool', num_values: 2 })

// a registry of the given blocks, state ids handed out in order, the last property varying fastest
const registryOf = defs => {
  let next = 1
  const blocksByName = {}
  for (const [name, states = []] of defs) {
    const count = states.reduce((n, s) => n * s.num_values, 1)
    blocksByName[name] = { name, minStateId: next, maxStateId: next + count - 1, states }
    next += count
  }
  return { blocksByName, stateCount: next }
}

const FACING = enumState('facing', ['north', 'south', 'west', 'east'])
const SEGMENTS = enumState('segment_amount', ['1', '2', '3', '4'])
const CROSS_MATERIAL = { name: 'x', kind: 'cross', tex: [0, 0, 0], box: [0, 0, 0, 16, 16, 16], flags: 1 }
const CUBE_MATERIAL = { name: 'x', kind: 'cube', tex: [1, 1, 1], box: [0, 0, 0, 16, 16, 16], flags: 0 }
const BARE_MATERIAL = { name: 'x', kind: 'cube', tex: [-1, -1, -1], box: [0, 0, 0, 16, 16, 16], flags: 0 }

// every state of a block gets the one material (index 1), or the given function's
const classify = ({ defs, material, models, materialIndexOf = () => 1, extra = [] }) => {
  const registry = registryOf(defs)
  const materials = [{ name: 'air', kind: 'cube', tex: [-1, -1, -1], box: [0, 0, 0, 16, 16, 16], flags: 0 }, material, ...extra]
  const materialOf = Uint16Array.from({ length: registry.stateCount }, (_, id) => id === 0 ? 0 : materialIndexOf(id))
  return classifyStatic({ registry, materials, materialOf, models })
}

const FULL_ELEMENT = { from: [0, 0, 0], to: [16, 16, 16], faces: { up: { texture: '#all' } } }
const CUBE_MODELS = {
  blockstates: new Map(),
  models: new Map([
    ['block/cube', { elements: [FULL_ELEMENT] }],
    ['block/cube_all', { parent: 'block/cube', textures: {} }]
  ])
}
const modelsWith = (blockstates, models) => ({
  blockstates: new Map([...CUBE_MODELS.blockstates, ...Object.entries(blockstates)]),
  models: new Map([...CUBE_MODELS.models, ...Object.entries(models)])
})

const LEAF_LITTER = modelsWith(
  {
    leaf_litter: {
      multipart: [
        { apply: { model: 'minecraft:block/leaf_litter_1' }, when: { facing: 'north', segment_amount: '1' } },
        { apply: { model: 'minecraft:block/leaf_litter_2', y: 90 }, when: { facing: 'east', segment_amount: '2|3' } }
      ]
    }
  },
  {
    'block/template_leaf_litter_1': { elements: [{ from: [0, 0.25, 0], to: [8, 0.25, 8], faces: { up: { texture: '#texture', tintindex: 0 } } }] },
    'block/leaf_litter_1': { parent: 'minecraft:block/template_leaf_litter_1', textures: { texture: 'minecraft:block/leaf_litter' } },
    'block/leaf_litter_2': { parent: 'minecraft:block/template_leaf_litter_1', textures: { texture: 'minecraft:block/leaf_litter' } }
  }
)

const POPPY = modelsWith(
  { poppy: { variants: { '': { model: 'minecraft:block/poppy' } } } },
  {
    'block/cross': { elements: [{ from: [0.8, 0, 8], to: [15.2, 16, 8], faces: { north: { texture: '#cross' } } }] },
    'block/poppy': { parent: 'minecraft:block/cross', textures: { cross: 'minecraft:block/poppy' } }
  }
)

const STONE = modelsWith(
  { stone: { variants: { '': { model: 'minecraft:block/stone' } } } },
  { 'block/stone': { parent: 'minecraft:block/cube_all', textures: { all: 'minecraft:block/stone' } } }
)

const reasonsOf = records => records.map(r => r.reason).sort()

test('REASONS is the fixed list', () => {
  assert.deepEqual(REASONS, ['unknown-state', 'no-texture', 'shape-mismatch', 'shape-approximated', 'tint-missing', 'state-ignored', 'no-model-data', 'block-entity'])
})

test('a leaf_litter-like block drawn as a cross is a shape mismatch, untinted and ignores its properties', () => {
  const records = classify({ defs: [['leaf_litter', [FACING, SEGMENTS]]], material: CROSS_MATERIAL, models: LEAF_LITTER })
  assert.deepEqual(reasonsOf(records), ['shape-mismatch', 'state-ignored', 'tint-missing'])
  const byReason = Object.fromEntries(records.map(r => [r.reason, r]))
  assert.equal(byReason['shape-mismatch'].drawnAs, 'cross')
  assert.match(byReason['shape-mismatch'].detail, /leaf_litter_1/)
  assert.match(byReason['state-ignored'].detail, /facing/)
  assert.match(byReason['state-ignored'].detail, /segment_amount/)
  assert.equal(byReason['state-ignored'].states, 16)
  assert.equal(byReason['shape-mismatch'].name, 'leaf_litter')
  assert.deepEqual(byReason['shape-mismatch'].example, { stateId: 1, props: { facing: 'north', segment_amount: '1' } })
})

test('a property that changes the material is not ignored', () => {
  const records = classify({
    defs: [['leaf_litter', [FACING, SEGMENTS]]],
    material: CROSS_MATERIAL,
    extra: [{ ...CROSS_MATERIAL, flags: 5 }],
    materialIndexOf: id => 1 + ((id - 1) % 4 === 0 ? 1 : 0),
    models: LEAF_LITTER
  })
  assert.match(records.find(r => r.reason === 'state-ignored').detail, /depends on facing;/)
  assert.doesNotMatch(records.find(r => r.reason === 'state-ignored').detail, /segment_amount/)
})

for (const [label, defs, material, models] of [
  ['a poppy-like cross', [['poppy']], CROSS_MATERIAL, POPPY],
  ['stone', [['stone']], CUBE_MATERIAL, STONE]
]) {
  test(`${label} gives nothing`, () => {
    assert.deepEqual(classify({ defs, material, models }), [])
  })
}

test('a dried_ghast-like block without a texture is no-texture drawn as a hash colour', () => {
  const records = classify({ defs: [['dried_ghast', [boolState('waterlogged')]]], material: BARE_MATERIAL, models: null })
  assert.deepEqual(records.filter(r => r.name !== '*').map(r => [r.name, r.reason, r.drawnAs, r.states]), [['dried_ghast', 'no-texture', 'hash colour', 2]])
})

test('without model data only no-texture is reported, and one global note says so', () => {
  const records = classify({
    defs: [['leaf_litter', [FACING, SEGMENTS]], ['dried_ghast']],
    material: CROSS_MATERIAL,
    materialIndexOf: id => id <= 16 ? 1 : 2,
    extra: [BARE_MATERIAL],
    models: null
  })
  assert.deepEqual(records.map(r => [r.name, r.reason]), [['dried_ghast', 'no-texture'], ['*', 'no-model-data']])
  assert.match(records[1].detail, /no client jar/)
})

test('a block missing from the jar is no-model-data', () => {
  const records = classify({ defs: [['stone'], ['newblock']], material: CUBE_MATERIAL, models: STONE })
  assert.deepEqual(records.map(r => [r.name, r.reason]), [['newblock', 'no-model-data']])
})

test('a full-collision block with several model elements is shape-approximated, a single-element box is not', () => {
  const box = { ...CUBE_MATERIAL, kind: 'box', box: [0, 0, 0, 16, 8, 16] }
  const models = modelsWith(
    { slab: { variants: { '': { model: 'minecraft:block/slab' } } }, stairs: { variants: { '': { model: 'minecraft:block/stairs' } } } },
    {
      'block/slab': { elements: [{ from: [0, 0, 0], to: [16, 8, 16], faces: {} }] },
      'block/stairs': { elements: [{ from: [0, 0, 0], to: [16, 8, 16], faces: {} }, { from: [0, 8, 0], to: [8, 16, 16], faces: {} }] }
    }
  )
  const records = classify({ defs: [['slab'], ['stairs']], material: box, models })
  assert.deepEqual(records.map(r => [r.name, r.reason, r.drawnAs]), [['stairs', 'shape-approximated', 'box [0,0,0,16,8,16]']])
})

// ---- severity, model detail, block entities ----

test('SEVERITIES is missing, wrong, approximate', () => {
  assert.deepEqual(SEVERITIES, ['missing', 'wrong', 'approximate'])
})

test('each reason gets its default severity, and the records carry the model and the ignored properties', () => {
  const records = classify({ defs: [['leaf_litter', [FACING, SEGMENTS]], ['dried_ghast']], material: CROSS_MATERIAL, extra: [BARE_MATERIAL], materialIndexOf: id => id <= 16 ? 1 : 2, models: LEAF_LITTER })
  const bySeverity = Object.fromEntries(records.filter(r => r.name !== '*').map(r => [`${r.name} ${r.reason}`, r.severity]))
  assert.deepEqual(bySeverity, {
    'leaf_litter shape-mismatch': 'wrong',
    'leaf_litter tint-missing': 'wrong',
    'leaf_litter state-ignored': 'approximate',
    'dried_ghast no-texture': 'missing',
    'dried_ghast no-model-data': 'approximate'
  })
  const litter = Object.fromEntries(records.filter(r => r.name === 'leaf_litter').map(r => [r.reason, r]))
  assert.equal(litter['shape-mismatch'].model, 'block/leaf_litter_1')
  assert.deepEqual(litter['shape-mismatch'].parents, ['block/leaf_litter_1', 'block/template_leaf_litter_1'])
  assert.deepEqual(litter['state-ignored'].ignored, ['facing', 'segment_amount'])
})

test('a box that equals the single model element is no issue, one that differs is approximated', () => {
  const models = modelsWith(
    { carpet: { variants: { '': { model: 'minecraft:block/carpet' } } }, plate: { variants: { '': { model: 'minecraft:block/plate' } } } },
    { 'block/carpet': { elements: [{ from: [0, 0, 0], to: [16, 1, 16], faces: {} }] }, 'block/plate': { elements: [{ from: [1, 0, 1], to: [15, 1, 15], faces: {} }] } }
  )
  const flat = { ...CUBE_MATERIAL, kind: 'box', box: [0, 0, 0, 16, 1, 16] }
  assert.deepEqual(classify({ defs: [['carpet'], ['plate']], material: flat, models }).map(r => [r.name, r.reason]), [['plate', 'shape-approximated']])
})

test('drawnAs follows the affected states, and states of a block drawn several ways are counted honestly', () => {
  const layers = enumState('layers', ['1', '2'])
  const models = modelsWith(
    { snow: { variants: { 'layers=1': { model: 'minecraft:block/snow1' }, 'layers=2': { model: 'minecraft:block/snow1' } } } },
    { 'block/snow1': { elements: [{ from: [0, 0, 0], to: [16, 2, 16], faces: {} } ] } }
  )
  const thin = { ...CUBE_MATERIAL, kind: 'box', box: [0, 0, 0, 16, 2, 16] }
  const records = classify({ defs: [['snow', [layers]]], material: thin, extra: [CUBE_MATERIAL], materialIndexOf: id => id === 1 ? 1 : 2, models })
  assert.deepEqual(records.map(r => [r.reason, r.drawnAs, r.states, r.statesTotal]), [['shape-approximated', 'cube', 1, 2]])
})

test('an untinted overlay is only approximate, an untinted base is wrong', () => {
  const grass = modelsWith(
    { grass_block: { variants: { '': { model: 'minecraft:block/grass_block' } } } },
    { 'block/grass_block': { elements: [FULL_ELEMENT, { from: [0, 0, 0], to: [16, 16, 16], faces: { north: { texture: '#overlay', tintindex: 0 } } }], textures: { overlay: 'minecraft:block/grass_block_side_overlay' } } }
  )
  const tint = classify({ defs: [['grass_block']], material: CUBE_MATERIAL, models: grass }).find(r => r.reason === 'tint-missing')
  assert.equal(tint.severity, 'approximate')
})

test('a cross drawn for upright planes (a crop) is only approximate, a flat model is wrong', () => {
  const crop = modelsWith(
    { wheat: { variants: { '': { model: 'minecraft:block/wheat' } } } },
    { 'block/wheat': { elements: [{ from: [4, 0, 0], to: [4, 16, 16], faces: {} }, { from: [0, 0, 4], to: [16, 16, 4], faces: {} }] } }
  )
  assert.deepEqual(classify({ defs: [['wheat']], material: CROSS_MATERIAL, models: crop }).map(r => [r.reason, r.severity]), [['shape-mismatch', 'approximate']])
})

const tintedModels = name => modelsWith(
  { [name]: { variants: { '': { model: `minecraft:block/${name}` } } } },
  { [`block/${name}`]: { elements: [{ from: [0, 0, 0], to: [16, 16, 16], faces: { up: { texture: '#all', tintindex: 0 } } }], textures: { all: `minecraft:block/${name}` } } }
)

test('tint-missing fires only for blocks the game tints: cherry_leaves has a tintindex but no colour provider', () => {
  const reasonsFor = name => classify({ defs: [[name]], material: CUBE_MATERIAL, models: tintedModels(name) }).map(r => r.reason)
  assert.deepEqual(reasonsFor('cherry_leaves'), [])
  assert.deepEqual(reasonsFor('bamboo'), [])
  assert.deepEqual(reasonsFor('leaf_litter'), ['tint-missing'])
})

test('bubble_column is a wrong shape (the game draws water), not missing or a block entity', () => {
  const models = modelsWith({ bubble_column: { variants: { '': { model: 'minecraft:block/bubble_column' } } } }, { 'block/bubble_column': { textures: { particle: 'minecraft:block/water_still' } } })
  const records = classify({ defs: [['bubble_column', [boolState('drag')]]], material: BARE_MATERIAL, models })
  assert.deepEqual(records.map(r => [r.reason, r.severity, r.drawnAs, r.detail]), [['shape-mismatch', 'wrong', 'cube', 'drawn as cube; the game draws it as water']])
})

test('moving_piston is never drawn as a block, so it is not reported', () => {
  assert.deepEqual(classify({ defs: [['moving_piston']], material: BARE_MATERIAL, models: null }).filter(r => r.name !== '*'), [])
})

test('no-texture says when the jar model names textures the lookup missed', () => {
  const models = modelsWith(
    { dispenser: { variants: { '': { model: 'minecraft:block/dispenser' } } } },
    { 'block/dispenser': { elements: [FULL_ELEMENT], textures: { particle: 'minecraft:block/furnace_top', top: 'minecraft:block/furnace_top', front: 'minecraft:block/dispenser_front' } } }
  )
  const [record] = classify({ defs: [['dispenser']], material: BARE_MATERIAL, models })
  assert.equal(record.reason, 'no-texture')
  assert.equal(record.detail, 'texture lookup by name failed; the model uses furnace_top, dispenser_front')
})

test('crops and torches drawn as a cross are only approximate', () => {
  const models = modelsWith(
    { potatoes: { variants: { '': { model: 'minecraft:block/potatoes' } } }, torch: { variants: { '': { model: 'minecraft:block/torch' } } } },
    {
      'block/crop': { elements: [{ from: [4, 0, 0], to: [4, 16, 16], faces: {} }, { from: [0, 0, 4], to: [16, 16, 4], faces: {} }] },
      'block/potatoes': { parent: 'minecraft:block/crop' },
      'block/template_torch': { elements: [{ from: [7, 0, 7], to: [9, 10, 9], faces: {} }] },
      'block/torch': { parent: 'minecraft:block/template_torch' }
    }
  )
  assert.deepEqual(classify({ defs: [['potatoes'], ['torch']], material: CROSS_MATERIAL, models }).map(r => [r.name, r.reason, r.severity]), [['potatoes', 'shape-mismatch', 'approximate'], ['torch', 'shape-mismatch', 'approximate']])
})

test('a block whose models have no elements is a block-entity, not a shape mismatch', () => {
  const entity = modelsWith(
    { chest: { variants: { '': { model: 'minecraft:block/chest' } } }, oak_sign: { variants: { '': { model: 'minecraft:block/oak_sign' } } } },
    { 'block/chest': { parent: 'builtin/entity', textures: { particle: 'minecraft:block/oak_planks' } }, 'block/oak_sign': { textures: { particle: 'minecraft:block/oak_planks' } } }
  )
  const records = classify({ defs: [['chest', [FACING]], ['oak_sign']], material: CUBE_MATERIAL, models: entity })
  assert.deepEqual(records.map(r => [r.name, r.reason, r.severity, r.drawnAs]), [['chest', 'block-entity', 'wrong', 'cube'], ['oak_sign', 'block-entity', 'wrong', 'cube']])
  assert.equal(records[0].model, 'block/chest')
})

test('blocks with geometry whose book or items an entity renderer adds are a milder block-entity', () => {
  const lectern = modelsWith({ lectern: { variants: { '': { model: 'minecraft:block/lectern' } } } }, { 'block/lectern': { elements: [FULL_ELEMENT, { from: [4, 0, 4], to: [12, 16, 12], faces: {} }] } })
  const box = { ...CUBE_MATERIAL, kind: 'box', box: [0, 0, 0, 16, 14, 16] }
  const records = classify({ defs: [['lectern']], material: box, models: lectern })
  assert.deepEqual(records.filter(r => r.reason === 'block-entity').map(r => r.severity), ['approximate'])
})

// ---- merge ----

const record = (name, reason, extra = {}) => ({ name, reason, drawnAs: 'cross', detail: '', example: { stateId: 1, props: {} }, states: 1, ...extra })
const FIRST = { world: 'w', x: 1, y: 2, z: 3, agent: 'Bob' }

test('mergeSeen adds seen and firstSeen, one record per name and reason, sorted by severity, then seen, then name', () => {
  const merged = mergeSeen(
    [record('b', 'shape-mismatch'), record('a', 'tint-missing'), record('b', 'shape-mismatch'), record('b', 'state-ignored'), record('c', 'no-texture')],
    new Map([['b', { count: 7, first: FIRST }], ['a', { count: 2, first: FIRST }]])
  )
  assert.deepEqual(merged.map(r => [r.name, r.reason, r.seen, r.severity]), [
    ['c', 'no-texture', 0, 'missing'],
    ['b', 'shape-mismatch', 7, 'wrong'],
    ['a', 'tint-missing', 2, 'wrong'],
    ['b', 'state-ignored', 7, 'approximate']
  ])
  assert.deepEqual(merged[1].firstSeen, FIRST)
  assert.equal(merged[0].firstSeen, null)
})

test('mergeSeen turns unknown state ids into their own records', () => {
  const merged = mergeSeen([record('a', 'no-texture')], new Map([['state:99999', { count: 4, first: FIRST }], ['a', { count: 1, first: FIRST }]]))
  assert.deepEqual(merged.map(r => [r.name, r.reason, r.seen]), [['state:99999', 'unknown-state', 4], ['a', 'no-texture', 1]])
  assert.equal(merged[0].example.stateId, 99999)
  assert.equal(merged[0].severity, 'missing')
})

test('mergeSeen turns an unknown id range into one record with its positions', () => {
  const firsts = [FIRST, { ...FIRST, x: 9 }]
  const merged = mergeSeen([], new Map([['state:30000-30003', { count: 6, first: FIRST, firsts }]]))
  assert.deepEqual(merged.map(r => [r.name, r.reason, r.seen, r.states, r.example.stateId]), [['state:30000-30003', 'unknown-state', 6, 4, 30000]])
  assert.deepEqual(merged[0].positions, firsts)
})

test('mergeSeen does not mutate its inputs', () => {
  const statics = [record('a', 'no-texture')]
  mergeSeen(statics, new Map([['a', { count: 1, first: FIRST }]]))
  assert.equal(statics[0].seen, undefined)
})

// ---- the real table and jar ----

const root = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const jarPath = (await import('../tools/view/jar-read.mjs')).findClientJar()
const haveReal = jarPath !== null && fs.existsSync(path.join(root, 'textures')) && fs.readdirSync(path.join(root, 'textures')).length > 0

test('the real 26.1 table classifies vanilla blocks sensibly', { skip: !haveReal }, async () => {
  const { classifyReal } = await import('../tools/view/block-scan.mjs')
  const { records } = classifyReal({ version: '26.1', textureDir: path.join(root, 'textures'), jarPath })
  const flagged = name => new Set(records.filter(r => r.name === name).map(r => r.reason))
  assert.deepEqual([...flagged('poppy')], [])
  assert.deepEqual([...flagged('short_grass')], [])
  assert.deepEqual([...flagged('stone')], [])
  for (const [name, reason] of [['leaf_litter', 'shape-mismatch'], ['leaf_litter', 'tint-missing'], ['pink_petals', 'shape-mismatch'], ['wildflowers', 'shape-mismatch'], ['oak_stairs', 'shape-approximated']]) {
    assert.ok(flagged(name).has(reason), `${name} ${reason}`)
  }
})

test('every block in the from-memory colormap list exists in the 26.1 registry', () => {
  const { blocksByName } = prismarineRegistry('26.1')
  assert.deepEqual(COLORMAP_BLOCKS.filter(name => !blocksByName[name]), [])
})
