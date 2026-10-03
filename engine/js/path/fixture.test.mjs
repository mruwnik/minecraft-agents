import { test } from 'node:test'
import assert from 'node:assert/strict'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { fixtureSnapshot, layers, stateId } from './fixture.mjs'
import { UNLOADED } from './snapshot.mjs'

const registry = prismarineRegistry('26.1')
const Block = prismarineBlock(registry)
const nameAt = (snap, x, y, z) => Block.fromStateId(snap.stateAt(x, y, z), 0).name

test('blocks land where asked, air elsewhere in the touched column', () => {
  const snap = fixtureSnapshot({ blocks: [[3, 64, 5, 'stone']] })
  assert.equal(nameAt(snap, 3, 64, 5), 'stone')
  assert.equal(nameAt(snap, 4, 64, 5), 'air')
  assert.equal(nameAt(snap, 15, 300, 15), 'air')
  assert.equal(nameAt(snap, 0, -64, 0), 'air')
})

test('untouched columns are unloaded', () => {
  const snap = fixtureSnapshot({ blocks: [[3, 64, 5, 'stone']] })
  assert.equal(snap.stateAt(16, 64, 5), UNLOADED)
  assert.equal(snap.stateAt(3, 64, -1), UNLOADED)
})

test('fill boxes apply before blocks, inclusive, across columns', () => {
  const snap = fixtureSnapshot({
    fill: [[-2, 63, -2, 17, 63, 2, 'stone']],
    blocks: [[0, 63, 0, 'dirt']]
  })
  assert.equal(nameAt(snap, -2, 63, -2), 'stone')
  assert.equal(nameAt(snap, 17, 63, 2), 'stone')
  assert.equal(nameAt(snap, 0, 63, 0), 'dirt')
  assert.equal(nameAt(snap, 18, 63, 0), 'air')
  assert.ok(snap.hasColumn(-1, 0) && snap.hasColumn(1, 0))
})

const cases = [
  ['oak_slab', { type: 'bottom' }, { type: 'bottom' }],
  ['oak_slab', { type: 'top' }, { type: 'top' }],
  ['snow', { layers: 3 }, { layers: '3' }],
  ['oak_trapdoor', { facing: 'north', open: 'true' }, { facing: 'north', open: true }],
  ['oak_trapdoor', {}, { facing: 'north', half: 'bottom', open: false }]
]
for (const [name, props, expected] of cases) {
  test(`stateId ${name} ${JSON.stringify(props)}`, () => {
    const props2 = Block.fromStateId(stateId(name, props), 0).getProperties()
    assert.deepEqual(Object.fromEntries(Object.keys(expected).map(k => [k, props2[k]])), expected)
  })
}

test('unspecified properties take the default state', () => {
  assert.equal(stateId('oak_slab', { type: 'bottom' }), registry.blocksByName.oak_slab.defaultState)
  assert.equal(stateId('stone'), registry.blocksByName.stone.defaultState)
})

test('unknown block name throws', () => {
  assert.throws(() => stateId('no_such_block'))
})

test('layers builds blocks from ascii, bottom layer first', () => {
  const blocks = layers([
    ['SS', 'S.'],
    ['..', ' w']
  ], { S: 'stone', w: ['oak_slab', { type: 'top' }] }, { x: 10, y: 60, z: 20 })
  assert.deepEqual(blocks, [
    [10, 60, 20, 'stone'], [11, 60, 20, 'stone'], [10, 60, 21, 'stone'], [11, 60, 21, 'air'],
    [10, 61, 20, 'air'], [11, 61, 20, 'air'], [11, 61, 21, 'oak_slab', { type: 'top' }]
  ])
})
