import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { dryHorseLanding } from '../src/navigation/horse-landing.mjs'
const require = createRequire(import.meta.url)
const registry = require('minecraft-data')('26.1')
const Block = require('prismarine-block')(registry)
const block = name => Block.fromStateId(registry.blocksByName[name].defaultState, 0)
const world = (cover = 'leaf_litter', override = () => undefined) => (x, y, z) => {
  const replacement = override(x, y, z)
  if (replacement !== undefined) return replacement
  return block(y === 116 ? 'grass_block' : y === 117 ? cover : y === 119 ? 'oak_leaves' : 'air')
}
test('actual groundcover collision permits dry player dismount below canopy', () => {
  assert.equal(dryHorseLanding(world(), { x: 389.54, y: 117, z: -224.6 }), true)
  assert.equal(dryHorseLanding(world('wildflowers'), { x: 389.54, y: 117, z: -224.6 }), true)
})
test('landing requires loaded safe full-footprint support and real player clearance', () => {
  for (const name of ['water', 'cactus', 'wither_rose', 'cobweb', 'oak_log']) {
    assert.equal(dryHorseLanding(world(name), { x: 0.5, y: 117, z: 0.5 }), false, name)
  }
  for (const floor of [null, block('water'), block('oak_leaves'), block('magma_block')]) {
    assert.equal(dryHorseLanding(world('air', (x, y, z) => y === 116 && x === 1 ? floor : undefined), { x: 0.9, y: 117, z: 0.5 }), false)
  }
  assert.equal(dryHorseLanding(world('air', (x, y) => y === 118 ? block('oak_leaves') : undefined), { x: 0.5, y: 117, z: 0.5 }), false)
  assert.equal(dryHorseLanding(world(), { x: 0.5, y: 117.5, z: 0.5 }), false)
})
