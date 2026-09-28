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

import { checkedHorseLanding } from '../src/navigation/horse-landing.mjs'
const ordinary = (x, y, z) => block(y < 66 ? 'grass_block' : 'air')
const horse = { position: { x: 51.5, y: 66, z: -42.5 }, width: 1.3964844, height: 1.6, yaw: 0 }
test('horse landing follows vehicle heading and main hand, not an arbitrary safe neighbour', () => {
  for (const yaw of [0, Math.PI / 2, Math.PI, -Math.PI / 2, 0.37]) {
    const right = checkedHorseLanding(ordinary, { ...horse, yaw }, { mainHand: 'right' })
    const left = checkedHorseLanding(ordinary, { ...horse, yaw }, { mainHand: 'left' })
    assert.equal(right.y, 66)
    assert.ok(Math.abs((right.x + left.x) / 2 - horse.position.x) < 1e-9)
    assert.ok(Math.abs((right.z + left.z) / 2 - horse.position.z) < 1e-9)
    assert.ok(Math.abs(Math.max(Math.abs(right.x - horse.position.x), Math.abs(right.z - horse.position.z)) - (horse.width + 0.6 + 0.00001) / 2) < 1e-9)
  }
  const right = checkedHorseLanding(ordinary, horse)
  assert.ok(right.x > horse.position.x)
})
test('recorded bank position cannot dismount merely because a different neighbour is safe', () => {
  // Local reproduction of the failure class, not an invented complete scan:
  // preferred east exit drops one block; west stays at the horse's level.
  const bank = (x, y, z) => block(y < (x >= 52 ? 65 : 66) ? 'grass_block' : 'air')
  assert.equal(dryHorseLanding(bank, { x: 50.5, y: 66, z: -42.5 }), true)
  assert.throws(() => checkedHorseLanding(bank, horse), /fallback is unproved/)
})
test('first physically clear but hazardous candidate refuses instead of skipping to another side', () => {
  const lavaSide = (x, y, z) => x === 52 && y === 66 ? block('water') : ordinary(x, y, z)
  assert.throws(() => checkedHorseLanding(lavaSide, horse), /first horse landing is not checked dry/)
  const partial = (x, y, z) => x === 52 && y === 66 ? block('farmland') : ordinary(x, y, z)
  assert.throws(() => checkedHorseLanding(partial, horse), /first horse landing is not checked dry/)
})
test('vanilla upward standing search accepts a full block rise and refuses crouch-only or unknown exits', () => {
  const raised = (x, y, z) => block(y < (x === 52 ? 67 : 66) ? 'grass_block' : 'air')
  assert.equal(checkedHorseLanding(raised, horse).y, 67)
  const roof = (x, y, z) => x === 52 && y >= 67 ? block('stone') : ordinary(x, y, z)
  assert.throws(() => checkedHorseLanding(roof, horse), /fallback is unproved/)
  assert.throws(() => checkedHorseLanding((x, y, z) => x === 52 ? null : ordinary(x, y, z), horse), /unloaded cell/)
  assert.throws(() => checkedHorseLanding(ordinary, { ...horse, yaw: undefined }), /confirmed dimensions/)
})
