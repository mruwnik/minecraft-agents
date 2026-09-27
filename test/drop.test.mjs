// The goal a walk aims at to reach a dropped item (card 21657b89): a farmland cell is 15/16 high, a slab half that,
// so an item resting on either floors to the SURFACE block, not the air cell above it, and a walk sent there with
// range=0 finds nothing to stand in and is refused. Pure geometry: floor the position, then widen the range so the
// walk can land on the cell above (or beside) a short surface without needing to know what the block is.
import test from 'node:test'
import assert from 'node:assert/strict'
import { dropGoal } from '../src/drop.mjs'
import { cellOf } from '../src/farm/field.mjs'
import { noStanding } from '../src/navigation/walk.mjs'
import collect from '../library/collect.mjs'
import { fakeApi } from './helpers.mjs'

const at = (x, y, z) => ({ x, y, z })
for (const [name, entityPos, expected] of [
  ['on farmland: floors to the farmland cell itself, range widened so a walk can stand above it',
    at(14.5, 62.9375, -71.3), { x: 14, y: 62, z: -72, range: 1 }],
  ['on a bottom slab: floors to the slab cell, range widened the same way',
    at(5.7, 70.5001, 3.2), { x: 5, y: 70, z: 3, range: 1 }],
  ['in water: already floors to the water cell itself, which is standable, but still gets range=1',
    at(8.1, 65.31, -2.9), { x: 8, y: 65, z: -3, range: 1 }],
  ['on a bare crop cell (harvested wheat lying on farmland)',
    at(20.6, 63.9375, 10.1), { x: 20, y: 63, z: 10, range: 1 }],
  ['on an ordinary full block: already floors to the air cell above it',
    at(0.5, 65.01, 0.5), { x: 0, y: 65, z: 0, range: 1 }],
  ['negative coordinates floor toward negative infinity, not toward zero',
    at(-0.3, 61.02, -12.9), { x: -1, y: 61, z: -13, range: 1 }]
]) test(`dropGoal: ${name}`, () => assert.deepEqual(dropGoal(entityPos), expected))

const planted = () => {
  const world = {}
  for (let x = -2; x <= 2; x++) for (let z = -2; z <= 2; z++) {
    world[`${x},62,${z}`] = 'farmland'
    world[`${x},63,${z}`] = 'wheat#5'
    for (let y = 64; y <= 66; y++) world[`${x},${y},${z}`] = 'air'
  }
  return world
}
const drop = (id, x = 0, z = 0) => ({ id, x, y: 62, z, item: 'wheat_seeds', dist: 2, deep: false })
const cropShapes = api => {
  const block = api.block
  api.block = (...at) => {
    const found = block(...at)
    return found?.name === 'wheat' ? { ...found, solid: false } : found
  }
}

test('dropGoal reads short solid surfaces at feet level while preserving water and ordinary floor goals', () => {
  for (const name of ['farmland', 'oak_slab']) assert.deepEqual(dropGoal(at(0.5, 62.9375, 0.5), () => ({ name, solid: true })), { x: 0, y: 63, z: 0, range: 1 })
  for (const name of ['air', 'water']) assert.deepEqual(dropGoal(at(0.5, 63.01, 0.5), () => ({ name, solid: false })), { x: 0, y: 63, z: 0, range: 1 })
})

test('collect skips repeated impossible crop-cell pickups and continues with a reachable drop', async () => {
  const world = planted()
  Object.assign(world, { '5,62,0': 'dirt', '5,63,0': 'air', '5,64,0': 'air' })
  const drops = [drop(1), drop(2), { ...drop(3, 5), y: 63 }]
  const { api, calls } = fakeApi({ world, drops, answers: { goto: a => {
    assert.equal(a.x, 5, 'never calls goto for either impossible wheat-cell drop')
    drops.splice(drops.findIndex(d => d.id === 3), 1)
  } } })
  cropShapes(api)
  const result = await collect.run(api, {})
  assert.equal(result.picked, 1)
  assert.match(result.couldNotReach, /2 still lying/)
  assert.equal(calls.filter(c => c.startsWith('goto ')).length, 1)
  assert.equal(world['0,63,0'], 'wheat#5')
})

test('collect reaches a drop on cropped farmland from an adjacent covered channel', async () => {
  const world = planted()
  world['1,62,0'] = 'oak_slab~#top'
  world['1,63,0'] = 'air'
  const drops = [drop(1)]
  const { api, calls } = fakeApi({ world, drops, answers: { goto: goal => {
    const cellAt = (x, y, z) => cellOf(api.block(x, y, z))
    assert.ok(noStanding(cellAt, { ...goal, y: 62 }, 1), 'old surface-centered goal cannot reach the adjacent channel')
    assert.equal(noStanding(cellAt, goal, goal.range), null, 'raised goal reaches the safe channel')
    assert.deepEqual(goal, { x: 0, y: 63, z: 0, range: 1 })
    drops.length = 0
  } } })
  cropShapes(api)
  const result = await collect.run(api, {})
  assert.equal(result.picked, 1)
  assert.equal(result.couldNotReach, undefined)
  assert.deepEqual(calls, ['goto x=0 y=63 z=0 range=1'])
  assert.equal(world['0,63,0'], 'wheat#5')
})

test('collect never swallows cancellation or follows another drop after a nested hand-back', async () => {
  for (const failure of [new Error('cancelled'), { stopped: 'hurt' }]) {
    const { api, calls } = fakeApi({ drops: [drop(1), drop(2)], answers: { goto: failure } })
    await assert.rejects(collect.run(api, {}))
    assert.equal(calls.length, 1)
  }
})
