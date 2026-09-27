import test from 'node:test'
import assert from 'node:assert/strict'
import { farmFruitAt } from '../src/farm/fruit.mjs'
import { planCells } from '../src/lib.mjs'
import harvest from '../library/farm/harvest.mjs'
import { fakeApi } from './helpers.mjs'

const field = { name: 'fruit-field', kind: 'farm', x: 0, y: 63, z: 0, plan: 'm.k.*\n.....' }
const key = p => `${p.x},${p.y},${p.z}`
const fixture = () => {
  const world = {}
  for (let x = -5; x <= 9; x++) for (let z = -5; z <= 6; z++) {
    world[`${x},63,${z}`] = 'dirt'
    world[`${x},64,${z}`] = 'air'
    world[`${x},65,${z}`] = 'air'
  }
  for (const x of [0, 2, 4]) {
    world[`${x},63,0`] = 'farmland'
    world[`${x},64,0`] = `attached_${x === 2 ? 'pumpkin' : 'melon'}_stem`
    world[`${x},64,1`] = x === 2 ? 'pumpkin' : 'melon'
  }
  // A decorative fruit without a stem and another farmer's fruit remain intact.
  world['1,64,1'] = 'pumpkin'
  world['6,64,0'] = 'melon_stem#7'
  world['6,64,1'] = 'melon'
  const made = fakeApi({ world, places: [field], answers: {
    find_blocks: a => ({ positions: Object.entries(world).filter(([, name]) => a.block.includes(name.split('#')[0])).map(([at]) => {
      const [x, y, z] = at.split(',').map(Number)
      return { x, y, z }
    }) }),
    dig: p => { world[key(p)] = 'air' }
  } })
  const block = made.api.block
  made.api.block = (x, y, z) => {
    const value = block(x, y, z)
    if (value?.name.startsWith('attached_')) value.properties.facing = 'south'
    return value
  }
  return { ...made, world }
}

for (const replant of [true, false]) {
  test(`harvest gathers planned melon and pumpkin fruit on paths, preserving stems (replant=${replant})`, async () => {
    const { api, world, calls } = fixture()
    const result = await harvest.run(api, { place: field.name, replant })
    assert.deepEqual(result.harvested, { melon: 2, pumpkin: 1 })
    assert.equal(result.replanted, 0)
    assert.equal(result.stillGrowing, 0)
    assert.ok(!calls.some(c => c.startsWith('place ')))
    assert.equal(world['0,64,0'], 'attached_melon_stem')
    assert.equal(world['2,64,0'], 'attached_pumpkin_stem')
    assert.equal(world['4,64,0'], 'attached_melon_stem')
    assert.equal(world['1,64,1'], 'pumpkin')
    assert.equal(world['6,64,1'], 'melon')
    assert.equal(calls.filter(c => c.startsWith('dig ')).length, 3)
  })
}

test('fruit attribution requires the right stem, direction, height and planned crop', () => {
  const { api, world } = fixture()
  const cells = planCells(field)
  const fruit = { x: 0, y: 64, z: 1 }
  assert.equal(farmFruitAt(fruit, cells, api.block), 'melon')
  world['0,64,0'] = 'pumpkin_stem#7'
  assert.equal(farmFruitAt(fruit, cells, api.block), null)
  world['0,64,0'] = 'melon_stem#6'
  assert.equal(farmFruitAt(fruit, cells, api.block), null)
  world['0,64,0'] = 'melon_stem#7'
  assert.equal(farmFruitAt(fruit, cells, api.block), 'melon')
  assert.equal(farmFruitAt(fruit, cells.map(c => c.ch === 'm' ? { ...c, ch: 'w' } : c), api.block), null)
  assert.equal(farmFruitAt(fruit, cells.map(c => ({ ...c, y: 62 })), api.block), null)
  world['0,64,0'] = 'attached_melon_stem'
  const backwards = (x, y, z) => {
    const block = api.block(x, y, z)
    if (block?.name === 'attached_melon_stem') block.properties.facing = 'north'
    return block
  }
  assert.equal(farmFruitAt(fruit, cells, backwards), null)
})
