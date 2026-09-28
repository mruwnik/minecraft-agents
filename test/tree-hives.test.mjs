import test from 'node:test'
import assert from 'node:assert/strict'
import { migratePlan } from '../src/lib/plan.mjs'
import { forestHiveClaim, hiveSmokeCampfire, silkTouchTool } from '../src/tree/hives.mjs'
import { runTree } from '../src/tree/actions.mjs'

const plan = migratePlan({ name: 'forest', kind: 'forest', by: 'tester', x: 0, y: 0, z: 0, plan: 'or', legend: { o: 'oak_sapling', r: { kind: 'reserved' } } })
const wood = { name: 'oak_log', solid: true, properties: { axis: 'y' } }
const leaf = { name: 'oak_leaves', solid: true, properties: { persistent: false } }
const nest = { name: 'bee_nest', solid: true, properties: { facing: 'south', honey_level: 5 } }

test('forest hive permission follows the north-west plan anchor and owner', () => {
  assert.equal(forestHiveClaim(plan, 'tester', { x: 1, y: 2, z: 0 }, 'bee_nest'), true)
  assert.equal(forestHiveClaim(plan, 'tester', { x: 2, y: 2, z: 0 }, 'bee_nest'), false)
  assert.equal(forestHiveClaim(plan, 'other', { x: 1, y: 2, z: 0 }, 'bee_nest'), false)
  assert.equal(forestHiveClaim(plan, 'tester', { x: 1, y: 2, z: 0 }, 'creaking_heart'), false)
})

test('only an actually enchanted carried tool qualifies for hive pickup', () => {
  assert.equal(silkTouchTool([{ name: 'enchanted_book', enchants: [{ name: 'silk_touch', lvl: 1 }] }]), null)
  assert.equal(silkTouchTool([{ name: 'iron_axe', enchants: [{ name: 'efficiency', lvl: 1 }] }]), null)
  assert.equal(silkTouchTool([{ name: 'iron_axe', enchants: [{ name: 'minecraft:silk_touch', lvl: 1 }] }])?.name, 'iron_axe')
})

test('hive smoke requires a lit campfire directly below and a clear column', () => {
  const blocks = new Map([['1,1,0', { name: 'campfire', properties: { lit: true } }]])
  const at = (x, y, z) => blocks.get(`${x},${y},${z}`) ?? { name: 'air' }
  assert.deepEqual(hiveSmokeCampfire(at, { x: 1, y: 2, z: 0 }), { x: 1, y: 1, z: 0 })
  blocks.set('1,1,0', { name: 'campfire', properties: { lit: false } })
  assert.equal(hiveSmokeCampfire(at, { x: 1, y: 2, z: 0 }), null)
  blocks.set('1,1,0', { name: 'campfire', properties: { lit: true } })
  blocks.set('1,2,0', { name: 'oak_leaves' })
  assert.equal(hiveSmokeCampfire(at, { x: 1, y: 3, z: 0 }), null)
})

function scene (hasTool, smoke = false, campfireSupply = false) {
  const blocks = new Map(Object.entries({ '0,1,0': wood, '0,2,0': wood, '0,3,0': leaf, '1,3,0': leaf, '1,2,0': nest }))
  if (smoke) blocks.set('1,1,0', { name: 'campfire', solid: false, properties: { lit: true } })
  const inventory = {}
  if (campfireSupply) inventory.campfire = 1
  const calls = []
  let position = { x: -2.5, y: 1, z: .5 }
  let picked = false
  const api = {
    block: (x, y, z) => blocks.get(`${x},${y},${z}`) ?? { name: y <= 0 ? 'dirt' : 'air', solid: y <= 0, properties: {} },
    inv: () => inventory, hasSilkTouch: () => hasTool, pos: () => position,
    places: () => [plan], me: () => 'tester', checkpoint: async () => {}, report: () => {}, emit: () => {},
    async act (name, a = {}) {
      calls.push({ name, ...a })
      if (name === 'zones') return { zones: [] }
      if (name === 'goto') position = { x: a.x + .5, y: a.y, z: a.z + .5 }
      if (name === 'place' && a.item === 'campfire') blocks.set(`${a.x},${a.y},${a.z}`, { name: 'campfire', solid: false, properties: { lit: true } })
      if (name === 'dig') {
        if (a.x === 1 && a.y === 2 && a.z === 0) {
          if (hasTool) assert.equal(a.silk_touch, true)
          else { assert.equal(a.safe_hive, true); assert.deepEqual(a.smoke, { x: 1, y: 1, z: 0 }) }
          assert.equal(a.place, 'forest')
          picked = a.silk_touch === true
        }
        blocks.set(`${a.x},${a.y},${a.z}`, { name: 'air', solid: false, properties: {} })
      }
      if (name === 'collect' && picked) { inventory.bee_nest = (inventory.bee_nest ?? 0) + 1; picked = false }
      return {}
    }
  }
  return { api, blocks, inventory, calls }
}

test('maintenance hive policy retains an occupied nest without Silk Touch', async () => {
  const f = scene(false)
  const result = await runTree(f.api, { x: 0, y: 0, z: 0, species: 'oak', place: 'forest' }, 'harvest', { allowForestHives: true })
  assert.match(result.attention.join(' '), /no verifiable campfire smoke/)
  assert.equal(f.calls.some(call => call.name === 'dig'), false)
  assert.equal(f.blocks.get('1,2,0').name, 'bee_nest')
})

test('maintenance preflight leaves an owned no-Silk-Touch nest actionable for the smoke fallback', async () => {
  const f = scene(false)
  const result = await runTree(f.api, { x: 0, y: 0, z: 0, species: 'oak', place: 'forest' }, 'check', { allowForestHives: true })
  assert.deepEqual(result.attention, [])
  assert.equal(f.blocks.get('1,2,0').name, 'bee_nest')
  assert.equal(f.calls.some(call => call.name === 'dig'), false)
})

test('maintenance moves the nest before cutting wood and leaves foliage to decay', async () => {
  const f = scene(true)
  const result = await runTree(f.api, { x: 0, y: 0, z: 0, species: 'oak', place: 'forest' }, 'harvest', { allowForestHives: true })
  assert.deepEqual(result.attention, [])
  assert.equal(result.harvested, 2)
  assert.equal(f.inventory.bee_nest, 1)
  assert.deepEqual(f.calls.filter(call => call.name === 'dig').map(call => [call.x, call.y, call.z]), [[1, 2, 0], [0, 2, 0], [0, 1, 0]])
  assert.equal(f.blocks.get('0,3,0').name, 'oak_leaves')
})

test('maintenance destroys a claimed hive without Silk Touch only under verified smoke', async () => {
  const f = scene(false, true)
  const result = await runTree(f.api, { x: 0, y: 0, z: 0, species: 'oak', place: 'forest' }, 'harvest', { allowForestHives: true })
  assert.deepEqual(result.attention, [])
  assert.deepEqual(result.hivesDestroyed, [{ x: 1, y: 2, z: 0, name: 'bee_nest' }])
  assert.equal(f.inventory.bee_nest ?? 0, 0)
  assert.deepEqual(f.calls.filter(call => call.name === 'dig').map(call => [call.x, call.y, call.z]), [[1, 2, 0], [1, 1, 0], [0, 2, 0], [0, 1, 0]])
})

test('maintenance places and removes a temporary campfire before cutting', async () => {
  const f = scene(false, false, true)
  const result = await runTree(f.api, { x: 0, y: 0, z: 0, species: 'oak', place: 'forest' }, 'harvest', { allowForestHives: true })
  assert.deepEqual(result.attention, [])
  assert.deepEqual(f.calls.filter(call => call.name === 'place').map(call => [call.item, call.x, call.y, call.z]), [['campfire', 1, 1, 0]])
  assert.equal(f.blocks.get('1,1,0').name, 'air')
  const digs = f.calls.filter(call => call.name === 'dig').map(call => [call.x, call.y, call.z])
  assert.deepEqual(digs.slice(0, 3), [[1, 2, 0], [1, 1, 0], [0, 2, 0]])
})
