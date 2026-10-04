import test from 'node:test'
import assert from 'node:assert/strict'
import { cellKey, safeFullBlock, woodenGate } from '../src/enclosure/blocks.mjs'
import { roomPlan, roomInside } from '../src/enclosure/layout.mjs'
import { habitatThreats, habitatOwnership, closeHabitatGates, requireClosedGates } from '../src/enclosure/guards.mjs'
import { breedPlan } from '../src/villager/breed.mjs'
import { boatHabitatPlan } from '../src/boat/habitat.mjs'

test('breeding and arrival share the entire physical room, including the baby-safe airlock corner', () => {
  for (const width of [8, 10, 15]) {
    const args = { x: -143, y: 65, z: -175, target: 10, size: width, airlock: true, entryX: -143 + width, entryZ: -175 + width - 1 }
    const breeder = breedPlan(args)
    const arrival = boatHabitatPlan(args)
    for (const field of ['shell', 'gates', 'lights', 'center']) assert.deepEqual(breeder[field], arrival[field])
    const cells = new Set(arrival.shell.map(cellKey))
    for (let x = arrival.innerGate.x; x < arrival.entry.x; x++) for (let y = 65; y < 68; y++) assert.ok(cells.has(cellKey({ x, y, z: arrival.entry.z - 1 })))
    assert.equal(roomInside(arrival, { x: arrival.innerGate.x + 0.5, y: 65, z: arrival.entry.z + 0.5 }), false)
    assert.equal(roomInside(arrival, { x: arrival.innerGate.x - 0.5, y: 65, z: arrival.entry.z + 0.5 }), true)
    assert.ok(arrival.lights.every(p => !cells.has(cellKey(p))))
  }
})

test('a small room retains its roof and primary doorway without an arrival airlock', () => {
  const p = roomPlan({ x: 0, y: 64, z: 0, width: 5 })
  const cells = new Set(p.shell.map(cellKey))
  assert.equal(p.gates.length, 1)
  assert.equal(cells.has('-1,64,2'), false)
  assert.equal(cells.has('-1,65,2'), false)
  assert.equal(cells.has('-1,66,2'), true)
  assert.equal(p.shell.filter(q => q.y === 67).length, 49)
  assert.equal(roomInside(p, { x: 4.99, y: 64, z: 4.99 }), true)
})

test('shared support rules reject harmful or partial floors while wooden gate variants remain compatible', () => {
  for (const name of ['magma_block', 'honey_block', 'soul_sand', 'oak_slab', 'oak_fence_gate', 'white_bed', 'farmland']) assert.equal(safeFullBlock({ name, solid: true }), false, name)
  assert.equal(safeFullBlock({ name: 'grass_block', solid: true }), true)
  assert.equal(safeFullBlock(undefined), undefined)
  assert.equal(woodenGate('pale_oak_fence_gate'), true)
  assert.equal(woodenGate('iron_door'), false)
})

test('gate closure changes only observed open wooden gates and uses bounded cancellation cleanup', async () => {
  const p = roomPlan({ x: 0, y: 64, z: 0, width: 8, entry: { x: 8, y: 64, z: 7 }, airlock: true })
  const blocks = new Map(p.gates.map((q, i) => [cellKey(q), { name: ['birch_fence_gate', 'pale_oak_fence_gate', 'oak_fence_gate'][i], properties: { open: i !== 0 } }]))
  const changes = []
  const api = { block: (x, y, z) => blocks.get(`${x},${y},${z}`), act: () => assert.fail('normal action during cleanup'), cleanupAct: async (name, a) => { changes.push({ name, ...a }); blocks.get(cellKey(a)).properties.open = false } }
  await closeHabitatGates(api, p, 'oak_fence_gate', true)
  assert.deepEqual(changes.map(cellKey), [cellKey(p.entry), cellKey(p.innerGate)])
  requireClosedGates(api, p)
  blocks.get(cellKey(p.entry)).properties.open = true
  requireClosedGates(api, p, 'outer')
  assert.throws(() => requireClosedGates(api, p), /not closed/)
  blocks.delete(cellKey(p.innerGate))
  assert.throws(() => requireClosedGates(api, p, 'outer'), /not closed/)
})

test('shared site guards reject unknown threats, census truncation and protected work without beds', async () => {
  const p = roomPlan({ x: 0, y: 64, z: 0, width: 8 })
  const api = { me: () => 'Probe', zones: () => [], places: () => [], act: async () => ({ found: [{ exact: '1,64,1', uuid: 'hostile' }] }) }
  await assert.rejects(habitatThreats(api, p), /hostile .* near enclosure/)
  await assert.rejects(habitatThreats({ ...api, act: async () => ({ found: [{}] }) }, p), /confirmed position/)
  await assert.rejects(habitatThreats({ ...api, act: async () => ({ found: Array(1000).fill({}) }) }, p), /observation limit/)
  habitatOwnership(api, p, { clear: [] })
  assert.throws(() => habitatOwnership({ ...api, zones: () => [{ name: 'other-home', x1: -1, y1: 64, z1: -1, x2: 8, y2: 67, z2: 8 }] }, p, { clear: [] }), /protected zone/)
  assert.throws(() => habitatOwnership({ ...api, places: () => [{ name: 'other-home', by: 'Other', x: 0, z: 0 }] }, p, { clear: [] }), /ground/)
})
