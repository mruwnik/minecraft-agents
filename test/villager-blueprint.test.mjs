import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { prepare as prepareLegacy } from '../src/blueprint/build.mjs'
import { blueprintCells, matchesCell, flatGround, jobsFor, orderJobs, bill, fullBlock } from '../src/blueprint/format.mjs'
import { breedPlan, breedPreflight, breedKey } from '../src/villager/breed.mjs'

const prepare = args => prepareLegacy(args, name => ({ text: fs.readFileSync(new URL(`fixtures/blueprints/${name}.txt`, import.meta.url), 'utf8'), hash: 'legacy-fixture' }))
const anchor = { x: -144, y: 65, z: -176 }
const plan = breedPlan({ x: -143, y: 65, z: -175, target: 10, size: 8, airlock: true, entryX: -135, entryZ: -168 })
const worldFor = bp => {
  const cells = new Map(blueprintCells(bp).map(c => {
    const alt = c.spec.alts[0]
    return [breedKey({ x: anchor.x + c.dx, y: anchor.y + c.dy, z: anchor.z + c.dz }), { name: alt.name, solid: fullBlock(alt.name), properties: { ...alt.states, ...(alt.name.endsWith('_fence_gate') ? { open: false } : {}) } }]
  }))
  return (x, y, z) => cells.get(`${x},${y},${z}`) ?? { name: 'air', solid: false, properties: {} }
}

test('ten-bed blueprint satisfies the real breeder geometry and both bed halves', () => {
  const { bp, lint } = prepare({ name: 'villager-house-10' })
  assert.deepEqual(lint.errors, [])
  const world = worldFor(bp)
  const checked = breedPreflight(plan, world, 'oak_planks', 'oak_fence_gate')
  assert.deepEqual(checked.needed, [])
  assert.deepEqual(checked.missingBeds, [])
  assert.deepEqual(checked.clear, [])
  for (const gate of plan.gates) assert.equal(world(gate.x, gate.y, gate.z).properties.open, false)
  for (const { foot, head } of plan.beds) {
    assert.equal(world(foot.x, foot.y, foot.z).properties.part, 'foot')
    assert.equal(world(head.x, head.y, head.z).properties.part, 'head')
    assert.equal(world(foot.x, foot.y, foot.z).properties.facing, 'south')
  }
})

test('house material parameters and reusable natural floor preserve breeder validity', () => {
  const { bp } = prepare({ name: 'villager-house-10', params: { block: 'stone', wood: 'birch', bed: 'blue' } })
  const world = worldFor(bp)
  const checked = breedPreflight(plan, world, 'birch_planks', 'birch_fence_gate')
  assert.equal(checked.missingBeds.length, 0)
  assert.equal(checked.needed.length, 0)
  const floor = blueprintCells(bp).find(c => c.dy === -1)
  assert.equal(matchesCell({ name: 'grass_block', properties: {} }, floor.spec.alts), true)
  assert.equal(bill(bp).total.blue_bed, 10)
  assert.equal(bill(bp).total.stone, 314)
  assert.equal(bill(bp).total.birch_fence_gate, 3)
  assert.equal(bill(bp).total.birch_planks, undefined)
})

test('default stone blueprint reuses the actual oak house without accepting hazardous walls', () => {
  const { bp } = prepare({ name: 'villager-house-10' })
  const { bp: oak } = prepare({ name: 'villager-house-10', params: { block: 'oak_planks' } })
  const world = worldFor(oak)
  for (const c of blueprintCells(bp)) assert.equal(matchesCell(world(anchor.x + c.dx, anchor.y + c.dy, anchor.z + c.dz), c.spec.alts), true, `${c.dx},${c.dy},${c.dz}`)
  assert.equal(breedPreflight(plan, world).needed.length, 0)
  const wall = blueprintCells(bp).find(c => c.token === 'W')
  for (const name of ['magma_block', 'honey_block', 'soul_sand', 'oak_slab']) assert.equal(matchesCell({ name, properties: {} }, wall.spec.alts), false)
  assert.equal(bill(bp).total.cobblestone, 314)
})

test('mixed complete bed colors are accepted, but mismatched halves are refused', () => {
  const { bp } = prepare({ name: 'villager-house-10' })
  const base = worldFor(bp)
  const changes = new Map()
  for (const [i, bed] of plan.beds.entries()) for (const p of [bed.foot, bed.head]) changes.set(breedKey(p), { ...base(p.x, p.y, p.z), name: `${i % 2 ? 'red' : 'blue'}_bed` })
  const mixed = (x, y, z) => changes.get(`${x},${y},${z}`) ?? base(x, y, z)
  assert.equal(breedPreflight(plan, mixed).validBeds, 10)
  const head = plan.beds[0].head
  changes.set(breedKey(head), { ...mixed(head.x, head.y, head.z), name: 'red_bed' })
  assert.throws(() => breedPreflight(plan, mixed), /partial or has wrong facing/)
})

test('existing mixed wooden gates retain doorway axes and full wall safety', () => {
  const { bp } = prepare({ name: 'villager-house-10' })
  const base = worldFor(bp)
  const gate = plan.innerGate
  const world = (x, y, z) => x === gate.x && y === gate.y && z === gate.z ? { name: 'birch_fence_gate', solid: false, properties: { facing: 'west', open: false } } : base(x, y, z)
  assert.equal(breedPreflight(plan, world).needed.length, 0)
  const cell = blueprintCells(bp).find(c => anchor.x + c.dx === gate.x && anchor.y + c.dy === gate.y && anchor.z + c.dz === gate.z)
  assert.equal(matchesCell(world(gate.x, gate.y, gate.z), cell.spec.alts), true, 'blueprint and breeder accept either gate direction on the same physical axis')
  const aligned = { ...world(gate.x, gate.y, gate.z), properties: { facing: 'east', open: false } }
  assert.equal(matchesCell(aligned, cell.spec.alts), true)
  assert.throws(() => breedPreflight(plan, (x, y, z) => x === gate.x && y === gate.y && z === gate.z ? { ...aligned, properties: { facing: 'north', open: false } } : base(x, y, z)), /must face east or west/)
})

test('fresh house build has supported placements and reachable jobs on flat terrain', () => {
  const { bp } = prepare({ name: 'villager-house-10' })
  const ground = flatGround(64)
  const jobs = orderJobs(jobsFor(bp, anchor, ground), bp, anchor, ground)
  assert.deepEqual(jobs.unreachable, [])
  assert.equal(jobs.jobs.filter(j => j.item === 'white_bed').length, 10)
  assert.equal(jobs.jobs.filter(j => j.item === 'oak_fence_gate').length, 3)
})

test('v2 atomic house fixture passes the actual breeder safety validation', async () => {
  const { compileBlueprintStructure, concreteBlueprint } = await import('../src/blueprint/compiler.mjs')
  const { materialCandidates } = await import('../src/blueprint/materials.mjs')
  const document=JSON.parse(fs.readFileSync(new URL('../blueprints/villager-house-10.blueprint.json',import.meta.url),'utf8'))
  const ir=compileBlueprintStructure(document)
  const assignments=Object.fromEntries(ir.objects.map(o=>[o.id,o.block??materialCandidates(document.materials[o.material])[0]]))
  const bp=concreteBlueprint(ir,assignments), checked=breedPreflight(plan,worldFor(bp))
  assert.equal(checked.validBeds,10)
  assert.deepEqual(checked.needed,[])
  assert.deepEqual(checked.clear,[])
})
