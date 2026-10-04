import test from 'node:test'
import assert from 'node:assert/strict'
import { runTree } from '../src/tree/actions.mjs'
import { planCells, migratePlan } from '../src/lib/plan.mjs'
import { scaffoldId } from '../src/scaffold/access.mjs'
import { cleanupScaffold } from '../src/scaffold/access.mjs'
import { checkTree } from '../src/tree/inspect.mjs'
import { validateForestryRecord, legacyPlannedStumpRefusal } from '../src/tree/journal.mjs'

const at = { x: 0, y: 0, z: 0, species: 'oak' }
const wood = name => ({ name, solid: true, properties: { axis: 'y' } })
const leaf = { name: 'oak_leaves', solid: true, properties: { persistent: false } }

function fixture (entries = {}) {
  const world = new Map(Object.entries(entries))
  const records = new Map()
  const calls = []
  let pos = { x: -2.5, y: 1, z: 0.5 }
  const api = {
    block: (x, y, z) => world.get(`${x},${y},${z}`) ?? { name: y <= 0 ? 'dirt' : 'air', solid: y <= 0, properties: {} },
    inv: () => ({}), pos: () => pos, me: () => 'tester', places: () => [], checkpoint: async () => {}, report: () => {}, emit: () => {},
    forestry: (id, value) => { if (value === null) records.delete(id); else if (value !== undefined) records.set(id, structuredClone(value)); return records.get(id) ?? null },
    async act (name, args) {
      calls.push({ name, ...args })
      if (name === 'zones') return { zones: [] }
      if (name === 'goto') pos = { x: args.x + 0.5, y: args.y, z: args.z + 0.5 }
      if (name === 'dig') world.set(`${args.x},${args.y},${args.z}`, { name: 'air', solid: false, properties: {} })
      return {}
    }
  }
  return { api, world, records, calls }
}

const planned = () => {
  const plan = migratePlan({ name: 'grove', x: 0, y: 0, z: 0, plan: 'o', legend: { o: { kind: 'tree', species: 'oak' } } })
  plan.cells = planCells(plan)
  return plan
}

test('unmapped arbitrary wood without leaves remains protected', async () => {
  const f = fixture({ '0,1,0': wood('oak_log'), '0,2,0': wood('oak_log') })
  const r = await runTree(f.api, at, 'harvest')
  assert.ok(r.attention.some(s => /no attributable canopy/.test(s)))
  assert.equal(f.calls.some(c => c.name === 'dig'), false)
  assert.equal(f.records.size, 0)
})

test('mapped legacy two-log stump is narrowly recognized and recorded before removal', async () => {
  const f = fixture({ '0,1,0': wood('oak_log'), '0,2,0': wood('oak_log') })
  f.api.places = () => [planned()]
  const r = await runTree(f.api, { ...at, place: 'grove' }, 'harvest')
  assert.deepEqual(r.attention, [])
  assert.equal(r.harvested, 2)
  assert.equal(f.records.get(scaffoldId(at)).phase, 'decaying')
  assert.deepEqual(f.records.get(scaffoldId(at)).wood.map(p => p.y), [1, 2])
})

test('mapped single-tree three-log rooted stump resumes while branches and taller posts stay protected', async () => {
  const f = fixture({ '0,1,0': wood('oak_log'), '0,2,0': wood('oak_log'), '0,3,0': wood('oak_log') })
  f.api.places = () => [planned()]
  const r = await runTree(f.api, { ...at, place: 'grove' }, 'harvest')
  assert.deepEqual(r.attention, [])
  assert.equal(r.harvested, 3)
  assert.equal(f.records.get(scaffoldId(at)).phase, 'decaying')

  const taller = checkTree((x,y,z) => y === 0 ? { name: 'dirt' } : y >= 1 && y <= 4 && x === 0 && z === 0 ? { name: 'oak_log', properties: {} } : { name: 'air' }, at, 'oak', 'single', planCells(planned()))
  assert.match(legacyPlannedStumpRefusal(taller, planCells(planned())), /log limit/)
  const branched = checkTree((x,y,z) => y === 0 ? { name: 'dirt' } : (x === 0 && z === 0 && y <= 2) || (x === 1 && z === 0 && y === 2) ? { name: 'oak_log', properties: {} } : { name: 'air' }, at, 'oak', 'single', planCells(planned()))
  assert.match(legacyPlannedStumpRefusal(branched, planCells(planned())), /outside the first three rooted/)
})

test('journaled disconnected matching log resumes after its base was removed', async () => {
  const f = fixture({ '0,2,0': wood('oak_log') })
  f.records.set(scaffoldId(at), {
    root: { x: 0, y: 0, z: 0 }, species: 'oak', form: 'single',
    wood: [{ x: 0, y: 1, z: 0, name: 'oak_log' }, { x: 0, y: 2, z: 0, name: 'oak_log' }],
    leaves: [{ x: 0, y: 3, z: 0, name: 'oak_leaves' }], phase: 'harvesting', readyAt: null
  })
  const r = await runTree(f.api, at, 'harvest')
  assert.deepEqual(r.attention, [])
  assert.equal(r.harvested, 1)
  assert.deepEqual(r.remaining, [])
  assert.equal(f.records.get(scaffoldId(at)).phase, 'decaying')
  assert.ok(f.calls.some(c => c.name === 'dig' && c.y === 2))
})

test('journal accepts originally attributed large-tree wood inside the inspected envelope', () => {
  const root = { x: -102, y: 69, z: -234 }
  const logs = [
    [-102, 70, -234], [-101, 70, -234], [-102, 70, -233], [-101, 70, -233],
    [-100, 71, -232], [-100, 72, -232]
  ]
  const world = new Map(logs.map(([x,y,z]) => [`${x},${y},${z}`, { name: 'dark_oak_log', properties: {} }]))
  const at = (x,y,z) => world.get(`${x},${y},${z}`) ?? { name: y <= root.y ? 'dirt' : 'air', properties: {} }
  const tree = checkTree(at, root, 'dark_oak', 'large')
  assert.ok(tree.wood.some(p => p.x === -100 && p.z === -232), 'the inspection attributes connected off-footprint trunk wood')
  const record = { root, species: 'dark_oak', form: 'large', wood: tree.wood.map(({x,y,z,name}) => ({x,y,z,name})), leaves: [], phase: 'harvesting', readyAt: null }
  assert.equal(validateForestryRecord(record, tree, at).ok, true)
  record.wood.push({ x: root.x + tree.profile.radius + tree.profile.width, y: root.y + 1, z: root.z, name: 'dark_oak_log' })
  assert.equal(validateForestryRecord(record, tree, at).ok, false, 'forged cells outside the inspected envelope remain rejected')
})

test('cancel after a log removal leaves harvesting provenance and next run resumes', async () => {
  const f = fixture({ '0,1,0': wood('oak_log'), '0,2,0': wood('oak_log'), '0,3,0': leaf, '1,3,0': leaf })
  const dig = f.api.act.bind(f.api)
  let interrupted = false
  f.api.act = async (name, args) => {
    const result = await dig(name, args)
    if (name === 'dig' && !interrupted) { interrupted = true; throw new Error('cancelled') }
    return result
  }
  await assert.rejects(runTree(f.api, at, 'harvest'), /cancelled/)
  assert.equal(f.records.get(scaffoldId(at)).phase, 'harvesting')
  assert.equal(f.world.get('0,1,0').name, 'oak_log')
  f.api.act = dig
  const r = await runTree(f.api, at, 'harvest')
  assert.equal(r.harvested, 1)
  assert.deepEqual(r.remaining, [])
  assert.equal(f.records.get(scaffoldId(at)).phase, 'decaying')
})

test('cancel after base removal is cleaned before retry and advances to decay', async () => {
  const f = fixture({ '0,1,0': wood('oak_log'), '0,2,0': wood('oak_log'), '0,3,0': leaf })
  const scaffoldRecords = new Map()
  f.api.scaffolds = (id, value) => { if (value === null) scaffoldRecords.delete(id); else if (value !== undefined) scaffoldRecords.set(id, structuredClone(value)); return scaffoldRecords.get(id) ?? null }
  const act = f.api.act.bind(f.api)
  let interrupted = false
  f.api.act = async (name, args) => {
    const result = await act(name, args)
    if (name === 'dig' && args.y === 1 && !interrupted) { interrupted = true; throw new Error('cancelled') }
    return result
  }
  await assert.rejects(runTree(f.api, at, 'harvest'), /cancelled/)
  assert.equal(f.records.get(scaffoldId(at)).phase, 'harvesting')
  assert.equal(f.world.get('0,1,0').name, 'air')
  f.api.act = act
  // Maintenance recovers any stale supports before asking the tree reader again.
  await cleanupScaffold(f.api, scaffoldId(at), { attention: [] })
  const r = await runTree(f.api, at, 'harvest')
  assert.equal(r.state, 'empty')
  assert.equal(r.decay_wait, 120)
  assert.equal(f.records.get(scaffoldId(at)).phase, 'decaying')
  assert.equal(f.calls.filter(c => c.name === 'dig').length, 2)
})

test('mismatched journal cannot authorize another species or form', async () => {
  const f = fixture({ '0,1,0': wood('oak_log'), '0,2,0': wood('oak_log') })
  f.records.set(scaffoldId(at), {
    root: { x: 0, y: 0, z: 0 }, species: 'birch', form: 'single',
    wood: [{ x: 0, y: 1, z: 0, name: 'oak_log' }, { x: 0, y: 2, z: 0, name: 'oak_log' }],
    leaves: [], phase: 'harvesting', readyAt: null
  })
  const r = await runTree(f.api, at, 'harvest')
  assert.ok(r.attention.some(s => /journal root\/species\/form/.test(s)))
  assert.equal(f.calls.some(c => c.name === 'dig'), false)
})

test('successful cut persists a two-minute decay deadline for maintenance recovery', async () => {
  const f = fixture({ '0,1,0': wood('oak_log'), '0,2,0': wood('oak_log'), '0,3,0': leaf })
  await runTree(f.api, at, 'harvest')
  const record = f.records.get(scaffoldId(at))
  assert.equal(record.phase, 'decaying')
  assert.ok(record.readyAt >= Date.now() + 119000)
  assert.deepEqual(record.leaves, [{ x: 0, y: 3, z: 0, name: 'oak_leaves' }])
})

test('bush adjoining trunk is natural vegetation, not a placed-build warning', async () => {
  const f = fixture({ '0,1,0': wood('oak_log'), '0,2,0': wood('oak_log'), '0,3,0': leaf, '1,2,0': { name: 'bush', solid: false } })
  const r = await runTree(f.api, at, 'check')
  assert.deepEqual(r.attention, [])
})
