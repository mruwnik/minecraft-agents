import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { validateBlueprintDocument, canonicalBlueprint, semanticBlueprintHash } from '../src/blueprint/schema.mjs'
import { compileBlueprintStructure, concreteBlueprint } from '../src/blueprint/compiler.mjs'
import { allocateBlueprintMaterials } from '../src/blueprint/materials.mjs'
import { blueprintFileArguments, readBlueprintSource } from '../src/blueprint/source.mjs'
import { manifestId, writeBlueprintManifest, readBlueprintManifest } from '../src/blueprint/manifest.mjs'
import { REGISTRY } from '../src/blueprint/format.mjs'

const base = overrides => ({
  schemaVersion: 2,
  id: 'test-house',
  title: 'Test house',
  dimensions: [3, 3, 3],
  front: 'south',
  materials: {
    wall: { kind: 'full_cube', candidates: ['oak_planks', 'birch_planks'] },
    bed: { kind: 'bed', candidates: ['white_bed', 'red_bed'] },
    door: { kind: 'door', candidates: ['oak_door'] }
  },
  relationships: [],
  structure: { legend: {}, layers: [], objects: [], spaces: [] },
  ...overrides
})

const oneWall = (material = 'wall') => base({
  structure: { legend: {}, layers: [], objects: [{ id: 'wall-a', at: [0, 0, 0], type: 'block', material }], spaces: [] }
})

test('v2 schema is strict and semantic hashes ignore presentation metadata and key order', () => {
  const a = base({ metadata: { author: 'a' }, title: 'First' })
  const b = { ...a, title: 'Second', metadata: { author: 'b' }, structure: { ...a.structure, spaces: [], objects: [] } }
  assert.equal(semanticBlueprintHash(a), semanticBlueprintHash(b))
  assert.equal(canonicalBlueprint({ b: 1, a: { d: 2, c: 3 } }), canonicalBlueprint({ a: { c: 3, d: 2 }, b: 1 }))
  assert.equal(validateBlueprintDocument(a).schemaVersion, 2)
  assert.throws(() => compileBlueprintStructure(base({ structure: { objects: [{ id: '__proto__', at: [0,0,0], material: 'wall' }] } })), /invalid object identifier/)
  assert.throws(() => validateBlueprintDocument({ ...a, materials: { constructor: { kind: 'full_cube' } } }), /invalid material slot/)
  assert.throws(() => validateBlueprintDocument({ ...a, schemaVersion: 3 }), /schemaVersion/)
  assert.throws(() => validateBlueprintDocument({ ...a, surprise: true }), /unsupported field/)
  assert.throws(() => validateBlueprintDocument({ ...a, materials: { wall: { kind: 'full_cube', requires: { explosive: true } } } }), /unsupported field/)
})

test('compiler expands beds and doors into atomic, nonoverlapping world cells', () => {
  const source = base({
    materials: { bed: { kind: 'bed', candidates: ['white_bed'] }, door: { kind: 'door', candidates: ['oak_door'] } },
    structure: { legend: {}, layers: [], objects: [
      { id: 'bed-1', at: [0, 0, 0], type: 'bed', material: 'bed', constructionState: { facing: 'south' } },
      { id: 'door-1', at: [2, 0, 0], type: 'door', material: 'door', constructionState: { facing: 'east' } }
    ], spaces: [] }
  })
  const ir = compileBlueprintStructure(source)
  assert.equal(ir.objects.length, 2)
  assert.deepEqual(ir.objects[0].footprint.map(c => c.at), [[0, 0, 0], [0, 0, 1]])
  assert.deepEqual(ir.objects[1].footprint.map(c => c.at), [[2, 0, 0], [2, 1, 0]])
  assert.equal(new Set(ir.cells.map(c => c.at.join(','))).size, ir.cells.length)
  assert.throws(() => compileBlueprintStructure({ ...source, structure: { ...source.structure, objects: [source.structure.objects[0], { ...source.structure.objects[1], at: [0, 0, 1] }] } }), /overlapping cell/)
  assert.throws(() => compileBlueprintStructure({ ...source, structure: { ...source.structure, objects: [{ ...source.structure.objects[0], at: [2, 0, 2] }] } }), /outside dimensions/)

  const assigned = concreteBlueprint(ir, { 'bed-1': 'white_bed', 'door-1': 'oak_door' })
  assert.ok(assigned.v2.hash === ir.hash)
  assert.equal(Object.keys(assigned.legend).length, 4)
  assert.throws(() => concreteBlueprint(ir, { 'bed-1': 'oak_planks', 'door-1': 'oak_door' }), /wrong object kind|violates hard capabilities/)
})

test('allocator globally shares stock instead of greedily double-spending a flexible item', () => {
  const doc = base({
    materials: {
      flexible: { kind: 'full_cube', candidates: ['oak_planks', 'birch_planks'] },
      oak_only: { kind: 'full_cube', candidates: ['oak_planks'] }
    },
    structure: { legend: {}, layers: [], objects: [
      { id: 'a', at: [0, 0, 0], material: 'flexible' },
      { id: 'b', at: [1, 0, 0], material: 'oak_only' }
    ], spaces: [] }
  })
  const ir = compileBlueprintStructure(doc)
  const allocated = allocateBlueprintMaterials(ir, { stock: { oak_planks: 1, birch_planks: 1 } })
  assert.deepEqual(allocated.bill, { birch_planks: 1, oak_planks: 1 })
  assert.equal(allocated.assignments.b, 'oak_planks')
  assert.equal(allocated.assignments.a, 'birch_planks')
  assert.throws(() => allocateBlueprintMaterials(ir, { stock: { oak_planks: 1 } }), /not funded/)
})

test('required family constraints include reused objects and preferred uniformity falls back only when needed', () => {
  const grouped = base({
    materials: {
      boards: { kind: 'full_cube', familyClass: 'wood', candidates: ['oak_planks', 'birch_planks'] },
      logs: { kind: 'log', familyClass: 'wood', candidates: ['oak_log', 'birch_log'] }
    },
    relationships: [{ members: ['boards', 'logs'], relation: 'family', strength: 'required' }],
    structure: { legend: {}, layers: [], objects: [
      { id: 'board', at: [0, 0, 0], material: 'boards' },
      { id: 'log', at: [1, 0, 0], material: 'logs' }
    ], spaces: [] }
  })
  const ir = compileBlueprintStructure(grouped)
  const same = allocateBlueprintMaterials(ir, { stock: { oak_planks: 1, oak_log: 1, birch_planks: 1, birch_log: 1 } })
  assert.equal(same.assignments.board, 'birch_planks') // deterministic family binding order
  assert.equal(same.assignments.log, 'birch_log')
  assert.throws(() => allocateBlueprintMaterials(ir, { stock: {}, existing: { board: 'oak_planks', log: 'birch_log' } }), /required family relationship conflicts/)

  const preferredDoc = base({
    materials: { wall: { kind: 'full_cube', candidates: ['oak_planks', 'birch_planks'] } },
    relationships: [{ members: ['wall'], relation: 'material', strength: 'preferred' }],
    structure: { legend: {}, layers: [], objects: [
      { id: 'wall-a', at: [0, 0, 0], material: 'wall' },
      { id: 'wall-b', at: [1, 0, 0], material: 'wall' }
    ], spaces: [] }
  })
  const preferredIR = compileBlueprintStructure(preferredDoc)
  const uniform = allocateBlueprintMaterials(preferredIR, { stock: { oak_planks: 2, birch_planks: 2 } })
  assert.deepEqual(new Set(Object.values(uniform.assignments)), new Set(['birch_planks']))
  const mixedFallback = allocateBlueprintMaterials(preferredIR, { stock: { oak_planks: 1, birch_planks: 1 } })
  assert.deepEqual(new Set(Object.values(mixedFallback.assignments)), new Set(['oak_planks', 'birch_planks']))

  const badExisting = compileBlueprintStructure(oneWall())
  assert.throws(() => allocateBlueprintMaterials(badExisting, { stock: {}, existing: { 'wall-a': 'sand' } }), /violates its requirement/)
  assert.deepEqual(allocateBlueprintMaterials(badExisting, { stock: {}, existing: { 'wall-a': 'oak_planks' } }).bill, {})
})

test('concrete conversion emits legacy cells only after a complete supported allocation', () => {
  const ir = compileBlueprintStructure(oneWall())
  const allocation = allocateBlueprintMaterials(ir, { stock: { oak_planks: 1 } })
  const legacy = concreteBlueprint(ir, allocation.assignments)
  assert.equal(legacy.layers[0].grid.length, 3)
  assert.equal(legacy.layers[0].grid[0][0], Object.values(legacy.legend)[0].token)
  assert.equal(REGISTRY.blocksByName[allocation.assignments['wall-a']].boundingBox, 'block')
})

test('file and inline sources validate through the same schema and catalog lookup is constrained', t => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'bp-v2-source-'))
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  const doc = oneWall()
  fs.writeFileSync(path.join(dir, 'house.json'), JSON.stringify(doc))
  const fromFile = blueprintFileArguments('blueprint.build', { file: 'house.json', timeout: 10 }, dir)
  assert.equal(fromFile.plan.id, doc.id)
  assert.equal(fromFile.origin, path.join(dir, 'house.json'))
  assert.equal(readBlueprintSource({ plan: fromFile.plan }, dir).document.id, doc.id)
  assert.throws(() => blueprintFileArguments('blueprint.build', { file: 'house.json', name: 'house' }, dir), /exactly one/)
  assert.throws(() => readBlueprintSource({ file: 'house.json' }, dir), /calling CLI/)
  assert.throws(() => readBlueprintSource({ name: '../outside' }, dir), /invalid blueprint catalog name/)
  assert.throws(() => readBlueprintSource({ name: 'missing' }, dir), /no v2 blueprint/)
  fs.writeFileSync(path.join(dir, 'bad.json'), JSON.stringify({ ...doc, schemaVersion: 1 }))
  assert.throws(() => blueprintFileArguments('blueprint.check', { file: 'bad.json' }, dir), /schemaVersion/)
})

test('manifest snapshots are atomic-addressed, reloadable and reject missing or corrupt identity', t => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'bp-v2-state-'))
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  const id = manifestId('home', [-2, 64, 10])
  const note = writeBlueprintManifest({ id, place: 'home', at: [-2, 64, 10], sourceHash: 'a'.repeat(64), assignments: { wall: 'oak_planks' } }, dir)
  assert.match(note, /^bp2:[a-f0-9]{32}$/)
  assert.deepEqual(readBlueprintManifest(note, dir), {
    assignments: { wall: 'oak_planks' }, at: [-2, 64, 10], id, manifestVersion: 2, place: 'home', sourceHash: 'a'.repeat(64)
  })
  assert.equal(readBlueprintManifest('old-format-note', dir), null)
  assert.throws(() => readBlueprintManifest(`bp2:${'f'.repeat(32)}`, dir), /is missing/)
  fs.writeFileSync(path.join(dir, `${id}.json`), JSON.stringify({ id: '0'.repeat(32), manifestVersion: 1 }))
  assert.throws(() => readBlueprintManifest(note, dir), /identity\/version/)
})

test('four migrated fixtures retain multipart quantities and supported concrete construction', async () => {
  const { parseBlueprint, resolve, blueprintCells, bill, lint, flatGround } = await import('../src/blueprint/format.mjs')
  const { materialCandidates } = await import('../src/blueprint/materials.mjs')
  const { checkBlueprintV2 } = await import('../src/blueprint/v2.mjs')
  for (const name of ['starter-hut', 'watchtower', 'wheat-field', 'villager-house-10']) {
    const root = new URL('../blueprints/', import.meta.url)
    const doc = JSON.parse(fs.readFileSync(new URL(`${name}.blueprint.json`, root)))
    const ir = compileBlueprintStructure(doc)
    const assignments = Object.fromEntries(ir.objects.map(o => [o.id, o.block ?? materialCandidates(doc.materials[o.material])[0]]))
    const actual = concreteBlueprint(ir, assignments)
    const old = resolve(parseBlueprint(fs.readFileSync(new URL(`fixtures/blueprints/${name}.txt`, import.meta.url), 'utf8')))
    const snapshot = bp => blueprintCells(bp).map(c => [c.dx, c.dy, c.dz, c.spec.alts[0].name, c.spec.alts[0].states]).sort((a, b) => JSON.stringify(a).localeCompare(JSON.stringify(b)))
    // Legacy runtime/derived states were descriptive, not construction requirements.
    const normalize = rows => rows.map(([x, y, z, n, s]) => [x, y, z, n, Object.fromEntries(Object.entries(s).filter(([k]) => !['age', 'open', 'occupied', 'hinge', 'type'].includes(k) || k === 'type' && /_slab$/.test(n)))])
    assert.deepEqual(normalize(snapshot(actual)), normalize(snapshot(old)), name)
    assert.deepEqual(bill(actual).total, bill(old).total, name)
    assert.deepEqual(lint(actual).errors, [], name)
    const items = { ...bill(actual).total, dirt: 512, iron_hoe: 1 }
    const calls = []
    const api = { inv: () => items, block: flatGround(64), places: () => [], zones: () => [], me: () => 'Tester', freeSlots: () => 34, act: async (action, args) => { calls.push({ action, args }); return {} } }
    const checked = await checkBlueprintV2(api, { plan: doc, x: 0, y: 65, z: 0 })
    assert.equal(checked.planned, true, name)
    assert.ok(calls.every(c => c.action === 'state'), 'check must not mutate')
  }
})
