import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { previewCells, blueprintDetail, blueprintDocumentDetail } from '../tools/dashboard/lib.mjs'
import { previewFaces, previewBounds, altColour, materialRoleRows } from '../tools/dashboard/blueprint.mjs'

const bp = (name, states = {}) => ({ legend: { B: { alts: [{ name, states }] } }, layers: [{ y: 0, grid: ['B'] }] })

test('preview uses state-specific slab, stair and bed geometry instead of full cubes', () => {
  const bottom = previewCells(bp('oak_slab', { type: 'bottom' }))[0]
  const top = previewCells(bp('oak_slab', { type: 'top' }))[0]
  assert.equal(Math.max(...bottom.shapes.map(s => s[4])), .5)
  assert.equal(Math.min(...top.shapes.map(s => s[1])), .5)
  const stairs = previewCells(bp('oak_stairs', { facing: 'east', half: 'bottom', shape: 'straight' }))[0]
  assert.ok(stairs.shapes.length > 1)
  assert.ok(previewCells(bp('red_bed', { part: 'foot', facing: 'south' }))[0].shapes.every(s => s[4] < 1))
  assert.deepEqual(previewCells(bp('air')), [])
  assert.ok(previewCells(bp('torch'))[0].shapes.length, 'non-colliding light remains visible')
})

test('rotation changes projected geometry and cutoff removes the roof, preserving material colors and source cells', () => {
  const cells = [{ ...previewCells(bp('birch_planks'))[0], x: 2, y: 0, z: 0 }, { ...previewCells(bp('red_wool'))[0], x: 2, y: 3, z: 0 }]
  const whole = previewFaces(cells, { angle: 0 })
  const cut = previewFaces(cells, { angle: 0, maxY: 0 })
  assert.equal(whole.length, 6)
  assert.equal(cut.length, 3)
  assert.ok(cut.every(f => f.cell === cells[0]))
  assert.notDeepEqual(previewFaces(cells, { angle: Math.PI / 2 })[0].points, whole[0].points)
  assert.ok(whole.every((f, i) => !i || f.depth >= whole[i - 1].depth))
  assert.notEqual(altColour(cells[0]), altColour(cells[1]))
  assert.ok(previewBounds(whole).y1 < previewBounds(cut).y1)
  assert.deepEqual(previewBounds([]), { x1: 0, x2: 1, y1: 0, y2: 1 })
})

test('real house payload includes every non-air blueprint cell and finite drawable faces', () => {
  const text = fs.readFileSync(new URL('../blueprints/villager-house-10.blueprint.json', import.meta.url), 'utf8')
  const detail = blueprintDetail({ name: 'villager-house-10', text, hash: 'preview' })
  const beds = detail.preview.filter(c => /_bed$/.test(c.name))
  assert.equal(beds.length, 20, 'both bed halves are rendered')
  assert.equal(detail.preview.filter(c => /_fence_gate$/.test(c.name)).length, 3)
  const faces = previewFaces(detail.preview)
  assert.ok(faces.length > detail.preview.length)
  assert.ok(faces.every(f => f.points.every(p => Object.values(p).every(Number.isFinite))))
})

 test('material cards distinguish hard constraints, preferences, exact pins and stock allocation', () => {
  const detail = { materials: { shell: { kind: 'full_cube', requires: { gravity: false }, preferences: ['oak_planks'] } }, relationships: [{ members: ['shell'], relation: 'material', strength: 'preferred' }], materialObjects: [{ id: 'wall', material: 'shell' }, { id: 'light', block: 'torch' }], allocation: { assignments: { wall: 'birch_planks', light: 'torch' } } }
  const rows = materialRoleRows(detail)
  assert.ok(rows[0].required.includes('gravity: excluded'))
  assert.ok(rows[0].preferences.some(s => s.includes('oak_planks') && s.includes('other allowed')))
  assert.ok(rows[0].preferences.some(s => s.includes('throughout this role') && s.includes('mixing allowed')))
  assert.deepEqual(rows[0].palette, ['birch_planks'])
  assert.deepEqual(rows[1].required, ['must use: torch'])
  delete detail.allocation
  assert.deepEqual(materialRoleRows(detail)[0].palette, [])
 })

test('v2 preview exposes abstract roles and labels stock allocation without selecting inventory implicitly', () => {
 const plan=JSON.parse(fs.readFileSync(new URL('../blueprints/starter-hut.blueprint.json',import.meta.url),'utf8'))
 const sample=blueprintDocumentDetail(plan)
 assert.deepEqual(sample.errors,[])
 assert.equal(sample.palette,'representative')
 assert.equal(sample.allocation,null)
 assert.ok(materialRoleRows(sample).some(r=>r.role==='wood_planks' && r.preferences.some(p=>p.includes('oak_planks'))))
 const stock=Object.fromEntries(Object.entries(sample.bill.total).map(([n,c])=>[n,c]))
 const selected=blueprintDocumentDetail(plan,stock)
 assert.deepEqual(selected.errors,[])
 assert.equal(selected.palette,'resolved-declared-stock')
 assert.ok(materialRoleRows(selected).some(r=>r.palette.includes('oak_planks')))
 const invalid=blueprintDocumentDetail({...plan,materials:{...plan.materials,shell:{kind:'full_cube',requires:{unknownTrait:true}}}})
 assert.equal(invalid.bp,null)
 assert.match(invalid.errors[0],/unsupported field/)
})
