import test from 'node:test'
import assert from 'node:assert/strict'
import { clearClickRay, placementSight } from '../src/blueprint/visibility.mjs'
import { REGISTRY, resolve, parseBlueprint, jobsFor, orderJobs, flatGround } from '../src/blueprint/format.mjs'

const block = (name, props = {}) => ({ name, getProperties: () => props })
const world = cells => (x, y, z) => cells[`${x},${y},${z}`] ?? block('air')
const hasBox = name => REGISTRY.blocksByName[name]?.boundingBox === 'block'
const job = (extra = {}) => ({ x: 0, y: 1, z: 0, do: 'place', block: { name: 'cobblestone' }, ...extra })

test('a ray above a bottom slab is clear, but the same top slab obstructs it', () => {
  const from = { x: -1, y: 1.75, z: .5 }; const point = { x: 2, y: 1.75, z: .5 }
  assert.equal(clearClickRay(world({ '0,1,0': block('oak_slab', { type: 'bottom' }) }), from, point), true)
  assert.equal(clearClickRay(world({ '0,1,0': block('oak_slab', { type: 'top' }) }), from, point), false)
})

test('fence geometry protrudes into the otherwise empty voxel above', () => {
  const from = { x: -1, y: 1.25, z: .5 }; const point = { x: 2, y: 1.25, z: .5 }
  assert.equal(clearClickRay(world({ '0,0,0': block('oak_fence') }), from, point), false)
  assert.equal(clearClickRay(world({ '0,0,0': block('oak_fence') }), { ...from, z: .1 }, { ...point, z: .1 }), true)
})

test('unloaded ray cells and out-of-reach cursors refuse', () => {
  assert.equal(clearClickRay((x, y, z) => x === 0 ? null : block('air'), { x: -1, y: 2, z: .5 }, { x: 2, y: 2, z: .5 }), false)
  assert.equal(clearClickRay(world({}), { x: 0, y: 2, z: 0 }, { x: 5, y: 2, z: 0 }), false)
})

test('even a named full-cube reference cannot be clicked through its hidden back face', () => {
  assert.equal(clearClickRay(world({ '0,1,0': block('stone') }), { x: -1, y: 1.5, z: .5 }, { x: 1, y: 1.5, z: .5 }, 4.5, { x: 0, y: 1, z: 0 }), false)
})

test('an occluded below reference can use a visible side; its chosen face remains explicit', () => {
  const w = world({ '0,0,0': block('stone'), '1,1,0': block('stone'), '-1,1,0': block('stone') })
  assert.deepEqual(placementSight(job(), { x: 0, y: 1, z: -1 }, w, hasBox), { dx: 0, dy: -1, dz: 0 })
  // Only the east support is allowed: operator must be on its exposed west side.
  assert.ok(placementSight(job({ against: { dx: 1, dy: 0, dz: 0 } }), { x: -1, y: 1, z: 0 }, w, hasBox))
  assert.equal(placementSight(job({ against: { dx: 1, dy: 0, dz: 0 } }), { x: 2, y: 1, z: 0 }, w, hasBox), null)
})

test('axis logs consider both legal supports instead of freezing the first hidden side', () => {
  const w = world({ '-1,1,0': block('stone'), '1,1,0': block('stone') })
  assert.deepEqual(placementSight(job({ along: 'x', block: { name: 'oak_log' } }), { x: 1, y: 2, z: 1 }, w, hasBox), { dx: -1, dy: 0, dz: 0 })
})

test('top slabs refuse the below face, bottom slabs refuse the above face', () => {
  const top = job({ block: { name: 'oak_slab' }, half: 'top' })
  assert.equal(placementSight(top, { x: -1, y: 1, z: 0 }, world({ '0,0,0': block('stone') }), hasBox), null)
  const bottom = job({ block: { name: 'oak_slab' }, half: 'bottom' })
  assert.equal(placementSight(bottom, { x: -1, y: 0, z: 0 }, world({ '0,2,0': block('stone') }), hasBox), null)
  assert.ok(placementSight(top, { x: -1, y: 1, z: 0 }, world({ '1,1,0': block('stone') }), hasBox))
})

test('top stairs need a compatible underside or upper side click, not the floor top', () => {
  const top = job({ block: { name: 'oak_stairs' }, half: 'top', facing: 'north' })
  assert.equal(placementSight(top, { x: -1, y: 1, z: 0 }, world({ '0,0,0': block('stone') }), hasBox), null)
  assert.deepEqual(placementSight(top, { x: -1, y: 1, z: 0 }, world({ '1,1,0': block('stone') }), hasBox), { dx: 1, dy: 0, dz: 0 })
})

test('floor torches on a fence and side clicks on partial slab supports stay legal', () => {
  assert.deepEqual(placementSight(job({ block: { name: 'torch' }, against: { dx: 0, dy: -1, dz: 0 } }), { x: -1, y: 1, z: 0 }, world({ '0,0,0': block('oak_fence') }), hasBox), { dx: 0, dy: -1, dz: 0 })
  assert.ok(placementSight(job({ against: { dx: 1, dy: 0, dz: 0 } }), { x: -1, y: 1, z: 0 }, world({ '1,1,0': block('oak_slab', { type: 'bottom' }) }), hasBox))
  assert.equal(placementSight(job({ against: { dx: 1, dy: 0, dz: 0 } }), { x: 2, y: 1, z: 0 }, world({ '1,1,0': block('oak_slab', { type: 'bottom' }) }), hasBox), null)
})

test('wall attachments and ceiling attachments cannot switch to an unrelated support', () => {
  const w = world({ '0,0,0': block('stone'), '1,1,0': block('stone') })
  assert.equal(placementSight(job({ block: { name: 'wall_torch' }, against: { dx: -1, dy: 0, dz: 0 } }), { x: -1, y: 1, z: 1 }, w, hasBox), null)
  assert.ok(placementSight(job({ block: { name: 'lantern' }, against: { dx: 0, dy: 1, dz: 0 } }), { x: -1, y: 0, z: 0 }, world({ '0,2,0': block('stone') }), hasBox))
})

test('a complete roof does not prevent resumed interior bed placement', () => {
  const text = '---\nname: r\ntitle: R\ndescription: d\ntags: shelter\nfront: south\nfoundation: flat\n---\n\n```legend\nS stone\nF white_bed[facing=south,part=foot]\nH white_bed[facing=south,part=head]\n```\n\n## y0\n```layer\nF.\nH.\n```\n\n## y2\n```layer\nSS\nSS\n```\n'
  const bp = resolve(parseBlueprint(text)); const at = { x: 0, y: 1, z: 0 }
  const base = flatGround(0)
  const w = (x, y, z) => y === 3 && x >= 0 && x < 2 && z >= 0 && z < 2 ? block('stone') : base(x, y, z)
  const ordered = orderJobs(jobsFor(bp, at, w), bp, at, w)
  assert.equal(ordered.unreachable.length, 0)
  assert.equal(ordered.jobs.filter(j => j.item === 'white_bed').length, 1)
  assert.ok(ordered.jobs.find(j => j.item === 'white_bed').against)
})
