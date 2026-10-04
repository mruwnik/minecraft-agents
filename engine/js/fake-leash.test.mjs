import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = (x, y, z) => ({ x, y, z })
const cow = (extra = {}) => ({ id: 1, name: 'cow', kind: 'passive', pos: at(1, 64, 0), ...extra })
const fenced = { blocks: { '5,64,0': 'oak_fence' }, entities: [cow()], inventory: [{ name: 'lead', count: 1 }] }
const fake = spec => { const p = createFake(spec); p.setOwner('t'); return p }
const cowOf = p => p.world.state.entities.find(e => e.id === 1)
const leads = p => p.world.state.entities.filter(e => e.kind === 'item' && e.item.name === 'lead').length

test('a lead on a cow marks it leashed to the body, the entities reading shows it', async () => {
  const p = fake(fenced)
  await p.interact('t', { id: 1, item: 'lead' })
  const seen = p.entities({}).find(e => e.id === 1)
  assert.deepEqual([seen.leashed, seen.leashedToMe], [true, true])
})

test('a led animal follows the body when it walks', async () => {
  const p = fake(fenced)
  await p.interact('t', { id: 1, item: 'lead' })
  await p.moveTo('t', { pos: at(30, 64, 0), range: 1 })
  assert.equal(Math.hypot(cowOf(p).pos.x - 30, cowOf(p).pos.z), 2)
  assert.equal(cowOf(p).leashedToMe, true)
})

test('a lead on a snapping animal breaks on the walk and drops as an item', async () => {
  const p = fake({ ...fenced, entities: [cow({ snaps: true })] })
  await p.interact('t', { id: 1, item: 'lead' })
  await p.moveTo('t', { pos: at(30, 64, 0), range: 1 })
  assert.equal(cowOf(p).leashedToMe, false)
  assert.equal(leads(p), 1)
})

const knotOf = p => p.world.state.entities.find(e => e.name === 'leash_knot')
const tie = async (p, item) => {
  await p.interact('t', { id: 1, item: 'lead' })
  return p.useOn('t', { pos: at(5, 64, 0), ...(item && { item }) })
}

test('a click on a fence ties the led animal to a knot there, empty hand or lead; the body no longer holds it', async () => {
  for (const item of [undefined, 'lead']) {
    const p = fake({ ...fenced, inventory: [{ name: 'lead', count: 2 }], self: { pos: at(4, 64, 0) } })
    const r = await tie(p, item)
    assert.equal(r.status, 'used')
    assert.deepEqual([cowOf(p).leashed, cowOf(p).leashedToMe, cowOf(p).leashHolder], [true, false, knotOf(p).id])
  }
})

test('a click on a fence with nothing led changes nothing', async () => {
  const p = fake({ ...fenced, self: { pos: at(4, 64, 0) } })
  const r = await p.useOn('t', { pos: at(5, 64, 0) })
  assert.equal(r.status, 'unchanged')
  assert.equal(knotOf(p), undefined)
})

test('an empty hand on the knot removes it and hands the animal to the body; a click on the animal then drops the lead', async () => {
  const p = fake({ ...fenced, self: { pos: at(4, 64, 0) } })
  await tie(p)
  const r = await p.interact('t', { id: knotOf(p).id })
  assert.equal(r.status, 'used')
  assert.deepEqual([cowOf(p).leashed, cowOf(p).leashedToMe], [true, true])
  assert.equal(knotOf(p), undefined)
  assert.equal(leads(p), 0)
  await p.interact('t', { id: 1 })
  assert.equal(cowOf(p).leashed, false)
  assert.equal(leads(p), 1)
})

test('an empty hand on a cow tied to a fence does not free it', async () => {
  const p = fake({ ...fenced, self: { pos: at(2, 64, 0) } })
  await tie(p)
  const r = await p.interact('t', { id: 1 })
  assert.equal(r.leash, null)
  assert.equal(cowOf(p).leashed, true)
})
