import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = (x, y, z) => ({ x, y, z })
const cow = (id, x, z, extra = {}) => ({ id, name: 'cow', kind: 'passive', pos: at(x, 64, z), ...extra })
// a fence line at x 10, z -5..5, with a gate at z 0
const wall = Object.fromEntries([-5, -4, -3, -2, -1, 1, 2, 3, 4, 5].map(z => [`10,64,${z}`, 'oak_fence']))
const fenced = (open, entities) => ({
  self: { pos: at(14, 64, 0) },
  blocks: { ...wall, '10,64,0': 'oak_fence_gate' },
  states: { '10,64,0': { open } },
  entities,
  inventory: [{ name: 'wheat', count: 4 }, { name: 'lead', count: 1 }]
})
const fake = spec => { const p = createFake(spec); p.setOwner('t'); return p }
const cowOf = (p, id) => p.world.state.entities.find(e => e.id === id)
const dist = (a, b) => Math.hypot(a.x - b.x, a.z - b.z)

test('an animal walks to the body holding its food and stops short of it', async () => {
  const p = fake({ self: { pos: at(0, 64, 0) }, entities: [cow(1, 6, 0)], inventory: [{ name: 'wheat', count: 1 }] })
  await p.equip('t', { item: 'wheat' })
  await p.wait('t', { ms: 500 })
  assert.equal(Math.round(dist(cowOf(p, 1).pos, at(0, 64, 0)) * 10) / 10, 2.5)
})

test('an animal is not drawn by an empty hand, by food it does not eat or from beyond the range', async () => {
  for (const [label, held, x] of [['empty', null, 6], ['lead', 'lead', 6], ['far', 'wheat', 12]]) {
    const p = fake({ self: { pos: at(0, 64, 0) }, entities: [cow(1, x, 0)], inventory: [{ name: 'wheat', count: 1 }, { name: 'lead', count: 1 }] })
    if (held) await p.equip('t', { item: held })
    await p.wait('t', { ms: 500 })
    assert.deepEqual(cowOf(p, 1).pos, at(x, 64, 0), label)
  }
})

test('a shut gate keeps the animal on its side, against the fence', async () => {
  const p = fake(fenced(false, [cow(1, 6, 3)]))
  await p.equip('t', { item: 'wheat' })
  await p.wait('t', { ms: 500 })
  assert.ok(cowOf(p, 1).pos.x < 10, 'still west of the fence')
})

test('an open gate is walked through, also from off its axis', async () => {
  const p = fake(fenced(true, [cow(1, 6, 3)]))
  await p.equip('t', { item: 'wheat' })
  await p.wait('t', { ms: 500 })
  assert.ok(cowOf(p, 1).pos.x >= 11, `east of the gate, at ${JSON.stringify(cowOf(p, 1).pos)}`)
})

test('a walking body draws the animal too; a leashed one is left to its lead', async () => {
  const p = fake({ self: { pos: at(0, 64, 0) }, entities: [cow(1, 3, 0), cow(2, 3, 2, { leashed: true, leashedToMe: true })], inventory: [{ name: 'wheat', count: 1 }] })
  await p.equip('t', { item: 'wheat' })
  await p.moveTo('t', { pos: at(-6, 64, 0), range: 0 })
  assert.equal(Math.round(dist(cowOf(p, 1).pos, at(-6, 64, 0)) * 10) / 10, 2.5)
  assert.deepEqual(cowOf(p, 2).pos, at(-8, 64, 0), 'the lead drags it as before')
})
