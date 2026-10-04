import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = (x, y, z) => ({ x, y, z })
// a fence ring around x 10..16, z -3..3 (cells), gate in the west side at (10, 0)
const ring = [
  ...[10, 11, 12, 13, 14, 15, 16].flatMap(x => [`${x},64,-3`, `${x},64,3`]),
  ...[-2, -1, 1, 2].map(z => `10,64,${z}`),
  ...[-2, -1, 0, 1, 2].map(z => `16,64,${z}`)
]
const spec = (open, cowExtra = {}, cowZ = 0.5) => ({
  self: { pos: at(4.5, 64, 0.5) },
  blocks: { ...Object.fromEntries(ring.map(k => [k, 'oak_fence'])), '10,64,0': 'oak_fence_gate' },
  states: { '10,64,0': { open } },
  entities: [{ id: 1, name: 'cow', kind: 'passive', pos: at(3.5, 64, cowZ), ...cowExtra }],
  inventory: [{ name: 'lead', count: 1 }]
})
const fake = async s => { const p = createFake(s); p.setOwner('t'); await p.interact('t', { id: 1, item: 'lead' }); p.world.state.self.pos = at(7.5, 64, 0.5); return p }
const cowOf = p => p.world.state.entities.find(e => e.id === 1)
const behind = p => Math.hypot(cowOf(p).pos.x - p.self().pos.x, cowOf(p).pos.z - p.self().pos.z)
const step = (p, x) => p.moveTo('t', { pos: at(x, 64, 0.5), range: 0 })
const xs = [8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5]

test('short steps through an open gate: the cow paths behind the body and ends inside the pen', async () => {
  const p = await fake(spec(true))
  for (const x of xs) {
    await step(p, x)
    assert.ok(behind(p) >= 3.29 && behind(p) <= 3.5, `body ${x}: ${behind(p)}`)
  }
  assert.ok(cowOf(p).pos.x > 11, `at ${cowOf(p).pos.x}`)
})

test('short steps with the gate shut: the cow stays outside', async () => {
  const p = await fake(spec(false))
  for (const x of xs) await step(p, x)
  assert.ok(cowOf(p).pos.x < 10, `at ${cowOf(p).pos.x}`)
})

test('one long walk drags the cow in a straight line, stopping outside at the gate', async () => {
  const p = await fake(spec(true))
  await step(p, 15.5)
  assert.ok(cowOf(p).pos.x < 10 && cowOf(p).pos.x > 9, `at ${cowOf(p).pos.x}`)
})

test('a pinned cow never moves in short steps', async () => {
  const p = await fake(spec(true, { pin: true }))
  for (const x of xs.slice(0, 2)) await step(p, x)
  assert.deepEqual(cowOf(p).pos, at(3.5, 64, 0.5))
})

test('a cow two blocks off the axis ends on the axis and passes the gate', async () => {
  const p = await fake(spec(true, {}, 2.5))
  for (const x of xs) await step(p, x)
  assert.ok(cowOf(p).pos.x > 11, `at ${JSON.stringify(cowOf(p).pos)}`)
})
