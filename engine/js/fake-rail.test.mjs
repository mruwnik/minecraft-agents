import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = (x, y, z) => ({ x, y, z })
const fake = spec => { const p = createFake(spec); p.setOwner('t'); return p }
const floor = Object.fromEntries(Array.from({ length: 30 }, (_, x) => [`${x},63,0`, 'stone']))
const row = (name, xs) => Object.fromEntries(xs.map(x => [`${x},64,0`, name]))
const props = (p, x, z = 0) => p.blockAt(at(x, 64, z)).properties
const range = (a, b) => Array.from({ length: b - a + 1 }, (_, i) => a + i)

test('rails in a row along x read east_west, along z north_south, and a lone rail north_south', () => {
  const p = fake({ blocks: { ...row('rail', [2, 3, 4]), '10,64,5': 'rail', '10,64,6': 'powered_rail', '20,64,0': 'rail' } })
  assert.deepEqual([2, 3, 4].map(x => props(p, x).shape), ['east_west', 'east_west', 'east_west'])
  assert.equal(props(p, 10, 5).shape, 'north_south')
  assert.equal(props(p, 10, 6).shape, 'north_south')
  assert.equal(props(p, 20).shape, 'north_south')
})

test('a placed rail reports the shape it reads with after joining its neighbour', async () => {
  const p = fake({ self: { pos: at(3, 64, 2) }, blocks: { ...floor, '2,64,0': 'rail' }, inventory: [{ name: 'rail', count: 1 }] })
  const r = await p.place('t', { pos: at(3, 64, 0), item: 'rail' })
  assert.equal(r.status, 'placed')
  assert.equal(r.placed.properties.shape, 'east_west')
  assert.equal(props(p, 2).shape, 'east_west')
})

test('a redstone block under a powered rail lights it and 8 more each way along the run, not the 9th', () => {
  const p = fake({ blocks: { ...row('powered_rail', range(0, 20)), '10,63,0': 'redstone_block' } })
  assert.deepEqual(range(0, 20).map(x => props(p, x).powered),
    range(0, 20).map(x => Math.abs(x - 10) <= 8))
})

test('a redstone torch beside a powered rail lights it, a lever only when switched on, nothing leaves it unlit', () => {
  for (const [label, source, state, lit] of [
    ['torch', 'redstone_torch', undefined, true],
    ['lever on', 'lever', { powered: true }, true],
    ['lever off', 'lever', { powered: false }, false],
    ['stone', 'stone', undefined, false]
  ]) {
    const p = fake({ blocks: { ...row('powered_rail', [5]), '5,64,1': source }, states: state ? { '5,64,1': state } : {} })
    assert.equal(props(p, 5).powered, lit, label)
  }
})

test('a normal rail breaks the run of power', () => {
  const p = fake({ blocks: { ...row('powered_rail', [0, 1, 3, 4]), '2,64,0': 'rail', '0,63,0': 'redstone_block' } })
  assert.deepEqual([0, 1, 3, 4].map(x => props(p, x).powered), [true, true, false, false])
})

test('a stored state wins over the worked-out one', () => {
  const p = fake({ blocks: row('rail', [2, 3, 4]), states: { '3,64,0': { shape: 'north_south' } } })
  assert.equal(props(p, 3).shape, 'north_south')
  assert.equal(props(p, 2).shape, 'east_west')
})
