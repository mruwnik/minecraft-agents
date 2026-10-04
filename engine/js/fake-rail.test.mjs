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

// ---- the connection rule: a rail takes its shape when placed, from the rails beside it that still have a free end

const bigFloor = Object.fromEntries(Array.from({ length: 12 }, (_, x) => Array.from({ length: 12 }, (_, z) => [`${x},63,${z}`, 'stone'])).flat())
const placing = (extra = {}, body = at(4, 64, 2)) => ({
  self: { pos: body },
  blocks: { ...bigFloor, ...extra },
  inventory: [{ name: 'rail', count: 40 }, { name: 'powered_rail', count: 40 }]
})
const placeAll = async (p, cells, item = 'rail') => {
  for (const [x, y, z] of cells) assert.equal((await p.place('t', { pos: at(x, y, z), item })).status, 'placed')
}
const shapes = (p, cells) => cells.map(([x, y, z]) => p.blockAt(at(x, y, z)).properties.shape)
const perms = xs => xs.length < 2 ? [xs] : xs.flatMap((x, i) => perms([...xs.slice(0, i), ...xs.slice(i + 1)]).map(r => [x, ...r]))

const straight = [[2, 64, 0], [3, 64, 0], [4, 64, 0], [5, 64, 0], [6, 64, 0]]
const corner = [[1, 64, 0], [2, 64, 0], [3, 64, 0], [4, 64, 0], [4, 64, 1], [4, 64, 2], [4, 64, 3]]
const cornerShapes = ['east_west', 'east_west', 'east_west', 'south_west', 'north_south', 'north_south', 'north_south']
const climb = [[1, 64, 0], [2, 64, 0], [3, 65, 0], [4, 65, 0]]
const descend = [[1, 65, 0], [2, 65, 0], [3, 64, 0], [4, 64, 0]]

test('a straight line comes out straight whatever order its rails are placed in', async () => {
  for (const order of [[2, 0, 4, 1, 3], [0, 1, 2, 3, 4], [4, 3, 2, 1, 0], [1, 3, 0, 4, 2]]) {
    const p = fake(placing())
    await placeAll(p, order.map(i => straight[i]))
    assert.deepEqual(shapes(p, straight), Array(5).fill('east_west'), order.join())
  }
})

test('an L comes out right in every order: arms straight, the corner a corner', async () => {
  for (const order of perms([0, 1, 2, 3, 4, 5, 6]).filter((_, i) => i % 97 === 0)) {
    const p = fake(placing())
    await placeAll(p, order.map(i => corner[i]))
    assert.deepEqual(shapes(p, corner), cornerShapes, order.join())
  }
})

test('the four corners take their shapes from the two arms they join', async () => {
  for (const [cells, shape] of [
    [[[2, 64, 5], [3, 64, 5], [4, 64, 5], [4, 64, 6], [4, 64, 7]], 'south_west'],
    [[[2, 64, 5], [3, 64, 5], [4, 64, 5], [4, 64, 4], [4, 64, 3]], 'north_west'],
    [[[6, 64, 5], [5, 64, 5], [4, 64, 5], [4, 64, 6], [4, 64, 7]], 'south_east'],
    [[[6, 64, 5], [5, 64, 5], [4, 64, 5], [4, 64, 4], [4, 64, 3]], 'north_east']
  ]) {
    const p = fake(placing({}, at(4, 64, 5)))
    await placeAll(p, cells)
    assert.equal(shapes(p, [cells[2]])[0], shape)
  }
})

test('a slope comes out right in every order: the lower rail climbs towards the upper one', async () => {
  for (const order of perms([0, 1, 2, 3])) {
    const p = fake(placing({ '3,64,0': 'stone', '4,64,0': 'stone' }))
    await placeAll(p, order.map(i => climb[i]))
    assert.deepEqual(shapes(p, climb), ['east_west', 'ascending_east', 'east_west', 'east_west'], order.join())
  }
})

test('a slope down is the same climb the other way', async () => {
  for (const order of perms([0, 1, 2, 3])) {
    const p = fake(placing({ '1,64,0': 'stone', '2,64,0': 'stone' }))
    await placeAll(p, order.map(i => descend[i]))
    assert.deepEqual(shapes(p, descend), ['east_west', 'east_west', 'ascending_west', 'east_west'], order.join())
  }
})

test('a rail placed beside the end of a line bends that end, one beside a middle rail changes nothing', async () => {
  const end = fake(placing())
  await placeAll(end, straight)
  await placeAll(end, [[6, 64, 1]])
  assert.equal(shapes(end, [[6, 64, 0]])[0], 'south_west')
  assert.equal(shapes(end, [[6, 64, 1]])[0], 'north_south')
  const middle = fake(placing())
  await placeAll(middle, straight)
  await placeAll(middle, [[4, 64, 1]])
  assert.deepEqual(shapes(middle, straight), Array(5).fill('east_west'))
  assert.equal(shapes(middle, [[4, 64, 1]])[0], 'north_south')
})

test('a lone rail takes its first shape from where the body stands: along x when it stands out along x', async () => {
  for (const [body, shape] of [[at(5, 64, 3), 'north_south'], [at(8, 64, 0), 'east_west'], [at(2, 64, 0), 'east_west']]) {
    const p = fake(placing({}, body))
    await placeAll(p, [[5, 64, 0]])
    assert.equal(shapes(p, [[5, 64, 0]])[0], shape)
  }
})

test('a powered rail never takes a corner shape', async () => {
  const p = fake(placing())
  await placeAll(p, [[1, 64, 0], [2, 64, 0]])
  await placeAll(p, [[3, 64, 0]], 'powered_rail')
  await placeAll(p, [[3, 64, 1], [3, 64, 2]])
  assert.match(shapes(p, [[3, 64, 0]])[0], /^(north_south|east_west)$/)
})

test('a broken rail leaves its neighbours as they are, and placing it again joins them as before', async () => {
  const p = fake(placing())
  await placeAll(p, corner)
  await p.dig('t', { pos: at(4, 64, 0) })
  assert.deepEqual(shapes(p, [[3, 64, 0], [4, 64, 1]]), ['east_west', 'north_south'])
  await placeAll(p, [[4, 64, 0]])
  assert.deepEqual(shapes(p, corner), cornerShapes)
})

test('rails written into the world read as if they had been placed in line order', () => {
  const p = fake({ blocks: { '0,64,0': 'rail', '1,64,0': 'rail', '2,64,0': 'rail', '2,64,1': 'rail', '2,64,2': 'rail', '5,64,5': 'rail', '6,65,5': 'rail', '7,65,5': 'rail' } })
  assert.deepEqual([[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]].map(([x, z]) => props(p, x, z).shape),
    ['east_west', 'east_west', 'south_west', 'north_south', 'north_south'])
  assert.deepEqual([[5, 64], [6, 65], [7, 65]].map(([x, y]) => p.blockAt(at(x, y, 5)).properties.shape),
    ['ascending_east', 'east_west', 'east_west'])
})

test('power runs along slopes and stops at a normal rail', () => {
  const p = fake({ blocks: { '3,64,0': 'powered_rail', '4,65,0': 'powered_rail', '5,66,0': 'powered_rail', '6,66,0': 'powered_rail', '7,66,0': 'rail', '8,66,0': 'powered_rail', '4,65,-1': 'redstone_torch' } })
  assert.deepEqual([[3, 64], [4, 65], [5, 66], [6, 66], [8, 66]].map(([x, y]) => p.blockAt(at(x, y, 0)).properties.powered),
    [true, true, true, true, false])
})
