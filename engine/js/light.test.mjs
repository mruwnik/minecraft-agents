import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { emitOf, lightTable, relightBox, boxIndex } from './light.mjs'

const require = createRequire(import.meta.url)
const registry = require('prismarine-registry')('26.1')
const Block = require('prismarine-block')(registry)
const table = lightTable(registry)

const sid = (name, props = {}) => Block.fromProperties(registry.blocksByName[name].id, props, 0).stateId
const AIR = sid('air')
const STONE = sid('stone')

const makeBox = (size, fill = AIR) => {
  const n = size[0] * size[1] * size[2]
  return { size, states: new Uint16Array(n).fill(fill), sky: new Uint8Array(n), block: new Uint8Array(n) }
}
const at = (b, x, y, z) => boxIndex(b.size, x, y, z)
const forEachCell = (b, fn) => {
  for (let y = 0; y < b.size[1]; y++) for (let z = 0; z < b.size[2]; z++) for (let x = 0; x < b.size[0]; x++) fn(x, y, z, at(b, x, y, z))
}
const relight = (b) => relightBox({ table, ...b })
// light value at (x,y,z) of a result
const get = (res, b, kind, x, y, z) => res[kind][at(b, x, y, z)]
// box with sky 15 on the top shell layer
const skyBox = (size) => {
  const b = makeBox(size)
  forEachCell(b, (x, y, z, i) => { b.sky[i] = y === size[1] - 1 ? 15 : 0 })
  return b
}
const fillLayer = (b, y, state, pred = () => true) =>
  forEachCell(b, (x, yy, z, i) => { b.states[i] = yy === y && x > 0 && z > 0 && x < b.size[0] - 1 && z < b.size[2] - 1 && pred(x, z) ? state : b.states[i] })

const emitCases = [
  ['glowstone', {}, 15], ['torch', {}, 14], ['wall_torch', { facing: 'north' }, 14], ['lantern', {}, 15], ['lava', {}, 15],
  ['furnace', { lit: false }, 0], ['furnace', { lit: true }, 13],
  ['campfire', { lit: true }, 15], ['campfire', { lit: false }, 0],
  ['redstone_lamp', { lit: true }, 15], ['redstone_lamp', { lit: false }, 0],
  ['sea_pickle', { pickles: 3, waterlogged: true }, 12], ['sea_pickle', { pickles: 3, waterlogged: false }, 0],
  ['light', { level: 7 }, 7], ['candle', { candles: 2, lit: true }, 6], ['candle', { candles: 2, lit: false }, 0],
  ['respawn_anchor', { charges: 0 }, 0], ['respawn_anchor', { charges: 2 }, 7], ['respawn_anchor', { charges: 4 }, 15],
  ['cave_vines', { berries: true }, 14], ['cave_vines_plant', { berries: false }, 0],
  ['glow_lichen', {}, 7], ['sculk_catalyst', {}, 6],
  ['redstone_ore', { lit: true }, 9], ['redstone_ore', { lit: false }, 0], ['redstone_torch', { lit: true }, 7], ['redstone_torch', { lit: false }, 0],
  ['copper_bulb', { lit: true }, 15], ['exposed_copper_bulb', { lit: true }, 12], ['waxed_weathered_copper_bulb', { lit: true }, 8], ['oxidized_copper_bulb', { lit: true }, 4],
  ['copper_bulb', { lit: false }, 0], ['crying_obsidian', {}, 10], ['beacon', {}, 15], ['end_rod', { facing: 'up' }, 14], ['stone', {}, 0]
]
for (const [name, props, want] of emitCases) {
  test(`emit ${name} ${JSON.stringify(props)} = ${want}`, () => {
    assert.equal(table.emit[sid(name, props)], want)
  })
}

test('emitOf is applied to name, props and base', () => {
  assert.equal(emitOf('torch', {}, 14), 14)
  assert.equal(emitOf('furnace', { lit: true }, 0), 13)
  assert.equal(emitOf('mystery', { lit: false }, 9), 0)
  assert.equal(emitOf('stone', undefined, 0), 0)
})

const filterCases = [
  ['stone', {}, 15], ['glass', {}, 0], ['oak_leaves', { persistent: true }, 1], ['water', { level: 0 }, 1],
  ['oak_slab', { type: 'bottom', waterlogged: true }, 1], ['oak_slab', { type: 'bottom', waterlogged: false }, 0], ['air', {}, 0]
]
for (const [name, props, want] of filterCases) {
  test(`filter ${name} ${JSON.stringify(props)} = ${want}`, () => {
    assert.equal(table.filter[sid(name, props)], want)
  })
}

const faceCases = [
  ['bottom slab', 'oak_slab', { type: 'bottom', waterlogged: false }, 0b000001],
  ['top slab', 'oak_slab', { type: 'top', waterlogged: false }, 0b000010],
  ['snow layers=4', 'snow', { layers: 4 }, 0b000001],
  ['stairs north bottom', 'oak_stairs', { facing: 'north', half: 'bottom', shape: 'straight', waterlogged: false }, 0b000101],
  ['farmland', 'farmland', { moisture: 0 }, 0b000001],
  ['stone', 'stone', {}, 0],
  ['air', 'air', {}, 0],
  ['double slab', 'oak_slab', { type: 'double', waterlogged: false }, 0]
]
for (const [label, name, props, want] of faceCases) {
  test(`faces ${label}`, () => {
    assert.equal(table.faces[sid(name, props)], want)
  })
}

test('torch alone: light falls off by Manhattan distance', () => {
  const b = makeBox([31, 31, 31])
  b.states[at(b, 15, 15, 15)] = sid('torch')
  const r = relight(b)
  const cases = [[0, 0, 0, 14], [3, 0, 0, 11], [0, 2, 1, 11], [13, 0, 0, 1], [0, 0, 14, 0], [0, 14, 0, 0]]
  for (const [dx, dy, dz, want] of cases) {
    assert.equal(get(r, b, 'block', 15 + dx, 15 + dy, 15 + dz), want, `offset ${dx},${dy},${dz}`)
  }
})

test('a stone wall with a hole forces light around it', () => {
  const b = makeBox([15, 15, 15])
  forEachCell(b, (x, y, z, i) => { b.states[i] = x === 5 && y > 0 && z > 0 && y < 14 && z < 14 ? STONE : AIR })
  b.states[at(b, 5, 7, 8)] = AIR
  b.states[at(b, 3, 7, 7)] = sid('torch')
  const r = relight(b)
  const cases = [[4, 7, 7, 13], [5, 7, 7, 0], [5, 7, 8, 11], [6, 7, 7, 9], [7, 7, 7, 8]]
  for (const [x, y, z, want] of cases) assert.equal(get(r, b, 'block', x, y, z), want, `cell ${x},${y},${z}`)
})

test('emitting opaque block keeps its own light and lights neighbours', () => {
  const b = makeBox([9, 9, 9])
  b.states[at(b, 4, 4, 4)] = sid('glowstone')
  const r = relight(b)
  assert.deepEqual([4, 5].map((x) => get(r, b, 'block', x, 4, 4)), [15, 14])
})

test('lit furnace cell holds 13', () => {
  const b = makeBox([9, 9, 9])
  b.states[at(b, 4, 4, 4)] = sid('furnace', { facing: 'north', lit: true })
  const r = relight(b)
  assert.deepEqual([4, 5].map((x) => get(r, b, 'block', x, 4, 4)), [13, 12])
})

test('open sky: every interior cell is 15', () => {
  const b = skyBox([9, 20, 9])
  const r = relight(b)
  const bad = []
  forEachCell(b, (x, y, z, i) => {
    const interior = x > 0 && x < 8 && y > 0 && y < 19 && z > 0 && z < 8
    if (interior && r.sky[i] !== 15) bad.push([x, y, z])
  })
  assert.deepEqual(bad, [])
})

test('stone roof with a hole: 15 in the shaft, falling off sideways', () => {
  const b = skyBox([11, 12, 11])
  fillLayer(b, 10, STONE, (x, z) => !(x === 5 && z === 5))
  const r = relight(b)
  const cases = [[5, 9, 5, 15], [5, 1, 5, 15], [6, 5, 5, 14], [7, 5, 5, 13], [5, 5, 3, 13], [8, 9, 5, 12], [2, 9, 2, 9]]
  for (const [x, y, z, want] of cases) assert.equal(get(r, b, 'sky', x, y, z), want, `cell ${x},${y},${z}`)
})

test('leaves layer: 14 inside, 13 below, then 12', () => {
  const b = skyBox([7, 14, 7])
  fillLayer(b, 7, sid('oak_leaves', { persistent: true }))
  const r = relight(b)
  assert.deepEqual([8, 7, 6, 5].map((y) => get(r, b, 'sky', 3, y, 3)), [15, 14, 13, 12])
})

test('water column: 14, 13, 12, 11 then 10 below', () => {
  const b = skyBox([7, 16, 7])
  const water = sid('water', { level: 0 })
  for (const y of [10, 9, 8, 7]) fillLayer(b, y, water)
  const r = relight(b)
  assert.deepEqual([11, 10, 9, 8, 7, 6].map((y) => get(r, b, 'sky', 3, y, 3)), [15, 14, 13, 12, 11, 10])
})

test('bottom slab: blocks sky going down, passes it sideways', () => {
  const b = skyBox([14, 12, 7])
  fillLayer(b, 6, sid('oak_slab', { type: 'bottom', waterlogged: false }), (x) => x <= 5)
  const r = relight(b)
  const cases = [[3, 7, 3, 15], [3, 6, 3, 15], [5, 5, 3, 14], [4, 5, 3, 13], [1, 5, 3, 10], [8, 5, 3, 15]]
  for (const [x, y, z, want] of cases) assert.equal(get(r, b, 'sky', x, y, z), want, `cell ${x},${y},${z}`)
})

test('top slab: sky enters from the side but not from above', () => {
  const b = skyBox([14, 12, 7])
  fillLayer(b, 6, sid('oak_slab', { type: 'top', waterlogged: false }), (x) => x <= 5)
  const r = relight(b)
  assert.equal(get(r, b, 'sky', 3, 7, 3), 15)
  assert.equal(get(r, b, 'sky', 3, 6, 3), 12, 'slab cell lit from the side: 3 cells under open air at x=6')
})

test('shell values are never changed', () => {
  const b = makeBox([9, 9, 9])
  forEachCell(b, (x, y, z, i) => { b.sky[i] = (x * 7 + y * 3 + z) % 16; b.block[i] = (x + y * 5 + z * 11) % 16 })
  b.states[at(b, 4, 4, 4)] = sid('glowstone')
  const r = relight(b)
  const bad = []
  forEachCell(b, (x, y, z, i) => {
    const shell = x === 0 || y === 0 || z === 0 || x === 8 || y === 8 || z === 8
    if (shell && (r.sky[i] !== b.sky[i] || r.block[i] !== b.block[i])) bad.push([x, y, z])
  })
  assert.deepEqual(bad, [])
})

test('shell light flows into the interior', () => {
  const b = makeBox([9, 9, 9])
  b.block[at(b, 0, 4, 4)] = 12
  const r = relight(b)
  assert.deepEqual([1, 2, 3].map((x) => get(r, b, 'block', x, 4, 4)), [11, 10, 9])
})

test('a removed torch comes back 0: old interior values are ignored', () => {
  const b = makeBox([15, 15, 15])
  forEachCell(b, (x, y, z, i) => { b.block[i] = x > 0 && x < 14 && y > 0 && y < 14 && z > 0 && z < 14 ? 15 : 0 })
  const r = relight(b)
  assert.equal(r.block.every((v) => v === 0), true)
})

test('input arrays are not mutated', () => {
  const b = makeBox([9, 9, 9])
  b.states[at(b, 4, 4, 4)] = sid('torch')
  relight(b)
  assert.equal(b.block.every((v) => v === 0), true)
})

const randomBox = (size, seed) => {
  let s = seed
  const rnd = () => ((s = (s * 1664525 + 1013904223) >>> 0) / 2 ** 32)
  const b = makeBox(size)
  forEachCell(b, (x, y, z, i) => {
    b.states[i] = rnd() < 0.2 ? STONE : AIR
    b.sky[i] = y === size[1] - 1 ? 15 : 0
  })
  for (let k = 0; k < 4; k++) b.states[at(b, 5 + k * 6, 10 + k, 16)] = sid('torch')
  return b
}

for (const size of [[33, 33, 33], [33, 80, 33]]) {
  test(`performance ${size.join('x')}`, () => {
    const b = randomBox(size, 42)
    const times = [0, 1, 2].map(() => { const t = performance.now(); relight(b); return performance.now() - t })
    console.log(`relight ${size.join('x')}: first ${times[0].toFixed(1)} ms, best ${Math.min(...times).toFixed(1)} ms`)
    assert.ok(times[0] < 30, `took ${times[0]} ms`)
  })
}
