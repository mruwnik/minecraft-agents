// Why JavaScript: tests lazy-chunk.mjs, which stays JS: it patches prismarine-chunk's binary column loading.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import vec3 from 'vec3'
import prismarineRegistry from 'prismarine-registry'
import { installLazyChunks } from './lazy-chunk.mjs'

const { Vec3 } = vec3
const require = createRequire(import.meta.url)
const registry = prismarineRegistry('26.1')
const EagerClass = require('prismarine-chunk')(registry)
installLazyChunks()
const LazyClass = require('prismarine-chunk')(registry)
const ids = ['stone', 'dirt', 'water', 'lava', 'oak_log', 'gravel', 'granite', 'andesite', 'diorite', 'sand', 'clay', 'coal_ore', 'iron_ore', 'oak_leaves']
  .map(name => registry.blocksByName[name].defaultState)
const maxState = registry.blocksArray.reduce((m, b) => Math.max(m, b.maxStateId), 0)

let seed = 7
const rand = n => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return (seed >> 8) % n }

// sections of every container kind: single value, small palette, large palette, global (direct) palette
function sourceColumn () {
  const column = new EagerClass()
  const fill = (y0, count, pick) => {
    for (let i = 0; i < count; i++) column.setBlockStateId(new Vec3(rand(16), y0 + rand(16), rand(16)), pick())
  }
  column.setBlockStateId(new Vec3(0, -64, 0), ids[0])
  fill(-48, 300, () => ids[rand(3)])
  fill(-32, 600, () => ids[rand(ids.length)])
  fill(0, 4096, () => 1 + rand(maxState))
  fill(64, 50, () => ids[2])
  for (let i = 0; i < 400; i++) {
    column.setSkyLight(new Vec3(rand(16), -64 + rand(384), rand(16)), rand(16))
    column.setBlockLight(new Vec3(rand(16), -64 + rand(384), rand(16)), rand(16))
  }
  for (let i = 0; i < 40; i++) column.setBiome(new Vec3(rand(16), -64 + rand(384), rand(16)), rand(5))
  return column
}

const loadInto = (column, source) => {
  column.load(source.dump())
  const light = source.dumpLight()
  column.loadParsedLight(light.skyLight, light.blockLight, light.skyLightMask, light.blockLightMask, light.emptySkyLightMask, light.emptyBlockLightMask)
  return column
}
const load = (Class, source) => loadInto(new Class(), source)

const readAll = column => {
  const out = []
  for (let y = -64; y < 320; y += 3) {
    for (let x = 0; x < 16; x += 5) {
      for (let z = 0; z < 16; z += 3) {
        const p = new Vec3(x, y, z)
        out.push([column.getBlockStateId(p), column.getSkyLight(p), column.getBlockLight(p), column.getBiome(p)])
      }
    }
  }
  return out
}

test('a lazily loaded column reads the same states, light and biomes as an eager one', () => {
  const source = sourceColumn()
  assert.deepEqual(readAll(load(LazyClass, source)), readAll(load(EagerClass, source)))
})

test('a section nobody read stays unparsed, and reading one block parses only its section', () => {
  const column = load(LazyClass, sourceColumn())
  const index = (70 + 64) >> 4
  column.getBlockStateId(new Vec3(1, 70, 1))
  assert.ok(Object.hasOwn(column.sections[index], 'data'))
  assert.ok(!Object.hasOwn(column.sections[index + 1], 'data'))
})

test('writing blocks into lazy and eager columns gives the same states and section counts', () => {
  const source = sourceColumn()
  const lazy = load(LazyClass, source)
  const eager = load(EagerClass, source)
  for (let i = 0; i < 3000; i++) {
    const p = new Vec3(rand(16), -64 + rand(384), rand(16))
    const id = rand(3) === 0 ? 0 : ids[rand(ids.length)]
    lazy.setBlockStateId(p, id)
    eager.setBlockStateId(p, id)
  }
  assert.deepEqual(readAll(lazy), readAll(eager))
  assert.deepEqual(lazy.sections.map(s => [s.solidBlockCount, s.fluidCount]), eager.sections.map(s => [s.solidBlockCount, s.fluidCount]))
})

test('a second load into the same column replaces the first', () => {
  const lazy = load(LazyClass, sourceColumn())
  const next = sourceColumn()
  loadInto(lazy, next)
  assert.deepEqual(readAll(lazy), readAll(load(EagerClass, next)))
})

test('truncated chunk data fails at load, as an eager load does', () => {
  const data = sourceColumn().dump()
  assert.throws(() => new LazyClass().load(data.subarray(0, data.length - 100)))
})

test('dump of a lazily loaded column equals the data it was loaded from', () => {
  const source = sourceColumn()
  assert.deepEqual(load(LazyClass, source).dump(), source.dump())
})
