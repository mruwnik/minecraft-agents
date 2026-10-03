import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import minecraftData from 'minecraft-data'
import { decodePng } from '../src/vision/renderer.mjs'
import { zipEntries, entryContent, findClientJar } from '../tools/view/jar-read.mjs'
import { biomeColors, biomeColorsByName, biomeTable } from '../tools/view/biome-colors.mjs'

const VERSION = '26.1'
const GROUP = { grass: 0, foliage: 1, dry: 2, water: 3 }
const hex = h => [(h >> 16) & 255, (h >> 8) & 255, h & 255]
const colorOf = (result, biome, group) => {
  const at = (result.ids.indexOf(biome) * 4 + GROUP[group]) * 3
  return [...result.colors.subarray(at, at + 3)]
}
const close = (actual, expected, tolerance) => actual.forEach((v, i) => assert.ok(Math.abs(v - expected[i]) <= tolerance, `${actual} vs ${expected}`))
const darkForestFormula = c => (((c & 0xFEFEFE) + 0x28340A) >> 1)

const cases = [
  ['plains', 'grass', 0x91BD59, 2],
  ['plains', 'foliage', 0x77AB2F, 2],
  ['plains', 'water', 0x3F76E4, 0],
  ['swamp', 'grass', 0x6A7039, 0],
  ['swamp', 'foliage', 0x6A7039, 0],
  ['swamp', 'water', 0x617B64, 0],
  ['jungle', 'grass', 0x59C93C, 2],
  ['badlands', 'grass', 0x90814D, 0],
  ['cherry_grove', 'grass', 0xB6DB61, 0],
  ['desert', 'grass', 0xBFB755, 2],
  ['snowy_plains', 'grass', 0x80B497, 2]
]

test('the real jar is the source', () => {
  assert.equal(biomeColors(VERSION).source, 'jar')
})

test('vanilla colours per biome', () => {
  const result = biomeColors(VERSION)
  cases.forEach(([biome, group, expected, tolerance]) => close(colorOf(result, biome, group), hex(expected), tolerance))
})

test('dark forest grass is the formula on its colormap colour', () => {
  const jar = findClientJar()
  const buf = fs.readFileSync(jar)
  const entry = zipEntries(buf).find(e => e.name === 'assets/minecraft/textures/colormap/grass.png')
  const { width, rgba } = decodePng(entryContent(buf, entry))
  const biome = JSON.parse(entryContent(buf, zipEntries(buf).find(e => e.name === 'data/minecraft/worldgen/biome/dark_forest.json')))
  const t = biome.temperature
  const at = (Math.floor((1 - biome.downfall * t) * 255) * width + Math.floor((1 - t) * 255)) * 4
  const packed = (rgba[at] << 16) | (rgba[at + 1] << 8) | rgba[at + 2]
  assert.deepEqual(colorOf(biomeColors(VERSION), 'dark_forest', 'grass'), hex(darkForestFormula(packed)))
})

test('ids follow minecraft-data order and colors are 12 bytes per biome', () => {
  const result = biomeColors(VERSION)
  const names = minecraftData(VERSION).biomesArray.map(b => b.name)
  assert.deepEqual(result.ids, names)
  assert.equal(result.colors.length, 12 * names.length)
})

test('results are cached per version', () => {
  assert.equal(biomeColorsByName(VERSION), biomeColorsByName(VERSION))
})

test('no jar falls back to tints.json and plains defaults', () => {
  ;[null, '/nonexistent/client.jar'].forEach(jar => {
    const result = biomeColors(VERSION, { jar })
    assert.equal(result.source, 'fallback')
    assert.deepEqual(colorOf(result, 'plains', 'grass'), [124, 189, 107])
    assert.deepEqual(colorOf(result, 'plains', 'foliage'), [89, 174, 48])
    assert.deepEqual(colorOf(result, 'plains', 'dry'), [160, 112, 47])
    assert.equal(result.colors.length, 12 * result.ids.length)
  })
})

const registry = JSON.parse(fs.readFileSync(new URL('../state/worlds/claude/biomes.json', import.meta.url), 'utf8')).biomes
const registryNames = registry.map(b => b.name)
const rowOf = (table, name, group) => {
  const at = (table.names.indexOf(name) * 4 + GROUP[group]) * 3
  return [...table.colors.subarray(at, at + 3)]
}

test('sulfur_caves is absent from the 26.1.2 client jar and minecraft-data, so it is unknown and gets plains', () => {
  const table = biomeTable(VERSION, registryNames)
  assert.deepEqual(table.unknown, ['sulfur_caves'])
  assert.deepEqual(rowOf(table, 'sulfur_caves', 'grass'), rowOf(table, 'plains', 'grass'))
})

test('biomeColorsByName covers minecraft-data names and has 12 bytes each', () => {
  const { byName, source } = biomeColorsByName(VERSION)
  assert.equal(source, 'jar')
  minecraftData(VERSION).biomesArray.forEach(b => assert.equal(byName.get(b.name)?.length, 12, b.name))
})

test('biomeTable puts swamp and plains at the world registry ids', () => {
  const table = biomeTable(VERSION, registryNames)
  assert.equal(table.colors.length, 12 * registryNames.length)
  assert.deepEqual(rowOf(table, 'swamp', 'grass'), hex(0x6A7039))
  assert.equal(table.names.indexOf('swamp'), registry.find(b => b.name === 'swamp').id)
  close(rowOf(table, 'plains', 'grass'), hex(0x91BD59), 2)
  assert.equal(table.names.indexOf('plains'), registry.find(b => b.name === 'plains').id)
  assert.ok(!table.unknown.includes('swamp') && !table.unknown.includes('plains'))
})

test('an unknown name gets plains colours and is listed', () => {
  const table = biomeTable(VERSION, ['swamp', 'no_such_biome'])
  assert.deepEqual(table.unknown, ['no_such_biome'])
  assert.deepEqual(rowOf(table, 'no_such_biome', 'grass'), rowOf(biomeTable(VERSION, ['plains']), 'plains', 'grass'))
})
