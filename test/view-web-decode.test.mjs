import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import zlib from 'node:zlib'
import { fileURLToPath } from 'node:url'
import { makeChunkClass, loadColumn } from '../tools/view/columns.mjs'
import { columnFormat } from '../tools/view/web-format.mjs'
import { inflate, parseColumnFile, decodeSections } from '../tools/view/web/decode.mjs'

const MIN_Y = -64
const HEIGHT = 384
const fixture = name => fileURLToPath(new URL(`./fixtures/view-columns/${name}`, import.meta.url))

const wrap = (sections, extra = Buffer.alloc(3)) => {
  const header = Buffer.from(JSON.stringify({
    v: 1, x: 0, z: 0, t: 0, body: 'b', mcVersion: 'x', minY: MIN_Y, worldHeight: HEIGHT,
    parts: [{ name: 'sections', len: sections.length }, { name: 'biomes', len: 0 }, { name: 'light', len: extra.length, meta: {} }]
  }))
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  return Buffer.concat([length, header, sections, extra])
}

const cells = function * (column) {
  for (let s = 0; s < column.numSections; s++) {
    for (let ly = 0; ly < 16; ly++) {
      for (let z = 0; z < 16; z++) {
        for (let x = 0; x < 16; x++) yield { index: s * 4096 + (ly << 8 | z << 4 | x), pos: { x, y: MIN_Y + s * 16 + ly, z } }
      }
    }
  }
}

const assertMatches = (column, decoded) => {
  assert.equal(decoded.ids.length, column.numSections * 4096)
  for (const { index, pos } of cells(column)) {
    if (decoded.ids[index] !== column.getBlockStateId(pos)) assert.fail(`mismatch at ${JSON.stringify(pos)}: ${decoded.ids[index]} vs ${column.getBlockStateId(pos)}`)
  }
  for (let s = 0; s < column.numSections; s++) {
    const any = decoded.ids.subarray(s * 4096, s * 4096 + 4096).some(v => v !== 0)
    assert.equal(decoded.nonEmpty[s], any ? 1 : 0, `nonEmpty[${s}]`)
  }
}

const syntheticColumn = (Chunk, version) => {
  const format = columnFormat(version)
  const column = new Chunk({ minY: MIN_Y, worldHeight: HEIGHT })
  // section 0: single value, non-zero
  column.sections[0] = new Chunk.section({ noSizePrefix: format.noSizePrefix, hasFluidCount: format.hasFluidCount, singleValue: 9, maxBitsPerBlock: format.maxBitsPerBlock })
  // section 1: indirect, a few ids
  for (let i = 0; i < 40; i++) column.setBlockStateId({ x: i % 16, y: MIN_Y + 16 + (i >> 4), z: (i * 3) % 16 }, [1, 9, 33][i % 3])
  // section 2: direct, 300 distinct ids
  for (let i = 0; i < 300; i++) column.setBlockStateId({ x: i % 16, y: MIN_Y + 32 + (i >> 8), z: (i >> 4) % 16 }, 1 + i)
  // section 3: a lot of cells with 200 ids (8-bit indirect)
  for (let i = 0; i < 600; i++) column.setBlockStateId({ x: i % 16, y: MIN_Y + 48 + (i >> 8), z: (i >> 4) % 16 }, 1 + (i % 200))
  // biomes: section 1 gets a few (indirect), section 2 gets many (direct)
  for (let i = 0; i < 3; i++) column.setBiome({ x: i * 4, y: MIN_Y + 16, z: 0 }, i + 1)
  for (let i = 0; i < 12; i++) column.setBiome({ x: (i % 4) * 4, y: MIN_Y + 32 + (i >> 2) * 4, z: 0 }, i + 1)
  return column
}

const versions = ['1.20.4', '1.21.4', '26.1']

for (const version of versions) {
  test(`decode: synthetic column matches prismarine (${version})`, async () => {
    const Chunk = makeChunkClass(version)
    const column = syntheticColumn(Chunk, version)
    const file = zlib.deflateSync(wrap(column.dump()))
    const { header, sections } = parseColumnFile(await inflate(file))
    assert.equal(header.minY, MIN_Y)
    const decoded = decodeSections(sections, { ...columnFormat(version), numSections: column.numSections })
    assertMatches(column, decoded)
    assert.ok(decoded.nonEmpty[0] && decoded.nonEmpty[1] && decoded.nonEmpty[2] && !decoded.nonEmpty[10])
  })
}

test('decode: columnFormat names the variants', () => {
  assert.deepEqual(versions.map(v => { const f = columnFormat(v); return [f.noSizePrefix, f.hasFluidCount] }), [[false, false], [false, false], [true, true]])
})

test('decode: inflate takes an ArrayBuffer too', async () => {
  const out = await inflate(Uint8Array.from(zlib.deflateSync(Buffer.from('hello'))).buffer)
  assert.equal(new TextDecoder().decode(out), 'hello')
})

test('decode: parseColumnFile refuses another version', () => {
  const raw = wrap(Buffer.alloc(0))
  const text = raw.toString('latin1').replace('"v":1', '"v":2')
  assert.throws(() => parseColumnFile(Uint8Array.from(Buffer.from(text, 'latin1'))), /version/)
})

for (const name of ['-1.-2.bin', '-119.-118.bin', '128.117.bin']) {
  test(`decode: real file ${name} matches prismarine's loadColumn`, async () => {
    const Chunk = makeChunkClass('26.1')
    const column = loadColumn(fixture(name), Chunk)
    const { header, sections } = parseColumnFile(await inflate(fs.readFileSync(fixture(name))))
    assert.equal(header.mcVersion, '26.1')
    assertMatches(column, decodeSections(sections, { ...columnFormat(header.mcVersion), numSections: column.numSections }))
  })
}
