import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import zlib from 'node:zlib'
import { fileURLToPath } from 'node:url'
import { makeChunkClass, loadColumn } from '../tools/view/columns.mjs'
import { decodeColumnFile, restoreColumn } from '../engine/js/view.mjs'
import { columnFormat } from '../tools/view/web-format.mjs'
import { inflate, parseColumnFile, decodeSections, decodeLight, textureOrder, lightColumn } from '../tools/view/web/decode.mjs'

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

const fixtures = ['-1.-2.bin', '-119.-118.bin', '128.117.bin']
const maskBit = (mask, i) => ((mask[i >> 6]?.[i & 32 ? 0 : 1] ?? 0) >>> (i & 31)) & 1

for (const name of fixtures) {
  test(`light: real file ${name} matches prismarine cell by cell`, async () => {
    const Chunk = makeChunkClass('26.1')
    const file = fs.readFileSync(fixture(name))
    const decodedFile = decodeColumnFile(file)
    // prismarine's loadParsedLight reads each 8 bytes as a big-endian long, which scrambles the vanilla nibble order
    // (byte i holds cells 2i low, 2i+1 high); reverse each 8-byte group first so prismarine reads it as vanilla does
    const { buffer } = decodedFile.light
    decodedFile.light.buffer = Buffer.from(buffer.map((_, i) => buffer[(i & ~7) + 7 - (i & 7)]))
    const column = restoreColumn(new Chunk({ minY: MIN_Y, worldHeight: HEIGHT }), decodedFile)
    const { header, light } = parseColumnFile(await inflate(file))
    assert.equal(light.meta.skyCount > 0, true)
    const out = decodeLight(light.bytes, light.meta, column.numSections)
    assert.equal(out.length, column.numSections * 4096)
    const noData = s => !maskBit(light.meta.skyLightMask, s + 1) && !maskBit(light.meta.emptySkyLightMask, s + 1)
    for (let s = 0; s < column.numSections; s++) {
      const skip = noData(s)
      if (skip) {
        assert.ok(out.subarray(s * 4096, s * 4096 + 4096).every(v => v >> 4 === 15), `no-data sky section ${s} is 15`)
      }
      for (let ly = 0; ly < 16 && !skip; ly++) {
        for (let z = 0; z < 16; z++) {
          for (let x = 0; x < 16; x++) {
            const pos = { x, y: header.minY + s * 16 + ly, z }
            const v = out[s * 4096 + (ly << 8 | z << 4 | x)]
            if (v >> 4 !== column.getSkyLight(pos) || (v & 15) !== column.getBlockLight(pos)) assert.fail(`light mismatch at ${JSON.stringify(pos)}: ${v >> 4}/${v & 15} vs ${column.getSkyLight(pos)}/${column.getBlockLight(pos)}`)
          }
        }
      }
    }
  })
}

test('light: parseColumnFile returns the light part, empty when absent', async () => {
  const withLight = parseColumnFile(await inflate(zlib.deflateSync(wrap(Buffer.alloc(0), Buffer.from([1, 2, 3])))))
  assert.deepEqual([withLight.light.meta, [...withLight.light.bytes]], [{}, [1, 2, 3]])
  const header = Buffer.from(JSON.stringify({ v: 1, parts: [{ name: 'sections', len: 0 }] }))
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  const none = parseColumnFile(Uint8Array.from(Buffer.concat([length, header])))
  assert.deepEqual([none.light.meta, none.light.bytes.length], [{}, 0])
})

const mask = bits => [[0, bits.reduce((a, b) => a | (1 << b), 0)]]
const packed = f => Uint8Array.from({ length: 2048 }, (_, i) => f(2 * i) | f(2 * i + 1) << 4)

test('light: no meta is open sky, no block light', () => {
  const out = decodeLight(new Uint8Array(0), {}, 2)
  assert.ok(out.every(v => v === 0xf0))
  assert.equal(out.length, 8192)
})

test('light: empty-sky section is 0, unmentioned sky is 15, block pattern lands per cell', () => {
  // 2 sections: light indexes 1 (s0) and 2 (s1); s0 empty sky; block data for s1 only
  const block = packed(i => i & 15)
  const meta = { skyCount: 0, blockCount: 1, sectionBytes: 2048, skyLightMask: [], blockLightMask: mask([2]), emptySkyLightMask: mask([1]), emptyBlockLightMask: [] }
  const out = decodeLight(block, meta, 2)
  assert.ok(out.subarray(0, 4096).every(v => v === 0))
  assert.ok(out.subarray(4096).every((v, i) => v === (0xf0 | (i & 15))))
})

test('textureOrder: small hand case', () => {
  // 2 sections (height 32), only cells x=1,z=2: y=0 -> 7, y=17 -> 9
  const v = new Uint8Array(2 * 4096)
  v[0 * 4096 + (0 << 8 | 2 << 4 | 1)] = 7
  v[1 * 4096 + (1 << 8 | 2 << 4 | 1)] = 9
  const t = textureOrder(v, 32)
  assert.equal(t.length, v.length)
  assert.equal(t[(2 * 32 + 0) * 16 + 1], 7)
  assert.equal(t[(2 * 32 + 17) * 16 + 1], 9)
  assert.equal(t.reduce((a, b) => a + b, 0), 16)
  assert.ok(t instanceof Uint8Array)
})

test('textureOrder: round trip and element type', () => {
  const v = Uint16Array.from({ length: 3 * 4096 }, (_, i) => i * 7 % 65521)
  const t = textureOrder(v, 48)
  assert.ok(t instanceof Uint16Array)
  for (let i = 0; i < v.length; i += 13) {
    const s = i >> 12; const ly = (i >> 8) & 15; const z = (i >> 4) & 15; const x = i & 15
    assert.equal(t[(z * 48 + s * 16 + ly) * 16 + x], v[i])
  }
  assert.deepEqual([...textureOrder(v, 48, new Uint16Array(v.length))].slice(0, 50), [...t].slice(0, 50))
})

test('lightColumn: decoded light in GPU order, block light of section 1 lands at its y', () => {
  const block = packed(i => i === (1 << 8 | 2 << 4 | 3) ? 9 : 0)
  const meta = { skyCount: 0, blockCount: 1, sectionBytes: 2048, skyLightMask: [], blockLightMask: mask([2]), emptySkyLightMask: [], emptyBlockLightMask: [] }
  const out = lightColumn({ bytes: block, meta }, 32)
  assert.ok(out instanceof Uint8Array)
  assert.equal(out.length, 8192)
  assert.equal(out[(2 * 32 + 17) * 16 + 3], 0xf9)
  assert.equal(out[(2 * 32 + 16) * 16 + 3], 0xf0)
})
