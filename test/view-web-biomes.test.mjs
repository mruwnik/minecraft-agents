import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import zlib from 'node:zlib'
import { fileURLToPath } from 'node:url'
import { makeChunkClass, loadColumn } from '../tools/view/columns.mjs'
import { columnFormat } from '../tools/view/web-format.mjs'
import { inflate, parseColumnFile, decodeSections } from '../tools/view/web/decode.mjs'
import { biomeTextureOrder } from '../tools/view/web/biomes.mjs'
const decodeBiomes = (bytes, format) => decodeSections(bytes, format).biomes

const MIN_Y = -64
const HEIGHT = 384
const fixture = name => fileURLToPath(new URL(`./fixtures/view-columns/${name}`, import.meta.url))

const wrap = sections => {
  const header = Buffer.from(JSON.stringify({
    v: 1, x: 0, z: 0, t: 0, body: 'b', mcVersion: 'x', minY: MIN_Y, worldHeight: HEIGHT,
    parts: [{ name: 'sections', len: sections.length }, { name: 'biomes', len: 0 }, { name: 'light', len: 0, meta: {} }]
  }))
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  return Buffer.concat([length, header, sections])
}

const assertMatches = (column, decoded) => {
  assert.equal(decoded.length, column.numSections * 64)
  for (let s = 0; s < column.numSections; s++) {
    for (let y4 = 0; y4 < 4; y4++) {
      for (let z4 = 0; z4 < 4; z4++) {
        for (let x4 = 0; x4 < 4; x4++) {
          const pos = { x: x4 * 4, y: MIN_Y + s * 16 + y4 * 4, z: z4 * 4 }
          const want = column.getBiome(pos)
          const got = decoded[s * 64 + (y4 << 4 | z4 << 2 | x4)]
          if (got !== Math.min(want, 255)) assert.fail(`mismatch at ${JSON.stringify(pos)}: ${got} vs ${want}`)
        }
      }
    }
  }
}

const syntheticColumn = (Chunk, version) => {
  const format = columnFormat(version)
  const column = new Chunk({ minY: MIN_Y, worldHeight: HEIGHT })
  for (let i = 0; i < 40; i++) column.setBlockStateId({ x: i % 16, y: MIN_Y + 16 + (i >> 4), z: (i * 3) % 16 }, 1 + (i % 3))
  for (let i = 0; i < 3; i++) column.setBiome({ x: i * 4, y: MIN_Y + 16, z: 0 }, i + 1) // indirect
  for (let i = 0; i < 16; i++) column.setBiome({ x: (i % 4) * 4, y: MIN_Y + 32 + (i >> 2) * 4, z: (i % 3) * 4 }, i + 1) // 4 bits: direct
  for (let i = 0; i < 20; i++) column.setBiome({ x: (i % 4) * 4, y: MIN_Y + 48 + (i >> 2) * 4 % 16, z: (i >> 2) % 4 * 4 }, 40 + i) // many ids
  column.setBiome({ x: 8, y: MIN_Y + 64, z: 4 }, 7) // a section that is otherwise single value
  return { column, format }
}

for (const version of ['1.21.4', '26.1']) {
  test(`biomes: synthetic column matches prismarine (${version})`, async () => {
    const { column, format } = syntheticColumn(makeChunkClass(version), version)
    const { sections } = parseColumnFile(await inflate(zlib.deflateSync(wrap(column.dump()))))
    assertMatches(column, decodeBiomes(sections, { ...format, numSections: column.numSections }))
  })
}

for (const name of ['-1.-2.bin', '-119.-118.bin', '128.117.bin']) {
  test(`biomes: real file ${name} matches prismarine's getBiome`, async () => {
    const column = loadColumn(fixture(name), makeChunkClass('26.1'))
    const { header, sections } = parseColumnFile(await inflate(fs.readFileSync(fixture(name))))
    const decoded = decodeBiomes(sections, { ...columnFormat(header.mcVersion), numSections: column.numSections })
    assertMatches(column, decoded)
    assert.ok(new Set(decoded).size >= 1)
  })
}

test('biomes: ids above 255 clamp to 255', () => {
  // one section, 1.21.4 layout: block container single value 0, biome container single value 300
  const bytes = Uint8Array.from([0, 0, 0, 0, 0, 0, 0xac, 0x02, 0])
  const out = decodeBiomes(bytes, { noSizePrefix: false, hasFluidCount: false, numSections: 1 })
  assert.ok(out.every(v => v === 255))
})

test('biomes: textureOrder puts section cell (s, y4, z4, x4) at (z4 * height + s * 4 + y4) * 4 + x4', () => {
  const ids = new Uint8Array(2 * 64)
  ids[1 * 64 + (2 << 4 | 3 << 2 | 1)] = 9 // section 1, y4 2, z4 3, x4 1
  const out = biomeTextureOrder(ids, 2)
  const height = 8
  assert.equal(out.length, 128)
  assert.equal(out[(3 * height + 6) * 4 + 1], 9)
  assert.equal(out.reduce((a, b) => a + b, 0), 9)
})

test('biomes: decoding a real 24-section column is fast', async () => {
  const name = '-1.-2.bin'
  const { header, sections } = parseColumnFile(await inflate(fs.readFileSync(fixture(name))))
  const format = { ...columnFormat(header.mcVersion), numSections: HEIGHT >> 4 }
  decodeBiomes(sections, format)
  const start = performance.now()
  const n = 200
  for (let i = 0; i < n; i++) decodeBiomes(sections, format)
  const ms = (performance.now() - start) / n
  console.log(`decodeBiomes: ${ms.toFixed(3)} ms per 24-section column`)
  assert.ok(ms < 5)
})

test('biomes: the known dark_forest cell of state/worlds/claude/chunks/-124.-120.bin', async t => {
  const file = fileURLToPath(new URL('../state/worlds/claude/chunks/-124.-120.bin', import.meta.url))
  if (!fs.existsSync(file)) return t.skip('no state file')
  const { header, sections } = parseColumnFile(await inflate(fs.readFileSync(file)))
  const decoded = decodeBiomes(sections, { ...columnFormat(header.mcVersion), numSections: header.worldHeight >> 4 })
  const s = (72 - header.minY) >> 4
  assert.equal(decoded[s * 64 + ((((72 - header.minY) & 15) >> 2) << 4 | 2 << 2 | 2)], 8)
})
