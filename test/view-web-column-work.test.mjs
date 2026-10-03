import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { fileURLToPath } from 'node:url'
import { materialTable } from '../tools/view/materials.mjs'
import { inflate, parseColumnFile, decodeSections, lightColumn } from '../tools/view/web/decode.mjs'
import { decodeColumn } from '../tools/view/web/column-work.mjs'

const fixtureDir = fileURLToPath(new URL('./fixtures/view-columns/', import.meta.url))
const textureDir = fileURLToPath(new URL('../textures', import.meta.url))
const table = materialTable('26.1', textureDir, { jarPath: null })
const materialOf = new Uint16Array(Uint8Array.from(Buffer.from(table.materialOf, 'base64')).buffer)

// the main-thread path app.mjs had before the worker: inflate, parse, decodeSections, materialColumn, lightColumn
const materialColumn = (ids, height, of) => {
  const mats = new Uint16Array(256 * height)
  const flags = new Uint8Array(height >> 4)
  for (let y = 0; y < height; y++) {
    const section = y >> 4
    const base = section * 4096 + ((y & 15) << 8)
    for (let z = 0; z < 16; z++) {
      for (let x = 0; x < 16; x++) {
        const material = of[ids[base + (z << 4 | x)]] ?? 0
        if (material === 0) continue
        mats[(z * height + y) * 16 + x] = material
        flags[section] = 1
      }
    }
  }
  return { mats, flags }
}

const oldPath = async (bytes, columnFormat) => {
  const { header, sections, light } = parseColumnFile(await inflate(bytes))
  const { ids } = decodeSections(sections, { ...columnFormat, numSections: header.worldHeight >> 4 })
  return { header, ...materialColumn(ids, header.worldHeight, materialOf), light: lightColumn(light, header.worldHeight) }
}

for (const name of fs.readdirSync(fixtureDir)) {
  test(`decodeColumn matches the old main-thread path on ${name}`, async () => {
    const bytes = new Uint8Array(fs.readFileSync(`${fixtureDir}${name}`))
    const { columnFormat } = await import('../tools/view/web-format.mjs')
    const fmt = columnFormat('26.1')
    const expected = await oldPath(bytes, fmt)
    const got = await decodeColumn(bytes, { format: fmt, materialOf })
    assert.deepEqual(got.header, expected.header)
    assert.deepEqual(got.mats, expected.mats)
    assert.deepEqual(got.flags, expected.flags)
    assert.deepEqual(got.light, expected.light)
    assert.ok(got.flags.some(f => f === 1), 'the fixture has blocks')
  })

  test(`decodeColumn outputs of ${name} are typed arrays owning whole buffers`, async () => {
    const { columnFormat } = await import('../tools/view/web-format.mjs')
    const got = await decodeColumn(new Uint8Array(fs.readFileSync(`${fixtureDir}${name}`)), { format: columnFormat('26.1'), materialOf })
    for (const key of ['mats', 'flags', 'light']) {
      assert.ok(ArrayBuffer.isView(got[key]), key)
      assert.equal(got[key].byteOffset, 0, key)
      assert.equal(got[key].buffer.byteLength, got[key].byteLength, key)
    }
  })
}
