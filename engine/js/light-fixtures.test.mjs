import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { writeLightFixture, readLightFixture, lightBreakdown } from './light-fixtures.mjs'

const sample = () => ({
  name: 't', version: '1.21.4', origin: [10, -5, 20], size: [3, 2, 2], capturedAt: 'x',
  states: Uint16Array.from({ length: 12 }, (_, i) => i * 1000),
  sky: Uint8Array.from({ length: 12 }, (_, i) => i % 16),
  block: Uint8Array.from({ length: 12 }, (_, i) => (i * 7) % 16)
})

test('fixture write/read round trip', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'lf-'))
  const file = path.join(dir, 'a.bin')
  const f = sample()
  writeLightFixture(file, f)
  assert.deepEqual(readLightFixture(file), f)
  fs.rmSync(dir, { recursive: true, force: true })
})

test('breakdown names the mismatching class and world coordinate', () => {
  const f = sample()
  const computed = { sky: Uint8Array.from(f.sky), block: Uint8Array.from(f.block) }
  computed.block[4] = (computed.block[4] + 1) & 15 // cell x1 y0 z1
  const registry = { blocksByStateId: { 4000: { name: 'stone' } } }
  const b = lightBreakdown({ fixture: f, computed, registry })
  assert.equal(b.blockTotal, 1)
  assert.equal(b.skyTotal, 0)
  assert.equal(b.classes[0].example.join(), '11,-5,21')
})
