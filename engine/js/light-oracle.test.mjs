// Oracle: the light model (light.mjs) against light captured from a live server (tools/light-capture.mjs).
// Each fixture is relit from its states with the interior wiped; the one-cell shell stays as captured (fixed sources).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'
import { lightTable, relightBox } from './light.mjs'
import { readLightFixture, lightBreakdown, formatBreakdown } from './light-fixtures.mjs'

const require = createRequire(import.meta.url)
const dir = path.join(import.meta.dirname, 'fixtures/light')
const files = fs.readdirSync(dir).filter(f => f.endsWith('.bin')).sort()
const CUBE = 17

const isShell = (size, x, y, z) => x === 0 || y === 0 || z === 0 || x === size[0] - 1 || y === size[1] - 1 || z === size[2] - 1

// the sub-box [lo, lo + size) of a fixture as a fixture of its own
const subFixture = (f, lo, size) => {
  const cells = size[0] * size[1] * size[2]
  const out = { ...f, origin: f.origin.map((v, i) => v + lo[i]), size, states: new Uint16Array(cells), sky: new Uint8Array(cells), block: new Uint8Array(cells) }
  for (let y = 0; y < size[1]; y++) {
    for (let z = 0; z < size[2]; z++) {
      for (let x = 0; x < size[0]; x++) {
        const from = ((y + lo[1]) * f.size[2] + z + lo[2]) * f.size[0] + x + lo[0]
        const to = (y * size[2] + z) * size[0] + x
        out.states[to] = f.states[from]
        out.sky[to] = f.sky[from]
        out.block[to] = f.block[from]
      }
    }
  }
  return out
}

const relight = (fixture, registry) => {
  const { size } = fixture
  const sky = Uint8Array.from(fixture.sky)
  const block = Uint8Array.from(fixture.block)
  for (let y = 0; y < size[1]; y++) {
    for (let z = 0; z < size[2]; z++) {
      for (let x = 0; x < size[0]; x++) {
        if (isShell(size, x, y, z)) continue
        const i = (y * size[2] + z) * size[0] + x
        sky[i] = 0
        block[i] = 0
      }
    }
  }
  return relightBox({ table: lightTable(registry), states: fixture.states, sky, block, size })
}

const check = (label, fixture, registry) => {
  const computed = relight(fixture, registry)
  const breakdown = lightBreakdown({ fixture, computed, registry, inside: (x, y, z) => !isShell(fixture.size, x, y, z) })
  console.log(formatBreakdown(label, breakdown))
  assert.equal(breakdown.skyTotal + breakdown.blockTotal, 0)
}

for (const file of files) {
  const fixture = readLightFixture(path.join(dir, file))
  const registry = require('prismarine-registry')(fixture.version)
  test(`oracle ${fixture.name}: whole box`, () => check(fixture.name, fixture, registry))
  test(`oracle ${fixture.name}: ${CUBE}^3 cube at the centre`, () => {
    const lo = fixture.size.map(s => Math.max(0, (s >> 1) - (CUBE >> 1)))
    const size = fixture.size.map((s, i) => Math.min(CUBE, s - lo[i]))
    check(`${fixture.name} cube`, subFixture(fixture, lo, size), registry)
  })
}
