import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { createRequire } from 'node:module'
import { createTerrainGeometry } from '../src/navigation/terrain.mjs'

const require = createRequire(import.meta.url)
const data = require('minecraft-data')('26.1')
const Block = require('prismarine-block')(data)
const evidence = JSON.parse(readFileSync(new URL('./fixtures/corin-observed-terrain.json', import.meta.url)))

function observedBlocks () {
  const cells = new Map()
  for (const grid of evidence.snapshots) {
    for (const [y, rows] of Object.entries(grid.layers)) {
      for (const [dz, row] of rows.entries()) {
        for (const [dx, symbol] of [...row].entries()) {
          const name = grid.palette[symbol]
          const key = `${grid.x1 + dx},${y},${grid.z1 + dz}`
          assert.ok(data.blocksByName[name], `registry block ${name}`)
          assert.ok(!cells.has(key) || cells.get(key).name === name, `consistent observation ${key}`)
          cells.set(key, Block.fromStateId(data.blocksByName[name].defaultState, 0))
        }
      }
    }
  }
  return (x, y, z) => cells.get(`${x},${y},${z}`) ?? null
}

test('observed return corridor clears a player but collides with the actual wider horse', () => {
  const geometry = createTerrainGeometry(observedBlocks(), { openDoors: false })
  const x = 424.5, y = 98, z = -230.5
  assert.equal(geometry.clearBox([x - 0.3001, y, z - 0.3001, x + 0.3001, y + 1.8, z + 0.3001]), true)
  const half = evidence.horse.width / 2
  assert.equal(geometry.clearBox([x - half, y, z - half, x + half, y + evidence.horse.height, z + half]), false)
})

test('partial live scans do not turn unobserved space into safe air', () => {
  const blockAt = observedBlocks()
  assert.equal(blockAt(424, 102, -231), null)
  const geometry = createTerrainGeometry(blockAt, { openDoors: false })
  assert.equal(geometry.clearBox([424.2, 101, -230.8, 424.8, 102.8, -230.2]), false)
})
