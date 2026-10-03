// The planner's bamboo (space.mjs boxesNear) and the body's physics (walk-fix's offset-shapes) must put the stalk at the same world box.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { fixtureSnapshot } from './fixture.mjs'
import { defaultStateTable } from './blocks.mjs'
import { boxesNear } from './space.mjs'
import { serverShapes } from '../offset-shapes.mjs'

const registry = prismarineRegistry('26.1')
const Block = prismarineBlock(registry)
const table = defaultStateTable()
const TOLERANCE = 1e-3 // boxesNear returns float32

const positions = [[0, 0], [1, 1], [5, -3], [-1, -1], [-7, 4], [-33, -50], [16, 16], [123, -456], [-300, 299], [2857, 3216]]

for (const [x, z] of positions) {
  test(`bamboo at ${x}, ${z}: the planner's box is the server's box`, () => {
    const snapshot = fixtureSnapshot({ blocks: [[x, 64, z, 'bamboo']] })
    // (cells in unloaded neighbour chunks come back as whole solid cubes: the stalk is the one thin box)
    const boxes = boxesNear(snapshot, table, x, 64, z, 64, 66)
    const at = Array.from({ length: boxes.length / 6 }, (_, k) => k * 6).find(k => boxes[k + 3] - boxes[k] < 0.5)
    const planner = Array.from(boxes.slice(at, at + 6))
    const block = Block.fromStateId(registry.blocksByName.bamboo.defaultState, 0)
    block.position = { x, y: 64, z }
    const [server] = serverShapes(block)
    const world = [x + server[0], 64 + server[1], z + server[2], x + server[3], 64 + server[4], z + server[5]]
    planner.forEach((v, k) => assert.ok(Math.abs(v - world[k]) < TOLERANCE, `coordinate ${k}: planner ${v}, server ${world[k]}`))
  })
}
