import { test } from 'node:test'
import assert from 'node:assert/strict'
import { lineClear } from './sight.mjs'

const pt = ([x, y, z]) => ({ x, y, z })
const solidSet = (...cells) => { const s = new Set(cells); return ({ x, y, z }) => s.has(`${x},${y},${z}`) }

for (const [name, from, to, solid, clear] of [
  ['an empty line', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], solidSet(), true],
  ['a block in the middle', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], solidSet('3,64,0'), false],
  ['a block beside the line', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], solidSet('3,64,1'), true],
  ['the start cell is ignored', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], solidSet('0,64,0'), true],
  ['the end cell is ignored', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], solidSet('5,64,0'), true],
  ['a block on a diagonal', [0.5, 64.5, 0.5], [4.5, 64.5, 4.5], solidSet('2,64,2'), false],
  ['a block above a sloping line', [0.5, 66.5, 0.5], [6.5, 64.5, 0.5], solidSet('2,66,0'), false],
  ['a line going backwards', [5.5, 64.5, 0.5], [0.5, 64.5, 0.5], solidSet('2,64,0'), false]
]) {
  test(`lineClear: ${name}`, () => assert.equal(lineClear(pt(from), pt(to), solid), clear))
}
