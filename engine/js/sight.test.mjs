import { test } from 'node:test'
import assert from 'node:assert/strict'
import { lineClear, rayClear, blocksSight } from './sight.mjs'

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

const CUBE = [0, 0, 0, 1, 1, 1]
const POST = [0.375, 0, 0.375, 0.625, 1.5, 0.625]
const PANE = [0.4375, 0, 0.4375, 0.5625, 1, 0.5625]
const shapesSet = (...cells) => { const m = new Map(cells.map(([c, shapes]) => [c, shapes])); return ({ x, y, z }) => m.get(`${x},${y},${z}`) ?? [] }

for (const [name, from, to, shapes, clear] of [
  ['an empty line', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], shapesSet(), true],
  ['a full cube between', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], shapesSet(['3,64,0', [CUBE]]), false],
  ['a cube beside the line', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], shapesSet(['3,64,1', [CUBE]]), true],
  ['a cube in the start cell', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], shapesSet(['0,64,0', [CUBE]]), true],
  ['a cube in the end cell', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], shapesSet(['5,64,0', [CUBE]]), true],
  ['a fence post the ray passes beside', [0.5, 64.5, 0.2], [5.5, 64.5, 0.2], shapesSet(['3,64,0', [POST]]), true],
  ['a fence post the ray goes through', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], shapesSet(['3,64,0', [POST]]), false],
  ['a fence post the ray passes over', [0.5, 66.0, 0.5], [5.5, 66.0, 0.5], shapesSet(['3,64,0', [POST]]), true],
  ['a pane post the ray goes through', [0.5, 64.5, 0.5], [5.5, 64.5, 0.5], shapesSet(['3,64,0', [PANE]]), false],
  ['a pane post the ray misses', [0.5, 64.5, 0.2], [5.5, 64.5, 0.2], shapesSet(['3,64,0', [PANE]]), true],
  ['a cube on a diagonal', [0.5, 64.5, 0.5], [4.5, 64.5, 4.5], shapesSet(['2,64,2', [CUBE]]), false],
  ['a ray going backwards', [5.5, 64.5, 0.5], [0.5, 64.5, 0.5], shapesSet(['2,64,0', [CUBE]]), false]
]) {
  test(`rayClear: ${name}`, () => assert.equal(rayClear(pt(from), pt(to), shapes), clear))
}

test('blocksSight: lava blocks, beds and glass do not, a full stone does', () => {
  const full = name => ({ name, boundingBox: 'block' })
  const none = name => ({ name, boundingBox: 'empty' })
  assert.deepEqual([full('stone'), none('lava'), full('red_bed'), full('glass'), null].map(b => blocksSight(b)), [true, true, false, false, false])
})
