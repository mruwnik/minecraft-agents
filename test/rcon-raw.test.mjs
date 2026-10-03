import test from 'node:test'
import assert from 'node:assert/strict'
import { buildRawCommand } from '../tools/rcon-raw.mjs'

const cases = [
  [['list'], 'list'],
  [['time', 'set', 'day'], 'time set day'],
  [['/list'], 'list'],
  [['/', 'say', 'hi'], 'say hi'],
  [['//foo'], '/foo'],
  [['say  hi'], 'say  hi']
]
for (const [argv, expected] of cases) {
  test(`buildRawCommand ${JSON.stringify(argv)}`, () => assert.equal(buildRawCommand(argv), expected))
}

for (const argv of [[], [''], ['/'], ['  ']]) {
  test(`buildRawCommand rejects ${JSON.stringify(argv)}`, () => assert.throws(() => buildRawCommand(argv), /usage/))
}
