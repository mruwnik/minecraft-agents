// Which ?agent= values the view page accepts: <world>/<name>, as the view server addresses a body.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { isAgentKey } from '../tools/view/web/agent-key.mjs'

for (const [value, expected] of [
  ['claude/ProbeDrive', true],
  ['w_1/Bob-2', true],
  ['ProbeDrive', false],
  ['a/b/c', false],
  ['/b', false],
  ['a/', false],
  ['a b/c', false],
  ['', false],
  [null, false]
]) {
  test(`isAgentKey ${JSON.stringify(value)}`, () => assert.equal(isAgentKey(value), expected))
}
