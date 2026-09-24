// The one-line result renderer that ./mc prints (src/cli.mjs, imported by nothing but tools/mc.mjs and lib.mjs)
import test from 'node:test'
import assert from 'node:assert/strict'
import { terse } from '../src/cli.mjs'

// `eat` answers ate= and gained= (AGENT_GUIDE), the same names a long result renders as +item:n and ate=item:n. The short
// renderer dropped both as long-result bookkeeping: Chani ate at food 7 and read `ok food=10 health=10`, then reported
// "eat returns ok without eating anything" (13:53Z); my carrot went 3 -> 2 and the line said only `ok food=20 health=20`
for (const [name, result, expected] of [
  ['eat says what it ate and what it gained', { ok: true, ate: 'carrot', gained: 3, food: 20, health: 20 }, 'ok ate=carrot gained=3 food=20 health=20'],
  ['a meal the plugin chose still names the food', { ok: true, ate: 'bread', gained: 5, food: 12, health: 9 }, 'ok ate=bread gained=5 food=12 health=9'],
  ['a meal that gained nothing says so (the food number can lag the meal)', { ok: true, ate: 'carrot', gained: 0, food: 20, health: 20 }, 'ok ate=carrot gained=0 food=20 health=20']
]) {
  test(`terse (short result): ${name}`, () => assert.equal(terse(result), expected))
}
// a long result's counts keep their signs and their place
test('terse (long result): gained, lost and ate keep the +/- notation', () =>
  assert.equal(terse({ ok: true, task: 3, action: 'goto', seconds: 4, gained: { mutton: 1 }, lost: { bread: 1 }, ate: { bread: 1 }, pos: { x: 1, y: 2, z: 3 } }), 'ok goto 4s +mutton:1 -bread:1 ate=bread:1 @1,2,3'))
