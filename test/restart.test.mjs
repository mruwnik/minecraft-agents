// Restarting a body at night: a body started outside after dark dies before its driver can act (13:40Z, a7cd6038).
// The advice a running body gives with code_updated, and the gate ./start runs first, both read the shared clock.
import test from 'node:test'
import assert from 'node:assert/strict'
import { nightClock, restartAdvice, startRefusal } from '../src/restart.mjs'

const NOW = 1790270754785
const fresh = (extra, ago = 5000) => ({ by: 'Mariel', at: NOW - ago, ...extra })
const day = fresh({ day: true, timeOfDay: 6000 })
const night = fresh({ day: false, timeOfDay: 14000 })

const clocks = [
  ['a clock that says day', day, false],
  ['a clock that says night', night, true],
  ['a tick past 12500 with no day flag', fresh({ timeOfDay: 13000 }), true],
  ['a tick just before dawn is still night', fresh({ timeOfDay: 23000 }), true],
  ['a tick after 23460 is day again', fresh({ timeOfDay: 23800 }), false],
  ['a tick before 12500 with no day flag', fresh({ timeOfDay: 12000 }), false],
  ['no clock at all', null, false],
  ['a clock nobody has written for two minutes (no body online: nothing to die at night)', fresh({ day: false, timeOfDay: 14000 }, 120000), false]
]
for (const [what, clock, expected] of clocks) {
  test(`nightClock: ${what}`, () => { assert.equal(nightClock(clock, NOW), expected) })
}

test('restartAdvice: by day, restart when idle somewhere safe', () => {
  assert.equal(restartAdvice(day, NOW), 'the shared code has fixes you are not running. No hurry: restart (./mc quit, then ./start) next time you are idle somewhere safe, or at once if a tool misbehaves')
})

test('restartAdvice: at night, wait for dawn unless you spawn into a bed', () => {
  assert.equal(restartAdvice(night, NOW), 'the shared code has fixes you are not running, but it is night (tick 14000) and a body restarted outside now dies: wait for dawn (./mc dawn) unless you spawn into a bed, then restart (./mc quit, then ./start)')
})

test('restartAdvice: with no clock, the daytime advice', () => {
  assert.equal(restartAdvice(null, NOW), restartAdvice(day, NOW))
})

const starts = [
  ['by day', day, false, null],
  ['with no clock', null, false, null],
  ['with a stale night clock', fresh({ day: false, timeOfDay: 14000 }, 120000), false, null],
  ['at night with --now (the driver spawns into a bed or under a roof)', night, true, null],
  ['at night', night, false, 'it is night (tick 14000, seen 5s ago by Mariel): a body started outside now dies. Wait for dawn (./mc dawn) unless you spawn into a bed, then ./start --now to say so']
]
for (const [what, clock, now, expected] of starts) {
  test(`startRefusal: ${what}`, () => { assert.equal(startRefusal(clock, NOW, now), expected) })
}
