// What a task carries out with it when it does not finish (card 8946c03a): the report a composite built before it was
// stopped, cancelled or killed rides on its FAIL line, and a death during it names where the kit lies
import test from 'node:test'
import assert from 'node:assert/strict'
import { carryReport, failedResult, deathLine, deathCancel } from '../src/composite.mjs'

const progress = { built: 12, stage: 'walls 2 of 3', dug: '3,64,1' }

// ---------------------------------------------------------------- carryReport
for (const [name, error, report, notes, expected] of [
  ['the report so far rides on the error', new Error('cancelled'), progress, [], progress],
  ['notes are joined onto it', new Error('cancelled'), progress, ['scaffold left at 3,66,1', 'gate shut'], { ...progress, notes: 'scaffold left at 3,66,1; gate shut' }],
  ['no notes, no notes key', new Error('cancelled'), { built: 1 }, [], { built: 1 }],
  ['what the error already carries wins', Object.assign(new Error('x'), { report: { restorationPending: '1,64,1', built: 99 } }), progress, [], { ...progress, restorationPending: '1,64,1', built: 99 }],
  ['an empty report is still attached, as nothing', new Error('x'), {}, [], {}]
]) {
  test(`carryReport: ${name}`, () => {
    const carried = carryReport(error, report, notes)
    assert.equal(carried, error)
    assert.deepEqual(carried.report, expected)
  })
}

// ---------------------------------------------------------------- failedResult
const base = { task: 4, action: 'blueprint.build', seconds: 84, pos: { x: 1, y: 64, z: 2 } }
const explain = m => `explained: ${m}`
for (const [name, given, expected] of [
  ['a stop keeps built=, stage= and dug=, and says stop',
    { base, report: progress, cancelled: true, why: 'stop', message: 'goal was changed' },
    { ...base, ...progress, ok: false, error: 'cancelled: stop' }],
  ['a death is a cancel in the death\'s words, with the report',
    { base, report: { ...progress, carried: 'stone_pickaxe lies at 1,64,2' }, cancelled: true, why: 'died at 1,64,2 (slain by Zombie)', message: 'goal was changed' },
    { ...base, ...progress, carried: 'stone_pickaxe lies at 1,64,2', ok: false, error: 'cancelled: died at 1,64,2 (slain by Zombie)' }],
  ['a step that threw keeps the report and explains the error',
    { base, report: progress, cancelled: false, message: 'no walkable path', explain },
    { ...base, ...progress, ok: false, error: 'explained: no walkable path' }],
  ['a composite still restoring a block keeps its own words over the cancel',
    { base, report: { restorationPending: '5,64,5' }, cancelled: true, why: 'superseded by goto', message: 'lectern not back yet', explain },
    { ...base, restorationPending: '5,64,5', ok: false, error: 'explained: lectern not back yet' }],
  ['the report never overrides ok= or error=',
    { base, report: { ok: true, error: 'none', built: 2 }, cancelled: true, why: 'stop', message: 'x' },
    { ...base, built: 2, ok: false, error: 'cancelled: stop' }],
  ['no report at all is the line it always was',
    { base, cancelled: true, why: 'stop', message: 'x' },
    { ...base, ok: false, error: 'cancelled: stop' }]
]) {
  test(`failedResult: ${name}`, () => assert.deepEqual(failedResult(given), expected))
}

// ---------------------------------------------------------------- deathLine
const fell = { x: 101.4, y: 66, z: -12.7 }
for (const [name, given, expected] of [
  ['a death during the task names the kit and the cell it fell in', { diedAt: 200, startedAt: 100, pos: fell, kit: 'cobblestone:40' }, 'cobblestone:40 lies at 101,66,-13'],
  ['a death before the task started is not this task\'s', { diedAt: 50, startedAt: 100, pos: fell, kit: 'bucket' }, null],
  ['no death at all', { diedAt: 0, startedAt: 100, pos: fell, kit: 'bucket' }, null],
  ['nothing worth naming carried: no line', { diedAt: 200, startedAt: 100, pos: fell, kit: null }, null],
  ['no cell known: no line', { diedAt: 200, startedAt: 100, pos: null, kit: 'bucket' }, null]
]) {
  test(`deathLine: ${name}`, () => assert.equal(deathLine(given), expected))
}

for (const [name, given, expected] of [
  ['cell and cause', { pos: fell, cause: 'slain by Zombie' }, 'died at 101,66,-13 (slain by Zombie)'],
  ['cell alone', { pos: fell }, 'died at 101,66,-13'],
  ['neither', {}, 'died']
]) {
  test(`deathCancel: ${name}`, () => assert.equal(deathCancel(given), expected))
}
