import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { handBackReason } from '../src/lib/composite.mjs'
import { nightPlan } from '../src/lib/sleep.mjs'

// checkpoint() runs between almost every composite step (library/blueprint/build.mjs calls it once per block placed, a
// 40s profile of farm.build found checkpoint -> bedsNear -> findBlocksNear taking a quarter of the body's wall time).
// handBackReason only reads bedNear when it is night (`s.night && !s.bedNear`), so the world scan it costs should only
// run then. checkpoint is a closure inside makeApi with no exported seam, so its source is spliced out and run against
// a fake bot, the same way test/escort-navigation.test.mjs exercises the production leadWalk.
const source = fs.readFileSync(new URL('../src/body/runner.mjs', import.meta.url), 'utf8')
const START = 'const checkpoint = async (extra = {}) => {'
const END = '\n  return {\n    notes,'
const body = source.split(START)[1].split(END)[0]

function makeCheckpoint ({ night, sleptTonight = true, beds = [], nightBed, goto = async () => {} } = {}) {
  const calls = { bedsNear: 0, automaticSleepBeds: 0, acts: [] }
  const env = {
    pendingNavigationFailure: null,
    alive: () => {},
    night: () => night,
    sleptTonight,
    bot: { vehicle: null, isSleeping: false, health: 20, food: 20, inventory: { emptySlotCount: () => 1 } },
    HandBack: class HandBack extends Error { constructor (reason) { super(reason); this.reason = reason } },
    automaticSleepBeds: () => { calls.automaticSleepBeds++; return [{}] },
    ownerTask: nightBed ? { nightBed } : null,
    pos: () => ({ x: 0, y: 64, z: 0 }),
    nightPlan,
    notes: [],
    task: null,
    emit: () => {},
    composite: 'test.composite',
    PAUSES: { night: 'paused' },
    jobEvent: () => {},
    long: {
      sleep: async args => { calls.acts.push(['sleep', args]) },
      goto: async args => { calls.acts.push(['goto', args]); return goto(args) }
    },
    until: async () => {},
    handBackReason,
    edibleCarried: () => true,
    failedTwice: () => undefined,
    a: {},
    worldDay: () => 0,
    startedDay: 0,
    bedsNear: () => { calls.bedsNear++; return beds },
    startedAt: Date.now()
  }
  const checkpoint = new Function(...Object.keys(env), 'return async function checkpoint(extra = {}) {' + body)(...Object.values(env))
  return { checkpoint, calls }
}

test('checkpoint does not scan the world for beds during the day', async () => {
  const { checkpoint, calls } = makeCheckpoint({ night: false })
  await checkpoint({})
  assert.equal(calls.bedsNear, 0)
})

test('checkpoint still scans for beds at night, to hand back when none are near', async () => {
  const { checkpoint, calls } = makeCheckpoint({ night: true, beds: [] })
  await assert.rejects(checkpoint({}), /night and no bed within 32 blocks/)
  assert.equal(calls.bedsNear, 1)
})

test('checkpoint does not hand back at night with a bed near', async () => {
  const { checkpoint } = makeCheckpoint({ night: true, beds: [{}] })
  await assert.doesNotReject(checkpoint({}))
})

// routine bed=: a step is its own composite, so its checkpoint is where the night is met. The routine's bed rides on
// the task they share and wins over a nearer bed
const hut = { bed: { name: 'the bed at 108,71,-107', x: 108, y: 71, z: -107 }, bedRange: 200 }
test('at nightfall a task\'s own bed is walked to and slept in, not the nearest one', async () => {
  const { checkpoint, calls } = makeCheckpoint({ night: true, sleptTonight: false, beds: [{}], nightBed: hut })
  await checkpoint({})
  assert.deepEqual(calls.acts, [['goto', { x: 108, y: 71, z: -107, range: 2 }], ['sleep', { bed: '108,71,-107' }]])
  assert.equal(calls.automaticSleepBeds, 0)
})

for (const [name, nightBed, goto, error] of [
  ['beyond bed_range', { ...hut, bedRange: 50 }, undefined, /night, and the bed at 108,71,-107 is \d+ blocks away, beyond bed_range=50$/],
  ['out of reach', hut, async () => { throw new Error('no path to the goal') }, /night, and the walk to the bed at 108,71,-107 failed: no path to the goal$/]
]) {
  test(`a task's own bed ${name} hands back rather than sleeping in a nearer one`, async () => {
    const { checkpoint, calls } = makeCheckpoint({ night: true, sleptTonight: false, beds: [{}], nightBed, goto })
    await assert.rejects(checkpoint({}), error)
    assert.deepEqual(calls.acts.filter(([act]) => act === 'sleep'), [])
  })
}
