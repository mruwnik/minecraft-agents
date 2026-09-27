import test from 'node:test'
import assert from 'node:assert/strict'
import routine from '../library/routine.mjs'
import { fakeApi } from './helpers.mjs'
import { wakeWorthy, waitReport } from '../src/cli.mjs'
import { outcomeStatus } from '../src/routine.mjs'

const sites = ['a', 'b'].map((name, i) => ({ name, by: 'Tester', kind: 'forest', x: i * 20, y: 64, z: 0 }))
for (const action of ['forestry.maintain', 'tree.harvest', 'scaffold.access', 'scaffold.cleanup']) {
  for (const failure of [new TypeError('unexpected tree failure'), { stopped: 'health 4' }, { stopped: 'cancelled' }]) {
    test(`${action}: routine propagates fatal result before the next site (${failure.message ?? failure.stopped})`, async () => {
      const { api, calls, events } = fakeApi({ places: sites, answers: { [action]: failure } })
      await assert.rejects(routine.run(api, { steps: [{ action, place: '$place' }], place: 'a,b' }), /unexpected tree failure|health 4|cancelled/)
      assert.equal(calls.filter(c => c.startsWith(action + ' ')).length, 1)
      assert.equal(events.at(-1).type, 'routine_stopped')
    })
  }
}
test('forestry attention reaches the waiting driver while useful work continues at the next site', async () => {
  const event = { type: 'forestry_attention', action: 'forestry.maintain', reasons: ['missing spruce_sapling:4'] }
  assert.equal(wakeWorthy(event, 'Tester'), true)
  assert.ok(waitReport(JSON.stringify(event) + '\n', 'Tester').lines.length)
  const { api, calls, events } = fakeApi({ places: sites, answers: { 'forestry.maintain': { attention: event.reasons, stopped: 'done' } } })
  const result = await routine.run(api, { steps: [{ action: 'forestry.maintain', place: '$place' }], place: 'a,b' })
  assert.equal(result.ran, 2)
  assert.equal(calls.filter(c => c.startsWith('forestry.maintain ')).length, 2)
  assert.match(events.find(e => e.type === 'routine_day').places.a['forestry.maintain'], /^incomplete/)
})
test('healthy forestry empty attention list does not mark a successful day incomplete', () => {
  assert.equal(outcomeStatus({ attention: [], planted: 1, stopped: 'done' }), 'ok')
  assert.equal(outcomeStatus({ attention: ['upper trunk unreachable'], stopped: 'done' }), 'incomplete')
})
for (const repeated of [false, true]) test(`forestry night handback retries the same work at most once (repeated=${repeated})`, async () => {
  let worked = 0
  const { api, calls } = fakeApi({ places: [...sites, { name: 'bed', kind: 'bed', by: 'Tester', x: 60, y: 64, z: 0 }], answers: {
    'forestry.maintain': () => ++worked === 1 || repeated ? { stopped: 'night and no bed within 32 blocks' } : { attention: [], sweeps: 1 }
  } })
  const run = routine.run(api, { steps: [{ action: 'forestry.maintain', place: '$place' }], place: 'a', bed: 'bed' })
  if (repeated) await assert.rejects(run, /night and no bed/)
  else await run
  assert.equal(worked, 2)
  assert.equal(calls.filter(c => c.startsWith('goto ') && c.endsWith('range=2')).length, 1)
})
