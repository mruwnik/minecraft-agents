// What a lead says when it gives up fetching an animal. Three fetches "got no nearer" used to blame the animal ("the
// cow will not follow: is there a fence or water between you?") when the evidence said the BODY never moved: three
// flock.lead runs beside a wheat field's fence stalled pressing forward at the same cell with a found path (card
// fc47bf28). A frozen walk during the fetches is the body's own failure, and the answer says so, with what froze it.
import test from 'node:test'
import assert from 'node:assert/strict'
import { fetchFailure, stalledSince } from '../src/fetch.mjs'

const FROZEN = { at: 1000, pos: { x: 119, y: 72, z: -66 }, advice: 'the head faces 90 degrees off the next node (119.5,72,-66.5) and the legs push into birch_fence at 119,72,-67: something else is turning the head (a lookAt in the task, or the fence nudge)' }

test('fetchFailure: no frozen walk, the animal did not come', () => {
  assert.equal(fetchFailure({ mob: 'cow', frozen: null }),
    'the cow will not follow (fetched it 3 times, got no nearer): is there a fence or water between you? Get them out in the open first, or lead fewer')
})

test('fetchFailure: a frozen walk during the fetches is my failure, not the animal’s, and names the cause', () => {
  assert.equal(fetchFailure({ mob: 'cow', frozen: FROZEN }),
    `I could not walk to the cow (stalled at 119,72,-66 pressing forward): ${FROZEN.advice}`)
})

test('fetchFailure: a frozen walk with no advice still says where', () => {
  assert.equal(fetchFailure({ mob: 'sheep', frozen: { at: 5, pos: { x: 1, y: 2, z: 3 } } }),
    'I could not walk to the sheep (stalled at 1,2,3 pressing forward)')
})

for (const [name, frozen, since, expected] of [
  ['none seen', null, 100, null],
  ['seen before the fetches began: an older stall is not this one', { ...FROZEN, at: 50 }, 100, null],
  ['seen since the fetches began', FROZEN, 100, FROZEN],
  ['seen the moment they began', { ...FROZEN, at: 100 }, 100, { ...FROZEN, at: 100 }]
]) {
  test(`stalledSince: ${name}`, () => assert.deepEqual(stalledSince(frozen, since), expected))
}
