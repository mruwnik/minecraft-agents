// What a lead says when it gives up fetching an animal. Three fetches "got no nearer" used to blame the animal ("the
// cow will not follow: is there a fence or water between you?") when the evidence said the BODY never moved: three
// flock.lead runs beside a wheat field's fence stalled pressing forward at the same cell with a found path (card
// fc47bf28). A frozen walk during the fetches is the body's own failure, and the answer says so, with what froze it.
import test from 'node:test'
import assert from 'node:assert/strict'
import { fetchFailure, stalledSince, fencedRefusal, wedgedIn, wedgedRefusal } from '../src/fetch.mjs'

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

// ---------------------------------------------------------------- an animal fenced in: said before the fetches, not after three
// Two cows stood inside a small decorative fence cell inside a wheat field (card fc47bf28): the walk into the pen
// never fails cleanly (the pathfinder follows partial paths round the fence until the 12 s stall alarm cancels the
// task), so the pen is judged before any walk: where the animal stands, and whether the body's own feet are in it
const PEN = { enclosed: true, cells: 2, floor: ['114,72,-70', '115,72,-70'] }
for (const [name, input, expected] of [
  ['the body stands outside the pen: fenced in, with the pen check to run', { mob: 'cow', at: '114,72,-70', pen: PEN, feet: { x: 108.3, y: 72, z: -68.6 } },
    'the cow at 114,72,-70 stands fenced in (a 2-cell pen) and I found no way in: open a gate or a fence post beside it (pen.check x=114 y=72 z=-70 names its gates), or lead from inside'],
  ['the body stands in the pen: nothing to say', { mob: 'cow', at: '114,72,-70', pen: PEN, feet: { x: 115.4, y: 72, z: -69.5 } }, null],
  ['the body stands in the pen on a half step', { mob: 'cow', at: '114,72,-70', pen: PEN, feet: { x: 114.5, y: 72.5, z: -69.2 } }, null],
  ['a whole block above the floor is not in the pen', { mob: 'cow', at: '114,72,-70', pen: PEN, feet: { x: 114.5, y: 74, z: -69.5 } },
    'the cow at 114,72,-70 stands fenced in (a 2-cell pen) and I found no way in: open a gate or a fence post beside it (pen.check x=114 y=72 z=-70 names its gates), or lead from inside'],
  ['no pen round the animal: the walk is the fetch loop\u2019s business', { mob: 'cow', at: '114,72,-70', pen: null, feet: { x: 108.3, y: 72, z: -68.6 } }, null],
  ['a leaking enclosure is no pen', { mob: 'sheep', at: '1,2,3', pen: { enclosed: false, via: '1,2,4' }, feet: { x: 0, y: 2, z: 0 } }, null]
]) {
  test(`fencedRefusal: ${name}`, () => assert.equal(fencedRefusal(input), expected))
}

// ---------------------------------------------------------------- an animal wedged in a block: it cannot walk, so it cannot be led
// The same two cows (card fc47bf28) both reported the cell 114,72,-70, a birch fence post: any animal centred in a
// fence's cell overlaps the fence's collision, so it stands wedged and no fetch moves it. A floor block that only
// raises the feet (a slab, a carpet) is what the animal stands ON, not what it stands IN.
const FENCE = { name: 'birch_fence', shapes: [[0.375, 0, 0.375, 0.625, 1.5, 0.625], [0, 0, 0.375, 0.375, 1.5, 0.625]] }
const SLAB = { name: 'oak_slab', shapes: [[0, 0, 0, 1, 0.5, 1]] }
const CARPET = { name: 'white_carpet', shapes: [[0, 0, 0, 1, 0.0625, 1]] }
const LITTER = { name: 'leaf_litter', shapes: [] }
for (const [name, block, feetY, expected] of [
  ['a fence post round the feet', FENCE, 72, 'birch_fence'],
  ['a full block round the feet', { name: 'dirt', shapes: [[0, 0, 0, 1, 1, 1]] }, 72, 'dirt'],
  ['a slab the feet stand on', SLAB, 72.5, null],
  ['a carpet the feet stand on', CARPET, 72.0625, null],
  ['ground cover with no box', LITTER, 72, null],
  ['air', { name: 'air', shapes: [] }, 72, null],
  ['an unloaded cell', null, 72, null]
]) {
  test(`wedgedIn: ${name}`, () => assert.equal(wedgedIn(block, feetY), expected))
}
for (const [name, input, expected] of [
  ['wedged: free it or lead another', { mob: 'cow', at: '114,72,-70', block: 'birch_fence' },
    'the cow at 114,72,-70 stands wedged in a birch_fence and cannot walk: free it (dig x=114 y=72 z=-70, if that fence is yours to break) or lead another'],
  ['not wedged: nothing to say', { mob: 'cow', at: '114,72,-69', block: null }, null]
]) {
  test(`wedgedRefusal: ${name}`, () => assert.equal(wedgedRefusal(input), expected))
}
