// Two body primitives from one tester day (cards c13b704d and 8c7b6652). `give count=101` tossed one stack of 64 and
// said taken=yes with 37 left in the pocket unmentioned, and a toss from 3 blocks off lay where the player never came;
// `craft item=bread count=3` read its pockets while the crafting window's slots were still coming back from the server
// and called the whole wheat stack spent, and the `eat` right after it asked for bread from a slot the server had
// just moved. The pure halves live in src/lib/handover.mjs (what to give, how far a drop lies) and src/lib/settle.mjs
// (when an inventory has stopped moving, and whether a meal that "never showed" showed after all).
import test from 'node:test'
import assert from 'node:assert/strict'
import { givePlan, GIVE_REACH, tooFarToGive, lyingFrom, shortNote } from '../src/lib/handover.mjs'
import { settleVerdict, SETTLE_QUIET_MS, SETTLE_MAX_MS, lateMeal } from '../src/lib/settle.mjs'
import { craftReport, craftShortfall } from '../src/lib/craft.mjs'

// what to toss: every stack until the count is met, and what stays behind when the pocket holds less than asked
for (const [name, args, expected] of [
  ['101 over two stacks', { count: 101, carried: 101 }, { give: 101, short: 0 }],
  ['91 over two stacks', { count: 91, carried: 91 }, { give: 91, short: 0 }],
  ['30 from one stack', { count: 30, carried: 64 }, { give: 30, short: 0 }],
  ['no count: the lot', { count: undefined, carried: 101 }, { give: 101, short: 0 }],
  ['more than carried: what there is, and the rest named', { count: 101, carried: 90 }, { give: 90, short: 11 }],
  ['nothing carried', { count: 5, carried: 0 }, { give: 0, short: 5 }]
]) {
  test(`givePlan: ${name}`, () => assert.deepEqual(givePlan(args), expected))
}

test('shortNote: nothing short says nothing', () => assert.equal(shortNote({ item: 'wheat', asked: 30, carried: 64 }), null))
test('shortNote: names what was asked, what was carried and what went', () => assert.equal(shortNote({ item: 'wheat', asked: 101, carried: 90 }), 'asked for wheat:101 but carried 90: gave the 90'))

// arm's reach: a toss flies about three blocks, so from further than two it lands beyond the player
test('GIVE_REACH is two blocks', () => assert.equal(GIVE_REACH, 2))
for (const [name, distance, expected] of [
  ['beside them', 1.4, null],
  ['at the edge', 2.0, null],
  ['half a block of slack for where they stand in their cell', 2.4, null],
  ['just beyond', 2.6, 'Jizo is 2.6 blocks away, beyond arm\'s reach (2): nothing given. Ask them to stand still, or use a chest'],
  ['walked off', 7.2, 'Jizo is 7.2 blocks away, beyond arm\'s reach (2): nothing given. Ask them to stand still, or use a chest']
]) {
  test(`tooFarToGive: ${name}`, () => assert.equal(tooFarToGive('Jizo', distance), expected))
}

// where the drop lies, with how far that is from the player: "has not picked it up" was read as a full inventory
// when it was three blocks of distance
for (const [name, drop, player, expected] of [
  ['three blocks off', { x: 19.3, y: 64, z: -75.5 }, { x: 16.5, y: 64, z: -75.5 }, '19,64,-76 (2.8 blocks from Jizo)'],
  ['at their feet', { x: 21.5, y: 63, z: -85.5 }, { x: 21.5, y: 63, z: -85.5 }, '21,63,-86 (0 blocks from Jizo)'],
  ['player gone', { x: 19.3, y: 64, z: -75.5 }, null, '19,64,-76 (Jizo out of sight)']
]) {
  test(`lyingFrom: ${name}`, () => assert.equal(lyingFrom(drop, player, 'Jizo'), expected))
}

// an inventory has settled when no slot has changed for SETTLE_QUIET_MS; a wait never outlives SETTLE_MAX_MS
test('settle: a tenth of a second of quiet, two seconds at most', () => { assert.equal(SETTLE_QUIET_MS, 100); assert.equal(SETTLE_MAX_MS, 2000) })
for (const [name, args, expected] of [
  ['a slot moved just now', { lastChangeAt: 1000, startedAt: 900, now: 1050 }, 'wait'],
  ['quiet for the tenth', { lastChangeAt: 1000, startedAt: 900, now: 1100 }, 'settled'],
  ['nothing ever moved', { lastChangeAt: null, startedAt: 900, now: 1000 }, 'settled'],
  ['slots still moving at the cap', { lastChangeAt: 2850, startedAt: 900, now: 2900 }, 'timeout'],
  ['quiet and past the cap both: settled', { lastChangeAt: 2700, startedAt: 900, now: 2950 }, 'settled']
]) {
  test(`settleVerdict: ${name}`, () => assert.equal(settleVerdict(args), expected))
}

// a meal the plugin called missing: read the pockets and the food number again once they have settled
const NEVER = new Error('the meal never showed: food is still 6 and I still carry 1 bread 3000 ms on')
for (const [name, args, expected] of [
  ['the food number rose after all', { failure: NEVER, before: { food: 6, carried: 1 }, after: { food: 11, carried: 0 } }, 'ate'],
  ['the bread left the pocket', { failure: NEVER, before: { food: 6, carried: 3 }, after: { food: 6, carried: 2 } }, 'ate'],
  ['nothing moved: the slot was stale, try once more', { failure: NEVER, before: { food: 6, carried: 1 }, after: { food: 6, carried: 1 } }, 'retry'],
  ['a second miss is a failure', { failure: NEVER, before: { food: 6, carried: 1 }, after: { food: 6, carried: 1 }, retried: true }, 'failed'],
  ['any other failure stands', { failure: new Error('Eating manually canceled!'), before: { food: 6, carried: 1 }, after: { food: 6, carried: 1 } }, 'failed'],
  ['no failure, nothing to judge', { failure: null, before: { food: 6, carried: 1 }, after: { food: 6, carried: 1 } }, 'failed']
]) {
  test(`lateMeal: ${name}`, () => assert.equal(lateMeal(args), expected))
}

// craftReport by delta: what went in is what the settled pockets lost, never the pre-craft count
for (const [name, args, expected] of [
  ['three of three made', { item: 'bread', count: 3, made: 3, spent: { wheat: 9 } }, { crafted: 'bread', made: 3 }],
  ['one of three made, three wheat gone', { item: 'bread', count: 3, made: 1, spent: { wheat: 3 }, why: 'the ingredients ran out' }, { error: 'the ingredients ran out: only 1 of 3 bread made, and wheat:3 went into them' }],
  ['none made, nothing gone', { item: 'bread', count: 3, made: 0, spent: { wheat: 0 }, why: 'the server kept rejecting the craft' }, { error: 'the server kept rejecting the craft: no bread made at all and nothing was consumed, so nothing is lost: retry once, the second call usually works' }]
]) {
  test(`craftReport by delta: ${name}`, () => assert.deepEqual(craftReport(args), expected))
}
test('craftShortfall: an ingredient short says how many', () => assert.equal(craftShortfall([{ wheat: 3 }], { wheat: 2 }), 'wheat:1'))
