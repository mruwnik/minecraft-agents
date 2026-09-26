// Leads (card 43a32481): the toolset had no lead action, so two leads in a chest did nothing and every lead walk was a
// lure with food. The pure half in src/lib/leash.mjs decides which animals get a lead, when the walk goes or waits,
// and what is said about leads borrowed from a chest; flock.lead borrows and returns them around the walk.
import test from 'node:test'
import assert from 'node:assert/strict'
import { LEAD_BREAK, LEAD_SLACK, leashable, leashPlan, leashVerdict, leadBroke, leadBorrow, leadReturn, leashedLine, NO_LEAD } from '../src/lib/leash.mjs'
import { fakeApi } from './helpers.mjs'
import lead from '../library/flock/lead.mjs'

test('a lead breaks at ten blocks and the walk holds at six', () => { assert.equal(LEAD_BREAK, 10); assert.equal(LEAD_SLACK, 6) })

for (const [mob, expected] of [['cow', true], ['sheep', true], ['horse', true], ['villager', false], ['zombie', false], ['player', false]]) {
  test(`leashable: ${mob}`, () => assert.equal(leashable(mob), expected))
}

// which animals get a lead
const cows = [
  { id: 1, dist: 3, grown: true, penned: false },
  { id: 2, dist: 5, grown: false, penned: false },
  { id: 3, dist: 7, grown: true, penned: false },
  { id: 4, dist: 2, grown: true, penned: true }
]
for (const [name, args, expected] of [
  ['one lead, the nearest grown one', { mob: 'cow', leads: 1, count: 1, candidates: cows }, { take: [1] }],
  ['two leads for two: grown before a calf, near before far', { mob: 'cow', leads: 2, count: 2, candidates: cows }, { take: [1, 3] }],
  ['three asked with two leads: two go, and the note says so', { mob: 'cow', leads: 2, count: 3, candidates: cows }, { take: [1, 3], note: 'only 2 leads carried: leashing 2, not 3' }],
  ['one lead for two', { mob: 'cow', leads: 1, count: 2, candidates: cows }, { take: [1], note: 'only 1 lead carried: leashing 1, not 2' }],
  ['a calf when nothing grown is free', { mob: 'cow', leads: 1, count: 1, candidates: [cows[1], cows[3]] }, { take: [2] }],
  ['penned=true takes the one in the pen', { mob: 'cow', leads: 1, count: 1, candidates: cows, allowPenned: true }, { take: [4] }],
  ['no leads', { mob: 'cow', leads: 0, count: 1, candidates: cows }, { error: NO_LEAD }],
  ['nothing in reach', { mob: 'cow', leads: 2, count: 1, candidates: [] }, { error: 'no cow within reach' }],
  ['only penned ones in reach', { mob: 'cow', leads: 2, count: 1, candidates: [cows[3]] }, { error: 'every cow in reach stands in a pen: penned=true to take one from a pen of your own' }],
  ['a villager takes no lead', { mob: 'villager', leads: 2, count: 1, candidates: cows }, { error: 'a villager takes no lead' }]
]) {
  test(`leashPlan: ${name}`, () => assert.deepEqual(leashPlan(args), expected))
}

// the walk: go while every lead is slack, hold when one is pulled tight, stop when a lead came off
for (const [name, args, expected] of [
  ['both close', { distances: [2, 4], held: 2 }, 'go'],
  ['one at the slack', { distances: [2, 6], held: 2 }, 'go'],
  ['one beyond the slack', { distances: [2, 6.5], held: 2 }, 'hold'],
  ['a lead came off', { distances: [3], held: 2 }, 'broke'],
  ['both gone', { distances: [], held: 2 }, 'broke'],
  ['nothing was ever held', { distances: [], held: 0 }, 'lost'],
  ['no path while they are close', { distances: [2, 3], held: 2, noPath: true }, 'noway']
]) {
  test(`leashVerdict: ${name}`, () => assert.equal(leashVerdict(args), expected))
}

test('leadBroke names where the loose animal is', () => assert.match(leadBroke('cow', [{ x: 12, y: 64, z: -30 }]), /^a lead came off on the way: the cow at 12,64,-30 walks free and its lead lies about there/))

// borrowing leads from a chest for a walk, and putting them back
for (const [name, args, expected] of [
  ['none carried, two wanted', { carried: 0, want: 2 }, 2],
  ['one carried, two wanted', { carried: 1, want: 2 }, 1],
  ['enough carried', { carried: 2, want: 2 }, 0],
  ['more than enough', { carried: 3, want: 2 }, 0]
]) {
  test(`leadBorrow: ${name}`, () => assert.equal(leadBorrow(args), expected))
}
const CHEST = { x: 12, y: 63, z: -80 }
for (const [name, args, expected] of [
  ['nothing borrowed, nothing said', { borrowed: 0, carried: 2, chest: CHEST }, null],
  ['two borrowed, two back', { borrowed: 2, carried: 2, chest: CHEST }, { deposit: 2, line: '2 leads back in the chest at 12,63,-80' }],
  ['one borrowed, one back', { borrowed: 1, carried: 1, chest: CHEST }, { deposit: 1, line: '1 lead back in the chest at 12,63,-80' }],
  ['two borrowed, one lost', { borrowed: 2, carried: 1, chest: CHEST }, { deposit: 1, line: '1 of 2 leads back in the chest at 12,63,-80: 1 lost on the way (a lead that comes off drops where the animal was; collect it and deposit it there)' }],
  ['borrowed two, carried three of my own: only two go back', { borrowed: 2, carried: 3, chest: CHEST }, { deposit: 2, line: '2 leads back in the chest at 12,63,-80' }]
]) {
  test(`leadReturn: ${name}`, () => assert.deepEqual(leadReturn(args), expected))
}

test('leashedLine: mob#id@cell', () => assert.equal(leashedLine([{ name: 'cow', id: 12, x: 1.5, y: 64, z: -2.5 }, { name: 'cow', id: 13, x: 3.2, y: 64, z: -2.1 }]), 'cow#12@1,64,-3 cow#13@3,64,-3'))

// flock.lead with leads=x,y,z: takes the leads it lacks from that chest, walks, puts them back, and says so
const PLACES = [{ name: 'pen', x: 10, y: 64, z: 10, by: 'Tester' }]
const escortAnswer = { arrived: true, with: 2, brought: 'cow:2', byLead: true }
test('flock.lead borrows two leads, walks on them, returns them and says so', async () => {
  const { api, calls, report } = fakeApi({ places: PLACES, items: { wheat: 5 }, answers: { escort: escortAnswer, withdraw: () => { api.inv().lead = 2; return {} }, deposit: {}, 'pen.check': { pen: 'CLOSED' } } })
  const out = await lead.run(api, { mob: 'cow', place: 'pen', leads: '12,63,-80' })
  assert.ok(calls.includes('withdraw item=lead count=2 x=12 y=63 z=-80'), calls.join('\n'))
  assert.ok(calls.includes('deposit item=lead count=2 x=12 y=63 z=-80'), calls.join('\n'))
  assert.ok(calls.indexOf('withdraw item=lead count=2 x=12 y=63 z=-80') < calls.findIndex(c => c.startsWith('escort ')), 'borrow before the walk')
  assert.ok(calls.findIndex(c => c.startsWith('escort ')) < calls.indexOf('deposit item=lead count=2 x=12 y=63 z=-80'), 'return after the walk')
  assert.equal(out.leads, '2 leads back in the chest at 12,63,-80')
  assert.equal(report.leads, '2 leads back in the chest at 12,63,-80')
})
test('flock.lead with leads carried already borrows none', async () => {
  const { api, calls } = fakeApi({ places: PLACES, items: { lead: 2 }, answers: { escort: escortAnswer, 'pen.check': { pen: 'CLOSED' } } })
  const out = await lead.run(api, { mob: 'cow', place: 'pen', leads: '12,63,-80' })
  assert.ok(!calls.some(c => c.startsWith('withdraw ')), calls.join('\n'))
  assert.ok(!calls.some(c => c.startsWith('deposit ')), calls.join('\n'))
  assert.equal(out.leads, undefined)
})
test('flock.lead with leads carried needs no food', async () => {
  const { api } = fakeApi({ places: PLACES, items: { lead: 1 }, answers: { escort: escortAnswer, 'pen.check': { pen: 'CLOSED' } } })
  const out = await lead.run(api, { mob: 'cow', place: 'pen' })
  assert.equal(out.arrived, true)
})
test('flock.lead with neither food nor leads still refuses before walking', async () => {
  const { api, calls } = fakeApi({ places: PLACES, items: {}, answers: { escort: escortAnswer } })
  await assert.rejects(lead.run(api, { mob: 'cow', place: 'pen' }), /a cow follows wheat: you carry none/)
  assert.ok(!calls.some(c => c.startsWith('escort ')))
})
test('flock.lead puts the leads back even when the walk fails', async () => {
  const { api, calls } = fakeApi({ places: PLACES, items: {}, answers: { escort: new Error('no cow within 32 blocks'), withdraw: () => { api.inv().lead = 2; return {} }, deposit: {}, 'pen.check': { pen: 'CLOSED' } } })
  await assert.rejects(lead.run(api, { mob: 'cow', place: 'pen', leads: '12,63,-80' }), /no cow within 32 blocks/)
  assert.ok(calls.includes('deposit item=lead count=2 x=12 y=63 z=-80'), calls.join('\n'))
})
test('flock.lead: a lost lead is named on the way back', async () => {
  const { api, calls, report } = fakeApi({ places: PLACES, items: {}, answers: { escort: () => { api.inv().lead = 1; return escortAnswer }, withdraw: () => { api.inv().lead = 2; return {} }, deposit: {}, 'pen.check': { pen: 'CLOSED' } } })
  await lead.run(api, { mob: 'cow', place: 'pen', leads: '12,63,-80' })
  assert.ok(calls.includes('deposit item=lead count=1 x=12 y=63 z=-80'), calls.join('\n'))
  assert.match(report.leads, /^1 of 2 leads back in the chest at 12,63,-80: 1 lost on the way/)
})
test('flock.lead: leads= that is not a cell is refused', async () => {
  const { api } = fakeApi({ places: PLACES, items: {}, answers: {} })
  await assert.rejects(lead.run(api, { mob: 'cow', place: 'pen', leads: 'somewhere' }), /leads= wants the chest cell as x,y,z/)
})
