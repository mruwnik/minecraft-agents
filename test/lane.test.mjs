// The lane census (card 29167296, reopened): `farm.fields` on jizo-melon-patch said 140 of its 288 crop cells had
// "nothing walkable beside them", and a sweep of that same field had just harvested and replanted the lot. The census
// counted only the cells a body could stand NEXT TO, but a job stands within WORK_RANGE of its cell: from a lane four
// across (one up and four over is 4.12) the arm reaches every bed, so a covered channel every eight rows serves a whole
// field, as walk.mjs says. This census judges reach: a crop is stranded when no walkable cell (path, covered channel,
// gate, flower, sapling, or the ground round the plan) lies within four blocks of it with nothing taller than a crop
// between. A fence, a gate seen from outside, a torch post or a chest on the line blocks the arm as it blocks the walk.
import test from 'node:test'
import assert from 'node:assert/strict'
import { planLane, parsePlan } from '../src/lib.mjs'

const laneOf = (...rows) => planLane(parsePlan(rows.join('\n')).cells)
const band = (rows, width) => Array.from({ length: rows }, () => 'w'.repeat(width))
const jizo = ['C' + 'w'.repeat(15), ...band(3, 16), '~'.repeat(16), ...band(8, 16), '~'.repeat(16), ...band(4, 16)]
const advice = 'lay a . path or a ~ channel through the rows, eight rows apart at most, or every job there answers nowhere to stand'

for (const [name, cells, expected] of [
  ['the melon patch: eight rows between two covered channels are all within reach of one', laneOf(...jizo), {}],
  ['a covered channel is a lane, and four rows either side of it are served', laneOf(...band(4, 12), '~'.repeat(12), ...band(4, 12)), {}],
  ['ten rows between channels leave the middle two out of reach', laneOf('~'.repeat(12), ...band(10, 12), '~'.repeat(12)),
    { noLane: `8 crop cells have nothing to stand on within 4 of them (4,5 5,5 6,5 7,5 and 4 more): ${advice}` }],
  ['the ground round the plan serves its edges: an open field twelve across is worked from its sides', laneOf(...band(4, 12)), {}],
  ['a fence stops the arm as it stops the walk', laneOf('###', '#w#', '###'),
    { noLane: `1 crop cell has nothing to stand on within 4 of it (1,1): ${advice}` }],
  ['the gate cell serves the rows within reach of it even with no path laid', laneOf('#G##', '#cc#', '#~~#', '#cc#', '####'), {}],
  ['rows more than four from the gate are out of reach', laneOf('#G##', '#cc#', '#cc#', '#cc#', '#cc#', '#cc#', '#cc#', '####'),
    { noLane: `5 crop cells have nothing to stand on within 4 of them (2,4 1,5 2,5 1,6 and 1 more): ${advice}` }],
  ['a chest on the line is in the way of the arm', laneOf('#G##', '#Cc#', '#Cc#', '####'),
    { noLane: `1 crop cell has nothing to stand on within 4 of it (2,2): ${advice}` }],
  ['a torch post is a fence post', laneOf('#G##', '#Tc#', '#Tc#', '####'),
    { noLane: `1 crop cell has nothing to stand on within 4 of it (2,2): ${advice}` }],
  ['a plan with no crops has nothing to reach', laneOf('###', '#.#', '#G#'), {}],
  ['an empty plan is not a complaint', planLane([]), {}]
]) {
  test(`planLane: ${name}`, () => assert.deepEqual(cells, expected))
}
