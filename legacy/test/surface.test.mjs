// Out of air under a roof. A body drowned at -129.3,33.2,-138.3 (card a164bbfd): the reflex read an open column over its
// head, pressed jump AND forward, and forward carried it two blocks sideways under a rock ceiling, where jump pushed
// against stone until it died with an air cell two blocks away. The way out is judged here, pure, from what is over the
// head, what open water lies near, and how long the ceiling would take to dig with the air that is left.
import test from 'node:test'
import assert from 'node:assert/strict'
import { surfaceWay, airBudgetTicks, swimProgress, roofAt, SURFACE_SCAN } from '../src/navigation/surface.mjs'

const me = { x: -130.3, y: 33.2, z: -138.3 }
const cell = (x, y, z) => ({ x, y, z })
const stone = { x: -131, y: 35, z: -139, name: 'stone', digTicks: 575 }
const dirt = { x: -131, y: 35, z: -139, name: 'dirt', digTicks: 100 }

for (const [name, given, expected] of [
  ['open water over my head: straight up, and nothing else',
    { column: ['water', 'air'], openings: [cell(-128, 34, -138)], me }, { way: 'up' }],
  ['deep water with nothing in the way: up',
    { column: ['water', 'water', 'water'], openings: [], me }, { way: 'up' }],
  ['a roof over my head and open water two blocks off: swim there',
    { column: ['water', 'stone'], openings: [cell(-131, 34, -141), cell(-125, 34, -138)], me },
    { way: 'sideways', to: cell(-131, 34, -141), dist: 2.2 }],
  ['the nearest opening by distance along the water, not the first in the list',
    { column: ['stone'], openings: [cell(-125, 34, -138), cell(-129, 34, -138)], me },
    { way: 'sideways', to: cell(-129, 34, -138), dist: 2 }],
  ['an opening already tried and not reached is passed over',
    { column: ['stone'], openings: [cell(-129, 34, -138), cell(-125, 34, -138)], me, tried: ['-129,34,-138'] },
    { way: 'sideways', to: cell(-125, 34, -138), dist: 5.9 }],
  ['an opening past the scan range is no opening',
    { column: ['stone'], openings: [cell(-120, 34, -138)], me }, { way: 'up', note: 'no open water within 6 and nothing over my head I can dig in time: swimming up anyway' }],
  ['roofed with no opening: dig the ceiling when the air allows (dirt in 5 s, 8 air and full health)',
    { column: ['water', 'dirt'], openings: [], me, ceiling: dirt, oxygen: 8, health: 20 },
    { way: 'pocket', at: cell(-131, 35, -139), ticks: 100 }],
  ['roofed with no opening and stone over me: too slow for 8 air, swim up and say so',
    { column: ['water', 'stone'], openings: [], me, ceiling: stone, oxygen: 8, health: 20 },
    { way: 'up', note: 'no open water within 6 and nothing over my head I can dig in time: swimming up anyway' }],
  ['a ceiling worth digging is second to open water in reach',
    { column: ['dirt'], openings: [cell(-127, 34, -138)], me, ceiling: dirt, oxygen: 8, health: 20 },
    { way: 'sideways', to: cell(-127, 34, -138), dist: 3.9 }],
  ['every opening tried and a diggable roof: the pocket',
    { column: ['dirt'], openings: [cell(-127, 34, -138)], me, tried: ['-127,34,-138'], ceiling: dirt, oxygen: 8, health: 20 },
    { way: 'pocket', at: cell(-131, 35, -139), ticks: 100 }],
  ['a roof I cannot dig at all (no tool, bedrock): up with the note',
    { column: ['bedrock'], openings: [], me, ceiling: { x: -131, y: 35, z: -139, name: 'bedrock', digTicks: Infinity }, oxygen: 8, health: 20 },
    { way: 'up', note: 'no open water within 6 and nothing over my head I can dig in time: swimming up anyway' }]
]) test(`surfaceWay: ${name}`, () => assert.deepEqual(surfaceWay(given), expected))

test('SURFACE_SCAN is the six blocks the reflex looks round the body', () => assert.equal(SURFACE_SCAN, 6))

// oxygen 0..20 is the air bar; each point is 15 ticks, and once the bar is empty a drowning body loses 2 hearts a second
for (const [name, state, expected] of [
  ['full air and health', { oxygen: 20, health: 20 }, 500],
  ['8 air, full health: 6 s of air and 10 s of drowning', { oxygen: 8, health: 20 }, 320],
  ['no air, 5 health: three hits', { oxygen: 0, health: 5 }, 60],
  ['no air, 1 health: the next hit kills', { oxygen: 0, health: 1 }, 20]
]) test(`airBudgetTicks: ${name}`, () => assert.equal(airBudgetTicks(state), expected))

// a sideways swim that gets no nearer for 2 s is pressing into a wall: give that opening up and pick the next
for (const [name, track, dist, now, expected] of [
  ['first reading', null, 2.8, 1000, { best: 2.8, at: 1000, stalled: false }],
  ['nearer: the clock restarts', { best: 2.8, at: 1000 }, 2.0, 2500, { best: 2.0, at: 2500, stalled: false }],
  ['a hair nearer is not progress', { best: 2.8, at: 1000, }, 2.7, 3200, { best: 2.8, at: 1000, stalled: true }],
  ['no nearer for 1.5 s: keep trying', { best: 2.8, at: 1000 }, 2.8, 2400, { best: 2.8, at: 1000, stalled: false }],
  ['no nearer for 2 s: stalled', { best: 2.8, at: 1000 }, 3.1, 3000, { best: 2.8, at: 1000, stalled: true }]
]) test(`swimProgress: ${name}`, () => assert.deepEqual(swimProgress(track, dist, now), expected))

// the ceiling the pocket is dug in: the first block over the head that is not water, by its index in the column
for (const [name, names, expected] of [
  ['stone right over the head', ['stone', 'stone'], 0],
  ['one water then dirt', ['water', 'dirt', 'stone'], 1],
  ['open to the sky: no ceiling', ['water', 'water', 'air'], -1],
  ['kelp is water', ['kelp', 'water', 'stone'], 2],
  ['nothing read', [], -1]
]) test(`roofAt: ${name}`, () => assert.equal(roofAt(names), expected))
