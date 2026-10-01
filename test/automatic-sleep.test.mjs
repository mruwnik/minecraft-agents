import test from 'node:test'
import assert from 'node:assert/strict'
import { automaticBeds, bedChoice, nearHumanBase, carriedBedSpot, HUMAN_BASE_MARGIN } from '../src/lib/sleep.mjs'
const bed={x:0,y:65,z:0},own={kind:'bed',by:'Observer',...bed}
test('automatic sleep requires positive ownership, not an unowned nearby bed',()=>{
 assert.deepEqual(automaticBeds([bed],[],[],'Observer'),[])
 assert.deepEqual(automaticBeds([bed],[],[own],'Observer'),[bed])
 assert.deepEqual(automaticBeds([bed],[],[{...own,by:'Other'}],'Observer'),[])
})
test('automatic sleep refuses occupied village beds even with an owned mark',()=>{
 assert.deepEqual(automaticBeds([bed],[],[own],'Observer',[{x:4,y:65,z:0}]),[])
 assert.deepEqual(automaticBeds([bed],[{name:'observer-village',x1:-2,x2:2,y1:60,y2:70,z1:-2,z2:2}],[own],'Observer'),[])
 assert.deepEqual(bedChoice([bed],[],'Observer'),{bed})
})

test('owned marks cannot override foreign protection; unmarked owned shelter remains usable',()=>{
 const zone={x1:-2,x2:2,y1:60,y2:70,z1:-2,z2:2}
 assert.deepEqual(automaticBeds([bed],[{...zone,name:'other-base'}],[own],'Observer'),[])
 assert.deepEqual(automaticBeds([bed],[{...zone,name:'observer-base'}],[],'Observer'),[bed])
})

// ---------------------------------------------------------------- nearHumanBase

const FAR_ZONE = { name: 'other-base', x1: 100, x2: 104, y1: 60, y2: 70, z1: 0, z2: 4 }
const nearHumanBaseCases = [
  ['inside a foreign zone: the zone name', { x: 102, y: 65, z: 2 }, [FAR_ZONE], [], 'other-base'],
  ['49 blocks from a foreign zone: still within margin', { x: 51, y: 65, z: 2 }, [FAR_ZONE], [], 'other-base'],
  ['51 blocks from a foreign zone: clear', { x: 49, y: 65, z: 2 }, [FAR_ZONE], [], null],
  ['a zone of my own does not count, however close', { x: 102, y: 65, z: 2 }, [{ ...FAR_ZONE, name: 'observer-base' }], [], null],
  ['a human base place within margin', { x: 10, y: 65, z: 0 }, [], [{ name: 'joey-base', kind: 'base', by: 'Joey', x: 10, y: 65, z: 40 }], 'joey-base'],
  ['my own base place does not count', { x: 10, y: 65, z: 0 }, [], [{ name: 'observer-base', kind: 'base', by: 'Observer', x: 10, y: 65, z: 40 }], null]
]
for (const [title, point, zones, places, expected] of nearHumanBaseCases) {
  test(`nearHumanBase: ${title}`, () => assert.equal(nearHumanBase(point, { zones, places, me: 'Observer' }), expected))
}

test('HUMAN_BASE_MARGIN is 50', () => assert.equal(HUMAN_BASE_MARGIN, 50))

// ---------------------------------------------------------------- carriedBedSpot

// A small grid: stone floor at y=64, air above, unless overridden. feet stands at y=65 on that floor.
const STONE = { name: 'stone', boundingBox: 'block' }
const AIR = { name: 'air', boundingBox: 'empty' }
const WATER = { name: 'water', boundingBox: 'empty' }
const gridCellAt = overrides => (x, y, z) => overrides[`${x},${y},${z}`] ?? (y === 64 ? STONE : AIR)

const FEET = { x: 0, y: 65, z: 0 }

const carriedBedSpotCases = [
  ['open ground: north is tried first', {}, [], [], [], { x: 0, y: 65, z: -1, facing: 'north' }],
  ['north blocked (stone in the head cell): falls to south', { '0,65,-2': STONE }, [], [], [], { x: 0, y: 65, z: 1, facing: 'south' }],
  ['foot over water: that direction is skipped', { '0,64,-1': WATER }, [], [], [], { x: 0, y: 65, z: 1, facing: 'south' }],
  ['low ceiling two above the floor: skipped', { '0,66,-1': STONE }, [], [], [], { x: 0, y: 65, z: 1, facing: 'south' }],
  ['enclosed on all sides: null', {
    '0,65,-2': STONE, '0,65,2': STONE, '2,65,0': STONE, '-2,65,0': STONE
  }, [], [], [], null],
  ['inside a foreign zone: null', {}, [{ name: 'other-base', x1: -5, x2: 5, y1: 60, y2: 70, z1: -5, z2: 5 }], [], [], null],
  ['own zone does not block it: a spot', {}, [{ name: 'observer-base', x1: -5, x2: 5, y1: 60, y2: 70, z1: -5, z2: 5 }], [], [], { x: 0, y: 65, z: -1, facing: 'north' }],
  ['a human-marked base place within 50: null', {}, [], [{ name: 'joey-base', kind: 'base', by: 'Joey', x: 0, y: 65, z: 10 }], [], null],
  ["the body's own base place does not block it: a spot", {}, [], [{ name: 'observer-base', kind: 'base', by: 'Observer', x: 0, y: 65, z: 10 }], [], { x: 0, y: 65, z: -1, facing: 'north' }],
  ['a villager within 16 of the foot: null', {}, [], [], [{ x: 0, y: 65, z: -1 }], null]
]
for (const [title, overrides, zones, places, residents, expected] of carriedBedSpotCases) {
  test(`carriedBedSpot: ${title}`, () =>
    assert.deepEqual(carriedBedSpot({ feet: FEET, cellAt: gridCellAt(overrides), zones, places, me: 'Observer', residents }), expected))
}
