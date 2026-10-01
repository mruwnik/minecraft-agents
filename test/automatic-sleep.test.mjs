import test from 'node:test'
import assert from 'node:assert/strict'
import { automaticBeds, bedChoice, nearHumanBase, carriedBedSpot, reflexPickups, automaticNightPlan, WALK_OVER_CARRIED } from '../src/lib/sleep.mjs'
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
  ['exactly 50 blocks from a foreign zone: blocked', { x: 50, y: 65, z: 2 }, [FAR_ZONE], [], 'other-base'],
  ['a zone of my own does not count, however close', { x: 102, y: 65, z: 2 }, [{ ...FAR_ZONE, name: 'observer-base' }], [], null],
  ['a human base place within margin', { x: 10, y: 65, z: 0 }, [], [{ name: 'joey-base', kind: 'base', by: 'Joey', x: 10, y: 65, z: 40 }], 'joey-base'],
  ['my own base place does not count', { x: 10, y: 65, z: 0 }, [], [{ name: 'observer-base', kind: 'base', by: 'Observer', x: 10, y: 65, z: 40 }], null],
  ["a base named for someone else counts, whoever marked it", { x: 10, y: 65, z: 0 }, [], [{ name: 'Joey-base', kind: 'base', by: 'Observer', x: 10, y: 65, z: 40 }], 'Joey-base']
]
for (const [title, point, zones, places, expected] of nearHumanBaseCases) {
  test(`nearHumanBase: ${title}`, () => assert.equal(nearHumanBase(point, { zones, places, me: 'Observer' }), expected))
}

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
  ['a villager within 16 of the foot: null', {}, [], [], [{ x: 0, y: 65, z: -1 }], null],
  ['the margin is measured from the bed: a head cell exactly 50 from a foreign zone skips that direction', {},
    [{ name: 'other-base', x1: -5, x2: 5, y1: 60, y2: 70, z1: -60, z2: -52 }], [], [], { x: 0, y: 65, z: 1, facing: 'south' }]
]
for (const [title, overrides, zones, places, residents, expected] of carriedBedSpotCases) {
  test(`carriedBedSpot: ${title}`, () =>
    assert.deepEqual(carriedBedSpot({ feet: FEET, cellAt: gridCellAt(overrides), zones, places, me: 'Observer', residents }), expected))
}

// ---------------------------------------------------------------- reflexPickups

const RED = { name: 'Observer-bed-0_65_0', kind: 'bed', by: 'Observer', reflex: true, item: 'red_bed', x: 0, y: 65, z: 0 }
const BLUE = { ...RED, name: 'Observer-bed-4_65_0', item: 'blue_bed', x: 4 }
const BED_CELLS = { '0,65,0': { name: 'red_bed' }, '4,65,0': { name: 'blue_bed' } }
const day = { night: false, asleep: false, reflexes: true, beds: [RED, BLUE], cellAt: (x, y, z) => BED_CELLS[`${x},${y},${z}`], from: { x: 0, y: 65, z: 0 }, inFlight: new Set() }
const reflexPickupsCases = [
  ['wake while still night: no dig job', { night: true }, []],
  ['first day tick: dig job per placed bed', {}, [{ bed: RED, do: 'dig' }, { bed: BLUE, do: 'dig' }]],
  ['asleep', { asleep: true }, []],
  ['reflexes off', { reflexes: false }, []],
  ['a bed already being dug is left to that job', { inFlight: new Set([RED.name]) }, [{ bed: BLUE, do: 'dig' }]],
  ['someone dug or replaced it: drop the mark', { cellAt: (x, y, z) => ({ name: x === 0 ? 'dirt' : 'blue_bed' }) }, [{ bed: RED, do: 'unmark' }, { bed: BLUE, do: 'dig' }]],
  ['an unloaded cell says nothing', { beds: [RED], cellAt: () => null }, []],
  ['17 blocks away: left standing for a later day tick near it', { beds: [RED], from: { x: 17, y: 65, z: 0 } }, []]
]
for (const [title, change, expected] of reflexPickupsCases) {
  test(`reflexPickups: ${title}`, () => assert.deepEqual(reflexPickups({ ...day, ...change }), expected))
}

// ---------------------------------------------------------------- automaticNightPlan

test('WALK_OVER_CARRIED is 64 blocks', () => assert.equal(WALK_OVER_CARRIED, 64))

const NP_FROM = { x: 0, y: 64, z: 0 }
const nightBed = x => ({ name: 'own-bed', x, y: 64, z: 0 })
const npBase = { near: false, from: NP_FROM, carried: false, walkFailed: false, hostileNear: false }
const WALK_FAIL_WHY = 'the walk to own-bed failed already tonight, and no bed is carried to place instead'
const HOSTILE_WHY = 'a monster is near: beds refuse, and no bed is carried to place instead'
const NO_BED_WHY = 'no bed of yours on the shared map: mark yours (mark name=<you>-bed kind=bed, standing on it) or pass bed=<place>'

const automaticNightPlanCases = [
  ['a bed within 32: sleep', { near: true }, { do: 'sleep' }],
  ['own bed at 50, no carry: walk', { bed: nightBed(50) }, { do: 'walk', to: nightBed(50), distance: 50 }],
  ['own bed at 150, no carry: walk', { bed: nightBed(150) }, { do: 'walk', to: nightBed(150), distance: 150 }],
  ['own bed at 150, carried: placement wins (over WALK_OVER_CARRIED)', { bed: nightBed(150), carried: true }, { do: 'place' }],
  ['own bed at 50, carried: still walk (under WALK_OVER_CARRIED)', { bed: nightBed(50), carried: true }, { do: 'walk', to: nightBed(50), distance: 50 }],
  ['own bed at 64, carried: walk (boundary)', { bed: nightBed(64), carried: true }, { do: 'walk', to: nightBed(64), distance: 64 }],
  ['own bed at 65, carried: place', { bed: nightBed(65), carried: true }, { do: 'place' }],
  ['own bed beyond 200, carried: place', { bed: nightBed(250), carried: true }, { do: 'place' }],
  ['own bed beyond 200, not carried: stop', { bed: nightBed(250) }, { do: 'stop', why: 'own-bed is 250 blocks away, beyond bed_range=200' }],
  ['no bed, carried: place', { bed: null, carried: true }, { do: 'place' }],
  ['no bed, not carried: stop', { bed: null }, { do: 'stop', why: NO_BED_WHY }],
  ['walk already failed tonight, carried: place', { bed: nightBed(50), carried: true, walkFailed: true }, { do: 'place' }],
  ['walk already failed tonight, no carry: stop', { bed: nightBed(50), walkFailed: true }, { do: 'stop', why: WALK_FAIL_WHY }],
  ['a monster near, bed at 50, carried: place', { bed: nightBed(50), carried: true, hostileNear: true }, { do: 'place' }],
  ['a monster near, no carry: stop', { bed: nightBed(50), hostileNear: true }, { do: 'stop', why: HOSTILE_WHY }]
]
for (const [title, change, expected] of automaticNightPlanCases) {
  test(`automaticNightPlan: ${title}`, () => assert.deepEqual(automaticNightPlan({ ...npBase, ...change }), expected))
}
