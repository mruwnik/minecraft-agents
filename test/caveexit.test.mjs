// "No path to the goal!" in one second from a cave pool says nothing a driver can act on: the body drowned there
// (card a164bbfd) after three of them, and a goto dig=true from a 1-wide shaft answered the same (card 5e16aff9).
// The search fails fast in these cases for reasons the body can read off itself: no sky over the head with the goal up
// on the surface; water touching the cell (a dig walk never breaks a block beside water, dontCreateFlow); a protected
// zone or built blocks round it (exclusionAreasBreak 100); or a 1-wide shaft. Pure: bot.mjs hands the evidence in.
import test from 'node:test'
import assert from 'node:assert/strict'
import { noPathAdvice } from '../src/caveexit.mjs'

const NO_PATH = 'no walkable path (walks don\'t dig or bridge): look for a way round, go in shorter legs, or pass dig=true if breaking and placing blocks on the way is fine'
const RAW = 'No path to the goal!'
const dry = { underground: false, goalDy: 0, wet: false, zoned: false, boxed: false }

for (const [name, given, expected] of [
  ['not a no-path failure: untouched', { text: 'a creeper hit me', dig: false, ...dry, underground: true, goalDy: 30 }, 'a creeper hit me'],
  ['no path on the surface with the goal level: the plain text stands', { text: NO_PATH, dig: false, ...dry }, NO_PATH],
  ['a cave with the goal 30 up: say so, and what climbs',
    { text: NO_PATH, dig: false, ...dry, underground: true, goalDy: 30 },
    'no path: you are underground (no sky over your head) and the goal is 30 blocks up; a walk neither digs nor climbs. goto the same spot with dig=true (it digs a staircase and climbs in legs of 6), or pillar up: place a block at your feet, again and again'],
  ['a cave pool with the goal above: the water comes first, since a dig walk never breaks a block beside water',
    { text: NO_PATH, dig: false, ...dry, underground: true, goalDy: 30, wet: true },
    'no path: you are underground (no sky over your head) and the goal is 30 blocks up; a walk neither digs nor climbs. You are in water: swim to the pool\'s edge and walk two blocks clear of it first (a dig walk breaks nothing that touches water), then goto the same spot with dig=true (it digs a staircase and climbs in legs of 6), or pillar up: place a block at your feet, again and again'],
  ['underground with the goal only two up: not the surface story',
    { text: NO_PATH, dig: false, ...dry, underground: true, goalDy: 2 }, NO_PATH],
  ['underground with no goal height known: the plain text stands',
    { text: NO_PATH, dig: false, ...dry, underground: true, goalDy: null }, NO_PATH],
  ['dig=true from the water: nothing beside water is ever dug',
    { text: RAW, dig: true, ...dry, wet: true, underground: true, goalDy: 30 },
    'no path even with dig=true: a dig walk breaks nothing that touches water (the way would flood), so from a pool nothing can be dug. Swim to the pool\'s edge, walk two blocks clear of the water, then goto dig=true again; or pillar up: place a block at your feet, again and again'],
  ['dig=true inside a protected zone: the walk digs nothing there',
    { text: RAW, dig: true, ...dry, zoned: true },
    'no path even with dig=true: a dig walk breaks nothing inside a protected zone or that looks built (cobblestone, planks, fences...). Dig by hand: dig x= y= z= the wall at head height, step up, again (a staircase), or pillar up: place a block at your feet'],
  ['dig=true from a 1-wide shaft: pillar up or dig steps by hand',
    { text: RAW, dig: true, ...dry, boxed: true },
    'no path even with dig=true: you stand in a 1-wide shaft. Pillar up: place a block at your feet, again and again (dirt or cobblestone in the pocket also lets the walk tower by itself), or dig a staircase by hand: dig the wall at head height, step up, again'],
  ['dig=true in the open with a plain no path: the raw text stands', { text: RAW, dig: true, ...dry }, RAW],
  ['the 5 s timeout with dig=true from the water is the same story',
    { text: 'Took to long to decide path to goal!', dig: true, ...dry, wet: true },
    'no path even with dig=true: a dig walk breaks nothing that touches water (the way would flood), so from a pool nothing can be dug. Swim to the pool\'s edge, walk two blocks clear of the water, then goto dig=true again; or pillar up: place a block at your feet, again and again']
]) test(`noPathAdvice: ${name}`, () => assert.equal(noPathAdvice(given), expected))
