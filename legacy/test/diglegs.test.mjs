// A dig walk of 10+ blocks through rock runs out of its 5 s search ("Took to long to decide path to goal!", ~8000 nodes)
// or circles for 40 s, while legs of 5-8 blocks arrive in 10-30 s each (cards 5e16aff9 and a164bbfd, a whole afternoon
// of tunnelling). goto dig=true cuts the straight line to its goal into legs of DIG_LEG by itself. Pure geometry.
import test from 'node:test'
import assert from 'node:assert/strict'
import { digLegs, DIG_LEG } from '../src/navigation/dig-legs.mjs'

test('DIG_LEG is six blocks', () => assert.equal(DIG_LEG, 6))

const at = (x, y, z) => ({ x, y, z })
for (const [name, from, to, expected] of [
  ['within one leg: the goal itself', at(0.5, 64, 0.5), at(4, 64, 3), [at(4, 64, 3)]],
  ['exactly one leg long: still one leg', at(0.5, 64, 0.5), at(6, 64, 0), [at(6, 64, 0)]],
  ['twenty blocks straight up: legs of five, the last one the goal', at(-120.5, 40, -140.5), at(-120, 60, -140),
    [at(-120, 45, -140), at(-120, 50, -140), at(-120, 55, -140), at(-120, 60, -140)]],
  ['thirteen blocks across: three even legs, not one of six and one of seven', at(0.5, 64, 0.5), at(13, 64, 0), [at(5, 64, 0), at(9, 64, 0), at(13, 64, 0)]],
  ['a slope: every leg on the line, the nearest cell', at(0.5, 64, 0.5), at(10, 70, -10),
    [at(4, 66, -3), at(7, 68, -6), at(10, 70, -10)]],
  ['a shaft from its bottom, a hair over one leg: halfway up, then the rim', at(5.5, 58, 5.5), at(5, 64, 6), [at(5, 61, 6), at(5, 64, 6)]],
  ['far away: never more than six blocks a leg', at(0.5, 64, 0.5), at(30, 64, 0),
    [at(6, 64, 0), at(12, 64, 0), at(18, 64, 0), at(24, 64, 0), at(30, 64, 0)]]
]) test(`digLegs: ${name}`, () => assert.deepEqual(digLegs(from, to), expected))
