// The goal a walk aims at to reach a dropped item (card 21657b89): a farmland cell is 15/16 high, a slab half that,
// so an item resting on either floors to the SURFACE block, not the air cell above it, and a walk sent there with
// range=0 finds nothing to stand in and is refused. Pure geometry: floor the position, then widen the range so the
// walk can land on the cell above (or beside) a short surface without needing to know what the block is.
import test from 'node:test'
import assert from 'node:assert/strict'
import { dropGoal } from '../src/drop.mjs'

const at = (x, y, z) => ({ x, y, z })
for (const [name, entityPos, expected] of [
  ['on farmland: floors to the farmland cell itself, range widened so a walk can stand above it',
    at(14.5, 62.9375, -71.3), { x: 14, y: 62, z: -72, range: 1 }],
  ['on a bottom slab: floors to the slab cell, range widened the same way',
    at(5.7, 70.5001, 3.2), { x: 5, y: 70, z: 3, range: 1 }],
  ['in water: already floors to the water cell itself, which is standable, but still gets range=1',
    at(8.1, 65.31, -2.9), { x: 8, y: 65, z: -3, range: 1 }],
  ['on a bare crop cell (harvested wheat lying on farmland)',
    at(20.6, 63.9375, 10.1), { x: 20, y: 63, z: 10, range: 1 }],
  ['on an ordinary full block: already floors to the air cell above it',
    at(0.5, 65.01, 0.5), { x: 0, y: 65, z: 0, range: 1 }],
  ['negative coordinates floor toward negative infinity, not toward zero',
    at(-0.3, 61.02, -12.9), { x: -1, y: 61, z: -13, range: 1 }]
]) test(`dropGoal: ${name}`, () => assert.deepEqual(dropGoal(entityPos), expected))
