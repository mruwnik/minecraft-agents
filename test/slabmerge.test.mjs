// Placing a slab against a cell that already holds a BOTTOM slab merges the two into a double slab: a full block
// with no gap for water underneath (jizo-melon-patch, 09-26: -1,62,-84 became oak_slab type=double this way,
// sealing the channel for good; dug out by hand, the next farm.build pass poured and covered it normally). A slab
// merges with the opposite half of an existing one regardless of which face it was clicked against, so the guard
// has to run before `place` ever tries, not rely on which neighbour it picked.
import test from 'node:test'
import assert from 'node:assert/strict'
import { slabMergeRefusal } from '../src/slabmerge.mjs'

const at = { x: -1, y: 62, z: -84 }
const bottomSlab = (name = 'oak_slab', waterlogged = false) => ({ name, properties: { type: 'bottom', ...(waterlogged ? { waterlogged: 'true' } : {}) } })
const topSlab = (name = 'oak_slab', waterlogged = false) => ({ name, properties: { type: 'top', ...(waterlogged ? { waterlogged: 'true' } : {}) } })
const doubleSlab = (name = 'oak_slab') => ({ name, properties: { type: 'double' } })

for (const [name, existing, item, expected] of [
  ['a dry bottom slab refuses another slab', bottomSlab(), 'oak_slab', '-1,62,-84 holds a bottom slab: dig it first, placing another merges them into a full block'],
  ['a waterlogged bottom slab refuses too: wet or dry, the merge is the same', bottomSlab('oak_slab', true), 'oak_slab', '-1,62,-84 holds a bottom slab: dig it first, placing another merges them into a full block'],
  ['a different wood still refuses: safety first, vanilla or not', bottomSlab('birch_slab'), 'oak_slab', '-1,62,-84 holds a bottom slab: dig it first, placing another merges them into a full block'],
  ['a top slab is already finished: nothing merges onto it', topSlab(), 'oak_slab', null],
  ['a waterlogged top slab likewise', topSlab('oak_slab', true), 'oak_slab', null],
  ['an already-merged double slab is not a fresh merge risk', doubleSlab(), 'oak_slab', null],
  ['a non-slab item onto a bottom slab is somebody else\'s problem (occupiedBy blocks it)', bottomSlab(), 'cobblestone', null],
  ['a bottom slab is not itself a risk without something to place', bottomSlab(), undefined, null],
  ['no existing block: nothing to merge with', null, 'oak_slab', null],
  ['a plain block in the way is not a slab-merge risk', { name: 'stone', properties: {} }, 'oak_slab', null]
]) {
  test(`slabMergeRefusal: ${name}`, () => assert.equal(slabMergeRefusal(at, existing, item), expected))
}
