import test from 'node:test'
import assert from 'node:assert/strict'
import { searchSections, enough, SECTION } from '../src/blocksearch.mjs'

// mariel-apiary, 2026-09-26 21:00Z: the body at -1.4,64,49.6 asked for campfires within 16 and got one of four. The
// three it missed sit at y=63 in the chunk section one over, one down and one across from the body's own, and the
// search mineflayer makes walks sections in an octahedron of apothem ceil((range + 8) / 16) = 2: a section three
// steps away in that metric is never visited, although its blocks are nine blocks off. The old rule, as the game
// client applies it, kept here only to show what it leaves out.
const octahedron = (point, maxDistance) => {
  const start = { x: Math.floor(point.x / SECTION), y: Math.floor(point.y / SECTION), z: Math.floor(point.z / SECTION) }
  const apothem = Math.ceil((maxDistance + 8) / SECTION)
  return s => Math.abs(s.x - start.x) + Math.abs(s.y - start.y) + Math.abs(s.z - start.z) <= apothem
}
const key = s => `${s.x},${s.y},${s.z}`
const has = (sections, s) => sections.some(t => key(t) === key(s))

test('the section holding three of the four apiary fires is nine blocks off, in the new search and not in the octahedron', () => {
  const body = { x: -1.4, y: 64, z: 49.6 }
  const fireSection = { x: 0, y: 3, z: 2 }
  const found = searchSections(body, 16)
  assert.deepEqual([has(found, fireSection), octahedron(body, 16)(fireSection), Math.hypot(4 - -2, 63 - 64, 43 - 50) <= 16], [true, false, true])
})

for (const [name, point, maxDistance, expected] of [
  ['the section under the point comes first, at no distance', { x: 10, y: 64, z: 10 }, 8, { first: { x: 0, y: 4, z: 0, near: 0 }, count: 6 }],
  ['a section whose nearest cell is out of range is not listed (16,64,16 is 8.5 from 10,64,10)', { x: 10, y: 64, z: 10 }, 8, { absent: { x: 1, y: 4, z: 1 }, count: 6 }],
  ['the same section one block nearer is in (16,64,16 is 7.8 from 10.5,64,11)', { x: 10.5, y: 64, z: 11 }, 8, { present: { x: 1, y: 4, z: 1 }, count: 8 }],
  ['negative coordinates floor towards minus infinity: -1 is in section -1', { x: -1, y: 64, z: -1 }, 1, { first: { x: -1, y: 4, z: -1, near: 0 }, count: 4 }],
  ['nothing below the world floor', { x: 0, y: -60, z: 0 }, 16, { absent: { x: 0, y: -5, z: 0 }, present: { x: 0, y: -4, z: 0 } }],
  ['nothing above the world ceiling', { x: 0, y: 315, z: 0 }, 16, { absent: { x: 0, y: 20, z: 0 }, present: { x: 0, y: 19, z: 0 } }],
  ['range 0 is the one section', { x: 5, y: 70, z: 5 }, 0, { count: 1, first: { x: 0, y: 4, z: 0, near: 0 } }]
]) {
  test(`searchSections: ${name}`, () => {
    const found = searchSections(point, maxDistance)
    const seen = {
      ...(expected.first ? { first: found[0] } : {}),
      ...(expected.count !== undefined ? { count: found.length } : {}),
      ...(expected.absent ? { absent: has(found, expected.absent) ? 'listed' : expected.absent } : {}),
      ...(expected.present ? { present: has(found, expected.present) ? expected.present : 'missing' } : {})
    }
    assert.deepEqual(seen, expected)
  })
}

test('searchSections: nearest first, and every section the sphere touches for range 16 (the octahedron lists fewer)', () => {
  const point = { x: 8, y: 72, z: 8 }
  const found = searchSections(point, 16)
  const sorted = found.every((s, i) => i === 0 || s.near >= found[i - 1].near)
  assert.deepEqual([sorted, found.length, found.filter(octahedron(point, 16)).length], [true, 27, 19])
})

test('searchSections: a smaller world height clips the top', () => {
  assert.equal(has(searchSections({ x: 0, y: 250, z: 0 }, 16, { minY: 0, height: 256 }), { x: 0, y: 16, z: 0 }), false)
})

// the search stops once `count` blocks are in hand and no later section can hold a nearer one
for (const [name, found, count, near, expected] of [
  ['fewer than asked: keep going', [1, 2], 3, 0, false],
  ['enough and the next section starts beyond the farthest kept: stop', [1, 5, 3], 3, 5.5, true],
  ['enough but the next section could hold a nearer one: keep going', [1, 5, 3], 3, 4, false],
  ['more than asked: only the count-th nearest decides', [9, 1, 5, 3], 3, 6, true]
]) {
  test(`enough: ${name}`, () => assert.equal(enough(found, count, near), expected))
}
