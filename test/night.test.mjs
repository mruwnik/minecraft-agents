// Night reflexes (card 0f110bb5). A body restarted at night inside a hut (15:14Z) chased a spider out through the door 18 s later,
// charged a skeleton that shot it, fled a zombie 52 blocks east into dark hills, and died at health 2 to a spider while both runs
// still said "from: zombie". Three rules, all pure: under a roof at night nothing is chased or charged; a night run is bounded
// and heads for lit, known ground; the threat is whatever last hurt me, and what cannot be outrun is fought
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { underRoof, walledIn, nightShelter, NIGHT_FLEE_MAX, nightFleeStep, nightFleeGoal, retarget, fightNotFlee, attackerCount, plugCells, holdNote } from '../src/survival/night.mjs'

for (const [name, above, expected] of [
  ['open sky', ['air', 'air', 'air', 'air', 'air', 'air', 'air', 'air'], false],
  ['a plank roof two over the head', ['air', 'oak_planks', 'air', 'air', 'air', 'air', 'air', 'air'], true],
  ['a low dirt roof', ['dirt', 'air', 'air', 'air', 'air', 'air', 'air', 'air'], true],
  ['leaves count: a tree hut', ['air', 'air', 'oak_leaves', 'air', 'air', 'air', 'air', 'air'], true],
  ['water over the head is not a roof', ['water', 'water', 'air', 'air', 'air', 'air', 'air', 'air'], false],
  ['a cave ceiling far up counts too', ['cave_air', 'cave_air', 'cave_air', 'cave_air', 'cave_air', 'cave_air', 'cave_air', 'stone'], true],
  ['nothing read', [], false]
]) {
  test(`underRoof: ${name}`, () => assert.equal(underRoof(above), expected))
}

const walls = (open) => Array.from({ length: 8 }, (_, i) => i < open ? 'air' : 'cobblestone')
for (const [name, sides, expected] of [
  ['a one-by-one cell: solid all round', walls(0), true],
  ['a doorway open at feet and head on one side', ['air', 'air', 'oak_planks', 'oak_planks', 'oak_planks', 'oak_planks', 'oak_planks', 'oak_planks'], true],
  ['a door block is a wall', ['oak_door', 'oak_door', 'oak_planks', 'oak_planks', 'oak_planks', 'oak_planks', 'oak_planks', 'oak_planks'], true],
  ['two sides open: not a room', walls(4), false],
  ['open ground', walls(8), false],
  ['grass and leaf litter beside the feet are not walls', ['short_grass', 'air', 'leaf_litter', 'air', 'air', 'air', 'air', 'air'], false]
]) {
  test(`walledIn: ${name}`, () => assert.equal(walledIn(sides), expected))
}

for (const [name, args, expected] of [
  ['night under a roof', { night: true, roofed: true }, true],
  ['night in a walled room with no roof', { night: true, walled: true }, true],
  ['night in the open', { night: true }, false],
  ['day under a roof: the reflexes are as they were', { roofed: true, walled: true }, false]
]) {
  test(`nightShelter: ${name}`, () => assert.equal(nightShelter(args), expected))
}

// the run itself: fleeStep's rules, plus a bound at night. 52 blocks into dark hills is where the zombie came back
test('NIGHT_FLEE_MAX: about twenty blocks', () => assert.equal(NIGHT_FLEE_MAX, 20))
for (const [name, args, expected] of [
  ['by day the run is fleeStep\'s own', { phase: 'away', threatDist: 3, homeDist: 40 }, { phase: 'away' }],
  ['at night, twenty out from home with the threat behind: the run stops there', { night: true, phase: 'away', threatDist: 3, homeDist: 20 }, { phase: null, event: 'flee_stuck' }],
  ['at night, nineteen out: still running', { night: true, phase: 'away', threatDist: 3, homeDist: 19 }, { phase: 'away' }],
  ['at night, clear of the threat: home as usual', { night: true, phase: 'away', threatDist: 30, homeDist: 25 }, { phase: 'back', event: 'flee_clear' }],
  ['the walk home is not bounded', { night: true, phase: 'back', homeDist: 25 }, { phase: 'back' }]
]) {
  test(`nightFleeStep: ${name}`, () => { const s = nightFleeStep(args); assert.deepEqual({ phase: s.phase, ...(s.event ? { event: s.event } : {}) }, expected) })
}
test('nightFleeStep: the bound says why and what to do', () => assert.match(nightFleeStep({ night: true, phase: 'away', threatDist: 3, homeDist: 20 }).note, /20 blocks from home.*dark|night/i))

// where a night run heads: lit, known ground away from the mob when there is any within the bound, else straight away but never
// past the bound from home
const me = { x: 0, y: 64, z: 0 }
const home = { x: 0, y: 64, z: 0 }
for (const [name, args, expected] of [
  ['by day: straight away from the mob', { me, home, mob: { x: -5, y: 64, z: 0 }, dist: 20 }, { x: 20, z: 0 }],
  ['night, a bed 10 east, the mob west: the bed', { night: true, me, home, mob: { x: -5, y: 64, z: 0 }, dist: 20, refuges: [{ x: 10, y: 64, z: 0, kind: 'bed' }] }, { x: 10, z: 0, kind: 'bed' }],
  ['night, the only torch stands past the mob: not through it', { night: true, me, home, mob: { x: -5, y: 64, z: 0 }, dist: 20, refuges: [{ x: -12, y: 64, z: 0, kind: 'torch' }] }, { x: 20, z: 0 }],
  ['night, a refuge beyond the bound is no refuge', { night: true, me, home, mob: { x: -5, y: 64, z: 0 }, dist: 20, refuges: [{ x: 30, y: 64, z: 0, kind: 'bed' }] }, { x: 20, z: 0 }],
  ['night, a torch two blocks off is no run: the goal must move the body', { night: true, me, home, mob: { x: -5, y: 64, z: 0 }, dist: 20, refuges: [{ x: 2, y: 64, z: 0, kind: 'torch' }] }, { x: 20, z: 0 }],
  ['night, the nearest refuge wins', { night: true, me, home, mob: { x: -5, y: 64, z: 0 }, dist: 20, refuges: [{ x: 15, y: 64, z: 0, kind: 'torch' }, { x: 0, y: 64, z: 8, kind: 'bed' }] }, { x: 0, z: 8, kind: 'bed' }],
  ['night, no refuge, already 15 out: the straight goal is cut at the bound', { night: true, me: { x: 15, y: 64, z: 0 }, home, mob: { x: 10, y: 64, z: 0 }, dist: 20 }, { x: 20, z: 0 }],
  ['night, sideways from home: cut along the line from home', { night: true, me: { x: 0, y: 64, z: 16 }, home, mob: { x: 0, y: 64, z: 10 }, dist: 20 }, { x: 0, z: 20 }]
]) {
  test(`nightFleeGoal: ${name}`, () => assert.deepEqual(nightFleeGoal(args), expected))
}

// the threat is whatever last hurt me: both runs said "from: zombie" while a spider took the body from 14 to 2
for (const [name, args, expected] of [
  ['hit by a spider while running from a zombie: the spider', { fleeing: 'zombie', hurtBy: ['spider'], hurtMsAgo: 1000 }, 'spider'],
  ['hit by the zombie I run from: no change', { fleeing: 'zombie', hurtBy: ['zombie'], hurtMsAgo: 1000 }, null],
  ['hit by something unseen: nothing to retarget to', { fleeing: 'zombie', hurtBy: [], hurtMsAgo: 1000 }, null],
  ['an old hit does not count', { fleeing: 'zombie', hurtBy: ['spider'], hurtMsAgo: 4000 }, null],
  ['not running: the last hit still names the threat', { fleeing: null, hurtBy: ['skeleton'], hurtMsAgo: 500 }, 'skeleton']
]) {
  test(`retarget: ${name}`, () => assert.equal(retarget(args), expected))
}

// what an armed body cannot outrun it fights: a spider climbs and leaps, and the run from it is what killed the body at health 2
for (const [name, args, expected] of [
  ['armed, a spider', { armed: true, mob: 'spider' }, true],
  ['armed, a cave spider', { armed: true, mob: 'cave_spider' }, true],
  ['armed, a phantom', { armed: true, mob: 'phantom' }, true],
  ['armed, a zombie: it can be outrun', { armed: true, mob: 'zombie' }, false],
  ['unarmed, a spider: nothing to fight with', { armed: false, mob: 'spider' }, false],
  ['a creeper is never fought', { armed: true, mob: 'creeper' }, false]
]) {
  test(`fightNotFlee: ${name}`, () => assert.equal(fightNotFlee(args), expected))
}

// shouldFlee's attacker count: the mob that hit me counts even when it stands out of the crowd's radius after the knockback
for (const [name, args, expected] of [
  ['two in the crowd, hit by one of them', { crowd: 2, seen: ['zombie', 'spider'], hurtBy: ['spider'], hurtMsAgo: 1000 }, 2],
  ['one in the crowd, hit by something not in it', { crowd: 1, seen: ['zombie'], hurtBy: ['skeleton'], hurtMsAgo: 1000 }, 2],
  ['nobody near, but hit a second ago', { crowd: 0, seen: [], hurtBy: ['spider'], hurtMsAgo: 1000 }, 1],
  ['nobody near, hit long ago', { crowd: 0, seen: [], hurtBy: ['spider'], hurtMsAgo: 9000 }, 0],
  ['nobody near, never hit', { crowd: 0, seen: [], hurtBy: [], hurtMsAgo: Infinity }, 0]
]) {
  test(`attackerCount: ${name}`, () => assert.equal(attackerCount(args), expected))
}

// an archer through a gap: the cells between me and it, head height first (the arrow comes at the head), then the feet
for (const [name, mob, expected] of [
  ['an archer to the east', { x: 8, y: 64, z: 1 }, [{ dx: 1, dy: 1, dz: 0 }, { dx: 1, dy: 0, dz: 0 }]],
  ['an archer to the north', { x: 1, y: 64, z: -9 }, [{ dx: 0, dy: 1, dz: -1 }, { dx: 0, dy: 0, dz: -1 }]],
  ['an archer above and west', { x: -6, y: 70, z: 0 }, [{ dx: -1, dy: 1, dz: 0 }, { dx: -1, dy: 0, dz: 0 }]]
]) {
  test(`plugCells: ${name}`, () => assert.deepEqual(plugCells(me, mob), expected))
}

for (const [name, args, pattern] of [
  ['holding against an archer, gap plugged', { mob: 'skeleton', plugged: true }, /skeleton.*not charging.*plugged/i],
  ['holding against an archer, nothing to plug with', { mob: 'skeleton', plugged: false }, /skeleton.*not charging.*(nothing to plug|step out of its line)/i],
  ['a zombie in reach: swing from inside', { mob: 'zombie', melee: true }, /zombie in reach.*from inside/i],
  ['a spider outside the door is held, not chased, and not called an archer', { mob: 'spider', door: true }, /^a spider outside at night: not chasing it out of the door(?!.*shooting)/i]
]) {
  test(`holdNote: ${name}`, () => assert.match(holdNote(args), pattern))
}
