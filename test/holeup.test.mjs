import { test } from 'node:test'
import assert from 'node:assert/strict'
import { holeUpRefusal, HOLE_HURT_MS } from '../src/lib.mjs'

// my body, 17:26Z at -113,28,-125: a zombie hit it four times in three seconds, the flee ran into the dark and stopped
// (flee_stuck), and the hole-up chose to dig because the zombie stood 4-8 blocks off at that instant. It was back on me
// 1.5 s later, and the body died at 0 with a stone sword in its pack, digging. A hit within the last seconds IS a hostile
// in reach, wherever the mob stands right now: an armed body fights instead
for (const [name, args, expected] of [
  ['armed, hit 1.8 s ago, the zombie 6 off after my run: fight, it is on me', { armed: true, hostileDist: 6, hurtMsAgo: 1800 }, 'fight'],
  ['armed, hit just now by something I cannot see: fight, do not dig', { armed: true, hostileDist: Infinity, hurtMsAgo: 300 }, 'fight'],
  ['armed, hit at the edge of the window', { armed: true, hostileDist: 6, hurtMsAgo: HOLE_HURT_MS }, 'fight'],
  ['armed, the last hit is old and the mob 6 off: dig', { armed: true, hostileDist: 6, hurtMsAgo: HOLE_HURT_MS + 1 }, null],
  ['armed, never hit, the mob within 4: fight (as before)', { armed: true, hostileDist: 3 }, 'fight'],
  ['armed, never hit, nothing near: dig', { armed: true, hostileDist: 9 }, null],
  ['unarmed and just hit: the ground is still all there is', { armed: false, hostileDist: 2, hurtMsAgo: 500 }, null]
]) {
  test(`holeUpRefusal: ${name}`, () => assert.equal(holeUpRefusal(args), expected))
}

// ---------------------------------------------------------------- the site probe (card c3f387d7)
// Jizo's body (15:02Z) holed up beside a farm channel: the three cells straight below were dirt, so the old check said dig, and the
// channel's water one cell to the side poured into the shaft as it went down. It surfaced for air and could not climb out. The floor
// is not just the column: what stands beside the shaft flows into it. A bad site is not a reason to wall in on the spot either,
// when a dry cell stands one step away, or three blocks in the pack make a pillar a creeper cannot reach
import { capChoice, floorVerdict, burrowSite, holeUpAborted, mobHit, holeUpBlock, refusalNote, shelterNote, HOLE_STEP } from '../src/survival/holeup.mjs'

const dry = { below: ['dirt', 'dirt', 'stone'], beside: [['dirt', 'dirt', 'dirt', 'dirt'], ['dirt', 'dirt', 'dirt', 'dirt'], ['stone', 'stone', 'stone', 'stone']] }
const channel = { below: ['dirt', 'dirt', 'stone'], beside: [['dirt', 'water', 'dirt', 'dirt'], ['dirt', 'dirt', 'dirt', 'dirt'], ['stone', 'stone', 'stone', 'stone']] }
const pond = { below: ['water', 'sand', 'stone'], beside: dry.beside }
const cave = { below: ['dirt', 'dirt', 'stone'], beside: [['dirt', 'dirt', 'dirt', 'dirt'], ['dirt', 'cave_air', 'dirt', 'dirt'], ['stone', 'stone', 'stone', 'stone']] }
const ledge = { below: ['dirt', 'dirt', 'stone'], beside: [['air', 'air', 'dirt', 'dirt'], ['dirt', 'dirt', 'dirt', 'dirt'], ['stone', 'stone', 'stone', 'stone']] }

for (const [name, args, expected] of [
  ['solid all round: a good floor', { ...dry, cap: true }, null],
  ['water straight below', { ...pond, cap: true }, /water under my feet/],
  ['lava two down', { below: ['stone', 'lava', 'stone'], beside: dry.beside, cap: true }, /lava under my feet/],
  ['the cells below are not loaded', { below: [null, null, null], beside: dry.beside, cap: true }, /could not read/],
  ['a channel one cell to the side, level with the first dig (Jizo, 15:02Z)', { ...channel, cap: true }, /water beside the shaft/],
  ['lava beside the third cell', { below: dry.below, beside: [dry.beside[0], dry.beside[1], ['stone', 'stone', 'lava', 'stone']], cap: true }, /lava beside the shaft/],
  ['a cave beside the second cell, with blocks to wall it: fine, the ring closes it', { ...cave, cap: true }, null],
  ['the same cave with nothing to wall it: the shaft would stand open to it', { ...cave, cap: false }, /open to a cave/],
  ['air beside the top cell is the ground stepping down, not a cave', { ...ledge, cap: false }, null],
  ['no sides read at all: judged on the column alone', { below: dry.below, cap: true }, null]
]) {
  test(`floorVerdict: ${name}`, () => { const v = floorVerdict(args); return expected ? assert.match(v, expected) : assert.equal(v, null) })
}

const site = (dx, dz, cells, standable = true) => ({ dx, dz, standable, ...cells })
for (const [name, args, expected] of [
  ['a good floor here: dig where I stand', { here: dry, around: [], blocks: 5 }, { way: 'dig', step: null }],
  ['water under me, dry ground one step east: step there and dig', { here: pond, around: [site(1, 0, dry)], blocks: 5 }, { way: 'dig', step: { dx: 1, dz: 0 } }],
  ['the nearer dry cell wins', { here: pond, around: [site(2, 0, dry), site(0, -1, dry)], blocks: 5 }, { way: 'dig', step: { dx: 0, dz: -1 } }],
  ['a dry cell I cannot stand in (a wall) is no site', { here: channel, around: [site(1, 0, dry, false)], blocks: 5 }, { way: 'pillar', step: null }],
  ['a channel beside every cell, three blocks in the pack: pillar up', { here: channel, around: [site(1, 0, channel), site(-1, 0, pond)], blocks: 3 }, { way: 'pillar', step: null }],
  ['two blocks are not a pillar: wall in where I stand', { here: channel, around: [site(1, 0, channel)], blocks: 2 }, { way: 'wall', step: null }],
  ['nothing readable, nothing carried: wall in (open, and said so)', { here: { below: [null, null, null] }, around: [], blocks: 0 }, { way: 'wall', step: null }],
  ['water under the feet and a full pack: no pillar, nothing to build it on (18:22Z)', { here: pond, around: [], blocks: 64 }, { way: 'wall', step: null }],
  ['water two down with a lid to stand on: the pillar goes up from the lid', { here: { below: ['dirt', 'water', 'dirt'], beside: dry.beside }, around: [], blocks: 3 }, { way: 'pillar', step: null }],
  ['a good floor is preferred to a pillar even with a full pack', { here: dry, around: [site(1, 0, pond)], blocks: 64 }, { way: 'dig', step: null }]
]) {
  test(`burrowSite: ${name}`, () => { const s = burrowSite(args); assert.deepEqual({ way: s.way, step: s.step }, expected) })
}
test('burrowSite: a step says what was under my feet and where it goes', () => assert.match(burrowSite({ here: pond, around: [site(1, 0, dry)], blocks: 1 }).why, /water under my feet.*one step (east|west|north|south)/))
test('burrowSite: a pillar says why the ground would not do', () => assert.match(burrowSite({ here: channel, around: [], blocks: 3 }).why, /water beside the shaft.*pillar/))
test('shelterNote: an open shaft says what stayed open, not that nothing was carried', () => assert.match(shelterNote({ way: 'dig', open: true, surface }), /OPEN: a side or the top could not be closed/))
test('burrowSite: the wall fallback says what it read', () => assert.match(burrowSite({ here: pond, around: [], blocks: 0 }).why, /water under my feet.*walling/))
test('burrowSite: a pillar needs exactly the hole depth in blocks', () => assert.deepEqual([2, 3].map(blocks => burrowSite({ here: channel, around: [], blocks }).way), ['wall', 'pillar']))
test('HOLE_STEP: sites are read two cells out', () => assert.equal(HOLE_STEP, 2))

// the cap that keeps nothing out: my body (17:27Z) capped its hole with leaf_litter, the first block-shaped item in its pack, and a
// sand cap would fall on its head. Only a full solid block is a cap; ground cover, gravity blocks, doors, gates and beds are not
for (const [name, items, expected] of [
  ['cobblestone', [{ name: 'cobblestone', boundingBox: 'block' }], 'cobblestone'],
  ['leaf litter is not a cap, the dirt behind it is', [{ name: 'leaf_litter', boundingBox: 'empty' }, { name: 'dirt', boundingBox: 'block' }], 'dirt'],
  ['short grass has no box either', [{ name: 'short_grass', boundingBox: 'empty' }], null],
  ['sand and gravel fall', [{ name: 'sand', boundingBox: 'block' }, { name: 'gravel', boundingBox: 'block' }], null],
  ['a gate, a door, a bed, a torch, a sapling', [{ name: 'oak_fence_gate', boundingBox: 'block' }, { name: 'oak_door', boundingBox: 'block' }, { name: 'red_bed', boundingBox: 'block' }, { name: 'torch', boundingBox: 'empty' }, { name: 'oak_sapling', boundingBox: 'empty' }], null],
  ['an item that is no block at all', [{ name: 'bread' }], null],
  ['leaves are solid and stop a zombie', [{ name: 'oak_leaves', boundingBox: 'block' }], 'oak_leaves'],
  ['nothing carried', [], null]
]) {
  test(`capChoice: ${name}`, () => assert.equal(capChoice(items), expected))
}

// my body (17:26:53Z) began a hole-up, died in it at :58, respawned into its bed at 17:27:00 and the same hole-up went on to report a
// hole at 17:27:04 from the bed. A hole-up whose body has died or gone to sleep since it began stops at the next check
for (const [name, args, expected] of [
  ['the same life, awake: go on', { life: 3, lives: 3, asleep: false }, null],
  ['died since it began', { life: 3, lives: 4, asleep: false }, /died/],
  ['asleep since it began (the respawn put it in a bed)', { life: 3, lives: 3, asleep: true }, /asleep|in bed/],
  ['both: the death is the news', { life: 3, lives: 4, asleep: true }, /died/]
]) {
  test(`holeUpAborted: ${name}`, () => { const v = holeUpAborted(args); return expected ? assert.match(v, expected) : assert.equal(v, null) })
}

// 17:46Z: drowning at health 5 with nothing to eat, the verdict said hole up and the refusal said "a hostile Infinity blocks off, and it
// hit me 0.1 s ago: I fight instead". The water hit it. Only a hit that a mob could have given counts as a mob in reach
for (const [name, cause, expected] of [
  ['no cause named: a mob I saw', null, true],
  ['no cause and nothing seen: something unseen hit me, still a mob', undefined, true],
  ['drowning', 'drowning: get to air', false],
  ['a fall', 'a fall of 5 blocks', false],
  ['starving', 'starving: eat', false],
  ['a creeper that has already gone off', 'a creeper blew up (it is gone now)', false],
  ['hit during a run from a zombie', 'hit while the body fled from a zombie by itself: that run is why you have moved', true]
]) {
  test(`mobHit: ${name}`, () => assert.equal(mobHit(cause), expected))
}

// the refusal for a creeper within reach left the body standing (creepers are never fought, and the run had just given up):
// it steps away instead. Everything else is holeUpRefusal's rule
for (const [name, args, expected] of [
  ['armed, a creeper 3 off: step away, do not dig, do not stand', { armed: true, hostile: 'creeper', hostileDist: 3 }, 'step'],
  ['unarmed, a creeper 3 off: the same', { armed: false, hostile: 'creeper', hostileDist: 3 }, 'step'],
  ['a creeper 6 off: far enough to dig', { armed: true, hostile: 'creeper', hostileDist: 6 }, null],
  ['armed, a zombie 3 off: fight', { armed: true, hostile: 'zombie', hostileDist: 3 }, 'fight'],
  ['armed, hit 1 s ago by something unseen: fight', { armed: true, hostileDist: Infinity, hurtMsAgo: 1000 }, 'fight'],
  ['unarmed, a zombie 3 off: the ground is all there is', { armed: false, hostile: 'zombie', hostileDist: 3 }, null],
  ['nothing near: dig', { armed: true, hostileDist: Infinity }, null]
]) {
  test(`holeUpBlock: ${name}`, () => assert.equal(holeUpBlock(args), expected))
}
for (const [name, args, pattern] of [
  ['step names the creeper and the move', { way: 'step', hostile: 'creeper', hostileDist: 3 }, /creeper 3 blocks off.*step/i],
  ['fight names the distance and the hit', { way: 'fight', hostile: 'zombie', hostileDist: 6, hurtMsAgo: 1800 }, /6 blocks off, and it hit me 1\.8 s ago.*fight/]
]) {
  test(`refusalNote: ${name}`, () => assert.match(refusalNote(args), pattern))
}

// the report after the hole, however it was made: a pillar says how to get down, a flooded shaft says it is flooded, and every one
// names the one command that brings the body back
const surface = { x: 5, y: 63, z: -87 }
for (const [name, args, patterns] of [
  ['a capped shaft', { way: 'dig', open: false, surface }, [/dug 3 straight down/, /goto x=5 y=63 z=-87 dig=true/]],
  ['a pillar', { way: 'pillar', open: false, surface }, [/pillared up 3/, /dig the blocks under my feet|goto x=5 y=63 z=-87 dig=true/]],
  ['a pillar that came up short', { way: 'pillar', open: true, surface }, [/pillar.*short|not the full 3/, /goto x=5 y=63 z=-87 dig=true/]],
  ['a shaft that filled with water (Jizo)', { way: 'dig', open: true, wet: true, surface }, [/filled with water|flooded/, /goto x=5 y=63 z=-87 dig=true/, /dig sideways|dig to one side/]],
  ['walled in', { way: 'wall', open: false, surface }, [/walled myself in/]]
]) {
  test(`shelterNote: ${name}`, () => patterns.forEach(p => assert.match(shelterNote(args), p)))
}
