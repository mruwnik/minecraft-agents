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
