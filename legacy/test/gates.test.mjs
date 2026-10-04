// Gates and walks: what the body does at a fence gate it did not mean to shut, and what the pathfinder makes of an open one
import test from 'node:test'
import assert from 'node:assert/strict'
import { shutNow, openGateWalk, patchParkourFences } from '../src/lib.mjs'

// `toggle x= y= z= open=true` is meant to hold a gate open, but the gates.log listener took the toggle's own click for a walk's
// (mine=true), and the reflex shut it 2 blocks behind the body: open-shut-open-shut three times, then "still closed after 3
// tries" (Kettricken 22:00Z; my test gate 17:01Z). A held gate is never the reflex's to shut
const OPEN_BEHIND = { near: false, open: true, mine: true, leading: false, moving: false, inDoorway: false }
for (const [name, state, expected] of [
  ['walked through and left it: shut', OPEN_BEHIND, true],
  ['held open by toggle: leave it', { ...OPEN_BEHIND, held: true }, false],
  ['held open, and I stand beside it: leave it', { ...OPEN_BEHIND, held: true, near: true }, false]
]) {
  test(`shutNow: ${name}`, () => assert.equal(shutNow(state), expected))
}

// prismarine-block gives an OPEN fence gate boundingBox 'block' with no shapes, so to the pathfinder it is a wall: it routes through
// a gate only while the gate is shut (it opens it itself) and once the gate stands open, held by toggle or opened by the walk
// itself before a replan, every path through it is gone. Chani's goto to the cell beyond her open gate answered "no walkable path"
// (13:19Z), and a walk replanned in the gate cell stalled there 12 s (14:19Z-16:03Z). An open gate is walked through like air
for (const [name, block, expected] of [
  ['an open gate is passable', { name: 'oak_fence_gate', open: true }, { safe: true, physical: false }],
  ['a cherry gate too', { name: 'cherry_fence_gate', open: true }, { safe: true, physical: false }],
  ['a shut gate is left to the pathfinder, which opens it', { name: 'oak_fence_gate', open: false }, null],
  ['a fence is not a gate', { name: 'oak_fence', open: undefined }, null],
  ['air', { name: 'air' }, null],
  ['nothing', null, null]
]) {
  test(`openGateWalk: ${name}`, () => assert.deepEqual(openGateWalk(block), expected))
}

// mineflayer-pathfinder 2.4.5 lib/movements.js getMoveParkourForward: a fence is `physical: false` (nothing stands on it), so from a start
// node one above a fence row (a body mid-jump in a gate cell) three fence posts in a row look like a gap to jump: Chani's stalled walks
// all had "113.5,73,-70.5" as their first node, four cells along her fence line from the gate, and the body bounced against the first
// post at y 72.8-73.2 until the 12 s alarm. Nothing parkours over a fence, a wall or a gate: the scan stops at the first one
const PARKOUR_START = "    if ((block1.physical && block1.height >= block0.height) ||\n"
const PARKOUR_START_FIXED = "    if (this.fences.has(block1.type) || // patched by bot/patch-deps.mjs: no parkour over a fence, wall or gate\n      (block1.physical && block1.height >= block0.height) ||\n"
const PARKOUR_SCAN = "      const blockD = this.getBlock(node, dx, -1, dz)\n"
const PARKOUR_SCAN_FIXED = "      const blockD = this.getBlock(node, dx, -1, dz)\n      if (this.fences.has(blockD.type)) break // patched by bot/patch-deps.mjs: no parkour over a fence, wall or gate\n"
for (const [name, source, expected] of [
  ['the upstream source gets both guards', `a\n${PARKOUR_START}b\n${PARKOUR_SCAN}c`, { status: 'patched', source: `a\n${PARKOUR_START_FIXED}b\n${PARKOUR_SCAN_FIXED}c` }],
  ['patched already: left alone', `a\n${PARKOUR_START_FIXED}b\n${PARKOUR_SCAN_FIXED}c`, { status: 'already', source: `a\n${PARKOUR_START_FIXED}b\n${PARKOUR_SCAN_FIXED}c` }],
  ['one anchor without the other: say so, change nothing', `a\n${PARKOUR_START}b`, { status: 'anchor missing', source: `a\n${PARKOUR_START}b` }],
  ['a new upstream version without that code: say so, change nothing', 'something else', { status: 'anchor missing', source: 'something else' }]
]) {
  test(`patchParkourFences: ${name}`, () => assert.deepEqual(patchParkourFences(source), expected))
}
