// A walk that presses forward with the position frozen (card 962beec2: five stalls in one day at 119,72,-66, every one
// during flock.lead, with a valid path and nothing to see): the evidence a body can add to say WHY, and honest names
// for blocks the client's registry does not know. Pure: nothing here touches a body.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { blockName, frozenWalk, facingOff, nextNode, aheadCells, serverSide, nearBy, frozenAdvice } from '../src/stall.mjs'

for (const [name, block, expected] of [
  ['a block the registry knows', { name: 'leaf_litter', stateId: 27846 }, 'leaf_litter'],
  ['a state past the registry (a bridged server’s new block)', { name: '', stateId: 29873 }, 'unknown(state 29873)'],
  ['a block with no name at all', { stateId: 30372 }, 'unknown(state 30372)'],
  ['not loaded here', null, 'unloaded'],
  ['undefined', undefined, 'unloaded']
]) {
  test(`blockName: ${name}`, () => assert.equal(blockName(block), expected))
}

for (const [name, sample, expected] of [
  ['forward held, not moving, 2 s', { keys: ['forward'], moved: 0.01, seconds: 2 }, true],
  ['forward held, not moving, 11 s (long before the 12 s alarm)', { keys: ['forward'], moved: 0, seconds: 11 }, true],
  ['forward held but only 1 s', { keys: ['forward'], moved: 0, seconds: 1 }, false],
  ['forward held and walking', { keys: ['forward'], moved: 1.5, seconds: 3 }, false],
  ['no key down', { keys: [], moved: 0, seconds: 30 }, false],
  ['only jump', { keys: ['jump'], moved: 0, seconds: 5 }, false]
]) {
  test(`frozenWalk: ${name}`, () => assert.equal(frozenWalk(sample), expected))
}

const from = { x: 119.5, y: 72, z: -65.5 }
for (const [name, nodes, expected] of [
  ['the first node still ahead', ['119.5,72,-64.5', '119.5,72,-63.5'], { x: 119.5, y: 72, z: -64.5 }],
  ['a node the body already stands on is skipped', ['119.5,72,-65.5', '119.5,72,-66.5'], { x: 119.5, y: 72, z: -66.5 }],
  ['no nodes', [], null],
  ['every node under the feet', ['119.5,72,-65.5'], null]
]) {
  test(`nextNode: ${name}`, () => assert.deepEqual(nextNode(from, nodes), expected))
}

// prismarine-physics walks forward along (-sin yaw, -cos yaw): yaw 0 faces north (-z), pi/2 faces west (-x)
for (const [name, yaw, nodes, expected] of [
  ['facing north, node north', 0, ['119.5,72,-66.5'], 0],
  ['facing north, node south', 0, ['119.5,72,-64.5'], 180],
  ['facing north, node east', 0, ['120.5,72,-65.5'], 90],
  ['facing west, node west', Math.PI / 2, ['118.5,72,-65.5'], 0],
  ['facing south, node north-east', Math.PI, ['120.5,72,-66.5'], 135],
  ['no node ahead', 0, [], null]
]) {
  test(`facingOff: ${name}`, () => {
    const got = facingOff({ yaw, from, nodes })
    return expected === null ? assert.equal(got, null) : assert.equal(got.degrees, expected)
  })
}

for (const [name, yaw, expected] of [
  ['facing north: the cell in front at feet and head', 0, [{ x: 119, y: 72, z: -67 }, { x: 119, y: 73, z: -67 }]],
  ['facing west', Math.PI / 2, [{ x: 118, y: 72, z: -66 }, { x: 118, y: 73, z: -66 }]],
  ['facing south', Math.PI, [{ x: 119, y: 72, z: -65 }, { x: 119, y: 73, z: -65 }]],
  ['facing east', -Math.PI / 2, [{ x: 120, y: 72, z: -66 }, { x: 120, y: 73, z: -66 }]]
]) {
  test(`aheadCells: ${name}`, () => assert.deepEqual(aheadCells({ x: 119.5, y: 72, z: -65.7 }, yaw), expected))
}

for (const [name, last, now, expected] of [
  ['no position packet since the spawn', null, 1000, null],
  ['the last packet, with its age', { x: 109.4, y: 72, z: -61.5, at: 400 }, 1000, { x: 109.4, y: 72, z: -61.5, agoMs: 600 }]
]) {
  test(`serverSide: ${name}`, () => assert.deepEqual(serverSide(last, now), expected))
}

const me = { id: 1, username: 'Perrin', position: { x: 119.5, y: 72, z: -65.7 } }
const entity = (id, name, x, z, extra = {}) => ({ id, name, position: { x, y: 72, z }, ...extra })
for (const [name, entities, expected] of [
  ['a cow pressed against the legs', [me, entity(2, 'cow', 119.9, -65.2)], ['cow 0.6m']],
  ['nearest first, players by their name', [me, entity(3, 'player', 121, -65.7, { username: 'Chani' }), entity(2, 'sheep', 119.5, -64.9)], ['sheep 0.8m', 'Chani 1.5m']],
  ['nothing within two blocks', [me, entity(2, 'cow', 125, -70)], []],
  ['the body itself is not a neighbour', [me], []]
]) {
  test(`nearBy: ${name}`, () => assert.deepEqual(nearBy(entities, me), expected))
}

for (const [name, evidence, expected] of [
  ['the head turned away from the path, legs in a fence', { facing: { degrees: 172, node: '119.5,72,-64.5' }, ahead: ['oak_fence@120,72,-66', 'air@120,73,-66'], near: [], server: null },
    'the head faces 172 degrees off the next node (119.5,72,-64.5) and the legs push into oak_fence at 120,72,-66: something else is turning the head (a lookAt in the task, or the fence nudge)'],
  ['a fence between the body and the next node', { facing: { degrees: 4, node: '119.5,72,-64.5' }, ahead: ['oak_fence@119,72,-65', 'air@119,73,-65'], near: [], server: null },
    'the next node (119.5,72,-64.5) lies beyond oak_fence at 119,72,-65: the path was planned from a cell the body is not really in. goto two blocks back the way it came, then retry'],
  ['a mob against the legs', { facing: { degrees: 3, node: '119.5,72,-64.5' }, ahead: ['air@119,72,-65', 'air@119,73,-65'], near: ['cow 0.5m'], server: null },
    'nothing solid ahead but cow 0.5m is pressed against the body: it is being pushed back as fast as it walks. Step sideways first (goto), then retry'],
  ['the server put it somewhere else', { facing: { degrees: 3, node: '119.5,72,-64.5' }, ahead: ['air@119,72,-65', 'air@119,73,-65'], near: [], server: { x: 109.4, y: 72, z: -61.5, agoMs: 900 } },
    'the server placed the body at 109.4,72,-61.5 0.9 s ago while the client walks from 119.5,72,-65.7: the two disagree on where it stands. Copy this into ../../BUGS.md'],
  ['nothing at all', { facing: { degrees: 3, node: '119.5,72,-64.5' }, ahead: ['air@119,72,-65', 'air@119,73,-65'], near: [], server: null },
    'nothing solid ahead, nothing near, no server correction: the physics itself is not moving the body (a speed of 0 from the server?). Copy this into ../../BUGS.md'],
  ['no path node at all', { facing: null, ahead: ['air@119,72,-65', 'air@119,73,-65'], near: [], server: null },
    'nothing solid ahead, nothing near, no server correction: the physics itself is not moving the body (a speed of 0 from the server?). Copy this into ../../BUGS.md']
]) {
  test(`frozenAdvice: ${name}`, () => assert.equal(frozenAdvice({ exact: [119.5, 72, -65.7], ...evidence }), expected))
}
