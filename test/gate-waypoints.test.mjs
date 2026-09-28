import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { Vec3 } from 'vec3'
import { patchGateWaypoints } from '../src/lib/patches.mjs'
import { patchTerrainWaypoints } from '../src/navigation/terrain.mjs'
import { dryStandable } from '../src/navigation/walk.mjs'
import { standingSpots } from '../src/navigation/stand.mjs'
import { cellOf, fieldEdge } from '../src/farm/field.mjs'

const installed = fs.readFileSync(new URL('../node_modules/mineflayer-pathfinder/index.js', import.meta.url), 'utf8')
const patched = patchGateWaypoints(installed)
const section = (source, start, end) => source.slice(source.indexOf(start), source.indexOf(end, source.indexOf(start)))
// Execute the dependency's own waypoint processor and floor-shape helper, with
// the real gate geometry and a tiny read-only world. No server or physics ticks.
const processor = source => {
  const process = section(source, '  function postProcessPath (path)', '  function pathFromPlayer')
  const top = section(source, '  function getPositionOnTopOf (block)', '\n  /**')
  const bot = { pathfinder: { enablePathShortcut: false }, blockAt: at => {
    const p = at.floored()
    const gate = p.x === 60 && p.y === 69 && p.z === -122
    return {
      name: gate ? 'oak_fence_gate' : p.y === 68 ? 'dirt' : 'air', type: gate ? 2 : p.y === 68 ? 1 : 0, position: p,
      shapes: gate ? [[0, 0, 0.375, 1, 1.5, 0.625]] : p.y === 68 ? [[0, 0, 0, 1, 1, 1]] : []
    }
  } }
  return new Function('bot', 'Vec3', 'waterType', 'ladderId', 'vineId', 'stateMovements', `${top}\n${process}\nreturn postProcessPath`)(bot, Vec3, 3, 4, 5, {})
}
const node = (x, z, toPlace = [], toBreak = []) => ({ x, y: 69, z, toPlace, toBreak })

test('the real waypoint processor centres the gate and subsequent route away from the neighbouring fence', () => {
  assert.ok(['patched', 'already'].includes(patched.status))
  const gateAction = { x: 60, y: 69, z: -122, useOne: true }
  const route = [node(60, -123), node(60, -122, [gateAction]), node(60, -121), node(59, -121)]
  const result = processor(patched.source)(route)
  assert.deepEqual(result.map(p => [p.x, p.y, p.z]), [[60.5, 69, -122.5], [60.5, 69, -121.5], [60.5, 69, -120.5], [59.5, 69, -120.5]])
  assert.deepEqual(result[1].toPlace[0], gateAction, 'opening the gate remains part of the cloned path')
  // Gate59-neighbour fence ends at x60: the old x60 waypoint overlaps it.
  assert.ok(result[1].x - 0.3001 > 60)
  assert.equal(patchGateWaypoints(patched.source).status, 'already')
  assert.equal(patchGateWaypoints(patchTerrainWaypoints(patched.source).source).status, 'already')
  assert.equal(patchGateWaypoints('unrecognised dependency').status, 'anchor missing')
})

for (const kind of ['dig', 'scaffold']) {
  test(`gate processing still leaves ${kind} and its following terrain untouched`, () => {
    const work = kind === 'dig' ? node(60, -121, [], [{ x: 60, y: 69, z: -121 }]) : node(60, -121, [{ x: 60, y: 68, z: -121 }])
    const tail = node(59, -121)
    const route = [node(60, -122, [{ x: 60, y: 69, z: -122, useOne: true }]), work, tail]
    const processed = processor(patched.source)(route)
    assert.deepEqual([processed[0].x, processed[0].z], [60.5, -121.5])
    assert.deepEqual([processed[1].x, processed[1].z, processed[2].x, processed[2].z], [60, -121, 59, -121])
    assert.deepEqual([route[0].x, route[0].z, work.x, work.z, tail.x, tail.z], [60, -122, 60, -121, 59, -121], 'processor leaves the search path untouched')
  })
}

for (const barrier of ['oak_fence', 'cobblestone_wall', 'oak_fence_gate']) {
  test(`farm standing excludes the half-block above ${barrier}, preserving ordinary floors`, () => {
    const blockAt = (x, y, z) => ({ name: y === 68 ? 'dirt' : x === 53 && y === 69 ? barrier : 'air', solid: y === 68 || x === 53 && y === 69 })
    const cellAt = (x, y, z) => cellOf(blockAt(x, y, z))
    assert.equal(dryStandable(cellAt, { x: 53, y: 70, z: -112 }), false)
    assert.equal(dryStandable(cellAt, { x: 52, y: 69, z: -111 }), true)
    const spots = standingSpots({ target: { x: 53, y: 69, z: -111 }, blockAt, see: false })
    assert.ok(spots.length)
    assert.ok(spots.every(p => p.y === 69), 'torch-post candidates must stand beside the fence row')
    const edge = fieldEdge(cellAt, [{ x: 53, y: 68, z: -111, ch: 'T' }], { x: 53, y: 70, z: -111 })
    assert.equal(edge.y, 69)
  })
}
