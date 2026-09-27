import test from 'node:test'
import assert from 'node:assert/strict'
import { checkedBoatLanding } from '../src/navigation/boat-landing.mjs'
const air = { name: 'air', shapes: [] }
const cube = name => ({ name, shapes: [[0, 0, 0, 1, 1, 1]] })
const boat = { position: { x: 0.15, y: 63.5, z: 0.5 }, yaw: -Math.PI / 2, width: 1.375, height: 0.5625 }
const target = { x: 1, y: 64, z: 0 }
const shore = (x, y, z) => y === 63 ? (x >= 1 ? cube('stone') : { name: 'water', shapes: [] }) : air

test('landing follows vanilla square escape vector onto a full dry platform', () => {
  const p = checkedBoatLanding(shore, boat, target)
  assert.ok(p.x > 1.42 && p.x < 1.43)
  assert.equal(p.y, 64)
  assert.equal(p.z, 0.5)
  assert.equal(p.yaw, -Math.PI / 2)
})
test('landing refuses water, unsupported edges, partial floors, hazards and blocked headroom', () => {
  for (const bad of [
    () => ({ name: 'water', shapes: [] }),
    (x, y, z) => y === 63 ? air : shore(x, y, z),
    (x, y, z) => y === 63 ? { name: 'stone_slab', shapes: [[0, 0, 0, 1, 0.5, 1]] } : shore(x, y, z),
    (x, y, z) => y === 63 ? cube('magma_block') : shore(x, y, z),
    (x, y, z) => y === 65 ? cube('stone') : shore(x, y, z)
  ]) assert.throws(() => checkedBoatLanding(bad, boat, target), /water|support|clearance/)
})
test('landing rejects unreachable shore and yaw that vanilla would clamp', () => {
  assert.throws(() => checkedBoatLanding(shore, boat, { ...target, x: 4 }), /does not reach/)
  assert.throws(() => checkedBoatLanding(shore, { ...boat, yaw: Math.PI / 2 }, target), /behind/)
})
