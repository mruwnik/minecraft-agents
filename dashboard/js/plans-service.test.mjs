import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createPlansService } from './plans-service.mjs'

const structure = {
  legend: { w: { kind: 'crop', crop: 'wheat', seed: 'wheat_seeds', ground: 'farmland', literal: false } },
  layers: [{ y: 1, rows: ['ww'] }]
}
const places = [{ name: 'field', kind: 'farm', by: 'bob', x: 0, y: 70, z: 0, structure }, { name: 'spot', x: 5, y: 5, z: 5 }]

const setup = () => {
  const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'plans-svc-'))
  fs.mkdirSync(path.join(stateDir, 'worlds', 'w'), { recursive: true })
  fs.writeFileSync(path.join(stateDir, 'worlds', 'w', 'places.json'), JSON.stringify(places))
  return stateDir
}

test('list: only plans, each with a summary (no chunks dumped: all unknown)', () => {
  const svc = createPlansService({ stateDir: setup() })
  const { world, plans } = svc.list('w')
  assert.equal(world, 'w')
  assert.deepEqual(plans.map(p => p.name), ['field'])
  assert.deepEqual(plans[0].summary, { total: 2, match: 0, missing: 0, wrong: 0, unknown: 2, percent: 0 })
})

test('list: summaries are recomputed at most once per ttl', () => {
  let now = 1000
  let calls = 0
  const svc = createPlansService({ stateDir: setup(), ttlMs: 10000, now: () => now, onCompare: () => calls++ })
  svc.list('w'); svc.list('w')
  assert.equal(calls, 1)
  now += 10001
  svc.list('w')
  assert.equal(calls, 2)
})

test('get: full comparison with layers and bill; null for an unknown plan', () => {
  const svc = createPlansService({ stateDir: setup() })
  const plan = svc.get('w', 'field')
  assert.equal(plan.name, 'field')
  assert.deepEqual(plan.layers.map(l => l.y), [1])
  assert.deepEqual(plan.bill, { wheat_seeds: 2 })
  assert.deepEqual(plan.bounds, { x1: 0, y1: 71, z1: 0, x2: 1, y2: 71, z2: 0 })
  assert.equal(svc.get('w', 'spot'), null)
  assert.equal(svc.get('w', 'nope'), null)
})

test('a world without places.json lists nothing', () => {
  const svc = createPlansService({ stateDir: setup() })
  assert.deepEqual(svc.list('other').plans, [])
})
