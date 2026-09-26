import { test } from 'node:test'
import assert from 'node:assert/strict'
import prepare from '../library/villager/prepare.mjs'
import { breedPlan } from '../src/villager-breed.mjs'

const villagerA = 'c071f7d4-8b43-4f01-9c2f-92b648d3d143'
const airNames = new Set(['air', 'cave_air', 'void_air'])
const key = p => `${p.x},${p.y},${p.z}`

function habitatApi ({ target = 10, size = 8, entryZ = 4, airlock = false, residents = [], inventory = {}, position = { x: 100, y: 63, z: 100 } } = {}) {
  const args = { target, x: -120, y: 63, z: -180, size, airlock, entryX: -120 + size, entryZ: -180 + entryZ }
  const plan = breedPlan(args)
  const blocks = new Map()
  const put = (p, name, properties = {}) => blocks.set(key(p), { name, solid: !airNames.has(name), properties })
  for (let x = plan.x - 1; x <= plan.x + size; x++) for (let z = plan.z - 1; z <= plan.z + size; z++) put({ x, y: plan.y - 1, z }, 'stone')
  const block = (x, y, z) => blocks.get(`${x},${y},${z}`) ?? { name: 'air', solid: false, properties: {} }
  const calls = []
  const reports = []
  let actorPosition = { ...position }
  const api = {
    block,
    inv: () => ({ cobblestone: 1000, oak_fence_gate: 4, white_bed: 24, torch: 100, ...inventory }),
    me: () => 'test-bot',
    pos: () => actorPosition,
    zones: () => [],
    places: () => [],
    report: value => reports.push(value),
    act: async (name, action = {}) => {
      calls.push({ name, args: action })
      if (name === 'entity') return { found: action.hostile === true ? [] : residents }
      if (name === 'place') {
        if (/_bed$/.test(action.item)) {
          put(action, action.item, { part: 'foot', facing: action.facing })
          put({ x: action.x, y: action.y, z: action.z + 1 }, action.item, { part: 'head', facing: action.facing })
        } else put(action, action.item, { ...action, open: false })
        return { placed: 1 }
      }
      if (name === 'toggle') {
        const current = block(action.x, action.y, action.z)
        put(action, current.name, { ...current.properties, open: action.open })
        return { closed: !action.open }
      }
      if (name === 'dig') { put(action, 'air'); return { dug: true } }
      if (name === 'goto') {
        actorPosition = { x: action.x + 0.5, y: action.y, z: action.z + 0.5 }
        return {}
      }
      throw new Error(`unexpected action ${name}`)
    },
    cleanupAct: async (name, action) => api.act(name, action)
  }
  return { api, args, plan, blocks, calls, reports, setPosition: p => { actorPosition = { ...p } } }
}

test('villager.prepare plans target ten with ten beds and no food or adult prerequisite', async () => {
  const run = habitatApi({ inventory: { bread: 0 } })
  const args = { ...run.args, airlock: true, entryX: run.plan.x + run.plan.width, entryZ: run.plan.z + run.plan.width - 1 }
  const plan = await prepare.run(run.api, { ...args, plan: true })
  assert.equal(plan.target, 10)
  assert.equal(plan.bedCount, 10)
  assert.equal(plan.airlock, true)
  assert.deepEqual(plan.innerGate, { x: run.plan.x + 5, y: run.plan.y, z: run.plan.z + 7 })
  assert.equal(plan.population, 0)
  assert.equal(plan.ready, true)
  assert.equal(plan.entry.x, run.plan.x + run.plan.width)
  assert.equal(plan.entry.z, args.entryZ)
  assert.equal(plan.insideEntry.x, run.plan.x + run.plan.width - 1)
  assert.equal(plan.bill.white_bed, 10)
  assert.equal(plan.bill.bread, undefined)
  assert.equal(run.calls.some(c => ['place', 'dig', 'toggle', 'villager_food'].includes(c.name)), false)
})

test('villager.prepare builds and closes a ten-bed habitat without villagers or food, then resumes idempotently', async () => {
  const run = habitatApi({ inventory: { bread: 0 } })
  const first = await prepare.run(run.api, run.args)
  assert.equal(first.prepared, true)
  assert.equal(first.secure, true)
  assert.equal(first.population, 0)
  assert.equal(first.bedCount, 10)
  assert.equal(first.breedArgs.target, 10)
  assert.equal(first.breedArgs.size, 8)
  assert.equal(first.breedArgs.entryX, run.args.entryX)
  assert.equal(run.calls.some(c => c.name === 'villager_food'), false)
  assert.equal(run.calls.filter(c => c.name === 'place' && /_bed$/.test(c.args.item)).length, 10)
  for (const gate of run.plan.gates) assert.equal(run.api.block(gate.x, gate.y, gate.z).properties.open, false)
  const placed = run.calls.filter(c => c.name === 'place').length
  const resumed = await prepare.run(run.api, run.args)
  assert.equal(resumed.prepared, true)
  assert.equal(run.calls.filter(c => c.name === 'place').length, placed)
})

test('villager.prepare preserves an existing on-foot resident UUID through construction', async () => {
  const resident = { uuid: villagerA, exact: '-119.5,63,-179.5', name: 'villager', baby: false, vehicleId: null }
  const run = habitatApi({ target: 10, residents: [resident], inventory: { bread: 0 } })
  const result = await prepare.run(run.api, run.args)
  assert.deepEqual(result.uuids, [villagerA])
  assert.equal(result.population, 1)
  assert.equal(result.secure, true)
})

test('villager.prepare returns reusable breeding args with its custom palette and fixed arrival entry', async () => {
  const run = habitatApi({ inventory: { stone: 1000, spruce_fence_gate: 4, orange_bed: 24, bread: 0 } })
  const args = { ...run.args, block: 'stone', gate: 'spruce_fence_gate', bed: 'orange_bed' }
  const result = await prepare.run(run.api, args)
  assert.deepEqual(result.breedArgs, {
    target: 10,
    x: run.args.x,
    y: run.args.y,
    z: run.args.z,
    size: 8,
    airlock: false,
    entryX: run.args.entryX,
    entryZ: run.args.entryZ,
    block: 'stone',
    gate: 'spruce_fence_gate',
    bed: 'orange_bed'
  })
  assert.equal(run.calls.filter(c => c.name === 'place' && c.args.item === 'orange_bed').length, 10)
  assert.ok(run.calls.some(c => c.name === 'place' && c.args.item === 'spruce_fence_gate'))
})

test('villager.prepare enters the room before building and places each airlock gate from its adjacent clear stance', async () => {
  const run = habitatApi({ airlock: true, entryZ: 7, position: { x: -100, y: 63, z: -160 } })
  const args = { ...run.args, airlock: true, entryX: run.plan.x + run.plan.width, entryZ: run.plan.z + run.plan.width - 1 }
  await prepare.run(run.api, args)
  const firstPlace = run.calls.findIndex(c => c.name === 'place')
  const firstGoto = run.calls.findIndex(c => c.name === 'goto')
  assert.ok(firstGoto >= 0 && firstGoto < firstPlace, 'the builder moves into the room before placing an enclosing wall')
  assert.deepEqual(run.calls[firstGoto].args, { ...run.plan.center, range: 0, into: true })
  for (const gate of run.plan.gates) {
    const placeIndex = run.calls.findIndex(c => c.name === 'place' && c.args.item === 'oak_fence_gate' && key(c.args) === key(gate))
    assert.ok(placeIndex >= 0, `gate placed at ${key(gate)}`)
    const stance = { x: gate.x + (key(gate) === key(run.plan.gate) ? 1 : -1), y: gate.y, z: gate.z }
    const previousGoto = run.calls.slice(0, placeIndex).findLast(c => c.name === 'goto')
    assert.ok(previousGoto, `adjacent stance selected for gate at ${key(gate)}`)
    assert.deepEqual(previousGoto.args, { ...stance, range: 0, into: true })
  }
  for (const bed of run.plan.beds) {
    const placeIndex = run.calls.findIndex(c => c.name === 'place' && c.args.item === 'white_bed' && key(c.args) === key(bed.foot))
    assert.ok(placeIndex >= 0, `bed placed at ${key(bed.foot)}`)
    const previousGoto = run.calls.slice(0, placeIndex).findLast(c => c.name === 'goto')
    assert.deepEqual(previousGoto?.args, { x: bed.foot.x - 1, y: bed.foot.y, z: bed.foot.z, range: 0, into: true })
  }
})

test('villager.prepare resumes from a sealed airlock through the adjacent inner-gate stance', async () => {
  const run = habitatApi({ airlock: true, entryZ: 7 })
  const args = { ...run.args, airlock: true, entryX: run.plan.x + run.plan.width, entryZ: run.plan.z + run.plan.width - 1 }
  await prepare.run(run.api, args)
  const inner = run.plan.innerGate
  run.blocks.delete(key(run.plan.lights[0]))
  run.setPosition({ x: run.plan.entry.x + 0.5, y: run.plan.y, z: run.plan.entry.z + 0.5 })
  run.calls.length = 0
  await prepare.run(run.api, args)
  const openAt = run.calls.findIndex(c => c.name === 'toggle' && key(c.args) === key(inner) && c.args.open === true)
  const approachAt = run.calls.findIndex((c, i) => i > openAt && c.name === 'goto' && c.args.x === inner.x - 1 && c.args.y === inner.y && c.args.z === inner.z && c.args.into === true)
  const closeAt = run.calls.findIndex((c, i) => i > approachAt && c.name === 'toggle' && key(c.args) === key(inner) && c.args.open === false)
  const centerAt = run.calls.findIndex((c, i) => i > closeAt && c.name === 'goto' && key(c.args) === key(run.plan.center) && c.args.into === true)
  const placeAt = run.calls.findIndex(c => c.name === 'place' && c.args.item === 'torch')
  assert.ok(openAt >= 0 && approachAt > openAt && closeAt > approachAt && centerAt > closeAt && placeAt > centerAt)
  assert.equal(run.api.block(run.plan.entry.x, run.plan.entry.y, run.plan.entry.z).properties.open, false, 'the outer gate stays shut while crossing the inner gate')
  assert.equal(run.api.block(inner.x, inner.y, inner.z).properties.open, false, 'the inner gate is shut again before construction resumes')
})
