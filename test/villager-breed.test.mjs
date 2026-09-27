import { test } from 'node:test'
import assert from 'node:assert/strict'
import breed from '../library/villager/breed.mjs'
import { breedBill, breedCensus, breedFeedStance, breedPlan, breedPreflight } from '../src/villager-breed.mjs'

const a = 'c071f7d4-8b43-4f01-9c2f-92b648d3d143'
const b = '87b3392e-ae93-4f51-bf07-2f53add88880'
const baby = '95299feb-5cfb-428a-9ac1-72de2d36dc12'
const baby2 = 'f24fa0b0-11c8-40a8-8e7b-4bc2f9d34b6d'
const baby3 = '6e95a7f9-f045-4c1d-9c5a-b720dc6769a9'
const moreBabies = Array.from({ length: 5 }, (_, i) => `00000000-0000-4000-8000-${(i + 1).toString(16).padStart(12, '0')}`)
const newbornIds = [baby, baby2, baby3, ...moreBabies]
const adult3 = 'd10427da-91bc-43a1-9c0e-b2b5ef4f7ad9'
const entity = (uuid, exact, isBaby = false, sleeping = false) => ({ uuid, exact, name: 'villager', metadata: JSON.stringify({ 16: isBaby, 6: sleeping ? 2 : 0 }), vehicleId: null })
const key = p => `${p.x},${p.y},${p.z}`
const foodMarkerName = (plan, suffix = '') => `test-bot-villager-food-${plan.x}-${plan.y}-${plan.z}${suffix}`
function foodMarkers (plan, { credits = {}, total = Object.values(credits).reduce((sum, n) => sum + n, 0), before = [a, b], pending } = {}) {
  const base = foodMarkerName(plan)
  const rows = [{ name: base, by: 'Test-Bot', kind: 'work', x: plan.x, y: plan.y, z: plan.z, note: 'bread' }]
  for (const uuid of before) rows.push({ name: `${base}-before-${uuid}`, by: 'Test-Bot', note: 'before' })
  for (const uuid of [a, b]) rows.push({ name: `${base}-parent-${uuid}`, by: 'Test-Bot', note: String(credits[uuid] ?? 0) })
  rows.push({ name: `${base}-total`, by: 'Test-Bot', note: String(total) })
  if (pending) rows.push({ name: `${base}-pending`, by: 'Test-Bot', note: `${pending.uuid}:${pending.count}` })
  return rows
}

function emptySite (plan) {
  const blocks = new Map()
  const put = (p, name, properties = {}) => blocks.set(key(p), { name, solid: !['air', 'cave_air', 'void_air'].includes(name), properties })
  for (let dx = -1; dx <= plan.width; dx++) for (let dz = -1; dz <= plan.width; dz++) put({ x: plan.x + dx, y: plan.y - 1, z: plan.z + dz }, 'stone')
  const block = (x, y, z) => blocks.get(`${x},${y},${z}`) ?? { name: 'air', solid: false }
  return { blocks, block, put }
}

function breederApi ({ prepared = true, target = 3, size, airlock = false, population, plants = [], noBirth = false, adultOnlyAfterRound = false, hideAdultAfterFirstFood = false, escapeBaby = false, escapeKnownBabyAfterFirstFood = false, checkpointFailAt = Infinity, openGateAfterFood = false, hostile = false, hostileType = 'zombie', hostileAfterFirstFood = false, foodResult, day = true, wakeOnWait = false, groundOnWait = false, birthCountLimit = Infinity } = {}) {
  const args = { x: 10, y: 64, z: 20, target, ...(size === undefined ? {} : { size }) }
  if (airlock) Object.assign(args, { size: size ?? 8, airlock: true, entryX: args.x + (size ?? 8), entryZ: args.z + (size ?? 8) - 1 })
  const plan = breedPlan(args)
  const site = emptySite(plan)
  if (prepared) {
    for (const cell of plan.shell) site.put(cell, 'cobblestone')
    for (const gate of plan.gates) site.put(gate, 'oak_fence_gate', { facing: 'east', open: false })
    for (const bed of plan.bedSlots) {
      site.put(bed.foot, 'white_bed', { part: 'foot', facing: 'south' })
      site.put(bed.head, 'white_bed', { part: 'head', facing: 'south' })
    }
    for (const light of plan.lights) site.put(light, 'torch')
  }
  for (const plant of plants) site.put(plant, plant.name)
  let residents = population ?? [entity(a, '11.5,64,20.5'), entity(b, '14.5,64,23.5')]
  let foodDrops = 0
  let births = 0
  while (newbornIds[births] && residents.some(e => e.uuid === newbornIds[births])) births++
  let checkpoints = 0
  let hostileActive = hostile
  let currentDay = day
  const calls = []
  const reports = []
  const foodGateStates = []
  const foodStances = []
  const markers = []
  const pauses = []
  const groundWaitChecks = []
  const api = {
    position: { ...plan.center, x: plan.center.x + 0.5, z: plan.center.z + 0.5 },
    block: site.block,
    pos: () => api.position,
    inv: () => ({ cobblestone: 1000, oak_fence_gate: 2, white_bed: 24, torch: 100, bread: 100 }),
    me: () => 'test-bot',
    clock: () => ({ day: currentDay }),
    pause: async seconds => pauses.push(seconds),
    zones: () => [],
    places: () => markers.map(p => ({ ...p })),
    report: value => reports.push(value),
    checkpoint: async () => {
      checkpoints++
      if (checkpoints === checkpointFailAt) {
        if (openGateAfterFood) site.put(plan.gate, 'oak_fence_gate', { facing: 'east', open: true })
        throw new Error('cancel requested')
      }
    },
    act: async (name, action = {}) => {
      calls.push({ name, args: action })
      if (name === 'entity') {
        if (action.hostile === true) return { found: hostileActive ? [{ id: 99, uuid: '11111111-1111-4111-8111-111111111111', exact: `${plan.x + 0.5},${plan.y},${plan.z + 0.5}`, name: hostileType, hostile: true }] : [] }
        return { found: residents }
      }
      if (name === 'mark') {
        const saved = markers.find(p => p.name === action.name)
        const marker = { ...action, by: api.me() }
        assert.ok(String(action.note ?? '').length <= 80, 'shared marker notes are limited to 80 characters')
        if (saved) Object.assign(saved, marker)
        else markers.push(marker)
        return { marked: action.name }
      }
      if (name === 'unmark') {
        const index = markers.findIndex(p => p.name === action.name)
        if (index >= 0) markers.splice(index, 1)
        return { unmarked: action.name }
      }
      if (name === 'place') {
        if (/_bed$/.test(action.item)) {
          site.put({ x: action.x, y: action.y, z: action.z }, action.item, { part: 'foot', facing: action.facing })
          site.put({ x: action.x, y: action.y, z: action.z + 1 }, action.item, { part: 'head', facing: action.facing })
        } else site.put(action, action.item, action.facing ? { facing: action.facing, open: false } : {})
        return { placed: 1 }
      }
      if (name === 'dig') { site.put(action, 'air'); return { dug: true } }
      if (name === 'toggle') {
        site.put(action, 'oak_fence_gate', { open: action.open, facing: 'east' })
        return { closed: !action.open }
      }
      if (name === 'goto') { api.position = { ...action, x: action.x + 0.5, z: action.z + 0.5 }; return {} }
      if (name === 'villager_food') {
        foodDrops++
        foodGateStates.push(site.block(plan.gate.x, plan.gate.y, plan.gate.z).properties.open === true)
        foodStances.push(api.position)
        if (hideAdultAfterFirstFood && foodDrops === 1) residents = residents.filter(e => e.uuid !== b)
        if (escapeKnownBabyAfterFirstFood && foodDrops === 1) residents = residents.map(e => e.uuid === baby ? { ...e, exact: `${plan.x + plan.width + 2},${plan.y},${plan.z + 0.5}` } : e)
        if (hostileAfterFirstFood && foodDrops === 1) hostileActive = true
        const result = foodResult ? foodResult(action, foodDrops) : { uuid: action.uuid, item: action.item, count: action.count, tossed: action.count, collectedBy: { [action.uuid]: action.count }, totalCollected: action.count, confirmed: true }
        const journalPrefix = foodMarkerName(plan)
        const totalRow = markers.find(p => p.name === `${journalPrefix}-total`)
        const currentTotal = totalRow ? Number(totalRow.note) : markers.filter(p => p.name.startsWith(`${journalPrefix}-parent-`)).reduce((sum, p) => sum + Number(p.note), 0)
        const received = result.totalCollected === action.count && Object.entries(result.collectedBy ?? {}).reduce((sum, [, n]) => sum + n, 0) === action.count && Object.keys(result.collectedBy ?? {}).every(uuid => residents.some(e => e.uuid === uuid))
        const baseline = markers.filter(p => p.name.startsWith(`${journalPrefix}-before-`)).map(p => p.name.slice(`${journalPrefix}-before-`.length))
        const required = (args.target - residents.length) * 6 + 4
        if (received && currentTotal + action.count >= required && !residents.some(e => e.baby && !baseline.includes(e.uuid))) {
          if (adultOnlyAfterRound) residents = [...residents, entity(adult3, `${plan.x + 1.5},${plan.y},${plan.z + 0.5}`)]
          else if (!noBirth) {
            const count = Math.min(args.target - residents.length, birthCountLimit, Math.floor((currentTotal + action.count - 4) / 6))
            for (let index = 0; index < count; index++) {
              const id = newbornIds[births++]
              const p = entity(id, `${plan.x + 0.5},${plan.y},${plan.z + 0.5}`, true)
              residents = [...residents, escapeBaby ? { ...p, exact: `${plan.x + plan.width + 2},${plan.y},${plan.z + 0.5}` } : p]
            }
          }
        }
        return result
      }
      throw new Error(`unexpected action ${name}`)
    },
    until: async (predicate, options) => {
      assert.ok(options.timeout > 0)
      if (await predicate()) return
      if (wakeOnWait) {
        currentDay = true
        residents = residents.map(e => ({ ...e, metadata: JSON.stringify({ 16: false, 6: 0 }) }))
        if (await predicate()) return
      }
      if (groundOnWait) {
        const high = residents.filter(e => Number(e.exact.split(',')[1]) > 64.05)
        if (high.length) {
          groundWaitChecks.push(foodDrops)
          residents = residents.map(e => {
            const [x, y, z] = e.exact.split(',')
            return Number(y) > 64.05 ? { ...e, exact: `${x},64,${z}` } : e
          })
          if (await predicate()) return
        }
      }
      throw new Error(options.what)
    },
    cleanupAct: async (name, action) => api.act(name, action)
  }
  return { api, args, plan, site, calls, reports, foodGateStates, foodStances, markers, pauses, groundWaitChecks, get foodDrops () { return foodDrops }, get residents () { return residents }, get gateOpen () { return site.block(plan.gate.x, plan.gate.y, plan.gate.z).properties.open === true } }
}

test('villager.breed validates target and bed capacity', () => {
  for (const target of [1, 25, 2.5, '3']) assert.throws(() => breedPlan({ x: 0, y: 64, z: 0, target }), /target=2\.\.24/)
  assert.equal(breedPlan({ x: 0, y: 64, z: 0, target: 4, size: 5 }).beds.length, 4)
  assert.throws(() => breedPlan({ x: 0, y: 64, z: 0, target: 5, size: 5 }), /size=.*enough.*bed slots/)
  assert.equal(breedPlan({ x: 0, y: 64, z: 0, target: 8 }).width >= 6, true)
  assert.ok(breedPlan({ x: 0, y: 64, z: 0, target: 24 }).width <= 15)
})

test('villager.breed census counts babies and requires UUID, age metadata, exact position, and on-foot residents', () => {
  const plan = breedPlan({ x: 0, y: 64, z: 0, target: 3 })
  assert.deepEqual(breedCensus(plan, [entity(a, '0.5,64,0.5'), entity(baby, '1.5,64,1.5', true)]).map(e => e.baby), [false, true])
  assert.equal(breedCensus(plan, [{ ...entity(a, '0.5,64,0.5'), metadata: undefined, baby: false }])[0].baby, false)
  for (const malformed of [
    { ...entity(a, '0.5,64,0.5'), uuid: undefined },
    { ...entity(a, '0.5,64,0.5'), metadata: '{}' },
    { ...entity(a, '0.5,64,0.5'), exact: undefined },
    { ...entity(a, '0.5,64,0.5'), vehicleId: 11 }
  ]) assert.throws(() => breedCensus(plan, [malformed]))
})

test('villager.breed preflight requires a flat loaded floor, clear headroom, complete beds, and a baby-safe roofed shell', () => {
  const plan = breedPlan({ x: 0, y: 64, z: 0, target: 2 })
  const site = emptySite(plan)
  const ready = breedPreflight(plan, site.block)
  assert.ok(ready.needed.some(p => p.y === plan.y + 3), 'roof must be provisioned')
  assert.ok(ready.needed.some(p => p.item === 'oak_fence_gate'))
  assert.equal(ready.missingBeds.length, plan.target)
  assert.throws(() => breedPreflight(plan, (x, y, z) => x === 1 && y === plan.y + 2 && z === 1 ? null : site.block(x, y, z)), /unloaded breeder cell/)
  assert.throws(() => breedPreflight(plan, (x, y, z) => x === 1 && y === plan.y + 2 && z === 1 ? { name: 'stone', solid: true } : site.block(x, y, z)), /headroom blocked/)
  assert.throws(() => breedPreflight(plan, (x, y, z) => y === plan.y - 1 && x === 0 ? { name: 'water', solid: false } : site.block(x, y, z)), /flat solid dry floor/)
  assert.throws(() => breedPreflight(plan, (x, y, z) => y === plan.y - 1 && x === 0 ? { name: 'stone_slab', solid: true } : site.block(x, y, z)), /flat solid dry floor/)
})

test('villager.breed refuses partial or reversed beds and computes exact bed/food bill', () => {
  const plan = breedPlan({ x: 0, y: 64, z: 0, target: 3 })
  const site = emptySite(plan)
  const first = plan.beds[0]
  site.put(first.foot, 'white_bed', { part: 'foot', facing: 'north' })
  site.put(first.head, 'white_bed', { part: 'head', facing: 'north' })
  assert.throws(() => breedPreflight(plan, site.block), /wrong facing/)
  site.put(first.foot, 'white_bed', { part: 'foot', facing: 'south' })
  site.put(first.head, 'air')
  assert.throws(() => breedPreflight(plan, site.block), /partial or has wrong facing/)
  const prepared = breedPreflight(plan, emptySite(plan).block)
  const { bill, shortages } = breedBill(prepared, { cobblestone: 1000, oak_fence_gate: 1, torch: 100, white_bed: 3, bread: 18 }, 3)
  assert.equal(bill.white_bed, 3)
  assert.equal(bill.bread, 18)
  assert.deepEqual(shortages, [])
  assert.ok(breedBill(prepared, { bread: 17 }, 3).shortages.some(s => s.startsWith('bread:1 ')))
})

test('villager.breed chooses dry supported feed stances with a clear adult approach', () => {
  const plan = breedPlan({ x: 0, y: 64, z: 0, target: 3 })
  const site = emptySite(plan)
  const stance = breedFeedStance(plan, { x: 2.5, y: 64, z: 2.5 }, site.block)
  assert.ok(Math.hypot(stance.x + 0.5 - 2.5, stance.z + 0.5 - 2.5) >= 0.8)
  assert.ok(Math.hypot(stance.x + 0.5 - 2.5, stance.z + 0.5 - 2.5) <= 2.5)
  assert.equal(site.block(stance.x, stance.y - 1, stance.z).solid, true)
  assert.equal(site.block(stance.x, stance.y + 1, stance.z).name, 'air')
  assert.equal(plan.beds.some(b => (b.foot.x === stance.x && b.foot.z === stance.z) || (b.head.x === stance.x && b.head.z === stance.z)), false)
})

test('villager.breed bed rows have a walkable aisle from the gate to a clear neighbor of every bed', () => {
  const check = plan => {
    const bedCells = new Set(plan.beds.flatMap(b => [key(b.foot), key(b.head)]))
    const id = (x, z) => `${x},${z}`
    // Floor torches have no collision and stay walkable; beds occupy the bed-cell path.
    const isAisle = (x, z) => x >= plan.x && x < plan.x + plan.width && z >= plan.z && z < plan.z + plan.width && !bedCells.has(`${x},${plan.y},${z}`)
    const entrance = { x: plan.x, z: plan.z + Math.floor(plan.width / 2) }
    assert.ok(isAisle(entrance.x, entrance.z), `width ${plan.width} doorway must open into the aisle`)
    const visited = new Set([id(entrance.x, entrance.z)])
    const queue = [entrance]
    for (let i = 0; i < queue.length; i++) for (const [dx, dz] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
      const p = { x: queue[i].x + dx, z: queue[i].z + dz }
      if (!isAisle(p.x, p.z) || visited.has(id(p.x, p.z))) continue
      visited.add(id(p.x, p.z)); queue.push(p)
    }
    for (const bed of plan.beds) {
      assert.ok(bed.foot.x >= plan.x && bed.head.x < plan.x + plan.width && bed.foot.z >= plan.z && bed.head.z < plan.z + plan.width, `both halves of bed ${key(bed.foot)} must fit inside`)
      const adjacent = [bed.foot, bed.head].flatMap(p => [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => ({ x: p.x + dx, z: p.z + dz })))
      assert.ok(adjacent.some(p => visited.has(id(p.x, p.z))), `bed ${key(bed.foot)} needs a connected approach cell`)
    }
  }
  for (let target = 2; target <= 24; target++) check(breedPlan({ x: 0, y: 64, z: 0, target }))
  for (let size = 5; size <= 15; size++) check(breedPlan({ x: 0, y: 64, z: 0, size, target: Math.min(24, Math.floor(size / 2) * Math.floor((size + 1) / 3)) }))
})

test('villager.breed target includes existing babies, but over-target never culls residents', async () => {
  const reached = breederApi({ target: 3, population: [entity(a, '12.5,64,21.5'), entity(b, '13.5,64,21.5'), entity(baby, '14.5,64,21.5', true)] })
  const result = await breed.run(reached.api, reached.args)
  assert.equal(result.reached, true)
  assert.equal(result.population, 3)
  assert.equal(result.babies, 1)
  assert.equal(reached.foodDrops, 0)
  assert.equal(reached.gateOpen, false)
  const over = breederApi({ target: 2, population: [entity(a, '12.5,64,21.5'), entity(b, '13.5,64,21.5'), entity(baby, '14.5,64,21.5', true)] })
  await assert.rejects(breed.run(over.api, over.args), /below the 3 observed residents; no villagers will be removed/)
  assert.equal(over.calls.some(c => ['place', 'toggle', 'villager_food'].includes(c.name)), false)
})

test('villager.breed confirms both food pickup and a new baby UUID before claiming success', async () => {
  const run = breederApi({ target: 3 })
  const result = await breed.run(run.api, run.args)
  assert.equal(result.reached, true)
  assert.equal(result.population, 3)
  assert.deepEqual(result.newborns, [baby])
  assert.equal(run.foodDrops, 1)
  assert.deepEqual(run.calls.filter(c => c.name === 'villager_food').map(c => [c.args.uuid, c.args.item, c.args.count]), [[a, 'bread', 10]])
  assert.equal(run.gateOpen, false)
  assert.deepEqual(run.foodGateStates, [false], 'the refuge gate stays observed closed during shared food delivery')
  for (const stance of run.foodStances) {
    const x = Math.floor(stance.x); const z = Math.floor(stance.z)
    assert.equal(run.site.block(x, stance.y - 1, z).solid, true)
    assert.equal(run.site.block(x, stance.y + 1, z).name, 'air')
    assert.equal(run.plan.beds.some(b => (b.foot.x === x && b.foot.z === z) || (b.head.x === x && b.head.z === z)), false)
  }
})

test('villager.breed supports read-only planning and resumes a prepared shelter without duplicate construction', async () => {
  const preview = breederApi({ target: 3 })
  const report = await breed.run(preview.api, { ...preview.args, plan: true })
  assert.equal(report.plan, true)
  assert.equal(report.ready, true)
  assert.equal(preview.calls.some(c => ['place', 'toggle', 'goto', 'villager_food'].includes(c.name)), false)

  const run = breederApi({ target: 3, size: 6 })
  await breed.run(run.api, run.args)
  const placed = run.calls.filter(c => c.name === 'place').length
  run.args.target = 4
  const resumed = await breed.run(run.api, run.args)
  assert.equal(resumed.population, 4)
  assert.equal(resumed.newborns[0], baby2)
  const additions = run.calls.filter(c => c.name === 'place').slice(placed)
  assert.equal(additions.length, 0, 'the existing larger blueprint supplies the next bed without construction')
})

test('villager.breed reports vegetation and incomplete shelter without performing construction', async () => {
  const plant = { x: 10, y: 64, z: 21, name: 'leaf_litter' }
  const run = breederApi({ target: 2, plants: [plant] })
  const result = await breed.run(run.api, { ...run.args, plan: true })
  assert.deepEqual(result.clears, [key(plant)])
  assert.equal(result.habitatReady, false)
  assert.equal(result.ready, false)
  await assert.rejects(breed.run(run.api, run.args), /blueprint.check and blueprint.build/)
  assert.equal(run.calls.some(c => ['place', 'dig', 'villager_food'].includes(c.name)), false)
  assert.equal(run.site.block(plant.x, plant.y, plant.z).name, 'leaf_litter')
  const empty = breederApi({ prepared: false })
  await assert.rejects(breed.run(empty.api, empty.args), /habitat is not complete/)
  assert.equal(empty.calls.some(c => ['place', 'dig', 'villager_food'].includes(c.name)), false)
})

test('villager.breed requires two adults inside before construction or food delivery', async () => {
  const run = breederApi({ target: 3, population: [entity(a, '12.5,64,21.5'), entity(baby, '13.5,64,21.5', true)] })
  await assert.rejects(breed.run(run.api, run.args), /observed on-foot adults/)
  assert.equal(run.calls.some(c => ['place', 'toggle', 'villager_food'].includes(c.name)), false)
})

test('villager.breed with airlock refuses an outside start before opening the house gate', async () => {
  const run = breederApi({ target: 3, airlock: true })
  run.api.position = { x: run.args.x - 1, y: run.args.y, z: run.args.z + 1 }
  await assert.rejects(breed.run(run.api, run.args), /start breeding inside the main sleeping room/)
  assert.equal(run.calls.some(c => ['place', 'dig', 'toggle', 'goto', 'villager_food'].includes(c.name)), false)
})

test('villager.breed requires carried food but reuses beds already supplied by the blueprint', async () => {
  const short = breederApi({ target: 4 })
  short.api.inv = () => ({ bread: 5 })
  await assert.rejects(breed.run(short.api, short.args), /bread:/)
  assert.equal(short.calls.some(c => ['place', 'toggle', 'villager_food'].includes(c.name)), false)
  const ready = breederApi({ target: 3 })
  ready.api.inv = () => ({ bread: 10 })
  const result = await breed.run(ready.api, ready.args)
  assert.equal(result.reached, true)
  assert.equal(ready.calls.some(c => ['place', 'dig'].includes(c.name)), false)
})

test('villager.breed refuses entities classified hostile near the shelter before construction or feeding', async () => {
  for (const hostileType of ['zombie', 'slime', 'warden']) {
    const run = breederApi({ target: 3, hostile: true, hostileType })
    await assert.rejects(breed.run(run.api, run.args), /hostile .* near breeder/, hostileType)
    assert.equal(run.calls.some(c => ['place', 'toggle', 'villager_food'].includes(c.name)), false, hostileType)
  }
})

test('villager.breed stops before a second food drop if a hostile approaches during the run', async () => {
  const run = breederApi({ target: 3, hostileAfterFirstFood: true })
  await assert.rejects(breed.run(run.api, run.args), /hostile .* near breeder/)
  assert.equal(run.foodDrops, 1)
  assert.equal(run.gateOpen, false)
})

test('villager.breed never repeats food after unconfirmed pickup or an absent known villager', async () => {
  const pickup = breederApi({ target: 3 })
  const original = pickup.api.act
  pickup.api.act = async (name, args) => {
    const result = await original(name, args)
    return name === 'villager_food' ? { ...result, collectedBy: { [args.uuid]: 1 }, totalCollected: 1 } : result
  }
  await assert.rejects(breed.run(pickup.api, pickup.args), /food pickup was not confirmed/)
  assert.equal(pickup.foodDrops, 1)
  assert.equal(pickup.gateOpen, false)
  const missing = breederApi({ target: 3, hideAdultAfterFirstFood: true })
  await assert.rejects(breed.run(missing.api, missing.args), /known villager .* no longer observed inside/)
  assert.equal(missing.foodDrops, 1)
})

test('villager.breed credits only exact fresh adult-pair collection receipts', async () => {
  for (const collectedBy of [
    { '11111111-1111-4111-8111-111111111111': 3 },
    { [a]: 2 }
  ]) {
    const run = breederApi({ target: 3, foodResult: (action) => ({ uuid: action.uuid, item: action.item, count: action.count, tossed: action.count, collectedBy, totalCollected: Object.values(collectedBy).reduce((sum, n) => sum + n, 0) }) })
    await assert.rejects(breed.run(run.api, run.args), /food pickup was not confirmed/)
    assert.equal(run.foodDrops, 1, 'ambiguous or partial pickup stops before another offer')
    assert.equal(run.markers.length, 7, 'persist compact food, pair, baseline, aggregate, credits, and pending records')
    const base = foodMarkerName(run.plan)
    assert.equal(run.markers.find(p => p.name === base)?.note, 'bread')
    assert.equal(run.markers.find(p => p.name === `${base}-pending`)?.note, `${a}:10`)
    assert.ok(run.markers.every(p => (p.note ?? '').length <= 80))
  }
})

test('villager.breed resumes an aggregate food credit and requests only the remaining batch', async () => {
  const run = breederApi({ target: 3 })
  const markerName = foodMarkerName(run.plan)
  run.markers.push(...foodMarkers(run.plan, { credits: { [a]: 3 }, total: 3 }))
  const result = await breed.run(run.api, run.args)
  const fed = run.calls.filter(c => c.name === 'villager_food')
  assert.equal(fed.length, 1)
  assert.equal(fed[0].args.count, 7, 'request only the remaining amount after the 3-unit receipt')
  assert.equal(result.reached, true)
  assert.equal(run.markers.find(p => p.name === `${markerName}-total`)?.note, '10', 'keep aggregate food until its birth is observed')
})

test('villager.breed refuses to repeat a persisted pending toss without inspection', async () => {
  const run = breederApi({ target: 3 })
  run.markers.push(...foodMarkers(run.plan, { credits: { [a]: 3 }, pending: { uuid: b, count: 3 } }))
  await assert.rejects(breed.run(run.api, run.args), /pending|inspect/i)
  assert.equal(run.foodDrops, 0)
})

test('villager.breed waits for daylight and both observed parents to wake before offering food', async () => {
  const run = breederApi({ target: 3, day: false, wakeOnWait: true, population: [entity(a, '11.5,64,20.5', false, true), entity(b, '14.5,64,23.5', false, true)] })
  const result = await breed.run(run.api, run.args)
  assert.equal(result.reached, true)
  assert.equal(run.foodDrops, 1)
  assert.equal(run.calls.some(c => c.name === 'villager_food' && c.args.uuid === a), true)
})

test('villager.breed waits for a floating parent to reach the planned floor before offering food', async () => {
  const run = breederApi({ target: 3, groundOnWait: true, population: [entity(a, '11.5,65.56,20.5'), entity(b, '14.5,65.56,23.5')] })
  const result = await breed.run(run.api, run.args)
  assert.equal(result.reached, true)
  assert.deepEqual(run.groundWaitChecks, [0], 'the high parent was observed before any offer, then grounded at Y=64')
  assert.equal(run.foodDrops, 1)
})

test('villager.breed recognizes a fully self-returned offer without crediting either parent', async () => {
  const run = breederApi({ target: 3, foodResult: (action) => ({ uuid: action.uuid, item: action.item, count: action.count, tossed: action.count, collectedBy: {}, totalCollected: action.count, selfCollected: action.count, returnedToInventory: true, ambiguous: false }) })
  await assert.rejects(breed.run(run.api, run.args), /shared offering returned to the bot three times/)
  assert.equal(run.foodDrops, 3, 'stop after the bounded three confirmed self-returns')
  assert.deepEqual(run.pauses, [2, 2], 'retry only after the separation pause')
  const base = foodMarkerName(run.plan)
  assert.equal(run.markers.find(p => p.name === `${base}-parent-${a}`)?.note, '0')
  assert.equal(run.markers.some(p => p.name === `${base}-pending`), false, 'verified inventory return clears the pending offer')
})

test('villager.breed retries one full self-return and accepts a later exact adult receipt', async () => {
  const run = breederApi({ target: 3, foodResult: (action, attempt) => attempt === 1
    ? { uuid: action.uuid, item: action.item, count: action.count, tossed: action.count, collectedBy: {}, totalCollected: action.count, selfCollected: action.count, returnedToInventory: true, ambiguous: false }
    : { uuid: action.uuid, item: action.item, count: action.count, tossed: action.count, collectedBy: { [action.uuid]: action.count }, totalCollected: action.count, ambiguous: false } })
  const result = await breed.run(run.api, run.args)
  assert.equal(result.reached, true)
  const feeds = run.calls.filter(c => c.name === 'villager_food')
  assert.deepEqual(feeds.map(c => c.args.uuid), [a, a])
  assert.deepEqual(run.pauses, [2])
  assert.equal(run.markers.find(p => p.name === `${foodMarkerName(run.plan)}-total`)?.note, '10')
})

test('villager.breed keeps a pending journal after an incomplete self-return receipt', async () => {
  const run = breederApi({ target: 3, foodResult: (action) => ({ uuid: action.uuid, item: action.item, count: action.count, tossed: action.count, collectedBy: {}, totalCollected: 2, selfCollected: 2, returnedToInventory: false, ambiguous: false }) })
  await assert.rejects(breed.run(run.api, run.args), /food pickup was not confirmed/)
  assert.equal(run.foodDrops, 1)
  const base = foodMarkerName(run.plan)
  assert.equal(run.markers.find(p => p.name === `${base}-pending`)?.note, `${a}:10`, 'uncertain partial return must block any repeat offer')
})

test('villager.breed retries only a confirmed-not-tossed offer after the parents separate', async () => {
  const run = breederApi({ target: 3, foodResult: (action, attempt) => attempt === 1
    ? { uuid: action.uuid, item: action.item, count: action.count, tossed: 0, notTossed: true, inventoryUnchanged: true, collectedBy: {}, totalCollected: 0, ambiguous: false }
    : { uuid: action.uuid, item: action.item, count: action.count, tossed: action.count, collectedBy: { [action.uuid]: action.count }, totalCollected: action.count, ambiguous: false } })
  const result = await breed.run(run.api, run.args)
  assert.equal(result.reached, true)
  const offers = run.calls.filter(c => c.name === 'villager_food')
  assert.deepEqual(offers.map(c => c.args.uuid), [a, a], 'only the proven untossed first offer is retried')
  assert.equal(run.markers.some(p => p.name.startsWith(`${foodMarkerName(run.plan)}-pending`)), false)
})

test('villager.breed carries compact aggregate credit and provisions the remaining batch', async () => {
  const run = breederApi({ target: 4, population: [entity(a, '11.5,64,20.5'), entity(b, '14.5,64,23.5')] })
  run.markers.push(...foodMarkers(run.plan, { credits: { [a]: 6 }, total: 6 }))
  const result = await breed.run(run.api, run.args)
  assert.equal(result.reached, true)
  assert.equal(result.population, 4)
  const offers = run.calls.filter(c => c.name === 'villager_food')
  assert.equal(offers.length, 1)
  assert.equal(offers[0].args.count, 10)
  const base = foodMarkerName(run.plan)
  assert.equal(run.markers.find(p => p.name === `${base}-total`)?.note, '16', 'retain aggregate food until the two births are observed')
})

test('villager.breed asks for a 32-unit shared batch with 14 credited for seven missing births', async () => {
  const run = breederApi({ target: 10, population: [entity(a, '11.5,64,20.5'), entity(b, '14.5,64,23.5'), entity(baby, '12.5,64,21.5', true)] })
  run.markers.push(...foodMarkers(run.plan, { total: 14, before: [a, b, baby] }))
  const result = await breed.run(run.api, run.args)
  assert.equal(result.population, 10)
  const offer = run.calls.find(c => c.name === 'villager_food')
  assert.equal(offer.args.count, 32, '42 bread for seven births plus four extra, less 14 credited')
  assert.equal(offer.args.uuid, a, 'the shared provision can be collected by any fresh known adult')
})

test('villager.breed keeps food collected by a known baby held until that baby grows', async () => {
  const run = breederApi({
    target: 6,
    birthCountLimit: 1,
    population: [entity(a, '11.5,64,20.5'), entity(b, '14.5,64,23.5'), entity(baby, '12.5,64,21.5', true)],
    foodResult: action => ({ uuid: action.uuid, item: action.item, count: action.count, tossed: action.count, collectedBy: { [baby]: action.count }, totalCollected: action.count, ambiguous: false })
  })
  await assert.rejects(breed.run(run.api, run.args), /no new baby observed after shared food provisioning/)
  assert.equal(run.foodDrops, 1)
  const base = foodMarkerName(run.plan)
  assert.equal(run.markers.find(p => p.name === `${base}-held-${baby}`)?.note, '22', 'baby-collected food remains family credit but is not available for another batch')
  assert.equal(run.markers.some(p => p.name === `${base}-pending`), false)
})

test('villager.breed stops when a fed pair produces no baby and does not blindly feed again', async () => {
  const run = breederApi({ target: 3, noBirth: true })
  await assert.rejects(breed.run(run.api, run.args), /no new baby observed after shared food provisioning/)
  assert.equal(run.foodDrops, 1)
  assert.equal(run.gateOpen, false)
})

test('villager.breed detects a newborn that escapes the planned enclosure before continuing', async () => {
  const run = breederApi({ target: 4, escapeBaby: true })
  await assert.rejects(breed.run(run.api, run.args), /no new baby observed after shared food provisioning/)
  assert.equal(run.foodDrops, 1)
})

test('villager.breed stops if a previously observed baby escapes the shelter', async () => {
  const run = breederApi({ target: 4, size: 6, population: [entity(a, '11.5,64,20.5'), entity(b, '15.5,64,25.5'), entity(baby, '14.5,64,21.5', true)], escapeKnownBabyAfterFirstFood: true })
  await assert.rejects(breed.run(run.api, run.args), /known villager .* no longer observed inside/)
  assert.equal(run.foodDrops, 1)
  assert.equal(run.gateOpen, false)
})

test('villager.breed rejects a population increase without a new baby UUID', async () => {
  const run = breederApi({ target: 3, adultOnlyAfterRound: true })
  await assert.rejects(breed.run(run.api, run.args), /population increased without an observed new baby UUID/)
  assert.equal(run.foodDrops, 1)
  assert.equal(run.gateOpen, false)
})

test('villager.breed waits for eight distinct baby UUIDs to grow two adults to target ten', async () => {
  const run = breederApi({ target: 10 })
  const result = await breed.run(run.api, run.args)
  assert.equal(result.population, 10)
  assert.deepEqual(result.newborns, newbornIds)
  assert.equal(result.newborns.length, 8)
  assert.equal(new Set(result.newborns).size, 8)
  assert.equal(run.foodDrops, 1, 'one aggregate batch provisions the missing births')
})

test('villager.breed cancellation stops before shared food delivery', async () => {
  const run = breederApi({ target: 4, checkpointFailAt: 1 })
  await assert.rejects(breed.run(run.api, run.args), /cancel requested/)
  assert.equal(run.foodDrops, 0)
  assert.equal(run.gateOpen, false)
})

test('villager.breed closes the gate during cancellation cleanup before returning an error', async () => {
  const run = breederApi({ target: 4, checkpointFailAt: 1, openGateAfterFood: true })
  await assert.rejects(breed.run(run.api, run.args), /cancel requested/)
  assert.equal(run.gateOpen, false)
  assert.equal(run.site.block(run.plan.gate.x, run.plan.gate.y, run.plan.gate.z).properties.open, false)
})
