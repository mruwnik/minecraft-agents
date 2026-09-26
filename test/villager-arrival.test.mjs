import { test } from 'node:test'
import assert from 'node:assert/strict'
import { arrivalPlan, arrivalPreflight } from '../src/villager-arrival.mjs'
import { breedCensus, breedInside, breedPlan } from '../src/villager-breed.mjs'
import prepare from '../library/villager/prepare.mjs'
import receive from '../library/villager/receive.mjs'
import breed from '../library/villager/breed.mjs'
import dock from '../library/villager/dock.mjs'
import { villagerDockPlan } from '../src/lib.mjs'

const houseArgs = { target: 10, x: -143, y: 65, z: -175, size: 8, entryX: -135, entryZ: -168, airlock: true }
const dockArgs = { dockX: -132, dockY: 63, dockZ: -169, riverX: 1, riverZ: 0 }
const key = p => `${p.x},${p.y},${p.z}`

test('arrival plan joins the prepared east entry to a dry west dock with a sealed 3-high passage', () => {
  const house = breedPlan(houseArgs)
  const arrival = arrivalPlan(house, dockArgs)
  assert.deepEqual(arrival.rear, { x: -134, y: 64, z: -168 })
  assert.equal(arrival.opening.length, 3)
  assert.deepEqual(arrival.opening.map(p => p.y), [64, 65, 66])
  assert.deepEqual(arrival.roof, { x: -134, y: 67, z: -168 })
  assert.equal(arrival.sides.length, 8)
  assert.deepEqual(house.innerGate, { x: -138, y: 65, z: -168 })
  assert.equal(house.shell.some(p => p.x === -137 && p.y === 66 && p.z === -169), true, 'the partition closes the baby escape corner above the inner gate')
  for (let x = -138; x <= -136; x++) for (let y = 65; y <= 67; y++) assert.ok(house.shell.some(p => p.x === x && p.y === y && p.z === -169), `solid partition at ${x},${y},-169`)
  assert.equal(breedInside(house, { x: -137.5, y: 65, z: -168.5 }), false, 'airlock vestibule is outside the counted sleeping room')
  assert.equal(breedInside(house, { x: -140.5, y: 65, z: -168.5 }), true, 'main sleeping room counts the resident')
  assert.equal(breedCensus(house, [entity(firstUuid, '-137.5,65,-168.5')]).length, 0, 'a villager at the inner gate does not count as housed')
  const wider = breedPlan({ ...houseArgs, size: 10, entryX: houseArgs.x + 10, entryZ: houseArgs.z + 9 })
  assert.equal(wider.lights.some(light => wider.shell.some(p => key(p) === key(light))), false, 'lights do not overlap the airlock partition in a wider layout')
  assert.equal(wider.lights.some(light => wider.gates.some(p => key(p) === key(light))), false, 'lights do not overlap airlock gates')
  assert.deepEqual(arrival.landing, { x: -131, y: 62.5, z: -168 })
  assert.deepEqual(arrival.dockArgs, { x: -132, y: 63, z: -169, riverX: 1, riverZ: 0 })
})

test('arrival preflight requires a dry supported passage and reports only roof/side construction', () => {
  const arrival = arrivalPlan(breedPlan(houseArgs), dockArgs)
  const blocks = new Map([[key({ x: arrival.rear.x, y: arrival.rear.y - 1, z: arrival.rear.z }), { name: 'stone', solid: true }]])
  const blockAt = (x, y, z) => blocks.get(`${x},${y},${z}`) ?? { name: 'air', solid: false }
  const needed = arrivalPreflight(arrival, blockAt)
  assert.equal(needed.length, 9)
  assert.deepEqual(new Set(needed.map(key)), new Set([key(arrival.roof), ...arrival.sides.map(key)]))
  for (const p of arrival.opening) assert.equal(blockAt(p.x, p.y, p.z).name, 'air', 'preflight does not close passage cells')
  assert.throws(() => arrivalPreflight(arrival, (x, y, z) => x === arrival.rear.x && y === arrival.rear.y - 1 && z === arrival.rear.z ? { name: 'air', solid: false } : blockAt(x, y, z)), /lacks dry support/)
  assert.throws(() => arrivalPreflight(arrival, (x, y, z) => x === arrival.rear.x && y === arrival.rear.y - 1 && z === arrival.rear.z ? { name: 'stone_slab', solid: true } : blockAt(x, y, z)), /lacks dry support/)
  assert.throws(() => arrivalPreflight(arrival, (x, y, z) => x === arrival.roof.x && y === arrival.roof.y && z === arrival.roof.z ? null : blockAt(x, y, z)), /boundary .* unloaded/)
})

test('arrival plan refuses misaligned or too-high house entries and non-east docks', () => {
  const house = breedPlan(houseArgs)
  assert.throws(() => arrivalPlan(house, { ...dockArgs, dockX: -131 }), /must adjoin the dry west dock rear/)
  assert.throws(() => arrivalPlan(house, { ...dockArgs, riverX: 0 }), /opening east toward the river/)
  assert.throws(() => arrivalPlan({ ...house, y: 66 }, dockArgs), /at most one upward foot step/)
})

const firstUuid = 'c071f7d4-8b43-4f01-9c2f-92b648d3d143'
const incomingUuid = '87b3392e-ae93-4f51-bf07-2f53add88880'
const entity = (uuid, exact, baby = false) => ({ uuid, exact, name: 'villager', baby, adult: !baby, vehicleId: null })
const arrivalNewborns = Array.from({ length: 8 }, (_, i) => `10000000-0000-4000-8000-${(i + 1).toString(16).padStart(12, '0')}`)

function receiveApi (initialResidents = []) {
  const house = breedPlan(houseArgs)
  const arrival = arrivalPlan(house, dockArgs)
  const dockPlan = villagerDockPlan({ x: dockArgs.dockX, y: dockArgs.dockY, z: dockArgs.dockZ }, { x: 1, z: 0 })
  const blocks = new Map()
  const markers = []
  const stepCell = { ...arrival.dock.cell, y: arrival.dock.cell.y - 1 }
  const put = (p, name, properties = {}) => blocks.set(key(p), { name, solid: !['air', 'water', 'cave_air', 'void_air'].includes(name), properties })
  const get = p => blocks.get(key(p)) ?? { name: 'air', solid: false, properties: {} }
  for (let x = house.x - 1; x <= house.x + house.width; x++) for (let z = house.z - 1; z <= house.z + house.width; z++) put({ x, y: house.y - 1, z }, 'stone')
  put({ x: arrival.rear.x, y: arrival.rear.y - 1, z: arrival.rear.z }, 'cobblestone')
  const gateCells = new Set(dockPlan.gateBase.map(key))
  for (const p of dockPlan.ring) {
    put(p, gateCells.has(key(p)) ? 'water' : 'cobblestone')
    put({ ...p, y: p.y - 1 }, gateCells.has(key(p)) ? 'water' : 'stone')
    put({ ...p, y: p.y - 2 }, 'stone')
    for (let n = 1; n <= 3; n++) put({ ...p, y: p.y + n }, dockPlan.gate.some(g => key(g) === key({ ...p, y: p.y + n })) ? 'air' : 'cobblestone')
  }
  for (const p of dockPlan.interior) {
    put({ ...p, y: p.y - 1 }, 'water')
    put({ ...p, y: p.y - 2 }, 'stone')
    put(p, 'air')
    put({ ...p, y: p.y + 1 }, 'air')
  }
  // The dry release cell inside the rear passage has a full-block floor.
  put({ x: arrival.rear.x + 1, y: arrival.rear.y - 1, z: arrival.rear.z }, 'grass_block')
  put(dockPlan.interior[0], 'grass_block')
  put(dockPlan.serviceStand, 'water')
  put({ ...dockPlan.serviceStand, y: dockPlan.serviceStand.y - 1 }, 'stone')
  let residents = [...initialResidents]
  let passenger = null
  let passengerReleased = false
  let boatPresent = true
  let foodDrops = 0
  let births = 0
  let actorPosition = { x: dockPlan.serviceStand.x + 0.5, y: dockPlan.serviceStand.y, z: dockPlan.serviceStand.z + 0.5 }
  const calls = []
  const api = {
    block: (x, y, z) => get({ x, y, z }),
    inv: () => ({ cobblestone: 1000, stone: 1000, oak_fence_gate: 4, white_bed: 20, torch: 100, bread: 100 }),
    me: () => 'test-bot',
    zones: () => [], places: () => markers,
    pos: () => actorPosition,
    clock: () => ({ day: true }),
    pause: async () => {},
    report: () => {},
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (name === 'entity') {
        const aboard = passenger && !passengerReleased ? [entity(passenger, `${dockArgs.dockX + 0.5},${dockArgs.dockY},${dockArgs.dockZ + 0.5}`)] : []
        return { found: args.hostile === true ? [] : [...residents, ...aboard] }
      }
      if (name === 'boat_state') {
        if (args.id === undefined) return { selfId: 12, mounted: null, boats: [] }
        if (!boatPresent) return { selfId: 12, mounted: null, boats: [] }
        return { selfId: 12, mounted: null, boats: [{ id: args.id, exact: `${dockArgs.dockX + 0.5},${dockArgs.dockY},${dockArgs.dockZ + 0.5}`, leashHolderId: null, passengers: [{ uuid: passenger, id: 90, name: 'villager' }] }] }
      }
      if (name === 'place') {
        if (key(stepCell) === key(args)) {
          assert.ok(residents.some(e => e.uuid === passenger && e.vehicleId === null && e.baby === false && e.adult === true), 'place the step only after the exact released adult is observed on foot')
          assert.equal((await api.act('boat_state', {})).boats.length, 0, 'the arrival boat is gone before the step is placed')
        }
        if (/_bed$/.test(args.item)) {
          put(args, args.item, { part: 'foot', facing: args.facing })
          put({ x: args.x, y: args.y, z: args.z + 1 }, args.item, { part: 'head', facing: args.facing })
        } else put(args, args.item, { facing: args.facing, open: false })
        return { placed: 1 }
      }
      if (name === 'toggle') {
        const old = get(args)
        put(args, old.name, { ...old.properties, open: args.open })
        return { closed: !args.open }
      }
      if (name === 'dig') {
        const refills = dockPlan.gateBase.some(p => p.x === args.x && p.z === args.z && args.y === p.y - 1)
        put(args, refills || key(stepCell) === key(args) ? 'water' : 'air')
        return { dug: 1 }
      }
      if (name === 'goto') { actorPosition = { x: args.x + 0.5, y: args.y, z: args.z + 0.5 }; return {} }
      if (name === 'boat_release') {
        assert.equal(args.passengerUuid, passenger, 'release must name the exact boat passenger UUID')
        assert.equal(get(house.entry).properties.open, false, 'house entry remains closed while dock releases the passenger')
        assert.equal(get(house.innerGate).properties.open, false, 'inner airlock gate remains closed during release')
        const openGate = dockPlan.gate.find(p => !api.block(p.x, p.y, p.z).solid)
        assert.equal(openGate, undefined, 'dock seals every gate cell before release')
        assert.deepEqual(actorPosition, { x: arrival.rear.x + 1.5, y: arrival.rear.y, z: arrival.rear.z + 0.5 }, 'release happens from the verified dry interior stance')
        residents = [...residents, entity(passenger, `${dockPlan.cell.x + 1.5},${dockPlan.cell.y + 1},${dockPlan.cell.z + 0.5}`)]
        passengerReleased = true
        return { released: passenger, onFoot: { uuid: passenger, vehicleId: null, exact: `${dockPlan.cell.x + 1.5},${dockPlan.cell.y + 1},${dockPlan.cell.z + 0.5}` } }
      }
      if (name === 'boat_unleash') return { unleashed: true }
      if (name === 'mark') {
        assert.ok(String(args.note ?? '').length <= 80, 'shared place marker notes are limited to 80 characters')
        const prior = markers.find(p => p.name === args.name)
        if (prior) Object.assign(prior, args, { by: 'test-bot' })
        else markers.push({ ...args, by: 'test-bot' })
        return { marked: args.name }
      }
      if (name === 'unmark') {
        const i = markers.findIndex(p => p.name === args.name)
        if (i >= 0) markers.splice(i, 1)
        return { unmarked: args.name }
      }
      if (name === 'villager_food') {
        foodDrops++
        const newbornCount = Math.min(10 - residents.length, Math.max(1, Math.floor((args.count - 4) / 6)))
        for (let i = 0; i < newbornCount; i++) residents.push(entity(arrivalNewborns[births++], `${house.x + 2.5},${house.y},${house.z + 1.5}`, true))
        return { collectorUuid: args.uuid, pickedUp: args.count }
      }
      throw new Error(`unexpected action ${name}`)
    },
    checkpoint: async () => {},
    until: async (predicate, options) => {
      assert.ok(options.timeout > 0)
      // A villager only enters once the inner gate is open. Model the pathfinding lure
      // by moving the arriving UUID through the enclosed corridor into the house.
      if (!await predicate()) {
        const row = residents.find(e => e.uuid === passenger)
        if (row && get(house.entry).properties.open === true && get(house.innerGate).properties.open === true) {
          row.exact = `${house.x + 2.5},${house.y},${house.z + 1.5}`
          if (!await predicate()) throw new Error(options.what)
        } else throw new Error(options.what)
      }
    },
    cleanupAct: async (name, args) => api.act(name, args),
    preparePassenger: uuid => { passenger = uuid },
    setBoatPresent: value => { boatPresent = value },
    setPassengerReleased: value => { passengerReleased = value },
    setResidents: value => { residents = value },
    setBlock: (p, name) => put(p, name),
    setPosition: value => { actorPosition = { ...value } },
    sealDock: () => {
      for (const p of dockPlan.gate) put(p, 'cobblestone')
      put(dockPlan.serviceFoundation, 'cobblestone')
    }
  }
  return { api, house, arrival, dockPlan, calls, get residents () { return residents } }
}

test('villager.receive seals the dock before exact UUID release and keeps prior residents inside on a second arrival', async () => {
  const existing = entity(firstUuid, `${houseArgs.x + 2.5},${houseArgs.y},${houseArgs.z + 1.5}`)
  const run = receiveApi([existing])
  const prepareArgs = { ...houseArgs, ...dockArgs }
  const prepared = await prepare.run(run.api, prepareArgs)
  assert.equal(prepared.secure, true)
  assert.deepEqual(prepared.receiveArgs, { ...prepared.breedArgs, dockX: dockArgs.dockX, dockY: dockArgs.dockY, dockZ: dockArgs.dockZ, riverX: 1, riverZ: 0, materials: 'cobblestone' })
  // The boat portal opens for the incoming boat only after the internal entry is closed.
  run.api.sealDock()
  const beforePortal = run.calls.length
  const portal = await receive.run(run.api, { ...prepareArgs, prepare: true })
  assert.equal(portal.gateOpen, true)
  assert.equal(portal.entryOpen, true)
  assert.equal(portal.innerGateClosed, true)
  assert.deepEqual(portal.ferryStageArgs, {
    x: run.arrival.landing.x + 5, y: dockArgs.dockY - 1, z: run.arrival.landing.z,
    centerX: run.arrival.landing.x + 5, centerZ: run.arrival.landing.z, radius: 3,
    pullX: run.arrival.landing.x - 1, pullY: dockArgs.dockY - 1, pullZ: run.arrival.landing.z
  })
  assert.deepEqual(portal.stageArgs, {
    z: run.arrival.landing.z, minX: dockArgs.dockX + 4, tolerance: 0.3, timeout: 90,
    startX: run.dockPlan.serviceStand.x, startY: run.dockPlan.serviceStand.y, startZ: run.dockPlan.serviceStand.z
  })
  assert.deepEqual(portal.ferryArgs, {
    x: dockArgs.dockX, y: dockArgs.dockY - 1, z: dockArgs.dockZ,
    centerX: run.arrival.landing.x, centerZ: run.arrival.landing.z, radius: 0.8,
    alignZ: run.arrival.landing.z, alignTolerance: 0.3, ...portal.ferryPull
  })
  assert.equal(portal.ferryPull.pullInto, true)
  assert.equal(run.api.block(run.house.entry.x, run.house.entry.y, run.house.entry.z).properties.open, true, 'the covered outer transfer gate opens for the passenger')
  assert.equal(run.api.block(run.house.innerGate.x, run.house.innerGate.y, run.house.innerGate.z).properties.open, false, 'the existing resident remains behind the closed inner gate')
  assert.equal(breedCensus(run.house, run.residents).some(e => e.uuid === firstUuid), true, 'the resident stays in the main room while the service passage is ready')
  const dockPortalOpenedAt = run.calls.findIndex((c, i) => i >= beforePortal && c.name === 'dig' && run.dockPlan.gate.some(p => key(p) === key(c.args)))
  const boothStanceAt = run.calls.findIndex((c, i) => i >= beforePortal && c.name === 'goto' && c.args.x === run.house.entry.x - 1 && c.args.y === run.house.y && c.args.z === run.house.entry.z && c.args.into === true)
  const rearStanceAt = run.calls.findIndex((c, i) => i >= beforePortal && c.name === 'goto' && c.args.x === run.arrival.rear.x + 1 && c.args.y === run.arrival.rear.y && c.args.z === run.arrival.rear.z && c.args.into === true)
  const rearPassageDigAt = run.calls.findIndex((c, i) => i >= beforePortal && c.name === 'dig' && run.arrival.opening.some(p => key(p) === key(c.args)))
  assert.ok(dockPortalOpenedAt >= beforePortal && boothStanceAt > dockPortalOpenedAt && rearPassageDigAt > boothStanceAt && rearStanceAt > rearPassageDigAt, 'open the dock portal, reach the outer booth, clear the passage from there, then enter it')
  const incomingStart = run.calls.length
  run.api.preparePassenger(incomingUuid)
  const received = await receive.run(run.api, { ...prepareArgs, uuid: incomingUuid, boat: 42, timeout: 30 })
  assert.equal(received.received, true)
  assert.equal(received.population, 2)
  assert.deepEqual(new Set(received.uuids), new Set([firstUuid, incomingUuid]))
  const stepInfo = run.api.places().find(p => p.name === 'test-bot-villager-step--132-62--169')
  assert.ok(stepInfo, 'the command records ownership of its temporary block')
  assert.deepEqual(JSON.parse(stepInfo.note), ['cobblestone', incomingUuid, 'p'])
  assert.ok(stepInfo.note.length <= 80)
  assert.equal(run.api.block(-132, 62, -169).name, 'cobblestone')
  const stepPlaceAt = run.calls.findIndex(c => c.name === 'place' && key(c.args) === key({ x: -132, y: 62, z: -169 }))
  const intentMarkAt = run.calls.findIndex(c => c.name === 'mark' && JSON.parse(c.args.note ?? '[]')[2] === 'i')
  const placedMarkAt = run.calls.findIndex((c, i) => i > stepPlaceAt && c.name === 'mark' && JSON.parse(c.args.note ?? '[]')[2] === 'p')
  assert.ok(intentMarkAt >= 0 && intentMarkAt < stepPlaceAt && placedMarkAt > stepPlaceAt, 'persist intent before placement and confirm ownership after observing the block')
  const releasedAt = run.calls.findIndex(c => c.name === 'boat_release')
  const incomingCalls = run.calls.slice(incomingStart, releasedAt)
  assert.equal(incomingCalls.some(c => c.name === 'dig' && key(c.args) === key(run.dockPlan.service)), false, 'inside release does not open an exterior service slot')
  assert.equal(incomingCalls.some(c => c.name === 'goto' && c.args.x === run.dockPlan.serviceStand.x && c.args.y === run.dockPlan.serviceStand.y && c.args.z === run.dockPlan.serviceStand.z), false, 'inside release never routes the operator to the exterior service stand')
  const openedHouseAt = run.calls.findIndex((c, i) => i > releasedAt && c.name === 'toggle' && c.args.x === run.house.entry.x && c.args.y === run.house.entry.y && c.args.z === run.house.entry.z && c.args.open === true)
  assert.ok(releasedAt >= 0 && openedHouseAt > releasedAt, 'exact passenger releases while house entry is closed; inner gate opens after')
  assert.equal(run.api.block(run.house.entry.x, run.house.entry.y, run.house.entry.z).properties.open, false, 'inner gate closes after the new arrival enters')
  assert.equal(run.api.block(run.house.innerGate.x, run.house.innerGate.y, run.house.innerGate.z).properties.open, false, 'airlock inner gate closes too')
  assert.equal(run.residents.some(e => e.uuid === firstUuid), true, 'the earlier resident remains observed inside')
  const beforeCleanup = run.calls.length
  const nextReady = await receive.run(run.api, { ...prepareArgs, prepare: true })
  assert.equal(nextReady.ready, true)
  assert.equal(run.api.places().some(p => p.name === stepInfo.name), false, 'remove the owned marker after the UUID is securely in the house')
  assert.equal(run.api.block(-132, 62, -169).name, 'water', 'restore the surveyed water cell after secure arrival')
  const cleanupCalls = run.calls.slice(beforeCleanup)
  const removeStepAt = cleanupCalls.findIndex(c => c.name === 'dig' && key(c.args) === key({ x: -132, y: 62, z: -169 }))
  const unmarkAt = cleanupCalls.findIndex(c => c.name === 'unmark' && c.args.name === stepInfo.name)
  assert.ok(removeStepAt >= 0 && unmarkAt > removeStepAt, 'remove the block first, then clear only its ownership marker')
})

test('villager.receive returns normalized breeding args and leaves the operator inside for two adults to grow to ten', async () => {
  const existing = entity(firstUuid, `${houseArgs.x + 2.5},${houseArgs.y},${houseArgs.z + 1.5}`)
  const run = receiveApi([existing])
  const args = { ...houseArgs, ...dockArgs }
  await prepare.run(run.api, args)
  run.api.sealDock()
  const firstPortal = await receive.run(run.api, { ...args, prepare: true })
  assert.deepEqual(firstPortal.exitStand, run.dockPlan.serviceStand)
  run.api.preparePassenger(incomingUuid)
  const received = await receive.run(run.api, { ...args, uuid: incomingUuid, boat: 42, timeout: 30 })
  assert.deepEqual(received.breedArgs, {
    target: 10, x: -143, y: 65, z: -175, size: 8,
    block: 'cobblestone', gate: 'oak_fence_gate', bed: 'white_bed', airlock: true,
    entryX: -135, entryZ: -168
  })
  assert.ok(breedInside(breedPlan(received.breedArgs), received.operatorAt), 'receive parks the operator in the main sleeping room')
  const grown = await breed.run(run.api, received.breedArgs)
  assert.equal(grown.reached, true)
  assert.equal(grown.population, 10)
  assert.equal(grown.newborns.length, 8)
  assert.equal(new Set(grown.newborns).size, 8)
})

test('villager.receive resumes an exact passenger already on foot in the secured dock without releasing again', async () => {
  const existing = entity(firstUuid, `${houseArgs.x + 2.5},${houseArgs.y},${houseArgs.z + 1.5}`)
  const run = receiveApi([existing])
  const args = { ...houseArgs, ...dockArgs }
  await prepare.run(run.api, args)
  run.api.preparePassenger(incomingUuid)
  const dockVillager = entity(incomingUuid, `${dockArgs.dockX + 0.5},${dockArgs.dockY + 1},${dockArgs.dockZ + 0.5}`)
  run.api.setPassengerReleased(true)
  run.api.sealDock()
  await receive.run(run.api, { ...args, prepare: true })
  run.api.setResidents([existing, dockVillager])
  run.api.setBoatPresent(false)
  // The previous dock release has already closed the boat gate and resealed its service cell.
  run.api.sealDock()
  const result = await receive.run(run.api, { ...args, uuid: incomingUuid, boat: 42, timeout: 30 })
  assert.equal(result.received, true)
  assert.deepEqual(new Set(result.uuids), new Set([firstUuid, incomingUuid]))
  assert.equal(run.calls.some(c => c.name === 'boat_release'), false)
  assert.ok(run.calls.some(c => c.name === 'dig'), 'resume may finish opening the prepared internal passage')
})

test('villager.receive returns resumed success for the exact UUID already inside the secure house', async () => {
  const resident = entity(incomingUuid, `${houseArgs.x + 2.5},${houseArgs.y},${houseArgs.z + 1.5}`)
  const run = receiveApi([resident])
  const args = { ...houseArgs, ...dockArgs }
  await prepare.run(run.api, args)
  const result = await receive.run(run.api, { ...args, uuid: incomingUuid, boat: 42, timeout: 30 })
  assert.equal(result.resumed, true)
  assert.equal(result.received, true)
  assert.equal(run.calls.some(c => c.name === 'boat_release' || c.name === 'toggle'), false)
})

test('dock refuses an unsafe interior release stance before any movement or world edits', async () => {
  const run = receiveApi([])
  const inside = { x: run.arrival.rear.x + 1, y: run.arrival.rear.y, z: run.arrival.rear.z }
  run.api.setBlock({ x: inside.x, y: inside.y - 1, z: inside.z }, 'air')
  await assert.rejects(dock.run(run.api, {
    uuid: incomingUuid, boat: 42, x: dockArgs.dockX, y: dockArgs.dockY, z: dockArgs.dockZ,
    riverX: 1, riverZ: 0, insideX: inside.x, insideY: inside.y, insideZ: inside.z
  }), /interior release stance needs safe solid support/)
  assert.equal(run.calls.length, 0, 'stance validation precedes movement, gate edits, and boat actions')
})

test('the second boat portal can reopen after receive while the resident house gate stays closed', async () => {
  const run = receiveApi([])
  const args = { ...houseArgs, ...dockArgs }
  await prepare.run(run.api, args)
  await receive.run(run.api, { ...args, prepare: true })
  run.api.preparePassenger(firstUuid)
  const first = await receive.run(run.api, { ...args, uuid: firstUuid, boat: 41, timeout: 30 })
  assert.equal(first.secure, true)
  for (const p of run.dockPlan.gate) assert.equal(run.api.block(p.x, p.y, p.z).solid, true, 'first receive closes the dock gate')
  assert.equal(run.api.block(run.house.entry.x, run.house.entry.y, run.house.entry.z).properties.open, false)
  assert.equal(run.api.block(run.house.innerGate.x, run.house.innerGate.y, run.house.innerGate.z).properties.open, false)

  const beforePortal = run.calls.length
  const portal = await receive.run(run.api, { ...args, prepare: true })
  assert.ok(run.dockPlan.gate.every(p => !run.api.block(p.x, p.y, p.z).solid), 'second boat gate is open for arrival')
  assert.deepEqual(portal.exitStand, run.dockPlan.serviceStand)
  assert.deepEqual(portal.operatorAt, { x: run.dockPlan.serviceStand.x + 0.5, y: run.dockPlan.serviceStand.y, z: run.dockPlan.serviceStand.z + 0.5 })
  const outerOpened = run.calls.findIndex((c, i) => i >= beforePortal && c.name === 'toggle' && key(c.args) === key(run.house.entry) && c.args.open === true)
  const serviceExit = run.calls.findIndex((c, i) => i > outerOpened && c.name === 'goto' && key(c.args) === key(run.dockPlan.serviceStand) && c.args.into === true && c.args.range === 0)
  assert.ok(outerOpened >= beforePortal && serviceExit > outerOpened, 'operator leaves through the verified service stand after the outer entry is ready')
  assert.equal(run.api.block(run.house.entry.x, run.house.entry.y, run.house.entry.z).properties.open, true, 'the outer transfer gate opens to the covered passage')
  assert.equal(run.api.block(run.house.innerGate.x, run.house.innerGate.y, run.house.innerGate.z).properties.open, false, 'the inner airlock gate remains closed while dock portal reopens')
  assert.equal(run.api.block(run.house.gate.x, run.house.gate.y, run.house.gate.z).properties.open, false, 'primary main-room gate remains shut')
  assert.equal(breedCensus(run.house, run.residents).some(e => e.uuid === firstUuid), true, 'the main-room resident stays enclosed while the passage is open for another boat')
})

test('guarded receive preparation from the outside booth opens only the outer gate, digs top-first, then enters the passage', async () => {
  const existing = entity(firstUuid, `${houseArgs.x + 2.5},${houseArgs.y},${houseArgs.z + 1.5}`)
  const run = receiveApi([existing])
  const args = { ...houseArgs, ...dockArgs }
  await prepare.run(run.api, args)
  run.api.sealDock()
  run.api.setPosition({ x: run.arrival.rear.x - 1.5, y: houseArgs.y, z: run.arrival.rear.z + 0.5 })
  const entry = run.house.entry
  const inner = run.house.innerGate
  const originalAct = run.api.act
  run.api.act = async (name, callArgs = {}) => {
    if (name === 'dig' && run.arrival.opening.some(p => key(p) === key(callArgs))) {
      assert.equal(run.api.block(entry.x, entry.y, entry.z).properties.open, true, 'the operator uses only the outer booth gate')
      assert.equal(run.api.block(inner.x, inner.y, inner.z).properties.open, false, 'the main room remains sealed while passage blocks are removed')
      assert.equal(breedCensus(run.house, run.residents).some(e => e.uuid === firstUuid), true, 'the previous adult remains in the main room during the booth work')
    }
    return originalAct(name, callArgs)
  }
  const before = run.calls.length
  const result = await receive.run(run.api, { ...args, prepare: true })
  assert.equal(result.ready, true)
  const calls = run.calls.slice(before)
  const openOuter = calls.findIndex(c => c.name === 'toggle' && key(c.args) === key(entry) && c.args.open === true)
  const boothStance = calls.findIndex(c => c.name === 'goto' && c.args.x === entry.x - 1 && c.args.y === houseArgs.y && c.args.z === entry.z && c.args.into === true)
  const digs = calls.map((c, i) => c.name === 'dig' && run.arrival.opening.some(p => key(p) === key(c.args)) ? i : -1).filter(i => i >= 0)
  const intoPassage = calls.findIndex((c, i) => i > Math.max(...digs) && c.name === 'goto' && c.args.x === run.arrival.rear.x + 1 && c.args.y === run.arrival.rear.y && c.args.z === run.arrival.rear.z && c.args.into === true)
  assert.ok(openOuter >= 0 && boothStance > openOuter, 'open the outer gate and take the booth-side stance first')
  assert.equal(digs.length, 3)
  assert.deepEqual(digs.map(i => key(calls[i].args)), run.arrival.opening.slice().reverse().map(key), 'remove the rear wall from top to bottom from the booth')
  assert.ok(intoPassage > Math.max(...digs), 'enter the rear passage only after its wall is cleared')
  assert.equal(run.api.block(inner.x, inner.y, inner.z).properties.open, false)
  assert.equal(breedCensus(run.house, run.residents).some(e => e.uuid === firstUuid), true)
})
