import { boatHabitatPlan, boatHabitatPreflight, boatHabitatCensus, boatHabitatInside, boatHabitatSecure } from '../../src/boat/habitat.mjs'
import dockCommand from './dock.mjs'
import { entityUuid, boatPassengerStatus, boatPassengerProfile } from '../../src/boat/passenger.mjs'
import { breedKey, breedMaterial } from '../../src/villager/breed.mjs'
import { arrivalStepInfo, placeArrivalStep, clearArrivalStep } from '../../src/boat/arrival-step.mjs'
import { arrivalPlan, arrivalPreflight } from '../../src/boat/arrival.mjs'
import { habitatThreats, habitatOwnership, closeHabitatGates } from '../../src/villager/habitat.mjs'

export default {
  doc: 'boat.receive x= y= z= entryX= entryZ= dockX= dockY= dockZ= riverX=1 riverZ=0 uuid= boat= [size block gate materials timeout=1200]: secure and release an arrived passenger in an adjacent dock, then observe that exact passenger enter the prepared house before closing its internal gate',
  stops: 'the exact arrived UUID is on foot inside the closed prepared house; otherwise the internal gate closes and transfer is reported pending',
  args: { x: 'number!', y: 'number!', z: 'number!', entryX: 'number!', entryZ: 'number!', airlock: 'boolean', dockX: 'number!', dockY: 'number!', dockZ: 'number!', riverX: 'number!', riverZ: 'number!', uuid: 'string', boat: 'number', prepare: 'boolean', size: 'number', block: 'string', gate: 'string', materials: 'string', timeout: 'number', plan: 'boolean' },
  async run (api, a) {
    if (a.prepare !== true && !Number.isInteger(a.boat)) throw new Error('boat= must be the observed arrived boat ID')
    if (a.prepare !== true && !entityUuid(a.uuid)) throw new Error('uuid= must be the observed passenger UUID')
    const plan = boatHabitatPlan(a); const arrival = arrivalPlan(plan, a)
    const material = a.block ?? 'cobblestone'; const gateItem = a.gate ?? 'oak_fence_gate'
    const palette = [...new Set(String(a.materials ?? material).split(',').map(s => s.trim()))]
    if (palette.some(item => item !== 'dirt' && !breedMaterial(item))) throw new Error('arrival materials must be full solid building blocks')
    let step = arrivalStepInfo(api, arrival, palette)
    if (a.prepare !== true && step.needed && !step.item) throw new Error('reserve one carried full building block for the temporary arrival step')
    const destinationArgs = { x: a.x, y: a.y, z: a.z, size: plan.width, block: material, gate: gateItem, airlock: plan.airlock, entryX: plan.entry.x, entryZ: plan.entry.z }
    const timeout = a.timeout ?? 1200
    if (!Number.isFinite(timeout) || timeout < 1 || timeout > 14400) throw new Error('timeout= must be 1..14400 seconds')
    const preflight = boatHabitatPreflight(plan, api.block, material, gateItem)
    habitatOwnership(api, { ...plan, shell: [...plan.shell, arrival.roof, ...arrival.sides, ...arrival.opening, step.cell] }, preflight)
    if (preflight.needed.length || preflight.missingBeds.length || preflight.clear.length) throw new Error('prepare the complete habitat before receiving a passenger')
    if (arrivalPreflight(arrival, api.block).length) throw new Error('prepare the enclosed dock-to-house passage before receiving')
    const observe = async () => (await api.act('entity', { name: '*', uuid: true, count: 1000 })).found ?? []
    await habitatThreats(api, plan)
    const initial = await observe()
    if (a.prepare !== true) {
      const target = initial.find(e => e.uuid === a.uuid)
      const profile = boatPassengerProfile(target)
      if (!profile.ok) throw new Error(profile.error)
      const state = boatHabitatCensus(plan, initial).some(e => e.uuid === a.uuid) ? { boats: [] } : await api.act('boat_state', { id: a.boat })
      const boat = state.boats?.find(b => b.id === a.boat)
      if (boat) {
        const issue = boatPassengerStatus(state, a.boat, a.uuid)
        if (issue) throw new Error(issue)
        if (boat.passengers.length !== 1) throw new Error('arrival boat must contain only the exact passenger')
      }
    }
    if (a.plan !== true) await closeHabitatGates(api, plan, gateItem)
    boatHabitatSecure(api, plan, material, gateItem)
    if (initial.length >= 1000) throw new Error('resident census reached its observation limit')
    const insideInitial = boatHabitatCensus(plan, initial)
    const known = new Set(insideInitial.map(e => e.uuid))
    if (a.plan === true && a.prepare !== true) return {
      plan: true, uuid: a.uuid, boat: a.boat, destinationArgs,
      population: insideInitial.length, uuids: insideInitial.map(e => e.uuid),
      alreadyInside: insideInitial.some(e => e.uuid === a.uuid), landing: arrival.landing
    }
    if (insideInitial.some(e => e.uuid === a.uuid)) return { destinationArgs, operatorAt: api.pos(), received: true, secure: true, resumed: true, temporaryStep: step.owned ? step.cell : null, stepCleanupPending: step.owned, uuid: a.uuid, population: insideInitial.length, uuids: insideInitial.map(e => e.uuid) }
    if (a.prepare !== true && known.size && boatHabitatInside(plan, api.pos())) throw new Error('start receive from the dock exterior service stance; crossing an occupied house gate before the boat entrance closes is unsafe')
    await habitatThreats(api, plan)
    const ferryPull = plan.airlock ? { pullInto: true, pullX: plan.innerGate.x + 1, pullY: plan.y, pullZ: plan.entry.z, pullVia: `${arrival.rear.x + 1}:${arrival.rear.y}:${arrival.rear.z},${arrival.rear.x}:${arrival.rear.y}:${arrival.rear.z},${plan.entry.x}:${plan.y}:${plan.entry.z}` } : null
    const approach = plan.airlock ? {
      ferryStageArgs: { x: arrival.landing.x + 5, y: a.dockY - 1, z: arrival.landing.z, centerX: arrival.landing.x + 5, centerZ: arrival.landing.z, radius: 3, pullX: arrival.landing.x - 1, pullY: a.dockY - 1, pullZ: arrival.landing.z },
      stageArgs: { z: arrival.landing.z, minX: a.dockX + 4, tolerance: 0.3, timeout: 90, startX: arrival.dock.serviceStand.x, startY: arrival.dock.serviceStand.y, startZ: arrival.dock.serviceStand.z },
      ferryArgs: { x: a.dockX, y: a.dockY - 1, z: a.dockZ, centerX: arrival.landing.x, centerZ: arrival.landing.z, radius: 0.8, alignZ: arrival.landing.z, alignTolerance: 0.3, ...ferryPull }
    } : {}
    if (a.prepare === true) {
      if (!plan.airlock) throw new Error('guarded next arrival requires airlock=true to retain existing residents')
      if (a.uuid !== undefined || a.boat !== undefined) throw new Error('prepare=true takes no passenger uuid or boat')
      if (a.plan === true) return { plan: true, ready: true, landing: arrival.landing, ferryPull, ...approach }
      try {
        if (boatHabitatInside(plan, api.pos())) {
          if (known.size && arrival.dock.gate.some(p => !api.block(p.x, p.y, p.z)?.solid)) throw new Error('close dock boat portal before moving from occupied house to service airlock')
          await api.act('goto', { x: ferryPull.pullX, y: ferryPull.pullY, z: ferryPull.pullZ, range: 0, into: true })
          await closeHabitatGates(api, plan, gateItem)
        }
        let rearAccess = false
        const closeReadyGates = async () => {
          for (const p of plan.gates) {
            if (rearAccess && breedKey(p) === breedKey(plan.entry)) continue
            if (api.block(p.x, p.y, p.z)?.properties?.open === true) await api.act('toggle', { ...p, open: false })
          }
        }
        const safeApi = { ...api, act: async (name, args) => {
          boatHabitatSecure(api, plan, material, gateItem, rearAccess ? 'outer' : false)
          const rows = await observe()
          const inside = boatHabitatCensus(plan, rows)
          if (![...known].every(uuid => inside.some(e => e.uuid === uuid))) throw new Error('resident left main room before opening arrival portal')
          const result = await api.act(name, args)
          await closeReadyGates()
          return result
        } }
        await dockCommand.run(safeApi, { ...arrival.dockArgs, prepare: true, materials: a.materials ?? material })
        const fromBooth = api.pos().x < arrival.rear.x && api.pos().y >= plan.y - 0.25
        if (fromBooth) {
          rearAccess = true
          await safeApi.act('toggle', { ...plan.entry, open: true })
          await safeApi.act('goto', { x: plan.entry.x - 1, y: plan.y, z: plan.entry.z, range: 0, into: true })
        } else await safeApi.act('goto', { x: arrival.rear.x + 1, y: arrival.rear.y, z: arrival.rear.z, range: 0, into: true })
        const openingPalette = String(a.materials ?? material).split(',').map(s => s.trim())
        for (const p of arrival.opening.slice().reverse()) {
          const b = api.block(p.x, p.y, p.z)
          if (b?.solid) {
            if (!openingPalette.includes(b.name)) throw new Error(`rear passage ${breedKey(p)} is not an authorized dock building block`)
            await safeApi.act('dig', p)
          }
          if (!['air', 'cave_air', 'void_air'].includes(api.block(p.x, p.y, p.z)?.name)) throw new Error(`rear passage did not clear at ${breedKey(p)}`)
        }
        if (fromBooth) await safeApi.act('goto', { x: arrival.rear.x + 1, y: arrival.rear.y, z: arrival.rear.z, range: 0, into: true })
        if (step.record) await clearArrivalStep(safeApi, step, [...known])
        rearAccess = false
        await closeReadyGates()
        if (arrivalPreflight(arrival, api.block).length) throw new Error('arrival passage boundary changed before towing')
        boatHabitatSecure(api, plan, material, gateItem)
        await api.act('toggle', { ...plan.entry, open: true })
        boatHabitatSecure(api, plan, material, gateItem, 'outer')
        // Leave through the verified service floor before starting any swim:
        // swimming from the rear can drive the body into the dock side wall.
        await api.act('goto', { ...arrival.dock.serviceStand, range: 0, into: true })
        boatHabitatSecure(api, plan, material, gateItem, 'outer')
        const finalResidents = boatHabitatCensus(plan, await observe())
        if (![...known].every(uuid => finalResidents.some(e => e.uuid === uuid))) throw new Error('resident left main room while exiting the ready dock')
        return { exitStand: arrival.dock.serviceStand, operatorAt: api.pos(), ready: true, gateOpen: true, entryOpen: true, innerGateClosed: true, landing: arrival.landing, ferryPull, ...approach, population: known.size }
      } catch (error) {
        try { await closeHabitatGates(api, plan, gateItem, true) } catch (closeError) { throw new Error(`${error.message}; house gate closure pending: ${closeError.message}`) }
        throw error
      }
    }
    if (a.plan === true) return { ready: true, plan: true, uuid: a.uuid, population: known.size, landing: arrival.landing, entry: plan.entry, temporaryStep: step.cell, stepRequired: step.needed, reservedStepItem: step.item }
        const boundary = () => {
      if (arrivalPreflight(arrival, api.block).length) throw new Error('arrival passage boundary is no longer sealed')
      for (const p of [...arrival.dock.walls, ...arrival.dock.gate, arrival.dock.serviceFoundation]) {
        if (arrival.opening.some(q => breedKey(q) === breedKey(p))) continue
        if (!api.block(p.x, p.y, p.z)?.solid) throw new Error(`arrival dock boundary open at ${breedKey(p)}`)
      }
      boatHabitatSecure(api, plan, material, gateItem, true)
    }
    try {
      // Existing residents remain behind the closed internal gate while the
      // boat entrance is closed and the exact passenger is released.
      const dockApi = { ...api, inv: () => {
        const inventory = { ...api.inv() }
        if (step.needed && step.item) inventory[step.item] = Math.max(0, (inventory[step.item] ?? 0) - 1)
        return inventory
      }, act: async (name, args) => {
        const result = await api.act(name, args)
        await closeHabitatGates(api, plan, gateItem)
        return result
      } }
      const boatState = await api.act('boat_state', { id: a.boat })
      if (boatState.boats?.some(b => b.id === a.boat)) {
        const issue = boatPassengerStatus(boatState, a.boat, a.uuid)
        if (issue) throw new Error(issue)
        const insideStand = { x: arrival.rear.x + 1, y: arrival.rear.y, z: arrival.rear.z }
        if (plan.airlock) {
          // The ferry leaves the operator in the service booth. Enter the
          // dock before repairing its rear, retaining residents behind the
          // closed inner gate, then release from this dry contained stance.
          await api.act('toggle', { ...plan.entry, open: true })
          boatHabitatSecure(api, plan, material, gateItem, 'outer')
          await api.act('goto', { ...insideStand, range: 0, into: true })
          await closeHabitatGates(api, plan, gateItem)
        }
        await dockCommand.run(dockApi, { ...arrival.dockArgs, uuid: a.uuid, boat: a.boat, materials: palette.join(','), ...(plan.airlock ? { insideX: insideStand.x, insideY: insideStand.y, insideZ: insideStand.z, ...(initial.find(e => e.uuid === a.uuid)?.name === 'villager' ? { deferCapX: arrival.rear.x, deferCapZ: arrival.rear.z } : {}) } : {}) })
      } else {
        const target = initial.find(e => e.uuid === a.uuid)
        const [x, y, z] = String(target?.exact ?? target?.at).split(',').map(Number)
        const inDock = Math.abs(x - (a.dockX + 0.5)) < 1.5 && Math.abs(z - (a.dockZ + 0.5)) < 1.5 && y >= a.dockY - 1.5 && y <= a.dockY + 4
        const inService = plan.airlock && x >= plan.innerGate.x && x < arrival.rear.x + 1 && z >= plan.entry.z && z < plan.entry.z + 1 && y >= arrival.rear.y - 0.2 && y < plan.y + 3
        if (!target || target.vehicleId != null || !Number.isFinite(y) || (!inDock && !inService)) throw new Error('boat is absent and exact passenger is not observed on foot inside the secured dock or service airlock; cannot resume transfer')
      }
      boundary()
      step = await placeArrivalStep(api, step, a.uuid)
      for (const p of arrival.opening.slice().reverse()) {
        const b = api.block(p.x, p.y, p.z)
        if (b?.solid) {
          if (!palette.includes(b.name)) throw new Error(`rear passage ${breedKey(p)} is not an authorized dock building block`)
          await api.act('dig', p)
        }
        if (!['air', 'cave_air', 'void_air'].includes(api.block(p.x, p.y, p.z)?.name)) throw new Error(`rear passage did not clear at ${breedKey(p)}`)
      }
      boundary()
      await api.act('toggle', { ...plan.entry, open: true })
      if (plan.innerGate) await api.act('toggle', { ...plan.innerGate, open: true })
      // Stand inside; beds provide the ordinary village pathfinding lure.
      await api.act('goto', { ...plan.center, range: 0, into: true })
      await api.until(async () => {
        await habitatThreats(api, plan); boundary()
        const rows = await observe()
        if (rows.length >= 1000) throw new Error('resident census reached its observation limit')
        const inside = boatHabitatCensus(plan, rows)
        for (const uuid of known) {
          const e = rows.find(e => e.uuid === uuid)
          if (!e) throw new Error(`resident ${uuid} is no longer observed; transfer pending`)
          const [x, y, z] = String(e.exact ?? e.at).split(',').map(Number)
          const inAnnex = Number.isFinite(y) && Math.abs(x - (a.dockX + 0.5)) < 1.5 && Math.abs(z - (a.dockZ + 0.5)) < 1.5 && y >= a.dockY - 1.5 && y <= a.dockY + 4
          const inPassage = x >= plan.entry.x && x < arrival.rear.x + 1 && z >= arrival.rear.z && z < arrival.rear.z + 1 && y >= arrival.rear.y - 0.2 && y <= arrival.roof.y
          const inBooth = x >= plan.innerGate.x && x < plan.entry.x && z >= plan.entry.z && z < plan.entry.z + 1 && y >= plan.y - 0.1 && y < plan.y + 3
          if (!inside.some(e => e.uuid === uuid) && !inAnnex && !inPassage && !inBooth) throw new Error(`resident ${uuid} left the enclosed house and arrival annex; transfer pending`)
        }
        // Residents may enter the secured annex while the internal gate is
        // open. Never close them out of the final sleeping enclosure.
        if (![...known].every(uuid => inside.some(e => e.uuid === uuid))) return false
        const target = inside.find(e => e.uuid === a.uuid)
        api.report({ received: false, uuid: a.uuid, population: inside.length })
        return !!target && boatHabitatInside(plan, target.position)
      }, { timeout, every: 2, what: `passenger ${a.uuid} has not entered the prepared house; transfer pending in the secured annex (use ordinary species-appropriate food to lure through the enclosed passage)` })
      await closeHabitatGates(api, plan, gateItem)
      boatHabitatSecure(api, plan, material, gateItem)
      const inside = boatHabitatCensus(plan, await observe())
      if (![...known, a.uuid].every(uuid => inside.some(e => e.uuid === uuid))) throw new Error('resident moved before final house closure; transfer pending')
      return { destinationArgs, operatorAt: api.pos(), received: true, secure: true, temporaryStep: step.owned ? step.cell : null, stepCleanupPending: step.owned, uuid: a.uuid, population: inside.length, uuids: inside.map(e => e.uuid), house: { x: plan.x, y: plan.y, z: plan.z, size: plan.width } }
    } catch (error) {
      try { await closeHabitatGates(api, plan, gateItem, true) } catch (closeError) { throw new Error(`${error.message}; internal gate closure pending: ${closeError.message}`) }
      throw error
    }
  }
}
