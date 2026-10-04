// The runtimes of src/body/ (surface walk, boat, boat travel, travel, riding, villager), each given the live body.
// Their own long and quick actions join the action tables in src/bot.mjs.
import path from 'node:path'
import { isNight } from '../lib.mjs'
import { makeSurfaceWalkRuntime } from '../navigation/surface-walk.mjs'
import { makeBoatRuntime } from './boat.mjs'
import { makeBoatTravelRuntime } from './boat-travel.mjs'
import { driveBoat } from '../navigation/boat-travel.mjs'
import { makeTravelRuntime } from './travel.mjs'
import { makeRidingRuntime } from './riding.mjs'
import { driveHorse } from '../navigation/horse.mjs'
import { makeVillagerRuntime } from './villager.mjs'
import { makeVillagerRosterObserver, saveVillagerObservation } from '../villager/roster.mjs'
import { DATA_ROOT, ROOT, cfg } from './home.mjs'
import { emit } from './events.mjs'
import { edibleCarried } from './runner.mjs'
import { inventoryCounts, findItem, vecOf, goNear } from './helpers.mjs'
import { goals, Vec3, reportPerformance, bot, cancelGuard, pos } from './state.mjs'
import { boatLeashHolder } from './connection.mjs'
import { isHostile, columnAbove } from './reflexes.mjs'
import { makeMoves } from './actions/move.mjs'
import { feeding, setFeeding } from './actions/creature.mjs'

export let swimStepTarget = null
export const surfaceWalkRuntime = makeSurfaceWalkRuntime({
  getBot: () => bot, Vec3, goals, makeMoves, cancelGuard,
  reportPerformance: (...args) => reportPerformance(...args),
  report: data => emit('surface_walk', data),
  dangerous: p => isNight(bot.time.timeOfDay) || Object.values(bot.entities).some(e => e.isValid && isHostile(e) && e.position.distanceTo(p ? new Vec3(p.x, p.y, p.z) : bot.entity.position) < 12)
})
export const boatRuntime = makeBoatRuntime({
  getBot: () => bot, getBoatLeashHolder: () => boatLeashHolder,
  Vec3, vecOf: (...args) => vecOf(...args), goNear, findItem, inventoryCounts, pos, columnAbove: (...args) => columnAbove(...args), cancelGuard,
  getSwimStepTarget: () => swimStepTarget, setSwimStepTarget: value => { swimStepTarget = value }
})
export const boatTravelRuntime = makeBoatTravelRuntime({
  getBot: () => bot, Vec3, cancelGuard, edibleCarried: (...args) => edibleCarried(...args), driveBoat,
  readBoatState: a => boatRuntime.quick.boat_state(a),
  getLeashHolder: id => boatLeashHolder.get(id),
  reportPerformance: (...args) => reportPerformance(...args),
  report: progress => emit('boat_progress', progress)
})
const villagerRosterFile = path.join(DATA_ROOT, 'state', 'villagers.json')
export const villagerRoster = makeVillagerRosterObserver({ file: villagerRosterFile, by: cfg.username })
export const travelRuntime = makeTravelRuntime({ getBot: () => bot, Vec3, cancelGuard, edibleCarried: (...args) => edibleCarried(...args), reportPerformance: (...args) => reportPerformance(...args) })
export const ridingRuntime = makeRidingRuntime({
  getBot: () => bot, Vec3, cancelGuard, edibleCarried: (...args) => edibleCarried(...args), driveHorse, surfaceWalk: surfaceWalkRuntime,
  reportPerformance: (...args) => reportPerformance(...args),
  goNear: async (entity, check) => { check(); await goNear(entity.position, 2.5); check() },
  report: progress => emit('riding_progress', progress)
})
export const villagerRuntime = makeVillagerRuntime({
  getBot: () => bot, Vec3, goNear, findItem, inventoryCounts, cancelGuard, emit,
  by: cfg.username,
  recordVillagerObservation: input => saveVillagerObservation(villagerRosterFile, input),
  getFeeding: () => feeding, setFeeding: value => { setFeeding(value) },
  currentVehicleId: boatRuntime.currentVehicleId
})
