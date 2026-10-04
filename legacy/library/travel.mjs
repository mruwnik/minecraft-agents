import { atRailLaunch, checkedRailRoute, checkedBoatItinerary, planTravel, travelPoint, TravelValidationError } from '../src/navigation/travel.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
import { checkedHorseRoute, HorseRouteError } from '../src/navigation/horse.mjs'

async function act (api, name, args) {
  const result = await api.act(name, args)
  if (result?.stopped) throw new CompositeHandBack(result.stopped)
  return result
}
async function walk (api, args) {
  const state = await act(api, 'rail_state', {})
  if (state.mounted !== null) throw new Error(`travel cannot walk while riding vehicle ${state.mounted}; inspect and dismount safely first`)
  return act(api, 'goto', args)
}
export default {
  doc: 'travel x= y= z= [mode=auto|walk|rail|boat|horse] [boat=id shore=x:y:z] [horse=id] [cart=id track=x:y:z,x:y:z exit=x:y:z] [plan=true] [return=true]: compare complete walking, checked boat-to-shore, powered rail or prepared horse itineraries; return=true prices a walked return only',
  args: { x: 'number!', y: 'number!', z: 'number!', mode: 'string', boat: 'number', shore: 'string', horse: 'number', cart: 'number', track: 'string', exit: 'string', plan: 'boolean', return: 'boolean' },
  stops: 'arrived on foot, or cancellation, changed terrain, unavailable mode, or vehicle progress failure',
  async run (api, a) {
    const from = { ...api.pos() }, to = { x: a.x, y: a.y, z: a.z }
    // Validate the public mode/coordinates before reading assets or moving.
    planTravel({ from, to, mode: a.mode ?? 'auto' })
    let rail = null, unavailable = null
    const supplied = [a.cart, a.track, a.exit].some(value => value !== undefined)
    if (supplied && (!Number.isInteger(a.cart) || !a.track || !a.exit)) throw new Error('rail travel requires cart=id track=fromX:fromY:fromZ,toX:toY:toZ exit=x:y:z together')
    const state = await act(api, 'rail_state', supplied ? { id: a.cart } : {})
    if (supplied) {
      const began = performance.now()
      try { rail = checkedRailRoute(api.block, a.track, travelPoint(a.exit, 'exit')) } catch (error) {
        if (!(error instanceof TravelValidationError)) throw error
        unavailable = error.message
      } finally {
        api.performance?.('travel.plan', performance.now() - began, { track: a.track, cart: a.cart })
      }
      if (rail) {
        if (!state.cart || state.mounted !== null || state.cart.passengers.length || !atRailLaunch(state.cart.position, rail)) {
          unavailable = 'cart must be observed empty at the launch and the traveler must be on foot'; rail = null
        }
      }
    }
    let horse = null, horseUnavailable = null
    if (a.horse !== undefined) {
      if (!Number.isInteger(a.horse)) throw new Error('horse must be an explicit entity id')
      const observed = await act(api, 'horse_state', { id: a.horse })
      const asset = observed.horse
      if (!observed.goalTravel || !asset || asset.tamed !== true || asset.saddled !== true || asset.baby !== false || asset.passengers.length || !Number.isFinite(asset.movementSpeed) || asset.movementSpeed <= 0 || asset.movementSpeed > 0.5) {
        horseUnavailable = 'horse needs a ready controller, confirmed adult/tamed/saddled state, no passenger, and an observed movement speed'
      } else {
        const began = performance.now()
        try { horse = { ...checkedHorseRoute(api.block, asset.at, to), speed: asset.movementSpeed } } catch (error) {
          if (!(error instanceof HorseRouteError)) throw error
          horseUnavailable = error.message
        } finally { api.performance?.('travel.horse', performance.now() - began, { horse: a.horse }) }
      }
    }
    let boat = null, boatUnavailable = null
    if (a.boat !== undefined || a.shore !== undefined) {
      if (!Number.isInteger(a.boat) || a.boat < 0 || !a.shore) throw new Error('boat travel requires boat=<authorized id> shore=<dry feet x:y:z> together')
      const shore = travelPoint(a.shore, 'shore'), observed = await act(api, 'boat_state', { id: a.boat })
      const asset = observed.boats?.find(candidate => candidate.id === a.boat)
      const mounted = observed.mounted === a.boat && state.mounted === a.boat
      if (!observed.goalTravel || !asset || !Array.isArray(asset.passengers) || asset.leashHolderId != null ||
        (mounted ? asset.passengers.length !== 1 || asset.controller?.id !== observed.selfId : asset.passengers.length !== 0 || observed.mounted !== null)) {
        boatUnavailable = 'boat needs a ready controller, no lead, and either an empty authorized boat or this traveler as its sole controlling passenger'
      } else {
        const began = performance.now()
        try { boat = checkedBoatItinerary(api.block, asset, shore, from, { mounted }) } catch (error) {
          if (!(error instanceof TravelValidationError)) throw error
          boatUnavailable = error.message
        } finally { api.performance?.('travel.boat', performance.now() - began, { boat: a.boat, shore: a.shore }) }
      }
    }
    const plan = planTravel({ from, to, rail, horse, boat, mode: a.mode ?? 'auto', returnTrip: a.return === true })
    if (unavailable) plan.options.find(option => option.mode === 'rail').reason = unavailable
    if (horseUnavailable) plan.options.find(option => option.mode === 'horse').reason = horseUnavailable
    if (boatUnavailable) plan.options.find(option => option.mode === 'boat').reason = boatUnavailable
    if (state.mounted !== null) {
      plan.mounted = state.mounted
      for (const option of plan.options.filter(option => option.available)) {
        if (option.mode === 'boat' && boat?.mounted) continue
        option.available = false
        option.reason = `travel cannot start while riding vehicle ${state.mounted}; inspect and dismount safely first`
      }
      const selected = plan.options.find(option => option.available && (a.mode === undefined || a.mode === 'auto' || a.mode === option.mode))
      plan.selected = selected?.mode ?? null; plan.seconds = selected?.seconds ?? null
    }
    const resume = `travel x=${a.x} y=${a.y} z=${a.z} mode=${a.mode ?? 'auto'}${a.boat !== undefined ? ` boat=${a.boat} shore=${a.shore}` : ''}${a.horse !== undefined ? ` horse=${a.horse}` : ''}${supplied ? ` cart=${a.cart} track=${a.track} exit=${a.exit}` : ''}${a.return ? ' return=true' : ''}`
    api.report({ travelMode: plan.selected ?? 'unavailable', estimatedSeconds: plan.seconds, resume })
    if (a.plan) return { ...plan, resume }
    if (!plan.selected) throw new Error(plan.options.find(option => option.mode === (a.mode === 'auto' || !a.mode ? 'walk' : a.mode))?.reason ?? 'no supported travel mode')
    await api.checkpoint()
    if (plan.selected === 'rail') {
      await walk(api, { ...rail.from, range: 2, dig: false })
      await api.checkpoint()
      // Runtime checks the complete route again after the approach and owns
      // cancellation while aboard. A failed ride never blindly dismounts.
      await act(api, 'rail_ride', { id: a.cart, track: a.track, exit: a.exit })
      await api.checkpoint()
    }
    if (plan.selected === 'horse') {
      await walk(api, { ...horse.from, range: 2, dig: false })
      await api.checkpoint()
      await act(api, 'ride', { id: a.horse, ...to })
      await api.checkpoint()
      await act(api, 'horse_dismount', { id: a.horse })
      await api.checkpoint()
    }
    if (plan.selected === 'boat') {
      if (!boat.mounted) {
        await walk(api, { ...boat.boarding, range: 0, dig: false })
        await api.checkpoint()
      }
      await act(api, 'boat_drive', { id: a.boat, ...boat.to })
      await api.checkpoint()
      const landing = await act(api, 'boat_land', { id: a.boat, ...boat.shore })
      if (landing.serverConfirmed !== true) throw new Error('boat landing lacks server confirmation; inspect state before walking')
      await api.checkpoint()
    }
    await walk(api, { ...to, range: 1, dig: false })
    return { arrived: true, mode: plan.selected, at: api.pos(), plan, resume }
  }
}
