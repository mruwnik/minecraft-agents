import { foodReceiptStore } from '../../src/villager/food-receipt.mjs'
import { habitatOwnership, habitatThreats, habitatSecure, closeHabitatGates } from '../../src/villager/habitat.mjs'
import { BREED_FOOD, breedPlan, breedPreflight, breedCensus, breedBill, breedKey, breedFeedStance, breedFeedGrounded, breedInside } from '../../src/villager/breed.mjs'

export default {
  doc: 'villager.breed target= x= y= z= [size= block=cobblestone gate=oak_fence_gate bed=white_bed food=bread timeout= plan=true]: verify an existing lit roofed habitat and provision shared food until observed population reaches target; prepare with blueprint.check/build first; x/y/z is the interior foot anchor',
  stops: 'target total UUID population is observed inside, or resources, enclosure, pickup evidence or births cannot be confirmed',
  args: { target: 'number!', x: 'number!', y: 'number!', z: 'number!', size: 'number', entryX: 'number', entryZ: 'number', airlock: 'boolean', block: 'string', gate: 'string', bed: 'string', food: 'string', timeout: 'number', plan: 'boolean' },
  async run (api, a) {
    const started = Date.now()
    const plan = breedPlan(a)
    const material = a.block ?? 'cobblestone'; const gateItem = a.gate ?? 'oak_fence_gate'
    const bedItem = a.bed ?? 'white_bed'; const food = a.food ?? 'bread'
    const safeMobs = () => habitatThreats(api, plan)
    const observe = async () => {
      const found = (await api.act('entity', { name: 'villager', uuid: true, count: 1000 })).found ?? []
      if (found.length >= 1000) throw new Error('villager census reached its observation limit; population cannot be confirmed')
      return breedCensus(plan, found)
    }
    await safeMobs()
    let villagers = await observe()
    const initial = villagers.length
    const summary = () => ({ target: a.target, population: villagers.length, adults: villagers.filter(e => !e.baby).length, babies: villagers.filter(e => e.baby).length, uuids: villagers.map(e => e.uuid) })
    if (initial > a.target) throw new Error(`target ${a.target} is below the ${initial} observed residents; no villagers will be removed; use target=${initial} or higher to prepare their shelter`)
    const known = new Set(villagers.map(e => e.uuid))
    const newborns = []
    const timeout = a.timeout ?? Math.min(14400, 900 * Math.max(1, a.target - initial))
    if (!Number.isFinite(timeout) || timeout < 30 || timeout > 14400) throw new Error('timeout= must be 30..14400 seconds for the complete breeding run')
    const preflight = breedPreflight(plan, api.block, material, gateItem)
    const receiptStore = foodReceiptStore(api, plan, food)
    let receipt = receiptStore.load()
    const birthsNeeded = a.target - initial
    const extras = food === 'bread' ? 4 : 16
    const observedBirthsWhilePaused = receipt ? villagers.filter(e => e.baby && !receipt.before.includes(e.uuid)).length : 0
    const foodCredit = Math.max(Object.values(receipt?.held ?? {}).reduce((sum, n) => sum + n, 0), (receipt?.total ?? 0) - observedBirthsWhilePaused * 2 * BREED_FOOD[food])
    const habitatReady = !preflight.needed.length && !preflight.missingBeds.length && !preflight.clear.length
    const { bill, shortages } = breedBill({ needed: [], missingBeds: [] }, api.inv(), birthsNeeded, food, bedItem, foodCredit, extras)
    habitatOwnership(api, plan, preflight)
    const adults = villagers.filter(e => !e.baby)
    if (a.plan === true) return { ...summary(), plan: true, size: plan.width, bill, shortages, habitatReady, missingBeds: preflight.missingBeds.length, missingStructure: preflight.needed.map(breedKey), clears: preflight.clear.map(breedKey), ready: habitatReady && !shortages.length && (initial === a.target || adults.length >= 2), needsAdults: initial === a.target ? 0 : Math.max(0, 2 - adults.length), beds: plan.beds.map(b => breedKey(b.foot)) }
    if (plan.airlock && initial > 0 && !breedInside(plan, api.pos())) throw new Error('start breeding inside the main sleeping room; boat.receive leaves the operator there. Enter through the sealed annex before running, keeping the outer gate closed before opening the inner gate')
    if (initial < a.target && adults.length < 2) throw new Error(`breeder has ${adults.length} observed on-foot adults; bring two inside the planned ${plan.width}x${plan.width} footprint before running`)
    if (preflight.needed.length || preflight.missingBeds.length || preflight.clear.length) throw new Error('habitat is not complete for the requested population; use blueprint.check and blueprint.build (villager-house-10 supports up to ten beds), then rerun breeding')
    if (shortages.length) throw new Error(`breeder supplies short: ${shortages.join('; ')}`)
    const closeGate = (cleanup = false) => closeHabitatGates(api, plan, gateItem, cleanup)
    const refresh = async () => {
      if ((Date.now() - started) / 1000 >= timeout) throw new Error(`breeding run timed out at ${villagers.length}/${a.target}; inspect before resuming`)
      await safeMobs()
      villagers = await observe()
      for (const uuid of known) if (!villagers.some(e => e.uuid === uuid)) throw new Error(`known villager ${uuid} is no longer observed inside; stopped without replacement feeding`)
      for (const e of villagers) {
        if (!known.has(e.uuid) && e.baby) newborns.push(e.uuid)
        known.add(e.uuid)
      }
      api.report(summary())
      return villagers
    }
    try {
      await closeGate()
      habitatSecure(api, plan, material, gateItem)
      await refresh()
      const secure = () => habitatSecure(api, plan, material, gateItem)
      let rounds = 0
      if (receipt && observedBirthsWhilePaused) {
        receipt.total = foodCredit
        receipt.before = villagers.map(e => e.uuid)
        await receiptStore.save(receipt)
      }
      while (villagers.length < a.target) {
        await api.checkpoint()
        secure()
        await refresh()
        if (villagers.length >= a.target) break
        const adultUuids = villagers.filter(e => !e.baby).map(e => e.uuid)
        if (adultUuids.length < 2) throw new Error('two observed adults are required for the next birth')
        receipt ??= { food, pair: adultUuids.slice(0, 2), before: villagers.map(e => e.uuid), credits: {}, total: 0, held: {} }
        let wanted = (a.target - villagers.length) * 2 * BREED_FOOD[food] + extras
        while (receipt.total < wanted) {
          secure()
          await refresh()
          if (villagers.length >= a.target) break
          wanted = (a.target - villagers.length) * 2 * BREED_FOOD[food] + extras
          if (receipt.total >= wanted) break
          let target, stance
          await api.until(async () => {
            secure()
            await refresh()
            if (villagers.length >= a.target) return true
            if (!api.clock().day) return false
            for (const adult of villagers.filter(e => !e.baby && !e.sleeping && breedFeedGrounded(plan, e.position, api.block))) {
              try { stance = breedFeedStance(plan, adult.position, api.block); target = adult; return true } catch {}
            }
            return false
          }, { timeout: timeout - (Date.now() - started) / 1000, every: 2, what: 'waiting for an awake adult on the solid room floor before shared food provisioning' })
          if (villagers.length >= a.target) break
          await api.act('goto', { ...stance, range: 0, into: true })
          await refresh()
          if (villagers.length >= a.target) break
          const count = Math.min(64, wanted - receipt.total)
          let result
          for (let attempt = 0, selfReturns = 0; attempt < 20; attempt++) {
            secure()
            await refresh()
            const current = villagers.find(e => e.uuid === target.uuid && !e.baby && !e.sleeping)
            if (!api.clock().day || !current || !breedFeedGrounded(plan, current.position, api.block)) throw new Error('target must remain awake on the room floor before shared food is offered')
            receipt.pending = { uuid: target.uuid, count }
            await receiptStore.save(receipt)
            result = await api.act('villager_food', { uuid: target.uuid, item: food, count })
            const returned = result.ambiguous !== true && result.selfCollected === count && result.returnedToInventory === true && result.totalCollected === count
            const notTossed = result.notTossed === true && result.tossed === 0 && result.inventoryUnchanged === true
            if (!returned && !notTossed) break
            delete receipt.pending
            await receiptStore.save(receipt)
            if (returned) {
              api.report({ foodSelfReturn: result, selfReturnAttempt: ++selfReturns })
              if (selfReturns >= 3) throw new Error(`shared offering returned to the bot three times; no adult credit added; evidence=${JSON.stringify(result)}`)
            }
            await api.pause(2)
            result = null
          }
          if (!result) throw new Error('shared offering repeatedly deferred without tossing; inspect before resuming')
          await refresh()
          const collected = result.collectedBy ?? (result.collectorUuid && result.pickedUp === count ? { [result.collectorUuid]: count } : {})
          const entries = Object.entries(collected)
          if (result.ambiguous === true || result.tossed !== undefined && result.tossed !== count || entries.reduce((sum, [, n]) => sum + n, 0) !== count || entries.some(([uuid, n]) => !villagers.some(e => e.uuid === uuid && known.has(uuid)) || !Number.isInteger(n) || n <= 0)) throw new Error(`shared food pickup was not confirmed by known residents; evidence=${JSON.stringify(result)}; no further food will be dropped`)
          delete receipt.pending
          receipt.total += count
          receipt.held ??= {}
          for (const [uuid, n] of entries) if (villagers.find(e => e.uuid === uuid)?.baby) receipt.held[uuid] = (receipt.held[uuid] ?? 0) + n
          await receiptStore.save(receipt)
          api.report({ householdFoodDelivered: receipt.total, foodHeldByBabies: Object.values(receipt.held ?? {}).reduce((sum, n) => sum + n, 0), foodForRemainingBirths: wanted, collectedBy: collected })
        }
        if (villagers.length >= a.target) break
        rounds++
        const before = new Set(receipt.before)
        await api.until(async () => {
          secure()
          await refresh()
          receipt.held ??= {}
          let matured = false
          for (const uuid of Object.keys(receipt.held)) if (villagers.some(e => e.uuid === uuid && !e.baby)) { delete receipt.held[uuid]; matured = true }
          if (matured) await receiptStore.save(receipt)
          const held = Object.values(receipt.held).reduce((sum, n) => sum + n, 0)
          api.report({ householdFoodCredit: receipt.total, foodHeldByBabies: held, adultFoodCredit: receipt.total - held })
          return villagers.some(e => e.baby && !before.has(e.uuid))
        }, { timeout: timeout - (Date.now() - started) / 1000, every: 2, what: 'no new baby observed after shared food provisioning; inspect beds, cooldown and mobGriefing; no repeated feeding without an observed birth' })
        const births = villagers.filter(e => e.baby && !before.has(e.uuid)).length
        receipt.total = Math.max(Object.values(receipt.held ?? {}).reduce((sum, n) => sum + n, 0), receipt.total - births * 2 * BREED_FOOD[food])
        receipt.before = villagers.map(e => e.uuid)
        await receiptStore.save(receipt)
      }
      secure()
      await refresh()
      if (villagers.length < a.target) throw new Error('population changed before final confirmation')
      if (initial < a.target && !newborns.length) throw new Error('population increased without an observed new baby UUID; breeding was not confirmed')
      return { ...summary(), reached: true, secure: true, validBeds: plan.target, rounds, newborns }
    } catch (error) {
      try { await closeGate(true) } catch (closeError) { throw new Error(`${error.message}; gate closure pending at ${breedKey(plan.gate)}: ${closeError.message}`) }
      throw error
    }
  }
}
