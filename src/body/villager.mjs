// Villager trading and attributed household food delivery runtime.
// Live body dependencies are injected, keeping client lifecycle in bot.mjs.
import { isBaby, within, offerCost, tradeLine } from '../lib.mjs'
import { compatibleInventoryStacks } from '../inventory-compact.mjs'

export function makeVillagerRuntime (deps) {
  const { Vec3, goNear, findItem, inventoryCounts, cancelGuard, emit,
    getFeeding, setFeeding, currentVehicleId } = deps
  let bot
  const sync = () => { bot = deps.getBot() }
  const PROFESSIONS = 'unemployed armorer butcher cartographer cleric farmer fisherman fletcher leatherworker librarian mason nitwit shepherd toolsmith weaponsmith'.split(' ')
  function villagerData (entity) {
    const raw = entity.metadata?.[19] ?? entity.metadata?.[18]
    const profession = typeof raw?.profession === 'string' ? raw.profession.replace(/^minecraft:/, '') : PROFESSIONS[raw?.villagerProfession ?? raw?.profession ?? raw?.[1] ?? 0] ?? 'unknown'
    return { profession, level: raw?.level ?? raw?.[2] ?? 1, adult: !isBaby(entity.metadata), nitwit: profession === 'nitwit' }
  }
  function targetVillager (a) {
    const target = a.x === undefined ? bot.entity.position : new Vec3(a.x + 0.5, a.y, a.z + 0.5)
    const nearby = Object.values(bot.entities).filter(e => e.name === 'villager' && e.position.distanceTo(target) <= (a.id === undefined ? 6 : 8))
    const entity = a.id !== undefined ? nearby.find(e => e.id === a.id) : nearby.sort((x, y) => x.position.distanceTo(target) - y.position.distanceTo(target))[0]
    if (!entity) throw new Error(`no villager within 6 blocks${a.id === undefined ? '' : ` with id ${a.id}`}`)
    return entity
  }
  function villagerEnchants (item) {
    const list = item?.enchants
    if (Array.isArray(list) && list.length) return list
    const raw = item?.componentMap?.get('stored_enchantments')?.data
    const values = Array.isArray(raw) ? raw : raw?.enchantments ?? raw?.levels ?? []
    return values.map(e => ({ name: e.name ?? bot.registry.enchantments?.[e.id]?.name ?? String(e.id), lvl: e.lvl ?? e.level }))
  }
  let lastVillagerOffers = null
  let villagerWindowUncertain = false
  let tradeOutcomeUncertain = false
  let tradeInFlight = null
  async function openVillagerWindow (entity, timeoutMessage) {
    if (villagerWindowUncertain) throw new Error('a villager window timed out; restart the body before opening another')
    const opening = bot.openVillager(entity)
    try { return await within(5000, opening, timeoutMessage) } catch (error) {
      if (error.message === `${timeoutMessage} took longer than 5s`) {
        villagerWindowUncertain = true
        // Mineflayer removes its trade-list listener when this window closes. A late result is also closed.
        const closed = new Set()
        const closeOnce = window => { if (window && !closed.has(window)) { closed.add(window); window.close() } }
        opening.then(window => { try { closeOnce(window) } catch (e) { console.log(`[villager window cleanup] ${e.message}`) } }, () => {})
        const window = bot.currentWindow
        if (window && /villager|merchant/.test(window.type)) closeOnce(window)
      }
      throw error
    }
  }
  const long = {
    async trades (a) {
      const entity = targetVillager(a)
      const data = villagerData(entity)
      if (data.profession === 'unemployed' || data.profession === 'nitwit' || !data.adult) return { ...data, id: entity.id, offers: [], text: data.adult ? data.profession : 'baby' }
      await goNear(entity.position, 2.5)
      if (!bot.entities[entity.id] || bot.entity.position.distanceTo(entity.position) > 6) throw new Error('the villager walked off before the window opened')
      const window = await openVillagerWindow(entity, 'villager window did not open in 5 s')
      try {
        const offers = window.trades.map((o, n) => ({ index: n + 1, inputItem1: o.inputItem1 && { name: o.inputItem1.name, count: o.realPrice ?? o.inputItem1.count }, inputItem2: o.inputItem2 && { name: o.inputItem2.name, count: o.inputItem2.count }, outputItem: o.outputItem && { name: o.outputItem.name, count: o.outputItem.count, enchants: villagerEnchants(o.outputItem) }, nbTradeUses: o.nbTradeUses, maximumNbTradeUses: o.maximumNbTradeUses, tradeDisabled: o.tradeDisabled }))
        lastVillagerOffers = { id: entity.id, offers, at: Date.now() }
        return { ...villagerData(entity), id: entity.id, offers, text: [`profession=${data.profession} level=${data.level} offers=${offers.length}`, ...offers.map((o, n) => tradeLine(o, n + 1))].join('\n') }
      } finally { window.close() }
    },

    async trade (a) {
      if (!(a.offer >= 1 && Number.isInteger(a.offer)) || !(a.times === undefined || (Number.isInteger(a.times) && a.times >= 1))) throw new Error('trade needs offer=1,2,... and positive times=')
      if (tradeOutcomeUncertain || tradeInFlight) throw new Error('a prior trade outcome is uncertain; inspect inventory and offers, then restart this body before another purchase')
      const entity = targetVillager(a)
      const cached = lastVillagerOffers?.id === entity.id && Date.now() - lastVillagerOffers.at < 5000 ? lastVillagerOffers.offers[a.offer - 1] : null
      if (cached) {
        const needs = Object.fromEntries(Object.entries(offerCost(cached)).map(([name, n]) => [name, n * (a.times ?? 1)]))
        const held = inventoryCounts()
        if (Object.entries(needs).some(([name, n]) => (held[name] ?? 0) < n)) throw new Error(`trade needs ${Object.entries(needs).map(([name, n]) => `${n} ${name}`).join(' and ')}: carrying ${Object.entries(needs).map(([name]) => `${held[name] ?? 0} ${name}`).join(', ')}`)
      }
      await goNear(entity.position, 2.5)
      const window = await openVillagerWindow(entity, 'villager window did not open in 5 s')
      let originalOpen = true
      const closeOriginal = () => { if (originalOpen) { originalOpen = false; window.close() } }
      try {
        const offer = window.trades[a.offer - 1]
        if (!offer) throw new Error(`villager has ${window.trades.length} offers, no offer ${a.offer}`)
        if (offer.tradeDisabled) throw new Error(`offer ${a.offer} is disabled`)
        const times = a.times ?? 1
        const needs = Object.fromEntries(Object.entries(offerCost(offer)).map(([name, n]) => [name, n * times]))
        const before = inventoryCounts()
        const short = Object.entries(needs).filter(([name, n]) => (before[name] ?? 0) < n)
        if (short.length) throw new Error(`trade needs ${Object.entries(needs).map(([name, n]) => `${n} ${name}`).join(' and ')}: carrying ${short.map(([name]) => `${before[name] ?? 0} ${name}`).join(', ')}`)
        if (offer.maximumNbTradeUses - offer.nbTradeUses < times) throw new Error(`offer ${a.offer} has only ${offer.maximumNbTradeUses - offer.nbTradeUses} uses left`)
        const output = offer.outputItem
        const usedBefore = offer.nbTradeUses
        tradeOutcomeUncertain = true
        const purchase = bot.trade(window, a.offer - 1, times)
        tradeInFlight = purchase
        purchase.then(() => { if (tradeInFlight === purchase) tradeInFlight = null }, () => { if (tradeInFlight === purchase) tradeInFlight = null })
        await within(10000, purchase, 'trade outcome uncertain after 10 s; inspect inventory and offer uses before retrying')
        // Mineflayer keeps the changed slots in the merchant window until close copies
        // them back to bot.inventory. Reading inventory before close reports a false miss.
        closeOriginal()
        await bot.waitForTicks(5)
        const after = inventoryCounts()
        const bought = (after[output.name] ?? 0) - (before[output.name] ?? 0)
        const paid = Object.fromEntries(Object.keys(needs).map(name => [name, (before[name] ?? 0) - (after[name] ?? 0)]))
        if (bought < output.count * times || Object.entries(needs).some(([name, n]) => paid[name] < n)) throw new Error(`trade click was not confirmed by inventory: bought=${bought} paid=${JSON.stringify(paid)}`)
        // Offer uses may reset on a workstation restock immediately after purchase.
        // The exact item exchange confirms this purchase; the caller may reopen
        // once to verify the villager's profession and wanted book.
        tradeOutcomeUncertain = false
        return { bought: `${output.name}:${bought}`, paid, uses: null, usedBefore }
      } finally { closeOriginal() }
    },
  }
  const quick = {
    async villager_food (a) {
      if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(a.uuid ?? '')) throw new Error('villager_food needs an exact villager uuid=')
      if (a.otherUuid !== undefined && (a.otherUuid === a.uuid || !/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(a.otherUuid))) throw new Error('villager_food otherUuid must name a distinct exact parent')
      if (!['bread', 'carrot', 'potato', 'beetroot'].includes(a.item) || !Number.isInteger(a.count) || a.count < 1 || a.count > 64) throw new Error('villager_food needs bread/carrot/potato/beetroot and count=1..64')
      const villager = Object.values(bot.entities).find(e => e.uuid === a.uuid && e.name === 'villager' && e.isValid !== false)
      if (!villager || !villagerData(villager).adult || currentVehicleId(villager) !== null) throw new Error('villager_food needs the exact visible adult on foot')
      const alive = cancelGuard()
      const dryFooting = () => bot.entity.onGround && !bot.entity.isInWater && bot.blockAt(bot.entity.position.offset(0, -0.1, 0))?.boundingBox === 'block'
      for (let tick = 0; tick < 20 && !dryFooting(); tick++) { alive(); await bot.waitForTicks(1) }
      alive()
      const me = bot.entity.position.clone()
      if (me.distanceTo(villager.position) > 3) throw new Error('villager_food needs the bot within three blocks of the target; approach from a dry stance first')
      if (!dryFooting()) throw new Error('villager_food needs solid dry footing')
      const before = inventoryCounts()[a.item] ?? 0
      if (before < a.count) throw new Error(`villager_food needs ${a.count} carried ${a.item}; have ${before}`)
      if (bot.autoEat?.isEating) throw new Error('villager_food must wait for the bot to finish its own meal')
      if (Object.values(bot.entities).some(e => {
        if (e.name !== 'item' || e.position.distanceTo(me) >= 3) return false
        const item = e.getDroppedItem?.()
        return !item || item.name === a.item
      })) throw new Error('villager_food needs nearby matching or unidentified food drops cleared first so pickup can be attributed')
      bot.pathfinder.setGoal(null)
      await bot.lookAt(villager.position.offset(0, 1.5, 0), true)
      // Forced look changes local angles; physics sends the server rotation on its next tick.
      await bot.waitForTicks(2)
      alive()
      const obstruction = bot.blockAtCursor(3)
      if (obstruction && obstruction.position.offset(0.5, 0.5, 0.5).distanceTo(me.offset(0, bot.entity.eyeHeight, 0)) < me.distanceTo(villager.position) - 0.5) throw new Error(`villager_food ray is blocked by ${obstruction.name}`)
      const spawned = new Set(), drops = new Set()
      const dropTrace = new Map()
      const aim = { from: me.toArray(), target: villager.position.toArray(), yaw: bot.entity.yaw, pitch: bot.entity.pitch }
      let pickedUp = 0, collectorUuid = null, observedDropCount = 0, ambiguous = false, wrongCollector = false, totalCollected = 0, selfCollected = 0
      const collectedBy = {}
      const spawn = e => {
        if (e.name === 'item' && Math.hypot(e.position.x - me.x, e.position.z - me.z) <= 0.75 && e.position.y >= me.y + 0.8 && e.position.y <= me.y + 1.9) { spawned.add(e.id); dropTrace.set(e.id, { id: e.id, spawn: e.position.toArray(), velocity: e.velocity?.toArray() }) }
      }
      const itemDrop = e => {
        if (!spawned.has(e.id) || drops.has(e.id)) return
        const item = e.getDroppedItem?.()
        if (item?.name !== a.item || !Number.isInteger(item.count) || item.count < 1 || item.count > a.count) return
        observedDropCount += item.count
        if (observedDropCount > a.count) { ambiguous = true; return }
        drops.add(e.id)
      }
      const collect = packet => {
        if (!drops.has(packet.collectedEntityId)) return
        const collector = packet.collectorEntityId === bot.entity.id ? bot.entity : bot.entities[packet.collectorEntityId]
        collectorUuid = collector?.uuid ?? (packet.collectorEntityId === bot.entity.id ? bot._client.uuid : null) ?? null
        const count = packet.pickupItemCount ?? 0
        const trace = dropTrace.get(packet.collectedEntityId)
        if (trace) { trace.collectedAt = bot.entities[packet.collectedEntityId]?.position?.toArray(); trace.collectorId = packet.collectorEntityId; trace.collectorUuid = collectorUuid; trace.count = count }
        if (packet.collectorEntityId === bot.entity.id) selfCollected += count
        totalCollected += count
        if (collectorUuid) collectedBy[collectorUuid] = (collectedBy[collectorUuid] ?? 0) + count
        else ambiguous = true
        if (totalCollected > a.count) ambiguous = true
        if (collectorUuid === a.uuid) pickedUp += count
        else wrongCollector = true
      }
      const wasFeeding = getFeeding()
      const priorHeld = bot.heldItem
      setFeeding(true)
      bot.on('entitySpawn', spawn)
      bot.on('itemDrop', itemDrop)
      bot._client.on('collect', collect)
      try {
        alive()
        const item = findItem(a.item)
        await bot.equip(item, 'hand')
        if (a.otherUuid !== undefined) {
          const freshTarget = Object.values(bot.entities).find(e => e.uuid === a.uuid && e.name === 'villager' && e.isValid !== false)
          const other = Object.values(bot.entities).find(e => e.uuid === a.otherUuid && e.name === 'villager' && e.isValid !== false)
          const here = bot.entity.position.clone()
          if (!freshTarget || !other || !villagerData(freshTarget).adult || !villagerData(other).adult || currentVehicleId(freshTarget) !== null || currentVehicleId(other) !== null || freshTarget.metadata?.[6] === 2 || other.metadata?.[6] === 2 || here.distanceTo(freshTarget.position) > 3 || here.distanceTo(other.position) < Math.max(4, here.distanceTo(freshTarget.position) + 0.8)) return { uuid: a.uuid, item: a.item, count: a.count, tossed: 0, notTossed: true, inventoryUnchanged: (inventoryCounts()[a.item] ?? 0) === before, error: 'parents moved or crowded before offering' }
          await bot.lookAt(freshTarget.position.offset(0, 1.5, 0), true)
          await bot.waitForTicks(2)
          alive()
          const now = bot.entity.position.clone()
          Object.assign(aim, { from: now.toArray(), target: freshTarget.position.toArray(), other: other.position.toArray(), yaw: bot.entity.yaw, pitch: bot.entity.pitch })
          const targetSupport = bot.blockAt(freshTarget.position.offset(0, -0.1, 0))
          if (Math.abs(freshTarget.position.y - Math.round(freshTarget.position.y)) > 0.05 || targetSupport?.boundingBox !== 'block' || /(?:bed|slab|stairs|fence|wall|trapdoor)$/.test(targetSupport.name)) return { uuid: a.uuid, item: a.item, count: a.count, tossed: 0, notTossed: true, inventoryUnchanged: (inventoryCounts()[a.item] ?? 0) === before, error: 'target must step off partial bed footing before offering' }
          if (now.distanceTo(freshTarget.position) < 1.5 || now.distanceTo(freshTarget.position) > 2.25 || freshTarget.metadata?.[6] === 2 || other.metadata?.[6] === 2 || now.distanceTo(other.position) < Math.max(4, now.distanceTo(freshTarget.position) + 0.8)) return { uuid: a.uuid, item: a.item, count: a.count, tossed: 0, notTossed: true, inventoryUnchanged: (inventoryCounts()[a.item] ?? 0) === before, error: 'parents moved or crowded before offering' }
          const hit = bot.blockAtCursor(3)
          if (hit && hit.position.offset(0.5, 0.5, 0.5).distanceTo(now.offset(0, bot.entity.eyeHeight, 0)) < now.distanceTo(freshTarget.position) - 0.5) return { uuid: a.uuid, item: a.item, count: a.count, tossed: 0, notTossed: true, inventoryUnchanged: (inventoryCounts()[a.item] ?? 0) === before, error: 'target ray became blocked before offering' }
        }
        await bot.toss(item.type, null, a.count)
        await bot.waitForTicks(3)
        const tossed = before - (inventoryCounts()[a.item] ?? 0)
        if (tossed !== a.count) throw new Error(`villager_food inventory delta was ${tossed}, expected ${a.count}; inspect before retrying`)
        for (let tick = 0; tick < 200 && totalCollected < a.count && !ambiguous; tick++) { alive(); await bot.waitForTicks(1) }
        const returnedToInventory = !ambiguous && selfCollected === a.count && inventoryCounts()[a.item] === before
        const receipt = { uuid: a.uuid, item: a.item, count: a.count, tossed, collectorUuid, collectedBy, totalCollected, ambiguous, selfCollected, returnedToInventory, aim, drops: [...drops].map(id => dropTrace.get(id)), pickedUp: ambiguous ? 0 : Math.min(pickedUp, a.count), confirmed: !ambiguous && !wrongCollector && pickedUp === a.count, ...(ambiguous || wrongCollector ? { error: ambiguous ? 'ambiguous matching item drops; inspect before feeding again' : 'another entity collected the offered food' } : {}) }
        emit('villager_food_receipt', receipt)
        return receipt
      } finally {
        bot.off('entitySpawn', spawn)
        bot.off('itemDrop', itemDrop)
        bot._client.off('collect', collect)
        setFeeding(wasFeeding)
        if (priorHeld) { try { alive(); const restore = bot.inventory.items().find(i => compatibleInventoryStacks(i, priorHeld)); if (restore) await bot.equip(restore, 'hand') } catch {} }
      }
    },
  }
  const bind = table => Object.fromEntries(Object.entries(table).map(([name, fn]) => [name, a => { sync(); return fn(a) }]))
  return {
    long: bind(long), quick: bind(quick),
    invalidateOffers: () => { lastVillagerOffers = null }
  }
}
