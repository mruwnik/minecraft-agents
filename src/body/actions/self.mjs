// My own body (help section self).
import { namedBed } from '../../lib/sleep.mjs'
import { isGreeting } from '../../chatter.mjs'
import { chatText, bedExit, eatFailure, uneatenMeal, eatRefusal, eatAllowed, bedChoice, ownBed, nightPlan, automaticNightPlan, BED_RANGE, within, lateMeal } from '../../lib.mjs'
import { addressedTo, whisperHint, offlineWhisper, splitSay, sayLimit, heardWhisper } from '../../talk.mjs'
import { cfg } from '../home.mjs'
import { zones, readPlaces, emit } from '../events.mjs'
import { inventoryCounts, inventoryQuiet, goNear, bedsNear } from '../helpers.mjs'
import { carriedFood, eatOnce, nearbyHostiles, onlinePlayers, automaticSleepBeds, carriedBed, carriedBedPlace, placeReflexBed, lastDriven, setLastDriven, bedWalkFailed, setBedWalkFailed, leaveBed, bedExits } from '../../bot.mjs'
import { bot, cancelGuard, pos } from '../state.mjs'

export const selfLong = {
  async sleep (a) {
    if (bot.vehicle) throw new Error('confirm a safe dismount before walking to a bed')
    const alive = cancelGuard()
    // a taken bed is passed over for the next one I may use (a shared bedroom: "the bed is occupied" was the end of the night)
    const occupied = new Set()
    // bed=<place|x,y,z>: that bed and no other, walked to when it is within bed_range (a routine's bed= arrives here
    // from every step's night checkpoint, and the nearest bed was an old one a creeper waited by)
    if (a.bed && !a.automatic) {
      const target = ownBed(readPlaces(), cfg.username, { bed: a.bed })
      if (!target) throw new Error(`no place called ${a.bed} on the shared map`)
      const plan = nightPlan({ near: false, bed: target, from: pos(), bedRange: a.bed_range ?? BED_RANGE })
      if (plan.do !== 'walk') throw new Error(plan.why)
      await goNear(target, 2)
      const p = namedBed(bedsNear(), target)
      if (!p) throw new Error(`no bed at ${target.name}`)
      await bot.sleep(bot.blockAt(p))
      return { trap: bedExit(bedExits(p)) ?? undefined }
    }
    // no bed within 32 is not the end of the night when one of my own is on the shared map within bed_range (default 200,
    // card bebf3a5f): walk there once (my nearest kind=bed mark; src/lib/sleep.mjs ownBed) and look again
    let walked = false
    let placed = false
    for (;;) {
      const { bed: p, error } = bedChoice(a.automatic ? automaticSleepBeds() : bedsNear(), zones, cfg.username, a.any === true && !a.automatic, occupied)
      if (error && a.automatic && !placed) {
        // walked there already and bedChoice still has nothing for me: that walk counted as failed, so tonight goes straight to placement
        if (walked) setBedWalkFailed(true)
        const from = pos()
        const plan = automaticNightPlan({
          near: false, bed: ownBed(readPlaces(), cfg.username, { from }), from,
          carried: Boolean(carriedBed()) && Boolean(carriedBedPlace()), walkFailed: bedWalkFailed || a.walk === false, hostileNear: nearbyHostiles(8).length > 0
        })
        if (plan.do === 'walk') {
          walked = true
          try {
            await goNear(plan.to, 2)
          } catch (e) {
            setBedWalkFailed(true)
            alive()
            if (/goal was changed|path was stopped/i.test(e.message)) throw e
          }
          continue
        }
        if (plan.do === 'place') {
          const item = carriedBed()
          const spot = item && nearbyHostiles(8).length === 0 ? carriedBedPlace() : null
          if (spot) {
            alive()
            placed = true
            await placeReflexBed(item.name, spot)
            continue
          }
        }
        throw new Error(plan.why ? `${error} (${plan.why})` : error)
      }
      if (error && !a.automatic && !walked && /^no bed within 32/.test(error)) {
        const from = pos()
        const plan = nightPlan({ near: false, bed: ownBed(readPlaces(), cfg.username, { from }), from, bedRange: a.bed_range ?? BED_RANGE })
        if (plan.do !== 'walk') throw new Error(`${error} (${plan.why})`)
        walked = true
        await goNear(plan.to, 2)
        continue
      }
      if (error) throw new Error(error)
      await goNear(p, 2)
      const taken = await bot.sleep(bot.blockAt(p)).then(() => false, e => { if (!/occupied/.test(e.message)) throw e; return true })
      if (!taken) return { trap: bedExit(bedExits(p)) ?? undefined }
      occupied.add(`${p.x},${p.y},${p.z}`)
    }
  }
}

export const selfQuick = {
  // stop this body for good (logging off for the night, or done playing): answers first, then leaves the server and exits
  quit: () => {
    emit('quit', {})
    setTimeout(() => { bot.quit('quit'); process.exit(0) }, 200)
    return { note: 'body stopped: ./start brings it back' }
  },

  // chat goes to everyone: a message that opens with an online player's name still goes, with a hint to whisper next time (src/talk.mjs)
  // a long text goes out in numbered pieces under the server's line limit instead of being cut off (src/talk.mjs)
  chat (a) {
    const said = chatText(a, Infinity)
    if (said.error) throw new Error(said.error)
    const chattiness = cfg.chat?.chattiness ?? 1
    if (chattiness < 0.2 && isGreeting(said.text)) throw new Error(`chattiness ${chattiness}: greetings and acks are not sent; whisper if it matters`)
    const parts = splitSay(said.text, sayLimit())
    for (const part of parts) bot.chat(part)
    const to = addressedTo(said.text, onlinePlayers())
    return { ...(parts.length > 1 && { parts: parts.length }), ...(to && { hint: whisperHint(to) }) }
  },
  // a whisper to someone offline is /tell into the void: the server's "No player was found" never reaches the driver
  whisper (a) {
    const said = chatText(a, Infinity)
    if (said.error) throw new Error(said.error)
    const offline = offlineWhisper(a.player, onlinePlayers())
    if (offline) throw new Error(offline)
    const parts = splitSay(said.text, sayLimit(a.player))
    for (const part of parts) bot.whisper(a.player, part)
    return parts.length > 1 ? { parts: parts.length } : {}
  },
  // the dashboard's: a line typed into an agent's popup, recorded as the whisper it stands for (bot.on('whisper') above)
  hear (a) {
    const said = heardWhisper(a)
    setLastDriven(Date.now())
    emit('whisper', said)
    return {}
  },
  // The reflex should beat you to this (see eat_failed when it cannot), and a body that will not eat has to be drivable
  // by hand. It is also the only way to read what mineflayer-auto-eat really answers: its own reflex swallowed every word.
  async eat (a) {
    const carried = carriedFood()
    const refusal = eatRefusal({ food: bot.food, item: a.item, carried, anyway: a.anyway })
    if (refusal) throw new Error(refusal)
    const { allowed } = eatAllowed({ food: bot.food, carried, anyway: a.anyway })
    const edible = bot.inventory.items().filter(i => allowed.includes(i.name))
    const before = bot.food
    const countsBefore = inventoryCounts()
    // sanitizeOpts writes its choice back into this object, so an eat with no item= still says what it ate. With
    // nothing on the ordinary list the choice is made here instead: the plugin would refuse what the floor allowed.
    const pick = a.item ? edible.find(i => i.name === a.item) : (carried.edible.length ? null : edible[0])
    const opts = pick ? { food: pick } : {}
    // with strictErrors off a failed meal resolves and emits eatFail instead of throwing: catch both, or `ate` would lie
    const attempt = async () => {
      let failure = null
      const onFail = error => { failure ??= error }
      bot.autoEat.on('eatFail', onFail)
      try {
        await eatOnce(opts)
      } catch (error) {
        failure ??= error
      } finally {
        bot.autoEat.off('eatFail', onFail)
      }
      return failure
    }
    // a meal "never showed" right after a craft: the plugin asked for bread from a slot the server had just moved, and
    // the second eat worked (card c13b704d). Once the pockets have settled, judge it again and try once more
    const pocket = () => inventoryCounts()[opts.food?.name] ?? 0
    const judge = (failure, retried) => lateMeal({ failure, before: { food: before, carried: countsBefore[opts.food?.name] ?? 0 }, after: { food: bot.food, carried: pocket() }, retried })
    let failure = await attempt()
    let late = null
    if (failure) { await inventoryQuiet(); late = judge(failure, false) }
    if (late === 'retry') {
      failure = await attempt()
      late = failure ? (await inventoryQuiet(), judge(failure, true)) : 'retried'
    }
    if (failure && late !== 'ate') throw new Error(eatFailure(failure, carriedFood().edible))
    await bot.waitForTicks(5) // the food number comes in the update_health after the meal, not with it
    const eaten = opts.food?.name ?? null
    const uneaten = late === 'ate' ? null : uneatenMeal({ item: eaten, before: countsBefore[eaten] ?? 0, after: inventoryCounts()[eaten] ?? 0 })
    if (uneaten) throw new Error(uneaten)
    const note = late === 'ate' ? 'the meal showed once the pockets settled' : late === 'retried' ? 'the first try asked for a slot the server had just moved; the second ate' : null
    return { ate: eaten, gained: bot.food - before, food: bot.food, health: Math.round(bot.health), ...(note ? { note } : {}) }
  },

  async wake () {
    if (!bot.isSleeping) throw new Error('already awake')
    const woke = new Promise(resolve => bot.once('wake', resolve))
    leaveBed()
    await within(3000, woke, 'waking up')
    return {}
  }
}
