// Animals and monsters (help sections creature and pen).
import { pitAdvice, leadPick, penStance, stanceNote, outOfSight, herdOrder, ledReport, tagalongs, ledExtra, isBaby, deepestCell, unpenned, leadVerdict, creatureFood, CREATURE_FOOD, breedingFood, BREEDING_FOOD, chaseVerdict, attackRefusal, nextSheep, leashable, leashPlan, leashedLine } from '../../lib.mjs'
import { fetchFailure, stalledSince, fencedRefusal, wedgedIn, wedgedRefusal } from '../../fetch.mjs'
import { matcher, vecOf, sweepDrops, goNear, leashHolderOf, onMyLeads, leadsCarried, leashCandidate, leashOne, unleashOne, leadWalk } from '../helpers.mjs'
import { lastPath, lastFrozen, equipBestWeapon, blindGateAdvice, penAround, censusOf } from '../../bot.mjs'
import { goals, Vec3, bot, cancelGuard, pos } from '../state.mjs'
import { makeMoves } from './move.mjs'

export let leading = false // animals are following me: doors and gates stay open behind me until they have caught up
export let following = [] // the animals a lead is bringing along: a gate stays open until they are through it (herdPassed), then shuts at once
export const setLeading = v => { leading = v }
export const setFollowing = v => { following = v }
export let luring = false // a lead is on, from its first step towards the animal: the food stays in my hand, gates or no gates
export let feeding = false // feed is holding food out to an animal: it stays in my hand
export const setFeeding = v => { feeding = v }

export const creatureLong = {
  // farming in one call: every ripe crop within `within` blocks is dug and replanted with its own seed, then the drops are picked up.
  // Works inside protected zones on purpose: crops are there to be harvested, and what it breaks it replants.
  // wool without killing: shears on up to `count` sheep nearby, then picks the wool up
  async shear (a) {
    const shears = bot.inventory.items().find(i => i.name === 'shears')
    if (!shears) throw new Error('no shears: craft item=shears (2 iron ingots)')
    const shorn = new Set()
    const alive = cancelGuard()
    // the sheep's colour byte (metadata 18 on this protocol, 17 on older ones; seen with `entity name=sheep`): bit 0x10 means already shorn
    const bare = e => [17, 18].some(i => (e.metadata?.[i] ?? 0) & 0x10)
    const sheepInSight = () => Object.values(bot.entities).filter(e => e.name === 'sheep' && !bare(e)).map(e => ({ id: e.id, entity: e, dist: e.position.distanceTo(bot.entity.position) }))
    while (shorn.size < (a.count ?? 4)) {
      alive()
      const sheep = nextSheep(sheepInSight(), shorn, a.within ?? 40)
      if (!sheep) break
      await bot.pathfinder.goto(new goals.GoalFollow(sheep.entity, 2))
      await bot.equip(shears, 'hand')
      await bot.useOn(sheep.entity)
      shorn.add(sheep.id)
      await bot.waitForTicks(10)
    }
    if (!shorn.size) throw new Error(`no sheep with wool within ${a.within ?? 40} blocks`)
    await sweepDrops(8)
    return { tried: shorn.size }
  },

  // give one animal the food it breeds on: walk to it, hold the food out, put it away again
  async feed (a) {
    if (!CREATURE_FOOD[a.mob]) throw new Error(`cannot feed ${a.mob}: one of ${Object.keys(CREATURE_FOOD).join(', ')}`)
    const foodName = creatureFood(a.mob, bot.inventory.items().map(i => i.name))
    if (!foodName) throw new Error(`a ${a.mob} eats ${CREATURE_FOOD[a.mob].join(' or ')}: you carry none`)
    const near = e => e.position.distanceTo(bot.entity.position)
    const animal = a.id === undefined
      ? Object.values(bot.entities).filter(e => e.name === a.mob && !isBaby(e.metadata)).sort((x, y) => near(x) - near(y))[0]
      : bot.entities[a.id]
    if (!animal?.isValid) throw new Error(a.id === undefined ? `no grown ${a.mob} about` : `the ${a.mob} with id ${a.id} is gone (despawned, unloaded or already led off)`)
    const carried = () => bot.inventory.items().filter(i => i.name === foodName).reduce((n, i) => n + i.count, 0)
    const before = carried()
    feeding = true
    try {
      await bot.pathfinder.goto(new goals.GoalFollow(animal, 2))
      const food = bot.inventory.items().find(i => i.name === foodName)
      if (!food) throw new Error(`the ${foodName} is gone from my hands`)
      await bot.equip(food, 'hand')
      await bot.useOn(animal)
      await bot.waitForTicks(10)
    } finally {
      feeding = false
      // food left in my hand walks the herd out through the gate at my heels (Vivenna lost a cow that way two mornings running)
      await bot.unequip('hand').catch(() => {})
    }
    // the game takes the food only from a grown one that is ready: `fed` says whether this one really ate
    return { fed: before - carried(), with: foodName, id: animal.id }
  },

  // walk animals to a spot with their food in my hand: they follow from 10 blocks and are slower than I am, so stop for stragglers.
  // flock.lead decides where this goes, shuts a gate that stands open there and counts the pen afterwards; this is the walk itself
  async escort (a) {
    // on leads when carried (card 43a32481): pulled after me, the animals need see no food and a gate only has to open
    if (leashable(a.mob) && (leadsCarried() > 0 || ['horse', 'donkey', 'mule'].includes(a.mob) && onMyLeads().some(e => e.name === a.mob))) return leadWalk(a)
    const foodName = breedingFood(a.mob, bot.inventory.items().map(i => i.name))
    if (!BREEDING_FOOD[a.mob]) throw new Error(`cannot lead ${a.mob}: one of ${Object.keys(BREEDING_FOOD).join(', ')}`)
    if (!foodName) throw new Error(`a ${a.mob} follows ${BREEDING_FOOD[a.mob].join(' or ')}: you carry none`)
    const to = a
    if (to.x === undefined || to.y === undefined || to.z === undefined) throw new Error('escort needs x= y= z= (flock.lead takes place= too)')
    const near = e => e.position.distanceTo(bot.entity.position)
    // leading INTO a pen: the ones already in it stay where they are (the nearest cow was the one in the pen, 09-19)
    const pen = penAround(new Vec3(to.x, to.y, to.z).floored())
    const floor = pen?.enclosed ? pen.floor : null
    // The food in my hand is visible to every animal of its kind that can see me, not only to the ones I pick, so a
    // lead for two can walk a queue of six in and `with=2` says nothing about the other four (Perrin, item 17). Who
    // was standing at the goal BEFORE the walk has to be read before the walk - and only counts when the goal was in
    // sight then: from far enough off the pen's own animals are not loaded yet, and counting those as followers would
    // be a lie told confidently
    const toVec = new Vec3(to.x, to.y, to.z)
    const atGoal = e => floor ? unpenned(floor, [e], x => x.position).length === 0 : e.position.distanceTo(toVec) <= 4
    const standingThere = () => Object.values(bot.entities).filter(e => e.name === a.mob && e.isValid && atGoal(e)).map(e => e.id)
    const alreadyThere = bot.blockAt(toVec) ? standingThere() : null
    const free = () => unpenned(floor, Object.values(bot.entities).filter(e => e.name === a.mob), e => e.position)
    const inRange = free().filter(e => near(e) <= (a.within ?? 32)).sort((x, y) => near(x) - near(y))
    if (!inRange.length) throw new Error(`no ${a.mob} within ${a.within ?? 32} blocks${floor ? ' (not counting those already in the pen)' : ''}`)
    // one that stands in a pen is somebody's (my lead went to Aviendha's base for her cow). Only the nearest few are checked: a pen check in open country is a long walk
    const candidates = inRange.slice(0, 6).map(e => ({ id: e.id, at: `${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`, penned: Boolean(penAround(e.position.floored())?.enclosed), grown: !isBaby(e.metadata), wedged: wedgedIn(bot.blockAt(e.position.floored()), e.position.y) }))
    // one wedged in a fence post cannot walk (card fc47bf28: two cows floored to the post's own cell): a free one first, and the wedge is the refusal only when nothing else is in range
    const walkable = candidates.filter(c => !c.wedged)
    const picked = leadPick(walkable.length ? walkable : candidates, a.penned === true, a.mob)
    if (picked.error) throw new Error(`no ${a.mob} to lead: ${picked.error}`)
    const first = inRange.find(e => e.id === picked.id)
    const alive = cancelGuard()
    luring = true
    try {
      await bot.equip(bot.inventory.items().find(i => i.name === foodName), 'hand')
      // one that stands in a pen: INTO the pen, to its own cell. Two blocks from it is also a spot outside the fence, and from there I walked off without ever
      // opening the gate (my sheep, with=0 twice: it stood at the shut gate and watched the wheat go)
      const pick = candidates.find(c => c.id === picked.id)
      const wedged = wedgedRefusal({ mob: a.mob, at: pick.at, block: pick.wedged })
      if (wedged) return { arrived: false, with: 0, why: wedged, pos: pos() }
      if (pick.penned) {
        // a pen the body is not in is the end of the lead, said before any walk: a walk into it follows partial paths
        // round the fence until the 12 s stall alarm cancels the task, and the fetch loop below would otherwise walk
        // three times to the nearest cell outside the fence and blame the animal (card fc47bf28)
        const fenced = fencedRefusal({ mob: a.mob, at: pick.at, pen: penAround(first.position.floored()), feet: pos() })
        if (fenced) return { arrived: false, with: 0, why: fenced, pos: pos() }
        await goNear(first.position.floored(), 0).catch(() => {})
      } else await bot.pathfinder.goto(new goals.GoalFollow(first, 2))
      alive()
      // the ones that come along are the ones close to me now, where they can see the food - the GROWN ones first, or a
      // lead for a breeding pair comes home with two calves and a herd that cannot breed (Perrin, from 24 cows)
      const herd = herdOrder(free().filter(e => near(e) <= 8).sort((x, y) => near(x) - near(y))
        .map(e => Object.assign(e, { grown: !isBaby(e.metadata) }))).slice(0, a.count ?? 2)
      const stroll = makeMoves(false)
      stroll.allowSprinting = false
      stroll.allowParkour = false
      bot.pathfinder.setMovements(stroll)
      // 1, not 2: two blocks from a spot inside a pen can be outside its fence
      const goal = new goals.GoalNear(to.x, to.y, to.z, a.range ?? 1)
      let holding = false
      let heldSince = 0
      let walking = false
      let walkingSince = 0
      let fetchesSinceProgress = 0
      let bestToGo = Infinity
      // a frozen walk from here on is one of the fetches: three of them beside a wheat field's fence (card fc47bf28)
      // ended "the cow will not follow" while the body itself had never moved
      const fetchingSince = Date.now()
      following = herd
      leading = true
      while (!goal.isEnd(bot.entity.position.floored())) {
        alive()
        const toGo = bot.entity.position.distanceTo(new Vec3(to.x, to.y, to.z))
        if (toGo < bestToGo - 8) { bestToGo = toGo; fetchesSinceProgress = 0 }
        const noPath = walking && lastPath?.status === 'noPath' && lastPath.at > walkingSince
        const verdict = leadVerdict({ distances: herd.filter(e => e.isValid).map(near), holding, heldFor: holding ? (Date.now() - heldSince) / 1000 : 0, fetchesSinceProgress, noPath })
        if (verdict === 'noway') { const along = herd.filter(e => e.isValid && near(e) <= 5); bot.pathfinder.setGoal(null); return { arrived: false, with: along.length, brought: ledReport(a.mob, along), toGo: Math.round(toGo), why: `no route on foot from here to ${to.x},${to.y},${to.z}. One of: the spot is not free floor to stand on; the gate is in a corner or something stands outside it (pen.check names such gates: blindGates=); a gap, drop or fence somewhere between here and there. The animals are with you: walk the way yourself (goto), fix what blocks it, then lead again`, pos: pos() } }
        if (verdict === 'giveup') { bot.pathfinder.setGoal(null); return { arrived: false, with: 0, why: fetchFailure({ mob: a.mob, frozen: stalledSince(lastFrozen, fetchingSince) }), pos: pos() } }
        if (verdict === 'lost') { bot.pathfinder.setGoal(null); return { arrived: false, with: 0, why: `the ${a.mob} are gone (despawned or unloaded)`, pos: pos() } }
        if (verdict === 'fetch') {
          bot.pathfinder.setGoal(null)
          walking = false
          fetchesSinceProgress++
          const straggler = herd.filter(e => e.isValid).sort((x, y) => near(y) - near(x))[0]
          // a fixed spot, not GoalFollow: a jostling animal makes the pathfinder replan every tick and never take a step
          await goNear(straggler.position.floored(), 2)
        }
        if (verdict === 'hold' && !holding) heldSince = Date.now()
        holding = verdict === 'hold'
        if (holding && walking) { bot.pathfinder.setGoal(null); walking = false }
        if (verdict === 'go' && !walking) { bot.pathfinder.setGoal(goal); walking = true; walkingSince = Date.now() }
        await bot.waitForTicks(5)
      }
      bot.pathfinder.setGoal(null)
      // into a pen: on to the cell furthest from them, or they stop 2.5 blocks behind me, in the gateway, and the gate shuts in their face
      const inPen = e => floor ? unpenned(floor, [e], x => x.position).length === 0 : near(e) <= 4
      const deepest = floor ? new Vec3(...deepestCell(floor, herd.find(e => e.isValid)?.position ?? bot.entity.position)) : null
      if (deepest) await goNear(deepest, 0).catch(() => {})
      // I am the faster one: give them time to catch up before counting who came along
      const waitForHerd = async ms => {
        const until = Date.now() + ms
        while (Date.now() < until && herd.some(e => e.isValid && !inPen(e))) { alive(); await bot.waitForTicks(5) }
      }
      await waitForHerd(deepest ? 8000 : 20000)
      // one that followed me along the OUTSIDE of the fence never finds the gate by itself, and the gate stood open for 20 s while I waited (a sheep of the human's
      // at 11,67,-117): go and get it once, the way back leads it through the gate
      const straggler = deepest && herd.find(e => e.isValid && !inPen(e))
      if (straggler) {
        await goNear(straggler.position.floored(), 2).catch(() => {})
        await goNear(deepest, 0).catch(() => {})
        await waitForHerd(12000)
      }
      // food out of sight, or the whole herd walks out again at my heels
      await bot.unequip('hand')
      const arrivals = herd.filter(e => e.isValid && inPen(e))
      const came = arrivals.length
      leading = false
      // counted BEFORE any walk to a gate: Ganesha's body reported from 170 blocks away, the cow long out of sight.
      // From the cells walked, not by eye: a cow beside the fence counted as inside. The gate we came through may still
      // stand open (the runner shuts it after this), and an open gate makes the whole pen read as open country
      const census = floor ? censusOf(floor) : {}
      const animals = herd.filter(e => e.isValid).map(e => `${a.mob}@${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`).join(' ')
      // one that never came may be unable to: say so, rather than let the driver lead it again and again (Ganesha, 6 times)
      const passable = v => bot.blockAt(v)?.boundingBox !== 'block'
      const riseAt = (feet, dx, dz) => [0, 1, 2, 3].find(up => passable(feet.offset(dx, up, dz)) && passable(feet.offset(dx, up + 1, dz))) ?? Infinity
      const stuck = herd.filter(e => e.isValid && !inPen(e)).map(e => e.position.floored())
        .map(feet => pitAdvice(a.mob, `${feet.x},${feet.y},${feet.z}`, [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => riseAt(feet, dx, dz)))).find(Boolean)
      // `with=3` was true and useless: nothing in it said that two of the three were calves and the pen now holds
      // nothing that can breed. brought= says which, and the note says when a calf came for want of anything else
      // and the ones that came uninvited, which no count of the ones I asked for could show
      const extra = alreadyThere ? ledExtra(a.mob, tagalongs(herd.map(e => e.id), alreadyThere, standingThere())) : {}
      return { arrived: true, with: came, brought: ledReport(a.mob, arrivals), animals, stuck, ...(picked.note ? { note: picked.note } : {}), ...extra, ...census, pos: pos() }
    } finally {
      leading = false
      luring = false
      following = []
      // also when cancelled or given up: food left in my hand drags every animal in sight after me
      await bot.unequip('hand').catch(() => {})
    }
  },

  // leads (card 43a32481): a lead in the hand used on an animal ties it to me, and it is pulled after me from then on
  async leash (a) {
    const named = a.id === undefined ? null : bot.entities[a.id]
    if (a.id !== undefined && !named) throw new Error(`nothing here with id ${a.id}: it is dead, or out of sight. animals gives the ids that are still there`)
    if (!named && !a.mob) throw new Error('leash needs mob= or id=')
    const mob = named ? named.name : a.mob
    const m = matcher(mob)
    const near = e => e.position.distanceTo(bot.entity.position)
    const candidates = (named ? [named] : Object.values(bot.entities).filter(e => e !== bot.entity && e.isValid && m(e.name ?? '') && near(e) <= (a.within ?? 16)))
      .filter(e => !leashHolderOf(e)).map(leashCandidate)
    const plan = leashPlan({ mob, leads: leadsCarried(), count: a.count ?? 1, candidates, allowPenned: named ? true : a.penned === true })
    if (plan.error) throw new Error(plan.error)
    for (const id of plan.take) await leashOne(bot.entities[id])
    return { leashed: leashedLine(onMyLeads().map(e => ({ name: e.name, id: e.id, ...e.position }))), leads: leadsCarried(), ...(plan.note ? { note: plan.note } : {}) }
  },
  // the leads come off: on the animals, each lead drops and is picked up; with a fence post at x= y= z=, every animal
  // on my leads is tied to a knot there and the leads stay on the knot
  async unleash (a) {
    const held = onMyLeads()
    if (!held.length) throw new Error('nothing is on my leads')
    if (a.x !== undefined) {
      const post = bot.blockAt(vecOf(a))
      if (!post || !/_fence$|_wall$/.test(post.name)) throw new Error(`${a.x},${a.y},${a.z} is ${post?.name ?? 'nothing'}, not a fence post or wall to tie a lead to`)
      await goNear(post.position, 2)
      await bot.unequip('hand')
      await bot.activateBlock(post)
      for (let i = 0; i < 20 && onMyLeads().length; i++) await bot.waitForTicks(1)
      const still = onMyLeads()
      if (still.length) throw new Error(`${still.length} of ${held.length} still on my leads after the knot: stand closer to ${a.x},${a.y},${a.z} and try again`)
      return { tied: held.length, at: `${a.x},${a.y},${a.z}`, note: 'the leads stay on the knot; break the knot (attack it, or right-click it empty-handed) to free them and drop the leads' }
    }
    const before = leadsCarried()
    for (const e of held) await unleashOne(e)
    await sweepDrops(8).catch(() => {})
    const lying = before + held.length - leadsCarried()
    return { unleashed: held.length, leads: leadsCarried(), ...(lying > 0 ? { leadsLying: `${lying} lead${lying === 1 ? '' : 's'} dropped and not picked up: collect` } : {}) }
  },

  async attack (a) {
    const m = matcher(a.mob)
    const named = a.id === undefined ? null : bot.entities[a.id]
    // by id (from animals) when the caller means ONE animal and not simply the nearest of its kind: culling the wrong
    // cow, or somebody else's from outside the fence, cannot be undone
    if (a.id !== undefined && !named) throw new Error(`nothing here with id ${a.id}: it is dead, or out of sight. animals gives the ids that are still there`)
    if (named && !m(named.name ?? '')) throw new Error(`id ${a.id} is a ${named.name}, not a ${a.mob}`)
    const target = named ?? Object.values(bot.entities)
      .filter(e => e !== bot.entity && e.type !== 'player' && m(e.name ?? ''))
      .sort((x, y) => x.position.distanceTo(bot.entity.position) - y.position.distanceTo(bot.entity.position))[0]
    if (!target) throw new Error(`no ${a.mob} in sight`)
    // #97: pvp aims at the target's head to swing, and an enderman's head is exactly what must not be aimed at
    const refused = attackRefusal(target.name)
    if (refused) throw new Error(refused)
    await equipBestWeapon()
    const start = bot.entity.position.clone()
    bot.pvp.attack(target)
    const verdict = await new Promise(resolve => {
      const iv = setInterval(() => {
        const v = chaseVerdict({ targetValid: target.isValid, hunting: !!bot.pvp.target, strayed: bot.entity.position.distanceTo(start), leash: a.leash ?? 24 })
        if (!v) return
        clearInterval(iv)
        resolve(v)
      }, 250)
    })
    if (verdict.gaveUp) bot.pvp.stop()
    return verdict
  }
}

export const creatureQuick = {
  // will this pen hold? Walks the way an animal can from a spot inside (default: where I stand) and says where it gets out
  'pen.check' (a) {
    const feet = a.x === undefined ? bot.entity.position.floored() : vecOf(a)
    // a cell in a chunk this body was never sent reads as nothing at all, and nothing at all used to come back as "not
    // a spot to stand on": Perrin's check on a pen 200 blocks off blamed his coordinates for a world I had not seen
    const blind = outOfSight(bot.blockAt(feet), feet, bot.entity.position)
    if (blind) throw new Error(`pen.check: ${blind}`)
    const found = penAround(feet, a.radius)
    if (!found) throw new Error(`${feet.x},${feet.y},${feet.z} is not a spot to stand on: give the x y z of a free floor cell INSIDE the pen (y = where feet would be), or stand in it`)
    // every verdict here is a verdict about ONE cell, and until now the reply never said which (backlog #125)
    const from = `${feet.x},${feet.y},${feet.z}`
    if (!found.enclosed) {
      return {
        pen: 'LEAKS',
        from,
        side: stanceNote(from, penStance({ start: [feet.x, feet.y, feet.z], topsAt: found.topsAt, radius: a.radius ?? 24 })),
        via: found.via,
        advice: 'an animal can walk out: via= is where (x,height,z): one spot = a gap or open gate on level ground; three = the step it climbs, the barrier top it crosses, where it lands. A fence or wall must stand 2 above EVERY block next to it, inside and out, corner to corner included. Fix it and check again'
      }
    }
    const census = { ...censusOf(found.floor), ...blindGateAdvice(found.floor, found.topsAt) }
    if (found.cells >= 16) return { pen: 'holds', from, cells: found.cells, ...census }
    return { pen: 'holds', from, cells: found.cells, ...census, advice: 'but it is small: an animal led in stops 2.5 blocks from you, so under 16 cells it stops in the gateway' }
  }
}
