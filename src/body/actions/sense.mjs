// What I can see from here (help section sense): these only read.
import path from 'node:path'
import { timedScan } from '../../performance.mjs'
import { doingText, isBaby, scanCap, unpenned, scanWhere, routeSummary, CREATURE_FOOD, NEVER_FIGHT, inventorySlots, armorPoints, feetCell, renderScan, describePlaces, compact, isNight, agentNames, splitPlayers, chestFree, eventLines } from '../../lib.mjs'
import { noStanding, thinkBudget, goalDistance, rimGoal } from '../../navigation/walk.mjs'
import { blockName } from '../../navigation/stall.mjs'
import { stuckField } from '../../navigation/stuck.mjs'
import { serverPosNote } from '../../survival/airlog.mjs'
import { horseState } from '../riding.mjs'
import { ROOT, cfg } from '../home.mjs'
import { readPlaces, recent } from '../events.mjs'
import { matcher, inventoryCounts, vecOf, cellAt, findBlockByName, containerAt } from '../helpers.mjs'
import { eyes, openWindow, lastServerPos } from '../../bot.mjs'
import { goals, Vec3, reportPerformance, bot, task, pos, roundVec } from '../state.mjs'
import { codeHere } from '../code-version.mjs'
import { reflexes, isHostile } from '../reflexes.mjs'
import { stuckNow } from '../stuck-watch.mjs'
import { surfaceWalkRuntime, boatRuntime } from '../runtimes.mjs'
import { followTarget, jobShelf } from '../jobs.mjs'
import { firstMoveNote } from '../explain.mjs'
import { penAround } from '../pens.mjs'
import { long } from './tables.mjs'
import { makeMoves } from './move.mjs'

export const senseLong = {
  async chest_contents (a) {
    const chest = await bot.openContainer(await containerAt(a))
    const items = {}
    for (const i of chest.containerItems()) items[i.name] = (items[i.name] || 0) + i.count
    // free= and slots= say the chest's room up front, so a deposit plan can choose a chest before walking there
    const free = chestFree(chest.slots, chest.inventoryStart)
    chest.close()
    return { items, free, slots: chest.inventoryStart }
  }
}

export const senseQuick = {
  // debugging aid: what the pathfinder makes of a walk from here, without walking it. stroll=true: with lead's movements (no sprint, no parkour)
  path_to: (a) => {
    if (a.surface !== undefined) return surfaceWalkRuntime.preview(a)
    const fresh = makeMoves(a.dig === true)
    // live=true: plan with the movements the walks really use, and name every setting where they differ from a fresh set
    const moves = a.live ? bot.pathfinder.movements : fresh
    const differs = a.live ? Object.keys(fresh).filter(k => ['number', 'boolean', 'string'].includes(typeof fresh[k]) && fresh[k] !== moves[k]).map(k => `${k}:${moves[k]}`).join(' ') : ''
    if (a.stroll) { moves.allowSprinting = false; moves.allowParkour = false }
    // the same judgement a walk makes before it searches: a goal on the floor of a pit walks to its rim; range 0 at a ground block, or a cell walled in by crops, is a refusal, not a 5 s timeout
    const rim = rimGoal(cellAt, a, a.range ?? 0, { into: a.into === true, from: feetCell(bot.entity.position, bot.entity.onGround) })
    const aim = rim ?? { x: a.x, y: a.y, z: a.z, range: a.range ?? 0 }
    const nowhere = noStanding(cellAt, aim, aim.range)
    if (nowhere) return { status: 'refused', ms: 0, why: nowhere }
    const began = Date.now()
    const budget = thinkBudget(goalDistance(aim, bot.entity.position))
    let r = bot.pathfinder.getPathTo(moves, new goals.GoalNear(aim.x, aim.y, aim.z, aim.range), budget)
    // one call searches for a single 40 ms slice: go on the way a walk does, until it is done or the time a walk this long gets is over
    while (r.status === 'partial' && r.context && Date.now() - began < budget) r = Object.assign(r.context.compute(), { context: r.context })
    const last = r.path[r.path.length - 1]
    reportPerformance('path_to', Date.now() - began, { status: r.status, visited: r.visitedNodes, generated: r.generatedNodes, goal: aim, budget_ms: budget })
    const stuckHere = r.status === 'noPath' && r.visitedNodes <= 1 ? firstMoveNote(moves) : null
    return { status: r.status, ms: Date.now() - began, nodes: r.path.length, cost: Math.round(r.cost), visited: r.visitedNodes, ends: last ? `${last.x},${last.y},${last.z}` : 'here', ...(stuckHere && { why: stuckHere }), ...(rim && { note: rim.note }), gates: r.path.filter(n => n.toPlace?.some(t => t.useOne)).length, ...(a.route ? routeSummary(r.path) : {}), ...(a.live ? { differs: differs || 'nothing' } : {}) }
  },
  // debugging aid: the raw metadata of the nearest entities with this name (how does the server mark a shorn sheep?)
  entity: (a) => ({
    found: Object.values(bot.entities).filter(e => e !== bot.entity && matcher(a.name)(e.name ?? '') && (a.hostile !== true || isHostile(e)))
      .sort((x, y) => x.position.distanceTo(bot.entity.position) - y.position.distanceTo(bot.entity.position)).slice(0, a.count ?? 2)
      .map(e => ({ id: e.id, name: e.name, width: e.width, height: e.height, hostile: isHostile(e), ...(a.uuid ? { uuid: e.uuid, vehicleId: boatRuntime.currentVehicleId(e) } : {}), ...(['villager', 'cow', 'sheep', 'pig'].includes(e.name) ? { baby: isBaby(e.metadata), adult: !isBaby(e.metadata) } : {}), dist: Math.round(e.position.distanceTo(bot.entity.position)), at: e.position.floored().toArray().join(','), exact: e.position.toArray().map(n => Math.round(n * 100) / 100).join(','), metadata: JSON.stringify(e.metadata), attributes: e.attributes, equipment: (e.equipment ?? []).flatMap((item, slot) => item ? [{ slot, name: item.name, count: item.count }] : []) }))
  }),
  state () {
    const others = Object.values(bot.players).filter(p => p.username !== bot.username)
    const playersSeen = Object.fromEntries(others.map(p => [p.username, p.entity ? roundVec(p.entity.position) : 'out of sight']))
    const { humans } = splitPlayers(playersSeen, agentNames(path.join(ROOT, 'state')))
    const chattiness = cfg.chat?.chattiness ?? 1
    return {
      hp: Math.round(bot.health),
      food: bot.food,
      xp: bot.experience.level,
      oxygen: bot.oxygenLevel,
      inWater: bot.entity.isInWater,
      exact: bot.entity.position.toArray().map(n => Math.round(n * 100) / 100).join(','),
      // where the server last put the body, when that is off the client's position (card 962beec2)
      ...serverPosNote({ client: bot.entity.position, server: lastServerPos, now: Date.now() }),
      time: `${isNight(bot.time.timeOfDay) ? 'night' : 'day'} ${bot.time.timeOfDay}`,
      pos: pos(),
      dimension: bot.game.dimension === 'overworld' ? null : bot.game.dimension,
      raining: bot.isRaining,
      holding: bot.heldItem?.name,
      asleep: bot.isSleeping,
      doing: task && doingText({ name: task.name, seconds: Math.round((Date.now() - task.started) / 1000), paused: task.paused }),
      queued: jobShelf.list().queued.map(id => { const j = jobShelf.get(id); return j ? `${j.name} (${j.id})` : String(id) }).join(', ') || undefined,
      queueHeld: jobShelf.snapshot().held?.reason,
      stuck: stuckField(stuckNow),
      following: followTarget,
      reflexesOff: !reflexes,
      // which code this is, so `am I running the fix?` is answered by the line every driver already reads (#140)
      code: codeHere.code,
      dirty: codeHere.dirty,
      players: playersSeen,
      // only the humans some body can currently see right now (not agent bodies, and not 'out of sight' ones)
      humans: humans.filter(name => playersSeen[name] !== 'out of sight').join(','),
      // only shown below 1 (today's behaviour, unfiltered): card 2e032c4a
      ...(chattiness < 1 && { chattiness })
    }
  },

  look_around (a) {
    const range = a.range ?? 32
    const me = bot.entity.position
    const groups = {}
    for (const e of Object.values(bot.entities)) {
      if (e === bot.entity || !e.position) continue
      const d = e.position.distanceTo(me)
      if (d > range) continue
      const name = e.type === 'player' ? `player:${e.username}` : (e.name ?? e.type)
      const g = groups[name] ??= { count: 0, nearest: Infinity }
      g.count++
      if (d < g.nearest) { g.nearest = Math.round(d); g.at = roundVec(e.position) }
    }
    const interesting = a.blocks ?? ['*_ore', '*_log', 'chest', 'barrel', 'crafting_table', 'furnace', '*_bed', 'water', 'lava', 'spawner', '*_door', 'farmland']
    const blocks = {}
    for (const pattern of interesting) {
      const found = findBlockByName(pattern, a.blockRange ?? 24, 64)
      for (const p of found) {
        const name = bot.blockAt(p).name
        const b = blocks[name] ??= { count: 0, nearest: p, dist: Infinity }
        b.count++
        const d = p.distanceTo(me)
        if (d < b.dist) { b.dist = Math.round(d); b.nearest = p }
      }
    }
    const below = bot.blockAt(me.offset(0, -1, 0))
    const line = (count, dist, at) => `${count}x ${dist}m @${compact(at)}`
    const nearestBlocks = Object.entries(blocks).sort((p, q) => p[1].dist - q[1].dist).slice(0, a.limit ?? 10)
    // mob=cow: where every one of them is (a herd count does not say who is outside the fence)
    const each = a.mob && Object.values(bot.entities).filter(e => e.name === a.mob && e.position.distanceTo(me) <= range)
      .sort((p, q) => p.position.distanceTo(me) - q.position.distanceTo(me)).slice(0, 24).map(e => compact(roundVec(e.position))).join(' ')
    if (a.mob) return { pos: pos(), mob: a.mob, each: each || 'none in range' }
    return {
      pos: pos(),
      on: below?.name,
      entities: Object.fromEntries(Object.entries(groups).map(([n, g]) => [n, line(g.count, g.nearest, g.at)])),
      blocks: Object.fromEntries(nearestBlocks.map(([n, b]) => [n, line(b.count, b.dist, b.nearest)])),
      places: describePlaces(readPlaces(), me, { limit: 5, maxDist: 64, notes: false }),
      // #97: the one entity in this list you must not aim at. It is named here because the count alone reads like any other mob
      ...(Object.keys(groups).some(n => NEVER_FIGHT.has(n)) ? { careful: `${Object.keys(groups).filter(n => NEVER_FIGHT.has(n)).join(' and ')} in sight: do not attack or aim at one, my body loses that fight in seconds. Keep a block between you and walk away` } : {})
    }
  },

  inventory () {
    const slot = n => bot.inventory.slots[bot.getEquipmentDestSlot(n)]?.name ?? null
    return {
      items: inventoryCounts(),
      freeSlots: bot.inventory.emptySlotCount(),
      armor: { head: slot('head'), torso: slot('torso'), legs: slot('legs'), feet: slot('feet'), offhand: slot('off-hand') }
    }
  },

  // the dashboard's: what the player's screen shows. Every stack by slot would be dozens of tokens a driver pays for on
  // each ./mc inventory, so they live here and not there
  screen () {
    return {
      hp: Math.round(bot.health),
      food: bot.food,
      xp: bot.experience.level,
      oxygen: bot.oxygenLevel,
      armor: armorPoints(bot.entity.attributes),
      ...inventorySlots(bot.inventory.slots, bot.quickBarSlot),
      window: openWindow()
    }
  },

  // the farm animals about me, one line each: a driver's eye cannot tell a lamb from a sheep, nor which side of a fence one stands on
  animals (a) {
    const me = bot.entity.position
    // which pen counts as "in": the one around me, or the one around a cell I name, so a pen can be counted from outside it
    const pen = penAround(a.x === undefined ? me.floored() : vecOf(a))
    const floor = pen?.enclosed ? pen.floor : null
    const wanted = a.mob ? matcher(a.mob) : () => true
    const near = e => e.position.distanceTo(me)
    return {
      found: Object.values(bot.entities)
        .filter(e => e.name && (CREATURE_FOOD[e.name] || ['horse', 'donkey', 'mule'].includes(e.name)) && wanted(e.name) && near(e) <= (a.within ?? 24))
        .sort((x, y) => near(x) - near(y))
        .map(e => ({
          mob: e.name,
          id: e.id,
          at: `${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`,
          dist: Math.round(near(e)),
          grown: ['horse', 'donkey', 'mule'].includes(e.name) ? horseState(bot, e)?.baby === false : !isBaby(e.metadata),
          inMyPen: Boolean(floor) && unpenned(floor, [e], x => x.position).length === 0
        }))
    }
  },

  // x= y= z= anchors the search on a cell instead of on me: a place's hives are the same list from wherever I stand
  find_blocks (a) {
    const point = a.x === undefined ? bot.entity.position : vecOf(a)
    const positions = timedScan(reportPerformance, 'find_blocks', () => findBlockByName(a.block, a.maxDistance ?? 64, a.count ?? 10, point),
      { block: a.block, range: a.maxDistance ?? 64, count: a.count ?? 10, at: { x: point.x, y: point.y, z: point.z } })
    return { positions }
  },

  // one item in one chest, for a flow's goal (routine until=): walks to the chest when it is out of reach
  async chest_count (a) {
    const { items } = await long.chest_contents(a)
    return { item: a.item, count: items[a.item] ?? 0, chest: `${a.x},${a.y},${a.z}` }
  },

  block_at (a) {
    const b = bot.blockAt(vecOf(a))
    return b ? { name: blockName(b), properties: b.getProperties?.() } : { name: null }
  },

  // render what the bot sees to a PNG (see eyes.mjs): look | look pano=true | look dir=north | look x= y= z=
  look (a) { return eyes(a) },

  // ASCII slices of the box between two corners; one call instead of hundreds of block_at round trips
  scan (a) {
    const volume = ['x', 'y', 'z'].reduce((n, k) => n * (Math.abs(a[k + '2'] - a[k + '1']) + 1), 1)
    if (!(volume <= scanCap(a.where))) throw new Error(`scan needs x1,y1,z1,x2,y2,z2 spanning at most ${scanCap(a.where)} blocks (got ${volume})${a.where ? '' : '; to find one kind of block in a bigger box add where=<name>, for a wider view use look'}`)
    const nameAt = (x, y, z) => blockName(bot.blockAt(new Vec3(x, y, z)))
    // where= answers with coordinates only: the picture is the dear part, and whoever asks where wants to act, not to look
    return a.where ? { where: scanWhere(nameAt, a, a.where) } : { map: renderScan(nameAt, a) }
  },
  async look_at (a) { await bot.lookAt(new Vec3(a.x, a.y, a.z)); return {} },
  // recent history without reading the log: events [type=chat] [last=20] [all=true] (the last 500, earlier runs included)
  events (a) {
    return { text: eventLines(recent, { type: a.type, last: a.last, all: a.all === true }).join('\n') || 'nothing yet' }
  }
}
