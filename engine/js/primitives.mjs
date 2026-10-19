// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// The mineflayer layer of the engine: a small set of time-bounded operations, each a cut point. The contract is in
// engine/README.md (Primitives). Nothing here imports from src/.

import './compile-cache.mjs'
import { readFileSync } from 'node:fs'
import { join } from 'node:path'
import { connectBot } from './connect.mjs'
import { createGameClock } from './game-clock.mjs'
import { createView } from './view.mjs'
import { createRawWorld } from './raw-world.mjs'
import { ensureAwake } from './bed.mjs'
import { createUseOn } from './use-on.mjs'
import { createSteer } from './steer.mjs'
import { trackLeashes } from './leash.mjs'
import { mobFields } from './interact.mjs'
import { trackLiveEntities, liveEntities } from './live-entities.mjs'
import { trackVehicles } from './vehicle.mjs'
import { avoidEnderStare } from './gaze.mjs'
import { traceSteps } from './step-trace.mjs'
import { missingPatches, missingRequired } from './deps-check.mjs'
import { wrapBlockAt } from './offset-shapes.mjs'
import { say } from './chat.mjs'
import vec3 from 'vec3'
import { BODY_HALF_WIDTH, EYE_HEIGHT, sleepMs, waitForWorld, xyz, isNum, vec, cutError, badArgs, isCut, failed, need, attempt, sleepStatusOf, spawnSetMessage, droppedItem, itemCounts } from './prim-base.mjs'
import { createWalk } from './prim-walk.mjs'
import { createSense } from './prim-sense.mjs'
import { createMove } from './prim-move.mjs'
import { createDig } from './prim-dig.mjs'
import { createItems } from './prim-items.mjs'
import { setting, useSettings } from './settings.mjs'

const { Vec3 } = vec3

export { cutError, mcToMineflayerLook, mineflayerToMcLook, sleepStatusOf, waitForWorld } from './prim-base.mjs'

// Builds the primitives over an already spawned bot. `timeScale` multiplies every time bound (tests shrink it).
// `reconnect` (internal; createPrimitives passes it) makes a fresh spawned bot with the same connection params and
// enables `offline`; without it `offline` is unsupported.
export function createPrimitivesFromBot (initialBot, { timeScale = 1, reconnect = null, view = null, worldTimeoutMs = setting('prim/world-timeout-ms'), settleMs = setting('prim/settle-ms'), pending = [] } = {}) {
  let bot = initialBot
  trackLeashes(bot)
  trackLiveEntities(bot)
  trackVehicles(bot)
  view?.attach(bot)
  let closed = false
  let owner = null
  const inflight = new Set()
  const listeners = new Set()
  const entityDeathListeners = new Set()
  // Offline is body state: set from the moment `offline` quits the bot until the fresh bot is adopted (or the
  // reconnect gave up); resolves then. Sensing answers 'offline' meanwhile and acting calls wait for it.
  let away = null
  // Down: the connection dropped unplanned (kick, socket end, server restart) and no fresh bot is adopted yet. The body
  // reconnects by itself meanwhile (autoReconnect); sensing and acting answer 'offline'.
  let down = false
  const isOffline = () => away !== null || down

  const isOwner = token => token !== null && token !== undefined && token === owner
  const setOwner = token => {
    owner = token ?? null
    for (const call of [...inflight]) if (call.token !== owner) call.cut()
  }

  const here = () => xyz(bot.entity.position)

  // a pick-up shortly after, and near, the body's own dig, use or chest transfer is its own work, not found goods
  let ownWork = null
  const noteOwnWork = () => { ownWork = { at: Date.now(), pos: here() } }
  const ownWorkNear = () => {
    if (!ownWork || Date.now() - ownWork.at > OWN_WORK_MS) return false
    const p = here()
    return Math.hypot(p.x - ownWork.pos.x, p.y - ownWork.pos.y, p.z - ownWork.pos.z) <= OWN_WORK_BLOCKS
  }
  // the cell the pathfinder plans from: one up when standing on a block lower than a cube (farmland), as it offsets
  const standingCell = () => {
    const p = bot.entity.position
    const floored = vec(p).floored()
    const low = p.y - floored.y > 0.001 && bot.entity.onGround && bot.blockAt(floored)?.boundingBox === 'block'
    return low ? floored.offset(0, 1, 0) : floored
  }
  // the eyes: Mineflayer's height is the top of the head (1.8), eyeHeight the eyes (1.62), as the server reads reach and air
  const eye = () => ({ x: bot.entity.position.x, y: bot.entity.position.y + (bot.entity.eyeHeight ?? EYE_HEIGHT), z: bot.entity.position.z })
  const inventory = () => bot.inventory.items()
  // Worn and off-hand stacks are not in inventory().items(); they sit in these slots of the player window.
  const WORN_SLOTS = { head: 5, torso: 6, legs: 7, feet: 8, offHand: 45 }
  const gearView = item => {
    if (!item) return null
    const max = bot.registry?.itemsByName?.[item.name]?.maxDurability
    const enchants = (item.enchants ?? []).map(e => ({ name: e.name, level: e.lvl ?? e.level }))
    return { name: item.name, count: item.count, ...(max > 0 && { durability: max - (item.durabilityUsed ?? 0) }), ...(enchants.length > 0 && { enchants }) }
  }
  const equipment = () => ({
    ...Object.fromEntries(Object.entries(WORN_SLOTS).map(([part, slot]) => [part, gearView(bot.inventory.slots[slot])])),
    mainHand: gearView(bot.heldItem)
  })
  const countsNow = () => itemCounts(inventory())

  // Runs `body(ctx)` as one call owned by `token`. The call ends the moment the owner changes (rejects with cut), or
  // when its time bound passes (resolves onTimeout(), default {status: 'timeout'} plus `inventoryChange` {item: delta}
  // when the stacks changed meanwhile, so a half-done transfer, craft or trade is not read as nothing done); both run the aborts the body
  // registered, and `ctx.alive()` then throws so the body stops reaching the bot. Any other throw from the body (a
  // mineflayer rejection) resolves {status: 'failed', reason}. Entry with a stale token throws.
  const inventoryDelta = (before, after) => Object.fromEntries([...new Set([...Object.keys(before), ...Object.keys(after)])]
    .map(name => [name, (after[name] ?? 0) - (before[name] ?? 0)]).filter(([, d]) => d !== 0))
  const act = (token, { boundS, onTimeout }, body) => {
    if (!isOwner(token)) return Promise.reject(cutError())
    const startCounts = onTimeout ? null : countsNow()
    const timedOut = onTimeout ?? (() => {
      const inventoryChange = inventoryDelta(startCounts, countsNow())
      return Object.keys(inventoryChange).length > 0 ? { status: 'timeout', inventoryChange } : { status: 'timeout' }
    })
    return new Promise((resolve, reject) => {
      const aborts = []
      let settled = false
      const runAborts = () => aborts.splice(0).reverse().forEach(fn => { try { Promise.resolve(fn()).catch(() => {}) } catch { /* the bot may already be gone */ } })
      const settle = then => value => {
        if (settled) return
        settled = true
        clearTimeout(timer)
        inflight.delete(call)
        then(value)
      }
      const call = { token, cut: () => { if (settled) return; runAborts(); settle(reject)(cutError()) } }
      const ctx = {
        alive: () => { if (settled || !isOwner(token)) throw cutError() },
        onAbort: fn => aborts.push(fn)
      }
      const timer = setTimeout(() => { if (settled) return; runAborts(); settle(resolve)(timedOut()) }, Math.max(1, boundS * 1000 * timeScale))
      inflight.add(call)
      Promise.resolve().then(() => body(ctx)).then(settle(resolve), err => settled ? undefined : (isCut(err) ? settle(reject)(err) : settle(resolve)(failed(err))))
    })
  }

  const env = { get bot () { return bot }, timeScale, settleMs, act, isOwner, here, eye, inventory, standingCell, countsNow, isOffline, equipment }
  Object.assign(env, createWalk(env), createSense(env))
  const { stopWalking, lookNow } = env
  const { self, worldAge, entities, blockAt, isSettling, settleFromNow, rememberSelf, lastKnown, columnLoaded } = env
  Object.assign(env, createMove(env), createDig(env), createItems(env))
  const { moveTo, swim, jumpPlace, attack, sleep, look, mount, dismount, paddle, dig, place, collect, digTime, clearTime, harvestTools } = env
  const { inspectContainer, transfer, equip, toss, craft, furnace, enchant, eat, fish, interact, trade, unequip } = env

  // the message arrives validated and cleaned by engine.chat (gate!, direct!); say asserts the last line
  const chat = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    return act(token, { boundS: 3 }, ctx => say(bot, ctx, a, { timeScale }))
  }

  // Control-adjacent chat is independent of the body ownership token: sending a
  // message must not cut a running job.
  // It is deliberately online-only, so requests are never deferred until reconnect.
  const chatDirect = async (a = {}) => {
    if (closed || away || down || !bot.player) return { status: 'disconnected' }
    const cleanups = []
    const ctx = {
      alive: () => { if (closed || away || down || !bot.player) throw new Error('chat body disconnected') },
      onAbort: fn => cleanups.push(fn)
    }
    try { return await say(bot, ctx, a, { timeScale }) }
    finally { cleanups.splice(0).reverse().forEach(fn => { try { fn() } catch {} }) }
  }


  // Waits `ms` (clamped to 0..WAIT_MAX_MS, scaled by timeScale) without touching the bot; a cut rejects at once.
  const wait = async (token, a = {}) => {
    const ms = Math.min(Math.max(isNum(a.ms) ? a.ms : 0, 0), setting('prim/wait-max-ms'))
    // the act's own time bound is the wait: it resolves ok when the bound passes, and a cut rejects before that
    return act(token, { boundS: ms / 1000, onTimeout: () => ({ status: 'ok' }) }, () => new Promise(() => {}))
  }

  // ---- body events ----

  const emit = event => listeners.forEach(fn => fn(event))
  const inventoryNow = () => inventory().map(i => ({ name: i.name, count: i.count, slot: i.slot }))

  const relayEntityDeath = (entity, removal) => entityDeathListeners.forEach(fn => fn({ entity, source: bot, dimension: bot.game?.dimension, removal }))
  // item entities that leave the world (despawn, pickup) are forgotten by the entity cache too
  const relayEntityGone = entity => relayEntityDeath(entity, 'gone')
  const relayCollected = (_collector, collected) => relayEntityDeath(collected, 'collect')
  const relayDead = entity => relayEntityDeath(entity)
  const removalEvents = [['entityDead', relayDead], ['entityGone', relayEntityGone], ['playerCollect', relayCollected]]

  // Wires one bot's events to the listeners; returns the function that unwires them.
  let lastTick = Date.now() // of the current bot's physics; the watchdog below reads it
  let stalled = false
  const bindEvents = target => {
    if (target.physics) target.physics.playerHalfWidth = BODY_HALF_WIDTH
    wrapBlockAt(target) // physics collides with bamboo/dripstone where the server has them, not at minecraft-data's fixed box
    const unguard = avoidEnderStare(target)
    const untrace = traceSteps(target, process.env.MC_STEP_TRACE)
    lastTick = Date.now()
    stalled = false
    settleFromNow()
    let lastPos = target.entity.position.clone()
    let lastHealth = target.health
    let respawning = false
    // The server clears the slots of an instant death (/kill, void, damage) before the death event is read, so the
    // died record is built from the last snapshot of a living body: taken on every health event above zero and about
    // once a second of physics ticks. Lava and suffocation deaths keep their slots, the live inventory is used then.
    let snapshot = inventoryNow()
    let ticks = 0
    // What the body stands in, as jobs.lib.cost.value/lethal-cause? names it. The death message of the server is not used:
    // it arrives in a later packet than the death event; the lethal hit's damage packet names the rest (killerNow).
    const causeNow = () => {
      const pos = target.entity.position
      if (pos.y < -64) return { cause: 'void' }
      const name = target.blockAt(new Vec3(Math.floor(pos.x), Math.floor(pos.y), Math.floor(pos.z)))?.name
      if (name === 'lava') return { cause: 'lava' }
      return /^(soul_)?fire$/.test(name ?? '') ? { cause: 'fire' } : {}
    }
    const remember = () => { if (target.health > 0) snapshot = inventoryNow() }
    // The damage packets to this body since the last health packet: the damage type's name (target.damageTypeNames
    // comes from the login registry) and the entity responsible, as a client can know them. The server sends one
    // health packet per tick, so a blast and a hit in one tick are one loss; the next health loss reports all of them
    // (damages) and the last one's fields; engine.hurt turns them into the causes.
    let damages = []
    const fresh = list => list.filter(d => Date.now() - d.at < setting('prim/damage-fresh-ms'))
    const onDamage = packet => {
      if (packet.entityId !== target.entity?.id) return
      const source = packet.sourceCauseId > 0 ? target.entities?.[packet.sourceCauseId - 1] : undefined
      damages = fresh(damages) // a hit that never lowered health would otherwise stay for good
      // Only the way the hit came from, as a player feels it (the camera tilts), never the shooter's place.
      const from = source?.position ?? packet.sourcePosition
      const me = target.entity?.position
      const len = from && me ? Math.hypot(from.x - me.x, from.z - me.z) : 0
      damages.push({
        at: Date.now(),
        damageType: target.damageTypeNames?.[packet.sourceTypeId],
        ...(source && { attacker: { id: source.id, name: source.name ?? source.username } }),
        ...(len > 0 && { dir: { x: (from.x - me.x) / len, z: (from.z - me.z) / len } })
      })
    }
    target._client?.on('damage_event', onDamage)
    let lastHurt = null // the damage packets of the last health loss, for a death that follows it
    const hurtNow = () => {
      const list = fresh(damages)
      damages = []
      if (!list.length) return {}
      lastHurt = list
      const { at, ...fields } = list[list.length - 1]
      return { ...fields, damages: list.map(({ at, ...d }) => d) }
    }
    // The died event's cause when the block gives none: the attacker of the lethal loss's first damage packet, else
    // its damage type (a blast comes before the hit that follows it in the tick).
    const killerNow = () => {
      const list = lastHurt && fresh(lastHurt)
      if (!list?.length) return {}
      const { at, ...fields } = list[list.length - 1]
      const { at: firstAt, ...first } = list[0]
      const cause = first.attacker?.name ?? first.damageType?.replace(/^minecraft:/, '')
      return { ...fields, damages: list.map(({ at, ...d }) => d), ...(cause && { cause }) }
    }
    // raw levels; engine.senses turns them into weather-changed when raining or thundering flips
    const levelsNow = () => ({ rain: target.rainState ?? 0, thunder: target.thunderState ?? 0 })
    let levels = levelsNow()
    const handlers = {
      physicsTick: () => { lastTick = Date.now(); stalled = false; lastPos = target.entity.position.clone(); if (++ticks % 20 === 0) remember() },
      health: () => {
        remember()
        if (target.health < lastHealth) emit({ kind: 'hurt', health: target.health, food: target.food, amount: lastHealth - target.health, ...hurtNow(), ...causeNow() })
        lastHealth = target.health
      },
      // bot.experience still holds the pre-death values here; the server resets it in a later packet.
      death: () => { stopWalking(target); emit({
        kind: 'died',
        pos: here(),
        dimension: target.game?.dimension,
        inventory: inventoryNow().length > 0 ? inventoryNow() : snapshot,
        experience: { level: target.experience?.level ?? 0, points: target.experience?.points ?? 0 },
        ...killerNow(),
        ...causeNow()
      }) },
      respawn: () => { stopWalking(target); respawning = true },
      forcedMove: () => {
        const moved = target.entity.position.distanceTo(lastPos)
        lastPos = target.entity.position.clone()
        if (moved > setting('prim/teleport-blocks')) settleFromNow()
      },
      chat: (from, message) => { if (from !== target.username) emit({ kind: 'chat', from, message }) },
      whisper: (from, message) => { if (from !== target.username) emit({ kind: 'whisper', from, message }) },
      wake: () => emit({ kind: 'woke' }),
      actionBar: msg => {
        const status = sleepStatusOf(msg)
        if (status) emit({ kind: 'sleep-status', ...status })
      },
      // "Respawn point set" is a system chat line, not an action bar one ('message' fires for either)
      message: msg => { if (spawnSetMessage(msg)) emit({ kind: 'spawn-set', pos: here() }) },
      // mineflayer's reading of the "no home bed or it was obstructed" game event: the respawn point is gone
      spawnReset: () => emit({ kind: 'spawn-reset' }),
      playerJoined: player => { if (player?.username && player.username !== target.username) emit({ kind: 'player-joined', player: player.username }) },
      playerLeft: player => { if (player?.username && player.username !== target.username) emit({ kind: 'player-left', player: player.username }) },
      weatherUpdate: () => {
        const now = levelsNow()
        if (now.rain === levels.rain && now.thunder === levels.thunder) return
        levels = now
        emit({ kind: 'weather-levels', ...now })
      },
      playerCollect: (collector, collected) => {
        if (collector !== target.entity) return
        const item = collected && droppedItem(target, collected)
        if (item) emit({ kind: 'picked-up', item: item.name, count: item.count, pos: here(), ...(ownWorkNear() && { own: true }) })
      },
      spawn: () => {
        emit({ kind: 'spawned' })
        if (!respawning) return
        respawning = false
        settleFromNow()
        emit({ kind: 'respawned', pos: here(), dimension: target.game?.dimension })
      },
      end: reason => dropped(String(reason)),
      kicked: reason => dropped(JSON.stringify(reason)),
      // an unhandled 'error' on an EventEmitter throws and takes the process down; report it instead
      error: err => emit({ kind: 'error', reason: String(err?.message ?? err) })
    }
    Object.entries(handlers).forEach(([name, fn]) => target.on(name, fn))
    if (entityDeathListeners.size) removalEvents.forEach(([n, f]) => target.on(n, f))
    return () => {
      untrace()
      unguard()
      target._client?.removeListener('damage_event', onDamage)
      Object.entries(handlers).forEach(([name, fn]) => target.removeListener(name, fn))
      removalEvents.forEach(([n, f]) => target.removeListener(n, f))
    }
  }
  let unbind = bindEvents(bot)

  // Physics-stall watchdog: the column under the body unloaded and no tick for PHYSICS_STALL_MS. Once per stall it
  // reports, drops the walk goal and releases the controls; the next physicsTick re-arms it.
  const watchdog = setInterval(() => {
    if (closed || down || isOffline() || stalled) return
    const ms = Date.now() - lastTick
    if (ms <= setting('prim/physics-stall-ms') * timeScale || columnLoaded()) return
    stalled = true
    emit({ kind: 'physics-stalled', pos: here(), ms })
    bot.pathfinder?.setGoal(null)
    bot.clearControlStates()
  }, Math.max(setting('prim/stall-poll-ms') * timeScale, 10)) // not below 10 ms: tests shrink time a lot and leave bodies unclosed
  watchdog.unref()
  const onBodyEvent = listener => {
    listeners.add(listener)
    pending.splice(0).forEach(e => listener(e)) // events from before anyone listened (createPrimitives' wait)
    return () => listeners.delete(listener)
  }

  // Raw Mineflayer boundary for the CLJS observation cache: no radius/default32 filtering,
  // no sensing packets, and no stale quit/stalled connection exposed as a fresh observation.
  const entityObservation = () => ({
    source: bot,
    online: !closed && !down && !stalled && !isOffline() && Date.now() - lastTick <= 10000,
    dimension: bot.game?.dimension,
    entities: liveEntities(bot),
    fields: e => mobFields(bot, e)
  })
  const onEntityDeath = listener => {
    if (!entityDeathListeners.size) removalEvents.forEach(([n, f]) => bot.on(n, f))
    entityDeathListeners.add(listener)
    return () => {
      entityDeathListeners.delete(listener)
      if (!entityDeathListeners.size) removalEvents.forEach(([n, f]) => bot.removeListener(n, f))
    }
  }

  // ---- offline ----

  // wait that a cut or close can end early; resolves true when it ran its full course
  const waitOrWake = (ms, call) => new Promise(resolve => {
    const timer = setTimeout(() => resolve(true), ms)
    call.wake = () => { clearTimeout(timer); resolve(false) }
  })

  // waits for the fresh bot's world; a timeout is a warn event and the bot is used anyway. null when close() came first.
  // A fresh bot that is kicked or ends while its world loads is not adopted: null (it is not bound yet,
  // so nothing else would notice the drop), and the wait stops at once with no world-not-loaded
  const awaitWorldUnlessEnded = async fresh => {
    let ended = false
    const mark = () => { ended = true }
    // connect's own error listener is gone once the bot spawned and nothing binds it yet: a late socket error (write EPIPE
    // after a kick) would throw uncaught and end the process. Kept for good: the bot may be dropped, and a bound one reports
    // errors through its own handler.
    fresh.on('error', () => {})
    // the server announces the sleep count on the join, before the world is loaded and the bot bound: kept for adopt
    fresh.on('actionBar', msg => { if (fresh !== bot) fresh.sleepStatusAtJoin = sleepStatusOf(msg) ?? fresh.sleepStatusAtJoin })
    fresh.once('end', mark)
    fresh.once('kicked', mark)
    const loaded = await waitForWorld(fresh, { timeoutMs: worldTimeoutMs, stop: () => ended })
    fresh.removeListener('end', mark)
    fresh.removeListener('kicked', mark)
    if (ended) return null
    if (!loaded) emit({ kind: 'world-not-loaded', ms: worldTimeoutMs })
    if (!closed) return fresh
    fresh.quit()
    return null
  }

  // a fresh bot, retried a few times; null when close() came first (a bot made meanwhile is quit)
  const reconnectBot = async () => {
    for (let attempt = 1; ; attempt++) {
      if (closed) return null
      const fresh = await reconnect().then(b => ({ b }), error => ({ error }))
      if (fresh.b && closed) { fresh.b.quit(); return null }
      if (fresh.b) {
        const ready = await awaitWorldUnlessEnded(fresh.b)
        if (ready || closed) return ready
        fresh.error = new Error('the connection ended before the world loaded')
      }
      if (attempt >= setting('prim/reconnect-tries')) throw fresh.error
      await sleepMs(setting('prim/reconnect-retry-ms') * timeScale)
    }
  }

  const adopt = fresh => {
    unbind()
    bot = fresh
    trackLeashes(bot)
    trackLiveEntities(bot)
    trackVehicles(bot)
    unbind = bindEvents(bot)
    view?.attach(bot)
    down = false
    emit({ kind: 'online', pos: here() })
    if (fresh.sleepStatusAtJoin) emit({ kind: 'sleep-status', ...fresh.sleepStatusAtJoin })
  }

  // a backoff wait that close() ends early and that never keeps the process alive
  let wakeBackoff = null
  const backoffWait = ms => new Promise(resolve => {
    const timer = setTimeout(resolve, ms)
    timer.unref?.()
    wakeBackoff = () => { clearTimeout(timer); resolve() }
  })

  // After an unplanned drop: one try at once, then RECONNECT_BACKOFF_FIRST_MS doubling up to RECONNECT_BACKOFF_MAX_MS
  // between tries, each failure a reconnect-failed event with the wait before the next. Runs until the body is back
  // or close(); at most one loop runs.
  let recovering = null
  const autoReconnect = () => {
    if (!reconnect || closed) return
    recovering ??= (async () => {
      for (let attempt = 1; !closed; attempt++) {
        const fresh = await reconnect().then(b => ({ b }), error => ({ error }))
        if (fresh.b && closed) { fresh.b.quit(); return }
        if (fresh.b) {
          const ready = await awaitWorldUnlessEnded(fresh.b)
          if (ready) { adopt(ready); return }
          if (closed) return
          fresh.error = new Error('the connection ended before the world loaded')
        }
        const retryMs = Math.min(setting('prim/reconnect-backoff-first-ms') * 2 ** (attempt - 1), setting('prim/reconnect-backoff-max-ms'))
        emit({ kind: 'reconnect-failed', reason: String(fresh.error?.message ?? fresh.error), attempt, retryMs })
        await backoffWait(retryMs * timeScale)
      }
    })().finally(() => { recovering = null })
  }

  // The bound bot's connection ended without the body asking for it (offline unbinds before it quits, so its own
  // quit never lands here): the body is down and starts bringing itself back.
  const dropped = reason => {
    if (closed) return
    if (!isOffline()) rememberSelf()
    down = true
    emit({ kind: 'disconnected', reason })
    // a call still waiting on the dead bot would sit until its time bound: cut it now so its job resumes after the reconnect
    for (const call of [...inflight]) call.cut()
    autoReconnect()
  }

  // An acting primitive answers 'offline' while the connection is down (the body is reconnecting by itself). A stale
  // token still rejects with cut before anything else. While `offline` is bringing the body back (a cut ended its
  // wait) the call waits for that reconnect instead of acting on the quit bot.
  const whenUp = fn => async (token, a) => {
    if (!isOwner(token)) throw cutError()
    if (away) await away
    if (!isOwner(token)) throw cutError()
    if (down) return { status: 'offline' }
    if (bot.isSleeping && fn !== sleep) await ensureAwake(bot, { timeoutMs: 1000 * timeScale })
    return fn(token, a)
  }

  // Leaves the server for `ms`, then comes back with the same connection params. The body is offline (isOffline,
  // sensing says so) until the fresh bot is adopted. A cut ends the wait early but the body is still brought back
  // first, then the call resolves 'cut'; close() cancels the reconnect and resolves 'closed'.
  const offline = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(a.ms === undefined || a.ms === null || (isNum(a.ms) && a.ms >= 0), 'offline needs ms, a number of milliseconds of at least 0')
    if (!reconnect) return { status: 'unsupported' }
    if (down) return { status: 'offline' } // the connection already dropped; the body is reconnecting by itself
    const ms = Math.floor(Math.min(a.ms ?? setting('prim/offline-default-ms'), setting('prim/offline-max-ms')))
    const call = { token, cut: () => call.wake?.(), wake: null }
    let release
    rememberSelf()
    away = new Promise(resolve => { release = resolve })
    inflight.add(call)
    emit({ kind: 'offline', ms })
    unbind()
    view?.detach()
    bot.on('error', () => {}) // the quitting client may still complain; nothing listens for it any more
    bot.quit()
    try {
      const full = await waitOrWake(ms * timeScale, call)
      inflight.delete(call)
      const fresh = await reconnectBot().catch(error => { emit({ kind: 'disconnected', reason: String(error.message) }); throw error })
      if (!fresh) return { status: 'closed' }
      away = null
      adopt(fresh)
      return full && isOwner(token) ? { status: 'ok', ms } : { status: 'cut' }
    } finally {
      inflight.delete(call)
      const lost = away !== null && !closed // no bot came back: the body keeps trying by itself
      away = null
      if (lost) { down = true; autoReconnect() }
      release()
    }
  }

  const close = async () => {
    closed = true
    wakeBackoff?.()
    clearInterval(watchdog)
    unbind()
    entityDeathListeners.clear()
    bot.on('error', () => {}) // a quitting socket may still report a late error after lifecycle cleanup
    setOwner(null)
    await view?.detach()
    view?.stop()
    bot.quit()
  }

  // the walking primitives do nothing aboard (no physics tick runs), so they refuse at once
  const onFoot = fn => async (token, a) => {
    if (!isOwner(token)) throw cutError()
    return bot.vehicle ? { status: 'refused-mounted' } : fn(token, a)
  }

  const useOn = createUseOn({ act, getBot: () => bot, inventory, eye, lookNow, timeScale, isOwner, cutError, badArgs })

  const { steer, pathWorld } = createSteer({ act, getBot: () => bot, badArgs })

  // dig, place, jumpPlace and useOn mark the cell's column for the view dump once they settle, whatever the outcome
  const marking = fn => async (token, a) => {
    noteOwnWork()
    const mark = () => {
      noteOwnWork()
      const p = a?.pos
      if (isNum(p?.x) && isNum(p?.y) && isNum(p?.z)) view?.markCell?.(Math.floor(p.x), Math.floor(p.y), Math.floor(p.z))
    }
    try { return await fn(token, a) } finally { mark() }
  }

  const working = fn => async (token, a) => {
    noteOwnWork()
    try { return await fn(token, a) } finally { noteOwnWork() }
  }

  const acting = Object.fromEntries(Object.entries({ moveTo: onFoot(moveTo), dig: marking(dig), place: marking(place), jumpPlace: marking(jumpPlace), collect, inspectContainer, transfer: working(transfer), equip, toss, craft, furnace, enchant, chat, eat, fish, attack, interact, trade, unequip, sleep, look, swim, useOn: marking(useOn), steer: onFoot(steer), mount, dismount, paddle })
    .map(([name, fn]) => [name, whenUp(fn)]))
  // the raw world engine.perception looks at (stateAt, lightAt, eye, block changes): body-side only, never a job's
  const rawWorld = createRawWorld({ getBot: () => bot, isOffline: () => isOffline() || down, lightOverlay: (cx, cz, s) => view?.lightOverlay?.(cx, cz, s) })
  return { setOwner, isOwner, self, worldAge, entities, blockAt, harvestTools, digTime, clearTime, pathWorld, ...acting, chatDirect, wait, isOffline, isSettling, lastKnown, offline, onBodyEvent, entityObservation, onEntityDeath, rawWorld, physicsMs: () => bot.physicsClock?.intervalMs() ?? 50, close }
}

const OWN_WORK_MS = 15000
const OWN_WORK_BLOCKS = 8
const REPO_ROOT = join(import.meta.dirname, '..', '..')
const readRepoFile = rel => { try { return readFileSync(join(REPO_ROOT, rel), 'utf8') } catch { return null } }

// The README's factory: connects, resolves once spawned.
// `connect` and `timeScale` exist for tests: a stand-in for connectBot, and shrunken time bounds.
// `opts.view` ({stateDir, agent, world, onEvent}) turns on the view dump (docs/view-format.md); BODY_VIEW=0 turns the dump off (the light overlay perception reads stays on).
// A patch the planner relies on (deps-check REQUIRED) that is gone makes it refuse: before it connects, with the fix named.
export async function createPrimitives ({ view: viewOpts, settings, ...opts }, { connect = connectBot, timeScale = 1, worldTimeoutMs, settleMs, readFile = readRepoFile } = {}) {
  useSettings(settings) // the live reader from main (key -> value); every tuning number is read at use, so a reload reaches here
  worldTimeoutMs ??= setting('prim/world-timeout-ms')
  const required = missingRequired(readFile)
  if (required.length) throw new Error(`refusing to start: dependency patches missing (${required.join(', ')}); run node tools/patch-deps.mjs (an npm install undid them)`)
  const view = viewOpts ? createView(viewOpts) : null
  const gameClock = createGameClock()
  const connectOpts = { ...opts, gameClock }
  const bot = await connect(connectOpts)
  // connect's own guards are gone at spawn and bindEvents comes later: a socket error would crash the process and a kick
  // would go unseen, so guard the wait and refuse to start on a bot that already dropped
  let dropped = null
  const onDrop = reason => { dropped = reason }
  const guardError = () => {}
  bot.on('error', guardError)
  bot.once('end', onDrop)
  bot.once('kicked', onDrop)
  const loaded = await waitForWorld(bot, { timeoutMs: worldTimeoutMs, stop: () => dropped !== null })
  bot.removeListener('end', onDrop)
  bot.removeListener('kicked', onDrop)
  // a dropped bot is thrown away, so its guard stays; a started one is bound at once and reports errors through its own handler
  if (dropped === null) bot.removeListener('error', guardError)
  if (dropped !== null) throw new Error(`connection dropped while the world loaded: ${JSON.stringify(dropped)}`)
  const pending = loaded ? [] : [{ kind: 'world-not-loaded', ms: worldTimeoutMs }]
  if (bot.tickRateStock) pending.push({ kind: 'tick-rate.stock', level: 'info', reason: bot.tickRateStock, text: `tick-rate physics shim not applied, stock physics: ${bot.tickRateStock}` })
  const titles = missingPatches(readFile)
  if (titles.length) pending.push({ kind: 'dependency-patches-missing', titles, text: 'run node tools/patch-deps.mjs (an npm install undid them)' })
  const prims = createPrimitivesFromBot(bot, { timeScale, reconnect: () => connect(connectOpts), view, worldTimeoutMs, settleMs, pending })
  return Object.assign(prims, { gameClock })
}