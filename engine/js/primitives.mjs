// The mineflayer layer of the engine: a small set of time-bounded operations, each a cut point. The contract is in
// engine/README.md (Primitives). Ideas copied from src/body/actions/*.mjs; nothing here imports from src/.
import vec3 from 'vec3'
import pf from 'mineflayer-pathfinder'
import { connectBot } from './connect.mjs'
import { createView } from './view.mjs'
import { lineClear } from './sight.mjs'
import { isReplaceable } from './blocks.mjs'

const { Vec3 } = vec3
const { goals } = pf

export const REACH = 4.5
export const ATTACK_REACH = 3.5
const MONSTER_RANGE = 8
const DROP_RADIUS = 2
const DROP_WAIT_S = 1
const POLL_MS = 50
const HURT_WAIT_MS = 300 // attack waits this long for the server's entityHurt on the target
const CONTAINER = /chest|barrel|shulker_box|furnace|smoker|hopper|dispenser|dropper|brewing_stand/
const DESTS = ['hand', 'off-hand', 'head', 'torso', 'legs', 'feet']
const DEFAULT_RADIUS = 16
const SEE_THROUGH = /glass|^water$|^fire$|grass$|^snow$|^vine$|^ladder$|torch$|^lava$/
const KINDS = ['hostile', 'passive', 'player', 'item', 'other']
const OFFLINE_DEFAULT_MS = 5 * 60 * 1000
const OFFLINE_MAX_MS = 10 * 60 * 1000
const JUMP_PLACE_MAX = 8
const JUMP_PLACE_BLOCK_S = 2
const RISE_WAIT_MS = 800
const LAND_WAIT_MS = 1000
const SWIM_DEFAULT_MS = 3000
const SWIM_MAX_MS = 10000
const RECONNECT_TRIES = 3
const RECONNECT_RETRY_MS = 5000
const WORLD_TIMEOUT_MS = 10000
const WORLD_POLL_MS = 50
const POSE_SLEEPING = 2
// Step-up out of a 1-deep hole when the pathfinder stalls flush against the ledge (see stepUp).
const STEP_RISE = 1.0
const CENTRE_TOLERANCE = 0.1
const CENTRE_S = 1
const CENTRE_SPEED = 0.005 // blocks per tick: centred means within tolerance and (nearly) stopped
const LOOK_TICK_MS = 100 // a look waits for the next physics tick (the rotation goes out there), at most this long
const STEP_S = 1.5
const STEP_ATTEMPTS = 2
const FLAG_ON_FIRE = 0x01

const WAIT_MAX_MS = 10000
const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))
// Resolves true once the column under the body is loaded (blockAt there is non-null), false after `timeoutMs`.
// A body that acts before its chunk arrives sees no roof, no bed and no other players.
export async function waitForWorld (bot, { timeoutMs = WORLD_TIMEOUT_MS, pollMs = WORLD_POLL_MS } = {}) {
  const deadline = Date.now() + timeoutMs
  while (!bot.blockAt(bot.entity.position)) {
    if (Date.now() >= deadline) return false
    await sleepMs(pollMs)
  }
  return true
}
const xyz = v => ({ x: v.x, y: v.y, z: v.z })
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const center = p => ({ x: p.x + 0.5, y: p.y + 0.5, z: p.z + 0.5 })
const isAir = name => name === 'air' || name.endsWith('_air')
const isNum = n => typeof n === 'number' && Number.isFinite(n)
const isPos = p => Boolean(p) && isNum(p.x) && isNum(p.y) && isNum(p.z)
const cell = p => ({ x: Math.floor(p.x), y: Math.floor(p.y), z: Math.floor(p.z) })
const vec = p => new Vec3(p.x, p.y, p.z)

const codedError = (code, message) => Object.assign(new Error(message), { code, [code === 'cut' ? 'cut' : 'badArgs']: true })
export const cutError = () => codedError('cut', 'cut: the ownership token no longer matches')
const badArgs = message => codedError('bad-args', message)
const isCut = err => err?.code === 'cut'
// a mineflayer rejection is a domain failure: a status, never a throw (only cut and bad-args reject)
const failed = err => ({ status: 'failed', reason: String(err?.message ?? err).slice(0, 200) })
const BUCKET_WAIT_S = 2
const need = (ok, message) => { if (!ok) throw badArgs(message) }

const entityKind = e => {
  if (e.type === 'player') return 'player'
  if (e.name === 'item' || e.name === 'item_stack' || e.displayName === 'Item') return 'item'
  if (e.type === 'hostile' || /hostile/i.test(e.kind ?? '')) return 'hostile'
  if (['passive', 'animal', 'ambient', 'water_creature'].includes(e.type) || /passive|animal/i.test(e.kind ?? '')) return 'passive'
  return 'other'
}

// A slot value as the client library leaves it: a prismarine Item {name|type, count}, a network slot
// {itemId, itemCount, present?} (1.13+) or {blockId, itemCount} (older), possibly without a count.
const slotLike = m => m !== null && typeof m === 'object' && [m.itemId, m.blockId, m.type, m.name].some(v => v !== undefined)

// {name, count} for a slot value; null for an empty slot; name 'unknown' for an id the registry lacks.
const stackOf = (bot, slot) => {
  if (!slotLike(slot) || slot.present === false) return null
  const count = slot.itemCount ?? slot.count ?? 1
  if (!(count > 0)) return null
  const id = slot.itemId ?? slot.blockId ?? slot.type
  return { name: slot.name ?? bot.registry?.items?.[id]?.name ?? 'unknown', count }
}

// The index of a named entity-metadata field: the registry knows it per entity type, the older fixed layout is the fallback.
const metaIndex = (bot, e, key, fallback) => {
  const at = bot.registry?.entitiesByName?.[e.name]?.metadataKeys?.indexOf(key)
  return at >= 0 ? at : fallback
}
const metaValue = (bot, e, key, fallback) => e.metadata?.[metaIndex(bot, e, key, fallback)]
const burning = (bot, e) => ((metaValue(bot, e, 'shared_flags', 0) ?? 0) & FLAG_ON_FIRE) !== 0
const lyingDown = (bot, e) => metaValue(bot, e, 'pose', 6) === POSE_SLEEPING

const attempt = f => { try { return f() } catch { return null } }

// getDroppedItem throws or returns null when the library cannot read the slot (it reads one fixed metadata index),
// so fall back to scanning the metadata for any slot-shaped value.
const droppedItem = (bot, e) =>
  stackOf(bot, attempt(() => e.getDroppedItem?.())) ?? stackOf(bot, Object.values(e.metadata ?? {}).find(slotLike))

const itemCounts = items => items.reduce((acc, i) => ({ ...acc, [i.name]: (acc[i.name] ?? 0) + i.count }), {})
const gained = (before, after) => Object.entries(after)
  .map(([name, count]) => ({ name, count: count - (before[name] ?? 0) }))
  .filter(g => g.count > 0)

// Builds the primitives over an already spawned bot. `timeScale` multiplies every time bound (tests shrink it).
// `reconnect` (internal; createPrimitives passes it) makes a fresh spawned bot with the same connection params and
// enables `offline`; without it `offline` is unsupported.
export function createPrimitivesFromBot (initialBot, { timeScale = 1, reconnect = null, view = null, worldTimeoutMs = WORLD_TIMEOUT_MS, pending = [] } = {}) {
  let bot = initialBot
  view?.attach(bot)
  let closed = false
  let owner = null
  const inflight = new Set()
  const listeners = new Set()
  // Offline is body state: set from the moment `offline` quits the bot until the fresh bot is adopted (or the
  // reconnect gave up); resolves then. Sensing answers 'offline' meanwhile and acting calls wait for it.
  let away = null
  const isOffline = () => away !== null

  const isOwner = token => token !== null && token !== undefined && token === owner
  const setOwner = token => {
    owner = token ?? null
    for (const call of [...inflight]) if (call.token !== owner) call.cut()
  }

  const here = () => xyz(bot.entity.position)
  const eye = () => ({ x: bot.entity.position.x, y: bot.entity.position.y + (bot.entity.height ?? 1.62), z: bot.entity.position.z })
  const inventory = () => bot.inventory.items()
  const countsNow = () => itemCounts(inventory())
  const hostilesNear = () => Object.values(bot.entities)
    .filter(e => e !== bot.entity && entityKind(e) === 'hostile')
    .filter(e => Math.hypot(e.position.x - bot.entity.position.x, e.position.z - bot.entity.position.z) <= MONSTER_RANGE && Math.abs(e.position.y - bot.entity.position.y) <= 5)

  // Runs `body(ctx)` as one call owned by `token`. The call ends the moment the owner changes (rejects with cut), or
  // when its time bound passes (resolves onTimeout(), default {status: 'timeout'}); both run the aborts the body
  // registered, and `ctx.alive()` then throws so the body stops reaching the bot. Any other throw from the body (a
  // mineflayer rejection) resolves {status: 'failed', reason}. Entry with a stale token throws.
  const act = (token, { boundS, onTimeout = () => ({ status: 'timeout' }) }, body) => {
    if (!isOwner(token)) return Promise.reject(cutError())
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
      const timer = setTimeout(() => { if (settled) return; runAborts(); settle(resolve)(onTimeout()) }, Math.max(1, boundS * 1000 * timeScale))
      inflight.add(call)
      Promise.resolve().then(() => body(ctx)).then(settle(resolve), err => settled ? undefined : (isCut(err) ? settle(reject)(err) : settle(resolve)(failed(err))))
    })
  }

  // a walk toward `goal`, abortable; resolves true when the pathfinder reached it, false when it gave up
  // The goal is cleared on every way out (arrived, gave up, timed out, cut): a goal left set makes the pathfinder
  // walk the body back on its own, e.g. after a respawn.
  const stopWalking = target => { target.pathfinder?.setGoal(null); target.clearControlStates?.() }
  const stepUpTarget = (path, body) => {
    const next = path?.[0]
    if (!next) return null
    // pathfinder nodes are cell centres (x.5), so they are floored before they are compared with the body's cell
    const from = cell(body.entity.position)
    const to = cell(next)
    const adjacent = Math.abs(to.x - from.x) + Math.abs(to.z - from.z) === 1
    return adjacent && to.y === from.y + 1 ? to : null
  }
  const faceCentre = (body, c) => body.lookAt(new Vec3(c.x + 0.5, body.entity.position.y + (body.entity.height ?? 1.62), c.z + 0.5), true)
  // A forced look only sets the rotation locally; mineflayer writes it to the server in the physics tick that follows,
  // synchronously after emitting 'physicsTick'. So resolve on the next tick (bounded, physics may be off). Always
  // wait, even when the rotation did not change: another forced look (faceCentre, jumpPlace, swim) may have set it
  // already without a tick having sent it yet.
  const lookNow = async aim => {
    await aim()
    await new Promise(resolve => {
      const done = () => { clearTimeout(timer); bot.off('physicsTick', done); resolve() }
      const timer = setTimeout(done, Math.max(1, LOOK_TICK_MS * timeScale))
      bot.on('physicsTick', done)
    })
  }
  const waitUntil = async (ctx, done, boundS) => {
    const deadline = Date.now() + boundS * 1000 * timeScale
    while (!done() && Date.now() < deadline) {
      await sleepMs(POLL_MS * timeScale)
      ctx.alive()
    }
  }
  // Sneaks to the middle of `centre` (a cell), forward held only while off-centre: sneaking is slow enough not to
  // overshoot into the far wall. True when within `tolerance` and stopped (already or after), false after CENTRE_S;
  // always leaves sneak and forward released. Forward is released as soon as the body is within tolerance, and the
  // body is only done once it has coasted to a stop: it would otherwise drift ~0.1 further during a jump.
  // The wait is a body's `velocity` (blocks per tick); a body without one counts as stopped.
  const centreBody = async (ctx, body, centre, tolerance = CENTRE_TOLERANCE) => {
    // a server correction replaces the position object, so it is read fresh at every use
    const live = () => body.entity.position
    const toCentre = () => Math.hypot(centre.x + 0.5 - live().x, centre.z + 0.5 - live().z)
    const speed = () => Math.hypot(body.entity.velocity?.x ?? 0, body.entity.velocity?.z ?? 0)
    const centred = () => toCentre() < tolerance && speed() < CENTRE_SPEED
    if (centred()) return true
    const onCentre = () => {
      const off = toCentre() >= tolerance
      if (off) faceCentre(body, centre).catch(() => {})
      body.setControlState('forward', off)
    }
    body.setControlState('sneak', true)
    body.on('physicsTick', onCentre)
    try {
      await waitUntil(ctx, centred, CENTRE_S)
      return centred()
    } finally {
      body.off('physicsTick', onCentre)
      body.setControlState('forward', false)
      body.setControlState('sneak', false)
    }
  }
  // Out of a 1-deep hole: the server rejects every jump while the body is flush against a wall, so centre in the cell
  // first, then hold jump alone, and press forward only once the feet are a block above where they started.
  const stepUp = async (ctx, body, target) => {
    const live = () => body.entity.position
    const centre = cell(live())
    const startY = live().y
    const onTick = () => { if (live().y >= startY + STEP_RISE) body.setControlState('forward', true) }
    stopWalking(body)
    try {
      await centreBody(ctx, body, centre) // a failure to centre is ignored: the jump below tries anyway
      await faceCentre(body, target)
      ctx.alive()
      body.setControlState('jump', true)
      body.on('physicsTick', onTick)
      const landed = () => { const now = cell(live()); return now.x === target.x && now.y === target.y && now.z === target.z && body.entity.onGround }
      await waitUntil(ctx, landed, STEP_S)
    } finally {
      body.off('physicsTick', onTick)
      body.clearControlStates()
    }
  }
  // A goto is rejected by our own setGoal(null) when a step-up starts; that is not a failure, the walk re-issues it.
  const walk = async (ctx, goal) => {
    const walking = bot
    ctx.onAbort(() => stopWalking(walking))
    let path = null
    let onStuck = () => {}
    const onUpdate = result => { if (result?.status === 'success' || result?.status === 'partial') path = result.path }
    const onReset = reason => { if (reason === 'stuck') onStuck() }
    walking.on('path_update', onUpdate)
    walking.on('path_reset', onReset)
    try {
      for (let attempts = 0; ; attempts++) {
        ctx.alive()
        const stuck = new Promise(resolve => {
          onStuck = () => { const target = attempts < STEP_ATTEMPTS ? stepUpTarget(path, walking) : null; if (target) resolve(target) }
        })
        const ended = await Promise.race([walking.pathfinder.goto(goal).then(() => true, () => false), stuck])
        ctx.alive()
        if (typeof ended === 'boolean') return ended
        await stepUp(ctx, walking, ended)
        ctx.alive()
      }
    } finally {
      walking.off('path_update', onUpdate)
      walking.off('path_reset', onReset)
      stopWalking(walking)
    }
  }

  // ---- sensing ----

  const feetIn = name => bot.blockAt(vec(cell(here())))?.name === name

  // "FireResistance" (registry) or "minecraft:fire_resistance" -> "fire_resistance"
  const effectName = id => {
    const name = bot.registry?.effects?.[id]?.name
    if (!name) return String(id)
    return name.replace(/^minecraft:/, '').replace(/([a-z0-9])([A-Z])/g, '$1_$2').toLowerCase()
  }
  const effects = () => Object.values(bot.entity.effects ?? {})
    .map(e => ({ name: effectName(e.id), amplifier: e.amplifier, duration: e.duration }))

  const self = () => {
    if (isOffline()) return { status: 'offline' }
    const timeOfDay = bot.time.timeOfDay
    return {
      username: bot.username,
      pos: here(),
      health: bot.health,
      food: bot.food,
      foodSaturation: bot.foodSaturation,
      oxygen: bot.oxygenLevel ?? 20,
      onFire: burning(bot, bot.entity),
      inWater: bot.entity.isInWater ?? feetIn('water'),
      inLava: bot.entity.isInLava ?? feetIn('lava'),
      onGround: Boolean(bot.entity.onGround),
      isSleeping: Boolean(bot.isSleeping),
      effects: effects(),
      experience: { level: bot.experience?.level ?? 0, points: bot.experience?.points ?? 0, progress: bot.experience?.progress ?? 0 },
      dimension: bot.game?.dimension,
      timeOfDay,
      isDay: timeOfDay < 12542 || timeOfDay > 23460,
      held: bot.heldItem?.name ?? null,
      inventory: inventory().map(i => ({ name: i.name, count: i.count, slot: i.slot }))
    }
  }

  // Whether a block stops the eye: a full-cube bounding box, except glass. An unloaded cell never blocks, so a
  // threat is not hidden by a gap in the map.
  const blocksSight = p => {
    const block = bot.blockAt(vec(p))
    return Boolean(block) && block.boundingBox === 'block' && !SEE_THROUGH.test(block.name)
  }
  // eye to the middle of the entity; the walk is bounded by that segment, which the caller keeps within its radius
  const canSee = e => lineClear(eye(), { x: e.position.x, y: e.position.y + (e.height ?? 1.8) / 2, z: e.position.z }, blocksSight)

  const entities = ({ radius = DEFAULT_RADIUS, kind, names, max = 32 } = {}) => {
    if (isOffline()) return []
    const me = here()
    return Object.values(bot.entities)
      .filter(e => e !== bot.entity && e.position)
      .map(e => ({ e, distance: dist(me, e.position), kind: entityKind(e) }))
      .filter(({ e, distance, kind: k }) => distance <= radius && (!kind || k === kind) && (!names || names.includes(e.name ?? e.username)))
      .sort((a, b) => a.distance - b.distance)
      .slice(0, max)
      .map(({ e, distance, kind: k }) => ({
        id: e.id,
        name: e.name ?? e.username,
        kind: k,
        pos: xyz(e.position),
        distance,
        ...(k === 'hostile' && { visible: canSee(e) }),
        ...(k === 'item' && { item: droppedItem(bot, e) }),
        ...(k === 'player' && { username: e.username, sleeping: lyingDown(bot, e) }),
        ...(e.name === 'creeper' && { creeper: true })
      }))
  }

  // {name, pos}, plus the crop `age` state when the block has one (wheat, carrots, sweet berries)
  const blockInfo = block => {
    const age = block.getProperties?.().age
    return { name: block.name, pos: xyz(block.position), ...(age !== undefined && { age: Number(age) }) }
  }

  const blocks = ({ radius = DEFAULT_RADIUS, names, match, max = 64 } = {}) => {
    if (isOffline()) return []
    const wanted = names && new Set(names)
    const matching = block => Boolean(block) && (wanted ? wanted.has(block.name) : match ? match(block.name) : !isAir(block.name))
    const me = here()
    return bot.findBlocks({ matching, maxDistance: radius, count: max })
      .map(p => ({ ...blockInfo(bot.blockAt(p)), distance: dist(me, p) }))
      .sort((a, b) => a.distance - b.distance)
  }

  const blockAt = pos => {
    if (isOffline()) return null
    const block = bot.blockAt(vec(pos))
    return block ? blockInfo(block) : null
  }

  // ---- acting ----

  const moveTo = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'moveTo needs pos {x, y, z}')
    const { range = 1, timeoutS = 20, maxDistance = 64 } = a
    const target = cell(a.pos)
    const start = here()
    const before = dist(start, target)
    const capped = before > maxDistance
    const goal = capped
      ? new goals.GoalNearXZ(Math.round(start.x + (target.x - start.x) * maxDistance / before), Math.round(start.z + (target.z - start.z) * maxDistance / before), 2)
      : new goals.GoalNear(target.x, target.y, target.z, range)
    // `reached` is only what the pathfinder promised: goto resolves on a noPath update with an empty path, so the
    // goal itself is checked against where the body stands.
    const satisfied = () => !capped && goal.isEnd(vec(here()).floored())
    const outcome = (reached) => {
      const distance = dist(here(), target)
      const base = { pos: here(), distance }
      if (reached && satisfied()) return { status: 'arrived', ...base }
      if (distance < before - 1) return { status: 'partial', ...base }
      return { status: 'blocked', ...(reached && !capped && { reason: 'noPath' }), ...base }
    }
    return act(token, { boundS: Math.min(timeoutS, 60), onTimeout: () => outcome(false) }, async ctx => outcome(await walk(ctx, goal)))
  }

  const oxygenNow = () => bot.oxygenLevel ?? 20
  const headUnderwater = () => bot.blockAt(vec(cell(eye())))?.name === 'water'

  // Holds jump until the head is out of the water. The pathfinder has no swim-up move, so a submerged body cannot
  // surface with moveTo. Jump is released on every exit: surfaced, timeout, cut, error.
  // Dry: the feet cell holds no water. Standing: dry, and the cell under it is a full block.
  const dry = () => bot.blockAt(vec(cell(here())))?.name !== 'water'
  const standing = () => dry() && bot.blockAt(vec({ ...cell(here()), y: cell(here()).y - 1 }))?.boundingBox === 'block'

  // Swimming toward a rim, the body is made 0.01 wider: the server rejects a client body that rests flush against a
  // wall and snaps it back every tick, so the water-exit impulse of the physics never lands. Verified live against
  // 26.1: half-width 0.3 stays pressed on the rim wall, 0.31 climbs out. Restored on every exit.
  const SWIM_HALF_WIDTH = 0.31

  const swim = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(a.ms === undefined || a.ms === null || (isNum(a.ms) && a.ms > 0), 'swim needs ms, a number of milliseconds above 0')
    need(a.toward === undefined || a.toward === null || isPos(a.toward), 'swim toward needs {x, y, z}')
    const ms = Math.min(a.ms ?? SWIM_DEFAULT_MS, SWIM_MAX_MS)
    const toward = a.toward ?? null
    const before = oxygenNow()
    const result = status => ({ status, oxygen: { before, after: oxygenNow() } })
    return act(token, { boundS: ms / 1000, onTimeout: () => result('timeout') }, async ctx => {
      const pressed = new Set()
      const physics = bot.physics
      const width = physics?.playerHalfWidth
      const press = control => { if (!pressed.has(control)) { pressed.add(control); bot.setControlState(control, true) } }
      const release = () => {
        for (const control of [...pressed]) { pressed.delete(control); bot.setControlState(control, false) }
        if (physics && width !== undefined) physics.playerHalfWidth = width
      }
      ctx.onAbort(release)
      try {
        if (toward) {
          // climb out toward the target: the pathfinder has no move from floating feet onto a rim just above
          const there = center(cell(toward))
          const arrived = () => standing() || (dry() && dist(here(), there) <= 1.5)
          if (arrived()) return result('landed')
          await bot.lookAt(vec(there), true)
          ctx.alive()
          if (physics && width !== undefined) physics.playerHalfWidth = SWIM_HALF_WIDTH
          while (!arrived()) {
            press('jump')
            press('forward')
            await sleepMs(POLL_MS * timeScale)
            ctx.alive()
          }
          return result('landed')
        }
        while (headUnderwater()) {
          press('jump')
          await sleepMs(POLL_MS * timeScale)
          ctx.alive()
        }
        return result('surfaced')
      } finally {
        release()
      }
    })
  }

  // Pillars up: for each repetition the body looks straight down, jumps, and once its feet clear the cell it stood in
  // places the item there against the block under that cell (the placed block lands under the falling body), releases
  // jump and waits to stand one block higher. Stops at the first thing that goes wrong and reports what it achieved.
  const jumpPlace = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(typeof a.item === 'string', 'jumpPlace needs item')
    need(a.count === undefined || a.count === null || (Number.isInteger(a.count) && a.count >= 1), 'jumpPlace needs count, an integer of at least 1')
    const count = Math.min(a.count ?? 1, JUMP_PLACE_MAX)
    let placed = 0
    const outcome = reason => ({ status: placed === count ? 'done' : placed > 0 ? 'partial' : 'failed', placed, ...(reason && { reason }) })
    return act(token, { boundS: JUMP_PLACE_BLOCK_S * count, onTimeout: () => outcome('timeout') }, async ctx => {
      let pressed = false
      const release = () => { if (pressed) { pressed = false; bot.setControlState('jump', false) } }
      ctx.onAbort(release)
      const pause = async ms => { await sleepMs(ms * timeScale); ctx.alive() }
      const solidAt = c => bot.blockAt(vec(c))?.boundingBox === 'block'
      try {
        while (placed < count) {
          const item = inventory().find(i => i.name === a.item)
          if (!item) return outcome('no-item')
          const start = cell(here())
          const below = bot.blockAt(vec({ ...start, y: start.y - 1 }))
          if (below?.boundingBox !== 'block') return outcome('no-support')
          if (solidAt({ ...start, y: start.y + 2 })) return outcome('no-headroom')
          // centring gets the body off a wall, where the jump never happens; the server does not refuse an off-centre
          // placement, so a failure to centre is ignored (a body that cannot jump ends not-raised)
          await centreBody(ctx, bot, start)
          await bot.equip(item, 'hand')
          ctx.alive()
          await bot.look(bot.entity.yaw ?? 0, -Math.PI / 2, true)
          ctx.alive()
          pressed = true
          bot.setControlState('jump', true)
          const riseDeadline = Date.now() + RISE_WAIT_MS * timeScale
          while (bot.entity.position.y < start.y + 1.01 && Date.now() < riseDeadline) await pause(20)
          if (bot.entity.position.y < start.y + 1.01) return outcome('not-raised')
          // placeBlock's own unforced lookAt turns gradually when the body is slightly off-centre and delays the packet
          // ~1 s, until the body has fallen back into the cell and the server refuses; 'ignore' sends it at once
          const failure = await bot._placeBlockWithOptions(below, new Vec3(0, 1, 0), { swingArm: 'right', forceLook: 'ignore' }).then(() => null, err => err)
          ctx.alive()
          release()
          if (failure) return outcome(`place-failed: ${String(failure.message ?? failure).slice(0, 100)}`)
          const landDeadline = Date.now() + LAND_WAIT_MS * timeScale
          while (!bot.entity.onGround && Date.now() < landDeadline) await pause(20)
          if (bot.entity.position.y < start.y + 0.9) return outcome('not-raised')
          placed += 1
        }
        return outcome()
      } finally {
        release()
      }
    })
  }

  const dropsNear = p => Object.values(bot.entities)
    .filter(e => entityKind(e) === 'item' && dist(center(p), e.position) <= DROP_RADIUS)
    .map(e => ({ id: e.id, ...droppedItem(bot, e), pos: xyz(e.position) }))

  const dig = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'dig needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: 10 }, async ctx => {
      const block = bot.blockAt(vec(p))
      if (!block || isAir(block.name)) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      if (!block.diggable) return { status: 'cannot' }
      ctx.onAbort(() => bot.stopDigging())
      ctx.alive()
      await bot.dig(block, true)
      ctx.alive()
      const deadline = Date.now() + DROP_WAIT_S * 1000 * timeScale
      let drops = dropsNear(p)
      while (drops.length === 0 && Date.now() < deadline) {
        await sleepMs(POLL_MS * timeScale)
        ctx.alive()
        drops = dropsNear(p)
      }
      return { status: 'dug', block: block.name, drops }
    })
  }

  const supportFor = p => [[0, -1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1], [0, 1, 0]]
    .map(([dx, dy, dz]) => ({ ref: bot.blockAt(new Vec3(p.x + dx, p.y + dy, p.z + dz)), face: new Vec3(-dx, -dy, -dz) }))
    .find(({ ref }) => ref && !isAir(ref.name) && ref.boundingBox === 'block')

  const isBucket = name => name === 'bucket' || name.endsWith('_bucket')
  const isLiquid = name => name === 'water' || name === 'lava'

  // A bucket is used, not placed: look at the block the liquid goes on (or at the liquid to scoop) and activate the
  // item, then check that the cell p changed within a short bound.
  const useBucket = async (ctx, item, p) => {
    const scoop = item.name === 'bucket'
    const there = bot.blockAt(vec(p))
    if (scoop && !(there && isLiquid(there.name))) return { status: 'missing' }
    if (!scoop && there && !isAir(there.name) && !isReplaceable(there.name)) return { status: 'occupied' }
    const aim = scoop ? there : supportFor(p)?.ref
    if (!aim) return { status: 'no-support' }
    if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
    const count = name => inventory().filter(i => i.name === name).reduce((sum, i) => sum + i.count, 0)
    const filledName = `${there?.name}_bucket`
    const [filledBefore, heldBefore] = [count(filledName), count(item.name)]
    ctx.alive()
    await bot.equip(item, 'hand')
    ctx.alive()
    await lookNow(() => bot.lookAt(aim.position.offset(0.5, 0.5, 0.5), true))
    ctx.alive()
    await bot.activateItem()
    // The block update can arrive after the window while the inventory already changed, so either one counts: the
    // cell flipped, or the held bucket turned into the filled one (scoop) / the emptied one (pour).
    const cellChanged = () => { const now = bot.blockAt(vec(p)); return scoop ? !(now && isLiquid(now.name)) : Boolean(now && isLiquid(now.name)) }
    const itemChanged = () => scoop ? count(filledName) > filledBefore : count(item.name) < heldBefore
    const changed = () => cellChanged() || itemChanged()
    const deadline = Date.now() + BUCKET_WAIT_S * 1000 * timeScale
    while (!changed() && Date.now() < deadline) {
      await sleepMs(POLL_MS * timeScale)
      ctx.alive()
    }
    if (!changed()) return { status: 'failed', reason: 'unchanged' }
    return { status: 'placed', block: scoop ? 'bucket' : item.name.replace('_bucket', '') }
  }

  const place = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos) && typeof a.item === 'string', 'place needs pos {x, y, z} and item')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      if (isBucket(a.item)) {
        const bucket = inventory().find(i => i.name === a.item)
        return bucket ? useBucket(ctx, bucket, p) : { status: 'no-item' }
      }
      const there = bot.blockAt(vec(p))
      if (there && !isAir(there.name) && !isReplaceable(there.name) && there.name !== 'water' && there.name !== 'lava') return { status: 'occupied' }
      const item = inventory().find(i => i.name === a.item)
      if (!item) return { status: 'no-item' }
      const support = supportFor(p)
      if (!support) return { status: 'no-support' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      ctx.alive()
      await bot.equip(item, 'hand')
      ctx.alive()
      await bot.placeBlock(support.ref, support.face)
      return { status: 'placed', block: a.item }
    })
  }

  const collect = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'collect needs an entity id')
    const { id, timeoutS = 10 } = a
    const before = countsNow()
    return act(token, { boundS: Math.min(timeoutS, 20), onTimeout: () => ({ status: 'timeout', gained: gained(before, countsNow()) }) }, async ctx => {
      const target = bot.entities[id]
      if (!target || entityKind(target) !== 'item') return { status: 'gone' }
      const reached = await walk(ctx, new goals.GoalNear(target.position.x, target.position.y, target.position.z, 1))
      if (!reached && bot.entities[id]) return { status: 'unreachable', gained: gained(before, countsNow()) }
      while (bot.entities[id]) {
        await sleepMs(POLL_MS * timeScale)
        ctx.alive()
      }
      const got = gained(before, countsNow())
      return got.length ? { status: 'collected', gained: got } : { status: 'gone', gained: [] }
    })
  }

  const withWindow = async (ctx, block, use) => {
    const win = await bot.openContainer(block)
    ctx.onAbort(() => bot.closeWindow(win))
    try {
      ctx.alive()
      return await use(win)
    } finally {
      bot.closeWindow(win)
    }
  }
  const containerAt = p => {
    const block = bot.blockAt(vec(p))
    return block && CONTAINER.test(block.name) ? block : null
  }
  const slots = items => items.map(i => ({ name: i.name, count: i.count, slot: i.slot }))

  const inspectContainer = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'inspectContainer needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = containerAt(p)
      if (!block) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      return withWindow(ctx, block, async win => ({ status: 'ok', items: slots(win.containerItems()) }))
    })
  }

  const transfer = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos) && typeof a.item === 'string' && ['deposit', 'withdraw'].includes(a.direction), 'transfer needs pos, item and direction deposit|withdraw')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = containerAt(p)
      if (!block) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      const type = bot.registry.itemsByName[a.item]?.id
      return withWindow(ctx, block, async win => {
        const source = a.direction === 'deposit' ? inventory() : win.containerItems()
        const available = source.filter(i => i.name === a.item).reduce((sum, i) => sum + i.count, 0)
        const count = Math.min(a.count ?? available, available)
        if (count <= 0 || type === undefined) return { status: 'no-item', moved: 0 }
        ctx.alive()
        const failure = await (a.direction === 'deposit' ? win.deposit(type, null, count) : win.withdraw(type, null, count)).then(() => null, err => err)
        ctx.alive()
        if (failure && !/full|room|space/i.test(failure.message)) throw failure
        return failure ? { status: 'full', moved: 0 } : { status: 'ok', moved: count }
      })
    })
  }

  const equip = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    const { item: name, dest = 'hand' } = a
    need(typeof name === 'string' && DESTS.includes(dest), 'equip needs item and dest hand|off-hand|head|torso|legs|feet')
    return act(token, { boundS: 2 }, async ctx => {
      const item = inventory().find(i => i.name === name)
      if (!item) return { status: 'no-item' }
      await bot.equip(item, dest)
      return { status: 'equipped' }
    })
  }

  // With `slot`, throws exactly that slot's whole stack (bot.toss would take from whichever slot it finds first).
  // Throws carried items in the direction the body looks (it does not look anywhere itself); the stacks of the item
  // are summed, so a count may span several slots.
  const toss = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(typeof a.item === 'string', 'toss needs item, an item name')
    need(a.slot === undefined || a.slot === null || isNum(a.slot), 'toss slot must be a number')
    return act(token, { boundS: 2 }, async ctx => {
      if (isNum(a.slot)) {
        const stack = bot.inventory.slots[a.slot]
        if (!stack || stack.name !== a.item) return { status: 'no-item', count: 0 }
        await bot.tossStack(stack)
        ctx.alive()
        return { status: 'tossed', count: stack.count }
      }
      const total = inventory().filter(i => i.name === a.item).reduce((sum, i) => sum + i.count, 0)
      const count = Math.min(a.count ?? total, total)
      const type = bot.registry.itemsByName[a.item]?.id
      if (count <= 0 || type === undefined) return { status: 'no-item', count: 0 }
      await bot.toss(type, null, count)
      ctx.alive()
      return { status: 'tossed', count }
    })
  }

  const bestFood = () => inventory()
    .filter(i => bot.registry.foodsByName?.[i.name])
    .sort((a, b) => bot.registry.foodsByName[b.name].foodPoints - bot.registry.foodsByName[a.name].foodPoints)[0]

  const eat = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    const startFood = bot.food
    return act(token, { boundS: 5, onTimeout: () => bot.food > startFood ? { status: 'ate', item: a.item ?? null, food: bot.food } : { status: 'timeout' } }, async ctx => {
      const item = a.item ? inventory().find(i => i.name === a.item) : bestFood()
      if (!item) return { status: 'no-food' }
      if (bot.food >= 20) return { status: 'full' }
      ctx.onAbort(() => bot.deactivateItem())
      await bot.equip(item, 'hand')
      ctx.alive()
      await bot.consume()
      return { status: 'ate', item: item.name, food: bot.food }
    })
  }

  const attack = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'attack needs an entity id')
    return act(token, { boundS: 1 }, async ctx => {
      const target = bot.entities[a.id]
      if (!target) return { status: 'gone' }
      if (dist(eye(), { x: target.position.x, y: target.position.y + (target.height ?? 1) / 2, z: target.position.z }) > ATTACK_REACH) return { status: 'out-of-reach' }
      let hurtSeen = false
      const onHurt = entity => { if (entity?.id === a.id) hurtSeen = true }
      bot.on('entityHurt', onHurt) // before the swing, so a fast event is not missed
      try {
        await bot.attack(target)
        ctx.alive()
        const deadline = Date.now() + HURT_WAIT_MS * timeScale
        while (!hurtSeen && bot.entities[a.id] && Date.now() < deadline) {
          await sleepMs(POLL_MS * timeScale)
          ctx.alive()
        }
      } finally {
        bot.removeListener('entityHurt', onHurt)
      }
      const now = bot.entities[a.id]
      const health = typeof now?.health === 'number' ? now.health : undefined
      const killed = !now || (health !== undefined && health <= 0)
      return { status: killed ? 'killed' : 'hit', ...(health !== undefined && { health }), hurt: hurtSeen || killed }
    })
  }

  const sleep = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'sleep needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = bot.blockAt(vec(p))
      if (!block || !block.name.endsWith('_bed')) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      const { timeOfDay } = bot.time
      if (timeOfDay < 12542 || timeOfDay > 23460) return { status: 'not-night' }
      if (hostilesNear().length > 0) return { status: 'monsters-near' }
      ctx.onAbort(() => bot.wake())
      ctx.alive()
      const failure = await bot.sleep(block).then(() => null, err => err)
      ctx.alive()
      if (failure && /occupied/i.test(failure.message)) return { status: 'occupied' }
      if (failure) throw failure
      return { status: 'sleeping' }
    })
  }

  const look = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos) || (isNum(a.yaw) && isNum(a.pitch)), 'look needs pos {x, y, z} or yaw and pitch')
    return act(token, { boundS: 1 }, async ctx => {
      await lookNow(() => isPos(a.pos) ? bot.lookAt(vec(a.pos), true) : bot.look(a.yaw, a.pitch, true))
      return { status: 'ok' }
    })
  }

  // Waits `ms` (clamped to 0..WAIT_MAX_MS, scaled by timeScale) without touching the bot; a cut rejects at once.
  const wait = async (token, a = {}) => {
    const ms = Math.min(Math.max(isNum(a.ms) ? a.ms : 0, 0), WAIT_MAX_MS)
    // the act's own time bound is the wait: it resolves ok when the bound passes, and a cut rejects before that
    return act(token, { boundS: ms / 1000, onTimeout: () => ({ status: 'ok' }) }, () => new Promise(() => {}))
  }

  // ---- body events ----

  const emit = event => listeners.forEach(fn => fn(event))
  const inventoryNow = () => inventory().map(i => ({ name: i.name, count: i.count, slot: i.slot }))

  // Wires one bot's events to the listeners; returns the function that unwires them.
  const bindEvents = target => {
    let lastHealth = target.health
    let respawning = false
    // The server clears the slots of an instant death (/kill, void, damage) before the death event is read, so the
    // died record is built from the last snapshot of a living body: taken on every health event above zero and about
    // once a second of physics ticks. Lava and suffocation deaths keep their slots, the live inventory is used then.
    let snapshot = inventoryNow()
    let ticks = 0
    const remember = () => { if (target.health > 0) snapshot = inventoryNow() }
    const handlers = {
      physicsTick: () => { if (++ticks % 20 === 0) remember() },
      health: () => {
        remember()
        if (target.health < lastHealth) emit({ kind: 'hurt', health: target.health, food: target.food })
        lastHealth = target.health
      },
      // bot.experience still holds the pre-death values here; the server resets it in a later packet.
      death: () => { stopWalking(target); emit({
        kind: 'died',
        pos: here(),
        inventory: inventoryNow().length > 0 ? inventoryNow() : snapshot,
        experience: { level: target.experience?.level ?? 0, points: target.experience?.points ?? 0 }
      }) },
      respawn: () => { stopWalking(target); respawning = true },
      chat: (from, message) => emit({ kind: 'chat', from, message }),
      wake: () => emit({ kind: 'woke' }),
      playerCollect: (collector, collected) => {
        if (collector !== target.entity) return
        const item = collected && droppedItem(target, collected)
        if (item) emit({ kind: 'picked-up', item: item.name, count: item.count })
      },
      spawn: () => {
        emit({ kind: 'spawned' })
        if (!respawning) return
        respawning = false
        emit({ kind: 'respawned', pos: here(), dimension: target.game?.dimension })
      },
      end: reason => { down = true; emit({ kind: 'disconnected', reason: String(reason) }) },
      kicked: reason => { down = true; emit({ kind: 'disconnected', reason: JSON.stringify(reason) }) },
      // an unhandled 'error' on an EventEmitter throws and takes the process down; report it instead
      error: err => emit({ kind: 'error', reason: String(err?.message ?? err) })
    }
    Object.entries(handlers).forEach(([name, fn]) => target.on(name, fn))
    return () => Object.entries(handlers).forEach(([name, fn]) => target.removeListener(name, fn))
  }
  let down = false
  let unbind = bindEvents(bot)
  const onBodyEvent = listener => {
    listeners.add(listener)
    pending.splice(0).forEach(e => listener(e)) // events from before anyone listened (createPrimitives' wait)
    return () => listeners.delete(listener)
  }

  // ---- offline ----

  // wait that a cut or close can end early; resolves true when it ran its full course
  const waitOrWake = (ms, call) => new Promise(resolve => {
    const timer = setTimeout(() => resolve(true), ms)
    call.wake = () => { clearTimeout(timer); resolve(false) }
  })

  // waits for the fresh bot's world; a timeout is a warn event and the bot is used anyway. null when close() came first.
  const awaitWorld = async fresh => {
    if (!await waitForWorld(fresh, { timeoutMs: worldTimeoutMs })) emit({ kind: 'world-not-loaded', ms: worldTimeoutMs })
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
      if (fresh.b) return awaitWorld(fresh.b)
      if (attempt >= RECONNECT_TRIES) throw fresh.error
      await sleepMs(RECONNECT_RETRY_MS * timeScale)
    }
  }

  const adopt = fresh => {
    unbind()
    bot = fresh
    unbind = bindEvents(bot)
    view?.attach(bot)
    down = false
    emit({ kind: 'online', pos: here() })
  }

  // After an unplanned disconnect: the same reconnect path offline uses (3 tries). Shared by concurrent callers;
  // true when the body is back, false (with a reconnect-failed event) when it is not or close() came first.
  let recovering = null
  const recover = () => {
    if (!reconnect) return Promise.resolve(false)
    recovering ??= reconnectBot()
      .then(fresh => { if (fresh) adopt(fresh); return Boolean(fresh) })
      .catch(error => { emit({ kind: 'reconnect-failed', reason: String(error?.message ?? error) }); return false })
      .finally(() => { recovering = null })
    return recovering
  }

  // An acting primitive while the bot is down: reconnect first; resolves 'disconnected' when that fails. A stale
  // token still rejects with cut before anything else. While `offline` is bringing the body back (a cut ended its
  // wait) the call waits for that reconnect instead of acting on the quit bot.
  const whenUp = fn => async (token, a) => {
    if (!isOwner(token)) throw cutError()
    if (away) await away
    if (!isOwner(token)) throw cutError()
    if (down && !await recover()) return { status: 'disconnected' }
    return fn(token, a)
  }

  // Leaves the server for `ms`, then comes back with the same connection params. The body is offline (isOffline,
  // sensing says so) until the fresh bot is adopted. A cut ends the wait early but the body is still brought back
  // first, then the call resolves 'cut'; close() cancels the reconnect and resolves 'closed'.
  const offline = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(a.ms === undefined || a.ms === null || (isNum(a.ms) && a.ms >= 0), 'offline needs ms, a number of milliseconds of at least 0')
    if (!reconnect) return { status: 'unsupported' }
    const ms = Math.floor(Math.min(a.ms ?? OFFLINE_DEFAULT_MS, OFFLINE_MAX_MS))
    const call = { token, cut: () => call.wake?.(), wake: null }
    let release
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
      if (away) down = true // no bot came back: the next acting call tries the reconnect itself
      away = null
      release()
    }
  }

  const close = async () => {
    closed = true
    setOwner(null)
    await view?.detach()
    view?.stop()
    bot.quit()
  }

  const acting = Object.fromEntries(Object.entries({ moveTo, dig, place, jumpPlace, collect, inspectContainer, transfer, equip, toss, eat, attack, sleep, look, swim })
    .map(([name, fn]) => [name, whenUp(fn)]))
  return { setOwner, isOwner, self, entities, blocks, blockAt, ...acting, wait, isOffline, offline, onBodyEvent, close }
}

// The README's factory: connects, resolves once spawned.
// `connect` and `timeScale` exist for tests: a stand-in for connectBot, and shrunken time bounds.
// `opts.view` ({stateDir, agent, world, onEvent}) turns on the view dump (docs/view-format.md); BODY_VIEW=0 turns it off.
export async function createPrimitives ({ view: viewOpts, ...opts }, { connect = connectBot, timeScale = 1, worldTimeoutMs = WORLD_TIMEOUT_MS } = {}) {
  const view = viewOpts ? createView(viewOpts) : null
  const bot = await connect(opts)
  const pending = await waitForWorld(bot, { timeoutMs: worldTimeoutMs }) ? [] : [{ kind: 'world-not-loaded', ms: worldTimeoutMs }]
  return createPrimitivesFromBot(bot, { timeScale, reconnect: () => connect(opts), view, worldTimeoutMs, pending })
}
