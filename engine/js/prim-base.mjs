// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// Shared constants and helpers of the primitives modules (prim-*.mjs).

import vec3 from 'vec3'

const { Vec3 } = vec3
export const REACH = 4.5
export const ATTACK_REACH = 3.5
export const DROP_RADIUS = 2
export const DROP_WAIT_S = 1
export const DIG_MARGIN_S = 5 // slack over the expected dig time (latency, a tick of lag)
export const POLL_MS = 50
export const DAMAGE_FRESH_MS = 1000 // a damage packet older than this is not the cause of a health loss
export const HURT_WAIT_MS = 300 // attack waits this long for the server's entityHurt on the target
export const SETTLE_QUIET_MS = 150 // transfer closes its window only after this long without a slot update...
export const SETTLE_CAP_MS = 1500 // ...or this long in all
export const CONTAINER = /chest|barrel|shulker_box|furnace|smoker|hopper|dispenser|dropper|brewing_stand/
export const DESTS = ['hand', 'off-hand', 'head', 'torso', 'legs', 'feet']
export const DEFAULT_RADIUS = 16
export const KINDS = ['hostile', 'passive', 'player', 'item', 'other']
export const OFFLINE_DEFAULT_MS = 5 * 60 * 1000
export const OFFLINE_MAX_MS = 10 * 60 * 1000
export const JUMP_PLACE_MAX = 8
export const JUMP_PLACE_BLOCK_S = 2
export const RISE_WAIT_MS = 800
export const LAND_WAIT_MS = 1000
export const SWIM_DEFAULT_MS = 3000
export const SWIM_MAX_MS = 10000
export const RECONNECT_TRIES = 3
export const RECONNECT_RETRY_MS = 5000
// after an unplanned drop (kick, socket end, server restart) the body reconnects by itself: the first try at once,
// then after each failure a wait that doubles from the first to the cap, until it is back or closed
export const RECONNECT_BACKOFF_FIRST_MS = 1000
export const RECONNECT_BACKOFF_MAX_MS = 60000
export const WORLD_TIMEOUT_MS = 10000
export const WORLD_POLL_MS = 50
export const PHYSICS_STALL_MS = 2000 // no physicsTick this long over an unloaded column: the body hangs frozen
export const STALL_POLL_MS = 250
export const SETTLE_MS = 1000 // senses count as trustworthy this long after the column under the body is loaded
export const TELEPORT_BLOCKS = 16 // a forced move farther than this is a teleport; smaller ones are server corrections
// Every bound body is 0.01 wider than mineflayer's 0.3: the server rejects every move of a body whose box touches a
// block face exactly (pressed against a step, or the side of a block it walks past) and sets it back to the same
// position about 20 times a second, indefinitely. Verified live against 26.1: 2 of 2 walks past one block on a flat
// stone pad stuck 25 s at 0.3, 3 of 3 took 0.7-1 s with no setbacks at 0.31 (same on a leaf-litter hillside where
// every moveTo came back blocked); a swim toward a rim, flush against its wall, only climbs out at 0.31.
export const BODY_HALF_WIDTH = 0.31
export const HOP_RANGE = 4 // a capped hop ends within this many blocks (XZ) of its point on the line to the target
export const PLAN_REASONS = { NoPath: 'noPath', Timeout: 'planTimeout' } // goto's rejection names that say why it gave up
export const PROGRESS_BLOCKS = 1 // a walk must get this far (3D) from its anchor...
export const STALL_S = 8 // ...within this long, or it ends stalled (the pathfinder's own stuck reset plus one step-up fit in it)
// A wedged body stands perfectly still, a bobbing or climbing one does not; 4 s leaves room for the pathfinder's 3.5 s stuck reset.
export const STILL_BLOCKS = 0.1
export const STILL_S = 4
export const CLIMBABLE = /^(ladder|vine|scaffolding|weeping_vines(_plant)?|twisting_vines(_plant)?|cave_vines(_plant)?)$/
export const POSE_SLEEPING = 2
// Step-up out of a 1-deep hole when the pathfinder stalls flush against the ledge (see stepUp).
export const STEP_RISE = 1.0
export const CENTRE_TOLERANCE = 0.1
export const CENTRE_S = 1
export const CENTRE_SPEED = 0.005 // blocks per tick: centred means within tolerance and (nearly) stopped
export const LOOK_TICK_MS = 100 // a look waits for the next physics tick (the rotation goes out there), at most this long
export const STEP_S = 1.5
export const STEP_ATTEMPTS = 2
export const FLAG_ON_FIRE = 0x01

export const WAIT_MAX_MS = 10000
export const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))
// Resolves true once the column under the body is loaded (blockAt there is non-null), false after `timeoutMs`.
// A body that acts before its chunk arrives sees no roof, no bed and no other players.
export async function waitForWorld (bot, { timeoutMs = WORLD_TIMEOUT_MS, pollMs = WORLD_POLL_MS, stop = () => false } = {}) {
  const deadline = Date.now() + timeoutMs
  while (!bot.blockAt(bot.entity.position)) {
    if (stop() || Date.now() >= deadline) return false
    await sleepMs(pollMs)
  }
  return true
}
export const xyz = v => ({ x: v.x, y: v.y, z: v.z })
export const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
export const center = p => ({ x: p.x + 0.5, y: p.y + 0.5, z: p.z + 0.5 })
export const isAir = name => name === 'air' || name.endsWith('_air')
export const isNum = n => typeof n === 'number' && Number.isFinite(n)
export const isPos = p => Boolean(p) && isNum(p.x) && isNum(p.y) && isNum(p.z)
export const cell = p => ({ x: Math.floor(p.x), y: Math.floor(p.y), z: Math.floor(p.z) })
export const vec = p => new Vec3(p.x, p.y, p.z)

export const codedError = (code, message) => Object.assign(new Error(message), { code, [code === 'cut' ? 'cut' : 'badArgs']: true })
export const cutError = () => codedError('cut', 'cut: the ownership token no longer matches')

// Minecraft (F3) degrees <-> mineflayer radians. Yaw is normalised to 0..360, pitch clamped to -90..90.
export const mcToMineflayerLook = ({ yaw, pitch }) => ({ yaw: Math.PI - yaw * Math.PI / 180, pitch: -pitch * Math.PI / 180 })
export const mineflayerToMcLook = ({ yaw, pitch }) => {
  const deg = 180 - yaw * 180 / Math.PI
  return { yaw: ((deg % 360) + 360) % 360, pitch: Math.min(90, Math.max(-90, -pitch * 180 / Math.PI)) }
}
export const badArgs = message => codedError('bad-args', message)
export const isCut = err => err?.code === 'cut'
// a mineflayer rejection is a domain failure: a status, never a throw (only cut and bad-args reject)
export const failed = err => ({ status: 'failed', reason: String(err?.message ?? err).slice(0, 200) })
export const BUCKET_WAIT_S = 2
export const need = (ok, message) => { if (!ok) throw badArgs(message) }

export const entityKind = e => {
  if (e.type === 'player') return 'player'
  if (e.name === 'item' || e.name === 'item_stack' || e.displayName === 'Item') return 'item'
  if (e.type === 'hostile' || /hostile/i.test(e.kind ?? '')) return 'hostile'
  if (['passive', 'animal', 'ambient', 'water_creature'].includes(e.type) || /passive|animal/i.test(e.kind ?? '')) return 'passive'
  return 'other'
}

// A slot value as the client library leaves it: a prismarine Item {name|type, count}, a network slot
// {itemId, itemCount, present?} (1.13+) or {blockId, itemCount} (older), possibly without a count.
export const slotLike = m => m !== null && typeof m === 'object' && [m.itemId, m.blockId, m.type, m.name].some(v => v !== undefined)

// {name, count} for a slot value; null for an empty slot; name 'unknown' for an id the registry lacks.
export const stackOf = (bot, slot) => {
  if (!slotLike(slot) || slot.present === false) return null
  const count = slot.itemCount ?? slot.count ?? 1
  if (!(count > 0)) return null
  const id = slot.itemId ?? slot.blockId ?? slot.type
  return { name: slot.name ?? bot.registry?.items?.[id]?.name ?? 'unknown', count }
}

// The index of a named entity-metadata field: the registry knows it per entity type, the older fixed layout is the fallback.
export const metaIndex = (bot, e, key, fallback) => {
  const at = bot.registry?.entitiesByName?.[e.name]?.metadataKeys?.indexOf(key)
  return at >= 0 ? at : fallback
}
export const metaValue = (bot, e, key, fallback) => e.metadata?.[metaIndex(bot, e, key, fallback)]
export const burning = (bot, e) => ((metaValue(bot, e, 'shared_flags', 0) ?? 0) & FLAG_ON_FIRE) !== 0
export const lyingDown = (bot, e) => metaValue(bot, e, 'pose', 6) === POSE_SLEEPING

export const attempt = f => { try { return f() } catch { return null } }

// The night's sleep count every player sees in the action bar ("1/7 players sleeping"; "Sleeping through this night"
// once enough sleep): {sleeping, needed} or {skipping: true}; null for any other line. The server sends it only when
// the count changes (a join changes it).
export const countOf = w => Number(typeof w === 'object' && w !== null && w.text !== undefined && w.text !== '' ? w.text : String(w))
export const sleepStatusOf = msg => {
  if (msg?.translate === 'sleep.skipping_night') return { skipping: true }
  if (msg?.translate !== 'sleep.players_sleeping') return null
  const [sleeping, needed] = (msg.with ?? []).map(countOf)
  return Number.isFinite(sleeping) && Number.isFinite(needed) ? { sleeping, needed } : null
}

// The action bar line a player sees when a bed or anchor sets the respawn point ("Respawn point set"). The server
// sends it only when the point changes.
export const spawnSetMessage = msg => msg?.translate === 'block.minecraft.set_spawn'

// getDroppedItem throws or returns null when the library cannot read the slot (it reads one fixed metadata index),
// so fall back to scanning the metadata for any slot-shaped value.
export const droppedItem = (bot, e) =>
  stackOf(bot, attempt(() => e.getDroppedItem?.())) ?? stackOf(bot, Object.values(e.metadata ?? {}).find(slotLike))

export const itemCounts = items => items.reduce((acc, i) => ({ ...acc, [i.name]: (acc[i.name] ?? 0) + i.count }), {})
export const gained = (before, after) => Object.entries(after)
  .map(([name, count]) => ({ name, count: count - (before[name] ?? 0) }))
  .filter(g => g.count > 0)

// Builds the primitives over an already spawned bot. `timeScale` multiplies every time bound (tests shrink it).
// `reconnect` (internal; createPrimitives passes it) makes a fresh spawned bot with the same connection params and
