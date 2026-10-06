// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// Body movement and bodily acts for the primitives: moveTo, swim, jumpPlace, attack, sleep, look, mount, dismount.

import { isInteractable } from './blocks.mjs'
import { leaveBed } from './bed.mjs'
import { mountVehicle, dismountVehicle } from './vehicle.mjs'
import vec3 from 'vec3'
import pf from 'mineflayer-pathfinder'
import { REACH, ATTACK_REACH, POLL_MS, HURT_WAIT_MS, JUMP_PLACE_MAX, JUMP_PLACE_BLOCK_S, RISE_WAIT_MS, LAND_WAIT_MS, SWIM_DEFAULT_MS, SWIM_MAX_MS, HOP_RANGE, sleepMs, dist, center, isNum, isPos, cell, vec, cutError, failed, need } from './prim-base.mjs'

const { Vec3 } = vec3
const { goals } = pf

// GoalNearXZ takes any y, which walked a far-target hop down into caves: the end must also be open to the sky, read from
// blocks (the sky light mineflayer reads is wrong on this version). A canopy of leaves still counts as open.
// `getBot` is read at each check, a reconnect replaces the bot. `roof` caches, per column, the highest sky-blocking y.
class GoalSurfaceHop extends goals.GoalNearXZ {
  constructor (x, z, range, getBot) {
    super(x, z, range)
    this.getBot = getBot
    this.roofs = new Map()
  }

  // highest y of a block that shuts out the sky, -Infinity when none, Infinity when an unloaded cell is met first
  roof (x, z) {
    const key = `${x},${z}`
    if (this.roofs.has(key)) return this.roofs.get(key)
    const bot = this.getBot()
    const minY = bot.game?.minY ?? -64
    const top = minY + (bot.game?.height ?? 384) - 1
    let found = -Infinity
    for (let y = top; y >= minY; y--) {
      const block = bot.blockAt(new Vec3(x, y, z))
      if (!block) { found = Infinity; break }
      if (block.boundingBox !== 'block' || block.name.endsWith('leaves')) continue
      found = y
      break
    }
    this.roofs.set(key, found)
    return found
  }

  open (node) {
    return node.y > this.roof(Math.floor(node.x), Math.floor(node.z))
  }

  isEnd (node) {
    return super.isEnd(node) && this.open(node)
  }
}

export function createMove (env) {
  const { act, isOwner, here, eye, inventory, standingCell, hostilesNear, timeScale, lookNow, centreBody, walk } = env
  // ---- acting ----

  const moveTo = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'moveTo needs pos {x, y, z}')
    const { range = 1, timeoutS = 20, maxDistance = 64 } = a
    const target = cell(a.pos)
    const start = here()
    const before = dist(start, target)
    const capped = before > maxDistance
    const hx = Math.round(start.x + (target.x - start.x) * maxDistance / before)
    const hz = Math.round(start.z + (target.z - start.z) * maxDistance / before)
    const goal = capped
      ? new GoalSurfaceHop(hx, hz, HOP_RANGE, () => env.bot)
      : new goals.GoalNear(target.x, target.y, target.z, range)
    // `reached` is only what the pathfinder promised: goto resolves on a noPath update with an empty path, so the
    // goal itself is checked against where the body stands.
    const satisfied = () => !capped && goal.isEnd(standingCell())
    let hop = null
    const outcome = (reached) => {
      const distance = dist(here(), target)
      const base = { pos: here(), distance, ...(hop && { hop }) }
      if (reached.reached && satisfied()) return { status: 'arrived', ...base }
      const { reason } = reached
      if (distance < before - 1) return { status: 'partial', ...(reason && { reason }), ...base }
      return { status: 'blocked', ...(reason && { reason }), ...base }
    }
    return act(token, { boundS: Math.min(timeoutS, 60), onTimeout: () => outcome({ reached: false, reason: 'timeout' }) }, async ctx => {
      const first = await walk(ctx, goal)
      // a hop that starts underground may have no surface to end on: head for the XZ point instead
      if (!capped || first.reason !== 'noPath' || goal.open(standingCell())) return outcome(first)
      hop = 'xz'
      return outcome(await walk(ctx, new goals.GoalNearXZ(hx, hz, 2)))
    })
  }

  const oxygenNow = () => env.bot.oxygenLevel ?? 20
  const headUnderwater = () => env.bot.blockAt(vec(cell(eye())))?.name === 'water'

  // Holds jump until the head is out of the water. The pathfinder has no swim-up move, so a submerged body cannot
  // surface with moveTo. Jump is released on every exit: surfaced, timeout, cut, error.
  // Dry: the feet cell holds no water. Standing: dry, and the cell under it is a full block.
  const dry = () => env.bot.blockAt(vec(cell(here())))?.name !== 'water'
  const standing = () => dry() && env.bot.blockAt(vec({ ...cell(here()), y: cell(here()).y - 1 }))?.boundingBox === 'block'

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
      const press = control => { if (!pressed.has(control)) { pressed.add(control); env.bot.setControlState(control, true) } }
      const release = () => {
        for (const control of [...pressed]) { pressed.delete(control); env.bot.setControlState(control, false) }
      }
      ctx.onAbort(release)
      try {
        if (toward) {
          // climb out toward the target: the pathfinder has no move from floating feet onto a rim just above
          const there = center(cell(toward))
          const arrived = standing
          if (arrived()) return result('landed')
          await env.bot.lookAt(vec(there), true)
          ctx.alive()
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
      const release = () => { if (pressed) { pressed = false; env.bot.setControlState('jump', false) } }
      ctx.onAbort(release)
      const pause = async ms => { await sleepMs(ms * timeScale); ctx.alive() }
      const solidAt = c => env.bot.blockAt(vec(c))?.boundingBox === 'block'
      try {
        while (placed < count) {
          const item = inventory().find(i => i.name === a.item)
          if (!item) return outcome('no-item')
          const start = cell(here())
          const below = env.bot.blockAt(vec({ ...start, y: start.y - 1 }))
          if (below?.boundingBox !== 'block') return outcome('no-support')
          if (solidAt({ ...start, y: start.y + 2 })) return outcome('no-headroom')
          // centring gets the body off a wall, where the jump never happens; the server does not refuse an off-centre
          // placement, so a failure to centre is ignored (a body that cannot jump ends not-raised)
          await centreBody(ctx, env.bot, start)
          await env.bot.equip(item, 'hand')
          ctx.alive()
          await env.bot.look(env.bot.entity.yaw ?? 0, -Math.PI / 2, true)
          ctx.alive()
          pressed = true
          env.bot.setControlState('jump', true)
          const riseDeadline = Date.now() + RISE_WAIT_MS * timeScale
          while (env.bot.entity.position.y < start.y + 1.01 && Date.now() < riseDeadline) await pause(20)
          if (env.bot.entity.position.y < start.y + 1.01) return outcome('not-raised')
          // placeBlock's own unforced lookAt turns gradually when the body is slightly off-centre and delays the packet
          // ~1 s, until the body has fallen back into the cell and the server refuses; 'ignore' sends it at once
          const sneakOn = isInteractable(below.name)
          if (sneakOn) env.bot.setControlState('sneak', true)
          const failure = await env.bot._placeBlockWithOptions(below, new Vec3(0, 1, 0), { swingArm: 'right', forceLook: 'ignore' }).then(() => null, err => err)
          if (sneakOn) env.bot.setControlState('sneak', false)
          ctx.alive()
          release()
          if (failure) return outcome(`place-failed: ${String(failure.message ?? failure).slice(0, 100)}`)
          const landDeadline = Date.now() + LAND_WAIT_MS * timeScale
          while (!env.bot.entity.onGround && Date.now() < landDeadline) await pause(20)
          if (env.bot.entity.position.y < start.y + 0.9) return outcome('not-raised')
          placed += 1
        }
        return outcome()
      } finally {
        release()
      }
    })
  }
  const attack = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'attack needs an entity id')
    return act(token, { boundS: 1 }, async ctx => {
      const target = env.bot.entities[a.id]
      if (!target) return { status: 'gone' }
      if (dist(eye(), { x: target.position.x, y: target.position.y + (target.height ?? 1) / 2, z: target.position.z }) > ATTACK_REACH) return { status: 'out-of-reach' }
      let hurtSeen = false
      const onHurt = entity => { if (entity?.id === a.id) hurtSeen = true }
      let deadSeen = false
      const onDead = entity => { if (entity?.id === a.id) deadSeen = true }
      env.bot.on('entityDead', onDead)
      env.bot.on('entityHurt', onHurt) // before the swing, so a fast event is not missed
      try {
        await env.bot.attack(target)
        ctx.alive()
        const deadline = Date.now() + HURT_WAIT_MS * timeScale
        while (!hurtSeen && !deadSeen && env.bot.entities[a.id] && Date.now() < deadline) {
          await sleepMs(POLL_MS * timeScale)
          ctx.alive()
        }
      } finally {
        env.bot.removeListener('entityHurt', onHurt)
        env.bot.removeListener('entityDead', onDead)
      }
      const now = env.bot.entities[a.id]
      const health = typeof now?.health === 'number' ? now.health : undefined
      const killed = deadSeen || !now || (health !== undefined && health <= 0)
      return { status: killed ? 'killed' : 'hit', ...(health !== undefined && { health }), hurt: hurtSeen || killed }
    })
  }

  const sleep = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'sleep needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = env.bot.blockAt(vec(p))
      if (!block || !block.name.endsWith('_bed')) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      const { timeOfDay } = env.bot.time
      if (timeOfDay < 12542 || timeOfDay > 23460) return { status: 'not-night' }
      if (hostilesNear().length > 0) return { status: 'monsters-near' }
      ctx.onAbort(() => leaveBed(env.bot))
      ctx.alive()
      const failure = await env.bot.sleep(block).then(() => null, err => err)
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
      await lookNow(() => isPos(a.pos) ? env.bot.lookAt(vec(a.pos), true) : env.bot.look(a.yaw, a.pitch, true))
      return { status: 'ok' }
    })
  }
  // Boats, rafts, minecarts and rideable mobs (vehicle.mjs). Every wait is wall time: no physics tick runs mounted.
  const mount = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'mount needs an entity id')
    return act(token, { boundS: 2 }, ctx => mountVehicle(env.bot, ctx, a, { timeScale }))
  }

  const dismount = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(a.yaw == null || isNum(a.yaw), 'dismount yaw must be a number of degrees (0 south, 90 west)')
    need(a.pitch == null || isNum(a.pitch), 'dismount pitch must be a number of degrees')
    return act(token, { boundS: 2 }, ctx => dismountVehicle(env.bot, ctx, a, { timeScale }))
  }
  return { moveTo, swim, jumpPlace, attack, sleep, look, mount, dismount }
}
