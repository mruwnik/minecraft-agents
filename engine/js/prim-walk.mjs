// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// Walking for the primitives: the abortable goto with its stall watch and step-up, looks, and the centring sneak.

import vec3 from 'vec3'
import { POLL_MS, PLAN_REASONS, PROGRESS_BLOCKS, STALL_S, STILL_BLOCKS, STILL_S, CLIMBABLE, STEP_RISE, CENTRE_TOLERANCE, CENTRE_S, CENTRE_SPEED, LOOK_TICK_MS, STEP_S, STEP_ATTEMPTS, sleepMs, dist, cell, vec } from './prim-base.mjs'

const { Vec3 } = vec3

export function createWalk (env) {
  const { timeScale } = env
  // a walk toward `goal`, abortable; resolves {reached: true} when the pathfinder reached it, else {reached: false,
  // reason}: 'noPath' (goto resolved on an empty noPath update, or was rejected NoPath), 'planTimeout' (rejected
  // Timeout), 'stalled' when the body made no progress for STALL_S (unless `stall` is off); no reason for any other
  // rejection
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
      const done = () => { clearTimeout(timer); env.bot.off('physicsTick', done); resolve() }
      const timer = setTimeout(done, Math.max(1, LOOK_TICK_MS * timeScale))
      env.bot.on('physicsTick', done)
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
  // maxDropDown caps the drops of this walk's path (the movements' own, 4 by default, comes back after it).
  const walk = async (ctx, goal, { stall = true, maxDropDown } = {}) => {
    const walking = env.bot
    const movements = walking.pathfinder.movements
    const dropDown = movements?.maxDropDown
    if (movements && maxDropDown !== undefined) movements.maxDropDown = maxDropDown
    ctx.onAbort(() => stopWalking(walking))
    let path = null
    let onStuck = () => {}
    let emptyNoPath = false // goto resolves, not rejects, on a noPath update with an empty path
    let started = false // time only counts from the first path_update of the current goto: planning may take seconds
    let still = { pos: walking.entity.position.clone(), at: Date.now() }
    const onUpdate = result => {
      if (!started) { started = true; anchor = { pos: walking.entity.position.clone(), at: Date.now() }; still = { ...anchor } }
      if (result?.status === 'success' || result?.status === 'partial') path = result.path
      emptyNoPath = result?.status === 'noPath' && !result.path?.length
    }
    const onReset = reason => { if (reason === 'stuck') onStuck() }
    let anchor = { pos: walking.entity.position.clone(), at: Date.now() }
    let onStall = () => {}
    const poll = setInterval(() => {
      if (!started) return
      const pos = walking.entity.position
      const now = Date.now()
      if (dist(pos, anchor.pos) >= PROGRESS_BLOCKS) anchor = { pos: pos.clone(), at: now }
      else if (stall && now - anchor.at >= STALL_S * 1000 * timeScale) return onStall()
      if (dist(pos, still.pos) > STILL_BLOCKS) still = { pos: pos.clone(), at: now }
      else if (stall && now - still.at >= STILL_S * 1000 * timeScale && !walking.entity.isInWater && !CLIMBABLE.test(walking.blockAt(vec(cell(pos)))?.name ?? '')) onStall()
    }, Math.max(1, POLL_MS * timeScale))
    ctx.onAbort(() => clearInterval(poll)) // a timed-out or cut call never reaches the finally
    walking.on('path_update', onUpdate)
    walking.on('path_reset', onReset)
    try {
      for (let attempts = 0; ; attempts++) {
        ctx.alive()
        const stuck = new Promise(resolve => {
          onStuck = () => { const target = attempts < STEP_ATTEMPTS ? stepUpTarget(path, walking) : null; if (target) resolve(target) }
        })
        const stalled = new Promise(resolve => { onStall = () => resolve({ reached: false, reason: 'stalled' }) })
        emptyNoPath = false
        started = false
        const gone = walking.pathfinder.goto(goal).then(
          () => emptyNoPath ? { reached: false, reason: 'noPath' } : { reached: true },
          err => ({ reached: false, ...(PLAN_REASONS[err?.name] && { reason: PLAN_REASONS[err.name] }) })
        )
        const ended = await Promise.race([gone, stuck, stalled])
        ctx.alive()
        if ('reached' in ended) return ended
        await stepUp(ctx, walking, ended)
        ctx.alive()
      }
    } finally {
      clearInterval(poll)
      walking.off('path_update', onUpdate)
      walking.off('path_reset', onReset)
      if (movements && maxDropDown !== undefined) movements.maxDropDown = dropDown
      stopWalking(walking)
    }
  }
  return { stopWalking, lookNow, waitUntil, centreBody, walk }
}
