const air = name => ['air', 'cave_air', 'void_air'].includes(name)

// Correct the pathfinder's residual x/z error while remaining on one known
// work cell. This never steps, jumps, digs, places, or transfers to a neighbor.
export async function centerStand ({ bot, Vec3, target, support, alive, maxTicks = 24 }) {
  if (![target?.x, target?.y, target?.z].every(Number.isSafeInteger) || typeof support !== 'string' || !support) throw new Error('center_work_stand needs integer x/y/z and an owned support name')
  if (!Number.isInteger(maxTicks) || maxTicks < 1 || maxTicks > 24) throw new Error('center_work_stand tick limit must be 1..24')
  const centerX = target.x + 0.5, centerZ = target.z + 0.5
  const started = Date.now(), start = bot.entity.position.clone()
  if (bot.vehicle) throw new Error('center_work_stand requires no vehicle')
  if (Math.floor(start.x) !== target.x || Math.floor(start.z) !== target.z || Math.abs(start.y - target.y) > 0.12) {
    if (support === 'scaffolding') throw new Error('center_work_stand cannot stand on an airborne or off-cell scaffold position')
    throw new Error('center_work_stand only adjusts the current supported work cell')
  }
  if (support === 'scaffolding') {
    // Minecraft can leave onGround false for a player settled on a climbable
    // scaffold deck. Verify the exact owned support, feet height, and headroom
    // over consecutive physics ticks. Never use sneak here: on scaffolding it
    // is the descent control. If the pathfinder did not already center the
    // body, refuse safely so the caller can choose another work platform.
    bot.pathfinder.setGoal(null)
    const scaffoldPosition = () => {
      alive()
      const p = bot.entity.position
      if (bot.vehicle) throw new Error('center_work_stand lost its scaffold work cell; stop and inspect')
      if (Math.floor(p.x) !== target.x || Math.floor(p.z) !== target.z || Math.abs(p.y - target.y) > 0.05) throw new Error('center_work_stand is not settled on the verified scaffold deck')
      const floor = bot.blockAt(new Vec3(target.x, target.y - 1, target.z))
      if (floor?.name !== 'scaffolding') throw new Error('center_work_stand scaffold support changed; retain the current platform')
      if (![0, 1].every(dy => air(bot.blockAt(new Vec3(target.x, target.y + dy, target.z))?.name))) throw new Error('center_work_stand cannot stand on a scaffold climb cell as a work deck; feet or head cell is occupied')
      return p
    }
    try {
      let previous = null, stable = 0
      for (let tick = 0; tick < Math.min(maxTicks, 6); tick++) {
        const p = scaffoldPosition()
        if (previous && Math.hypot(p.x - previous.x, p.y - previous.y, p.z - previous.z) <= 0.01) stable++
        else stable = 0
        if (stable >= 1) {
          if (Math.hypot(centerX - p.x, centerZ - p.z) > 0.05) throw new Error('center_work_stand cannot center this scaffold deck safely; choose another platform')
          return { x: p.x, y: p.y, z: p.z }
        }
        previous = p.clone()
        await bot.waitForTicks(1)
      }
      throw new Error('center_work_stand scaffold deck did not settle; retain the current platform')
    } finally {
      bot.setControlState('forward', false)
      bot.setControlState('sneak', false)
      bot.setControlState('jump', false)
    }
  }
  if (!bot.entity.onGround) throw new Error('center_work_stand requires grounded feet and no vehicle')
  const validate = () => {
    alive()
    const p = bot.entity.position
    if (bot.vehicle || !bot.entity.onGround) throw new Error('center_work_stand lost grounded feet; stop and inspect')
    if (Math.floor(p.x) !== target.x || Math.floor(p.z) !== target.z || Math.abs(p.y - target.y) > 0.12) throw new Error('center_work_stand drifted from its verified work cell; stop and inspect')
    if (Math.hypot(p.x - start.x, p.z - start.z) > 0.75) throw new Error('center_work_stand exceeded its bounded same-cell movement')
    const floor = bot.blockAt(new Vec3(target.x, target.y - 1, target.z))
    if (floor?.name !== support || floor.boundingBox !== 'block') throw new Error('center_work_stand support changed; retain the current platform')
    if (![0, 1].every(dy => air(bot.blockAt(new Vec3(target.x, target.y + dy, target.z))?.name))) throw new Error('center_work_stand needs clear feet and head cells')
    return p
  }
  bot.pathfinder.setGoal(null)
  try {
    // Let any pathfinder momentum settle before aiming a one-cell correction.
    for (let tick = 0; Math.hypot(bot.entity.velocity?.x ?? 0, bot.entity.velocity?.z ?? 0) > 0.025 && tick < 8; tick++) {
      validate()
      await bot.waitForTicks(1)
      validate()
    }
    if (Math.hypot(bot.entity.velocity?.x ?? 0, bot.entity.velocity?.z ?? 0) > 0.025) throw new Error('center_work_stand could not settle movement on its work cell')
    for (let tick = 0; tick < maxTicks; tick++) {
      const p = validate(), dx = centerX - p.x, dz = centerZ - p.z
      if (Math.hypot(dx, dz) <= 0.045) break
      if (Date.now() - started > 2000) throw new Error('center_work_stand exceeded its 2 second limit')
      await bot.lookAt(new Vec3(centerX, p.y + 1.62, centerZ), true)
      validate()
      bot.setControlState('sneak', true)
      bot.setControlState('forward', true)
      try { await bot.waitForTicks(1) } finally { bot.setControlState('forward', false); bot.setControlState('sneak', false) }
      const moved = validate()
      const after = Math.hypot(centerX - moved.x, centerZ - moved.z)
      if (after > Math.hypot(dx, dz) + 0.035) throw new Error('center_work_stand moved away from the work-cell center')
      await bot.waitForTicks(1)
      validate()
    }
    const p = validate()
    if (Math.hypot(centerX - p.x, centerZ - p.z) > 0.05) throw new Error('center_work_stand could not center inside the verified work cell')
    return { x: p.x, y: p.y, z: p.z }
  } finally {
    bot.setControlState('forward', false)
    bot.setControlState('sneak', false)
  }
}
