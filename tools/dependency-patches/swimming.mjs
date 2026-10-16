// Why JavaScript: patches prismarine-physics source in node_modules (Mineflayer boundary).
// prismarine-physics 1.11.1 index.js has no swimming: a sprint in water moved like a paddle (inertia 0.8, water gravity,
// 1.96 b/s) and the look pitch did nothing. Vanilla (Player.travel, LivingEntity water travel) swims while sprinting with
// the eyes under water: inertia 0.9, no gravity, vy pulled toward the look's y, so about 3.9 b/s in any direction.
const HELPER_AT = '  function moveEntityWithHeading (entity, world, strafe, forward) {\n'
const HELPER = '  // patched by bot/patch-deps.mjs: vanilla swimming\n' +
  '  const isWaterAt = (world, p) => {\n' +
  '    const b = world.getBlock(p.floored())\n' +
  '    return Boolean(b && (waterIds.includes(b.type) || waterLike.has(b.type) || b.isWaterlogged))\n' +
  '  }\n\n'
const INERTIA = '      let horizontalInertia = inertia\n'
const SWIM = '      const swimming = entity.isInWater && entity.control.sprint && isWaterAt(world, pos.offset(0, 1.62, 0))\n' +
  '      if (swimming) {\n' +
  '        const lookY = Math.sin(entity.pitch)\n' +
  '        if (lookY <= 0 || entity.control.jump || isWaterAt(world, pos.offset(0, 0.9, 0))) vel.y += (lookY - vel.y) * (lookY < -0.2 ? 0.085 : 0.06)\n' +
  '      }\n' +
  '      let horizontalInertia = swimming ? 0.9 : inertia\n'
const GRAVITY = '      vel.y -= (entity.isInWater ? physics.waterGravity : physics.lavaGravity) * gravityMultiplier\n'
const NO_GRAVITY = '      if (!swimming) vel.y -= (entity.isInWater ? physics.waterGravity : physics.lavaGravity) * gravityMultiplier\n'

const EDITS = [[HELPER_AT, HELPER + HELPER_AT], [INERTIA, SWIM], [GRAVITY, NO_GRAVITY]]

export const patchSwimming = source => source.includes(HELPER)
  ? { status: 'already', source }
  : EDITS.every(([from]) => source.split(from).length === 2)
    ? { status: 'patched', source: EDITS.reduce((s, [from, to]) => s.replace(from, to), source) }
    : { status: 'anchor missing', source }

// the source as installed, before patchSwimming
export const unpatchSwimming = source => EDITS.reduce((s, [from, to]) => s.replace(to, from), source)
