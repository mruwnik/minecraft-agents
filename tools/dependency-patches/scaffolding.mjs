// Why JavaScript: patches JS physics dependency sources in node_modules (Mineflayer boundary).
// Context-dependent scaffold collision follows ScaffoldingBlock#getCollisionShape:
// https://github.com/mahtomedi/minecraft/blob/main/src/main/java/net/minecraft/world/level/block/ScaffoldingBlock.java
// A player above the deck collides unless descending; inside the column the
// outline posts/deck are not collision boxes. Suspended bottom blocks retain
// their 2/16 bottom platform even while descending. Keep ordinary blocks intact.
function baseScaffoldingPhysics (source) {
  const marker = '// patched by bot/patch-deps.mjs: context-dependent scaffolding collision'
  if (source.includes(marker)) return { status: 'already', source }
  const signature = '  function getSurroundingBBs (world, queryBB) {'
  const shapes = '            for (const shape of block.shapes) {'
  const vines = '    if (block.type === ladderId || block.type === vineId) { return true }'
  const clamp = '        vel.y = Math.max(vel.y, entity.control.sneak ? 0 : -physics.ladderMaxSpeed)'
  const moveStart = source.indexOf('  function moveEntity (entity, world, dx, dy, dz) {')
  const moveEnd = source.indexOf('  function applyHeading', moveStart)
  if (!source.includes(signature) || !source.includes(shapes) || !source.includes(clamp) || !source.includes(vines) || moveStart < 0 || moveEnd < 0 || !source.includes('const scaffoldingId =') || !source.includes('module.exports = { Physics, PlayerState }')) return { status: 'anchor missing', source }
  const originalMovement = source.slice(moveStart, moveEnd)
  const movementQueries = ['queryBB', 'getPlayerBB(pos).offset(dx, 0, 0)', 'getPlayerBB(pos).offset(0, 0, dz)', 'getPlayerBB(pos).offset(dx, 0, dz)']
  if (!movementQueries.every(query => originalMovement.includes(`getSurroundingBBs(world, ${query})`)) || originalMovement.split('getSurroundingBBs(world, ').length - 1 !== 5) return { status: 'anchor missing', source }
  let patched = source.replace(signature, '  function getSurroundingBBs (world, queryBB, entity = null) {')
    .replace(shapes, `            ${marker}
            let collisionShapes = block.shapes
            if (block.type === scaffoldingId) {
              const feet = entity ? entity.pos.y : queryBB.minY
              const descending = Boolean(entity && entity.control.sneak)
              const properties = block.getProperties()
              if (feet >= blockPos.y + 1 - 1e-5 && !descending) {
                collisionShapes = block.shapes
              } else if (Number(properties.distance) !== 0 && properties.bottom === true && feet >= blockPos.y - 1e-5) {
                collisionShapes = [[0, 0, 0, 1, 0.125, 1]]
              } else {
                collisionShapes = []
              }
            }
            for (const shape of collisionShapes) {`)
    .replace(clamp, `        const onScaffolding = world.getBlock(pos)?.type === scaffoldingId
        vel.y = Math.max(vel.y, entity.control.sneak && !onScaffolding ? 0 : -physics.ladderMaxSpeed)`)
  // All collision queries inside movement (including stepping and edge safety)
  // need the same player context. Height adjustment and geometry-only probes
  // can use the queried feet as a conservative nonsneaking context.
  const begin = patched.indexOf('  function moveEntity (entity, world, dx, dy, dz) {')
  const end = patched.indexOf('  function applyHeading', begin)
  let movement = patched.slice(begin, end)
  for (const query of movementQueries) {
    movement = movement.replaceAll(`getSurroundingBBs(world, ${query})`, `getSurroundingBBs(world, ${query}, entity)`)
  }
  patched = patched.slice(0, begin) + movement + patched.slice(end)
  // Exact vanilla block tag, including cave vines; not an arbitrary plant suffix.
  // https://raw.githubusercontent.com/misode/mcmeta/data-json/data/minecraft/tags/block/climbable.json
  patched = patched.replace(vines, `    const climbableVine = ['weeping_vines', 'weeping_vines_plant', 'twisting_vines', 'twisting_vines_plant', 'cave_vines', 'cave_vines_plant'].some(name => blocksByName[name]?.id === block.type)
    if (block.type === ladderId || block.type === vineId || climbableVine) { return true }`)
    .replace('module.exports = { Physics, PlayerState }', 'module.exports = { Physics, PlayerState, supportsScaffolding: true, supportsClimbableVines: true }')
  return { status: 'patched', source: patched }
}

// The same descent decision in preview and execution; collision alone cannot
// descend while the body leaves sneak released. Keep this limited to a centered
// vertical scaffold leg, and release our sneak as soon as that leg ends.
const DESCENT = `const scaffoldHere = this.world.getBlock(state.pos.floored())
      const scaffoldBelow = this.world.getBlock(state.pos.floored().offset(0, -1, 0))
      const scaffoldDescending = nextPoint.y < state.pos.y - 0.1 && Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35 &&
        (scaffoldHere?.name === 'scaffolding' || scaffoldBelow?.name === 'scaffolding')
      const climbNames = ['ladder', 'vine', 'scaffolding', 'weeping_vines', 'weeping_vines_plant', 'twisting_vines', 'twisting_vines_plant', 'cave_vines', 'cave_vines_plant']
      const onClimbable = climbNames.includes(scaffoldHere?.name) || climbNames.includes(scaffoldBelow?.name)
      const climbDescending = nextPoint.y < state.pos.y - 0.1 && Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35 && onClimbable
      const climbAscending = nextPoint.y > state.pos.y + 0.1 && Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35 && climbNames.includes(scaffoldHere?.name)`
function baseScaffoldingPreview (source) {
  const original = source
  if (source.includes('// patched by bot/patch-deps.mjs: scaffold descent preview') && !source.includes('scaffold descent preview v2')) {
    source = source.replace(/      \/\/ patched by bot\/patch-deps\.mjs: scaffold descent preview\n[\s\S]*?      state.control.sprint = [^\n]+/, '      state.control.forward = true\n      state.control.jump = jump && tick >= jumpAfter\n      state.control.sprint = sprint')
      .replace('Physics.supportsScaffoldingDescent = true\n', '')
  }
  const before = `      state.control.forward = true
      state.control.jump = jump && tick >= jumpAfter
      state.control.sprint = sprint`
  const after = `      // patched by bot/patch-deps.mjs: scaffold descent preview v2
      ${DESCENT}
      state.control.sneak = scaffoldDescending
      state.control.forward = !climbDescending && !climbAscending
      state.control.jump = climbAscending || !climbDescending && jump && tick >= jumpAfter
      state.control.sprint = !climbDescending && !climbAscending && sprint`
  if (source.includes(after)) return { status: 'already', source }
  return source.includes(before) && source.includes('module.exports = Physics') ? { status: 'patched', source: source.replace(before, after).replace('module.exports = Physics', 'Physics.supportsScaffoldingDescent = true\nmodule.exports = Physics') } : { status: 'anchor missing', source: original }
}
function baseScaffoldingDriver (source) {
  const original = source
  const marker = '// patched by bot/patch-deps.mjs: scaffold descent driver v2'
  if (source.includes('// patched by bot/patch-deps.mjs: scaffold descent driver') && !source.includes(marker)) {
    source = source.replace('  let scaffoldSneaking = false\n', '')
      .replace(/    const onScaffold = bot.blockAt\(p.floored\(\), false\)\?\.name === 'scaffolding' \|\|\n      bot.blockAt\(p.floored\(\).offset\(0, -1, 0\), false\)\?\.name === 'scaffolding'\n/, '')
      .replace('Math.abs(dy) < (onScaffold ? 0.2 : 1)', 'Math.abs(dy) < 1')
      .replace(/    \/\/ patched by bot\/patch-deps\.mjs: scaffold descent driver\n[\s\S]*?(?=    bot.look\(Math.atan2\(-dx, -dz\), 0\))/, '')
      .replace('\nmodule.exports.supportsScaffoldingDescent = true\n', '\n')
  }
  if (source.includes(marker)) return { status: 'already', source }
  const state = '  let path = []'
  const arrival = '    if (Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35 && Math.abs(dy) < 1) {'
  const drive = '    bot.look(Math.atan2(-dx, -dz), 0)'
  if (![state, arrival, drive].every(anchor => source.includes(anchor))) return { status: 'anchor missing', source: original }
  const onScaffold = `    const climbNames = ['ladder', 'vine', 'scaffolding', 'weeping_vines', 'weeping_vines_plant', 'twisting_vines', 'twisting_vines_plant', 'cave_vines', 'cave_vines_plant']
    const here = bot.blockAt(p.floored(), false)
    const below = bot.blockAt(p.floored().offset(0, -1, 0), false)
    const onClimbable = climbNames.includes(here?.name) || climbNames.includes(below?.name)
    const onScaffold = bot.blockAt(p.floored(), false)?.name === 'scaffolding' ||
      bot.blockAt(p.floored().offset(0, -1, 0), false)?.name === 'scaffolding'`
  return {
    status: 'patched',
    source: source.replace(state, `${state}\n  let scaffoldSneaking = false`)
      .replace(arrival, `${onScaffold}\n    if (Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35 && Math.abs(dy) < (onClimbable ? 0.2 : 1)) {`)
      .replace(drive, `    ${marker}
    const scaffoldDescending = onScaffold && nextPoint.y < p.y - 0.1 && Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35
    const climbDescending = onClimbable && nextPoint.y < p.y - 0.1 && Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35
    const climbAscending = climbNames.includes(here?.name) && nextPoint.y > p.y + 0.1 && Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35
    if (scaffoldDescending || scaffoldSneaking) bot.setControlState('sneak', scaffoldDescending)
    scaffoldSneaking = scaffoldDescending
    if (climbDescending || climbAscending) {
      bot.setControlState('forward', false)
      bot.setControlState('jump', climbAscending)
      bot.setControlState('sprint', false)
      if (performance.now() - lastNodeTime > 3500) resetPath('stuck')
      return
    }
${drive}`) + '\nmodule.exports.supportsScaffoldingDescent = true\n'
  }
}

// Keep the native open/aligned ladder rule; only broaden its registry coverage.
export function patchScaffoldingPhysics (source) {
  const result = baseScaffoldingPhysics(source)
  if (result.status === 'anchor missing') return result
  const anchor = '  const trapdoorIds = new Set()'
  const replacement = "  const trapdoorIds = new Set(Object.values(blocksByName).filter(block => block.name.endsWith('_trapdoor')).map(block => block.id))"
  if (!result.source.includes(anchor) && !result.source.includes(replacement)) return { status: 'anchor missing', source }
  const updated = result.source.replace(anchor, replacement)
  return { status: updated === source ? 'already' : 'patched', source: updated }
}
function contextualClimbControls (result, original, preview) {
  if (result.status === 'anchor missing') return result
  const here = preview ? 'scaffoldHere' : 'here'
  const below = preview ? 'scaffoldBelow' : 'below'
  const anchor = `const onClimbable = climbNames.includes(${here}?.name) || climbNames.includes(${below}?.name)`
  const replacement = `const hereProperties = ${here}?.getProperties() ?? {}
    const belowProperties = ${below}?.getProperties() ?? {}
    const contextualTrapdoor = ${here}?.name.endsWith('_trapdoor') && hereProperties.open === true && ${below}?.name === 'ladder' && hereProperties.facing !== undefined && hereProperties.facing === belowProperties.facing
    const hereClimbable = climbNames.includes(${here}?.name) || contextualTrapdoor
    const onClimbable = hereClimbable || climbNames.includes(${below}?.name)`
  if (!result.source.includes(anchor) && !result.source.includes(replacement)) return { status: 'anchor missing', source: original }
  const updated = result.source.replace(anchor, replacement).replace(`&& climbNames.includes(${here}?.name)`, '&& hereClimbable').replace(`const climbAscending = climbNames.includes(${here}?.name) &&`, 'const climbAscending = hereClimbable &&')
  return { status: updated === original ? 'already' : 'patched', source: updated }
}
export function patchScaffoldingPreview (source) {
  if (source.includes('const contextualTrapdoor = scaffoldHere?.name.endsWith')) return { status: 'already', source }
  return contextualClimbControls(baseScaffoldingPreview(source), source, true)
}
export function patchScaffoldingDriver (source) {
  return contextualClimbControls(baseScaffoldingDriver(source), source, false)
}
