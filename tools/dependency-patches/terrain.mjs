// Why JavaScript: patches JS dependency sources in node_modules (Mineflayer boundary).
// Source patches that let the terrain adapter (stateMovements) hook into mineflayer-pathfinder: waypoints, start, stop.
export function patchTerrainWaypoints (source) {
  const marker = '// terrain-aware waypoint adapter'
  if (source.includes(marker)) return { status: 'already patched', source }
  const anchor = '      const b = bot.blockAt(new Vec3(curPoint.x, curPoint.y, curPoint.z))'
  if (!source.includes(anchor)) return { status: 'anchor missing', source }
  return { status: 'patched', source: source.replace(anchor, `      ${marker}\n      if (stateMovements.resolveTerrainWaypoint) {\n        const terrainPoint = stateMovements.resolveTerrainWaypoint(curPoint)\n        if (terrainPoint) { Object.assign(curPoint, terrainPoint); continue }\n      }\n${anchor}`) }
}

// A* reconstructPath returns references to its search nodes. Rendering a
// partial path in place changes their integer positions while the same search
// is still running, corrupting later expansions (and repeatedly offsetting
// terrain waypoints). Execution also consumes interaction arrays with shift().
export function patchPathNodeCopies (source) {
  const marker = '// copy search nodes before rendering execution waypoints'
  if (source.includes(marker)) return { status: 'already patched', source }
  const anchor = '  function postProcessPath (path) {'
  if (!source.includes(anchor)) return { status: 'anchor missing', source }
  return { status: 'patched', source: source.replace(anchor, `${anchor}\n    ${marker}\n    path = path.map(node => Object.assign(Object.create(Object.getPrototypeOf(node)), node, {\n      toBreak: node.toBreak.map(block => block.clone ? block.clone() : { ...block }),\n      toPlace: node.toPlace.map(block => ({ ...block }))\n    }))`) }
}

// The start node comes from the terrain adapter's grounded position when it has one.
export function patchTerrainStart (source) {
  const marker = '// terrain-aware grounded start adapter'
  if (source.includes(marker)) return { status: 'already patched', source }
  const anchor = '      start = new Move(p.x, p.y + offset, p.z, movements.countScaffoldingItems(), 0)'
  if (!source.includes(anchor)) return { status: 'anchor missing', source }
  const replacement = `      ${marker}\n      const terrainStart = movements.resolveTerrainStart?.(startPos, bot.entity.onGround)\n      start = new Move(terrainStart?.x ?? p.x, terrainStart?.y ?? (p.y + offset), terrainStart?.z ?? p.z, movements.countScaffoldingItems(), 0)`
  return { status: 'patched', source: source.replace(anchor, replacement) }
}

// On stopping, keep the terrain-checked stance, and count a waypoint as reached only when the adapter agrees.
export function patchTerrainStop (source) {
  const marker = '// preserve checked terrain stance on stopping v2'
  if (source.includes(marker)) return { status: 'already patched', source }
  const anchor = '    const blockX = Math.floor(bot.entity.position.x) + 0.5'
  const arrival = '    if (Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35 &&'
  if (!source.includes(anchor) || !source.includes(arrival)) return { status: 'anchor missing', source }
  const next = source.includes('// preserve checked terrain stance on stopping')
    ? source.replace('// preserve checked terrain stance on stopping', marker)
    : source.replace(anchor, `    ${marker}\n    if (stateMovements?.preserveTerrainPosition?.(bot.entity.position)) return\n\n${anchor}`)
  return { status: 'patched', source: next.replace(arrival, '    if ((stateMovements.terrainWaypointReached?.(p, nextPoint) ?? true) && Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35 &&') }
}
