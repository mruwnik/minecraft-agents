import { createRequire } from 'node:module'
import { createTerrainGeometry, terrainProfile, terrainClimbable, PLAYER_HALF } from './terrain.mjs'
import { keepMove, cropStepCost, trampleCost } from '../lib/path.mjs'

const require = createRequire(import.meta.url)
const Move = require('mineflayer-pathfinder/lib/move')
export const scaffoldingAvailable = () => require('prismarine-physics').supportsScaffolding === true &&
  require('mineflayer-pathfinder').supportsScaffoldingDescent === true && require('mineflayer-pathfinder/lib/physics').supportsScaffoldingDescent === true
export const climbableVinesAvailable = () => require('prismarine-physics').supportsClimbableVines === true && scaffoldingAvailable()

// Configure the SAME native Movements used by goto/path_to. Geometry supplies
// missing partial-block/climb steps, while native costs, digging, gates, hazards,
// farm protection and path execution remain authoritative.
export function configureTerrainMoves (moves, { blockAt, scaffolding = false, climbableVines = scaffolding } = {}) {
  moves.scaffoldingSupported = scaffolding
  moves.climbableVinesSupported = climbableVines
  const options = { scaffolding, climbableVines, openDoors: moves.canOpenDoors, dry: false, avoidCrops: false, leaves: true }
  const originalBlock = moves.getBlock.bind(moves)
  let complex = false
  let profiles = null
  const states = new WeakMap()
  moves.getBlock = (...args) => {
    const raw = originalBlock(...args)
    if (!raw?.position) return raw
    const key = `${raw.position.x},${raw.position.y},${raw.position.z}`
    let p = profiles?.get(key)
    if (!p && Array.isArray(raw.shapes) && Number.isInteger(raw.stateId)) p = states.get(raw.shapes)?.get(raw.stateId)
    if (!p) {
      p = terrainProfile(raw, options)
      p.cube = p.shapes.length === 1 && p.shapes[0].every((value, i) => value === (i < 3 ? 0 : 1))
      p.crossed = p.shapes.some(s => s[0] < 0.5 + PLAYER_HALF && s[3] > 0.5 - PLAYER_HALF && s[2] < 0.5 + PLAYER_HALF && s[5] > 0.5 - PLAYER_HALF && s[4] > 0.1 && s[1] < 1)
      p.top = Math.max(0, ...p.support.map(s => s[4]))
      if (Array.isArray(raw.shapes) && Number.isInteger(raw.stateId)) {
        if (!states.has(raw.shapes)) states.set(raw.shapes, new Map())
        states.get(raw.shapes).set(raw.stateId, p)
      }
    }
    profiles?.set(key, p)
    const simple = !(p.hazardous || p.climbable || p.opening || p.liquid || p.noSupport && p.shapes.length || p.shapes.length && !p.cube)
    if (!simple) complex = true
    if (moves.canDig) return p.hazardous ? Object.assign(Object.create(Object.getPrototypeOf(raw)), raw, { safe: false }) : raw
    if (simple) return raw
    // Return a view: native neighbor generation mutates speculative heights.
    const climbable = /_trapdoor$/.test(p.name ?? '') && p.properties.open === true
      ? terrainClimbable(p, terrainProfile(blockAt(raw.position.x, raw.position.y - 1, raw.position.z), options)) : p.climbable
    return Object.assign(Object.create(Object.getPrototypeOf(raw)), raw, { safe: !p.hazardous && !p.crossed && !moves.blocksToAvoid.has(raw.type), physical: p.support.length > 0,
      climbable, liquid: p.liquid, height: raw.position.y + p.top })
  }
  const geometryNow = () => {
    const cache = new Map()
    return createTerrainGeometry((x, y, z) => {
      const key = `${x},${y},${z}`
      if (!cache.has(key)) cache.set(key, blockAt(x, y, z))
      return cache.get(key)
    }, options)
  }
  moves.resolveTerrainWaypoint = node => {
    const geometry = geometryNow()
    const stand = geometry.stand(node.x, node.y, node.z)
    return stand ? { x: stand.centerX ?? node.x + 0.5, y: stand.height, z: stand.centerZ ?? node.z + 0.5 } : null
  }
  moves.preserveTerrainPosition = position => {
    const x = Math.floor(position.x), y = Math.floor(position.y), z = Math.floor(position.z)
    for (const [dx, dz] of [[0, 0], [1, 0], [-1, 0], [0, 1], [0, -1]]) for (let dy = 0; dy <= 1; dy++) {
      if (blockAt(x + dx, y + dy, z + dz)?.name === 'cocoa') return true
    }
    return false
  }
  moves.terrainWaypointReached = (position, waypoint) => !moves.preserveTerrainPosition(waypoint) ||
    Math.floor(position.x) === Math.floor(waypoint.x) && Math.floor(position.z) === Math.floor(waypoint.z)
  moves.resolveTerrainStart = (position, onGround) => {
    if (!onGround) return null
    const x = Math.floor(position.x), y = Math.floor(position.y), z = Math.floor(position.z)
    const geometry = geometryNow()
    for (const nodeY of [y, y + 1]) {
      const stance = geometry.stand(x, nodeY, z)
      if (stance && Math.abs(stance.height - position.y) < 0.001) return { x, y: nodeY, z }
    }
    return null
  }
  const originalNeighbors = moves.getNeighbors.bind(moves)
  moves.getNeighbors = node => {
    complex = false
    profiles = new Map()
    let native
    try { native = originalNeighbors(node) } finally { profiles = null }
    if (!complex) return native
    native = native.filter(move => {
      const x = move.x, y = move.y - 1, z = move.z
      const floor = blockAt(x, y, z)
      if (!terrainProfile(floor, options).hazardous) return true
      return move.toBreak?.some(p => p.x === x && p.y === y && p.z === z) || move.toPlace?.some(p => !p.useOne && p.x + p.dx === x && p.y + p.dy === y && p.z + p.dz === z)
    })
    // Excavation/scaffolding searches model future blocks. Present-world dry
    // geometry must not erase those planned changes.
    if (moves.canDig || node.remainingBlocks > 0) return native
    const geometry = geometryNow()
    const from = geometry.stand(node.x, node.y, node.z)
    const valid = move => {
      // Ordinary walking can wade/swim at the surface, but never plans a
      // submerged transit under a bank. Native water nodes only check collision,
      // not breathable headroom; the executor then keeps jumping into the roof.
      const wet = geometry.get(move.x, move.y, move.z).liquid
      if (wet && geometry.get(move.x, move.y + 1, move.z).liquid) return false
      const to = geometry.stand(move.x, move.y, move.z)
      if (!to) return false
      if (!from) return true
      if (to.swimming || from.swimming) return geometry.edge(from, to, { diagonal: Math.abs(move.x - node.x) + Math.abs(move.z - node.z) > 1 })
      if (move.parkour || Math.abs(move.y - node.y) > 1) return true
      if (Math.abs(move.x - node.x) + Math.abs(move.z - node.z) <= 1) return geometry.edge(from, to)
      return geometry.edge(from, to, { diagonal: true })
    }
    const result = native.filter(valid)
    if (!from) return result
    const keys = new Set(result.map(m => `${m.x},${m.y},${m.z}`))
    for (const [dx, dz] of [[1, 0], [-1, 0], [0, 1], [0, -1], [0, 0]]) for (const dy of dx || dz ? [0, 1, -1] : [1, -1]) {
      const x = node.x + dx, y = node.y + dy, z = node.z + dz
      const key = `${x},${y},${z}`
      if (keys.has(key)) continue
      const to = geometry.stand(x, y, z)
      if (!to || geometry.get(x, y, z).liquid && geometry.get(x, y + 1, z).liquid || !geometry.edge(from, to)) continue
      const foot = moves.getBlock({ x, y, z }, 0, 0, 0)
      const floor = moves.getBlock({ x, y, z }, 0, -1, 0)
      const stepCost = moves.exclusionStep(foot)
      if (stepCost >= 100 || !keepMove(node, { x, y, z }, floor.name)) continue
      const opening = geometry.get(x, y, z).opening
      const toPlace = opening && foot.name?.endsWith('_fence_gate') ? [{ x, y, z, dx: 0, dy: 0, dz: 0, useOne: true }] : []
      const occupancy = moves.getNumEntitiesAt({ x, y, z }, 0, 0, 0) * moves.entityCost
      const cost = 1 + Math.abs(dy) + stepCost + occupancy + cropStepCost(foot.name) + trampleCost(node, { x, y, z }, floor.name)
      result.push(new Move(x, y, z, node.remainingBlocks, cost, [], toPlace))
      keys.add(key)
    }
    return result
  }
  return moves
}
