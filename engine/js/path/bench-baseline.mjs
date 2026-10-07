// Why JavaScript: adapter around mineflayer-pathfinder's own planner for the benchmark.
// Planner adapter: mineflayer-pathfinder's own A* and Movements, configured like engine/js/connect.mjs,
// reading blocks from a snapshot through a fake bot.
import { createRequire } from 'node:module'
import { performance } from 'node:perf_hooks'
import vec3 from 'vec3'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { UNLOADED } from './snapshot.mjs'

const require = createRequire(import.meta.url)
const Movements = require('mineflayer-pathfinder/lib/movements')
const AStar = require('mineflayer-pathfinder/lib/astar')
const { goals } = require('mineflayer-pathfinder')

const { Vec3 } = vec3
const registry = prismarineRegistry('26.1')
const Block = prismarineBlock(registry)

export const DEFAULT_TIMEOUT_MS = 5000

const makeEnv = snapshot => {
  const counter = { lookups: 0 }
  const blockAt = pos => {
    counter.lookups++
    const at = pos.floored()
    const id = snapshot.stateAt(at.x, at.y, at.z)
    if (id === UNLOADED) return null
    const block = Block.fromStateId(id, 0)
    block.position = at
    return block
  }
  const bot = {
    registry,
    blockAt,
    game: { minY: snapshot.minY },
    inventory: { items: () => [] },
    entities: {},
    entity: { position: new Vec3(0, 0, 0), effects: {} },
    pathfinder: { bestHarvestTool: () => null }
  }
  const movements = new Movements(bot)
  movements.canDig = false
  movements.allow1by1towers = false
  movements.scafoldingBlocks = [] // sic: the pathfinder's own spelling
  movements.allowEntityDetection = false
  return { counter, movements }
}

// building Movements walks the whole registry, so keep one per snapshot
const envs = new WeakMap()
const envFor = snapshot => {
  if (!envs.has(snapshot)) envs.set(snapshot, makeEnv(snapshot))
  return envs.get(snapshot)
}

const goalOf = ({ kind, x, y, z, range }) => kind === 'xz' ? new goals.GoalNearXZ(x, z, range) : new goals.GoalNear(x, y, z, range)

export function plan (snapshot, query, { timeout = DEFAULT_TIMEOUT_MS } = {}) {
  const { counter, movements } = envFor(snapshot)
  const { x, y, z } = query.from
  counter.lookups = 0
  const t0 = performance.now()
  const start = { x, y, z, remainingBlocks: 0, cost: 0, hash: `${x},${y},${z}`, toBreak: [], toPlace: [] }
  const result = new AStar(start, movements, goalOf(query.goal), timeout, 1e9, -1).compute()
  const ms = performance.now() - t0
  return {
    status: result.status,
    ms,
    expanded: result.visitedNodes,
    lookups: counter.lookups,
    path: result.path.map(p => ({ x: p.x, y: p.y, z: p.z }))
  }
}
