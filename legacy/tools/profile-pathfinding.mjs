// Offline profile using the installed native AStar and Movements, with real prismarine blocks.
// The baseline retains the previous farm pricing; only per-expansion block caching differs.
import { createRequire } from 'node:module'
import { performance } from 'node:perf_hooks'
import { fileURLToPath } from 'node:url'
import { resolve } from 'node:path'
import { farmWalk, keepMove, cropStepCost, trampleCost } from '../src/lib/path.mjs'

const require = createRequire(import.meta.url)
const registry = require('minecraft-data')('1.21.4')
const Block = require('prismarine-block')(registry)
const { Movements, goals } = require('mineflayer-pathfinder')
const AStar = require('mineflayer-pathfinder/lib/astar')
const Move = require('mineflayer-pathfinder/lib/move')

export class Before extends Movements {
  getNeighbors (node) {
    const floor = move => this.getBlock(move, 0, -1, 0)?.name
    return super.getNeighbors(node)
      .filter(move => keepMove(node, move, floor(move)))
      .map(move => Object.assign(move, {
        cost: move.cost + cropStepCost(this.getBlock(move, 0, 0, 0)?.name) + trampleCost(node, move, floor(move))
      }))
  }
}
export const After = farmWalk(Movements)

export function run (Type, shape) {
  let reads = 0
  const bot = {
    registry,
    game: { minY: -64 },
    entities: {},
    inventory: { items: () => [] },
    blockAt (position) {
      reads++
      const terrain = shape(position)
      if (terrain === null) return null
      const name = position.y < 64 ? 'stone' : terrain ? 'oak_fence' : 'air'
      const block = Block.fromStateId(registry.blocksByName[name].minStateId, 0)
      block.position = position
      return block
    }
  }
  const moves = new Type(bot)
  moves.canDig = false
  moves.allow1by1towers = false
  moves.scafoldingBlocks = []
  moves.canOpenDoors = true
  const search = new AStar(new Move(0, 64, 0, 0, 0), moves, new goals.GoalNear(30, 64, 0, 1), 10000, 10000, 160)
  const start = performance.now()
  const result = search.compute()
  return {
    status: result.status,
    ms: performance.now() - start,
    reads,
    visited: result.visitedNodes,
    cost: result.cost,
    path: result.path.map(move => [move.x, move.y, move.z, move.cost])
  }
}

if (process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1])) {
  const scenarios = [
    ['open', () => false],
    ['fence-detour', p => p.y === 64 && p.x === 15 && Math.abs(p.z) < 20],
    ['sealed', p => p.y === 64 && (
      ((p.x === 28 || p.x === 32) && Math.abs(p.z) <= 2) ||
      ((p.z === -2 || p.z === 2) && p.x >= 28 && p.x <= 32)
    )]
  ]
  for (const [name, shape] of scenarios) {
    // One warmup each, then alternate measurement order to reduce JIT/order bias.
    run(Before, shape)
    run(After, shape)
    const samples = { before: [], after: [] }
    let before, after
    for (let i = 0; i < 3; i++) {
      if (i % 2) { after = run(After, shape); before = run(Before, shape) }
      else { before = run(Before, shape); after = run(After, shape) }
      samples.before.push(before.ms)
      samples.after.push(after.ms)
    }
    const median = values => [...values].sort((a, b) => a - b)[Math.floor(values.length / 2)]
    console.log(JSON.stringify({
      name,
      runs: 3,
      warmup: 1,
      before: { ...before, path: undefined, ms: median(samples.before) },
      after: { ...after, path: undefined, ms: median(samples.after) },
      samePath: JSON.stringify(before.path) === JSON.stringify(after.path),
      samples
    }))
  }
}
