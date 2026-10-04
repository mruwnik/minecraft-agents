import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { Before, After, run } from '../tools/profile-pathfinding.mjs'
import { farmWalk, explainNoPath } from '../src/lib/path.mjs'

for (const [name, shape] of [
  ['open ground', () => false],
  ['fence detour', p => p.y === 64 && p.x === 15 && Math.abs(p.z) < 20],
  ['unloaded edge', p => Math.abs(p.z) > 3 ? null : p.y === 64 && p.x === 15 && Math.abs(p.z) < 2]
]) test(`cached native pathfinder preserves ${name} route and reduces world reads`, () => {
  const before = run(Before, shape)
  const after = run(After, shape)
  assert.equal(after.status, 'success')
  assert.equal(after.status, before.status)
  assert.equal(after.cost, before.cost)
  assert.equal(after.visited, before.visited)
  assert.deepEqual(after.path, before.path)
  assert.ok(after.reads < before.reads * 0.5, `${before.reads} -> ${after.reads}`)
})

test('cache ends at expansion boundary and restores getBlock after exception', () => {
  class Base {
    constructor () { this.name = 'air'; this.fail = false }
    getBlock (pos, dx, dy, dz) { return { name: this.name, position: { x: pos.x + dx, y: pos.y + dy, z: pos.z + dz } } }
    getNeighbors (node) { this.getBlock(node, 0, 0, 0); if (this.fail) throw new Error('failed'); return [{ x: 1, y: 64, z: 0, cost: 1 }] }
  }
  const moves = new (farmWalk(Base))()
  const read = moves.getBlock
  const node = { x: 0, y: 64, z: 0 }
  assert.equal(moves.getNeighbors(node)[0].cost, 1)
  moves.name = 'wheat'
  assert.equal(moves.getNeighbors(node)[0].cost, 11)
  assert.equal(moves.getBlock, read)
  moves.fail = true
  assert.throws(() => moves.getNeighbors(node), /failed/)
  assert.equal(moves.getBlock, read)
})

test('unloaded relative-height fallbacks are not cached by absolute coordinate', () => {
  const heights = []
  class Base {
    getBlock (pos, dx, dy, dz) { return { height: dy } }
    getNeighbors (node) {
      heights.push(this.getBlock(node, 0, 1, 0).height)
      heights.push(this.getBlock({ ...node, y: node.y + 1 }, 0, 0, 0).height)
      return []
    }
  }
  new (farmWalk(Base))().getNeighbors({ x: 0, y: 64, z: 0 })
  assert.deepEqual(heights, [1, 0])
})

test('scaffold planning cannot leak speculative support heights between neighbor choices', () => {
  class Base {
    getBlock (pos, dx, dy, dz) {
      return { name: 'stone', height: pos.y + dy + 1, position: { x: pos.x + dx, y: pos.y + dy, z: pos.z + dz } }
    }
    getNeighbors (node) {
      // Native getMoveJumpUp raises this temporary height when it considers
      // placing a support; subsequent directions must still see the world.
      this.getBlock(node, 1, -1, 0).height += 1
      return [{ x: 1, y: node.y, z: 0, cost: this.getBlock(node, 1, -1, 0).height }]
    }
  }
  const node = { x: 0, y: 64, z: 0, remainingBlocks: 2 }
  assert.deepEqual(new (farmWalk(Base))().getNeighbors(node), new Base().getNeighbors(node))
})

test('native placement-capable neighbor choices match uncached movement across gaps and steps', () => {
  const require = createRequire(import.meta.url)
  const registry = require('minecraft-data')('1.21.5')
  const Block = require('prismarine-block')(registry)
  const Move = require('mineflayer-pathfinder/lib/move')
  for (const raised of [false, true]) {
    const bot = {
      registry, game: { minY: -64 }, entities: {}, inventory: { items: () => [] },
      blockAt (position) {
        const support = position.y < 63 || position.y === 63 && (position.x <= 0 || position.z !== 0) || raised && position.x === -1 && position.y === 64
        const block = Block.fromStateId(registry.blocksByName[support ? 'stone' : 'air'].minStateId, 0)
        block.position = position
        return block
      }
    }
    const neighbors = Type => {
      const moves = new Type(bot)
      moves.canDig = false
      return moves.getNeighbors(new Move(0, 64, 0, 8, 0))
    }
    assert.deepEqual(neighbors(After), neighbors(Before))
  }
})

import { recoverableGotoFailure } from '../src/composite.mjs'
test('timeout advice describes a maximum budget and remains recoverable', () => {
  const message = explainNoPath('Took to long to decide path to goal!', false)
  assert.match(message, /up to 5 s/)
  assert.equal(recoverableGotoFailure(new Error(message)), true)
})
