import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { performance } from 'node:perf_hooks'
import { readFileSync } from 'node:fs'
import Module from 'node:module'
import { EventEmitter } from 'node:events'
import { terrainProfile, createTerrainGeometry, patchTerrainStart } from '../src/navigation/terrain.mjs'
import { configureTerrainMoves } from '../src/navigation/terrain-moves.mjs'
import { connectedFrontiers } from '../src/forage.mjs'
import { farmWalk } from '../src/lib/path.mjs'
const require = createRequire(import.meta.url)
const registry = require('minecraft-data')('1.21.5')
const Block = require('prismarine-block')(registry)
const { Vec3 } = require('vec3')
const { Movements, goals } = require('mineflayer-pathfinder')
const Move = require('mineflayer-pathfinder/lib/move')
const AStar = require('mineflayer-pathfinder/lib/astar')
const block = (name, properties = {}) => Block.fromProperties(name, properties, 0)
const air = block('air'), stone = block('stone')
function world (shape) {
  return (x, y, z) => {
    const source = shape(x, y, z)
    if (!source) return null
    const b = Block.fromStateId(source.stateId, 0)
    b.position = new Vec3(x, y, z)
    return b
  }
}
function movements (at, { dig = false, scaffolding = false } = {}) {
  const bot = { registry, entities: {}, entity: { effects: {} }, pathfinder: { bestHarvestTool: () => null }, game: { minY: -64 }, inventory: { items: () => [] }, blockAt: p => at(p.x, p.y, p.z) }
  const moves = new (farmWalk(Movements))(bot)
  moves.canDig = dig; moves.canOpenDoors = true; moves.allowParkour = false; moves.allow1by1towers = false; moves.scafoldingBlocks = []
  return configureTerrainMoves(moves, { blockAt: at, scaffolding })
}
const search = (moves, from, to) => new AStar(new Move(...from, 0, 0), moves, new goals.GoalBlock(...to), 1000, 1000, 30).compute()

test('every registered block state receives finite collision/support classification without crashing', t => {
  const started = performance.now()
  let states = 0
  for (const entry of registry.blocksArray) for (let state = entry.minStateId; state <= entry.maxStateId; state++) {
    const p = terrainProfile(Block.fromStateId(state, 0))
    assert.equal(p.loaded, true)
    assert.ok([...p.shapes, ...p.support].every(shape => shape.length === 6 && shape.every(Number.isFinite)))
    if (p.hazardous || p.name === 'bamboo') assert.equal(p.support.length, 0)
    states++
  }
  t.diagnostic(`${registry.blocksArray.length} block types / ${states} states classified in ${(performance.now() - started).toFixed(1)}ms`)
})
test('planned wooden-door opening matches registry collision in every orientation and preserves the swung panel', () => {
  for (const facing of ['north', 'south', 'east', 'west']) for (const hinge of ['left', 'right']) for (const half of ['lower', 'upper']) {
    const closed = block('oak_door', { facing, hinge, half, open: false })
    const opened = block('oak_door', { facing, hinge, half, open: true })
    assert.deepEqual(terrainProfile(closed).shapes, opened.shapes)
    assert.deepEqual(terrainProfile(closed, { openDoors: false }).shapes, closed.shapes)
  }
  const door = block('oak_door', { facing: 'north', hinge: 'left', open: true })
  const at = world((x, y, z) => y < 1 ? stone : x === 0 && z === 0 && y <= 2 ? door : air)
  const g = createTerrainGeometry(at)
  assert.ok(g.edge(g.stand(0, 1, -1), g.stand(0, 1, 0)))
  assert.equal(g.edge(g.stand(-1, 1, 0), g.stand(0, 1, 0)), false, 'the opened panel still blocks its hinge side')
})
test('support comes from shapes beneath the footprint, not an unrelated tallest corner or overhang', () => {
  const floor = { name: 'synthetic_support', shapes: [[0, 0, 0, 0.1, 1, 0.1]], solid: true }
  const g = createTerrainGeometry((x, y) => y === 0 ? floor : { name: 'air', shapes: [], solid: false })
  assert.equal(g.stand(0, 1, 0), null)
  const pot = block('flower_pot')
  const potWorld = world((x, y) => y < 0 ? stone : y === 0 ? pot : air)
  assert.equal(createTerrainGeometry(potWorld).stand(0, 1, 0).height, 0.375)
  assert.equal(movements(potWorld).resolveTerrainWaypoint({ x: 0, y: 1, z: 0 }).y, 0.375)
  assert.equal(terrainProfile(block('bamboo')).support.length, 0)
})
test('dry top-waterlogged slabs are usable while submerged half slabs and open fluids are classified wet', () => {
  for (const [type, expected] of [['top', true], ['bottom', false], ['double', true]]) {
    const slab = block('oak_slab', { type, waterlogged: true })
    const g = createTerrainGeometry(world((x, y) => y === 0 ? slab : y < 0 ? stone : air))
    assert.equal(!!g.stand(0, 1, 0), expected, type)
  }
  for (const name of ['water', 'kelp', 'kelp_plant', 'seagrass', 'tall_seagrass', 'bubble_column']) assert.equal(terrainProfile(block(name)).liquid, true, name)
})
test('native routes traverse open iron doors and thin trapdoor roofs but cannot cross closed iron doors', () => {
  for (const open of [true, false]) {
    const at = world((x, y, z) => x !== 0 || y < 1 || y > 3 ? stone : z === 0 && y <= 2 ? block('iron_door', { open, facing: 'north', hinge: 'left', half: y === 1 ? 'lower' : 'upper' }) : air)
    assert.equal(search(movements(at), [0, 1, -2], [0, 1, 2]).status, open ? 'success' : 'noPath')
  }
  const roof = block('oak_trapdoor', { open: false, half: 'top', waterlogged: false })
  const at = world((x, y, z) => x !== 0 || y < 1 ? stone : y === 2 ? roof : air)
  assert.equal(search(movements(at), [0, 1, 0], [0, 1, 8]).status, 'success', '1.8125 high corridor fits a 1.8-high player')
})
test('native routes reject floor hazards even for digging while preserving real Block methods', () => {
  for (const name of ['magma_block', 'cactus', 'campfire', 'soul_campfire', 'pointed_dripstone']) for (const dig of [true, false]) {
    const at = world((x, y, z) => y === 0 && x === 1 && z === 0 ? block(name, { lit: true }) : y < 1 ? stone : air)
    const moves = movements(at, { dig })
    const b = moves.getBlock(new Vec3(0, 1, 0), 0, -1, 0)
    assert.equal(typeof b.getProperties, 'function')
    assert.equal(typeof b.digTime, 'function')
    assert.equal(moves.getNeighbors(new Move(0, 1, 0, 0, 0)).some(m => m.x === 1 && m.y === 1 && m.z === 0 && !m.toBreak.length), false, `${name} dig=${dig}`)
  }
})
test('ladder midpoint explores both vertical branches and can exit downward from an upper dead end', () => {
  const ladder = block('ladder', { facing: 'north', waterlogged: false })
  const at = world((x, y, z) => y < 1 || z !== 0 || x < 0 || x > 8 || y >= 6 ? stone : x === 0 ? ladder : y <= 2 ? air : stone)
  const plan = connectedFrontiers(at, { x: 0, y: 3, z: 0 }, [{ x: 8, z: 0 }])
  assert.equal(plan.goals.length, 1)
  assert.equal(plan.goals[0].y, 1)
  assert.equal(search(movements(at), [0, 3, 0], [8, 1, 0]).status, 'success')
})
test('supplemental partial-block moves preserve entity occupancy costs and exclusion areas', () => {
  const roof = block('oak_trapdoor', { half: 'top', open: false, waterlogged: false })
  const at = world((x, y, z) => y < 1 ? stone : y === 2 ? roof : air)
  const moves = movements(at)
  moves.entityCost = 20
  moves.entityIntersections['1,1,0'] = 2
  const target = () => moves.getNeighbors(new Move(0, 1, 0, 0, 0)).find(m => m.x === 1 && m.y === 1 && m.z === 0)
  assert.ok(target().cost >= 41)
  moves.exclusionAreasStep.push(b => b.position.x === 1 ? 100 : 0)
  assert.equal(target(), undefined)
})
test('a suspended scaffolding bottom preserves its real one-eighth-block landing and cannot be descended through', () => {
  const scaffold = block('scaffolding', { bottom: true, distance: 1, waterlogged: false })
  const at = world((x, y, z) => y <= 1 ? stone : x === 0 && z === 0 && y === 3 ? scaffold : air)
  const geometry = createTerrainGeometry(at, { scaffolding: true })
  const from = geometry.stand(0, 3, 0), to = geometry.stand(0, 2, 0)
  assert.equal(from.height, 3.125)
  assert.equal(geometry.edge(from, to), false)
  const moves = movements(at, { scaffolding: true })
  assert.equal(moves.resolveTerrainWaypoint({ x: 0, y: 3, z: 0 }).y, 3.125)
  assert.equal(moves.getNeighbors(new Move(0, 3, 0, 0, 0)).some(p => p.x === 0 && p.z === 0 && p.y === 2), false)
})
test('open trapdoors climb only above a ladder with matching facing', () => {
  const ladder = block('ladder', { facing: 'north', waterlogged: false })
  for (const [open, facing, expected] of [[true, 'north', true], [true, 'south', false], [false, 'north', false]]) {
    const hatch = block('oak_trapdoor', { open, facing, half: 'top', waterlogged: false })
    const at = world((x, y, z) => y < 1 ? stone : x === 0 && z === 0 && y === 1 ? ladder : x === 0 && z === 0 && y === 2 ? hatch : air)
    assert.equal(!!createTerrainGeometry(at).stand(0, 2, 0)?.climbable, expected)
    assert.equal(movements(at).getBlock(new Vec3(0, 2, 0), 0, 0, 0).climbable, expected)
  }
})

test('native grounded start uses actual snow state support while airborne and explicit starts retain native behavior', () => {
  const file = require.resolve('mineflayer-pathfinder')
  const result = patchTerrainStart(readFileSync(file, 'utf8'))
  assert.ok(['patched', 'already patched'].includes(result.status))
  assert.equal(patchTerrainStart(result.source).status, 'already patched')
  assert.deepEqual(patchTerrainStart('unknown source'), { status: 'anchor missing', source: 'unknown source' })
  const mod = new Module(file)
  mod.filename = file
  mod.paths = Module._nodeModulePaths(file.slice(0, file.lastIndexOf('/')))
  mod._compile(result.source, file)
  for (let layers = 1; layers <= 8; layers++) {
    const at = world((x, y, z) => y < 64 ? stone : x === 0 && y === 64 && z === 0 ? block('snow', { layers }) : air)
    const position = new Vec3(0.5, 64 + (layers - 1) / 8, 0.5)
    const moves = movements(at)
    const bot = Object.assign(new EventEmitter(), { registry, version: '1.21.5', game: { minY: -64 }, entities: {},
      entity: { position, onGround: true, effects: {} }, inventory: { items: () => [] }, blockAt: p => at(p.x, p.y, p.z) })
    mod.exports.pathfinder(bot)
    let first
    const neighbors = moves.getNeighbors.bind(moves)
    moves.getNeighbors = node => { first ??= node; return neighbors(node) }
    const expected = layers === 1 ? 64 : 65
    const goal = new goals.GoalBlock(2, 64, 0)
    const plan = bot.pathfinder.getPathFromTo(moves, position, goal, { optimizePath: false }).next().value.result
    assert.equal(plan.status, 'success', `layers=${layers}`)
    assert.equal(first.y, expected, `layers=${layers}`)
    assert.equal(moves.resolveTerrainStart(position, false), null)
    assert.equal(moves.resolveTerrainStart(position.offset(0, 0.03, 0), true), null, 'unmatched support falls back')
    first = undefined
    bot.pathfinder.getPathFromTo(moves, position, goal, { optimizePath: false, startMove: new Move(0, 64, 0, 0, 0) }).next()
    assert.equal(first.y, 64, 'explicit caller start is authoritative')
  }
})
