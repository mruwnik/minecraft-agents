import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { createRequire } from 'node:module'
import { createTerrainGeometry, terrainProfile } from '../src/navigation/terrain.mjs'
import { configureTerrainMoves } from '../src/navigation/terrain-moves.mjs'
import { connectedFrontiers } from '../src/forage.mjs'
import { farmWalk } from '../src/lib/path.mjs'
import { rimGoal, noStanding } from '../src/navigation/walk.mjs'
const require = createRequire(import.meta.url)
const registry = require('minecraft-data')('26.1')
const Block = require('prismarine-block')(registry)
const { Vec3 } = require('vec3')
const { Physics, PlayerState } = require('prismarine-physics')
const driver = require('mineflayer-pathfinder')
const basis = { north: [1, 0, 0, 1], south: [-1, 0, 0, -1], east: [0, -1, 1, 0], west: [0, 1, -1, 0] }
function corridor (age, facing, { wide = false, podY = 1 } = {}) {
  const [a, b, c, d] = basis[facing]
  const cell = (u, v) => ({ x: a * u + b * v, z: c * u + d * v })
  const point = (u, v, y = 1) => { const p = cell(u, v); return new Vec3(p.x + 0.5, y, p.z + 0.5) }
  const at = (x, y, z) => {
    const u = a * x + c * z, v = b * x + d * z
    const name = y < 1 || y >= 3 || Math.abs(u) > 5 || v < 0 || v >= (wide ? 3 : 1)
      ? v === -1 && y >= 1 && y < 3 ? 'jungle_log' : 'stone'
      : u === 0 && v === 0 && y === podY ? 'cocoa' : 'air'
    const block = Block.fromProperties(name, name === 'cocoa' ? { age, facing } : {}, 0)
    block.position = new Vec3(x, y, z)
    return block
  }
  return { at, point, cell, getBlock: p => { const q = p.floored(); return at(q.x, q.y, q.z) } }
}
const controls = () => ({ forward: false, back: false, left: false, right: false, jump: false, sprint: false, sneak: false })
function setup (world, start) {
  const bot = Object.assign(new EventEmitter(), { version: '26.1', registry, game: { minY: -64 }, entities: {}, controlState: controls(),
    entity: { id: 1, position: start, velocity: new Vec3(0, 0, 0), onGround: true, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
    inventory: { slots: [], items: () => [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0,
    blockAt: p => world.getBlock(p), physics: Physics(registry, world),
    setControlState (name, value) { this.controlState[name] = value }, clearControlStates () { Object.assign(this.controlState, controls()) },
    look (yaw, pitch) { this.entity.yaw = yaw; this.entity.pitch = pitch } })
  driver.pathfinder(bot)
  const moves = new (farmWalk(driver.Movements))(bot)
  moves.canDig = false; moves.allowParkour = false; moves.allow1by1towers = false; moves.scafoldingBlocks = []
  configureTerrainMoves(moves, { blockAt: world.at })
  bot.pathfinder.setMovements(moves)
  const tick = () => {
    bot.emit('physicsTick')
    bot.physics.simulatePlayer(new PlayerState(bot, { ...bot.controlState }), world).apply(bot)
  }
  return { bot, moves, tick }
}

test('all cocoa ages and facings retain actual pod collision and are not destructive underfoot crops', () => {
  for (let age = 0; age <= 2; age++) for (const facing of Object.keys(basis)) {
    const pod = Block.fromProperties('cocoa', { age, facing }, 0), profile = terrainProfile(pod)
    assert.deepEqual(profile.shapes, pod.shapes)
    assert.equal(profile.crop, false)
    assert.ok(profile.shapes.length)
  }
})
test('young cocoa has a supported offset in a narrow corridor; older pods remain too wide', () => {
  for (let age = 0; age <= 2; age++) for (const facing of Object.keys(basis)) for (const podY of [1, 2]) {
    const world = corridor(age, facing, { podY }), geometry = createTerrainGeometry(world.at)
    const stance = geometry.stand(0, 1, 0)
    assert.equal(Boolean(stance), age === 0, `${age} ${facing} podY=${podY}`)
    if (stance) {
      assert.ok(stance.centerX !== 0.5 || stance.centerZ !== 0.5)
      const before = world.cell(-1, 0), after = world.cell(1, 0)
      assert.ok(geometry.edge(geometry.stand(before.x, 1, before.z), stance))
      assert.ok(geometry.edge(stance, geometry.stand(after.x, 1, after.z)))
    }
    const from = world.point(-3, 0), to = world.cell(3, 0)
    const frontier = connectedFrontiers(world.at, from, [to])
    assert.equal(frontier.goals.length, age === 0 ? 1 : 0, 'frontier and exact geometry agree')
  }
})
test('native walking executes young-pod offset entry and exit in every facing without breaking or placing', () => {
  for (const facing of Object.keys(basis)) for (const podY of [1, 2]) {
    const world = corridor(0, facing, { podY }), { bot, moves, tick } = setup(world, world.point(-3, 0))
    const to = world.cell(3, 0), goal = new driver.goals.GoalBlock(to.x, 1, to.z)
    const route = bot.pathfinder.getPathTo(moves, goal)
    assert.equal(route.status, 'success', `${facing} podY=${podY}`)
    assert.ok(route.path.every(p => !p.toBreak.length && !p.toPlace.length))
    const podNode = route.path.find(p => Math.floor(p.x) === 0 && Math.floor(p.z) === 0)
    assert.ok(podNode && (podNode.x !== 0.5 || podNode.z !== 0.5), 'actual executor receives the offset')
    let completed = false
    bot.on('goal_reached', () => { completed = true })
    bot.pathfinder.setGoal(goal)
    for (let t = 0; t < 240 && !completed; t++) tick()
    assert.equal(completed, true, `${facing} podY=${podY} stopped at ${bot.entity.position}`)
    assert.equal(world.at(0, podY, 0).name, 'cocoa')
  }
})
test('native routes detour around every cocoa state when there is room and reject older narrow pods', () => {
  for (let age = 0; age <= 2; age++) for (const facing of Object.keys(basis)) {
    for (const wide of [true, false]) {
      const world = corridor(age, facing, { wide }), { bot, moves, tick } = setup(world, world.point(-3, 0))
      const to = world.cell(3, 0), goal = new driver.goals.GoalBlock(to.x, 1, to.z)
      const route = bot.pathfinder.getPathTo(moves, goal)
      assert.equal(route.status, wide || age === 0 ? 'success' : 'noPath', `${age} ${facing} wide=${wide}`)
      if (!wide) continue
      let completed = false; bot.on('goal_reached', () => { completed = true }); bot.pathfinder.setGoal(goal)
      for (let t = 0; t < 240 && !completed; t++) tick()
      assert.equal(completed, true, `${age} ${facing} stopped at ${bot.entity.position}`)
    }
  }
})

function matureGap () {
  const at = (x, y, z) => {
    let name = y < 1 || y >= 3 || x < 0 || x > 1 || z >= 2 ? 'stone' : 'air'
    if (z === 0 && y >= 1 && y <= 2 && (x === 0 || x === 1)) name = 'cocoa'
    const b = Block.fromProperties(name, name === 'cocoa' ? { age: 2, facing: x === 0 ? 'west' : 'east' } : {}, 0)
    b.position = new Vec3(x, y, z)
    return b
  }
  return { at, getBlock: p => { const q = p.floored(); return at(q.x, q.y, q.z) } }
}
test('opposing mature pods leave a cross-cell 0.875-wide gap that native walking traverses intact', () => {
  const world = matureGap(), geometry = createTerrainGeometry(world.at)
  const stance = geometry.stand(0, 1, 0)
  assert.ok(stance.centerX > 0.86 && stance.centerX < 0.9)
  assert.ok(stance.centerX + 0.3 > 1, 'the footprint borrows checked ground in the adjacent cell')
  assert.equal(noStanding(world.at, { x: 0, y: 1, z: 0 }, 0), null, 'an attainable cocoa-cell goal is not refused as a whole solid block')
  const unsupported = createTerrainGeometry((x, y, z) => x === 1 && y === 0 ? Block.fromProperties('air', {}, 0) : world.at(x, y, z))
  assert.equal(unsupported.stand(0, 1, 0), null, 'never borrow missing ground')
  for (const start of [new Vec3(0.5, 1, -2.5), new Vec3(stance.centerX, 1, stance.centerZ)]) {
    const { bot, moves, tick } = setup(world, start)
    const goal = new driver.goals.GoalBlock(0, 1, 1)
    const route = bot.pathfinder.getPathTo(moves, goal)
    assert.equal(route.status, 'success')
    assert.ok(route.path.every(p => !p.toBreak.length && !p.toPlace.length))
    let completed = false; bot.on('goal_reached', () => { completed = true }); bot.pathfinder.setGoal(goal)
    for (let i = 0; i < 240 && !completed; i++) tick()
    assert.equal(completed, true, `start ${start} stopped at ${bot.entity.position}`)
    assert.equal(bot.entity.position.y, 1)
    assert.equal(world.at(0, 1, 0).name, 'cocoa')
  }
})
test('the cocoa collar is a passable gap, not a pit that redirects a ground goal onto the pods', () => {
  const at = (x, y, z) => {
    let name = y < 1 ? 'grass_block' : 'air', facing
    if (y >= 1 && y <= 3) for (const [tx, tz] of [[-1, 0], [2, 0], [-1, -3], [2, -3]]) {
      if (x === tx && z === tz) name = 'jungle_log'
      if (x === tx + 1 && z === tz) { name = 'cocoa'; facing = 'west' }
      if (x === tx - 1 && z === tz) { name = 'cocoa'; facing = 'east' }
      if (x === tx && z === tz + 1) { name = 'cocoa'; facing = 'north' }
      if (x === tx && z === tz - 1) { name = 'cocoa'; facing = 'south' }
    }
    const b = Block.fromProperties(name, name === 'cocoa' ? { age: 2, facing } : {}, 0)
    b.position = new Vec3(x, y, z)
    return b
  }
  const world = { at, getBlock: p => { const q = p.floored(); return at(q.x, q.y, q.z) } }
  const cellAt = (x, y, z) => {
    const b = world.at(x, y, z)
    return { name: b.name, solid: b.boundingBox === 'block', shapes: b.shapes, properties: b.getProperties(), liquid: false, crop: false }
  }
  const goal = { x: 0, y: 1, z: -1 }, from = { x: 1, y: 1, z: 3 }
  const before = rimGoal((x, y, z) => { const { shapes, ...legacy } = cellAt(x, y, z); return legacy }, goal, 0, { from })
  assert.ok(before, 'old bounding-box-only pit diagnosis redirects this physically open collar')
  assert.equal(rimGoal(cellAt, goal, 0, { from }), null)
  const { bot, moves, tick } = setup(world, new Vec3(1.07, 1, 2.52))
  const target = new driver.goals.GoalBlock(goal.x, goal.y, goal.z)
  const route = bot.pathfinder.getPathTo(moves, target)
  assert.equal(route.status, 'success')
  assert.ok(route.path.every(p => p.y === 1 && !p.toBreak.length && !p.toPlace.length))
  let completed = false; bot.on('goal_reached', () => { completed = true }); bot.pathfinder.setGoal(target)
  for (let i = 0; i < 240 && !completed; i++) tick()
  assert.equal(completed, true, String(bot.entity.position))
  assert.equal(bot.entity.position.y, 1)
  const insidePod = new driver.goals.GoalBlock(1, 1, 0)
  assert.equal(bot.pathfinder.getPathTo(moves, insidePod).status, 'success', 'corner stance remains connected back into the mature-pod gap')
  completed = false; bot.pathfinder.setGoal(insidePod)
  for (let i = 0; i < 240 && !completed; i++) tick()
  assert.equal(completed, true, String(bot.entity.position))
  assert.equal(bot.entity.position.y, 1)
  const p = bot.entity.position
  assert.ok(createTerrainGeometry(world.at).clearBox([p.x - 0.3, p.y, p.z - 0.3, p.x + 0.3, p.y + 1.8, p.z + 0.3]), 'native fullStop must preserve the offset instead of snapping into the pod')
  completed = false; bot.pathfinder.setGoal(new driver.goals.GoalBlock(0, 1, 0))
  for (let i = 0; i < 240 && !completed; i++) tick()
  assert.equal(completed, true, `mirrored crossing stopped at ${bot.entity.position}`)
})

test('shape-aware goal and pit checks preserve verified climbing capabilities', () => {
  for (const name of ['scaffolding', 'weeping_vines']) for (const enabled of [false, true]) {
    const at = (x, y, z) => {
      const block = Block.fromProperties(x === 0 && z === 0 && y >= 1 ? name : 'air', name === 'scaffolding' ? { bottom: false, distance: 0 } : {}, 0)
      return { name: block.name, shapes: block.shapes, properties: block.getProperties(), solid: block.boundingBox === 'block', scaffoldingSupported: enabled, climbableVinesSupported: enabled }
    }
    const goal = { x: 0, y: 3, z: 0 }
    assert.equal(noStanding(at, goal, 0) === null, enabled, name)
    if (enabled) assert.equal(rimGoal(at, goal, 0, { from: { x: 8, y: 3, z: 0 } }), null)
  }
})
