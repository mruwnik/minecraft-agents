import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createRequire } from 'node:module'
import Module from 'node:module'
import { patchScaffoldingPhysics, patchScaffoldingPreview, patchScaffoldingDriver } from '../src/navigation/scaffolding.mjs'

const require = createRequire(import.meta.url)
const physicsFile = require.resolve('prismarine-physics')
let source = fs.readFileSync(physicsFile, 'utf8')
// Restore the exact original anchors when start-body has already applied patches.
source = source.replace('  function getSurroundingBBs (world, queryBB, entity = null) {', '  function getSurroundingBBs (world, queryBB) {')
  .replace(/            \/\/ patched by bot\/patch-deps\.mjs: context-dependent scaffolding collision[\s\S]*?            for \(const shape of collisionShapes\) \{/, '            for (const shape of block.shapes) {')
  .replaceAll(', entity)', ')')
  .replace('        const onScaffolding = world.getBlock(pos)?.type === scaffoldingId\n        vel.y = Math.max(vel.y, entity.control.sneak && !onScaffolding ? 0 : -physics.ladderMaxSpeed)', '        vel.y = Math.max(vel.y, entity.control.sneak ? 0 : -physics.ladderMaxSpeed)')
  .replace(/    const climbableVine = \[[^\n]+\n    if \(block.type === ladderId \|\| block.type === vineId \|\| climbableVine\) \{ return true \}/, '    if (block.type === ladderId || block.type === vineId) { return true }')
  .replace(/module.exports = \{ Physics, PlayerState, supportsScaffolding: true(?:, supportsClimbableVines: true)? \}/, 'module.exports = { Physics, PlayerState }')
const VERSION = '1.21.5'
const registry = require('minecraft-data')(VERSION)
const Block = require('prismarine-block')(registry)
const { Vec3 } = require('vec3')
function physicsExports (text) {
  const mod = new Module(physicsFile)
  mod.filename = physicsFile
  mod.paths = Module._nodeModulePaths(physicsFile.slice(0, physicsFile.lastIndexOf('/')))
  mod._compile(text, physicsFile)
  return mod.exports
}
const baseline = physicsExports(source)
const patched = physicsExports(patchScaffoldingPhysics(source).source)
const controls = changes => ({ forward: false, back: false, left: false, right: false, jump: false, sprint: false, sneak: false, ...changes })
function simulate (engine, world, { pos = new Vec3(0.5, 0, 0.5), ticks = 100, control = {}, onGround = true } = {}) {
  const bot = {
    version: VERSION,
    entity: { position: pos, velocity: new Vec3(0, 0, 0), onGround, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
    inventory: { slots: [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0
  }
  const state = new engine.PlayerState(bot, controls(control))
  const physics = engine.Physics(registry, world)
  for (let i = 0; i < ticks; i++) physics.simulatePlayer(state, world)
  return state
}
function worldOf (choose) {
  return { getBlock (raw) {
    const p = raw.floored()
    const chosen = choose(p) ?? (p.y < 0 ? { name: 'stone' } : { name: 'air' })
    const block = Block.fromProperties(chosen.name, { waterlogged: false, facing: 'north', bottom: false, distance: 0, ...chosen.properties }, 0)
    block.position = p
    return block
  } }
}
const column = name => worldOf(p => p.x === 0 && p.z === 0 && p.y >= 0 && p.y < 5 ? { name } : null)

test('scaffold patch is idempotent and unsupported source remains untouched', () => {
  const result = patchScaffoldingPhysics(source)
  assert.equal(result.status, 'patched')
  assert.equal(patchScaffoldingPhysics(result.source).status, 'already')
  const unknown = 'function unrelated () {}'
  assert.deepEqual(patchScaffoldingPhysics(unknown), { status: 'anchor missing', source: unknown })
})
test('native physics climbs stacked scaffolding instead of colliding with each deck', () => {
  const world = column('scaffolding')
  const before = simulate(baseline, world, { control: { jump: true } })
  const after = simulate(patched, world, { control: { jump: true } })
  const ladder = simulate(patched, column('ladder'), { control: { jump: true } })
  assert.ok(before.pos.y < 0.2, `baseline ${before.pos.y}`)
  assert.ok(after.pos.y > 4, `patched ${after.pos.y}`)
  assert.ok(ladder.pos.y > 4)
})
test('standing on a scaffold deck stays supported while sneak descends inside a column', () => {
  const world = column('scaffolding')
  const standing = simulate(patched, world, { pos: new Vec3(0.5, 5, 0.5), ticks: 80 })
  assert.equal(standing.pos.y, 5)
  assert.equal(standing.onGround, true)
  const descending = simulate(patched, world, { pos: new Vec3(0.5, 5, 0.5), control: { sneak: true }, ticks: 100 })
  assert.ok(descending.pos.y < 1, `descent ${descending.pos.y}`)
  assert.ok(descending.pos.y >= 0)
})
test('walking can enter scaffold body volume without hitting outline posts', () => {
  const world = worldOf(p => p.x === 0 && p.z === 0 && p.y === 0 ? { name: 'scaffolding' } : null)
  const state = simulate(patched, world, { pos: new Vec3(0.5, 0, 1.5), control: { forward: true }, ticks: 8 })
  assert.ok(state.pos.z < 0.8, `z=${state.pos.z}`)
})
test('a suspended bottom platform catches controlled descent at its lower edge', () => {
  const world = worldOf(p => p.x === 0 && p.z === 0 && p.y === 3 ? { name: 'scaffolding', properties: { bottom: true, distance: 1 } } : null)
  const state = simulate(patched, world, { pos: new Vec3(0.5, 4, 0.5), control: { sneak: true }, ticks: 80 })
  assert.equal(state.pos.y, 3.125)
  assert.equal(state.onGround, true)
  const stable = worldOf(p => p.x === 0 && p.z === 0 && p.y === 3 ? { name: 'scaffolding', properties: { bottom: true, distance: 0 } } : null)
  assert.ok(simulate(patched, stable, { pos: new Vec3(0.5, 4, 0.5), control: { sneak: true }, ticks: 80 }).pos.y < 1)
})
test('ordinary ground and ladder simulations are unchanged', () => {
  for (const world of [worldOf(() => null), column('ladder')]) {
    const args = { control: { forward: true, jump: true }, ticks: 30 }
    const before = simulate(baseline, world, args)
    const after = simulate(patched, world, args)
    assert.deepEqual(after.pos, before.pos)
    assert.deepEqual(after.vel, before.vel)
    assert.equal(after.onGround, before.onGround)
  }
})

test('physics readiness is exported only by a successfully applied patch', () => {
  assert.equal(baseline.supportsScaffolding, undefined)
  assert.equal(patched.supportsScaffolding, true)
})

function compileNative (file, text, substitutes = {}) {
  const mod = new Module(file)
  mod.filename = file
  mod.paths = Module._nodeModulePaths(file.slice(0, file.lastIndexOf('/')))
  const originalRequire = mod.require.bind(mod)
  mod.require = request => request in substitutes ? substitutes[request] : originalRequire(request)
  mod._compile(text, file)
  return mod.exports
}
const previewFile = require.resolve('mineflayer-pathfinder/lib/physics')
const previewSource = fs.readFileSync(previewFile, 'utf8')
const Preview = compileNative(previewFile, patchScaffoldingPreview(previewSource).source, { 'prismarine-physics': patched })
const pathfinderFile = require.resolve('mineflayer-pathfinder')
const driverSource = fs.readFileSync(pathfinderFile, 'utf8')
const nativeDriver = compileNative(pathfinderFile, patchScaffoldingDriver(driverSource).source, { './lib/physics': Preview })

test('native scaffold descent preview generates sneak then releases it on a horizontal leg', () => {
  const world = column('scaffolding')
  const bot = {
    version: VERSION, controlState: controls(), blockAt: p => world.getBlock(p), physics: patched.Physics(registry, world),
    entity: { position: new Vec3(0.5, 5, 0.5), velocity: new Vec3(0, 0, 0), onGround: true, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
    inventory: { slots: [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0
  }
  const preview = new Preview(bot)
  const result = preview.simulateUntil(state => state.pos.y <= 4.15, preview.getController(new Vec3(0.5, 4, 0.5), false, false), 30)
  assert.ok(result.pos.y <= 4.15, `preview descent ${result.pos.y}`)
  assert.equal(result.control.sneak, true)
  preview.getController(new Vec3(2.5, 4, 0.5), false, false)(result, 0)
  assert.equal(result.control.sneak, false)
  assert.equal(result.control.forward, true)
})
test('native preview/driver patches compile, advertise readiness, and are idempotent', () => {
  for (const [patch, original] of [[patchScaffoldingPreview, previewSource], [patchScaffoldingDriver, driverSource]]) {
    const result = patch(original)
    assert.ok(['patched', 'already'].includes(result.status))
    assert.equal(patch(result.source).status, 'already')
    assert.deepEqual(patch('unrelated code'), { status: 'anchor missing', source: 'unrelated code' })
  }
  assert.equal(Preview.supportsScaffoldingDescent, true)
  assert.equal(nativeDriver.supportsScaffoldingDescent, true)
})

test('native physics climbs the exact additional vanilla climbable vine family', () => {
  for (const name of ['weeping_vines', 'weeping_vines_plant', 'twisting_vines', 'twisting_vines_plant', 'cave_vines', 'cave_vines_plant']) {
    const world = column(name)
    assert.ok(simulate(patched, world, { control: { jump: true } }).pos.y > 4, name)
    assert.ok(simulate(baseline, world, { control: { jump: true } }).pos.y < 2, name)
  }
  assert.equal(patched.supportsClimbableVines, true)
})

import { EventEmitter } from 'node:events'
import { configureTerrainMoves } from '../src/navigation/terrain-moves.mjs'
import { patchTerrainWaypoints } from '../src/navigation/terrain.mjs'
function drivenBot (world, position) {
  let clock = 0
  const driver = compileNative(pathfinderFile, patchTerrainWaypoints(patchScaffoldingDriver(driverSource).source).source, {
    './lib/physics': Preview, perf_hooks: { performance: { now: () => clock } }
  })
  const bot = new EventEmitter()
  Object.assign(bot, {
    version: VERSION, registry, game: { minY: -64 }, entities: {}, controlState: controls(),
    entity: { id: 1, position, velocity: new Vec3(0, 0, 0), onGround: true, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
    inventory: { slots: [], items: () => [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0,
    blockAt: p => world.getBlock(p), physics: patched.Physics(registry, world),
    setControlState (name, value) { this.controlState[name] = value },
    clearControlStates () { Object.assign(this.controlState, controls()) },
    look (yaw, pitch) { this.entity.yaw = yaw; this.entity.pitch = pitch }
  })
  driver.pathfinder(bot)
  const moves = new driver.Movements(bot)
  moves.canDig = false
  moves.canOpenDoors = true
  moves.scafoldingBlocks = []
  moves.allow1by1towers = false
  configureTerrainMoves(moves, { blockAt: (x, y, z) => world.getBlock(new Vec3(x, y, z)), scaffolding: true })
  bot.pathfinder.setMovements(moves)
  return {
    bot, moves, goals: driver.goals,
    tick () {
      clock += 50
      bot.emit('physicsTick')
      bot.physics.simulatePlayer(new patched.PlayerState(bot, { ...bot.controlState }), world).apply(bot)
    }
  }
}
const scaffoldShaft = (name = 'scaffolding') => worldOf(p => p.y >= 0 && p.y < 5 ? { name: p.x === 0 && p.z === 0 ? name : 'stone' } : null)
test('native generated scaffold routes execute ascent and descent, releasing controls on completion and cancellation', () => {
  for (const [from, to] of [[0, 4], [4, 0]]) {
    const driven = drivenBot(scaffoldShaft(), new Vec3(0.5, from, 0.5))
    const { bot, goals, moves } = driven
    const goal = new goals.GoalBlock(0, to, 0)
    const plan = bot.pathfinder.getPathTo(moves, goal)
    assert.equal(plan.status, 'success')
    assert.ok(plan.path.length > 0)
    assert.ok(plan.path.every(p => p.x === 0.5 && p.z === 0.5))
    let completed = false
    let sneaked = false
    bot.on('goal_reached', () => { completed = true })
    bot.pathfinder.setGoal(goal)
    for (let i = 0; i < 180 && !completed; i++) {
      driven.tick()
      sneaked ||= bot.controlState.sneak
    }
    assert.equal(completed, true, `${from}->${to}, actual ${bot.entity.position.y}`)
    if (from > to) assert.equal(sneaked, true)
    assert.ok(Math.abs(bot.entity.position.y - to) < 0.25)
    assert.deepEqual(bot.controlState, controls())
  }
  const driven = drivenBot(scaffoldShaft(), new Vec3(0.5, 4, 0.5))
  driven.bot.pathfinder.setGoal(new driven.goals.GoalBlock(0, 0, 0))
  driven.tick()
  assert.equal(driven.bot.controlState.sneak, true)
  driven.bot.pathfinder.setGoal(null)
  assert.deepEqual(driven.bot.controlState, controls())
})
test('blocked scaffold descent retains the native stuck watchdog and clears sneak', () => {
  const world = worldOf(p => p.x === 0 && p.z === 0 && p.y === 3 ? { name: 'scaffolding', properties: { bottom: true, distance: 1 } } : null)
  const driven = drivenBot(world, new Vec3(0.5, 4, 0.5))
  const { bot, goals } = driven
  // Simulate a route invalidated by a newly formed suspended-bottom platform.
  const Move = require('mineflayer-pathfinder/lib/move')
  bot.pathfinder.getPathTo = () => ({ status: 'success', path: [new Move(0, 2, 0, 0, 1)] .map(p => Object.assign(p, { x: 0.5, z: 0.5 })) })
  let stuck = false
  bot.on('path_reset', why => { if (why === 'stuck') stuck = true })
  bot.pathfinder.setGoal(new goals.GoalBlock(0, 2, 0))
  for (let i = 0; i < 100 && !stuck; i++) driven.tick()
  assert.equal(stuck, true)
  assert.equal(bot.controlState.sneak, false)
  assert.ok(bot.entity.position.y >= 3.125)
})

test('native generated ladder and every vanilla vine family route executes centered ascent/descent', () => {
  for (const name of ['ladder', 'vine', 'weeping_vines', 'weeping_vines_plant', 'twisting_vines', 'twisting_vines_plant', 'cave_vines', 'cave_vines_plant']) {
    for (const [from, to] of [[0, 4], [4, 0]]) {
      const driven = drivenBot(scaffoldShaft(name), new Vec3(0.5, from, 0.5))
      const goal = new driven.goals.GoalBlock(0, to, 0)
      assert.equal(driven.bot.pathfinder.getPathTo(driven.moves, goal).status, 'success', name)
      let completed = false
      driven.bot.on('goal_reached', () => { completed = true })
      driven.bot.pathfinder.setGoal(goal)
      for (let i = 0; i < 180 && !completed; i++) driven.tick()
      assert.equal(completed, true, `${name} ${from}->${to} actual ${driven.bot.entity.position.y}`)
      assert.equal(driven.bot.controlState.sneak, false)
      assert.ok(Math.abs(driven.bot.entity.position.y - to) < 0.25, `${name} ${from}->${to} actual ${driven.bot.entity.position.y}`)
      assert.deepEqual(driven.bot.controlState, controls())
    }
  }
})

test('open aligned trapdoors above ladders execute native generated climb routes across registry variants', () => {
  for (const name of ['oak_trapdoor', 'bamboo_trapdoor', 'pale_oak_trapdoor', 'copper_trapdoor']) {
    const world = worldOf(p => {
      if (p.y >= 0 && p.y < 5 && (p.x !== 0 || p.z !== 0)) return { name: 'stone' }
      if (p.x === 0 && p.z === 0 && p.y >= 0 && p.y < 3) return { name: 'ladder' }
      if (p.x === 0 && p.z === 0 && p.y === 3) return { name, properties: { open: true, facing: 'north', half: 'bottom' } }
    })
    const driven = drivenBot(world, new Vec3(0.5, 0, 0.5))
    const goal = new driven.goals.GoalBlock(0, 3, 0)
    assert.equal(driven.bot.pathfinder.getPathTo(driven.moves, goal).status, 'success', name)
    let completed = false
    driven.bot.on('goal_reached', () => { completed = true })
    driven.bot.pathfinder.setGoal(goal)
    for (let i = 0; i < 180 && !completed; i++) driven.tick()
    assert.equal(completed, true, `${name} actual ${driven.bot.entity.position.y}`)
    assert.ok(driven.bot.entity.position.y > 2.8, name)
    assert.deepEqual(driven.bot.controlState, controls())
  }
})

test('closed or mismatched trapdoors do not gain contextual climbing physics', () => {
  for (const properties of [{ open: false, facing: 'north' }, { open: true, facing: 'south' }]) {
    const world = worldOf(p => p.x === 0 && p.z === 0
      ? p.y === 2 ? { name: 'ladder' } : p.y === 3 ? { name: 'copper_trapdoor', properties } : null : null)
    const state = simulate(patched, world, { pos: new Vec3(0.5, 3, 0.5), ticks: 15, control: { jump: true }, onGround: false })
    assert.ok(state.pos.y < 3.5, JSON.stringify(properties))
  }
})

test('native route crosses a supported lateral scaffold deck, retraces it, then descends its own column',()=>{
 const world=worldOf(p=>{
  if(p.x===0&&p.z===0&&p.y>=0&&p.y<=7)return {name:'scaffolding',properties:{distance:0,bottom:false}}
  if(p.y===7&&p.z===0&&p.x>=1&&p.x<=4)return {name:'scaffolding',properties:{distance:p.x,bottom:true}}
 })
 const driven=drivenBot(world,new Vec3(.5,0,.5)),{bot,goals}=driven
 for(const target of [[0,8,0],[1,8,0],[2,8,0],[3,8,0],[4,8,0],[3,8,0],[2,8,0],[1,8,0],[0,8,0],[0,0,0],[-1,0,0]]){
  const goal=new goals.GoalBlock(...target)
  assert.equal(bot.pathfinder.getPathTo(driven.moves,goal).status,'success',`planned ${target}`)
  let done=false;const reached=()=>{done=true};bot.on('goal_reached',reached);bot.pathfinder.setGoal(goal)
  for(let i=0;i<220&&!done;i++)driven.tick()
  bot.off('goal_reached',reached)
  assert.equal(done,true,`${target}: actual ${bot.entity.position}`)
  assert.ok(Math.abs(bot.entity.position.y-target[1])<.6,`verified height ${target}: actual ${bot.entity.position}`)
 }
 assert.deepEqual(bot.controlState,controls())
})
