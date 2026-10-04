import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { performance } from 'node:perf_hooks'
import { readFileSync } from 'node:fs'
import { createEscortCorridor, configureEscortMoves, equine, escortAtDestination, runSurfaceEscort } from '../src/navigation/escort.mjs'
import { configureTerrainMoves } from '../src/navigation/terrain-moves.mjs'
import { farmWalk } from '../src/lib/path.mjs'
const require = createRequire(import.meta.url)
const registry = require('minecraft-data')('26.1')
const Block = require('prismarine-block')(registry), { Vec3 } = require('vec3')
const { Movements, goals } = require('mineflayer-pathfinder')
const Move = require('mineflayer-pathfinder/lib/move'), AStar = require('mineflayer-pathfinder/lib/astar')
const { Physics, PlayerState } = require('prismarine-physics')
const point = (x, y = 1, z = 0.5) => new Vec3(x, y, z)
function world (choose = () => undefined) {
  return (x, y, z) => {
    const name = choose(x, y, z) ?? (y < 1 ? 'grass_block' : 'air')
    if (name === 'unknown') return null
    const b = Block.fromProperties(name, { waterlogged: false }, 0); b.position = new Vec3(x, y, z); return b
  }
}
const options = (at, from = point(0.5), to = point(6.5)) => ({ blockAt: at, from, to, maxY: 320 })
const corridor = at => createEscortCorridor(at, options(at))
function moves (at, checked = true, from = point(0.5), to = point(6.5)) {
  const bot = { registry, entities: {}, entity: { effects: {} }, pathfinder: { bestHarvestTool: () => null }, game: { minY: -64 }, inventory: { items: () => [] }, blockAt: p => at(p.x, p.y, p.z) }
  const m = new (farmWalk(Movements))(bot)
  m.canDig = false; m.allowParkour = false; m.allow1by1towers = false; m.scafoldingBlocks = []
  configureTerrainMoves(m, { blockAt: at })
  return checked ? configureEscortMoves(m, options(at, from, to)) : m
}
function route (m, from, to) {
  const search = new AStar(new Move(...from, 0, 0), m, new goals.GoalBlock(...to), Infinity, Infinity, 12)
  let result
  do { result = search.compute() } while (result.status === 'partial')
  return result
}
test('arrival is relative to destination, not the nearby body', () => {
  assert.equal(escortAtDestination(point(0.5), point(30.5)), false)
  assert.equal(escortAtDestination(point(28.5), point(30.5)), true)
})
test('full horse footprint rejects narrow player passages, unsupported edges, leaves and hazards', () => {
  assert.ok(corridor(world()).edge(point(0.5), point(6.5)))
  for (const choose of [
    (x,y,z) => x === 3 && z === 1 && y === 1 ? 'oak_leaves' : undefined,
    (x,y,z) => x === 3 && z === 1 && y <= 0 ? 'air' : undefined,
    (x,y) => x === 3 && y === 0 ? 'magma_block' : undefined,
    (x,y) => x === 3 && y === 0 ? 'water' : undefined,
    (x,y) => x === 3 && y === 0 ? 'oak_leaves' : undefined,
    (x,y) => x === 3 && y === 1 ? 'bamboo' : undefined,
    (x,y) => x === 3 && y === 10 ? 'unknown' : undefined
  ]) assert.equal(corridor(world(choose)).edge(point(0.5), point(6.5)), false)
})
test('surface check excludes solid overhangs but permits high leaf canopy and ordinary stone terrain', () => {
  assert.equal(corridor(world((x,y) => x === 3 && y === 5 ? 'stone' : undefined)).edge(point(0.5), point(6.5)), false)
  assert.ok(corridor(world((x,y) => y < 1 ? 'stone' : y === 5 ? 'oak_leaves' : undefined)).edge(point(0.5), point(6.5)))
})
test('continuous one-block steps are allowed; larger drops, narrow slopes and ravine detours are excluded', () => {
  const step = world((x,y) => y < (x >= 3 ? 2 : 1) ? 'stone' : 'air')
  const g = createEscortCorridor(step, options(step, point(0.5), point(6.5, 2)))
  assert.ok(g.edge(point(2.5), point(3.5, 2)))
  assert.ok(g.edge(point(3.5, 2), point(2.5)))
  assert.equal(g.edge(point(3.5, 4), point(2.5)), false)
  const ravine = createEscortCorridor(world((x,y) => y < 78 ? 'stone' : 'air'), options(world(), point(417,81,-158), point(385,83,-150)))
  assert.equal(ravine.stance(point(394,78,-171)), null, 'observed Corin detour falls below and south of the surface envelope')
})
test('native horse-sized collision physics independently traverses the accepted single step without jumping', () => {
  const at = world((x,y) => y < (x >= 3 ? 2 : 1) ? 'stone' : 'air')
  const physicsWorld = { getBlock: p => at(Math.floor(p.x), Math.floor(p.y), Math.floor(p.z)) }
  const physics = Physics(registry, physicsWorld)
  physics.playerHalfWidth = 0.7; physics.playerHeight = 1.6; physics.stepHeight = 1
  const shadow = { version: '26.1', entity: { position: point(0.5), velocity: point(0,0,0), onGround: true, effects: {}, yaw: -Math.PI / 2, pitch: 0, attributes: {} }, inventory: { slots: [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0 }
  const state = new PlayerState(shadow, { forward: true, back: false, left: false, right: false, sneak: false, jump: false, sprint: false })
  for (let i = 0; i < 60; i++) physics.simulatePlayer(state, physicsWorld)
  assert.ok(state.pos.x > 5, JSON.stringify(state.pos))
  assert.equal(state.pos.y, 2)
})
test('actual native A* cannot replace a wide surface route with a player-only tunnel', t => {
  const at = world((x,y,z) => x === 3 && y >= 1 && y < 4 && Math.abs(z) <= 2 && z !== 0 ? 'stone' : x === 3 && z === 0 && y === 3 ? 'stone' : undefined)
  assert.equal(route(moves(at, false), [0,1,0], [6,1,0]).status, 'success')
  const began = performance.now(), m = moves(at), result = route(m, [0,1,0], [6,1,0])
  assert.equal(result.status, 'success')
  assert.ok(result.path.some(p => Math.abs(p.z) >= 4), 'wide route goes around the wall instead of through the tunnel')
  assert.ok(m.exclusionAreasStep.length > 0, 'native player-only shortcut is disabled')
  t.diagnostic(`native surface search ${(performance.now() - began).toFixed(1)}ms`)
})
function runtime (change = () => {}) {
  let time = 0, pos = point(0.5), hp = 20, active = false, starts = 0, stops = 0
  const animal = { position: point(0.5) }
  const args = { position: () => pos, animals: () => [animal], destination: point(8.5), check: () => {}, now: () => time, health: () => hp,
    start: () => { active = true; starts++ }, stop: () => { active = false; stops++ }, refresh: () => {}, corridor: () => ({ edge: () => true }),
    pause: async () => { time += 250; change({ time, animal, setPosition: p => { pos = p }, setHealth: value => { hp = value } }) } }
  return { args, state: () => ({ time, active, starts, stops }) }
}
test('bounded escort stops native movement on static stalls and damage; cancellation propagates with no fetch walk', async () => {
  const stalled = runtime()
  assert.match((await runSurfaceEscort(stalled.args)).why, /no sustained/)
  assert.equal(stalled.state().time, 20000); assert.equal(stalled.state().active, false)
  const hurt = runtime(({ setHealth }) => setHealth(19))
  assert.match((await runSurfaceEscort(hurt.args)).why, /damage/)
  assert.equal(hurt.state().time, 250); assert.equal(hurt.state().active, false)
  const cancelled = runtime()
  cancelled.args.check = () => { if (cancelled.state().time >= 250) throw new Error('cancelled') }
  await assert.rejects(runSurfaceEscort(cancelled.args), /cancelled/)
  assert.equal(cancelled.state().active, false)
})
test('taut or obstructed leads wait only ten seconds and never start a retrieval route', async () => {
  const f = runtime(); f.args.corridor = () => ({ edge: () => false })
  assert.match((await runSurfaceEscort(f.args)).why, /cannot follow/)
  assert.equal(f.state().time, 10000); assert.equal(f.state().starts, 0)
})
test('arrival requires horse and leader, not just a successful player goal', async () => {
  const f = runtime(({ animal, setPosition }) => { setPosition(point(8.5)); animal.position = point(7.5) })
  assert.equal((await runSurfaceEscort(f.args)).arrived, true)
  assert.equal(f.state().active, false)
})
test('losing one lead stops the whole escort instead of reporting success with the remaining horse', async () => {
  const f = runtime()
  const herd = [{ position: point(0.5) }, { position: point(0.5) }]
  f.args.animals = () => f.state().time ? herd.slice(0,1) : herd
  assert.match((await runSurfaceEscort(f.args)).why, /lead broke/)
  assert.equal(f.state().active, false)
})

test('runtime tether checks tolerate bounded horse bobbing while static unsupported nodes remain blocked', () => {
  const g = corridor(world())
  assert.equal(g.stance(point(0.5,1.01)), null)
  assert.ok(g.followingEdge(point(0.5,1.01), point(3.5)))
  assert.ok(g.followingEdge(point(0.5,1.5), point(3.5)))
  assert.equal(g.followingEdge(point(0.5,3), point(3.5)), false)
})
test('airborne leader frames do not cancel a checked step; native goal range determines arrival', async () => {
  const f = runtime(({ animal, setPosition }) => { setPosition(point(6.5)); animal.position = point(6.5) })
  f.args.grounded = () => false
  f.args.corridor = () => ({ edge: () => false })
  f.args.leaderArrived = p => new goals.GoalNear(8,1,0,2).isEnd(p.floored())
  assert.equal((await runSurfaceEscort(f.args)).arrived, true)
  assert.equal(f.state().starts, 1)
})
test('production leadWalk resumes an attached horse with no carried leads and does not fetch or collect after damage', async () => {
  const source = readFileSync(new URL('../src/body/helpers.mjs', import.meta.url), 'utf8').split('export async function leadWalk (a) {')[1]
  let clock = 0, started = 0, stopped = 0
  const horse = { id: 2, name: 'horse', isValid: true, position: point(0.5), width: 1.4, height: 1.6 }
  const at = world()
  const bot = { entity: { id: 1, position: point(0.5), onGround: true }, health: 20, entities: { 2: horse }, game: { minY: 0, height: 24 },
    blockAt: p => at(p.x,p.y,p.z), waitForTicks: async () => { clock += 250; bot.health = 19 },
    pathfinder: { setMovements: () => {}, setGoal: goal => { if (goal) started++; else stopped++ } } }
  const forbidden = () => { throw new Error('unexpected fetch, attachment or collection') }
  const env = { bot, Vec3, goals, equine, escortAtDestination, configureEscortMoves, penAround: () => null,
    leashHolderOf: () => 1, onMyLeads: () => [horse], leadsCarried: () => 0, leashCandidate: forbidden, leashPlan: forbidden,
    cancelGuard: () => () => {}, setLeading: () => {}, setFollowing: () => {},
    makeMoves: () => ({ getNeighbors: () => [], exclusionAreasStep: [] }), reportPerformance: () => {},
    runSurfaceEscort: args => runSurfaceEscort({ ...args, now: () => clock }), lastPath: null,
    cellOf: () => '0,1,0', pos: () => '0,1,0', leashOne: forbidden, unleashOne: forbidden, sweepDrops: forbidden }
  const leadWalk = new Function(...Object.keys(env), 'return async function leadWalk(a) {' + source)(...Object.values(env))
  const result = await leadWalk({ mob: 'horse', x: 8, y: 1, z: 0 })
  assert.equal(result.arrived, false)
  assert.match(result.why, /damage/)
  assert.equal(result.leadsAttached, 1)
  assert.equal(started, 1)
  assert.ok(stopped >= 1)
})

test('observed offset leader beside a terrace can be followed by a horse which steps up early', () => {
  // Synthetic loaded completion of the observed local terrace; exact live
  // actor positions are retained, without claiming an entire captured chunk.
  const horse=point(-15.812969676457474,67,-148.62891168590812), leader=point(-17.5,67,-149.5)
  const at=world((x,y,z)=>y<67?'grass_block':y===67&&x<=-19&&(z===-150||z===-149)?'grass_block':'air')
  for(const goal of [point(-28,67,-148),point(-16,67,-140)]) {
    const g=createEscortCorridor(at,{from:leader,to:goal,maxY:75})
    assert.equal(g.stance(leader).height,68,'wide horse must step before the narrower player does')
    assert.ok(g.edge(horse,leader),'planned support envelope already accepts this natural step')
    assert.ok(g.followingEdge(horse,leader),'live tether check must agree with that supported approach')
  }
})

test('early-step tether allowance retains horse headroom, cliff and current-body collision checks', () => {
  const horse=point(-15.812969676457474,67,-148.62891168590812), leader=point(-17.5,67,-149.5)
  for(const obstacle of ['ceiling','two-high','cliff','horse-collision']) {
    const at=world((x,y,z)=>{
      if(obstacle==='ceiling'&&y===69&&x===-18&&z===-150)return'oak_leaves'
      if(obstacle==='cliff'&&x===-17&&z===-150&&y<=66)return'air'
      if(obstacle==='horse-collision'&&y===67&&x===-16&&z===-149)return'oak_leaves'
      if(y<67)return'grass_block'
      if(x<=-19&&(z===-150||z===-149)&&y<=(obstacle==='two-high'?68:67))return'grass_block'
      return'air'
    })
    const g=createEscortCorridor(at,{from:leader,to:point(-16,67,-140),maxY:75})
    assert.equal(g.followingEdge(horse,leader),false,obstacle)
  }
})
test('native horse-sized physics executes the diagonal early step beside the live leader position', () => {
  const at=world((x,y,z)=>y<67?'grass_block':y===67&&x<=-19&&(z===-150||z===-149)?'grass_block':'air')
  const physicalWorld={getBlock:p=>at(Math.floor(p.x),Math.floor(p.y),Math.floor(p.z))}
  const physics=Physics(registry,physicalWorld)
  physics.playerHalfWidth=1.3964844/2;physics.playerHeight=1.6;physics.stepHeight=1
  const horse=point(-15.812969676457474,67,-148.62891168590812), leader=point(-17.5,67,-149.5)
  const control={forward:true,back:false,left:false,right:false,jump:false,sprint:false,sneak:false}
  const shadow={version:'26.1',entity:{position:horse,velocity:point(0,0,0),onGround:true,effects:{},attributes:{},yaw:Math.atan2(horse.x-leader.x,horse.z-leader.z),pitch:0},inventory:{slots:[]},jumpTicks:0,jumpQueued:false,fireworkRocketDuration:0}
  const state=new PlayerState(shadow,control)
  for(let tick=0;tick<60;tick++) {
    if(Math.hypot(state.pos.x-leader.x,state.pos.z-leader.z)<0.2)control.forward=false
    physics.simulatePlayer(state,physicalWorld)
  }
  assert.equal(state.pos.y,68)
  assert.ok(Math.hypot(state.pos.x-leader.x,state.pos.z-leader.z)<0.4)
  assert.equal(control.jump,false)
})
