import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import Module, { createRequire } from 'node:module'
import { readFileSync } from 'node:fs'
import path from 'node:path'
import { makeSurfaceWalkRuntime, surfaceRequest } from '../src/navigation/surface-walk.mjs'
import { configureTerrainMoves } from '../src/navigation/terrain-moves.mjs'
import { farmWalk } from '../src/lib/path.mjs'
import { trackReads } from '../src/lib/composite.mjs'
import { patchPathNodeCopies } from '../src/navigation/terrain.mjs'
const require = createRequire(import.meta.url)
const registry = require('minecraft-data')('26.1'), Block = require('prismarine-block')(registry)
const { Vec3 } = require('vec3')
const driverPath=require.resolve('mineflayer-pathfinder'), driverModule=new Module(driverPath)
driverModule.filename=driverPath; driverModule.paths=Module._nodeModulePaths(path.dirname(driverPath))
driverModule._compile(patchPathNodeCopies(readFileSync(driverPath,'utf8')).source,driverPath)
const driver=driverModule.exports
const { Physics, PlayerState } = require('prismarine-physics')
const target = { surface: 'horse', x: 6, y: 1, z: 0, range: 0, route: true }
const controls = () => ({ forward: false, back: false, left: false, right: false, jump: false, sprint: false, sneak: false })
function fixture ({ choose = () => undefined, tick = true, onPause = () => {}, dangerous = () => false, check = () => {}, go } = {}) {
  let time = 0
  const at = (x,y,z) => {
    const name = choose(x,y,z) ?? (y < 1 ? 'grass_block' : 'air')
    if (name === 'unknown') return null
    const b = Block.fromProperties(name, { waterlogged: false }, 0); b.position = new Vec3(x,y,z); return b
  }
  const world = { getBlock: p => at(Math.floor(p.x),Math.floor(p.y),Math.floor(p.z)) }
  const bot = Object.assign(new EventEmitter(), { version: '26.1', registry, health: 20, game: { minY: -64, height: 96 }, entities: {}, controlState: controls(),
    entity: { id: 1, position: new Vec3(0.5,1,0.5), velocity: new Vec3(0,0,0), onGround: true, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
    inventory: { slots: [], items: () => [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0,
    blockAt: p => world.getBlock(p), physics: Physics(registry,world),
    setControlState (name,value) { this.controlState[name] = value }, clearControlStates () { Object.assign(this.controlState,controls()) },
    look (yaw,pitch) { this.entity.yaw=yaw; this.entity.pitch=pitch } })
  driver.pathfinder(bot)
  const makeMoves = () => {
    const m = new (farmWalk(driver.Movements))(bot)
    m.canDig=false; m.allowParkour=false; m.allow1by1towers=false; m.scafoldingBlocks=[]
    return configureTerrainMoves(m,{blockAt:at})
  }
  const original = makeMoves(); bot.pathfinder.setMovements(original)
  const reports = [], events = []
  const runtime = makeSurfaceWalkRuntime({ getBot: () => bot, Vec3, goals: driver.goals, makeMoves, cancelGuard: () => check,
    ...(go ? { go } : {}),
    report: data => events.push(data), dangerous: p => dangerous(p,bot), now: () => time, reportPerformance: (...args) => reports.push(args),
    pause: async ms => {
      if (tick) for (let i=0;i<Math.ceil(ms/50);i++) {
        bot.emit('physicsTick')
        bot.physics.simulatePlayer(new PlayerState(bot,{...bot.controlState}),world).apply(bot)
      }
      time += ms
      onPause({bot,time})
      await new Promise(resolve => setTimeout(resolve,0))
    } })
  return { bot, runtime, original, reports, events, time:()=>time, advance:ms=>{time+=ms} }
}
test('surface mode requires an explicit short non-digging checkpoint and bounded range', () => {
  const from=new Vec3(0.5,1,0.5)
  for (const a of [{...target,surface:'boat'},{...target,dig:true},{...target,live:true},{...target,player:'Corin'}, {...target,y:undefined},{...target,x:17},{...target,range:3}]) assert.throws(()=>surfaceRequest(a,from))
  assert.equal(surfaceRequest(target,from).distance,6)
})
test('read-only native preview checks forward and reverse paths without changing active movements or goal', () => {
  const f=fixture(), before=f.bot.entity.position.clone(), goal=f.bot.pathfinder.goal
  const result=f.runtime.preview(target)
  assert.equal(result.status,'success')
  assert.ok(result.nodes>0 && result.reverseNodes>0)
  assert.deepEqual(result.retreat,{x:0.5,y:1,z:0.5})
  assert.ok(result.waypoints.every(p=>p.y===1))
  assert.deepEqual(f.bot.entity.position,before)
  assert.equal(f.bot.pathfinder.movements,f.original)
  assert.equal(f.bot.pathfinder.goal,goal)
  assert.equal(f.reports[0][0],'surface.plan')
})
test('unknown terrain, roofs and player-only tunnels never become successful surface previews', () => {
  for (const choose of [
    (x,y,z)=>x>=3?'unknown':undefined,
    (x,y,z)=>x===3&&y===4?'stone':undefined,
    (x,y,z)=>x===3&&y>=1&&(z!==0||y>=3)?'stone':undefined
  ]) {
    const f=fixture({choose})
    const result=f.runtime.preview(target)
    assert.notEqual(result.status,'success')
    assert.equal(f.bot.pathfinder.goal,null)
  }
})
test('a forward path without a complete reverse path is refused before any movement', async () => {
  const f=fixture(), original=f.bot.pathfinder.getPathFromTo
  let searches=0
  f.bot.pathfinder.getPathFromTo=function * (...args) {
    if (++searches===2) { yield {result:{status:'noPath',path:[],visitedNodes:1}}; return }
    yield * original(...args)
  }
  const result=await f.runtime.walk(target)
  assert.equal(result.arrived,false)
  assert.match(result.why,/retreat/)
  assert.equal(f.bot.pathfinder.movements,f.original)
  assert.equal(f.bot.pathfinder.goal,null)
})
test('route-wide hostile checks refuse a hazard beyond current nearby range', () => {
  const f=fixture({dangerous:p=>!!p&&p.x>4})
  assert.match(f.runtime.preview(target).why,/danger/)
  assert.equal(f.bot.pathfinder.goal,null)
})
test('actual native goto and player physics traverse a checked step, then restore the prior profile', async () => {
  const f=fixture({choose:(x,y)=>y<(x>=3?2:1)?'stone':'air'})
  const result=await f.runtime.walk({...target,y:2})
  assert.equal(result.arrived,true, JSON.stringify(result))
  assert.equal(f.bot.entity.position.y,2)
  assert.equal(Math.floor(f.bot.entity.position.x),6)
  assert.equal(f.bot.pathfinder.movements,f.original)
  assert.equal(f.bot.pathfinder.goal,null)
  assert.equal(f.bot.listenerCount('goal_updated'),0)
})
test('damage settles the actual native goto promise before returning and does not start an automatic retreat', async () => {
  const f=fixture({onPause:({bot,time})=>{if(time>=100)bot.health=19}})
  let starts=0; f.bot.on('goal_updated',goal=>{if(goal)starts++})
  await assert.rejects(f.runtime.walk(target),error=>/damage/.test(error.message)&&error.retreat.x===0.5)
  assert.equal(starts,1)
  assert.equal(f.events[0].automaticRetreat,false)
  assert.deepEqual(f.events[0].retreat,{x:0.5,y:1,z:0.5})
  assert.equal(f.bot.listenerCount('path_stop'),0)
  assert.equal(f.bot.pathfinder.goal,null)
  assert.equal(f.bot.pathfinder.movements,f.original)
})
test('cancellation keeps its error identity and static stalls release native goal ownership within twenty seconds', async () => {
  const cancellation=new Error('cancelled'), f=fixture({check:()=>{if(f.time()>=100)throw cancellation}})
  await assert.rejects(f.runtime.walk(target),error=>error===cancellation)
  assert.equal(f.bot.listenerCount('path_stop'),0)
  const stalled=fixture({tick:false})
  await assert.rejects(stalled.runtime.walk(target),/no sustained progress/)
  assert.equal(stalled.time(),20000)
  assert.equal(stalled.bot.pathfinder.goal,null)
  assert.equal(stalled.bot.listenerCount('path_stop'),0)
})

test('slow completed planning remains usable and is reported rather than discarded', () => {
  const f=fixture(), original=f.bot.pathfinder.getPathFromTo
  f.bot.pathfinder.getPathFromTo=function * (...args) {
    for (const value of original(...args)) { f.advance(600); yield value }
  }
  assert.equal(f.runtime.preview(target).status,'success')
  assert.ok(f.reports[0][1]>=1200)
})

test('partial native searches preserve graph nodes while driving a checked detour', async () => {
  const f=fixture({choose:(x,y,z)=>x===3&&y>=1&&y<4&&Math.abs(z)<=2?'stone':undefined})
  f.bot.pathfinder.tickTimeout=0.1
  let partials=0
  f.bot.on('path_update',r=>{if(r.status==='partial')partials++})
  assert.equal(f.runtime.preview(target).status,'success')
  assert.equal((await f.runtime.walk(target)).arrived,true)
  assert.ok(partials>0)
})

test('native jump arrival settles on real ground before reporting success', async () => {
  const nativeGoto=require('mineflayer-pathfinder/lib/goto')
  const f=fixture({choose:(x,y)=>y<(x>=3?2:1)?'stone':'air', go:async(bot,goal)=>{
    await nativeGoto(bot,goal)
    bot.entity.position.y+=0.01
    bot.entity.onGround=false
    bot.entity.velocity.y=-0.0784
  }})
  assert.equal((await f.runtime.walk({...target,y:2})).arrived,true)
  assert.equal(f.bot.entity.onGround,true)
  assert.equal(f.bot.entity.position.y,2)
})
test('a goal which remains airborne fails after bounded settling without another movement goal', async () => {
  const f=fixture({tick:false, go:async bot=>{
    bot.entity.position.set(6.5,1.2,0.5);bot.entity.onGround=false
  }})
  await assert.rejects(f.runtime.walk(target),/ended before reaching/)
  assert.ok(f.time()<=1700)
})
test('route=true is consumed and emits waypoints; an early refusal does not falsely mark it ignored', () => {
  for(const choose of [()=>undefined,(x,y)=>x>=3?'unknown':undefined]) {
    const f=fixture({choose}), tracked=trackReads({...target,route:true})
    const result=f.runtime.preview(tracked.args)
    assert.equal(tracked.unread().includes('route'),false)
    if(result.status==='success')assert.ok(result.waypoints.length>0&&result.reverseWaypoints.length>0)
  }
})

test('damage during the final landing frame still aborts arrival', async () => {
  let settling=false
  const nativeGoto=require('mineflayer-pathfinder/lib/goto')
  const f=fixture({go:async(bot,goal)=>{
    await nativeGoto(bot,goal)
    bot.entity.position.y+=0.01;bot.entity.onGround=false;bot.entity.velocity.y=-0.0784;settling=true
  },onPause:({bot})=>{if(settling&&bot.entity.onGround)bot.health=19}})
  await assert.rejects(f.runtime.walk(target),/damage/)
})

import { approachHorseSurface } from '../src/navigation/horse-approach.mjs'
test('moving-horse capture retargets actual native surface walks and settles before boarding distance',async()=>{
  let moved=false
  const f=fixture({onPause:({bot,time})=>{if(time>=100&&!moved){moved=true;bot.entities[2].position.x=10.5}}})
  const horse={id:2,isValid:true,name:'horse',position:new Vec3(8.5,1,0.5),width:1.4,height:1.6}
  f.bot.entities[2]=horse
  const result=await approachHorseSurface({bot:f.bot,entity:horse,check:()=>{},surfaceWalk:f.runtime,now:f.time})
  assert.equal(result.withinReach,true)
  assert.ok(result.attempts>=2)
  assert.ok(f.bot.entity.position.distanceTo(horse.position)<=2.5)
  assert.equal(f.bot.pathfinder.goal,null)
  assert.equal(f.bot.pathfinder.movements,f.original)
  assert.equal(f.bot.listenerCount('path_stop'),0)
})
