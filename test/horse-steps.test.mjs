import test from 'node:test'
import assert from 'node:assert/strict'
import {EventEmitter} from 'node:events'
import {createRequire} from 'node:module'
import {driveHorse} from '../src/navigation/horse.mjs'
import {HorseStepError} from '../src/navigation/horse-steps.mjs'
const require=createRequire(import.meta.url),{Vec3}=require('vec3')
function fixture(choose=()=>undefined,{from=[0.5,1,0.5],speed=0.29,version='26.1'}={}) {
  const registry=require('minecraft-data')(version),Block=require('prismarine-block')(registry)
  const horse={id:2,name:'horse',isValid:true,width:1.4,height:1.6,position:new Vec3(...from),velocity:new Vec3(0,0,0),attributes:{[registry.attributesByName.movementSpeed.resource]:{value:speed,modifiers:[]}}}
  const packets=[],client=new EventEmitter();client.write=(name,data)=>packets.push({name,data})
  const bot={version,registry,_client:client,health:20,entity:{id:1,position:new Vec3(...from)},vehicle:horse,entities:{2:horse},game:{minY:-64,height:96},supportFeature:()=>true,
    blockAt:p=>{p=p.floored();const name=choose(p)??(p.y<1?'stone':'air');if(name==='unknown')return null;const b=Block.fromProperties(name,{waterlogged:false},0);b.position=p;return b}}
  return{bot,entity:horse,packets,check:()=>{},pause:async()=>{}}
}
const step=p=>p.y<(p.x>=4?2:1)?'stone':'air'
test('preflight and real native controller cross a one-block ascent without jumping',async()=>{
  const f=fixture(step),goal={x:9,y:2,z:0}
  const preview=driveHorse.validate({...f,goal,terrain:'steps'})
  assert.equal(preview.jump,false)
  const result=await driveHorse({...f,goal,terrain:'steps'})
  assert.equal(result.arrived,true)
  assert.equal(f.entity.position.y,2)
  assert.ok(Math.abs(f.entity.position.x-9.5)<=0.35)
  assert.equal(f.packets.some(p=>p.name==='player_input'&&p.data.inputs?.jump),false)
  assert.equal(f.bot._client.listenerCount('vehicle_move'),0)
})
test('descending a checked one-block step sends true airborne state and finishes grounded',async()=>{
  const f=fixture(step,{from:[9.5,2,0.5]})
  const result=await driveHorse({...f,goal:{x:0,y:1,z:0},terrain:'steps'})
  assert.equal(result.arrived,true)
  const positions=f.packets.filter(p=>p.name==='vehicle_move')
  assert.ok(positions.some(p=>p.data.onGround===false))
  assert.equal(positions.at(-1).data.onGround,true)
  assert.equal(f.entity.position.y,1)
})
test('step controller refuses cliffs, unknown terrain, low rider ceilings and narrow footing before writing motion',async()=>{
  for(const choose of [
    p=>p.y<(p.x>=4?3:1)?'stone':'air',
    p=>p.x===5?'unknown':undefined,
    p=>p.x===5&&p.y===3?'oak_leaves':undefined,
    p=>p.x===5&&p.z===1&&p.y<1?'air':undefined,
    p=>p.x===5&&p.y===0?'magma_block':undefined,
    p=>p.x===5&&p.y===0?'water':undefined
  ]) {
    const f=fixture(choose)
    await assert.rejects(driveHorse({...f,goal:{x:9,y:1,z:0},terrain:'steps'}),HorseStepError)
    assert.equal(f.packets.length,0)
  }
})

test('multiple broad natural terraces and diagonal step legs remain physically executable',async()=>{
  for(const version of ['1.21.5','26.1'])for(const speed of [0.18,0.29,0.34]) {
    const f=fixture(p=>p.y<(p.x>=7?3:p.x>=4?2:1)?'stone':'air',{version,speed})
    assert.equal((await driveHorse({...f,goal:{x:11,y:3,z:2},terrain:'steps'})).arrived,true)
    assert.equal(f.entity.position.y,3)
    let previous=new Vec3(0.5,1,0.5)
    for(const {data}of f.packets.filter(p=>p.name==='vehicle_move')){
      assert.ok(Math.hypot(data.x-previous.x,data.z-previous.z)<0.6)
      assert.ok(Math.abs(data.y-previous.y)<=1)
      previous=new Vec3(data.x,data.y,data.z)
    }
  }
})
test('cancelling on a descending step settles under neutral physics and keeps the seat',async()=>{
  const f=fixture(step,{from:[9.5,2,0.5]});let cancel=false,airborne=false
  await assert.rejects(driveHorse({...f,terrain:'steps',goal:{x:0,y:1,z:0},check:()=>{if(cancel)throw new Error('cancelled')},pause:async()=>{
    const last=f.packets.filter(p=>p.name==='vehicle_move').at(-1)
    if(last?.data.onGround===false){cancel=true;airborne=true}
  }}),/cancelled/)
  assert.equal(airborne,true)
  assert.equal(f.entity.position.y,1)
  assert.ok(Math.hypot(f.entity.velocity.x,f.entity.velocity.z)<0.003)
  assert.equal(f.bot.vehicle,f.entity)
  assert.equal(f.bot._client.listenerCount('vehicle_move'),0)
  assert.deepEqual(f.packets.at(-1).data.inputs,{})
})
test('server correction is acknowledged and stops step prediction at the authoritative pose',async()=>{
  const f=fixture(step);let ticks=0
  await assert.rejects(driveHorse({...f,goal:{x:9,y:2,z:0},terrain:'steps',pause:async()=>{
    if(++ticks===5)f.bot._client.emit('vehicle_move',{x:1,y:1,z:0.5,yaw:90,pitch:0,onGround:true})
  }}),/server corrected/)
  assert.deepEqual(f.entity.position,new Vec3(1,1,0.5))
  assert.equal(f.packets.filter(p=>p.name==='vehicle_move').at(-1).data.x,1)
  assert.deepEqual(f.packets.at(-1).data.inputs,{})
})
test('changed rider headroom, passenger loss and damage abort without dismounting or jumping',async()=>{
  for(const why of ['ceiling','seat','damage','speed']) {
    let changed=false,ticks=0
    const f=fixture(p=>changed&&why==='ceiling'&&p.x>=2&&p.y===3?'stone':undefined)
    await assert.rejects(driveHorse({...f,goal:{x:9,y:1,z:0},terrain:'steps',pause:async()=>{
      if(++ticks===4){changed=true;if(why==='seat')f.bot.vehicle=null;if(why==='damage')f.bot.health=19;if(why==='speed')f.entity.attributes[f.bot.registry.attributesByName.movementSpeed.resource].value=0.4}
    }}))
    assert.equal(f.packets.some(p=>p.data.inputs?.shift||p.data.inputs?.jump),false)
    assert.equal(f.bot._client.listenerCount('vehicle_move'),0)
  }
})
test('step planning is explicitly bounded and default flat riding still rejects slopes',async()=>{
  const f=fixture(step)
  assert.throws(()=>driveHorse.validate({...f,goal:{x:17,y:2,z:0},terrain:'steps'}),/16 blocks/)
  assert.throws(()=>driveHorse.validate({...f,goal:{x:9,y:2,z:0}}),/flat/)
  const before=performance.now(), reports=[]
  driveHorse.validate({...f,goal:{x:9,y:2,z:0},terrain:'steps',reportPerformance:(...args)=>reports.push(args)})
  assert.equal(reports[0][0],'horse.route')
  assert.ok(performance.now()-before>=reports[0][1])
})

test('preflight rejects other animals and nearby threats before any vehicle input',async()=>{
  for(const hostile of [false,true]) {
    const f=fixture(step)
    f.bot.entities[3]={id:3,name:hostile?'zombie':'cow',type:hostile?'hostile':'animal',position:new Vec3(5,2,hostile?5:0.5),width:0.9,height:1.8}
    await assert.rejects(driveHorse({...f,goal:{x:9,y:2,z:0},terrain:'steps'}),hostile?/danger/:/entity/)
    assert.equal(f.packets.length,0)
  }
})

test('observed pen staging dimensions support a short westward terrace descent without crossing the fence',async()=>{
  // Synthetic continuation of the pilot's observed terrace: upper grass at
  // x12..15, lower grass west, fence row z=-123. Unknown distant routes are
  // not inferred from this local native-physics regression.
  const f=fixture(p=>{
    if(p.z===-123&&p.x>=10&&p.x<=15&&p.y===67)return'oak_fence'
    return p.y<(p.x>=12?67:66)?'grass_block':'air'
  },{from:[12.944,67,-124.053]})
  const result=await driveHorse({...f,goal:{x:8.5,y:66,z:-124.5},terrain:'steps'})
  assert.equal(result.arrived,true)
  assert.equal(f.entity.position.y,66)
  assert.ok(f.entity.position.distanceTo(new Vec3(8.5,66,-124.5))<0.35)
})

test('recorded bank lip geometry permits a natural descent and retains leaf-litter traversal',async()=>{
  // These synthetic broad terraces reproduce the recorded heights, not a
  // claim about the unobserved cells surrounding the live bank.
  const f=fixture(p=>{
    const top=p.z<=-102?66:65
    if(p.y<top)return'grass_block'
    if(p.z===-101&&p.y===65)return'leaf_litter'
    return'air'
  },{from:[13.52,66,-103.49]})
  const preview=driveHorse.validate({...f,goal:{x:13.5,y:66,z:-102.5},terrain:'steps'})
  assert.equal(preview.at.y,66)
  const result=await driveHorse({...f,goal:{x:13.5,y:65,z:-98.5},terrain:'steps'})
  assert.equal(result.arrived,true)
  assert.equal(f.entity.position.y,65)
})

test('unsupported-footing preview identifies the failed native sample and exact block',()=>{
  const f=fixture(p=>p.x===3&&p.y===0?'water':undefined)
  assert.throws(()=>driveHorse.validate({...f,goal:{x:9,y:1,z:0},terrain:'steps'}),error=>{
    assert.ok(error instanceof HorseStepError)
    assert.equal(error.detail.support.block,'water')
    assert.deepEqual(error.detail.support.cell,{x:3,y:0,z:-1})
    assert.ok(error.detail.at.x>2&&error.detail.at.x<3)
    assert.equal(error.detail.at.y,1)
    assert.match(error.message,/unsafe or unsupported footing block/)
    return true
  })
  assert.equal(f.packets.length,0)
})

test('mounted step routes allow tree logs above rider clearance but retain real branch and roof collisions',async()=>{
  for(const obstruction of ['high log','low log','stone roof','plank roof','unloaded sky']) {
    const f=fixture(p=>{
      if(p.x>=50&&p.x<=51&&p.z===-41&&p.y===(obstruction==='low log'?69:73))return obstruction==='unloaded sky'?'unknown':obstruction==='stone roof'?'stone':obstruction==='plank roof'?'oak_planks':'oak_log'
      return p.y<66?'grass_block':'air'
    },{from:[47.5,66,-40.5]})
    f.bot.game.height=384
    const go=()=>driveHorse({...f,goal:{x:50.5,y:66,z:-40.5},terrain:'steps'})
    if(obstruction==='high log')assert.equal((await go()).arrived,true)
    else {await assert.rejects(go(),HorseStepError);assert.equal(f.packets.length,0)}
  }
})
