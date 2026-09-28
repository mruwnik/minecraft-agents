import { createRequire } from 'node:module'
import { createEscortCorridor } from './escort.mjs'
import { createTerrainGeometry } from './terrain.mjs'

const require=createRequire(import.meta.url)
const { Physics, PlayerState }=require('prismarine-physics')
const { Vec3 }=require('vec3')
const HALF=0.7, CLEARANCE=3.5
const seat={horse:1.44375,donkey:1.1125,mule:1.2125}
const control=forward=>({forward,back:false,left:false,right:false,jump:false,sprint:false,sneak:false})
const copy=state=>Object.assign(Object.create(Object.getPrototypeOf(state)),state,{pos:state.pos.clone(),vel:state.vel.clone(),control:{...state.control}})
const point=p=>({x:p.x,y:p.y,z:p.z})
export class HorseStepError extends Error {}
const dangerAt=(bot,p)=>Object.values(bot.entities).some(e=>e.isValid!==false&&e.position&&(e.type==='hostile'||e.kind==='Hostile mobs')&&Math.hypot(e.position.x-p.x,e.position.y-p.y,e.position.z-p.z)<12)


const occupied=(bot,entity,a,b)=>Object.values(bot.entities).some(other=>{
  if(other===entity||other===bot.entity||other.isValid===false||!other.position||['item','experience_orb','arrow'].includes(other.name))return false
  const p=other.position,half=(other.width??0.6)/2
  return p.y<Math.max(a.y,b.y)+CLEARANCE&&p.y+(other.height??1.8)>Math.min(a.y,b.y)&&p.x+half>Math.min(a.x,b.x)-HALF&&p.x-half<Math.max(a.x,b.x)+HALF&&p.z+half>Math.min(a.z,b.z)-HALF&&p.z-half<Math.max(a.z,b.z)+HALF
})
const checkTraffic=(bot,entity,a,b)=>{
  if(dangerAt(bot,b))throw new HorseStepError('nearby danger intersects the horse step route')
  if(occupied(bot,entity,a,b))throw new HorseStepError('another entity entered the horse stopping corridor')
}

function geometryFor(bot,from,to) {
  const cache=new Map()
  const at=(x,y,z)=>{
    const key=`${x},${y},${z}`
    if(!cache.has(key))cache.set(key,bot.blockAt(new Vec3(x,y,z)))
    return cache.get(key)
  }
  const corridor=createEscortCorridor(at,{from,to,width:1.4,height:CLEARANCE,padding:0,maxY:(bot.game?.minY??-64)+(bot.game?.height??384)})
  const geometry=createTerrainGeometry(at,{openDoors:false,dry:true,avoidCrops:true})
  let groundFailure
  const ground=p=>{
    groundFailure=null
    for(const y of [Math.floor(p.y),Math.floor(p.y)-1]) {
      let failure
      const s=corridor.stance({x:p.x,y,z:p.z}, detail=>{failure=detail})
      groundFailure??=failure
      if(s&&s.height<=p.y+1e-5&&p.y-s.height<=1.001) {
        for(let x=Math.floor(p.x-HALF);x<=Math.floor(p.x+HALF-1e-7);x++)for(let z=Math.floor(p.z-HALF);z<=Math.floor(p.z+HALF-1e-7);z++) {
          if(/ice$/.test(at(x,Math.ceil(s.height)-1,z)?.name??'')){groundFailure={reason:'ice has unsupported stopping friction',cell:{x,y:Math.ceil(s.height)-1,z}};return null}
        }
        return s.height
      }
      if(s)groundFailure??={reason:'support is above the simulated horse or more than one block below',floor:s.height}
    }
    return null
  }
  const validate=(previous,state,phase)=>{
    const a=previous.pos,b=state.pos
    const support=ground(b)
    if(state.isInWater||state.isInLava||Math.abs(b.y-a.y)>1.001||support===null) {
      const detail={phase,from:point(a),at:point(b),onGround:state.onGround,velocity:point(state.vel),reason:state.isInWater||state.isInLava?'native physics entered fluid':Math.abs(b.y-a.y)>1.001?'native vertical step exceeds one block':groundFailure?.reason??'no checked support',support:groundFailure}
      const error=new HorseStepError(`horse step route needs continuous dry support with at most one-block natural steps; ${JSON.stringify(detail)}`)
      error.detail=detail
      throw error
    }
    const high=Math.max(a.y,b.y)
    // Lift before an ascent; cross before dropping. Never sweep through a
    // floor that native step physics legitimately climbs onto.
    if(!geometry.clearBox([a.x-HALF,a.y,a.z-HALF,a.x+HALF,high+CLEARANCE,a.z+HALF])||
       !geometry.clearBox([b.x-HALF,b.y,b.z-HALF,b.x+HALF,high+CLEARANCE,b.z+HALF])||
       !geometry.clearBox([Math.min(a.x,b.x)-HALF,high,Math.min(a.z,b.z)-HALF,Math.max(a.x,b.x)+HALF,high+CLEARANCE,Math.max(a.z,b.z)+HALF]))throw new HorseStepError('horse and rider lack swept 1.4-wide, 3.5-high step clearance')
  }
  return {ground,validate,reads:()=>cache.size}
}
function model(bot,entity,attributes,yaw) {
  const world={getBlock:p=>bot.blockAt(p.floored())}, physics=Physics(bot.registry,world)
  // Verified vanilla AbstractHorse.createBaseHorseAttributes sets STEP_HEIGHT
  // to1. This models natural collision stepping, never a charged horse jump.
  physics.playerHalfWidth=HALF;physics.playerHeight=1.6;physics.stepHeight=1
  const state=new PlayerState({version:bot.version,entity:{...entity,position:entity.position.clone(),velocity:entity.velocity?.clone()??new Vec3(0,0,0),attributes,effects:entity.effects??{},onGround:true,yaw,pitch:0},inventory:{slots:[]},jumpTicks:0,jumpQueued:false,fireworkRocketDuration:0},control(false))
  state.vel.set(0,-physics.gravity*physics.airdrag,0)
  return {state,physics,world}
}
const stopped=state=>state.onGround&&Math.hypot(state.vel.x,state.vel.z)<0.003&&Math.abs(state.vel.y)<0.09
function advance(model,state,forward,geometry,phase=forward?'forward movement':'neutral movement') {
  const before=copy(state)
  state.control=control(forward)
  model.physics.simulatePlayer(state,model.world)
  geometry.validate(before,state,phase)
  return before
}
function neutralCoast(model,state,geometry) {
  const coast=copy(state),frames=[]
  for(let i=0;i<40;i++) {
    advance(model,coast,false,geometry,`neutral stopping tick ${i}`)
    frames.push(copy(coast))
    if(stopped(coast))return frames
  }
  throw new HorseStepError('horse cannot settle on a checked step landing under neutral input')
}
export function planHorseSteps({bot,entity,goal,speed,attributes}) {
  const from=point(entity.position),to={x:goal.x+(Number.isInteger(goal.x)?0.5:0),y:goal.y,z:goal.z+(Number.isInteger(goal.z)?0.5:0)}
  if(![from,to].every(p=>Object.values(p).every(Number.isFinite))||Math.hypot(to.x-from.x,to.y-from.y,to.z-from.z)>16)throw new HorseStepError('terrain=steps requires a loaded straight horse leg within16 blocks')
  if(!Number.isInteger(from.y)||!Number.isInteger(to.y))throw new HorseStepError('horse step legs start and finish on ordinary whole-block footing')
  if(!Number.isFinite(speed)||speed<=0||speed>0.5||![entity.width,entity.height].every(Number.isFinite)||entity.width>1.4||entity.height>1.61)throw new HorseStepError('horse dimensions or observed movement speed are outside the checked step model')
  if(Math.hypot(entity.velocity?.x??0,entity.velocity?.z??0)>0.05||Math.abs(entity.velocity?.y??0)>0.1)throw new HorseStepError('wait for the horse to stand still before planning a step leg')
  if(bot.health<16)throw new HorseStepError('horse step travel requires at least16 health')
  const yaw=Math.atan2(from.x-to.x,from.z-to.z),m=model(bot,entity,attributes,yaw)
  const geometry=geometryFor(bot,from,to)
  geometry.validate(copy(m.state),m.state,'starting stance')
  const frames=[],distance=Math.hypot(to.x-from.x,to.z-from.z)
  for(let tick=0;tick<400;tick++) {
    if(dangerAt(bot,m.state.pos))throw new HorseStepError('nearby danger intersects the horse step route')
    // Every state, including the full stopping trajectory, is validated before
    // boarding. Pulsed forward input keeps step travel slower than flat travel.
    const coast=neutralCoast(m,m.state,geometry),end=coast.at(-1)
    for(const state of coast)checkTraffic(bot,entity,m.state.pos,state.pos)
    if(end.pos.distanceTo(new Vec3(to.x,to.y,to.z))<=0.35) {
      frames.push(...coast.map(state=>({state,forward:false})))
      return {from,to,yaw,distance,frames,summary:{terrain:'steps',distance,ticks:frames.length,at:point(end.pos),reads:geometry.reads(),naturalStepHeight:1,jump:false}}
    }
    const along=(m.state.pos.x-from.x)*(to.x-from.x)+(m.state.pos.z-from.z)*(to.z-from.z)
    if(along>distance*distance+0.25)throw new HorseStepError('horse step trajectory would overshoot the checked destination')
    const forward=tick%3===0
    const previous=advance(m,m.state,forward,geometry)
    checkTraffic(bot,entity,previous.pos,m.state.pos)
    frames.push({state:copy(m.state),forward})
  }
  throw new HorseStepError('horse cannot reach this step destination without jumping or leaving the checked corridor')
}
function input(bot,forward) {
  if(bot.supportFeature?.('newPlayerInputPacket'))bot._client.write('player_input',{inputs:forward?{forward:true}:{}})
  else bot._client.write('steer_vehicle',{sideways:0,forward:forward?1:0,jump:0})
}
export async function driveHorseSteps({bot,entity,goal,speed,attributes,check,checkPose,pause,report=()=>{},reportPerformance=()=>{}}) {
  check();checkPose()
  const began=performance.now()
  let plan
  try {plan=planHorseSteps({bot,entity,goal,speed,attributes})}finally{reportPerformance('horse.steps',performance.now()-began,{id:entity.id,phase:'departure'})}
  report({action:'ride',status:'step trajectory checked',...plan.summary})
  const m=model(bot,entity,attributes,plan.yaw),initialHealth=bot.health
  let lastSent=copy(m.state)
  let correction=null,completed=false
  const pose=()=>{
    bot.entity.position?.set(entity.position.x,entity.position.y+seat[entity.name]-0.6,entity.position.z)
    bot.entity.yaw=plan.yaw;bot.entity.pitch=0
  }
  const corrected=packet=>{
    correction=packet
    if(['x','y','z'].every(k=>Number.isFinite(packet[k]))) {
      entity.position.set(packet.x,packet.y,packet.z);entity.velocity?.set(0,0,0);pose()
      bot._client.write('vehicle_move',packet)
    }
  }
  const send=forward=>{
    const state=m.state,yaw=180-plan.yaw*180/Math.PI
    input(bot,forward)
    bot._client.write('look',{yaw,pitch:0,onGround:state.onGround,flags:{onGround:state.onGround,hasHorizontalCollision:state.isCollidedHorizontally}})
    bot._client.write('vehicle_move',{...point(state.pos),yaw,pitch:0,onGround:state.onGround})
    entity.position.set(state.pos.x,state.pos.y,state.pos.z);entity.velocity?.set(state.vel.x,state.vel.y,state.vel.z);entity.yaw=plan.yaw;pose()
    lastSent=copy(state)
  }
  bot._client.on('vehicle_move',corrected)
  try {
    for(const frame of plan.frames) {
      check();checkPose()
      if(correction)throw new HorseStepError('server corrected horse step movement; inspect authoritative position')
      if(bot.vehicle?.id!==entity.id)throw new HorseStepError('horse controlling seat was lost')
      if(bot.health<initialHealth||bot.health<16)throw new HorseStepError('horse step travel stopped for damage or low health')
      if(dangerAt(bot,m.state.pos))throw new HorseStepError('nearby danger entered the horse step route')
      const geometry=geometryFor(bot,plan.from,plan.to),previous=advance(m,m.state,frame.forward,geometry)
      if(m.state.pos.distanceTo(frame.state.pos)>0.01)throw new HorseStepError('horse step physics changed from the checked trajectory')
      const coast=neutralCoast(m,m.state,geometry)
      checkTraffic(bot,entity,previous.pos,m.state.pos)
      for(const state of coast)checkTraffic(bot,entity,m.state.pos,state.pos)
      send(frame.forward)
      await pause(50)
    }
    check()
    if(correction||bot.vehicle?.id!==entity.id||bot.health<initialHealth||dangerAt(bot,m.state.pos)||!stopped(m.state)||m.state.pos.distanceTo(new Vec3(plan.to.x,plan.to.y,plan.to.z))>0.4)throw new HorseStepError('horse step arrival was interrupted or not grounded')
    completed=true
    return {arrived:true,mounted:true,...plan.summary,at:point(m.state.pos),prediction:'native step collision physics; no server correction received'}
  } finally {
    if(!completed&&!correction&&bot.vehicle?.id===entity.id&&entity.isValid!==false&&bot.health>0) {
      m.state=copy(lastSent)
      m.state.pos.set(entity.position.x,entity.position.y,entity.position.z)
      if(entity.velocity)m.state.vel.set(entity.velocity.x,entity.velocity.y,entity.velocity.z)
      try {
        for(let i=0;i<40&&!stopped(m.state);i++) {
          if(correction||bot.vehicle?.id!==entity.id||bot.health<=0)break
          const geometry=geometryFor(bot,plan.from,plan.to),previous=advance(m,m.state,false,geometry)
          if(occupied(bot,entity,previous.pos,m.state.pos))break
          send(false);await pause(50)
        }
      }catch {report({action:'ride',status:'neutral input; checked step coast could not finish, inspect mounted state'})}
    }
    bot._client.removeListener('vehicle_move',corrected)
    try{input(bot,false)}catch{}
  }
}
