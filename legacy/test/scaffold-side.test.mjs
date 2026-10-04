import test from 'node:test'
import assert from 'node:assert/strict'
import { scaffoldSide } from '../src/scaffold/side.mjs'
import { visibleTreeBlock } from '../src/scaffold/reach.mjs'
class Vec {constructor(x,y,z){Object.assign(this,{x,y,z})}offset(x,y,z){return new Vec(this.x+x,this.y+y,this.z+z)}}
function fixture({distance=0,cancelAt}={}){
 let stock=8,placed=false,clicks=0,checks=0
 const bot={entity:{position:new Vec(.5,6,.5)},blockAt:p=>p.x===0&&p.y===5&&p.z===0?{name:'scaffolding',getProperties:()=>({distance})}:{name:placed&&p.x===1&&p.y===5?'scaffolding':'air',getProperties:()=>({distance:distance+1})},equip:async()=>{},setControlState:(k,v)=>assert.deepEqual([k,v],['sneak',false]),look:async(yaw)=>assert.equal(yaw,-Math.PI/2),_genericPlace:async(b,face,opts)=>{clicks++;assert.equal(face.y,1);assert.equal(opts.forceLook,'ignore');placed=true;stock--},waitForTicks:async()=>{}}
 return {ctx:{bot,Vec3:Vec,refusalFor:()=>null,cancelGuard:()=>()=>{if(++checks===cancelAt)throw Error('cancelled')},inventoryCounts:()=>({scaffolding:stock}),findItem:()=>({name:'scaffolding'})},clicks:()=>clicks}
}
const args={x:1,y:5,z:0,from_x:0,from_y:5,from_z:0}
test('side scaffold preserves horizontal facing through top-face packet and verifies stock and block',async()=>{
 const f=fixture({distance:5});assert.equal((await scaffoldSide(args,f.ctx)).placed,1);assert.equal(f.clicks(),1)
})
test('side scaffold refuses unsupported seventh span before equip or click',async()=>{
 const f=fixture({distance:6});await assert.rejects(scaffoldSide(args,f.ctx),/distance below 6/);assert.equal(f.clicks(),0)
})
test('side scaffold cancellation after equip or look never sends a placement',async()=>{
 for(const cancelAt of [1,2]){const f=fixture({cancelAt});await assert.rejects(scaffoldSide(args,f.ctx),/cancelled/);assert.equal(f.clicks(),0)}
})
test('side scaffold rejects wrong height, occupied headroom and false inventory success',async()=>{
 let f=fixture();await assert.rejects(scaffoldSide({...args,y:6},f.ctx),/adjacent same-height/)
 f=fixture();const old=f.ctx.bot.blockAt;f.ctx.bot.blockAt=p=>p.x===1&&p.y===7?{name:'bee_nest'}:old(p);await assert.rejects(scaffoldSide(args,f.ctx),/headroom/);assert.equal(f.clicks(),0)
 f=fixture();f.ctx.inventoryCounts=()=>({scaffolding:8});await assert.rejects(scaffoldSide(args,f.ctx),/did not take/)
})
test('tree reach rejects intervening deck or wall even when Euclidean reach succeeds',()=>{
 const feet={x:.5,y:5,z:.5},target={x:2,y:5,z:0}
 assert.equal(visibleTreeBlock(()=>({name:'air'}),feet,target),true)
 assert.equal(visibleTreeBlock((x,y,z)=>({name:x===1?'scaffolding':'air'}),feet,target),false)
 assert.equal(visibleTreeBlock(()=>null,feet,target),false)
})

test('side scaffold cannot claim success with unknown or invalid placed support distance',async()=>{
 for(const distance of [undefined,NaN,-1,7]){
  const f=fixture(),read=f.ctx.bot.blockAt
  f.ctx.bot.blockAt=p=>{const b=read(p);return p.x===1&&b.name==='scaffolding'?{...b,getProperties:()=>({distance})}:b}
  await assert.rejects(scaffoldSide(args,f.ctx),/did not take/)
 }
})
