import test from 'node:test'
import assert from 'node:assert/strict'
import { approachHorseSurface } from '../src/navigation/horse-approach.mjs'
function fixture () {
  const entity={id:2,isValid:true,position:{x:8.5,y:64,z:0.5}},bot={health:20,entity:{id:1,onGround:true,position:{x:0.5,y:64,z:0.5}},entities:{2:entity}}
  let time=0,calls=0
  const f={bot,entity,check:()=>{},now:()=>time,advance:ms=>{time+=ms},surfaceWalk:{walk:async(args,{check})=>{calls++;check();return{arrived:false,status:'noPath'}}},calls:()=>calls}
  return f
}
test('bounded horse approach refreshes a moved target only after the previous walk settles',async()=>{
  const f=fixture();let active=false,attempts=0
  f.surfaceWalk.walk=async(args,{check})=>{
    assert.equal(active,false);active=true;attempts++
    assert.equal(args.surface,'horse')
    try {
      check()
      if(attempts===1)f.entity.position.x=10
      else f.bot.entity.position.x=8
      check()
      assert.fail('internal target update should interrupt this settled leg')
    }finally{active=false}
  }
  const result=await approachHorseSurface(f)
  assert.equal(result.withinReach,true)
  assert.equal(result.attempts,2)
})
test('distant horse and local threats refuse without starting a walk',async()=>{
  for(const why of ['far','threat','health']){
    const f=fixture()
    if(why==='far')f.entity.position.x=60
    if(why==='threat')f.bot.entities[3]={type:'hostile',position:{x:15,y:64,z:0}}
    if(why==='health')f.bot.health=15
    await assert.rejects(approachHorseSurface(f))
    assert.equal(f.calls(),0)
  }
})
test('no-path, cancellation, damage, changed identity and time limits never become retries',async()=>{
  for(const why of ['noPath','cancel','damage','identity','budget']){
    const f=fixture(),cancel=new Error('cancelled');let attempts=0
    f.surfaceWalk.walk=async(args,{check})=>{
      attempts++
      if(why==='noPath')return{arrived:false,status:'noPath'}
      if(why==='cancel')throw cancel
      if(why==='damage')f.bot.health=19
      if(why==='identity')f.bot.entities[2]={...f.entity}
      if(why==='budget')f.advance(8000)
      check()
    }
    await assert.rejects(approachHorseSurface(f),why==='cancel'?e=>e===cancel:undefined)
    assert.equal(attempts,1)
  }
})
test('capture never reports reach while airborne and enforces the original local area',async()=>{
  const f=fixture();f.bot.entity.position.x=7;f.bot.entity.onGround=false
  await assert.rejects(approachHorseSurface(f),/airborne/)
  assert.equal(f.calls(),0)
  const moving=fixture()
  moving.surfaceWalk.walk=async(args,{check})=>{moving.entity.position.x=18;check()}
  await assert.rejects(approachHorseSurface(moving),/16-block/)
})
