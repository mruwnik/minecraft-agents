import test from 'node:test'
import assert from 'node:assert/strict'
import { treePlatformGoto } from '../src/scaffold/access.mjs'
import { spareTest } from '../src/farm/leg.mjs'

const pathFailure = new Error('goto: no walkable path (walks do not dig or bridge)')
const place = { name:'owned-forest', kind:'forest', by:'Tester', x:10, z:20,
 structure:{ legend:{P:'dirt'}, layers:[{y:0,rows:['P.','..']}] } }
const scope = { place:place.name, owner:place.by, x:place.x, z:place.z, width:2, height:2 }

test('tree platform retry is bounded to the verified owned forest and spares its planned columns', async()=>{
 const calls=[]
 const api={me:()=> 'Tester',places:()=>[place],pos:()=>({x:10.5,y:64,z:21.5}),acknowledgeFailure:()=>{},
  act:async(name,args)=>{calls.push({name,args});if(name==='goto'&&!args.dig)throw pathFailure;return {}}}
 assert.equal(await treePlatformGoto(api,{x:11,y:64,z:21,range:0,into:true},{attention:[]},{forestScope:scope}),true)
 assert.equal(calls.length,2)
 assert.deepEqual(calls[0].args,{x:11,y:64,z:21,range:0,into:true,dig:false})
 const retry=calls[1].args
 assert.deepEqual(retry.bounds,{x1:10,z1:20,x2:11,z2:21})
 const excludes=spareTest(retry)
 assert.equal(excludes({x:9,y:70,z:21}),true,'outside the forest cannot be dug')
 assert.equal(excludes({x:10,y:100,z:20}),true,'planned cell columns stay intact at every height')
 assert.equal(excludes({x:11,y:70,z:21}),false,'unplanned cells inside the forest may be cleared')
 assert.equal(excludes({x:11,y:70,z:21},'oak_leaves'),true,'leaf decay remains natural even during a dig retry')
})

test('tree platform does not retry with digging if either endpoint is outside the exact forest rectangle',async()=>{
 const calls=[],api={me:()=> 'Tester',places:()=>[place],pos:()=>({x:9.5,y:64,z:21.5}),acknowledgeFailure:()=>{},
  act:async(name,args)=>{calls.push({name,args});throw pathFailure}}
 const report={attention:[]}
 assert.equal(await treePlatformGoto(api,{x:11,y:64,z:21,range:0,into:true},report,{forestScope:scope}),false)
 assert.equal(calls.length,1)
 assert.match(report.attention.join(' '),/not both inside the verified owned forest plan/)
})

test('tree platform retry is disabled if the saved owner or dimensions no longer match',async()=>{
 const calls=[],api={me:()=> 'Tester',places:()=>[{...place,by:'SomebodyElse'}],pos:()=>({x:10.5,y:64,z:21.5}),acknowledgeFailure:()=>{},
  act:async(name,args)=>{calls.push({name,args});throw pathFailure}}
 const report={attention:[]}
 assert.equal(await treePlatformGoto(api,{x:11,y:64,z:21,range:0,into:true},report,{forestScope:scope}),false)
 assert.equal(calls.length,1)
 assert.match(report.attention.join(' '),/not both inside the verified owned forest plan/)
})
