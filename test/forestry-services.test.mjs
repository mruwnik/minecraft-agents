import test from 'node:test'
import assert from 'node:assert/strict'
import { maintainTreeServices } from '../src/tree/services.mjs'
const at={x:0,y:0,z:0},flower={...at,spec:{kind:'flower',item:'dandelion'}},chest={...at,spec:{kind:'chest',item:'chest'}}
function fixture(items={}){
 const world=new Map(),calls=[],report={attention:[]};let pos={x:0.5,y:1,z:0.5}
 const api={block:(x,y,z)=>world.get(`${x},${y},${z}`)??{name:y<=0?'grass_block':'air',solid:y<=0},inv:()=>items,pos:()=>pos,me:()=> 'test',checkpoint:async()=>{},act:async(name,a)=>{calls.push({name,...a});if(name==='zones')return{zones:[]};if(name==='goto')pos={x:a.x+.5,y:a.y,z:a.z+.5};if(name==='dig')world.set(`${a.x},${a.y},${a.z}`,{name:'air',solid:false});if(name==='place'){assert.ok(items[a.item]>0);items[a.item]--;world.set(`${a.x},${a.y},${a.z}`,{name:a.item,solid:true})}return{}}}
 return{api,world,calls,report}
}
test('mapped storage is provisioned from carried chest without surplus',async()=>{const f=fixture({chest:1});await maintainTreeServices(f.api,[chest],f.report);assert.equal(f.api.block(0,1,0).name,'chest');assert.equal(f.report.services_placed,1)})
test('missing chest and dirt are actionable even without harvest surplus; no support removed',async()=>{const f=fixture();f.world.set('0,0,0',{name:'dirt_path',solid:true});await maintainTreeServices(f.api,[flower,{...chest,x:3}],f.report);assert.match(f.report.attention.join(' '),/missing dirt.*missing chest/);assert.ok(!f.calls.some(c=>['dig','place'].includes(c.name)))})
test('owned path support repaired only after moving off it and verifying actual replacement',async()=>{const f=fixture({dirt:1});f.world.set('0,0,0',{name:'dirt_path',solid:true});await maintainTreeServices(f.api,[flower],f.report);assert.equal(f.api.block(0,0,0).name,'dirt');assert.equal(f.report.support_repaired,1);assert.ok(f.calls.findIndex(c=>c.name==='goto')<f.calls.findIndex(c=>c.name==='dig'))})
test('foreign zone and cancellation stop service work without any block mutation',async()=>{
 for(const kind of ['zone','cancel']){const f=fixture({chest:1});if(kind==='zone')f.api.act=async(name)=>name==='zones'?{zones:[{x1:-1,x2:1,y1:-1,y2:3,z1:-1,z2:1,owner:'other',by:'other'}]}:{};else f.api.checkpoint=async()=>{throw Error('cancelled')};await assert.rejects(()=>maintainTreeServices(f.api,[chest],f.report));assert.equal(f.api.block(0,1,0).name,'air')}
})
test('existing container and solid construction retained; failed placement is not claimed',async()=>{const f=fixture({chest:1,dirt:1});f.world.set('0,1,0',{name:'barrel',solid:true});await maintainTreeServices(f.api,[chest],f.report);assert.match(f.report.attention[0],/retained/);assert.equal(f.calls.some(c=>c.name==='dig'),false);const g=fixture({chest:1});const act=g.api.act;g.api.act=(n,a)=>n==='place'?Promise.resolve({}):act(n,a);await maintainTreeServices(g.api,[chest],g.report);assert.match(g.report.attention[0],/unverified/);assert.equal(g.report.services_placed,undefined)})
test('table and torch post are maintained with explicit upper torch dependency',async()=>{
 const f=fixture({crafting_table:1,oak_fence:1,torch:1})
 await maintainTreeServices(f.api,[{...at,x:3,spec:{kind:'table',item:'crafting_table'}},{...at,spec:{kind:'torch',item:'oak_fence'}}],f.report)
 assert.equal(f.api.block(3,1,0).name,'crafting_table');assert.equal(f.api.block(0,1,0).name,'oak_fence');assert.equal(f.api.block(0,2,0).name,'torch')
 const g=fixture({torch:1});await maintainTreeServices(g.api,[{...at,spec:{kind:'torch',item:'oak_fence'}}],g.report)
 assert.ok(!g.calls.some(c=>c.name==='place'));assert.match(g.report.attention.join(' '),/missing oak_fence.*torch waits/)
})
test('forestry days=1 observes an elapsed day even when sleepers skip every night poll',async()=>{
 const {default:maintain}=await import('../library/forestry/maintain.mjs')
 const {migratePlan,planCells}=await import('../src/lib/plan.mjs')
 const f=fixture({oak_sapling:1}),plan=migratePlan({name:'grove',x:0,y:0,z:0,plan:'t'});plan.cells=planCells(plan)
 let elapsed=0,waits=0
 Object.assign(f.api,{plan:()=>plan,places:()=>[plan],emit:()=>{},report:()=>{},clock:()=>({day:true,night:false,elapsedDays:elapsed}),until:async predicate=>{waits++;elapsed=1;assert.equal(predicate(),true,'day transition satisfies wait without ever observing night')}})
 const r=await maintain.run(f.api,{place:'grove',deposit:false,days:1})
 assert.equal(r.sweeps,1);assert.equal(r.planted,1);assert.ok(waits<=2)
})
