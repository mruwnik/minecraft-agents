import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { compileBlueprintStructure } from '../src/blueprint/compiler.mjs'
import { representativeBlueprint } from '../src/blueprint/palette.mjs'
import { blueprintCells,REGISTRY } from '../src/blueprint/format.mjs'
import { inspectVillage,maintainVillage,executeVillageImport,repairOccupiedVillage } from '../src/villager/maintenance.mjs'
import { WORKSTATION_CLAIM_BASIS } from '../src/villager/population.mjs'
import { mergeVillagerObservation,emptyVillagerRoster } from '../src/villager/roster.mjs'
const A='11111111-1111-4111-8111-111111111111',B='22222222-2222-4222-8222-222222222222'
function fixture({target=2,roles=[],workspaces=[],rows=[A,B],locked=true}={}){
 const source=JSON.parse(fs.readFileSync(new URL('../blueprints/villager-house-10.blueprint.json',import.meta.url)));source.population={target,roles,workspaces};
 for(const role of [...roles.filter(r=>r.workstation),...workspaces.map(w=>({...w,workstation:w.at}))]){const [x,y,z]=role.workstation;const token='Q';source.structure.legend[token]={type:'block',block:{farmer:'composter',fletcher:'fletching_table',librarian:'lectern'}[role.profession]};const layer=source.structure.layers.find(l=>l.y===y),row=[...layer.rows[z]];row[x]=token;layer.rows[z]=row.join('')}
 compileBlueprintStructure(source)
 const world=new Map(),calls=[],key=(x,y,z)=>`${x},${y},${z}`
 const air={name:'air',solid:false,properties:{}}
 for(const c of blueprintCells(representativeBlueprint(source))){const alt=c.spec.alts[0];world.set(key(c.dx,65+c.dy,c.dz),{name:alt.name,solid:REGISTRY.blocksByName[alt.name]?.boundingBox==='block',properties:{...alt.states,...(alt.name.endsWith('_fence_gate')?{open:false}:{})}})}
 let roster=emptyVillagerRoster()
 const entities=rows.map((uuid,i)=>({uuid,id:10+i,name:'villager',baby:false,exact:`${3.5+i*.3},65,3.5`,metadata:JSON.stringify({19:{profession:'farmer',level:1}}),width:.6,height:1.95}))
 for(const e of entities)roster=mergeVillagerObservation(roster,{uuid:e.uuid,at:new Date().toISOString(),by:'Tester',position:{x:3.5,y:65,z:3.5},baby:false,profession:'farmer',...(locked?{purchase:{offer:1,bought:'emerald'}}:{})})
 const api={me:()=> 'Tester',pos:()=>({x:3.5,y:65,z:3.5}),places:()=>[],zones:()=>[],inv:()=>({bread:64,cobblestone:999,torch:20,white_bed:20}),block:(x,y,z)=>world.get(key(x,y,z)) ?? air,checkpoint:async()=>{},report:()=>{},act:async(action,args)=>{
 calls.push({action,args});if(action==='entity')return {found:args.hostile?[]:entities};if(action==='trades'){const profession=JSON.parse(entities.find(e=>e.uuid===args.uuid).metadata)[19].profession;return {profession,level:1,offers:profession==='unemployed'?[]:[{outputItem:{name:profession==='fletcher'?'arrow':'emerald'}}]}};if(action==='path_to')return{status:'success',gates:0};if(action==='goto')return{};if(action==='place'){world.set(key(args.x,args.y,args.z),{name:args.item,solid:false,properties:{}});return{placed:1}};if(action==='toggle'){world.get(key(args.x,args.y,args.z)).properties.open=false;return{}};throw Error(action)
 }}
 return {api,source,entities,world,key,calls,io:{readRoster:()=>roster},a:{plan:source,x:0,y:65,z:0},setRoster:r=>{roster=r}}
}
test('surplus is retained and housing checks actual population rather than a five-bed cap',async()=>{
 const f=fixture({target:1,roles:[{id:'farmer',profession:'farmer'}]});const r=await maintainVillage(f.api,f.a,f.io)
 assert.equal(r.status,'satisfied');assert.equal(r.surplus,1);assert.equal(r.requiredBeds,2);assert.equal(r.structure.usableBeds,10)
 assert.ok(f.calls.every(c=>['entity','trades'].includes(c.action)))
})
test('stale unseen associated villager blocks replacement/breeding after inspection',async()=>{
 const f=fixture({rows:[A]});let roster=f.io.readRoster();roster=mergeVillagerObservation(roster,{uuid:B,at:new Date(Date.now()-999999).toISOString(),by:'Tester',position:{x:4,y:65,z:3},baby:false,profession:'farmer'});f.setRoster(roster)
 const r=await maintainVillage(f.api,{...f.a,breed:{x:1,y:65,z:1,size:8,airlock:true,entryX:9,entryZ:8}},f.io)
 assert.equal(r.status,'blocked');assert.match(r.blocked.join(' '),/unobserved/);assert.ok(!f.calls.some(c=>c.action==='villager.breed'))
})
test('broken occupied boundary refuses mutation; harmless extra table is retained',async()=>{
 const f=fixture();f.world.set(f.key(0,65,1),{name:'air',solid:false,properties:{}})
 const r=await maintainVillage(f.api,f.a,f.io);assert.match(r.blocked.join(' '),/occupied boundary/);assert.ok(!f.calls.some(c=>['place','dig'].includes(c.action)))
 const g=fixture();g.world.set(g.key(3,65,6),{name:'crafting_table',solid:true,properties:{}});const q=await maintainVillage(g.api,g.a,g.io);assert.equal(q.status,'satisfied');assert.ok(q.structure.additions.length)
})
test('missing light repair stays inside, proves gate-free path, then reuses the completed structure',async()=>{
 const f=fixture();f.world.set(f.key(2,65,3),{name:'air',solid:false,properties:{}})
 const r=await maintainVillage(f.api,f.a,f.io);assert.equal(r.status,'satisfied');assert.equal(f.calls.filter(c=>c.action==='place').length,1);assert.equal(f.calls.find(c=>c.action==='goto').args.into,true)
 const before=f.calls.filter(c=>c.action==='place').length;await maintainVillage(f.api,f.a,f.io);assert.equal(f.calls.filter(c=>c.action==='place').length,before)
})
test('unknown headroom and insufficient supported bed capacity prevent breeding',async()=>{
 const f=fixture({target:3});f.api.block=(x,y,z)=>y===67?undefined:f.world.get(f.key(x,y,z)) ?? {name:'air',solid:false,properties:{}}
 let bred=false;const r=await maintainVillage(f.api,{...f.a,breed:{x:1,y:65,z:1}}, {...f.io,breed:async()=>{bred=true}})
 assert.equal(bred,false);assert.equal(r.status,'blocked');assert.match(r.blocked.join(' '),/load every/)
})
test('role reconciliation preserves locked counterparts and only chooses an unassigned eligible UUID',async()=>{
 const f=fixture({roles:[{id:'farm',profession:'farmer'},{id:'arrows',profession:'fletcher',trade:{output:'arrow'},workstation:[3,0,6]}]})
 f.entities[1].metadata=JSON.stringify({19:{profession:'unemployed',level:1}})
 const old=f.io.readRoster();delete old.villagers[B].lockEvidence;old.villagers[B].profession='unemployed';f.setRoster(old)
 let selected
 const r=await maintainVillage(f.api,f.a,{...f.io,roll:async(_api,args)=>{selected=args.uuid;const next=f.io.readRoster();next.villagers[B]={...next.villagers[B],profession:'fletcher',lockEvidence:{profession:'fletcher',basis:'inventory-confirmed villager trade; profession is trade-locked'},offers:{observedAt:new Date().toISOString(),profession:'fletcher',items:[{outputItem:{name:'arrow'}}]}};f.setRoster(next);f.entities[1].metadata=JSON.stringify({19:{profession:'fletcher',level:1}});return{locked:true}}})
 assert.equal(selected,B);assert.ok(r.actions.some(x=>x.uuid===B));assert.ok(!r.actions.some(x=>x.uuid===A))
})
test('configured existing boat imports propagate the actual board ID and reject a changed passenger before any action',async()=>{
 const calls=[],api={checkpoint:async()=>{},act:async(action,args)=>{calls.push({action,args});return action==='boat.board'?{boat:99,boarded:true}:{}}}
 await executeVillageImport(api,{uuid:A,steps:[{action:'boat.board',args:{uuid:A,item:'oak_chest_boat',x:1,y:62,z:2}},{action:'boat.ferry',args:{uuid:A,x:4,y:62,z:2}},{action:'boat.receive',args:{uuid:A,x:5,y:64,z:2}}]})
 assert.equal(calls[1].args.boat,99);assert.equal(calls[2].args.boat,99)
 const before=calls.length;await assert.rejects(executeVillageImport(api,{uuid:A,steps:[{action:'boat.ferry',args:{uuid:B}}]}),/exact imported UUID/);assert.equal(calls.length,before)
})
test('generic locked roles never open unnecessary merchant windows',async()=>{
 const f=fixture({roles:[{id:'farm',count:2,profession:'farmer'}]});const r=await inspectVillage(f.api,f.a,f.io)
 assert.equal(r.report.satisfied,true);assert.ok(!f.calls.some(c=>c.action==='trades'))
})
test('only matching-profession stale offers are deliberately inspected before role intervention',async()=>{
 const f=fixture({roles:[{id:'farm',profession:'farmer'},{id:'arrow',profession:'fletcher',trade:{output:'arrow'}}]})
 f.entities[1].metadata=JSON.stringify({19:{profession:'fletcher',level:1}})
 const roster=f.io.readRoster();roster.villagers[B].profession='fletcher';roster.villagers[B].lockEvidence.profession='fletcher';f.setRoster(roster)
 const r=await inspectVillage(f.api,f.a,f.io)
 assert.equal(r.report.satisfied,true);assert.deepEqual(f.calls.filter(c=>c.action==='trades').map(c=>c.args.uuid),[B])
})
test('full maintenance runs housing before breeding and verifies actual fresh arrivals/births afterwards',async()=>{
 const f=fixture({target:3});let bred=0
 const r=await maintainVillage(f.api,f.a,{...f.io,breed:async(_api,args)=>{bred++;assert.equal(args.target,3);assert.equal(args.size,8);f.entities.push({uuid:'33333333-3333-4333-8333-333333333333',id:99,name:'villager',baby:true,exact:'4.5,65,3.5',metadata:JSON.stringify({19:{profession:'unemployed',level:1}})});return {population:3,newborns:[f.entities.at(-1).uuid]}}})
 assert.equal(bred,1);assert.equal(r.status,'satisfied');assert.equal(r.population,3);assert.equal(r.requiredBeds,3)
})
test('an authored floor-and-bed plan cannot claim safe housing without actual enclosing walls and roof',async()=>{
 const f=fixture({roles:[{id:'farm',count:2,profession:'farmer'}]});f.source.structure.layers=f.source.structure.layers.filter(l=>l.y!==3)
 for(let x=0;x<10;x++)for(let z=0;z<10;z++)f.world.set(f.key(x,68,z),{name:'air',solid:false,properties:{}})
 const r=await maintainVillage(f.api,f.a,f.io);assert.equal(r.populationSatisfied,true);assert.equal(r.status,'blocked');assert.match(r.blocked.join(' '),/roof/)
 assert.ok(!f.calls.some(c=>['place','dig','villager.breed'].includes(c.action)))
})
test('a resident isolated behind the closed internal gate cannot borrow another residents bed capacity',async()=>{
 const f=fixture();f.entities[1].exact='8.5,65,8.5';const r=await maintainVillage(f.api,f.a,f.io)
 assert.equal(r.status,'blocked');assert.equal(r.capacityStatus,'unknown');assert.equal(r.structure.residentBeds.find(r=>r.uuid===B).beds.length,0)
})
test('nested safety hand-backs stop explicit imports before another action',async()=>{
 const calls=[],api={checkpoint:async()=>{},act:async(action,args)=>{calls.push(action);return{stopped:'hungry'}}}
 await assert.rejects(executeVillageImport(api,{uuid:A,steps:[{action:'boat.board',args:{uuid:A}},{action:'boat.ferry',args:{uuid:A}}]}),e=>e.reason==='hungry')
 assert.deepEqual(calls,['boat.board'])
})

test('workstation-bound maintenance persists causal claim evidence and does not treat proximity as a locked association',async()=>{
 const workspace={id:'arrows',at:[3,0,6],profession:'fletcher',trade:{output:'arrow'}}
 const f=fixture({target:1,rows:[A],locked:false,workspaces:[workspace]});f.entities[0].metadata=JSON.stringify({19:{profession:'unemployed',level:1}});f.world.set(f.key(3,65,6),{name:'air',solid:false,properties:{}})
 let called
 f.io.saveObservation=input=>f.setRoster(mergeVillagerObservation(f.io.readRoster(),input))
 f.io.roll=async(api,args)=>{assert.ok(!f.calls.some(c=>c.action==='place'),'workstation must not be pre-built before unemployment is observed');called=args;f.world.set(f.key(3,65,6),{name:'fletching_table',solid:true,properties:{}});f.entities[0].metadata=JSON.stringify({19:{profession:'fletcher',level:1}});const at=new Date().toISOString();f.setRoster(mergeVillagerObservation(f.io.readRoster(),{uuid:A,at,by:'Tester',profession:'fletcher',purchase:{offer:1,bought:'arrow'}}));return{locked:true,workstationClaim:{x:3,y:65,z:6,block:'fletching_table',profession:'fletcher',observedAt:at,basis:WORKSTATION_CLAIM_BASIS}}}
 const result=await maintainVillage(f.api,f.a,f.io);assert.equal(called.proveStation,true);assert.equal(result.status,'satisfied');assert.deepEqual(result.workspaces[0].uuids,[A])
 const locked=fixture({target:1,rows:[A],workspaces:[workspace]});locked.entities[0].metadata=JSON.stringify({19:{profession:'fletcher',level:1}});locked.setRoster(mergeVillagerObservation(locked.io.readRoster(),{uuid:A,at:new Date().toISOString(),by:'Tester',profession:'fletcher',purchase:{offer:1,bought:'arrow'}}))
 const unknown=await maintainVillage(locked.api,locked.a,locked.io);assert.equal(unknown.status,'blocked');assert.equal(unknown.workspaces[0].status,'unknown');assert.ok(!locked.calls.some(c=>['dig','place','trade','villager.roll'].includes(c.action)))
})
