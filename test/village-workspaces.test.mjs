import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { populationReport,validatePopulation,WORKSTATION_CLAIM_BASIS } from '../src/villager/population.mjs'
import { compileBlueprintStructure } from '../src/blueprint/compiler.mjs'
import { mergeVillagerObservation,emptyVillagerRoster } from '../src/villager/roster.mjs'
import roll from '../src/villager/rolling.mjs'
const A='11111111-1111-4111-8111-111111111111',B='22222222-2222-4222-8222-222222222222',now=Date.now(),stamp=new Date(now).toISOString()
const station={x:3,y:0,z:3},trade={output:'arrow'},intent={target:1,roles:[{id:'arrows',profession:'fletcher',trade}],workspaces:[{id:'shop',at:[3,0,3],profession:'fletcher',trade}]}
const claim={...station,block:'fletching_table',profession:'fletcher',observedAt:stamp,basis:WORKSTATION_CLAIM_BASIS}
const record=(uuid=A)=>({uuid,profession:'fletcher',age:'adult',lastSeenAt:stamp,lastPosition:{x:1,y:0,z:1},lockEvidence:{profession:'fletcher',basis:'inventory-confirmed villager trade; profession is trade-locked'},offers:{observedAt:stamp,profession:'fletcher',items:[{outputItem:{name:'arrow'}}]},workstationClaim:{...claim}})
const options={now,blockAt:()=>({name:'fletching_table'})}
test('one verified station trader also satisfies the same global requirement without inflating the target',()=>{
 const report=populationReport(intent,[record()],options)
 assert.equal(report.satisfied,true);assert.equal(report.population,1);assert.deepEqual(report.roles[0].uuids,[A]);assert.deepEqual(report.workspaces[0].uuids,[A])
 const two={...intent,target:2,roles:[{...intent.roles[0],count:2}]}
 assert.equal(populationReport(two,[record()],options).satisfied,false)
 assert.equal(populationReport(two,[record(),{...record(B),workstationClaim:undefined}],options).satisfied,true)
})
test('proximity, query coordinates, stale claims and changed blocks cannot verify a workspace',()=>{
 for(const update of [{workstationClaim:undefined,workstationObservation:station},{workstationClaim:{...claim,x:4}},{workstationClaim:{...claim,observedAt:new Date(now-999999).toISOString()}},{workstationClaim:{...claim,observedAt:new Date(now+1).toISOString()}}]){
  const report=populationReport(intent,[{...record(),...update}],options);assert.equal(report.roles[0].status,'satisfied');assert.equal(report.workspaces[0].status,'unknown');assert.equal(report.satisfied,false)
 }
 assert.equal(populationReport(intent,[record()],{...options,blockAt:()=>({name:'air'})}).workspaces[0].status,'violated')
})
test('workspace counts, unique cells and authored profession-compatible blocks are validated',()=>{
 assert.throws(()=>validatePopulation({target:2,roles:[{id:'a',profession:'farmer',count:2,workstation:[1,0,1]}]}),/one trader/)
 assert.throws(()=>validatePopulation({...intent,workspaces:[...intent.workspaces,{...intent.workspaces[0],id:'other'}],target:2}),/distinct/)
 assert.throws(()=>validatePopulation({...intent,workspaces:[{id:'farm',at:[1,0,1],profession:'farmer'}]}),/simultaneous/)
 const source=JSON.parse(fs.readFileSync(new URL('../blueprints/villager-house-10.blueprint.json',import.meta.url)));source.population=intent
 assert.throws(()=>compileBlueprintStructure(source),/exact authored workstation/)
 source.structure.legend.Q={type:'block',block:'fletching_table'};const layer=source.structure.layers.find(l=>l.y===0),row=[...layer.rows[3]];row[3]='Q';layer.rows[3]=row.join('')
 assert.equal(compileBlueprintStructure(source).document.population.workspaces[0].id,'shop')
 source.structure.legend.Q.block='lectern';assert.throws(()=>compileBlueprintStructure(source),/for its profession/)
})
test('another verified claimant invalidates prior evidence and changed-block invalidation never refreshes unseen presence',()=>{
 let roster=mergeVillagerObservation(emptyVillagerRoster(),{uuid:A,at:stamp,by:'Probe',profession:'fletcher',position:{x:1,y:0,z:1},workstationClaim:claim})
 const later=new Date(now+1000).toISOString()
 roster=mergeVillagerObservation(roster,{uuid:B,at:later,by:'Probe',workstationClaim:{...claim,observedAt:later}})
 assert.equal(roster.villagers[A].workstationClaim.invalidationReason,'another exact UUID verified a claim at this station')
 roster=mergeVillagerObservation(roster,{uuid:A,at:stamp,by:'OldProbe',workstationClaim:claim});assert.equal(roster.villagers[B].workstationClaim.invalidatedAt,undefined,'older claim must not invalidate newer association');assert.equal(roster.villagers[A].workstationClaim.invalidatedAt,later,'older observation must not resurrect invalidated evidence')
 const seen=roster.villagers[B].lastSeenAt
 roster=mergeVillagerObservation(roster,{uuid:B,at:new Date(now+2000).toISOString(),by:'Probe',invalidateWorkstationClaim:'block removed'})
 assert.equal(roster.villagers[B].lastSeenAt,seen);assert.ok(roster.villagers[B].workstationClaim.invalidatedAt)
})
function rolling(used=0,employed=used>0){
 let profession=employed?'fletcher':'unemployed',block=employed;const calls=[],cell={x:3,y:65,z:3}
 const api={me:()=> 'Probe',places:()=>[],zones:()=>[],inv:()=>({emerald:10,fletching_table:2}),clock:()=>({day:true}),block:(x,y,z)=>{if(x===3&&y===65&&z===3)return{name:block?'fletching_table':'air'};if(y===64||y===68||x===1||x===5||z===2||z===6)return{name:'stone',solid:true};return{name:x===2&&y===65&&z===3?'torch':'air'}},report:()=>{},note:()=>{},checkpoint:async()=>{},until:async fn=>{assert.equal(await fn(),true)},act:async(action,args)=>{
  calls.push({action,args});if(action==='entity')return{found:[{uuid:A,id:1,exact:'3.5,65,4.5',baby:false,metadata:JSON.stringify({16:false,19:{profession,level:1}})}]}
  if(action==='find_blocks')return{positions:block?[cell]:[]};if(action==='trades')return{profession,level:1,offers:[{index:1,inputItem1:{name:'emerald',count:1},outputItem:{name:'arrow',count:16},nbTradeUses:used}]}
  if(action==='dig'){block=false;profession='unemployed'}if(action==='place'){block=true;profession='fletcher'}if(action==='trade')used++
  return{}
 }};return{api,calls,args:{...cell,uuid:A,block:'fletching_table',output:'arrow',pen:false,tries:1,buy:true,proveStation:true,claimHabitat:{at:{x:1,y:65,z:2},width:5,depth:5,height:4,gates:[]}}}
}
test('explicit station confirmation observes exact unemployed UUID adoption before locking, and never breaks a traded station',async()=>{
 const f=rolling(),result=await roll.run(f.api,f.args)
 assert.equal(result.locked,true);assert.equal(result.workstationClaim.basis,WORKSTATION_CLAIM_BASIS)
 const dig=f.calls.findIndex(c=>c.action==='dig'),place=f.calls.findIndex(c=>c.action==='place'),purchase=f.calls.findIndex(c=>c.action==='trade')
 assert.equal(dig,-1);assert.ok(place>=0&&purchase>place)
 const unknown=rolling(0,true);await assert.rejects(roll.run(unknown.api,unknown.args),/already-employed/);assert.ok(!unknown.calls.some(c=>['dig','place','trade'].includes(c.action)))
 const locked=rolling(1);await assert.rejects(roll.run(locked.api,locked.args),/locked trader/);assert.ok(!locked.calls.some(c=>['dig','place','trade'].includes(c.action)))
})
test('saved physical source permits only declared additive stations in authored air',async()=>{
 const {compatibleVillageIntent}=await import('../src/villager/village-plan.mjs')
 const before=JSON.parse(fs.readFileSync(new URL('../blueprints/villager-house-10.blueprint.json',import.meta.url))),after=structuredClone(before);after.population=intent
 after.structure.legend.Q={type:'block',block:'fletching_table'};const layer=after.structure.layers.find(l=>l.y===0),row=[...layer.rows[3]];row[3]='Q';layer.rows[3]=row.join('')
 assert.equal(compatibleVillageIntent(before,after),true)
 const changed=structuredClone(after);changed.materials.shell.preferences=['birch_planks'];assert.equal(compatibleVillageIntent(before,changed),false)
 const broken=structuredClone(after);broken.structure.layers.find(l=>l.y===3).rows[3]='..........';assert.equal(compatibleVillageIntent(before,broken),false)
 const wall=structuredClone(before);wall.population={target:1,roles:[],workspaces:[{id:'shop',at:[0,0,3],profession:'fletcher'}]};wall.structure.legend.Q={type:'block',block:'fletching_table'};const r=[...wall.structure.layers.find(l=>l.y===0).rows[3]];r[0]='Q';wall.structure.layers.find(l=>l.y===0).rows[3]=r.join('');assert.equal(compatibleVillageIntent(before,wall),false)
})

test('station confirmation refuses open-world or competing POI inference before any workstation mutation',async()=>{
 const unbounded=rolling();delete unbounded.args.claimHabitat;await assert.rejects(roll.run(unbounded.api,unbounded.args),/bounded verified/)
 assert.ok(!unbounded.calls.some(c=>['dig','place','trade'].includes(c.action)))
 const open=rolling(),original=open.api.block;open.api.block=(x,y,z)=>x===1&&y===65&&z===3?{name:'air'}:original(x,y,z)
 await assert.rejects(roll.run(open.api,open.args),/not closed/);assert.ok(!open.calls.some(c=>['dig','place','trade'].includes(c.action)))
 const competing=rolling(),before=competing.api.block;competing.api.block=(x,y,z)=>x===4&&y===65&&z===4?{name:'fletching_table'}:before(x,y,z)
 await assert.rejects(roll.run(competing.api,competing.args),/exactly the requested/);assert.ok(!competing.calls.some(c=>['dig','place','trade'].includes(c.action)))
})
