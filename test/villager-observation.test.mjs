import { test } from 'node:test'
import assert from 'node:assert/strict'
import { villagerObservation } from '../src/villager/observation.mjs'
import { boatPassengerProfile } from '../src/boat/passenger.mjs'
import { verifyCrowdedClaimants } from '../src/villager/claim.mjs'
const uuid = '12345678-1234-4234-8234-123456789abc'
const rivalUuid = '12345678-1234-4234-8234-123456789abd'
const cell = { x:0,y:64,z:0 }
const row = (id,uuid,profession=0,baby=false) => ({id,uuid,name:'villager',exact:'1,64,1',baby,metadata:JSON.stringify({16:baby,19:{villagerProfession:profession,level:1}})})
test('one decoder handles wire and serialized profession/age/level without granting unknown adulthood',()=>{
  const a=villagerObservation({...row(1,uuid,9),metadata:{18:{profession:'minecraft:librarian',level:2},16:true,6:2},baby:undefined})
  assert.equal(a.profession,'librarian');assert.equal(a.level,2);assert.equal(a.baby,true);assert.equal(a.sleeping,true)
  assert.equal(villagerObservation(row(1,uuid,11)).nitwit,true)
  const unknown=villagerObservation({uuid,metadata:'not-json'})
  assert.equal(unknown.profession,'unknown');assert.equal(unknown.baby,undefined);assert.equal(unknown.uuid,uuid)
  assert.equal(villagerObservation({uuid:'invalid'}).uuid,undefined)
  assert.equal(boatPassengerProfile({name:'villager',uuid,width:.6,height:1.95}).ok,false)
  assert.equal(boatPassengerProfile({...row(1,uuid),width:.6,height:1.95}).ok,true)
})
function harness({profession=5,locked=true,baby=false,change=false,station=false}={}){
  let scans=0; const calls=[]
  const api={act:async(name,args)=>{calls.push([name,args]); if(name==='entity')return {found:[row(1,uuid),row(20+scans++,rivalUuid,change&&scans>2?0:profession,baby)]};if(name==='trades')return {level:1,offers:[{nbTradeUses:locked?1:0}]};if(name==='find_blocks')return {positions:station?[{x:2,y:64,z:0}]:[]};throw Error(name)}}
  return {api,calls}
}
test('crowded claim checks refreshed exact rival identity and proven lock, never proximity alone',async()=>{
  const h=harness();await verifyCrowdedClaimants(h.api,{uuid,cell,block:'lectern'})
  assert.equal(h.calls.find(([n])=>n==='trades')[1].uuid,rivalUuid)
  for(const options of [{profession:0},{locked:false},{change:true},{station:true}]){
    const h=harness(options);await assert.rejects(verifyCrowdedClaimants(h.api,{uuid,cell,block:'lectern'}),/claim|lock|workstation/)
    assert.ok(h.calls.every(([n])=>!['dig','place','trade'].includes(n)))
  }
})
test('babies and nitwits cannot compete but unknown rival age fails closed',async()=>{
  for(const options of [{baby:true},{profession:11}]){const h=harness(options);await verifyCrowdedClaimants(h.api,{uuid,cell,block:'lectern'});assert.equal(h.calls.some(([n])=>n==='trades'),false)}
  const h=harness({baby:undefined});const act=h.api.act;h.api.act=async(n,a)=>{const r=await act(n,a);if(n==='entity'){delete r.found[1].baby;r.found[1].metadata='{}'}return r}
  await assert.rejects(verifyCrowdedClaimants(h.api,{uuid,cell,block:'lectern'}),/unknown UUID\/age/)
})
