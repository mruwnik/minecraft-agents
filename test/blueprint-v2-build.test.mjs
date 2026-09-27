import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { buildBlueprintV2, checkBlueprintV2 } from '../src/blueprint/v2.mjs'
import { readBlueprintManifest } from '../src/blueprint/manifest.mjs'
import { flatGround } from '../src/blueprint/format.mjs'
const doc = () => ({ schemaVersion: 2, id: 'resume-room', dimensions: [4, 2, 3], front: 'south', tags: ['workshop'], materials: { wall: { kind: 'full_cube', candidates: ['oak_planks','birch_planks'], preferences: ['oak_planks'] } }, structure: { objects: [
  { id: 'wall', at: [0,0,0], material: 'wall' },
  { id: 'bed', at: [1,0,1], type: 'bed', block: 'white_bed', constructionState: { facing: 'east' } },
  { id: 'gate', at: [3,0,1], type: 'fence_gate', block: 'oak_fence_gate', constructionState: { facing: 'east' }, initialState: { open: false } }
] }, site: { foundation: 'flat', removal: 'natural_only' } })
const body = () => {
  const world = new Map(), places = [], calls = [], stock = { oak_planks: 1, birch_planks: 10, white_bed: 1, oak_fence_gate: 1, dirt: 20 }
  let position = { x: 0.5, y: 65, z: 0.5 }, stop = false
  const key = a => `${a.x},${a.y},${a.z}`
  const api = {
    inv: () => stock, places: () => places, zones: () => [], me: () => 'Tester', freeSlots: () => 30, clock: () => ({night:false}), pos: () => position, report: () => {},
    block: (x,y,z) => world.get(`${x},${y},${z}`) ?? flatGround(64)(x,y,z),
    checkpoint: async () => { if (stop) { stop=false; throw new Error('intentional interruption') } },
    act: async (action, a) => {
      calls.push({action,a})
      if (action==='goto') position={x:a.x+.5,y:a.y,z:a.z+.5}
      if (action==='mark') { const row={...a,by:'Tester'};const i=places.findIndex(p=>p.name===a.name);if(i<0)places.push(row);else places[i]=row }
      if (action==='place') {
        assert.ok(stock[a.item]>0, 'placement cannot spend unavailable stock');stock[a.item]--
        const states={...(a.facing?{facing:a.facing}:{}),...(/_bed$/.test(a.item)?{part:'foot'}:{}),...(/_fence_gate$/.test(a.item)?{open:true}:{})}
        world.set(key(a),{name:a.item,solid:true,properties:states})
        if (/_bed$/.test(a.item)) { assert.equal(a.facing,'east');world.set(`${a.x+1},${a.y},${a.z}`,{name:a.item,solid:true,properties:{...states,part:'head'}}) }
      }
      if (action==='toggle') world.get(key(a)).properties.open=a.open
      return {}
    }
  }
  return {api,world,stock,places,calls,interrupt:()=>{stop=true}}
}
test('v2 build resumes its frozen source and allocation after source deletion and stock changes', async () => {
  const stateDir=fs.mkdtempSync(path.join(os.tmpdir(),'v2-build-')), b=body(), plan=doc()
  b.interrupt()
  await assert.rejects(buildBlueprintV2(b.api,{plan,place:'room',x:0,y:65,z:0},{stateDir}),/intentional interruption/)
  const manifest=readBlueprintManifest(b.places[0].note,stateDir)
  assert.equal(manifest.allocation.assignments.wall,'oak_planks')
  assert.equal(manifest.source.id,'resume-room')
  b.stock.oak_planks=0
  const done=await buildBlueprintV2(b.api,{place:'room'},{stateDir})
  assert.equal(done.initialStateVerified,true)
  assert.equal(b.world.get('0,65,0').name,'oak_planks')
  assert.equal(b.stock.birch_planks,10,'resume must not swap frozen palette')
  assert.equal(b.world.get('3,65,1').properties.open,false)
  assert.ok(b.places[0].note.length<=80)
  await assert.rejects(buildBlueprintV2(b.api,{place:'room',plan:{...plan,structure:{objects:[plan.structure.objects[0]]}}},{stateDir}),/source differs/)
  fs.rmSync(stateDir,{recursive:true,force:true})
})
test('v2 check is read-only and shortages or partial multipart objects fail before action', async () => {
  const b=body(), plan=doc()
  await checkBlueprintV2(b.api,{plan,x:0,y:65,z:0})
  assert.ok(b.calls.every(c=>c.action==='state'))
  b.stock.white_bed=0
  await assert.rejects(buildBlueprintV2(b.api,{plan,place:'room',x:0,y:65,z:0}),/not funded/)
  assert.ok(b.calls.every(c=>c.action==='state'))
  b.stock.white_bed=1
  b.world.set('1,65,1',{name:'white_bed',solid:true,properties:{facing:'east',part:'foot'}})
  await assert.rejects(checkBlueprintV2(b.api,{plan,x:0,y:65,z:0}),/partial or mismatched multipart/)
  assert.ok(b.calls.every(c=>c.action==='state'))
})

test('v2 reuses opposite gate-axis facing and refuses dimension changes and built clearing', async () => {
  const b=body(), plan=doc(), stateDir=fs.mkdtempSync(path.join(os.tmpdir(),'v2-policy-'))
  b.world.set('3,65,1',{name:'oak_fence_gate',solid:false,properties:{facing:'west',open:false}})
  b.stock.oak_fence_gate=0
  const checked=await checkBlueprintV2(b.api,{plan,x:0,y:65,z:0})
  assert.equal(checked.allocation.reused.gate,'oak_fence_gate')
  b.world.set('0,65,0',{name:'stone_bricks',solid:true})
  await assert.rejects(checkBlueprintV2(b.api,{plan,x:0,y:65,z:0,clear:true}),/natural_only/)
  b.world.delete('0,65,0')
  await buildBlueprintV2(b.api,{plan,place:'room',x:0,y:65,z:0},{stateDir})
  const act=b.api.act
  b.api.act=async (action,a)=>action==='state'?{dimension:'the_nether'}:act(action,a)
  await assert.rejects(buildBlueprintV2(b.api,{place:'room'},{stateDir}),/different world dimension/)
  fs.rmSync(stateDir,{recursive:true,force:true})
})
