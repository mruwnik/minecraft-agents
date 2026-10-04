import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { compileBlueprintStructure } from '../src/blueprint/compiler.mjs'
import { representativeAssignments, representativeBlueprint } from '../src/blueprint/palette.mjs'
import { rotateBlueprintPosition } from '../src/blueprint/transform.mjs'
import { blueprintOperationalCells, verifyBlueprintGuarantees } from '../src/blueprint/verify.mjs'
import { showBlueprintV2 } from '../src/blueprint/v2.mjs'
import { blueprintDetail } from '../tools/dashboard/lib.mjs'
import { bill } from '../src/blueprint/format.mjs'
const document = {schemaVersion:2,id:'shared-sample',dimensions:[3,3,5],front:'south',materials:{wall:{kind:'full_cube',candidates:['birch_planks','oak_planks']}},structure:{legend:{},layers:[],objects:[{id:'wall',type:'block',material:'wall',at:[1,0,2]},{id:'torch',type:'block',block:'torch',at:[0,0,1]}],spaces:[]}}
test('representative palette preserves exact pins and agrees across show and dashboard without stock allocation',()=>{
  const ir=compileBlueprintStructure(document), assignments=representativeAssignments(ir)
  assert.deepEqual(assignments,{wall:'birch_planks',torch:'torch'})
  const bp=representativeBlueprint(document),shown=showBlueprintV2({}, {plan:document}),dashboard=blueprintDetail({name:document.id,document,hash:ir.hash})
  assert.deepEqual(shown.bill,bill(bp).total)
  assert.deepEqual(dashboard.bill.total,shown.bill)
  assert.equal(dashboard.palette,'representative')
  assert.equal(dashboard.allocation,null)
  assert.deepEqual(dashboard.preview.filter(c=>c.name!=='air').map(c=>c.name).sort(),['birch_planks','torch'])
})
test('one rectangular transform covers every facing and negative world anchors',()=>{
  const expected=[[2,1,4],[0,1,2],[0,1,0],[4,1,0]],anchor={x:-20,y:64,z:-30}
  for(let turns=0;turns<4;turns++){
    const rotated=rotateBlueprintPosition([2,1,4],3,5,turns)
    assert.deepEqual(rotated,expected[turns])
    const source={...document,guarantees:['source_water'],structure:{legend:{},layers:[],objects:[{id:'gate',type:'fence_gate',block:'oak_fence_gate',at:[2,1,4],constructionState:{facing:'east'},initialState:{open:false}},{id:'water',type:'water',block:'water',at:[2,0,3]}],spaces:[]}}
    const manifest={source,at:anchor,facing:['south','west','north','east'][turns],allocation:{assignments:{gate:'oak_fence_gate',water:'water'}}}
    const gate=blueprintOperationalCells(manifest)[0]
    assert.deepEqual([gate.x,gate.y,gate.z],rotated.map((v,i)=>v+[anchor.x,anchor.y,anchor.z][i]))
    const water=rotateBlueprintPosition([2,0,3],3,5,turns).map((v,i)=>v+[anchor.x,anchor.y,anchor.z][i])
    verifyBlueprintGuarantees({block:(x,y,z)=>{assert.deepEqual([x,y,z],water);return{name:'water',properties:{level:0}}}},manifest)
  }
})
test('public catalog sources remain canonical v2 assets',()=>{
  for(const name of ['starter-hut','watchtower','villager-house-10','wheat-field']){
    const doc=JSON.parse(fs.readFileSync(new URL(`../../blueprints/${name}.blueprint.json`,import.meta.url),'utf8'))
    const palette=representativeAssignments(compileBlueprintStructure(doc))
    assert.ok(Object.values(palette).every(name=>typeof name==='string'&&name.length>0))
  }
})
