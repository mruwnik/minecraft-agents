import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { compileBlueprintStructure } from '../src/blueprint/compiler.mjs'
import { representativeAssignments } from '../src/blueprint/palette.mjs'
import { writeBlueprintManifest,manifestId } from '../src/blueprint/manifest.mjs'
import { villagePlan } from '../src/villager/village-plan.mjs'
import { saveVillageInspection,readVillageInspection,listVillageInspections } from '../src/villager/inspection.mjs'
import { blueprintFileArguments } from '../src/blueprint/source.mjs'
test('population intent attaches to an unchanged saved physical blueprint and resumes without changing its allocation',()=>{
 const dir=fs.mkdtempSync(path.join(os.tmpdir(),'village-intent-'))
 try{
 const base=JSON.parse(fs.readFileSync(new URL('../blueprints/villager-house-10.blueprint.json',import.meta.url))),ir=compileBlueprintStructure(base),at={x:-82,y:69,z:-46},place='test-village'
 const manifest={id:manifestId(place,at),place,at,source:base,sourceHash:ir.hash,allocation:{assignments:representativeAssignments(ir)},facing:'south'}
 const inspectionDir=path.join(dir,'inspections')
 const note=writeBlueprintManifest(manifest,dir),file=path.join(dir,`${manifest.id}.json`),before=fs.readFileSync(file,'utf8')
 const api={places:()=>[{name:place,by:'Tester',...at,note}],me:()=> 'Tester'}
 const source={...base,population:{target:5,roles:[{id:'farmers',profession:'farmer',count:2}]}}
 const plan=villagePlan(api,{place,plan:source},{stateDir:dir,inspectionDir})
 saveVillageInspection(plan,{population:3,status:'unknown'},inspectionDir)
 const resumed=villagePlan(api,{place},{stateDir:dir,inspectionDir})
 assert.equal(resumed.intent.target,5);assert.equal(resumed.at.x,-82);assert.equal(readVillageInspection(place,at,inspectionDir).sourceHash,plan.ir.hash)
 assert.equal(listVillageInspections(inspectionDir).filter(r=>r.place===place).length,1)
 assert.equal(fs.readFileSync(file,'utf8'),before)
 const changed=structuredClone(source);changed.materials.shell.preferences=['birch_planks']
 assert.throws(()=>villagePlan(api,{place,plan:changed},{stateDir:dir,inspectionDir}),/physical source differs/)
 }finally{fs.rmSync(dir,{recursive:true,force:true})}
})
test('village file sources use the canonical caller-side blueprint resolver',()=>{
 const file=new URL('../examples/village-five.blueprint.json',import.meta.url).pathname
 const a=blueprintFileArguments('village.check',{file})
 assert.equal(a.plan.population.target,5);assert.equal(a.file,undefined);assert.equal(compileBlueprintStructure(a.plan).document.id,'village-five')
 assert.throws(()=>blueprintFileArguments('village.maintain',{file,name:'other'}),/exactly one/)
})
