import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { compileBlueprintStructure } from '../src/blueprint/compiler.mjs'
import { compositeError } from '../src/lib.mjs'
import build from '../library/blueprint/build.mjs'
import check from '../library/blueprint/check.mjs'
import show from '../library/blueprint/show.mjs'
const cottage=JSON.parse(fs.readFileSync(new URL('../blueprints/island-cottage.blueprint.json',import.meta.url)))
test('blueprint facades accept structured plan inputs through the actual runtime loader',()=>{
 for(const [name,mod] of Object.entries({build,check,show}))assert.equal(compositeError(`blueprint.${name}`,mod),null)
})
test('island cottage has two atomic bed footprints under its protective ceiling',()=>{
 const ir=compileBlueprintStructure(cottage),beds=ir.objects.filter(o=>o.type==='bed')
 assert.equal(beds.length,2)
 for(const bed of beds){assert.equal(bed.footprint.length,2);assert.equal(bed.states.facing,'south')}
 const ceiling=cottage.structure.layers.find(l=>l.y===3)
 assert.ok(ceiling.rows.every(r=>r==='PPPPPPP'))
 assert.equal(ir.objects.filter(o=>o.type==='door').length,1)
 assert.equal(ir.objects.filter(o=>o.block==='torch').length,2)
 assert.equal(cottage.materials.shell.kind,'full_cube');assert.equal(cottage.materials.shell.requires.contactHazard,false)
})
