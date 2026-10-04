import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { compileBlueprintStructure, concreteBlueprint } from '../src/blueprint/compiler.mjs'
import { representativeBlueprint } from '../src/blueprint/palette.mjs'
import { jobsFor, orderJobs, scaffoldMatches } from '../src/blueprint/format.mjs'
import { compositeError } from '../src/lib.mjs'
import build from '../library/blueprint/build.mjs'
import check from '../library/blueprint/check.mjs'
import show from '../library/blueprint/show.mjs'
const cottage=JSON.parse(fs.readFileSync(new URL('../../blueprints/island-cottage.blueprint.json',import.meta.url)))
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

test("blueprint.show returns the Promise required by the composite runner", async()=>{
 const result=show.run({}, {name:"island-cottage"})
 assert.equal(typeof result.then,"function")
 assert.ok(await result)
})

test('cottage planner reuses an intact grounded scaffold column on resume',()=>{
 const bp=representativeBlueprint(cottage),at={x:0,y:64,z:0}
 const flat=(x,y,z)=>({name:y>=64?'air':y===63?'grass_block':'dirt',boundingBox:y>=64?'empty':'block',properties:{}})
 const initial=orderJobs(jobsFor(bp,at,flat),bp,at,flat)
 assert.equal(initial.unreachable.length,0)
 const earlierPillars=new Map();let repeated=0
 for(const job of initial.jobs){const stand=job.stand;if(!stand)continue;const key=`${stand.x},${stand.z}`;if(earlierPillars.get(key)===stand.y){assert.ok(stand.scaffoldCells?.length,`later work at ${key} must retain its earlier pillar dependency`);repeated++}if(stand.scaffoldCells?.length)earlierPillars.set(key,stand.y)}
 assert.ok(repeated>0)
 const column=initial.jobs.filter(j=>j.stand?.scaffoldCells).sort((a,b)=>b.stand.scaffold-a.stand.scaffold)[0].stand.scaffoldCells
 assert.ok(column.length>=3)
 const existing=new Set(column.map(c=>`${c.x},${c.y},${c.z}`))
 const resumed=(x,y,z)=>existing.has(`${x},${y},${z}`)?{name:'dirt',boundingBox:'block',properties:{}}:flat(x,y,z)
 assert.equal(orderJobs(jobsFor(bp,at,resumed),bp,at,resumed).unreachable.length,0)
 assert.equal(scaffoldMatches('grass_block','dirt'),true)
 const grown=(x,y,z)=>existing.has(`${x},${y},${z}`)?{name:y===column.at(-1).y?'grass_block':'dirt',boundingBox:'block',properties:{}}:flat(x,y,z)
 assert.equal(orderJobs(jobsFor(bp,at,grown),bp,at,grown).unreachable.length,0)
 const broken=(x,y,z)=>existing.has(`${x},${y},${z}`)&&y!==column[1].y?{name:'dirt',boundingBox:'block',properties:{}}:flat(x,y,z)
 assert.ok(orderJobs(jobsFor(bp,at,broken),bp,at,broken).jobs.every(j=>!j.stand?.scaffoldCells?.some(c=>c.x===column[0].x&&c.z===column[0].z)))
})

test('a reused grass foundation remains satisfied after a covered cell naturally becomes dirt',()=>{
 const ir=compileBlueprintStructure(cottage)
 const chosen={shell:'cobblestone',wood_log:'oak_log',wood_planks:'oak_planks',wood_stairs:'oak_stairs',wood_door:'oak_door',sleeping:'white_bed'}
 const assignments=Object.fromEntries(ir.objects.map(o=>[o.id,o.block??(o.material==='shell'&&o.footprint[0].at[1]===-1?'grass_block':chosen[o.material])]))
 const bp=concreteBlueprint(ir,assignments)
 const floor=bp.layers.find(l=>l.y===-1)
 assert.deepEqual(bp.legend[floor.grid[0][0]].alts.map(a=>a.name),['grass_block','dirt'])
 const at={x:0,y:64,z:0}
 const world=(x,y,z)=>({name:y===63?'dirt':y<63?'grass_block':'air',boundingBox:y<64?'block':'empty',properties:{}})
 assert.ok(jobsFor(bp,at,world).every(j=>j.y!==63))
})
