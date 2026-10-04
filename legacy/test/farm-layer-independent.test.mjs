// Independent acceptance of the canonical layered maintenance format and one-way import.
import test from 'node:test'
import assert from 'node:assert/strict'
import {parsePlan,parseStructurePlan,parsePlacePlan,planCells,planSpec,planErrors,planBill,migratePlan} from '../src/lib/plan.mjs'
import {farmJobs} from '../src/lib/jobs.mjs'
import {planChests} from '../src/lib/storage.mjs'
import {clutterBlocks} from '../library/farm/shared/clutter.mjs'
import {fieldCensus} from '../src/lib/anchor.mjs'
import {checkTree} from '../src/tree/inspect.mjs'
const at={x:10,y:63,z:20}
const sort=xs=>xs.sort((a,b)=>JSON.stringify(a).localeCompare(JSON.stringify(b)))
const meaning=c=>{const s=planSpec(c);return {x:c.x,y:c.y,z:c.z,kind:s.kind,crop:s.crop,seed:s.seed,item:s.item,cover:s.cover,ground:s.ground,generic:s.generic}}
function legacyCells(place){return parsePlan(place.plan,place.legend).cells.map(c=>({...c,x:place.x+c.dx,y:place.y+(planSpec(c).ground_offset??0),z:place.z+c.dz}))}
function imported(plan,legend){return migratePlan({name:'imported',by:'Tester',...at,plan,legend})}
const flat=(x,y,z)=>({name:y<=63?'dirt':'air',solid:y<=63,properties:{}})
test('layer independent: every legacy preset retains its world coordinate and maintenance meaning',()=>{
 const source={...at,plan:'*wcpbsmkB~.#GTCKFtA'}
 const saved=migratePlan(source)
 assert.equal(saved.plan,undefined);assert.equal(saved.legend,undefined)
 assert.deepEqual(sort(planCells(saved).map(meaning)),sort(legacyCells(source).map(meaning)))
 assert.deepEqual(planBill(parsePlacePlan(saved)),planBill(parsePlan(source.plan)))
 assert.deepEqual(migratePlan(JSON.parse(JSON.stringify(saved))),saved)
})
test('layer independent: migrating generic/specific crop, covered water, stems and multiblock torch retains generated jobs',()=>{
 const source={...at,plan:'*wmk~TCA',legend:{A:'barrel'}}
 const items={wheat_seeds:3,melon_seeds:1,pumpkin_seeds:1,water_bucket:1,oak_slab:1,oak_fence:1,torch:1,chest:1,barrel:1}
 const before=farmJobs({cells:legacyCells(source),worldAt:flat,items})
 const after=farmJobs({cells:planCells(migratePlan(source)),worldAt:flat,items})
 assert.deepEqual(sort(after),sort(before))
 assert.ok(after.some(j=>j.item==='torch'&&j.y===65))
 assert.ok(after.some(j=>j.do==='pour'&&j.y===62))
})
test('layer independent: signed tree offsets become layer elevations once and lose the legacy flag',()=>{
 const saved=imported('abc',{a:{kind:'tree',species:'oak',ground_offset:-3},b:{kind:'tree',species:'birch',ground_offset:5},c:{kind:'tree',species:'spruce'}})
 assert.deepEqual(planCells(saved).map(c=>[c.spec.species,c.y]).sort(),[['birch',68],['oak',60],['spruce',63]])
 assert.ok(Object.values(saved.structure.legend).every(s=>s.ground_offset===undefined))
 assert.deepEqual(planCells(JSON.parse(JSON.stringify(saved))),planCells(saved))
})
test('layer independent: reserved habitat remains claimed while unconstrained holes remain absent',()=>{
 const saved=imported('r r\n t ',{r:{kind:'reserved'}})
 const cells=planCells(saved)
 assert.equal(cells.filter(c=>c.spec.kind==='reserved').length,2)
 assert.equal(cells.length,3)
 assert.equal(farmJobs({cells:cells.filter(c=>c.spec.kind==='reserved'),worldAt:flat}).length,0)
})
const stacked={legend:{f:'farmland',w:'wheat',c:'carrots',a:'water',h:'chest'},layers:[
 {y:0,rows:['fa_']},{y:1,rows:['w_h']},{y:2,rows:['fa_']},{y:3,rows:['c_h']}
]}
test('layer independent: same x/z beds and chests retain distinct actual block heights',()=>{
 const p=parseStructurePlan(stacked)
 assert.equal(p.error,undefined)
 assert.deepEqual(planErrors(p),[])
 const cells=planCells({...at,structure:stacked})
 assert.deepEqual(cells.filter(c=>c.spec.kind==='crop').map(c=>[c.spec.crop,c.y]),[['wheat',63],['carrots',65]])
 assert.deepEqual(planChests(cells),[{x:12,y:64,z:20},{x:12,y:66,z:20}])
})
test('layer independent: water on another floor cannot hydrate a raised crop',()=>{
 const bad={legend:{w:'wheat',a:'water'},layers:[{y:0,rows:['_a']},{y:6,rows:['w_']}]}
 assert.ok(planErrors(parseStructurePlan(bad)).some(s=>/water/.test(s)))
 const good={...bad,layers:[{y:5,rows:['_a']},{y:6,rows:['w_']}]}
 assert.deepEqual(planErrors(parseStructurePlan(good)),[])
})
test('layer independent: stacked farmland and crops are not one another\'s clutter',()=>{
 const cells=planCells({...at,structure:stacked})
 const world=(x,y,z)=>({name:z!==20?'air':x===10?({63:'farmland',64:'wheat',65:'farmland',66:'carrots'}[y]??'air'):x===11&&[63,65].includes(y)?'water':x===12&&[64,66].includes(y)?'chest':'air',solid:false,properties:{age:7,level:0}})
 assert.deepEqual(clutterBlocks(cells,world),[])
 const census=fieldCensus(cells,world)
 assert.equal(census.crops.wheat,1);assert.equal(census.crops.carrots,1)
 assert.equal(farmJobs({cells,worldAt:world}).filter(j=>j.do==='plant').length,0)
})
test('layer independent: explicit air is actual block geometry while underscore is outside the plan',()=>{
 const structure={legend:{s:'stone'},layers:[{y:0,rows:['s._']}]}
 const cells=planCells({...at,structure})
 assert.equal(cells.length,2)
 const air=cells.find(c=>c.spec.kind==='air')
 assert.deepEqual([air.x,air.y+1,air.z],[11,63,20])
})
test('layer independent: independently elevated trees do not overlap merely because x/z match',()=>{
 const structure={legend:{o:{kind:'tree',species:'oak'}},layers:[{y:1,rows:['o']},{y:31,rows:['o']}]}
 const cells=planCells({x:0,y:0,z:0,structure})
 const world=(x,y,z)=>({name:(y===0||y===30)?'dirt':'air',solid:y===0||y===30,properties:{}})
 const r=checkTree(world,{x:0,y:0,z:0},'oak','single',cells)
 assert.deepEqual(r.attention,[])
})
test('layer independent: ambiguous overlap, material placeholders and offset-on-layer are rejected',()=>{
 for(const structure of [
  {legend:{s:'stone'},layers:[{y:0,rows:['s']},{y:0,rows:['s']}]},
  {legend:{s:{material:'wall'}},layers:[{y:0,rows:['s']}]},
  {legend:{s:{kind:'tree',species:'oak',ground_offset:2}},layers:[{y:1,rows:['s']}]}
 ])assert.ok(parseStructurePlan(structure).error)
})
test('layer independent: runtime refuses legacy records until explicitly migrated',()=>{
 assert.throws(()=>planCells({...at,plan:'w~'}),/legacy|migrat|structure/)
 assert.ok(planCells(imported('w~')).length)
})

import farmPlan from '../library/farm/plan.mjs'
import maintainFarm from '../library/farm/maintain.mjs'
import maintainForest from '../library/forestry/maintain.mjs'
function commandWorld({floors=[0],items={}}={}) {
 const world=new Map(),places=[],calls=[],events=[];let pos={x:2.5,y:1,z:0.5}
 const api={
  block:(x,y,z)=>{const name=world.get(`${x},${y},${z}`)??(floors.includes(y)?'dirt':'air');return{name,solid:!['air','water','wheat','carrots','oak_sapling','birch_sapling'].includes(name),properties:{age:0,level:0}}},
  places:()=>places,me:()=> 'Tester',pos:()=>pos,inv:()=>items,freeSlots:()=>36,drops:()=>[],
  plan:name=>{const p=places.find(p=>p.name===name);return{...p,cells:planCells(p),parsed:parsePlacePlan(p)}},
  checkpoint:async()=>{},clock:()=>({elapsedDays:0,day:true,night:false}),until:async()=>true,pause:async()=>{},note:()=>{},report:()=>{},emit:(type,data)=>events.push({type,...data}),
  async act(name,args={}) {
   calls.push({name,args})
   if(name==='mark'){places.push(JSON.parse(JSON.stringify({...args,by:'Tester'})));return{}}
   if(name==='zones')return{zones:[]}
   if(name==='goto')pos={x:args.x+.5,y:args.y,z:args.z+.5}
   if(name==='till')world.set(`${args.x},${args.y},${args.z}`,'farmland')
   if(name==='dig')world.set(`${args.x},${args.y},${args.z}`,'air')
   if(name==='place'){assert.ok(items[args.item]>0,`stock:${args.item}`);items[args.item]--;world.set(`${args.x},${args.y},${args.z}`,({wheat_seeds:'wheat',carrot:'carrots'})[args.item]??args.item)}
   return{}
  }
 }
 return{api,world,places,calls,events}
}
test('layer independent: register, reload and maintain two crop floors without aliasing their planting coordinates',async()=>{
 const f=commandWorld({floors:[0,4],items:{stone_hoe:1,wheat_seeds:1,carrot:1}})
 for(const y of [0,4]){f.world.set(`0,${y},0`,'farmland');f.world.set(`1,${y},0`,'water')}
 const structure={legend:{f:'farmland',w:'wheat',c:'carrots',a:'water'},layers:[{y:0,rows:['fa']},{y:1,rows:['w_']},{y:4,rows:['fa']},{y:5,rows:['c_']}]}
 await farmPlan.run(f.api,{name:'stacked',x:0,y:0,z:0,structure})
 assert.equal(f.places[0].plan,undefined)
 assert.deepEqual(f.places[0].structure,structure)
 const result=await maintainFarm.run(f.api,{place:'stacked',deposit:false,compost:false})
 assert.equal(f.world.get('0,1,0'),'wheat')
 assert.equal(f.world.get('0,5,0'),'carrots')
 assert.equal(result.replanted,2)
 assert.equal(f.world.get('0,4,0'),'farmland')
 assert.ok(!f.calls.some(c=>c.name==='dig'&&c.args.y===4),'upper floor retained')
})
test('layer independent: canonical forest registration maintains different actual root elevations after JSON reload',async()=>{
 const f=commandWorld({floors:[0,3],items:{oak_sapling:1,birch_sapling:1}})
 // Model the raised terrain only at the distant birch site, not as a low roof over oak.
 const read=f.api.block
 f.api.block=(x,y,z)=>y===3&&x<15?{name:'air',solid:false,properties:{}}:read(x,y,z)
 const rowsA='o'+'_'.repeat(19),rowsB='_'.repeat(19)+'b'
 const structure={legend:{o:{kind:'tree',species:'oak'},b:{kind:'tree',species:'birch'}},layers:[{y:1,rows:[rowsA]},{y:4,rows:[rowsB]}]}
 await farmPlan.run(f.api,{name:'terraces',x:0,y:0,z:0,structure})
 f.places[0]=JSON.parse(JSON.stringify(f.places[0]))
 const result=await maintainForest.run(f.api,{place:'terraces',deposit:false})
 assert.deepEqual(result.attention,[])
 assert.equal(f.world.get('0,1,0'),'oak_sapling')
 assert.equal(f.world.get('19,4,0'),'birch_sapling')
 assert.equal(result.planted,2)
})
