// Independent acceptance tests: exercise public commands against a mutable world.
import test from 'node:test'
import assert from 'node:assert/strict'
import plant from '../library/tree/plant.mjs'
import harvest from '../library/tree/harvest.mjs'
import prepare from '../library/tree/prepare.mjs'
import forestry from '../library/forestry/maintain.mjs'
import farmPlan from '../library/farm/plan.mjs'
import { parsePlan, planCells as resolvedPlanCells, planBill, planErrors, resolveLegend, migratePlan, parsePlacePlan } from '../src/lib/plan.mjs'
import { planChests } from '../src/lib/storage.mjs'
import { clutterBlocks } from '../library/farm/shared/clutter.mjs'
const coord = p => `${p.x},${p.y},${p.z}`
const root = { x: 0, y: 0, z: 0 }
// Legacy-shaped fixtures enter through the explicit importer; runtime sees canonical layers.
const planCells = place => resolvedPlanCells(migratePlan(place))
function worldFixture ({ soil = 'dirt', items = {}, plan, fail, checkpoint } = {}) {
  const world = new Map(), calls = [], events = []
  let pos = { x: 2.5, y: 1, z: 2.5 }
  const places = plan ? [migratePlan(plan)] : []
  const api = {
    block: (x,y,z) => ({ name: world.get(`${x},${y},${z}`) ?? (y <= 0 ? soil : 'air'), solid: !['air','oak_sapling','dandelion'].includes(world.get(`${x},${y},${z}`) ?? (y <= 0 ? soil : 'air')), properties: {} }),
    act: async (name, args = {}) => {
      calls.push({ name, args })
      if (fail) await fail(name,args)
      if (name === 'zones') return { zones: [] }
      if (name === 'goto') pos = { x:args.x+0.5, y:args.y, z:args.z+0.5 }
      if (name === 'place') { assert.ok(items[args.item] > 0, `fixture stock ${args.item}`); items[args.item]--; world.set(coord(args), args.item) }
      if (name === 'dig') world.set(coord(args),'air')
      if (name === 'mark') places.push(migratePlan({ ...args, ...(args.structure ? {} : { plan: args.map, legend: resolveLegend(args.legend) }), by:'Tester' }))
      return {}
    },
    pos: () => pos, me: () => 'Tester', inv: () => items, places: () => places,
    plan: name => { const p=places.find(p=>p.name===name); return {...p,cells:planCells(p),parsed:parsePlacePlan(p)} },
    checkpoint: async () => { if(checkpoint) await checkpoint() }, emit: (type,data) => events.push({type,...data}), report:()=>{}, note:()=>{},
    clock:()=>({elapsedDays:0,day:true,night:false}), until:async()=>true, drops:()=>[], freeSlots:()=>27
  }
  return {api,world,calls,events,items,places}
}
const forms = [
 ['oak','single','oak_sapling'], ['birch','single','birch_sapling'], ['spruce','single','spruce_sapling'], ['spruce','large','spruce_sapling'],
 ['jungle','single','jungle_sapling'], ['jungle','large','jungle_sapling'], ['acacia','single','acacia_sapling'], ['dark_oak','large','dark_oak_sapling'],
 ['pale_oak','large','pale_oak_sapling'], ['cherry','single','cherry_sapling'], ['mangrove','single','mangrove_propagule'], ['azalea','single','azalea'],
 ['crimson','single','crimson_fungus','crimson_nylium'], ['warped','single','warped_fungus','warped_nylium']
]
for(const [species,form,item,soil='dirt'] of forms) test(`independent: ${species}/${form} plan survives serialization and plants complete footprint`, async()=>{
 const p={name:'trees',by:'Tester',kind:'farm',...root,plan:'x',legend:{x:{kind:'tree',species,form}}}
 const reloaded=JSON.parse(JSON.stringify(p)), parsed=parsePlan(reloaded.plan,reloaded.legend)
 assert.deepEqual(planErrors(parsed),[])
 assert.equal(planBill(parsed)[item],form==='large'?4:1)
 const f=worldFixture({soil,plan:reloaded,items:{[item]:4}})
 const r=await plant.run(f.api,{...root,species,form,place:'trees'})
 assert.equal(r.planted,form==='large'?4:1)
 assert.deepEqual(r.attention,[])
 assert.equal(f.calls.filter(c=>c.name==='place').length,form==='large'?4:1)
})
test('independent: plan command persists custom legend and default symbol overrides through reload',async()=>{
 const f=worldFixture()
 await farmPlan.run(f.api,{name:'aliases',...root,map:'Cq~',legend:{C:'stone',q:'carrots'}})
 const saved=JSON.parse(JSON.stringify(f.places[0]))
 assert.equal(planCells(saved).find(c=>c.x===1&&c.spec.kind==='crop').spec.crop,'carrots')
 assert.deepEqual(planChests(planCells(saved)),[])
 assert.equal(planBill(parsePlacePlan(saved)).carrot,1)
})
test('independent: farm clutter leaves planned mature tree and overlapping canopy intact',()=>{
 const p={...root,plan:'x.',legend:{x:{kind:'tree',species:'oak'}}}, f=worldFixture()
 f.world.set('0,1,0','oak_log');f.world.set('0,2,0','oak_log');f.world.set('1,2,0','oak_leaves')
 assert.deepEqual(clutterBlocks(planCells(p),f.api.block),[])
})
test('independent: flower placement is verified before any sapling',async()=>{
 const f=worldFixture({items:{dandelion:1,oak_sapling:1}})
 await plant.run(f.api,{...root,species:'oak',flower:'dandelion'})
 assert.deepEqual(f.calls.filter(c=>c.name==='place').map(c=>c.args.item),['dandelion','oak_sapling'])
})
test('independent: complete 2x2 stock is required before first sapling, and shortage is attention',async()=>{
 const f=worldFixture({items:{dark_oak_sapling:3}})
 const r=await plant.run(f.api,{...root,species:'dark_oak',form:'large'})
 assert.ok(r.attention.some(s=>/missing/.test(s)))
 assert.equal(f.calls.filter(c=>c.name==='place').length,0)
 assert.ok(f.events.some(e=>e.type==='forestry_attention'))
})
function smallOak(f,height=3){for(let y=1;y<=height;y++)f.world.set(`0,${y},0`,'oak_log');f.world.set(`0,${height+1},0`,'oak_leaves')}
test('independent: forestry lifecycle harvests whole tree, collects then replants original plan',async()=>{
 const p={name:'trees',by:'Tester',kind:'farm',...root,plan:'x',legend:{x:{kind:'tree',species:'oak'}}}
 const f=worldFixture({plan:p,items:{oak_sapling:1}});smallOak(f)
 const r=await forestry.run(f.api,{place:'trees',deposit:false})
 assert.deepEqual(r.attention,[])
 assert.equal(r.planted,1)
 assert.equal(f.world.get('0,1,0'),'oak_sapling')
 assert.equal(f.world.get('0,4,0'),'air')
 const names=f.calls.map(c=>c.name)
 assert.ok(names.indexOf('collect')>names.lastIndexOf('dig'))
 assert.ok(names.indexOf('place')>names.indexOf('collect'))
})
for(const protectedBlock of ['bee_nest','beehive','creaking_heart']) test(`independent: ${protectedBlock} prevents all cutting`,async()=>{
 const f=worldFixture();smallOak(f);f.world.set('1,3,0',protectedBlock)
 const r=await harvest.run(f.api,{...root,species:'oak'})
 assert.ok(r.attention.some(s=>s.includes(protectedBlock)))
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
})
test('independent: inaccessible upper wood retains the entire tree and reports actionable access',async()=>{
 const f=worldFixture();smallOak(f,12)
 const r=await harvest.run(f.api,{...root,species:'oak'})
 assert.ok(r.attention.some(s=>/access|platform|reach/.test(s)))
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
 assert.equal(f.world.get('0,1,0'),'oak_log')
})
for(const error of [new Error('cancelled'),new TypeError('unexpected type'),new Error('disconnected')]) test(`independent: ${error.message} propagates before mutation`,async()=>{
 const f=worldFixture({items:{oak_sapling:1},checkpoint:()=>{throw error}})
 await assert.rejects(plant.run(f.api,{...root,species:'oak'}),e=>e===error)
 assert.equal(f.calls.filter(c=>c.name==='place').length,0)
})
test('independent: prepare clears only actual ground cover and preserves occupied planting site',async()=>{
 const f=worldFixture();f.world.set('0,1,0','chest')
 const r=await prepare.run(f.api,{...root,species:'oak'})
 assert.ok(r.attention.length)
 assert.equal(f.world.get('0,1,0'),'chest')
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
})
test('independent: planned default F diagonal flower is restored at its exact cell before sapling',async()=>{
 const p={name:'trees',by:'Tester',...root,plan:'x \n F',legend:{x:{kind:'tree',species:'oak'}}}
 const f=worldFixture({plan:p,items:{dandelion:1,oak_sapling:1}})
 const r=await forestry.run(f.api,{place:'trees',deposit:false})
 assert.deepEqual(r.attention,[])
 assert.equal(f.world.get('1,1,1'),'dandelion')
 assert.deepEqual(f.calls.filter(c=>c.name==='place').map(c=>c.args.item),['dandelion','oak_sapling'])
})
test('independent: an existing flowering azalea is preserved without extra stock',async()=>{
 const f=worldFixture();f.world.set('0,1,0','flowering_azalea')
 const r=await plant.run(f.api,{...root,species:'azalea'})
 assert.deepEqual(r.attention,[])
 assert.equal(f.calls.filter(c=>c.name==='place').length,0)
})
test('independent: attached neighboring trunk is retained with its tree',async()=>{
 const f=worldFixture();smallOak(f)
 for(let y=1;y<=3;y++)f.world.set(`2,${y},0`,'oak_log')
 f.world.set('1,4,0','oak_leaves');f.world.set('2,4,0','oak_leaves')
 const r=await harvest.run(f.api,{...root,species:'oak'})
 assert.ok(r.attention.some(s=>/neighbor|ambig/.test(s)))
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
})
test('independent: an adjoining constructed platform prevents harvest',async()=>{
 const f=worldFixture();smallOak(f);f.world.set('1,2,0','oak_planks')
 const r=await harvest.run(f.api,{...root,species:'oak'})
 assert.ok(r.attention.some(s=>/inspection|build/.test(s)))
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
})
test('independent: protected-zone refusal remains hard before mutation',async()=>{
 const f=worldFixture({items:{oak_sapling:1}}),act=f.api.act
 f.api.act=(name,args)=>name==='zones'?Promise.resolve({zones:[{name:'Other-home',x1:-2,x2:2,y1:0,y2:5,z1:-2,z2:2}]}):act(name,args)
 await assert.rejects(plant.run(f.api,{...root,species:'oak'}),/protected zone/)
 assert.equal(f.calls.filter(c=>c.name==='place').length,0)
})
test('independent: interruption after one harvested block reports remainder and retains lower trunk',async()=>{
 let digs=0
 const f=worldFixture({fail:name=>{if(name==='dig'&&++digs===2)throw new Error('out of reach')}});smallOak(f)
 const r=await harvest.run(f.api,{...root,species:'oak'})
 assert.equal(r.harvested,1)
 assert.equal(r.remaining.length,3)
 assert.ok(r.attention.some(s=>/reach/.test(s)))
 assert.equal(f.world.get('0,1,0'),'oak_log')
 assert.ok(f.calls.some(c=>c.name==='collect'))
})
test('independent: live path-search timeout reports access attention without cutting the tree',async()=>{
 const message='tree.harvest/goto: the search ran out of time (up to 5 s) before it found a way, which is not the same as there being none. The usual cause is a dead end close to the goal (a fenced alley beside a pen gate) that the search keeps trying first, and from inside that dead end even path_to finds nothing. Step back 10-20 blocks the way you came, then `path_to x= y= z= route=true` there names the gates of the long way round: walk it in legs, gate by gate'
 const f=worldFixture({fail:name=>{if(name==='goto')throw new Error(message)}});smallOak(f)
 const r=await harvest.run(f.api,{...root,species:'oak',scaffold:false})
 assert.equal(r.harvested,0)
 assert.equal(r.remaining.length,4)
 assert.ok(r.attention.some(s=>s.includes('search ran out of time')))
 assert.ok(f.events.some(e=>e.type==='forestry_attention'))
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
 assert.equal(f.world.get('0,1,0'),'oak_log')
})
for(const message of ['bot is dead','cancelled: out of air: swimming up to breathe. Work from dry land, then retry']) test(`independent: harvest preserves hard safety handback: ${message}`,async()=>{
 const error=new Error(message)
 const f=worldFixture({fail:name=>{if(name==='goto')throw error}});smallOak(f)
 await assert.rejects(harvest.run(f.api,{...root,species:'oak',scaffold:false}),e=>e===error)
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
})
test('independent: standalone days=1 terminates after an elapsed day',async()=>{
 let elapsed=0,waits=0
 const p={name:'trees',by:'Tester',...root,plan:'x',legend:{x:{kind:'tree',species:'oak'}}}
 const f=worldFixture({plan:p,items:{oak_sapling:1}})
 f.api.clock=()=>({elapsedDays:elapsed,day:true,night:false})
 f.api.until=async()=>{if(++waits>4)throw new Error('unbounded days loop');elapsed=1;return true}
 const r=await forestry.run(f.api,{place:'trees',deposit:false,days:1})
 assert.ok(r.sweeps<=2)
 assert.ok(waits<=2)
})
test('independent: fungi report mandatory growth supply when optional bone meal is disabled',async()=>{
 const p={name:'trees',by:'Tester',...root,plan:'x',legend:{x:{kind:'tree',species:'warped'}}}
 const f=worldFixture({soil:'warped_nylium',plan:p,items:{warped_fungus:1}})
 const r=await forestry.run(f.api,{place:'trees',deposit:false})
 assert.equal(r.planted,1)
 assert.ok(r.attention.some(s=>/requires bone meal/.test(s)))
 assert.ok(!f.calls.some(c=>c.name==='fertilize'))
})
test('independent: a large tree interrupted mid planting completes its remaining saplings on retry',async()=>{
 let placements=0,interrupted=true
 const f=worldFixture({items:{dark_oak_sapling:4},fail:name=>{
   if(name==='place'&&++placements===2&&interrupted)throw new Error('out of reach')
 }})
 const args={...root,species:'dark_oak',form:'large'}
 const first=await plant.run(f.api,args)
 assert.equal(first.planted,1)
 assert.ok(first.attention.length)
 interrupted=false
 const second=await plant.run(f.api,args)
 assert.deepEqual(second.attention,[])
 assert.equal(second.planted,3)
 for(const [x,z] of [[0,0],[1,0],[0,1],[1,1]])assert.equal(f.world.get(`${x},1,${z}`),'dark_oak_sapling')
})
test('independent: unloaded envelope prevents mutation instead of assuming empty space',async()=>{
 const f=worldFixture({items:{oak_sapling:1}}),read=f.api.block
 f.api.block=(x,y,z)=>x===5&&y===10&&z===0?null:read(x,y,z)
 const r=await plant.run(f.api,{...root,species:'oak'})
 assert.ok(r.attention.some(s=>/unloaded/.test(s)))
 assert.equal(f.calls.filter(c=>c.name==='place').length,0)
})
test('independent: placed persistent foliage prevents harvest of a decorative tree',async()=>{
 const f=worldFixture();smallOak(f)
 const read=f.api.block
 f.api.block=(x,y,z)=>{const b=read(x,y,z);return b.name==='oak_leaves'?{...b,properties:{persistent:true}}:b}
 const r=await harvest.run(f.api,{...root,species:'oak'})
 assert.ok(r.attention.some(s=>/persistent|build/.test(s)))
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
})
test('independent: a wrong fungus substrate is attention and is never silently replaced',async()=>{
 const f=worldFixture({soil:'crimson_nylium',items:{warped_fungus:1}})
 const r=await plant.run(f.api,{...root,species:'warped'})
 assert.ok(r.attention.some(s=>/warped_nylium/.test(s)))
 assert.equal(f.calls.filter(c=>['place','dig'].includes(c.name)).length,0)
})
test('independent: optional growth occurs after the exact planned flower and sapling',async()=>{
 const p={name:'trees',by:'Tester',...root,plan:'xF',legend:{x:{kind:'tree',species:'oak'}}}
 const f=worldFixture({plan:p,items:{dandelion:1,oak_sapling:1,bone_meal:1}})
 const act=f.api.act
 f.api.act=async(name,args)=>{const r=await act(name,args);if(name==='fertilize'){assert.equal(f.world.get('1,1,0'),'dandelion');assert.equal(f.world.get('0,1,0'),'oak_sapling');f.items.bone_meal--;smallOak(f)}return r}
 const r=await forestry.run(f.api,{place:'trees',deposit:false,bone_meal:true})
 assert.deepEqual(r.attention,[])
 assert.equal(r.bone_meal_used,1)
 assert.equal(f.world.get('0,1,0'),'oak_log')
 assert.deepEqual(f.calls.filter(c=>['place','fertilize'].includes(c.name)).map(c=>c.name),['place','place','fertilize'])
})
function enableScaffolds(f,{cancelAt}={}) {
 const records=new Map(), act=f.api.act
 f.api.navigationCapabilities=()=>({scaffolding:true})
 f.api.scaffolds=(id,value)=>{if(value===null)records.delete(id);else if(value!==undefined)records.set(id,structuredClone(value));return records.get(id)}
 f.api.scaffoldOccupied=()=>false
 f.api.pause=async()=>{}
 let count=0
 f.api.act=async(name,args)=>{
   if(name==='scaffold_extend'){
     f.calls.push({name,args});assert.ok(f.items.scaffolding>0);f.items.scaffolding--;f.world.set(coord(args),'scaffolding')
     if(++count===cancelAt)throw new Error('cancelled')
     return {}
   }
   const scaffoldBase=name==='dig'&&f.world.get(coord(args))==='scaffolding'
   if(scaffoldBase){assert.equal(f.api.pos().y,1,'must descend onto ground before removing support');assert.ok(f.api.pos().x!==args.x+.5||f.api.pos().z!==args.z+.5,'must leave the column first')}
   const r=await act(name,args)
   if(scaffoldBase){for(const [p,block]of f.world){const [x,,z]=p.split(',').map(Number);if(block==='scaffolding'&&x===args.x&&z===args.z)f.world.set(p,'air')}}
   return r
 }
 return records
}
test('independent: tall tree harvest builds access, removes upper tree, descends and cleans owned scaffold',async()=>{
 const f=worldFixture({items:{scaffolding:64}});smallOak(f,10)
 const records=enableScaffolds(f)
 const r=await harvest.run(f.api,{...root,species:'oak'})
 assert.deepEqual(r.attention,[])
 assert.equal(r.remaining.length,0)
 assert.equal(r.harvested,11)
 assert.ok(f.calls.some(c=>c.name==='scaffold_extend'))
 assert.equal(records.size,0)
 assert.ok(![...f.world.values()].includes('scaffolding'))
})
test('independent: cancellation during scaffold extension leaves recoverable provenance and no tree cuts',async()=>{
 const f=worldFixture({items:{scaffolding:64}});smallOak(f,10)
 const records=enableScaffolds(f,{cancelAt:1})
 await assert.rejects(harvest.run(f.api,{...root,species:'oak'}),/cancelled/)
 assert.equal(f.calls.filter(c=>c.name==='dig').length,0)
 assert.equal(f.world.get('0,1,0'),'oak_log')
 assert.equal(records.size,1)
 const record=[...records.values()][0]
 for(const [p,block]of f.world)if(block==='scaffolding')assert.ok(record.verified.includes(p),`placed scaffold ${p} journaled`)
})
test('independent: a registered serialized sloped tree plan maintains its actual ground height exactly once',async()=>{
 const f=worldFixture({items:{oak_sapling:1}})
 await farmPlan.run(f.api,{name:'slope',x:0,y:3,z:0,map:'x',legend:{x:{kind:'tree',species:'oak',ground_offset:-3}}})
 f.places[0]=JSON.parse(JSON.stringify(f.places[0]))
 assert.equal(planCells(f.places[0])[0].y,0)
 const r=await forestry.run(f.api,{place:'slope',deposit:false})
 assert.deepEqual(r.attention,[])
 assert.equal(r.planted,1)
 assert.equal(f.world.get('0,1,0'),'oak_sapling')
 assert.equal(f.world.get('0,4,0'),undefined)
})
