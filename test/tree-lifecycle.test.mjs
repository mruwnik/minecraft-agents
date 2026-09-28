import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, planCells as canonicalCells, migratePlan, planErrors, planBill, planSpec } from '../src/lib/plan.mjs'
import { planChests } from '../src/lib/storage.mjs'
import { farmJobs } from '../src/lib/jobs.mjs'
import { clutterBlocks } from '../library/farm/shared/clutter.mjs'
import { inspectTree, checkTree } from '../src/tree/inspect.mjs'
import { TREE_PROFILES, treeProfile } from '../src/tree/profiles.mjs'
import { runTree } from '../src/tree/actions.mjs'
import maintain from '../library/forestry/maintain.mjs'

const planCells = place => canonicalCells(migratePlan(place))

function fixture (extra = {}, inventory = {}) {
  const world = new Map(Object.entries(extra))
  let pos = { x: -2.5, y: 1, z: 0.5 }
  const calls = [], events = []
  const api = {
    block: (x,y,z) => world.get(`${x},${y},${z}`) ?? { name: y <= 0 ? 'dirt' : 'air', solid: y <= 0, properties: {} },
    inv: () => inventory, pos: () => pos, places: () => [], me: () => 'tester', checkpoint: async () => {}, report: () => {},
    emit: (name, data) => events.push({name,...data}),
    async act (name,a) {
      calls.push({name,...a})
      if (name === 'zones') return { zones: [] }
      if (name === 'goto') pos = { x:a.x+.5,y:a.y,z:a.z+.5 }
      if (name === 'place') { assert.ok(inventory[a.item]>0); inventory[a.item]--; world.set(`${a.x},${a.y},${a.z}`, {name:a.item,solid:!a.item.endsWith('sapling'),properties:{}}) }
      if (name === 'dig') world.set(`${a.x},${a.y},${a.z}`, {name:'air',solid:false,properties:{}})
      return {}
    }
  }
  return {api,world,calls,events}
}
const at={x:0,y:0,z:0,species:'oak'}
const wood=(name='oak_log')=>({name,solid:true,properties:{axis:'y'}})
const leaf=(name='oak_leaves')=>({name,solid:true,properties:{persistent:false}})
const tree={ '0,1,0':wood(),'0,2,0':wood(),'0,3,0':leaf(), '1,3,0':leaf() }

test('custom legend round trip, Unicode cells, defaults, predicates and literal identity',()=>{
 const legend={ X:'minecraft:chest',C:'stone','🌳':{kind:'tree',species:'birch'},q:{kind:'crop',generic:true},f:'poppy' }
 const saved=JSON.parse(JSON.stringify({x:0,y:0,z:0,plan:'X C 🌳 f',legend}))
 const parsed=parsePlan(saved.plan,saved.legend)
 assert.equal(parsed.width,7);assert.deepEqual(planErrors(parsed),[])
 assert.deepEqual(planChests(planCells(saved)),[{x:0,y:1,z:0}])
 assert.equal(planBill(parsed).birch_sapling,1)
 assert.equal(planSpec(parsePlan('q',legend).cells[0]).generic,true)
 assert.equal(parsePlan('w').cells[0].spec,undefined)
 const cells=planCells({x:0,y:0,z:0,plan:'a',legend:{a:'spruce_fence'}})
 assert.equal(planSpec(cells[0]).literal,true)
 assert.equal(clutterBlocks(cells,(x,y,z)=>({name:y===0?'dirt':y===1?'oak_fence':'air',solid:y<=1}))[0].name,'oak_fence')
})
test('large tree bill and conflicting footprint are explicit',()=>{
 assert.equal(planBill(parsePlan('S',{S:{kind:'tree',species:'spruce',form:'large'}})).spruce_sapling,4)
 assert.match(planErrors(parsePlan('Sf',{S:{kind:'tree',species:'spruce',form:'large'},f:'poppy'})).join(' '),/footprint/)
 assert.deepEqual(planErrors(parsePlan('Sr\nrr',{S:{kind:'tree',species:'spruce',form:'large'},r:{kind:'reserved'}})),[])
})
test('all profiles expose valid planting forms, fungi need matching nylium',()=>{
 assert.equal(Object.keys(TREE_PROFILES).length,12)
 for(const p of Object.values(TREE_PROFILES))for(const form of p.forms)assert.ok(treeProfile(p.species,form).plant)
 assert.throws(()=>treeProfile('dark_oak','single'),/invalid/)
 assert.match(checkTree(fixture().api.block,{x:0,y:0,z:0},'warped').attention.join(' '),/warped_nylium/)
})
test('inspect mature tree, planned farm tidy retains trunk and adjacent branch',()=>{
 const f=fixture({...tree,'1,2,0':wood()})
 assert.equal(inspectTree(f.api.block,{x:0,y:0,z:0},'oak').state,'mature')
 const cells=planCells({x:0,y:0,z:0,plan:'t.'})
 assert.deepEqual(clutterBlocks(cells,f.api.block),[])
})
test('plant ensures adjacent flower first and verifies planted sapling',async()=>{
 const f=fixture({}, {oak_sapling:1,dandelion:1})
 const r=await runTree(f.api,{...at,flower:'dandelion'},'plant')
 assert.equal(r.planted,1);assert.deepEqual(f.calls.filter(c=>c.name==='place').map(c=>c.item),['dandelion','oak_sapling'])
})
test('missing flower reports attention without planting',async()=>{
 const f=fixture({}, {oak_sapling:1})
 const r=await runTree(f.api,{...at,flower:'dandelion'},'plant')
 assert.match(r.attention.join(' '),/missing flower/);assert.equal(f.calls.some(c=>c.name==='place'),false)
 assert.equal(f.events[0].name,'forestry_attention')
})
test('small tree harvest removes only wood, retaining natural leaves for decay',async()=>{
 const f=fixture(tree)
 const r=await runTree(f.api,at,'harvest')
 assert.deepEqual(r.attention,[])
 assert.equal(r.harvested,2);assert.deepEqual(r.remaining,[])
 assert.deepEqual(f.calls.filter(c=>c.name==='dig').map(c=>c.y),[2,1])
 assert.ok(f.calls.some(c=>c.name==='collect'))
 assert.equal(f.api.block(0,3,0).name,'oak_leaves')
 assert.equal(r.decay_wait,120)
})
test('tall tree without elevated access retained whole with coordinates',async()=>{
 const blocks={};for(let y=1;y<=10;y++)blocks[`0,${y},0`]=wood();blocks['0,11,0']=leaf()
 const f=fixture(blocks);const r=await runTree(f.api,at,'harvest')
 assert.match(r.attention.join(' '),/whole-tree access missing/)
 assert.equal(f.calls.some(c=>c.name==='dig'),false)
})
for(const name of ['bee_nest','beehive','creaking_heart'])test(`${name} protects tree before any mutation`,async()=>{
 const f=fixture({...tree,'1,2,0':wood(name)})
 const r=await runTree(f.api,at,'harvest')
 assert.match(r.attention.join(' '),/protected/);assert.equal(f.calls.some(c=>['dig','place','goto'].includes(c.name)),false)
})
test('neighboring raised trunk and placed persistent leaves are ambiguous',()=>{
 const f=fixture({...tree,'1,2,0':wood(),'1,1,0':wood('dirt')})
 assert.match(inspectTree(f.api.block,{x:0,y:0,z:0},'oak').attention.join(' '),/neighboring/)
 const g=fixture({...tree,'0,3,0':{...leaf(),properties:{persistent:true}}})
 assert.match(inspectTree(g.api.block,{x:0,y:0,z:0},'oak').attention.join(' '),/persistent/)
})
test('cancellation and programming errors propagate before subsequent mutation',async()=>{
 for(const error of [new Error('cancelled'),new TypeError('broken implementation')]){
  const f=fixture({}, {oak_sapling:1});f.api.checkpoint=async()=>{throw error}
  await assert.rejects(runTree(f.api,at,'plant'),e=>e===error)
  assert.equal(f.calls.some(c=>c.name==='place'),false)
 }
})
test('forestry maintain harvests then replants same saved custom plan',async()=>{
 const f=fixture(tree,{oak_sapling:1})
 const plan=migratePlan({name:'grove',x:0,y:0,z:0,plan:'o',legend:{o:'oak_sapling'}})
 plan.cells=planCells(plan);f.api.plan=()=>plan;f.api.places=()=>[plan]
 const r=await maintain.run(f.api,{place:'grove',deposit:false})
 assert.equal(r.harvested,2);assert.equal(r.planted,1);assert.equal(r.sweeps,1)
})

test('signed tree ground offsets serialize and resolve once without moving other cells',()=>{
 const place={x:10,y:70,z:20,plan:'L.H',legend:{L:{kind:'tree',species:'oak',ground_offset:-3},H:{kind:'tree',species:'birch',ground_offset:4}}}
 const saved=JSON.parse(JSON.stringify(place)),cells=planCells(saved)
 assert.deepEqual(cells.map(c=>[c.x,c.y,c.z]),[[10,67,20],[11,70,20],[12,74,20]])
 assert.deepEqual(planCells({...saved,cells}),cells)
 assert.equal(planSpec(parsePlan(saved.plan,saved.legend).cells[0]).ground_offset,-3)
 assert.equal(planCells({...saved,legend:{L:'oak_sapling',H:'birch_sapling'}})[0].y,70)
 for(const value of [1.5,NaN,Infinity,'2',Number.MAX_SAFE_INTEGER+1])assert.match(parsePlan('a',{a:{kind:'tree',species:'oak',ground_offset:value}}).error,/ground_offset/)
 assert.match(parsePlan('a',{a:{kind:'flower',item:'dandelion',ground_offset:1}}).error,/ground_offset/)
})

for(const offset of [-3,4])test(`maintain and inspection use surveyed ground offset ${offset} exactly once`,async()=>{
 const shifted=Object.fromEntries(Object.entries(tree).map(([k,b])=>{const [x,y,z]=k.split(',').map(Number);return [`${x},${y+offset},${z}`,b]}))
 const f=fixture(shifted,{oak_sapling:1}),originalBlock=f.api.block
 f.api.block=(x,y,z)=>f.world.get(`${x},${y},${z}`)??{name:y<=offset?'dirt':'air',solid:y<=offset,properties:{}}
 const plan=migratePlan({name:'slope',x:0,y:0,z:0,plan:'o',legend:{o:{kind:'tree',species:'oak',ground_offset:offset}}})
 plan.cells=planCells(JSON.parse(JSON.stringify(plan)));f.api.plan=()=>plan;f.api.places=()=>[plan]
 const cell=plan.cells[0]
 assert.equal(inspectTree(f.api.block,cell,'oak').root.y,offset)
 const result=await maintain.run(f.api,{place:'slope',deposit:false})
 assert.equal(result.harvested,2);assert.equal(result.planted,1)
 assert.equal(f.calls.find(c=>c.name==='place'&&c.item==='oak_sapling').y,offset+1)
})

test('large-tree offset preserves footprint conflict and actual four-ground checks',()=>{
 const legend={S:{kind:'tree',species:'dark_oak',form:'large',ground_offset:2},r:{kind:'reserved'},f:'poppy'}
 assert.match(planErrors(parsePlan('Sf',legend)).join(' '),/footprint/)
 const place={x:0,y:0,z:0,plan:'Sr\nrr',legend},cells=planCells(place)
 const good=(x,y,z)=>({name:y<=2?'dirt':'air',solid:y<=2,properties:{}})
 assert.deepEqual(checkTree(good,cells.find(c=>planSpec(c)?.kind==='tree'),'dark_oak','large',cells).attention,[])
 const hole=(x,y,z)=>x===1&&z===1&&y===2?{name:'air',solid:false}:good(x,y,z)
 assert.match(checkTree(hole,cells.find(c=>planSpec(c)?.kind==='tree'),'dark_oak','large',cells).attention.join(' '),/1,2,1/)
})

test('harmless flowers on higher terrain do not block growth or make mature trees look built',()=>{
 const f=fixture({'2,2,0':{name:'dandelion',solid:false},'1,2,1':{name:'oxeye_daisy',solid:false}})
 assert.deepEqual(checkTree(f.api.block,{x:0,y:0,z:0},'spruce').attention,[])
 const mature=fixture({...tree,'1,1,0':{name:'dandelion',solid:false}})
 assert.deepEqual(inspectTree(mature.api.block,{x:0,y:0,z:0},'oak').attention,[])
 f.world.set('2,2,0',{name:'stone',solid:true})
 assert.match(checkTree(f.api.block,{x:0,y:0,z:0},'spruce').attention.join(' '),/occupied/)
})

test('maintenance leaves foliage intact, waits two minutes, returns for drops, then replants',async()=>{
 const f=fixture(tree,{oak_sapling:1}),plan=migratePlan({name:'grove',x:0,y:0,z:0,plan:'o',legend:{o:'oak_sapling'}})
 plan.cells=planCells(plan);f.api.plan=()=>plan;f.api.places=()=>[plan]
 let waited=0
 f.api.pause=async seconds=>{waited+=seconds;f.calls.push({name:'decay',seconds});assert.equal(f.api.block(0,3,0).name,'oak_leaves')}
 const result=await maintain.run(f.api,{place:'grove',deposit:false})
 assert.deepEqual(result.attention,[])
 assert.equal(waited,120)
 assert.equal(result.harvested,2);assert.equal(result.planted,1)
 const lastWait=f.calls.findLastIndex(c=>c.name==='decay')
 const collect=f.calls.findIndex((c,i)=>i>lastWait&&c.name==='collect')
 assert.ok(f.calls.findIndex((c,i)=>i>lastWait&&c.name==='goto')<collect)
 assert.ok(collect>lastWait)
 assert.ok(f.calls.findIndex(c=>c.name==='place')>collect)
 assert.deepEqual(f.calls.filter(c=>c.name==='dig').map(c=>c.y),[2,1])
})
test('cancellation during decay wait prevents later collection and replanting',async()=>{
 const f=fixture(tree,{oak_sapling:1}),plan=migratePlan({name:'grove',x:0,y:0,z:0,plan:'o',legend:{o:'oak_sapling'}})
 plan.cells=planCells(plan);f.api.plan=()=>plan;f.api.places=()=>[plan]
 let waiting=false,stoppedAt
 f.api.pause=async()=>{waiting=true;stoppedAt=f.calls.length}
 f.api.checkpoint=async()=>{if(waiting)throw Error('cancelled')}
 await assert.rejects(maintain.run(f.api,{place:'grove',deposit:false}),/cancelled/)
 assert.equal(f.calls.length,stoppedAt)
 assert.equal(f.api.block(0,3,0).name,'oak_leaves')
 assert.equal(f.calls.some(c=>c.name==='place'),false)
})
