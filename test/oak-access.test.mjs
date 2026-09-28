import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { runTree } from '../src/tree/actions.mjs'
import { planScaffoldAccess, planTreeAccess, scaffoldId, reachTreePlatform } from '../src/scaffold/access.mjs'
import { checkTree, harvestStands } from '../src/tree/inspect.mjs'
import { centerStand } from '../src/navigation/center-stand.mjs'
import { traceVisibleTreeBlock, visibleTreeBlock } from '../src/scaffold/reach.mjs'
const fixture=JSON.parse(fs.readFileSync(new URL('./fixtures/live-oak.json',import.meta.url),'utf8'))
const key=p=>`${p.x},${p.y},${p.z}`
class Vec3{constructor(x,y,z){Object.assign(this,{x,y,z})}clone(){return new Vec3(this.x,this.y,this.z)}}
function oakFixture(){
 const world=new Map(),items={scaffolding:30,dirt:64,spruce_log:3,birch_log:6},journal=new Map(),calls=[]
 for(const b of fixture.blocks)world.set(key(b),{...b,solid:true})
 for(const b of fixture.terrain)world.set(key(b),{...b,solid:b.name==='grass_block'||b.name==='dirt'})
 let pos={x:-100.5,y:70,z:-214.5}
 const block=(x,y,z)=>{const b=world.get(`${x},${y},${z}`);return b?{...b,boundingBox:b.solid?'block':'empty'}:{name:'air',solid:false,boundingBox:'empty',properties:{}}}
 const api={block,inv:()=>items,me:()=> 'test',places:()=>[],pos:()=>pos,report:()=>{},emit:()=>{},pause:async()=>{},checkpoint:async()=>{},navigationCapabilities:()=>({scaffolding:true}),scaffoldOccupied:()=>false,
 scaffolds:(id,value)=>{if(value===undefined)return journal.get(id)??null;if(value===null)journal.delete(id);else journal.set(id,structuredClone(value));return value},
 async act(name,a){calls.push({name,...a});if(name==='zones')return{zones:[]};if(name==='collect')return{};
  if(name==='goto'){const drift=a.y>70?-.3:0;pos={x:a.x+.5+drift,y:a.y,z:a.z+.5+drift};return{}}
  if(name==='center_work_stand'){
   let forward=false,aim=null
   const bot={vehicle:null,entity:{onGround:true,get position(){return new Vec3(pos.x,pos.y,pos.z)}},pathfinder:{setGoal:()=>{}},blockAt:p=>block(p.x,p.y,p.z),lookAt:async p=>{aim=p},setControlState:(s,v)=>{if(s==='forward')forward=v},waitForTicks:async n=>{for(let i=0;i<n;i++)if(forward&&aim){const dx=aim.x-pos.x,dz=aim.z-pos.z,d=Math.hypot(dx,dz),step=Math.min(.02,d);pos={...pos,x:pos.x+dx/d*step,z:pos.z+dz/d*step}}}}
   return centerStand({bot,Vec3,target:{x:a.x,y:a.y,z:a.z},support:a.support,alive:async()=>{}})
  }
  if(name==='pillar_up'){
   const p={x:Math.floor(pos.x),y:Math.round(pos.y),z:Math.floor(pos.z)}
   assert.equal(block(p.x,p.y,p.z).name,'air','pillar may use only clear cells')
   assert.ok(['dirt','grass_block'].includes(block(p.x,p.y-1,p.z).name),'pillar remains grounded')
   assert.ok(items[a.item]>0);items[a.item]--;world.set(key(p),{name:a.item,solid:true})
   pos={x:p.x+.2,y:p.y+1,z:p.z+.2};return{}
  }
  if(name==='dig'){
   const current=block(a.x,a.y,a.z)
   assert.ok(current.name==='oak_log'||(current.name==='dirt'&&journal.get(scaffoldId(fixture.root))?.cells.some(p=>key(p)===key(a))),`may dig only tree logs or owned pillar, saw ${current.name} at ${key(a)}`)
   if(current.name==='dirt'){
    const record=journal.get(scaffoldId(fixture.root))
    assert.ok(record.verified.includes(key(a)),`unverified/missing ownership at ${key(a)}: ${JSON.stringify(record)}`)
    assert.equal(pos.y,a.y+1)
   }
   world.delete(key(a));
   if(current.name==='dirt'){
    items.dirt++
    pos={x:a.x+.5,y:a.y,z:a.z+.5}
   }
   return{}
  }
  return{}
 }}
 return{api,world,items,journal,calls}
}
test('Treebeard oak logs are harvested from planned access without touching leaves or terrain',async()=>{
 const f=oakFixture();f.items.scaffolding=0
 const tree=checkTree(f.api.block,fixture.root,'oak'),woodTree={...tree,blocks:tree.wood}
 const api={...f.api,navigationCapabilities:()=>({scaffolding:true})},access=harvestStands(woodTree,f.api.block)
 const scaffold=planScaffoldAccess(api,woodTree,access)
 assert.doesNotMatch(scaffold.attention.join(' '),/whole-tree access missing for 1 blocks/,'wood targets may be coordinate-dug through this tree\'s leaves')
 const preferred=planTreeAccess(api,woodTree,access)
 assert.equal(preferred.attention.length,0)
 const r=await runTree(f.api,{...fixture.root,species:'oak'},'harvest')
 assert.deepEqual(r.attention,[])
 assert.equal(r.harvested,5);assert.deepEqual(r.remaining,[])
 assert.equal([...f.world.values()].filter(b=>b.name==='oak_leaves').length,71)
 assert.deepEqual(r.cleanup_left,[]);assert.equal(f.journal.size,0);assert.equal(f.items.dirt,64)
 assert.ok(f.calls.some(c=>c.name==='pillar_up'))
})

test('solid access uses verified nearby ground below the planting level',()=>{
 const f=oakFixture();f.items.scaffolding=0
 const tree=checkTree(f.api.block,fixture.root,'oak'),woodTree={...tree,blocks:tree.wood}
 // The approach side of a forest plot can sit one block below the registered
 // root. Remove the level candidate ground and leave the same full blocks below.
 for(const [k,b] of [...f.world])if(b.name==='grass_block'&&b.y===fixture.root.y){
  f.world.delete(k);f.world.set(`${b.x},${b.y-1},${b.z}`,{...b,y:b.y-1})
 }
 const access=harvestStands(woodTree,f.api.block)
 const plan=planTreeAccess(f.api,woodTree,access,[])
 assert.equal(plan.attention.length,0)
 assert.ok(plan.columns.some(c=>c.y===fixture.root.y))
 assert.deepEqual(plan.attention,[])
})

test('centering cancellation stops the current tick and always clears forward control',async()=>{
 const blocks=new Map([['0,0,0',{name:'dirt',boundingBox:'block'}]])
 let pos={x:.2,y:1,z:.2},controls={forward:false,sneak:false},ticks=0
 const bot={vehicle:null,entity:{onGround:true,get position(){return new Vec3(pos.x,pos.y,pos.z)}},pathfinder:{setGoal:()=>{}},blockAt:p=>blocks.get(`${p.x},${p.y},${p.z}`)??{name:'air',boundingBox:'empty'},lookAt:async()=>{},setControlState:(s,v)=>{controls[s]=v},waitForTicks:async()=>{if(controls.forward)pos={...pos,x:pos.x+.02,z:pos.z+.02};ticks++}}
 await assert.rejects(centerStand({bot,Vec3,target:{x:0,y:1,z:0},support:'dirt',alive:()=>{if(ticks>=3)throw new Error('cancelled')}}),/cancelled/)
 assert.equal(controls.forward,false);assert.equal(controls.sneak,false);assert.equal(Math.floor(pos.x),0);assert.equal(Math.floor(pos.z),0)
})

test('stable scaffold deck is accepted without onGround or sneak-centering',async()=>{
 const blocks=new Map([['0,0,0',{name:'scaffolding',boundingBox:'empty'}]])
 const controls={forward:false,sneak:false,jump:false};let ticks=0
 const pos={x:.5,y:1,z:.5}
 const bot={vehicle:null,entity:{onGround:false,get position(){return new Vec3(pos.x,pos.y,pos.z)}},pathfinder:{setGoal:()=>{}},blockAt:p=>blocks.get(`${p.x},${p.y},${p.z}`)??{name:'air',boundingBox:'empty'},setControlState:(s,v)=>{controls[s]=v},waitForTicks:async()=>{ticks++}}
 const reached=await centerStand({bot,Vec3,target:{x:0,y:1,z:0},support:'scaffolding',alive:async()=>{}})
 assert.deepEqual(reached,{x:.5,y:1,z:.5});assert.ok(ticks>=1);assert.deepEqual(controls,{forward:false,sneak:false,jump:false})
})

test('airborne scaffold stand outside the top-face tolerance is refused without movement',async()=>{
 const blocks=new Map([['0,0,0',{name:'scaffolding',boundingBox:'empty'}]])
 const controls={forward:false,sneak:false,jump:false},pos={x:.5,y:1.2,z:.5}
 const bot={vehicle:null,entity:{onGround:false,get position(){return new Vec3(pos.x,pos.y,pos.z)}},pathfinder:{setGoal:()=>{}},blockAt:p=>blocks.get(`${p.x},${p.y},${p.z}`)??{name:'air',boundingBox:'empty'},setControlState:(s,v)=>{controls[s]=v},waitForTicks:async()=>{}}
 await assert.rejects(centerStand({bot,Vec3,target:{x:0,y:1,z:0},support:'scaffolding',alive:async()=>{}}),/airborne or off-cell scaffold position/)
 assert.deepEqual(controls,{forward:false,sneak:false,jump:false})
})

test('scaffold climb cell with scaffolding in the feet block is not accepted as a work deck',async()=>{
 const blocks=new Map([['0,0,0',{name:'scaffolding',boundingBox:'empty'}],['0,1,0',{name:'scaffolding',boundingBox:'empty'}]])
 const controls={forward:false,sneak:false,jump:false},pos={x:.5,y:1,z:.5}
 const bot={vehicle:null,entity:{onGround:false,get position(){return new Vec3(pos.x,pos.y,pos.z)}},pathfinder:{setGoal:()=>{}},blockAt:p=>blocks.get(`${p.x},${p.y},${p.z}`)??{name:'air',boundingBox:'empty'},setControlState:(s,v)=>{controls[s]=v},waitForTicks:async()=>{}}
 await assert.rejects(centerStand({bot,Vec3,target:{x:0,y:1,z:0},support:'scaffolding',alive:async()=>{}}),/scaffold climb cell as a work deck/)
 assert.deepEqual(controls,{forward:false,sneak:false,jump:false})
})

test('scaffold work refusal inside the column continues to the next clear deck',async()=>{
 const world=new Map([['0,0,0',{name:'scaffolding',boundingBox:'empty'}],['0,1,0',{name:'scaffolding',boundingBox:'empty'}]])
 let pos={x:.5,y:1,z:.5};const calls=[],report={attention:[]}
 const column={x:0,y:0,z:0,top:1,exit:{x:1,y:1,z:0}},spots=[{x:0,y:1,z:0,scaffold:column},{x:0,y:2,z:0,scaffold:column}]
 const cells=[{x:0,y:0,z:0},{x:0,y:1,z:0}],record={columns:[column],platforms:[],cells,verified:cells.map(key)}
 const api={pos:()=>pos,block:(x,y,z)=>world.get(`${x},${y},${z}`)??{name:'air'},checkpoint:async()=>{},async act(name,a){calls.push({name,...a});if(name==='goto'){pos={x:a.x+.5,y:a.y,z:a.z+.5};return{}}if(name==='center_work_stand'){
   const bot={vehicle:null,entity:{onGround:false,get position(){return new Vec3(pos.x,pos.y,pos.z)}},pathfinder:{setGoal:()=>{}},blockAt:p=>{const b=world.get(`${p.x},${p.y},${p.z}`);return b??{name:'air',boundingBox:'empty'}},setControlState:()=>{},waitForTicks:async()=>{}}
   return centerStand({bot,Vec3,target:{x:a.x,y:a.y,z:a.z},support:a.support,alive:async()=>{}})
  }}}
 const reached=await reachTreePlatform(api,spots,report,record)
 assert.equal(reached,spots[1]);assert.deepEqual(report.attention,[])
 assert.ok(calls.some(c=>c.name==='goto'&&c.y===2),'the next clear deck is reached after refusing the climb cell')
})

test('tree ray refuses a target when the center ray clips a neighboring leaf',()=>{
 const blocks=new Map([['1,2,0',{name:'oak_leaves'}]])
 const at=(x,y,z)=>blocks.get(`${x},${y},${z}`)??{name:'air'}
 const feet={x:.5,y:0,z:.5},target={x:2,y:2,z:0}
 const centerOnly=at
 const centerHit=(()=>{
  const eye={x:feet.x,y:feet.y+1.62,z:feet.z},end={x:target.x+.5,y:target.y+.5,z:target.z+.5}
  const len=Math.hypot(end.x-eye.x,end.y-eye.y,end.z-eye.z)
  for(let d=.02;d<len;d+=.035){const p={x:Math.floor(eye.x+(end.x-eye.x)*d/len),y:Math.floor(eye.y+(end.y-eye.y)*d/len),z:Math.floor(eye.z+(end.z-eye.z)*d/len)};if(key(p)===key(target))return true;const b=centerOnly(p.x,p.y,p.z);if(b.name!=='air')return false}
  return true
 })()
 assert.equal(centerHit,false)
 assert.equal(visibleTreeBlock(at,feet,target),false)
 const trace=traceVisibleTreeBlock(at,feet,target)
 assert.equal(trace.visible,false)
 assert.equal(trace.obstruction.name,'oak_leaves')
 assert.deepEqual([trace.obstruction.x,trace.obstruction.y,trace.obstruction.z],[1,2,0])
})
