import test from 'node:test'
import assert from 'node:assert/strict'
import { cleanupPillar, descendPillar, reachPillar } from '../src/scaffold/pillar.mjs'
import { visibleTreeBlock } from '../src/scaffold/reach.mjs'

const key=p=>`${p.x},${p.y},${p.z}`
test('pillar enters through two clear trunk cells before clearing a leaf-occluded third log',async()=>{
 const world=new Map(),calls=[],items={dirt:1},journal=new Map()
 for(let y=67;y<=69;y++)world.set(`0,${y},0`,{name:'birch_log',solid:true})
 // The side exit is beside the top trunk log. This leaf blocks its center ray.
 world.set('1,69,0',{name:'birch_leaves',solid:true})
 world.set('1,70,0',{name:'birch_leaves',solid:true})
 let pos={x:1.5,y:67,z:.5}
 const block=(x,y,z)=>world.get(`${x},${y},${z}`)??{name:y<=66?'dirt':'air',solid:y<=66}
 const column={x:0,y:67,z:0,top:68,exit:{x:1,y:67,z:0}}
 const record={id:'tree_0_66_0',root:{x:0,y:66,z:0},kind:'pillar',species:'birch',form:'single',clearance:[67,68,69].map(y=>({x:0,y,z:0,name:'birch_log'})),columns:[column],platforms:[],cells:[],verified:[]}
 const api={
  block, pos:()=>pos, inv:()=>items, me:()=> 'test', checkpoint:async()=>{}, pause:async()=>{}, scaffoldOccupied:()=>false,
  scaffolds:(id,value)=>{if(value===undefined)return journal.get(id)??null;if(value===null)journal.delete(id);else journal.set(id,value);return value},
  act:async(name,a={})=>{
   calls.push({name,...a})
   if(name==='zones')return{zones:[]}
   if(name==='collect')return{}
   if(name==='goto'){pos={x:a.x+.5,y:a.y,z:a.z+.5};return{}}
   if(name==='dig'){
    const b=block(a.x,a.y,a.z)
    if(b.name==='dirt'){
     assert.equal(record.verified.includes(key(a)),true)
     world.delete(key(a));items.dirt++;pos={x:a.x+.5,y:a.y,z:a.z+.5}
    }else world.delete(key(a))
    return{}
   }
   if(name==='pillar_up'){
    const p={x:Math.floor(pos.x),y:Math.round(pos.y),z:Math.floor(pos.z)}
    assert.deepEqual([0,1,2,3].map(dy=>block(p.x,p.y+dy,p.z).name),['air','air','air','air'])
    world.set(key(p),{name:a.item,solid:true});
    items[a.item]--
    pos={x:p.x+.5,y:p.y+1,z:p.z+.5};return{}
   }
   if(name==='center_work_stand'){pos={x:a.x+.5,y:a.y,z:a.z+.5};return{}}
   return{}
  }
 }
 const outside={x:1.5,y:67,z:.5},upper={x:0,y:69,z:0}
 assert.equal(visibleTreeBlock(block,outside,upper),false)
 const report={attention:[]}
 const spot=await reachPillar(api,[{x:0,y:68,z:0,scaffold:column}],report,record)
 assert.deepEqual(report.attention,[])
 assert.ok(spot)
 assert.equal(block(0,69,0).name,'air','the upper trunk log is cleared after entering the opening')
 assert.equal(block(1,69,0).name,'birch_leaves','canopy leaves are retained')
 assert.ok(calls.some(c=>c.name==='pillar_up'))
 assert.ok(await descendPillar(api,record,report))
 assert.equal(block(0,67,0).name,'air','the temporary pillar is cleaned')
 assert.equal(items.dirt,1)
 assert.deepEqual(report.cleanup_left,[])
})

test('successful pillar_up return proves placement even while inventory count is stale',async()=>{
 const world=new Map(),journal=new Map(),items={dirt:1}
 let pos={x:.5,y:67.7,z:.5},onGround=false,pauses=0
 world.set('0,66,0',{name:'dirt',solid:true})
 world.set('0,67,0',{name:'air'})
 const r={id:'tree_0_66_0',root:{x:0,y:66,z:0},kind:'pillar',species:'birch',form:'single',clearance:[],columns:[{x:0,y:67,z:0,top:67}],platforms:[],cells:[],verified:[]}
 const api={
  block:(x,y,z)=>world.get(`${x},${y},${z}`)??{name:y<67?'dirt':'air',solid:y<67},
  pos:()=>pos,grounded:()=>onGround,inv:()=>items,me:()=> 'test',checkpoint:async()=>{},pause:async()=>{if(!onGround&&++pauses>=2){pos={x:.5,y:67,z:.5};onGround=true}},scaffoldOccupied:()=>false,
  scaffolds:(id,value)=>{if(value===undefined)return journal.get(id)??null;if(value===null)journal.delete(id);else journal.set(id,value);return value},
  act:async(name,a={})=>{
   if(name==='goto'){pos={x:.5,y:67.7,z:.5};onGround=false;return{}}
   if(name==='pillar_up'){
    assert.equal(onGround,true,'pillar_up is not called until landing is observed twice')
    world.set('0,67,0',{name:a.item,solid:true})
    pos={x:.5,y:68,z:.5}
    onGround=true
    return{from:{x:.5,y:67,z:.5},to:{x:.5,y:68,z:.5},raised:1,item:a.item}
   }
   if(name==='center_work_stand'){pos={x:a.x+.5,y:a.y,z:a.z+.5};return{}}
   if(name==='zones')return{zones:[]}
   if(name==='dig'){world.set(`${a.x},${a.y},${a.z}`,{name:'air'});return{}}
   return{}
  }
 }
 // Inventory remains unchanged even though the explicit primitive succeeded.
 const report={attention:[]}
 const spot=await reachPillar(api,[{x:0,y:68,z:0,scaffold:{x:0,y:67,z:0,top:67,exit:{x:0,y:67,z:0}}}],report,r)
 assert.ok(spot)
 assert.deepEqual(report.attention,[])
 assert.deepEqual(r.verified,['0,67,0'])
 assert.equal(r.cells[0].provenance,'pillar_up-confirmed')
})

test('never-grounded ascent attempt leaves existing support journaled and makes no pillar_up call',async()=>{
 const world=new Map(),journal=new Map(),calls=[]
 world.set('0,66,0',{name:'dirt',solid:true})
 world.set('0,67,0',{name:'dirt',solid:true})
 let pos={x:.5,y:68.4,z:.5}
 const cell={x:0,y:67,z:0,item:'dirt'},r={id:'tree_0_66_0',root:{x:0,y:66,z:0},kind:'pillar',species:'oak',form:'single',clearance:[],columns:[{x:0,y:67,z:0,top:68}],platforms:[],cells:[cell],verified:['0,67,0']}
 const api={
  block:(x,y,z)=>world.get(`${x},${y},${z}`)??{name:'air',solid:false},pos:()=>pos,grounded:()=>false,inv:()=>({dirt:4}),me:()=> 'test',checkpoint:async()=>{},pause:async()=>{},
  scaffolds:(id,value)=>{if(value===undefined)return journal.get(id)??null;if(value===null)journal.delete(id);else journal.set(id,value);return value},
  act:async(name,a={})=>{calls.push({name,...a});if(name==='zones')return{zones:[]};return{}}
 }
 api.scaffolds(r.id,r)
 const report={attention:[]}
 const spot=await reachPillar(api,[{x:0,y:69,z:0,scaffold:{x:0,y:67,z:0,top:68,exit:{x:1,y:67,z:0}}}],report,r)
 assert.equal(spot,null)
 assert.match(report.attention.join(' '),/did not settle on grounded feet/)
 assert.equal(calls.some(c=>c.name==='pillar_up'),false)
 assert.equal(world.get('0,67,0').name,'dirt')
 assert.deepEqual(journal.get(r.id).cells,[cell])
})

test('late grounded-precondition refusal is handled and acknowledges only that pillar_up failure',async()=>{
 const journal=new Map(),calls=[],acknowledged=[]
 let pos={x:.5,y:67,z:.5}
 const r={id:'tree_0_66_0',root:{x:0,y:66,z:0},kind:'pillar',species:'oak',form:'single',clearance:[],columns:[{x:0,y:67,z:0,top:67}],platforms:[],cells:[],verified:[]}
 const api={
  block:(x,y,z)=>({name:y<67?'dirt':'air',solid:y<67}),pos:()=>pos,grounded:()=>true,inv:()=>({dirt:4}),me:()=> 'test',checkpoint:async()=>{},pause:async()=>{},scaffoldOccupied:()=>false,
  acknowledgeFailure:name=>{acknowledged.push(name);return true},
  scaffolds:(id,value)=>{if(value===undefined)return journal.get(id)??null;if(value===null)journal.delete(id);else journal.set(id,value);return value},
  act:async(name,a={})=>{calls.push({name,...a});if(name==='goto')pos={x:a.x+.5,y:a.y,z:a.z+.5};if(name==='pillar_up')throw Error('forestry.maintain/pillar_up: pillar_up requires grounded feet and no vehicle');if(name==='zones')return{zones:[]};return{}}
 }
 const report={attention:[]}
 const spot=await reachPillar(api,[{x:0,y:68,z:0,scaffold:{x:0,y:67,z:0,top:67,exit:{x:0,y:67,z:0}}}],report,r)
 assert.equal(spot,null)
 assert.match(report.attention.join(' '),/lost grounded footing/)
 assert.deepEqual(acknowledged,['pillar_up'])
 assert.equal(journal.get(r.id).cells[0].lastAttempt.completed,false)
 assert.equal(journal.get(r.id).cells[0].lastAttempt.observed,'air')
})

test('legacy journaled pillar intent recovers only when it cannot be an original tree log',async()=>{
 const cases=[
  {originalName:null,recover:true},
  {originalName:'dark_oak_log',recover:false},
  {originalName:'oak_log',recover:true},
  {originalName:null,recordRoot:1,recover:false},
  {originalName:null,phase:'decaying',recover:false}
 ]
 for(const {originalName,recordRoot=0,phase='harvesting',recover} of cases){
  const world=new Map(),journal=new Map(),calls=[]
  world.set('0,69,0',{name:'dirt',solid:true})
  world.set('0,70,0',{name:'dark_oak_log',solid:true})
  let pos={x:.5,y:71,z:.5}
  const r={id:'tree_0_69_0',root:{x:0,y:69,z:0},kind:'pillar',species:'dark_oak',form:'large',clearance:[],columns:[{x:0,y:70,z:0,top:70}],platforms:[],cells:[{x:0,y:70,z:0,item:'dark_oak_log'}],verified:[]}
  const api={
   block:(x,y,z)=>world.get(`${x},${y},${z}`)??{name:y<69?'stone':'air',solid:y<69},
   pos:()=>pos,inv:()=>({}),me:()=> 'test',checkpoint:async()=>{},pause:async()=>{},scaffoldOccupied:()=>false,
   forestry:()=>({root:{x:recordRoot,y:69,z:0},species:'dark_oak',form:'large',phase,wood:originalName?[{x:0,y:70,z:0,name:originalName}]:[{x:1,y:70,z:0,name:'dark_oak_log'}]}),
   scaffolds:(id,value)=>{if(value===undefined)return journal.get(id)??null;if(value===null)journal.delete(id);else journal.set(id,value);return value},
   act:async(name,a={})=>{
    calls.push(name)
    if(name==='zones')return{zones:[]}
    if(name==='dig'){world.set(`${a.x},${a.y},${a.z}`,{name:'air'});pos={x:.5,y:70,z:.5};return{}}
    return{}
   }
  }
  const report={attention:[]}
  await cleanupPillar(api,r,report)
  if(!recover){
   assert.equal(world.get('0,70,0').name,'dark_oak_log')
   assert.equal(calls.includes('dig'),false)
   assert.match(report.attention[0],/unverified temporary pillar/)
  }else{
   assert.equal(world.get('0,70,0').name,'air')
   assert.ok(calls.includes('dig'))
   assert.equal(journal.has(r.id),false)
  }
 }
})

test('cleanup approaches a verified one-block pillar from ground before removing it',async()=>{
 const world=new Map(),journal=new Map(),calls=[]
 world.set('0,69,0',{name:'dirt',solid:true})
 world.set('0,70,0',{name:'dark_oak_log',solid:true})
 world.set('1,69,0',{name:'dirt',solid:true})
 let pos={x:1.5,y:70,z:.5}
 const p={x:0,y:70,z:0,item:'dark_oak_log'},r={id:'tree_0_69_0',root:{x:0,y:69,z:0},kind:'pillar',species:'dark_oak',form:'large',clearance:[],columns:[{x:0,y:70,z:0,top:70}],platforms:[],cells:[p],verified:['0,70,0']}
 const api={
  block:(x,y,z)=>world.get(`${x},${y},${z}`)??{name:'air',solid:false},pos:()=>pos,inv:()=>({}),me:()=> 'test',checkpoint:async()=>{},pause:async()=>{},scaffoldOccupied:()=>false,
  scaffolds:(id,value)=>{if(value===undefined)return journal.get(id)??null;if(value===null)journal.delete(id);else journal.set(id,value);return value},
  act:async(name,a={})=>{
   calls.push({name,...a})
   if(name==='zones')return{zones:[]}
   if(name==='goto'){pos={x:a.x+.5,y:a.y,z:a.z+.5};return{}}
   if(name==='dig'){world.set(`${a.x},${a.y},${a.z}`,{name:'air',solid:false});pos={x:a.x+.5,y:a.y,z:a.z+.5};return{}}
   return{}
  }
 }
 const report={attention:[]}
 await cleanupPillar(api,r,report)
 assert.deepEqual(report.attention,[])
 assert.deepEqual(report.cleanup_left,[])
 assert.equal(world.get('0,70,0').name,'air')
 assert.equal(journal.has(r.id),false)
 const go=calls.find(c=>c.name==='goto')
 assert.deepEqual({x:go.x,y:go.y,z:go.z,range:go.range,dig:go.dig,into:go.into},{x:0,y:71,z:0,range:0,dig:false,into:true})
})
