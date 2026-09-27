import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { runTree } from '../src/tree/actions.mjs'
import { cleanupScaffold, scaffoldId } from '../src/scaffold/access.mjs'
import { scaffoldJournal } from '../src/scaffold/journal.mjs'
import { digFromHere } from '../src/lib/dig.mjs'
const root={x:0,y:0,z:0}, args={...root,species:'oak'}
const key=p=>`${p.x},${p.y},${p.z}`
function fixture(stock=128){
 const world=new Map(),items={scaffolding:stock},journal=new Map(),calls=[],events=[]
 let pos={x:3.5,y:1,z:.5}
 for(let y=1;y<=12;y++)world.set(`0,${y},0`,{name:'oak_log',solid:true,properties:{axis:'y'}})
 world.set('0,13,0',{name:'oak_leaves',solid:true,properties:{persistent:false}})
 const block=(x,y,z)=>world.get(`${x},${y},${z}`)??{name:y<=0?'dirt':'air',solid:y<=0,properties:{}}
 const api={block,inv:()=>items,me:()=> 'test',places:()=>[],pos:()=>pos,report:()=>{},emit:(name,data)=>events.push({name,...data}),pause:async()=>{},checkpoint:async()=>{},navigationCapabilities:()=>({scaffolding:true}),scaffoldOccupied:()=>false,
 scaffolds:(id,value)=>{if(value===undefined)return journal.get(id)??null;if(value===null)journal.delete(id);else journal.set(id,structuredClone(value));return value},
 async act(name,a){
  calls.push({name,...a})
  if(name==='zones')return{zones:[]}
  if(name==='goto'){
   if(a.y>1)assert.equal(block(a.x,a.y-1,a.z).name,'scaffolding','elevated arrival needs installed support')
   pos={x:a.x+.5,y:a.y,z:a.z+.5}
  }
  if(name==='place'||name==='scaffold_extend'){
   if(name==='place')assert.equal(a.item,'scaffolding');assert.equal(block(a.x,a.y,a.z).name,'air');assert.ok(items.scaffolding>0)
   assert.ok(digFromHere(pos,name==='scaffold_extend'?{...a,y:a.base_y}:a),'scaffold click stays within reach')
   items.scaffolding--;world.set(key(a),{name:'scaffolding',solid:false,properties:{distance:0,bottom:a.y===1}})
  }
  if(name==='dig'){
   assert.ok(digFromHere(pos,a),'dig stays within reach, primitive need not walk')
   const name=block(a.x,a.y,a.z).name
   if(name==='scaffolding'){
    assert.equal(pos.y,1,'descent reaches ground before removal')
    assert.ok(Math.floor(pos.x)!==a.x||Math.floor(pos.z)!==a.z,'never dig own underfoot column')
    for(let y=a.y;block(a.x,y,a.z).name==='scaffolding';y++)world.set(`${a.x},${y},${a.z}`,{name:'air',solid:false,properties:{}})
   }else world.set(key(a),{name:'air',solid:false,properties:{}})
  }
  return{}
 }}
 return{api,world,items,journal,calls,events}
}
test('tall tree is harvested completely via verified scaffold and ground-safe cleanup',async()=>{
 const f=fixture();const result=await runTree(f.api,args,'harvest')
 assert.deepEqual(result.attention,[])
 assert.equal(result.harvested,13);assert.deepEqual(result.remaining,[]);assert.deepEqual(result.cleanup_left,[])
 assert.equal(f.journal.size,0)
 assert.ok(f.calls.some(c=>c.name==='place'&&c.item==='scaffolding'))
 const cut=f.calls.findIndex(c=>c.name==='dig'&&c.x===0&&c.z===0)
 assert.ok(cut>f.calls.findLastIndex(c=>c.name==='place'))
})
test('insufficient scaffold stock is detected before any placement or base chopping',async()=>{
 const f=fixture(1);const result=await runTree(f.api,args,'harvest')
 assert.match(result.attention.join(' '),/need .* scaffolding/)
 assert.equal(f.calls.some(c=>['place','dig'].includes(c.name)),false)
 assert.equal(f.journal.size,0)
})
test('build failure leaves tree intact, descends safely and cleans verified columns',async()=>{
 const f=fixture(),original=f.api.act
 f.api.act=async(name,a)=>{if(name==='scaffold_extend'&&a.y===4)throw new Error('placing scaffolding did not take');return original(name,a)}
 const result=await runTree(f.api,args,'harvest')
 assert.match(result.attention.join(' '),/did not take/)
 assert.equal(f.api.block(0,1,0).name,'oak_log')
 assert.equal(f.journal.size,0);assert.deepEqual(result.cleanup_left,[])
})
test('cancellation after server placement records exact provenance, acts no further, and recovers next pass',async()=>{
 const f=fixture(),original=f.api.act,cancel=new Error('cancelled')
 let stoppedAt=0
 f.api.act=async(name,a)=>{
  const result=await original(name,a)
  if(name==='scaffold_extend'&&a.y===3){stoppedAt=f.calls.length;throw cancel}
  return result
 }
 await assert.rejects(runTree(f.api,args,'harvest'),e=>e===cancel)
 assert.equal(f.calls.length,stoppedAt)
 const record=f.journal.get(scaffoldId(root))
 assert.equal(record.cells.length,3);assert.equal(record.verified.length,3)
 assert.equal(f.events.at(-1).name,'forestry_attention')
 f.api.act=original
 const recovered=await cleanupScaffold(f.api,scaffoldId(root),{attention:[]})
 assert.deepEqual(recovered.cleanup_left,[]);assert.equal(f.journal.size,0)
 assert.equal(f.api.block(0,1,0).name,'oak_log')
})
test('failed descent retains supports and reports cleanup coordinates instead of trapping body',async()=>{
 const f=fixture(),original=f.api.act
 let cut=false
 f.api.act=async(name,a)=>{
  if(name==='dig'&&a.x===0&&a.z===0)cut=true
  if(cut&&name==='goto'&&a.y===1)throw new Error('no first move')
  return original(name,a)
 }
 const result=await runTree(f.api,args,'harvest')
 assert.ok(result.cleanup_left.length>0);assert.equal(f.journal.size,1)
 assert.equal(f.calls.filter(c=>c.name==='dig'&&c.x!==0).length,0)
})
test('journal survives recreation and rejects path traversal IDs',()=>{
 const dir=fs.mkdtempSync(path.join(os.tmpdir(),'tree-scaffold-'))
 const file=path.join(dir,'scaffolds.json')
 const value={cells:[{x:1,y:2,z:3}],verified:['1,2,3']}
 scaffoldJournal(file)('tree_0_0_0',value)
 assert.deepEqual(scaffoldJournal(file)('tree_0_0_0'),value)
 assert.throws(()=>scaffoldJournal(file)('../escape',value),/invalid/)
 scaffoldJournal(file)('tree_0_0_0',null)
 assert.equal(scaffoldJournal(file)('tree_0_0_0'),null)
 fs.rmSync(dir,{recursive:true,force:true})
})

test('cleanup never collapses a newly attached unrecorded scaffold extension',async()=>{
 const f=fixture(),original=f.api.act,cancel=new Error('cancelled')
 f.api.act=async(name,a)=>{const result=await original(name,a);if(name==='scaffold_extend'&&a.y===3)throw cancel;return result}
 await assert.rejects(runTree(f.api,args,'harvest'),/cancelled/)
 const record=f.journal.get(scaffoldId(root)),column=record.columns[0]
 f.world.set(`${column.x+1},2,${column.z}`,{name:'scaffolding',solid:false,properties:{distance:1}})
 f.api.act=original
 const result=await cleanupScaffold(f.api,scaffoldId(root),{attention:[]})
 assert.match(result.attention.join(' '),/unrecorded scaffold connects/)
 assert.ok(result.cleanup_left.length>0)
 assert.equal(f.calls.some(c=>c.name==='dig'),false)
})

test('actual scaffold_extend clicks a horizontal base face and never clicks after cancellation',async()=>{
 const source=fs.readFileSync(new URL('../src/bot.mjs',import.meta.url),'utf8')
 const body=source.slice(source.indexOf('  async scaffold_extend (a) {'),source.indexOf('  async pillar_up (a) {'))
 const make=new Function('bot','vecOf','Vec3','isAir','refusalFor','digFromHere','cancelGuard','inventoryCounts','findItem',`let handPlacing=0;return ({${body}}).scaffold_extend`)
 class Vec{constructor(x,y,z){Object.assign(this,{x,y,z})}offset(x,y,z){return new Vec(this.x+x,this.y+y,this.z+z)}floored(){return new Vec(Math.floor(this.x),Math.floor(this.y),Math.floor(this.z))}}
 for(const cancelled of [false,true]){
  let count=3,placed=false,clicks=0
  const bot={entity:{position:new Vec(1.5,1,.5)},blockAt:p=>p.y===1?{name:'scaffolding',getProperties:()=>({distance:0})}:{name:placed&&p.y===2?'scaffolding':'air'},equip:async()=>{},setControlState:()=>{},waitForTicks:async()=>{},activateBlock:async(block,face)=>{clicks++;assert.equal(face.y,0);assert.equal(Math.abs(face.x)+Math.abs(face.z),1);placed=true;count--}}
  const fn=make(bot,a=>new Vec(a.x,a.y,a.z),Vec,n=>n==='air',()=>null,digFromHere,()=>()=>{if(cancelled)throw new Error('cancelled')},()=>({scaffolding:count}),()=>({name:'scaffolding'}))
  if(cancelled){await assert.rejects(fn({x:0,y:2,z:0,base_y:1}),/cancelled/);assert.equal(clicks,0)}
  else{assert.equal((await fn({x:0,y:2,z:0,base_y:1})).placed,1);assert.equal(clicks,1)}
 }
})

test('goto standing precheck accepts verified scaffold interior and top deck only with climb capability',async()=>{
 const {noStanding}=await import('../src/navigation/walk.mjs')
 const source=fs.readFileSync(new URL('../src/body/helpers.mjs',import.meta.url),'utf8')
 const body=source.slice(source.indexOf('export const cellAt ='),source.indexOf('export async function goNear'))
 const make=new Function('bot','Vec3','isWoodDoor','FLUIDS','breaksUnderfoot',body.replace('export const cellAt =','return '))
 class Vec{constructor(x,y,z){Object.assign(this,{x,y,z})}}
 for(const supported of [false,true]){
  const bot={pathfinder:{movements:{scaffoldingSupported:supported}},blockAt:p=>({name:p.y>=1&&p.y<=8?'scaffolding':p.y<1?'dirt':'air',boundingBox:p.y<=8?'block':'empty',getProperties:()=>({distance:0})})}
  const cellAt=make(bot,Vec,()=>false,new Set(),()=>false)
  if(supported){assert.equal(noStanding(cellAt,{x:0,y:4,z:0},0),null);assert.equal(noStanding(cellAt,{x:0,y:9,z:0},0),null)}
  else assert.match(noStanding(cellAt,{x:0,y:4,z:0},0),/nowhere to stand/)
 }
})

for(const offset of [-5,4])test(`scaffold access uses resolved tree ground offset ${offset} without applying it twice`,async()=>{
 const {planCells,migratePlan}=await import('../src/lib/plan.mjs')
 const plan=JSON.parse(JSON.stringify({x:0,y:-offset,z:0,plan:'T',legend:{T:{kind:'tree',species:'oak',ground_offset:offset}}}))
 const cell=planCells(migratePlan(plan))[0],f=fixture()
 assert.equal(cell.y,0)
 const result=await runTree(f.api,{x:cell.x,y:cell.y,z:cell.z,species:'oak'},'harvest')
 assert.equal(result.harvested,13)
 assert.equal(f.calls.find(c=>c.name==='place'&&c.item==='scaffolding').y,1)
 assert.deepEqual(result.cleanup_left,[])
})
test('scaffold work approaches verified exit and base before ascent and disables pit-rim substitution',async()=>{
 const {reachTreePlatform}=await import('../src/scaffold/access.mjs')
 const calls=[],report={attention:[]};let pos={x:8.5,y:1,z:.5}
 const column={x:0,y:1,z:0,top:8,exit:{x:1,y:1,z:0}},spot={x:0,y:9,z:0,scaffold:column}
 const api={pos:()=>pos,checkpoint:async()=>{},act:async(name,a)=>{calls.push(a);assert.equal(a.into,true);if(a.y>1)assert.equal(Math.floor(pos.x),0,'entered column before climbing');pos={x:a.x+.5,y:a.y,z:a.z+.5}}}
 assert.equal(await reachTreePlatform(api,[spot],report,[column]),spot)
 assert.deepEqual(calls.map(p=>[p.x,p.y,p.z]),[[1,1,0],[0,1,0],[0,9,0]])
})
test('switching scaffold columns descends recorded current exit first; recoverable alternative and cancellation bounded',async()=>{
 const {reachTreePlatform}=await import('../src/scaffold/access.mjs')
 const a={x:0,y:1,z:0,exit:{x:1,y:1,z:0}},b={x:5,y:1,z:0,exit:{x:6,y:1,z:0}}
 let pos={x:.5,y:8,z:.5};const calls=[],report={attention:[]}
 const api={pos:()=>pos,checkpoint:async()=>{},act:async(n,p)=>{calls.push(p);pos={x:p.x+.5,y:p.y,z:p.z+.5}}}
 await reachTreePlatform(api,[{x:5,y:8,z:0,scaffold:b}],report,[a,b]);assert.deepEqual([calls[0].x,calls[0].y,calls[0].z],[1,1,0])
 pos={x:20,y:1,z:0};api.act=async(n,p)=>{calls.push(p);if(p.x===2)throw Error('no walkable path');pos={x:p.x+.5,y:p.y,z:p.z+.5}}
 const spot=await reachTreePlatform(api,[{x:2,y:1,z:0},{x:1,y:1,z:0}],report)
 assert.equal(spot.x,1);assert.deepEqual(report.attention,[])
 api.checkpoint=async()=>{throw Error('cancelled')};await assert.rejects(()=>reachTreePlatform(api,[{x:4,y:1,z:0}],report),/cancelled/)
})
