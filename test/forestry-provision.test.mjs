import test from 'node:test'
import assert from 'node:assert/strict'
import { treeSupplies, provisionTreeBasics, provisionTreeScaffold } from '../src/tree/provision.mjs'
import { maintainTreeServices } from '../src/tree/services.mjs'
const tree={x:0,y:0,z:0,spec:{kind:'tree',species:'oak',form:'single'}},chest={x:8,y:0,z:0,spec:{kind:'chest',item:'chest'}},table={x:9,y:0,z:0,spec:{kind:'table',item:'crafting_table'}}
function fixture(stock={},items={}){
 const calls=[],world=new Map([['8,1,0','chest']]),report={attention:[]};let pos={x:8.5,y:1,z:1.5}
 const api={inv:()=>items,pos:()=>pos,me:()=> 'tester',places:()=>[],checkpoint:async()=>{},navigationCapabilities:()=>({scaffolding:true}),block:(x,y,z)=>({name:world.get(`${x},${y},${z}`)??(y<=0?'dirt':'air'),solid:(world.get(`${x},${y},${z}`)??(y<=0?'dirt':'air'))!=='air',properties:{}}),act:async(name,a)=>{
 calls.push({name,...a});if(name==='zones')return{zones:[]};if(name==='chest_contents')return{items:{...stock}}
 if(name==='withdraw')for(const[n,v]of Object.entries(a.items)){assert.ok(v<=stock[n]);stock[n]-=v;items[n]=(items[n]??0)+v}
 if(name==='goto')pos={x:a.x+.5,y:a.y,z:a.z+.5}
 if(name==='place'){assert.ok(items[a.item]>0);items[a.item]--;world.set(`${a.x},${a.y},${a.z}`,a.item)}
 if(name==='craft'){
 if(a.item==='crafting_table'){assert.ok(items.oak_planks>=4);items.oak_planks-=4}
 if(a.item==='oak_planks'){assert.ok(items.oak_log>=1);items.oak_log--}
 if(a.item==='scaffolding'){assert.ok(items.bamboo>=a.count);assert.ok(items.string>=a.count/6);items.bamboo-=a.count;items.string-=a.count/6}
 items[a.item]=(items[a.item]??0)+a.count
 }return{}
 }}
 return{api,calls,world,report,items,plan:{cells:[tree,chest,table]}}
}
test('only planned chest supplies complete large-tree footprint and needed flower; remainder stays stored',async()=>{
 const f=fixture({dark_oak_sapling:20,dandelion:9,diamond:64},{dark_oak_sapling:1})
 f.plan.cells=[{...tree,spec:{kind:'tree',species:'dark_oak',form:'large'}},chest,{x:2,y:0,z:0,spec:{kind:'flower',item:'dandelion'}}]
 const supply=await treeSupplies(f.api,f.plan,undefined,f.report);await provisionTreeBasics(f.api,f.plan,supply,f.report)
 assert.equal(f.items.dark_oak_sapling,4);assert.equal(f.items.dandelion,1);assert.equal(f.items.diamond,undefined)
 assert.deepEqual(f.calls.filter(c=>c.name==='chest_contents').map(c=>[c.x,c.y,c.z]),[[8,1,0]])
})
test('configured source=false reads no storage and never invents gains from a failed withdrawal',async()=>{
 const f=fixture({oak_sapling:9});const off=await treeSupplies(f.api,f.plan,false,f.report);await off.take({oak_sapling:1});assert.equal(f.calls.length,0)
 const original=f.api.act;f.api.act=(name,a)=>name==='withdraw'?Promise.resolve({}):original(name,a)
 const supply=await treeSupplies(f.api,f.plan,undefined,f.report);await supply.take({oak_sapling:1});assert.equal(f.items.oak_sapling,undefined)
})
test('available plank supply crafts and provisions only the planned table',async()=>{
 const f=fixture({oak_planks:24});const supply=await treeSupplies(f.api,f.plan,undefined,f.report)
 await provisionTreeBasics(f.api,f.plan,supply,f.report);await maintainTreeServices(f.api,f.plan.cells,f.report)
 assert.equal(f.world.get('9,1,0'),'crafting_table');assert.equal(f.items.oak_planks,0)
 assert.equal(f.calls.filter(c=>c.name==='craft'&&c.item==='crafting_table').length,1)
})
function tall(f){for(let y=1;y<=12;y++)f.world.set(`0,${y},0`,'oak_log');f.world.set('0,13,0','oak_leaves');f.world.set('9,1,0','crafting_table')}
test('preflight scaffold shortage sources recipe amounts and crafts bounded batches at planned table',async()=>{
 const f=fixture({bamboo:128,string:32});tall(f)
 const supply=await treeSupplies(f.api,f.plan,undefined,f.report);await provisionTreeScaffold(f.api,f.plan,tree,supply,f.report)
 const craft=f.calls.find(c=>c.name==='craft'&&c.item==='scaffolding');assert.ok(craft);assert.equal(craft.count%6,0);assert.ok(craft.count<=256+5)
 const withdraw=f.calls.find(c=>c.name==='withdraw');assert.deepEqual(withdraw.items,{bamboo:craft.count,string:craft.count/6})
 assert.equal(f.items.scaffolding,craft.count);assert.ok(!f.calls.some(c=>c.name==='dig'))
})
test('missing bamboo/string reports exact remaining recipe needs and leaves tree intact',async()=>{
 const f=fixture();tall(f);const supply=await treeSupplies(f.api,f.plan,undefined,f.report)
 await provisionTreeScaffold(f.api,f.plan,tree,supply,f.report)
 assert.match(f.report.attention.join(' '),/need \d+ scaffolding: missing \d+ bamboo and \d+ string to craft \d+/)
 assert.ok(!f.calls.some(c=>['dig','craft'].includes(c.name)))
})
test('cancellation propagates before withdrawal or craft',async()=>{
 const f=fixture({oak_sapling:8});const supply=await treeSupplies(f.api,f.plan,undefined,f.report)
 f.api.checkpoint=async()=>{throw Error('cancelled')}
 await assert.rejects(()=>supply.take({oak_sapling:1}),/cancelled/);assert.ok(!f.calls.some(c=>c.name==='withdraw'))
})
test('scaffolding batch assumption matches installed recipe registry',async()=>{
 const {createRequire}=await import('node:module'),require=createRequire(import.meta.url),data=require('minecraft-data')('1.21.5')
 const recipe=data.recipes[data.itemsByName.scaffolding.id][0],ingredients=recipe.inShape.flat().filter(n=>n!==null)
 assert.equal(recipe.result.count,6);assert.equal(ingredients.filter(id=>id===data.itemsByName.bamboo.id).length,6);assert.equal(ingredients.filter(id=>id===data.itemsByName.string.id).length,1)
})
test('all missing service supports source dirt, independently of flower support',async()=>{
 const f=fixture({dirt:9});f.plan.cells=[chest,{...table,x:9},{x:10,y:0,z:0,spec:{kind:'flower',item:'dandelion'}},{x:11,y:0,z:0,spec:{kind:'torch',item:'oak_fence'}}]
 for(const x of[9,10,11])f.world.set(`${x},0,0`,'air')
 const supply=await treeSupplies(f.api,f.plan,undefined,f.report);await provisionTreeBasics(f.api,f.plan,supply,f.report)
 assert.equal(f.items.dirt,3)
})
test('known partial scaffold craft reports actual gain and continues; cancellation still propagates',async()=>{
 const f=fixture(),supply=await treeSupplies(f.api,f.plan,false,f.report)
 f.api.act=async()=>{f.items.scaffolding=18;throw Error('forestry.maintain/craft: the server kept rejecting the craft: only 18 of 36 scaffolding made, and bamboo:18 string:3 went into them')}
 assert.equal(await supply.craft('scaffolding',36),18);assert.match(f.report.attention.join(' '),/18\/36/)
 f.api.act=async()=>{throw Error('cancelled')};await assert.rejects(()=>supply.craft('scaffolding',6),/cancelled/)
})
