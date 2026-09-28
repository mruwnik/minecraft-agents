import test from 'node:test'
import assert from 'node:assert/strict'
import { observedTreeProgress } from '../src/tree/actions.mjs'

test('tree progress recounts original wood from the world and excludes owned pillar support',()=>{
 const blocks=Array.from({length:33},(_,x)=>({x,y:70,z:0,name:'dark_oak_log'}))
 const world=new Map(blocks.map(p=>[`${p.x},${p.y},${p.z}`,{name:'dark_oak_log'}]))
 for(let x=0;x<12;x++)world.set(`${x},70,0`,{name:'air'})
 const support={x:12,y:70,z:0,item:'dark_oak_log'}
 const progress=observedTreeProgress(blocks,(x,y,z)=>world.get(`${x},${y},${z}`),{cells:[support]})
 assert.equal(progress.harvested,13)
 assert.equal(progress.remaining.length,20)
 assert.deepEqual(progress.remaining[0],{x:13,y:70,z:0,name:'dark_oak_log'})
})

test('tree progress keeps unexpected replacement blocks unresolved instead of counting them harvested',()=>{
 const blocks=[{x:1,y:70,z:0,name:'oak_log'}]
 const progress=observedTreeProgress(blocks,()=>({name:'stone'}))
 assert.equal(progress.harvested,0)
 assert.deepEqual(progress.remaining,blocks)
})
