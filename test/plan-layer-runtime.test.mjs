import test from 'node:test'
import assert from 'node:assert/strict'
import { fieldCrops } from '../src/farm/field.mjs'
import { unsureWater } from '../tools/dashboard/lib.mjs'
import { planCells } from '../src/lib/plan.mjs'
test('layered stalk harvest selects second segments on both storeys without crossing neighboring heights',()=>{
 const cells=[{x:0,y:10,z:0},{x:0,y:20,z:0}]
 const seen=[11,12,13,21,22,23].map(y=>({x:0,y,z:0}))
 assert.deepEqual(fieldCrops(cells,seen,2).map(c=>c.y),[12,22])
 assert.deepEqual(fieldCrops(cells,seen).map(c=>c.y),[11,21])
})
test('dashboard waterlogged verification distinguishes vertically stacked channel blocks',()=>{
 const place={x:0,y:10,z:0,structure:{legend:{w:'water'},layers:[{y:0,rows:['w']},{y:4,rows:['w']}]}}
 const world=[{x:0,y:10,z:0,name:'water'},{x:0,y:14,z:0,name:'oak_slab'}]
 assert.deepEqual(unsureWater(place,world),[{x:0,y:14,z:0}])
})
test('ordinary markers have no cells while legacy runtime plans require explicit import',()=>{
 assert.deepEqual(planCells({name:'marker',x:0,y:0,z:0}),[])
 assert.throws(()=>planCells({plan:'w',x:0,y:0,z:0}),/legacy 2D/)
})
