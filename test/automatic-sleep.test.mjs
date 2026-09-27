import test from 'node:test'
import assert from 'node:assert/strict'
import { automaticBeds, bedChoice } from '../src/lib/sleep.mjs'
const bed={x:0,y:65,z:0},own={kind:'bed',by:'Observer',...bed}
test('automatic sleep requires positive ownership, not an unowned nearby bed',()=>{
 assert.deepEqual(automaticBeds([bed],[],[],'Observer'),[])
 assert.deepEqual(automaticBeds([bed],[],[own],'Observer'),[bed])
 assert.deepEqual(automaticBeds([bed],[],[{...own,by:'Other'}],'Observer'),[])
})
test('automatic sleep refuses occupied village beds even with an owned mark',()=>{
 assert.deepEqual(automaticBeds([bed],[],[own],'Observer',[{x:4,y:65,z:0}]),[])
 assert.deepEqual(automaticBeds([bed],[{name:'observer-village',x1:-2,x2:2,y1:60,y2:70,z1:-2,z2:2}],[own],'Observer'),[])
 assert.deepEqual(bedChoice([bed],[],'Observer'),{bed})
})

test('owned marks cannot override foreign protection; unmarked owned shelter remains usable',()=>{
 const zone={x1:-2,x2:2,y1:60,y2:70,z1:-2,z2:2}
 assert.deepEqual(automaticBeds([bed],[{...zone,name:'other-base'}],[own],'Observer'),[])
 assert.deepEqual(automaticBeds([bed],[{...zone,name:'observer-base'}],[],'Observer'),[bed])
})
