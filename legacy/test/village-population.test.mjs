import test from 'node:test'
import assert from 'node:assert/strict'
import { validatePopulation,populationReport } from '../src/villager/population.mjs'
import { validateBlueprintDocument } from '../src/blueprint/schema.mjs'
import fs from 'node:fs'
const now=Date.now(),stamp=new Date(now).toISOString()
const record=(uuid,profession,offers=[])=>({uuid,profession,age:'adult',lastSeenAt:stamp,lastPosition:{x:1,y:65,z:1},lockEvidence:{profession,basis:'inventory-confirmed villager trade; profession is trade-locked'},offers:{observedAt:stamp,profession,items:offers}})
const offer=(enchant,level)=>({outputItem:{name:'enchanted_book',enchants:[{name:enchant,level}]}})
test('canonical blueprint accepts population intent and rejects unsupported removal/ambiguous roles',()=>{
 const doc=JSON.parse(fs.readFileSync(new URL('../../blueprints/villager-house-10.blueprint.json',import.meta.url)))
 doc.population={target:5,roles:[{id:'farmers',count:2,profession:'farmer'}]}
 assert.deepEqual(validateBlueprintDocument(doc).population,doc.population)
 assert.throws(()=>validatePopulation({...doc.population,surplus:'evict'}),/surplus/)
 assert.throws(()=>validatePopulation({target:1,roles:[{id:'farmers',count:2,profession:'farmer'}]}),/exceeds target/)
})
test('distinct matching avoids double counting and uses augmenting matches rather than greedy roles',()=>{
 const intent={target:2,roles:[{id:'any',profession:'librarian',trade:{output:'enchanted_book',enchant:'efficiency'}},{id:'exact',profession:'librarian',trade:{output:'enchanted_book',enchant:'efficiency',level:3}}]}
 const a=record('a','librarian',[offer('efficiency',3)]),b=record('b','librarian',[offer('efficiency',4)])
 assert.equal(populationReport(intent,[a,b],{now}).satisfied,true)
 assert.equal(populationReport(intent,[a,a],{now}).roles.filter(r=>r.status==='satisfied').length,1)
 assert.equal(populationReport(intent,[a,b,record('c','farmer')],{now}).surplus,1)
})
test('unknown/stale is not absence; matching requires actual inventory-confirmed locks and fresh offers',()=>{
 const intent={target:1,roles:[{id:'arrow',profession:'fletcher',trade:{output:'arrow'}}]},a=record('a','fletcher',[{outputItem:{name:'arrow'},tradeDisabled:true}])
 assert.equal(populationReport(intent,[a],{now}).satisfied,true)
 delete a.lockEvidence
 assert.equal(populationReport(intent,[a],{now}).satisfied,false)
 a.lastSeenAt=new Date(now-999999).toISOString()
 assert.equal(populationReport(intent,[a],{now}).populationStatus,'unknown')
 assert.equal(populationReport(intent,[a],{now}).known,1)
})
test('exact versus minimum enchantment levels remain distinct',()=>{
 const r=record('a','librarian',[offer('efficiency',4)])
 for(const [atLeast,expected] of [[false,false],[true,true]])assert.equal(populationReport({target:1,roles:[{id:'e',profession:'librarian',trade:{output:'enchanted_book',enchant:'efficiency',level:3,atLeast}}]},[r],{now}).satisfied,expected)
})
test('future-dated merchant evidence remains unknown instead of satisfying a trade role',()=>{
 const r=record('a','fletcher',[{outputItem:{name:'arrow'}}]);r.offers.observedAt=new Date(now+1000).toISOString()
 const report=populationReport({target:1,roles:[{id:'arrow',profession:'fletcher',trade:{output:'arrow'}}]},[r],{now})
 assert.equal(report.satisfied,false);assert.equal(report.roles[0].status,'unknown')
})
