import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { migrateMapFile, resolveMapFile } from '../tools/migrate-plans.mjs'
import { withMapLock } from '../src/map-store.mjs'
import { planCells, planSpec } from '../src/lib/plan.mjs'
const ROOT = path.join(import.meta.dirname, '..')
const fixture = t => {
 const dir=fs.mkdtempSync(path.join(os.tmpdir(),'plan-migration-'));t.after(()=>fs.rmSync(dir,{recursive:true,force:true}))
 const file=path.join(dir,'places.json')
 const records=[{name:'plain',kind:'storage',x:3,y:4,z:5,note:'unchanged'},{name:'farm',kind:'farm',x:10,y:60,z:20,plan:'w~C\n.tF',owner:'farmer'},{name:'forest',kind:'forest',x:0,y:65,z:0,plan:'s',legend:{s:{kind:'tree',species:'birch',ground_offset:-2}}}]
 fs.writeFileSync(file,JSON.stringify(records));return{file,records}
}
test('dry run is nonmutating; atomic apply backs up exact original and preserves other records',t=>{
 const {file,records}=fixture(t), original=fs.readFileSync(file,'utf8'),dry=migrateMapFile(file)
 assert.equal(dry.migrated,2);assert.equal(fs.readFileSync(file,'utf8'),original)
 assert.ok(dry.plans.every(p=>p.equivalent))
 const applied=migrateMapFile(file,{apply:true,expected:dry.sha256}),next=JSON.parse(fs.readFileSync(file))
 assert.equal(applied.applied,true);assert.equal(fs.readFileSync(applied.backup,'utf8'),original)
 assert.deepEqual(next[0],records[0]);assert.equal(next[1].owner,'farmer');assert.equal(next[1].plan,undefined)
 assert.equal(planCells(next[2]).find(c=>planSpec(c).kind==='tree').y,63)
 assert.equal(migrateMapFile(file).migrated,0)
 assert.equal(fs.existsSync(`${file}.lock`),false)
})
test('changed digest refuses replacement without losing concurrent additions',t=>{
 const {file,records}=fixture(t),dry=migrateMapFile(file);records.push({name:'new marker',x:1,y:2,z:3})
 const changed=JSON.stringify(records);fs.writeFileSync(file,changed)
 assert.throws(()=>migrateMapFile(file,{apply:true,expected:dry.sha256}),/changed/)
 assert.equal(fs.readFileSync(file,'utf8'),changed)
})
test('exclusive map writer lock prevents migration and is released after error',t=>{
 const {file}=fixture(t),dry=migrateMapFile(file)
 withMapLock(file,()=>assert.throws(()=>migrateMapFile(file,{apply:true,expected:dry.sha256}),/lock|busy/i))
 assert.equal(fs.existsSync(`${file}.lock`),false)
 assert.equal(migrateMapFile(file,{apply:true,expected:dry.sha256}).applied,true)
})
test('invalid legacy map fails before any write or backup',t=>{
 const {file}=fixture(t);const original=JSON.stringify([{name:'invalid',plan:'?',x:0,y:0,z:0}]);fs.writeFileSync(file,original)
 assert.throws(()=>migrateMapFile(file),/unknown/);assert.equal(fs.readFileSync(file,'utf8'),original)
 assert.deepEqual(fs.readdirSync(path.dirname(file)),['places.json'])
})

// --file= names a map directly; otherwise --world <name> resolves state/worlds/<name>/places.json, the live
// path a world's bodies actually write
const resolveRows = [
  ['--file= wins even with no --world', ['--file=/tmp/x/places.json'], { file: path.resolve('/tmp/x/places.json') }],
  ['--world name resolves state/worlds/<name>/places.json', ['--world', 'main'], { file: path.join(ROOT, 'state', 'worlds', 'main', 'places.json') }],
  ['--world=name form', ['--world=test'], { file: path.join(ROOT, 'state', 'worlds', 'test', 'places.json') }]
]
for (const [name, args, expected] of resolveRows) {
  test(`resolveMapFile: ${name}`, () => assert.deepEqual(resolveMapFile(args, ROOT), expected))
}

test('resolveMapFile: neither --world nor --file= refuses', () =>
  assert.match(resolveMapFile([], ROOT).error, /--world/))
