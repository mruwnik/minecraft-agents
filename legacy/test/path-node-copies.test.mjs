import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { readFileSync } from 'node:fs'
import { patchPathNodeCopies } from '../src/navigation/terrain.mjs'
const require=createRequire(import.meta.url)
const Move=require('mineflayer-pathfinder/lib/move')
const installed=readFileSync(require.resolve('mineflayer-pathfinder'),'utf8')
const marker='    // copy search nodes before rendering execution waypoints\n'
const start=installed.indexOf(marker)
const baseline=start<0?installed:installed.slice(0,start)+installed.slice(installed.indexOf('    for (let i = 0; i < path.length; i++)',start))
function renderer(source) {
  const body=source.slice(source.indexOf('  function postProcessPath (path) {'),source.indexOf('  function pathFromPlayer'))
  return new Function('stateMovements','bot',body+'\nreturn postProcessPath')({
    resolveTerrainWaypoint:p=>({x:p.x+0.5,y:p.y,z:p.z+0.5})
  },{pathfinder:{enablePathShortcut:false}})
}
test('real native rendering previously changed A* nodes; patched rendering keeps repeated partial paths stable',()=>{
  const original=new Move(0,1,0,0,1)
  renderer(baseline)([original])
  assert.equal(original.x,0.5,'demonstrates the upstream aliasing defect')
  const patched=renderer(patchPathNodeCopies(baseline).source), node=new Move(0,1,0,0,1)
  const first=patched([node]),second=patched([node])
  assert.equal(node.x,0)
  assert.equal(node.hash,'0,1,0')
  assert.equal(first[0].x,0.5)
  assert.equal(second[0].x,0.5)
  assert.equal(first[0] instanceof Move,true)
  assert.equal(typeof first[0].distanceSquared,'function')
})
test('consuming execution interactions does not erase the unfinished search node actions',()=>{
  const node=new Move(0,1,0,0,1,[{x:0,y:2,z:0}],[{x:1,y:1,z:0,useOne:true}])
  const [rendered]=renderer(patchPathNodeCopies(baseline).source)([node])
  rendered.toBreak.shift();rendered.toPlace[0].x=9;rendered.toPlace.shift()
  assert.equal(node.toBreak.length,1)
  assert.equal(node.toPlace.length,1)
  assert.equal(node.toPlace[0].x,1)
})
test('search-node patch is idempotent and leaves incompatible dependency versions untouched',()=>{
  const first=patchPathNodeCopies(baseline)
  assert.equal(first.status,'patched')
  assert.deepEqual(patchPathNodeCopies(first.source),{status:'already patched',source:first.source})
  assert.deepEqual(patchPathNodeCopies('unrecognized'),{status:'anchor missing',source:'unrecognized'})
})
