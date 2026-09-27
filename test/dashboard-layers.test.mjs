import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import vm from 'node:vm'
import { parsePlacePlan, planLevels, layerDiff, cellColour, cellLabel, cellWorldBlock, mapPoints, planRects } from '../tools/dashboard/map.mjs'
import { planCells, planSpec } from '../src/lib/plan.mjs'
const plans=JSON.parse(fs.readFileSync(new URL('./fixtures/dashboard-layered-plans.json',import.meta.url)))
test('all 15 real migrated farm, forest and pen plans expose every cell exactly once across dashboard elevations',()=>{
 assert.equal(plans.length,15)
 for(const place of plans){
  assert.ok(!parsePlacePlan(place).error,place.name)
  const levels=planLevels(place),all=levels.flatMap(y=>layerDiff(place,[],y).cells)
  assert.equal(all.length,planCells(place).length,place.name)
  assert.equal(new Set(all.map(c=>`${c.x},${c.y},${c.z}`)).size,all.length)
  assert.equal(planRects([place]).length,1)
  for(const c of all){assert.notEqual(cellLabel(c.ch,c.spec),'unknown');assert.match(cellColour(c.ch,c.spec),/^#[a-f0-9]{6}$/)}
 }
})
test('private-use migrated path tokens retain semantic path colors rather than unknown grey',()=>{
 const cell=plans.flatMap(planCells).find(c=>c.ch.codePointAt(0)>=0xe000 && planSpec(c).kind==='path')
 assert.ok(cell);assert.equal(cellColour(cell.ch,cell.spec),cellColour('.'));assert.equal(cellLabel(cell.ch,cell.spec),'dirt')
})
test('actual block slices keep stacked crop species, custom overrides and hover coordinates separate',()=>{
 const place={x:8,y:60,z:9,structure:{legend:{w:'carrots',u:'wheat',g:'farmland'},layers:[{y:0,rows:['g']},{y:1,rows:['w']},{y:4,rows:['g']},{y:5,rows:['u']}]}}
 const world=[{x:8,y:60,z:9,name:'farmland'},{x:8,y:61,z:9,name:'carrots'},{x:8,y:64,z:9,name:'farmland'},{x:8,y:65,z:9,name:'wheat'}]
 assert.deepEqual(planLevels(place),[60,61,64,65])
 const low=layerDiff(place,world,61).cells[0],high=layerDiff(place,world,65).cells[0]
 assert.equal(low.ok,true);assert.equal(high.ok,true);assert.equal(low.y,61);assert.equal(high.y,65)
 assert.equal(cellLabel(low.ch,low.spec),'carrots');assert.equal(cellColour(low.ch,low.spec),cellColour('c'))
 assert.equal(cellWorldBlock(layerDiff(place,world,60).cells[0]),'farmland')
 assert.equal(cellWorldBlock(low),'carrots')
 const points=mapPoints([], [place], [], []);assert.ok(points.some(p=>p.x===9&&p.z===10))
})
test('browser popup opens canonical plan, offers all elevations, redraws and hovers the selected layer',()=>{
 const html=fs.readFileSync(new URL('../tools/dashboard/index.html',import.meta.url),'utf8')
 const start=html.indexOf('const CELL_PX = 22'),end=html.indexOf("el('planCanvas').addEventListener('mouseleave'",start)
 const nodes=new Map(),node=id=>{if(!nodes.has(id))nodes.set(id,{textContent:'',style:{},dataset:{},listeners:{},children:[],addEventListener(type,fn){this.listeners[type]=fn},replaceChildren(...v){this.children=v},append(){},querySelector(){return{classList:{toggle(){}}}},getBoundingClientRect(){return{left:0,top:0}},getContext(){return new Proxy({},{get:()=>()=>{}})}});return nodes.get(id)}
 const context={parsePlacePlan,planLevels,layerDiff,cellColour,cellLabel,cellWorldBlock,planDiff:()=>{},cellExpectation:()=>'',worldColour:()=> '#000000',worldLabel:()=>'',el:node,tag:()=>node('tag'),window:{devicePixelRatio:1},document:{createElement:()=>({})},localStorage:{getItem:()=>null,setItem(){}},clearInterval(){},setInterval(){},snap:{places:plans},fetch(){throw Error('plan view must not fetch')}}
 vm.createContext(context)
 const script=html.slice(start,end>start?end:html.indexOf("el('planClose')",start))
 vm.runInContext(script+'\nglobalThis.openTest=showPlan; globalThis.currentCells=()=>diffOf().cells;',context)
 for(const place of plans){context.openTest(place);assert.equal(node('planOverlay').hidden,false);assert.equal(node('planLayer').children.length,planLevels(place).length+1);assert.equal(node('planLayer').value,'birds-eye');assert.equal(node('planCanvas').width,parsePlacePlan(place).width*22)}
 context.openTest(plans[0]);assert.equal(node('planOverlay').hidden,false)
 assert.equal(node('planLayer').children.length,planLevels(plans[0]).length+1)
 const level=planLevels(plans[0]).at(-1);node('planLayer').listeners.change({target:{value:String(level)}})
 assert.ok(context.currentCells().every(c=>c.y===level))
 node('planLayer').listeners.change({target:{value:'birds-eye'}})
 assert.equal(context.currentCells().length,new Set(planCells(plans[0]).map(c=>`${c.x},${c.z}`)).size)
 node('planCanvas').listeners.mousemove({clientX:0,clientY:0})
 assert.ok(node('planMeta').textContent.includes('highest planned block'))
})

test('bird’s-eye projection combines elevations and reveals blocks beneath air and preserve markers',()=>{
 const place={x:0,y:60,z:0,structure:{legend:{w:'wheat',c:'carrots','~':'water',r:{require:'preserve'}},layers:[{y:5,rows:['c__']},{y:9,rows:['.r_']},{y:0,rows:['__~']},{y:1,rows:['ww_']}]}}
 const world=[{x:0,y:64,z:0,name:'farmland'},{x:0,y:65,z:0,name:'carrots'},{x:1,y:60,z:0,name:'farmland'},{x:1,y:61,z:0,name:'wheat'},{x:2,y:60,z:0,name:'water'},{x:2,y:61,z:0,name:'air'}]
 const overview=layerDiff(place,world)
 assert.deepEqual(overview.cells.map(c=>[c.x,c.y,c.spec.crop??c.spec.kind]).sort((a,b)=>a[0]-b[0]),[[0,65,'carrots'],[1,61,'wheat'],[2,60,'water']])
 assert.equal(overview.total,3);assert.equal(overview.differ,0);assert.equal(overview.unseen,0)
 assert.equal(layerDiff(place,world,61).cells.length,2)
 assert.equal(layerDiff(place,world,69).cells.length,2)
})
