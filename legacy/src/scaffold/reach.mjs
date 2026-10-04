import { isAir, isGroundCover } from '../lib/world.mjs'
import { digFromHere } from '../lib/dig.mjs'
const key=p=>`${p.x},${p.y},${p.z}`
// Conservative block ray: unknown blocks and even an owned scaffold intercept
// the click. A deck directly between the eye and the tree is not usable access.
export function traceVisibleTreeBlock(blockAt,feet,target,removed=new Set(),transparent=new Set()) {
 const eye={x:feet.x,y:feet.y+1.62,z:feet.z}
 const end={x:target.x+.5,y:target.y+.5,z:target.z+.5}
 if(!digFromHere(feet,target))return {visible:false,eye,aim:end,obstruction:null,tooFar:true}
 const length=Math.hypot(end.x-eye.x,end.y-eye.y,end.z-eye.z)
 let obstruction=null
 for(let d=.02;d<length;d+=.035){
  const p={x:Math.floor(eye.x+(end.x-eye.x)*d/length),y:Math.floor(eye.y+(end.y-eye.y)*d/length),z:Math.floor(eye.z+(end.z-eye.z)*d/length)}
  if(key(p)===key(target))return {visible:true,eye,aim:end,obstruction:null}
  if(removed.has(key(p))||transparent.has(key(p)))continue
  // The body can climb inside an open scaffold frame. Its eye cell does not
  // obstruct the outward click, but any intervening deck still does.
  if(key(p)===`${Math.floor(eye.x)},${Math.floor(eye.y)},${Math.floor(eye.z)}`&&blockAt(p.x,p.y,p.z)?.name==='scaffolding')continue
  const block=blockAt(p.x,p.y,p.z)
  if(!block||(!isAir(block.name)&&!isGroundCover(block.name))){obstruction={...p,name:block?.name??'unloaded'};break}
 }
 return {visible:false,eye,aim:end,obstruction}
}
export function visibleTreeBlock(blockAt,feet,target,removed=new Set(),transparent=new Set()) {
 return traceVisibleTreeBlock(blockAt,feet,target,removed,transparent).visible
}
// A planned stance must survive small pathfinder/controller offsets. Rays
// through a shared diagonal corner can pass the center sample yet hit a leaf
// after only a few centimeters of drift, so accept the center only with a
// small horizontal visibility margin. The live click still checks its exact
// current ray with visibleTreeBlock.
export function robustVisibleTreeBlock(blockAt,feet,target,removed=new Set(),margin=.1,transparent=new Set()) {
 return [[0,0],[margin,0],[-margin,0],[0,margin],[0,-margin]].every(([dx,dz])=>visibleTreeBlock(blockAt,{...feet,x:feet.x+dx,z:feet.z+dz},target,removed,transparent))
}
export function treeHarvestSequence(tree,access,blockAt){
 const left=[...tree.blocks].sort((a,b)=>b.y-a.y||a.x-b.x||a.z-b.z),removed=new Set(),transparent=new Set((tree.leaves??[]).map(key)),steps=[]
 while(left.length){
  const index=left.findIndex(b=>(access.get(key(b))??[]).some(p=>robustVisibleTreeBlock(blockAt,{x:p.x+.5,y:p.y,z:p.z+.5},b,removed,.1,transparent)))
  if(index<0)return {steps,missing:left}
  const [block]=left.splice(index,1)
  const spots=(access.get(key(block))??[]).filter(p=>robustVisibleTreeBlock(blockAt,{x:p.x+.5,y:p.y,z:p.z+.5},block,removed,.1,transparent))
  steps.push({block,spots});removed.add(key(block))
 }
 return {steps,missing:[]}
}
