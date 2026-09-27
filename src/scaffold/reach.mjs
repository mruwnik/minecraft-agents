import { isAir, isGroundCover } from '../lib/world.mjs'
import { digFromHere } from '../lib/dig.mjs'
const key=p=>`${p.x},${p.y},${p.z}`
// Conservative block ray: unknown blocks and even an owned scaffold intercept
// the click. A deck directly between the eye and the tree is not usable access.
export function visibleTreeBlock(blockAt,feet,target,removed=new Set()) {
 if(!digFromHere(feet,target))return false
 const eye={x:feet.x,y:feet.y+1.62,z:feet.z}
 const end={x:target.x+.5,y:target.y+.5,z:target.z+.5}
 const length=Math.hypot(end.x-eye.x,end.y-eye.y,end.z-eye.z)
 for(let d=.05;d<length;d+=.08){
  const p={x:Math.floor(eye.x+(end.x-eye.x)*d/length),y:Math.floor(eye.y+(end.y-eye.y)*d/length),z:Math.floor(eye.z+(end.z-eye.z)*d/length)}
  if(key(p)===key(target))return true
  if(removed.has(key(p)))continue
  // The body can climb inside an open scaffold frame. Its eye cell does not
  // obstruct the outward click, but any intervening deck still does.
  if(key(p)===`${Math.floor(eye.x)},${Math.floor(eye.y)},${Math.floor(eye.z)}`&&blockAt(p.x,p.y,p.z)?.name==='scaffolding')continue
  const b=blockAt(p.x,p.y,p.z)
  if(!b||(!isAir(b.name)&&!isGroundCover(b.name)))return false
 }
 return true
}
export function treeHarvestSequence(tree,access,blockAt){
 const left=[...tree.blocks].sort((a,b)=>b.y-a.y||a.x-b.x||a.z-b.z),removed=new Set(),steps=[]
 while(left.length){
  const index=left.findIndex(b=>(access.get(key(b))??[]).some(p=>visibleTreeBlock(blockAt,{x:p.x+.5,y:p.y,z:p.z+.5},b,removed)))
  if(index<0)return {steps,missing:left}
  const [block]=left.splice(index,1)
  const spots=(access.get(key(block))??[]).filter(p=>visibleTreeBlock(blockAt,{x:p.x+.5,y:p.y,z:p.z+.5},block,removed))
  steps.push({block,spots});removed.add(key(block))
 }
 return {steps,missing:[]}
}
