import { isAir } from '../lib/world.mjs'
import { digFromHere } from '../lib/dig.mjs'

// Java uses the player's horizontal facing when the top of scaffolding is
// clicked without sneaking. Preserve that facing through the placement packet.
export async function scaffoldSide(a, {bot, Vec3, refusalFor, cancelGuard, inventoryCounts, findItem}) {
  if (![a.x,a.y,a.z,a.from_x,a.from_y,a.from_z].every(Number.isInteger) || a.y!==a.from_y || Math.abs(a.x-a.from_x)+Math.abs(a.z-a.from_z)!==1) throw Error('scaffold_side needs integer adjacent same-height target and from_x/from_y/from_z')
  const target=new Vec3(a.x,a.y,a.z), from=new Vec3(a.from_x,a.from_y,a.from_z)
  const validate=()=>{
    const support=bot.blockAt(from),distance=Number(support?.getProperties?.().distance)
    if(support?.name!=='scaffolding'||!Number.isInteger(distance)||distance<0||distance>=6)throw Error('cannot place scaffold_side: requires verified scaffolding support distance below 6')
    if(![0,1,2].every(dy=>isAir(bot.blockAt(target.offset(0,dy,0))?.name)))throw Error('cannot place scaffold_side: requires clear loaded target and headroom')
    const refusal=refusalFor('place',{x:a.x,y:a.y,z:a.z,item:'scaffolding'});if(refusal)throw Error(refusal)
    if(!digFromHere(bot.entity.position,from))throw Error('scaffold_side: out of reach; stand beside its support')
    return support
  }
  validate();const alive=cancelGuard(),before=inventoryCounts().scaffolding??0
  if(!before)throw Error('no scaffolding carried')
  await bot.equip(findItem('scaffolding'),'hand');alive();validate()
  bot.setControlState('sneak',false)
  await bot.look(Math.atan2(-(a.x-a.from_x),-(a.z-a.from_z)),-Math.PI/3,true)
  alive();const support=validate()
  await bot._genericPlace(support,new Vec3(0,1,0),{forceLook:'ignore',swingArm:'right'})
  await bot.waitForTicks(5);alive()
  const actual=bot.blockAt(target),distance=Number(actual?.getProperties?.().distance)
  if(actual?.name!=='scaffolding'||!Number.isInteger(distance)||distance<0||distance>6||(inventoryCounts().scaffolding??0)>=before)throw Error(`placing scaffolding did not take at ${a.x},${a.y},${a.z}`)
  return {placed:1,at:`${a.x},${a.y},${a.z}`}
}
