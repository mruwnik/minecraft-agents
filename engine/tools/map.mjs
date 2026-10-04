#!/usr/bin/env node
import path from 'node:path'
import {fileURLToPath} from 'node:url'
import {parseArgs} from 'node:util'
import {defaultStateDir} from './drive-lib.mjs'
import {readEDN,writeEDN,keyword} from './observe-lib.mjs'
import {context,readDocument,listDocuments,mutateDocument,query,boxOf,name,fail,rawBound} from './world-data.mjs'
import {mapRefusal,markFields} from '../../src/lib/places.mjs'
export const usage='map.mjs --world WORLD find|show|add|edit|remove|renew|release [marker|zone|claim] [ID] [--data EDN --revision REV --by ACTOR --for 30m --dry-run --raw --center EDN --place ID --radius 128 --type TYPE --owner OWNER --status STATUS --text TEXT --limit 10 --offset 0 --state DIR]'
export function ttl(text='30m'){const m=/^(\d+(?:\.\d+)?)(ms|s|m|h|d)?$/.exec(text);const n=m?Number(m[1])*({ms:1,s:1000,m:60000,h:3600000,d:86400000}[m[2]??'s']):NaN;if(!Number.isFinite(n)||n<100||n>604800000)throw fail('invalid-duration','claim duration must be100ms..7d');return n}
export function options(argv){const p=parseArgs({args:argv,allowPositionals:true,options:{world:{type:'string'},state:{type:'string',default:defaultStateDir},'repo-root':{type:'string'},data:{type:'string'},revision:{type:'string'},'if-revision':{type:'string'},by:{type:'string'},for:{type:'string'},'dry-run':{type:'boolean'},preview:{type:'boolean'},raw:{type:'boolean'},center:{type:'string'},place:{type:'string'},radius:{type:'string'},type:{type:'string'},owner:{type:'string'},status:{type:'string'},text:{type:'string'},limit:{type:'string'},offset:{type:'string'}}});const [command='find',kind,id,...extra]=p.positionals,v=p.values;if(v['if-revision']){if(v.revision)throw fail('invalid-option','choose --if-revision or --revision');v.revision=v['if-revision'];delete v['if-revision']}
  if(extra.length||!['find','show','add','edit','remove','renew','release'].includes(command))throw fail('invalid-command',usage)
  if(command==='find'?(kind!==undefined):(id===undefined||!['marker','zone','claim'].includes(kind)))throw fail('invalid-command',usage)
  if(['renew','release'].includes(command)&&kind!=='claim')throw fail('invalid-command','renew/release apply only to claims')
  const allowed={find:['center','place','radius','type','owner','status','text','limit','offset','raw'],show:['raw'],add:['data','by','for','dry-run','preview','raw'],edit:['data','revision','by','for','dry-run','preview','raw'],remove:['revision','by','dry-run','preview','raw'],renew:['revision','by','for','dry-run','preview','raw'],release:['revision','by','dry-run','preview','raw']}[command]
  for(const key of Object.keys(v))if(!['world','state','repo-root',...allowed].includes(key))throw fail('invalid-option',`--${key} does not apply to ${command}`)
  if(v.for&&kind!=='claim')throw fail('invalid-option','--for applies only to claims')
  const ctx=context({state:v.state,world:v.world,repoRoot:v['repo-root']})
  if(!['find','show'].includes(command)&&(!v.by||!v.by.trim()))throw fail('actor-required','mutations require --by')
  if(!['find','show','add'].includes(command)&&!/^[0-9a-f]{24}$/.test(v.revision??''))throw fail('revision-required','give --revision from show/find')
  return {command,kind,id,ctx,values:v}
}
export function filters(ctx,v){if(v.center&&v.place)throw fail('invalid-center','choose --center or --place')
  let center=v.center?readEDN(v.center):undefined
  if(v.place){const place=readDocument(ctx,'marker',v.place);if(!place)throw fail('place-not-found','no such marker');center=[place.value.x,place.value.y,place.value.z]}
  if(v.radius&&!center)throw fail('invalid-center','radius requires explicit center or place')
  return {center,radius:Number(v.radius??128),type:v.type,owner:v.owner,status:v.status,text:v.text,limit:Number(v.limit??10),offset:Number(v.offset??0)}
}
export function summary(d){const v=d.value;return {kind:keyword(d.kind),id:d.id,revision:d.revision,scope:keyword(d.scope),source:keyword(d.kind==='marker'?(v.source??'unknown'):'intent'),...(v.owner??v.by?{owner:v.owner??v.by}:{}),...(v.kind?{type:keyword(name(v.kind))}:{}),...(boxOf(v)?{box:boxOf(v)}:{}),...(v.status?{status:keyword(name(v.status)==='active'&&v.until&&v.until<=Date.now()?'expired':name(v.status))}:{}),...(v.until?{until:v.until}:{}),...(v.note?{note:v.note.slice(0,240),...(v.note.length>240?{'note-truncated?':true}:{})}:{}),...(v.structure?{'has-structure?':true}:{}),...(Number.isFinite(d.distance)?{distance:Math.round(d.distance)}:{})}}
function preflightRawMutation(record,kind,id,op){rawBound({ok:true,kind:keyword(kind),id,revision:'0'.repeat(24),op:keyword(op),cursor:{stream:'0'.repeat(36),seq:Number.MAX_SAFE_INTEGER},record})}
export function validateZone(v){const keys=['name','min','max','owner','allow','note'];if(!v||Object.keys(v).some(k=>!keys.includes(k)))throw fail('validation','zone contains unsupported keys')
  if(typeof v.name!=='string'||!v.name.trim()||typeof v.owner!=='string'||!v.owner.trim())throw fail('validation','zone needs name and owner')
  if(![v.min,v.max].every(c=>Array.isArray(c)&&c.length===3&&c.every(Number.isInteger))||v.min.some((n,i)=>n>v.max[i]))throw fail('validation','zone needs ordered integer min/max corners')
  if(v.allow!==undefined&&(!Array.isArray(v.allow?.set)||v.allow.set.some(x=>!['dig','place','harvest'].includes(name(x)))))throw fail('validation','allow must be a set of :dig :place :harvest')
  if(v.note!==undefined&&typeof v.note!=='string')throw fail('validation','note must be text')
}
function owns(previous,actor){const owner=previous?.owner??previous?.by;if(owner&&owner.toLowerCase()!==actor.toLowerCase())throw fail('not-owner',`object belongs to ${owner}`,{owner})}
function intersects(a,b){return a&&b&&[0,1,2].every(i=>a[0][i]<=b[1][i]&&b[0][i]<=a[1][i])}
export async function execute(r){const {ctx,command,kind,id,values:v}=r
  if(command==='find'){const page=query(['marker','zone','claim'].flatMap(k=>listDocuments(ctx,k)).filter(d=>d.kind!=='claim'||v.status||name(d.value.status)==='active'&&d.value.until>Date.now()),filters(ctx,v));return rawBound({...page,items:page.items.map(d=>v.raw?{...summary(d),record:d.value}:summary(d))})}
  if(command==='show'){const d=readDocument(ctx,kind,id);if(!d)throw fail('not-found','object not found',{kind,id});return rawBound(v.raw?{...summary(d),record:d.value}:summary(d))}
  let patch
  if(['add','edit'].includes(command)){if(!v.data||Buffer.byteLength(v.data)>65536)throw fail('invalid-data','give --data EDN map up to64KiB');patch=readEDN(v.data);if(!patch||Array.isArray(patch)||typeof patch!=='object'||patch.key||patch.list||patch.set)throw fail('invalid-data','data must be a map')}
  let value,conflicts=[]
  // Build and validate while mutateDocument holds the shared lock, using its validation callback.
  const previous=readDocument(ctx,kind,id)
  if(['remove','renew','release'].includes(command)&&!previous)throw fail('not-found','object not found',{kind,id})
  value=command==='remove'?null:{...(previous?.value??{}),...(patch??{})}
  if(kind==='marker'&&value){if(patch?.name!==undefined&&patch.name!==id)throw fail('validation','name must match ID');value.name=id;value.by=previous?.value.by??v.by;value.source=name(patch?.source??previous?.value.source??'intent')
    if(!['intent','observation'].includes(value.source))throw fail('validation','marker source must be intent or observation')
    for(const k of ['x','y','z'])if(!Number.isFinite(value[k])||Math.abs(value[k])>30000000)throw fail('validation','marker needs finite x/y/z within world bounds')
    if(value.kind!==undefined&&typeof value.kind!=='string')value.kind=name(value.kind)
    if(typeof(value.kind??'place')!=='string')throw fail('validation','kind must be text');value.kind??='place'
    const fields=markFields({saved:previous?.value,by:v.by,note:value.note});if(fields.error)throw fail('validation',fields.error);Object.assign(value,fields)
    if(patch?.by!==undefined&&patch.by!==value.by)throw fail('not-owner','marker authorship cannot be transferred by edit')
  }
  if(kind==='zone'&&value){if(patch?.name!==undefined&&patch.name!==id)throw fail('validation','name must match ID');value.name=id;value.owner??=v.by;validateZone(value)}
  if(kind==='claim'&&value){if(patch?.id!==undefined&&patch.id!==id)throw fail('validation','claim id must match ID');value.id=id;value.owner??=v.by
    if(command==='add'||command==='renew'||v.for){value.until=Date.now()+ttl(v.for);value.status=keyword('active')}
    if(command==='release'){value.status=keyword('released');value.until=Date.now()}
    if(!Array.isArray(value.min)||!Array.isArray(value.max)||!boxOf(value)||![...value.min,...value.max].every(Number.isInteger)||value.min.length!==3||value.max.length!==3||value.min.some((n,i)=>n>value.max[i]))throw fail('validation','claim needs ordered integer min/max corners')
    if(typeof value.owner!=='string'||!value.owner.trim()||!['active','released'].includes(name(value.status))||!Number.isSafeInteger(value.until))throw fail('validation','claim needs owner, valid status and expiry')
    if(Object.keys(value).some(k=>!['id','owner','min','max','until','status','note'].includes(k)))throw fail('validation','unsupported claim key')
    if(value.note!==undefined&&typeof value.note!=='string')throw fail('validation','note must be text')
    if(value.until>Date.now()+604800000)throw fail('invalid-duration','claim expiry cannot be more than7d away')
  }
  if(v.raw)preflightRawMutation(command==='remove'?previous?.value:value,kind,id,command)
  const result=await mutateDocument(ctx,{kind,id,expectedRevision:command==='add'?null:v.revision,value,by:v.by,source:value?.source??'intent',dryRun:v['dry-run']||v.preview,validate:()=>{
    const current=readDocument(ctx,kind,id)?.value
    if(kind==='marker'){
      const rewrites=command==='remove'||Object.keys(patch??{}).some(k=>!['note'].includes(k));const refusal=rewrites?mapRefusal(current,v.by):null;if(refusal)throw fail('not-owner',refusal,{owner:current?.by})
      if(patch?.structure!==undefined||patch?.map!==undefined||patch?.legend!==undefined)throw fail('validation','map tool does not edit embedded legacy structures; use plans')
    }else {owns(current,v.by);if(value?.owner&&value.owner.toLowerCase()!==v.by.toLowerCase())throw fail('not-owner','owner must be the acting author')}
    if(kind==='claim'&&value&&name(value.status)==='active'&&value.until>Date.now()){
      conflicts=listDocuments(ctx,'claim').filter(d=>d.id!==id&&name(d.value.status)==='active'&&d.value.until>Date.now()&&d.value.owner.toLowerCase()!==v.by.toLowerCase()&&intersects(boxOf(value),boxOf(d.value))).map(d=>summary(d))
      if(conflicts.length){const center=value.min.map((n,i)=>(n+value.max[i])/2),radius=Math.hypot(...value.min.map((n,i)=>(value.max[i]-n)/2)),spatialFind=`map.mjs --world ${ctx.world} find --type claim --status active --center '${writeEDN(center)}' --radius ${radius} --limit 100`
        throw fail('claim-conflict',`another owner has an active overlapping intent claim; inspect other claims with ${spatialFind}`,{conflicts,'conflicts-total':conflicts.length,'more?':conflicts.length>10,next:spatialFind})}
    }
  }})
  const rawRecord=v.raw?(command==='remove'?previous?.value:result.preview?value:readDocument(ctx,kind,id)?.value):undefined
  return rawBound({...result,...(v.raw&&rawRecord?{record:rawRecord}:{}),...(conflicts.length?{conflicts}:{})})
}
export async function main(argv=process.argv.slice(2)){try{const result=await execute(options(argv));process.stdout.write(writeEDN(result)+'\n');return 0}catch(e){process.stdout.write(writeEDN({ok:false,reason:keyword(e.reason??'invalid-request'),message:e.message.slice(0,500),...(e.current!==undefined?{current:e.current}:{}),...(e.owner?{owner:e.owner}:{}),...(e.conflicts?{conflicts:e.conflicts.slice(0,10),'conflicts-total':e['conflicts-total']??e.conflicts.length,'more?':e['more?']??e.conflicts.length>10,...(e.next?{next:e.next}:{})}:{})})+'\n');return 1}}
if(process.argv[1]&&path.resolve(process.argv[1])===fileURLToPath(import.meta.url))process.exitCode=await main()
