import fs from 'node:fs'
import path from 'node:path'
import {createHash,randomUUID} from 'node:crypto'
import {fileURLToPath} from 'node:url'
import {setTimeout as delay} from 'node:timers/promises'
import {readEDN,writeEDN,keyword} from './observe-lib.mjs'

export const MAX_FILE_BYTES=8388608, MAX_OBJECTS=10000, MAX_EVENTS=2048
export const ID=/^[A-Za-z0-9_-]{1,100}$/
export const name=v=>v?.key??v
export const fail=(reason,message,extra={})=>Object.assign(new Error(message),{reason,...extra})
export const revision=text=>text===null?null:createHash('sha256').update(text).digest('hex').slice(0,24)
export function context({state,world,repoRoot=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'../..')}) {
  if(!/^[A-Za-z0-9_-]{1,64}$/.test(world??''))throw fail('invalid-world','give an explicit valid --world')
  const worldDir=path.resolve(state,'worlds',world)
  if(!fs.existsSync(path.join(worldDir,'world.json')))throw fail('world-not-found',`no world ${world}`)
  return {state:path.resolve(state),world,repoRoot:path.resolve(repoRoot),worldDir,plansDir:path.join(worldDir,'plans'),blueprintDir:path.join(path.resolve(repoRoot),'blueprints'),columnsDir:path.join(worldDir,'chunks'),metadataDir:path.join(worldDir,'.agent-data')}
}
function checkedID(id){if(typeof id!=='string'||!ID.test(id))throw fail('invalid-id','ID must use letters, digits, _ or - (up to100)');return id}
export function documentPath(ctx,kind,id) {
  if(kind==='plan')return path.join(ctx.plansDir,`${checkedID(id)}.edn`)
  if(kind==='blueprint')return path.join(ctx.blueprintDir,`${checkedID(id)}.edn`)
  if(['marker','zone','claim'].includes(kind)){if(typeof id!=='string'||!id.trim()||id.length>100||id.includes('\0'))throw fail('invalid-id','map names need1..100 characters');return path.join(ctx.worldDir,{marker:'places.json',zone:'zones.edn',claim:'claims.edn'}[kind])}
  throw fail('invalid-kind','kind must be marker, zone, claim, plan or blueprint')
}
let scanBudget=null
function readText(file,max=MAX_FILE_BYTES) {
  try{const stat=fs.statSync(file);if(!stat.isFile()||stat.size>max)throw fail('file-too-large',`file exceeds ${max} bytes`,{file});if(scanBudget!==null){scanBudget-=stat.size;if(scanBudget<0)throw fail('scan-too-large','shared world scan exceeds64MiB; archive unused documents')}return fs.readFileSync(file,'utf8')}
  catch(e){if(e.code==='ENOENT')return null;throw e}
}
function parseCollection(kind,text) {
  if(text===null)return []
  let values
  try{values=kind==='marker'?JSON.parse(text):readEDN(text)}catch{throw fail('invalid-file',`unreadable ${kind} collection`)}
  if(!Array.isArray(values)||values.length>MAX_OBJECTS)throw fail('invalid-file',`expected a bounded ${kind} vector`)
  const ids=new Set()
  for(const value of values){const id=kind==='claim'?value.id:value.name;if(typeof id!=='string'||ids.has(id))throw fail('invalid-file','missing or duplicate object ID');ids.add(id)}
  return values
}
function record(ctx,kind,id,value,text,file){return {kind,id,value,text,path:file,revision:revision(text),scope:kind==='blueprint'?'global':'world'}}
export function readDocument(ctx,kind,id) {
  const file=documentPath(ctx,kind,id),text=readText(file)
  if(text===null)return null
  if(['marker','zone','claim'].includes(kind)) {
    const value=parseCollection(kind,text).find(v=>(kind==='claim'?v.id:v.name)===id)
    return value?record(ctx,kind,id,value,kind==='marker'?JSON.stringify(value):writeEDN(value),file):null
  }
  try{return record(ctx,kind,id,readEDN(text),text,file)}catch{throw fail('invalid-file',`unreadable ${kind} ${id}`)}
}
export function listDocuments(ctx,kind) {
  if(['marker','zone','claim'].includes(kind)) {
    const file=documentPath(ctx,kind,'collection')
    return parseCollection(kind,readText(file)).map(v=>record(ctx,kind,kind==='claim'?v.id:v.name,v,kind==='marker'?JSON.stringify(v):writeEDN(v),file))
  }
  const dir=kind==='plan'?ctx.plansDir:kind==='blueprint'?ctx.blueprintDir:null
  if(!dir)throw fail('invalid-kind','unknown kind')
  let files
  try{files=fs.readdirSync(dir).filter(f=>f.endsWith('.edn')).sort()}catch(e){if(e.code==='ENOENT')return [];throw e}
  if(files.length>1000)throw fail('object-limit','at most1000 documents per directory')
  const prior=scanBudget;scanBudget=Math.min(scanBudget??67108864,67108864)
  try{return files.map(f=>readDocument(ctx,kind,f.slice(0,-4)))}finally{if(prior===null)scanBudget=null}
}
function atomicText(file,text){fs.mkdirSync(path.dirname(file),{recursive:true});const temp=`${file}.${process.pid}.${randomUUID()}.tmp`;try{const fd=fs.openSync(temp,'wx',0o600);try{fs.writeFileSync(fd,text);fs.fsyncSync(fd)}finally{fs.closeSync(fd)}fs.renameSync(temp,file)}finally{if(fs.existsSync(temp))fs.unlinkSync(temp)}}
async function locked(file,run,{timeoutMs=1500}={}) {
  fs.mkdirSync(path.dirname(file),{recursive:true});const lock=`${file}.lock`,deadline=Date.now()+timeoutMs,token=`${process.pid}\n${randomUUID()}`
  let fd
  while(fd===undefined){try{fd=fs.openSync(lock,'wx',0o600);fs.writeFileSync(fd,token)}catch(e){if(e.code!=='EEXIST')throw e
    // The guard serializes stale-lock readers: each rechecks the current owner.
    let reaper
    try{reaper=fs.openSync(`${lock}.reaper`,'wx',0o600);const owner=readText(lock,200),pid=Number(owner?.split('\n')[0]);let dead=false
      if(Number.isInteger(pid)&&pid>0)try{process.kill(pid,0)}catch(e){dead=e.code==='ESRCH'}
      if(dead&&readText(lock,200)===owner)fs.unlinkSync(lock)
    }catch(e){if(!['EEXIST','ENOENT'].includes(e.code))throw e}
    finally{if(reaper!==undefined){fs.closeSync(reaper);fs.unlinkSync(`${lock}.reaper`)}}
    if(!fs.existsSync(lock))continue
    if(Date.now()>=deadline)throw fail('busy','shared data is busy; a crashed reaper guard requires inspection before removal')
    await delay(20)
  }}
  try{return await run()}finally{fs.closeSync(fd);if(readText(lock,200)===token)fs.unlinkSync(lock)}
}
function ledgerFile(ctx){return path.join(ctx.metadataDir,'changes.edn')}
function loadLedger(ctx) {
  const text=readText(ledgerFile(ctx),8*MAX_FILE_BYTES)
  if(text===null)return {stream:randomUUID(),seq:0,index:{},events:[]}
  const l=readEDN(text)
  if(typeof l.stream!=='string'||!Number.isSafeInteger(l.seq)||!Array.isArray(l.events)||l.events.length>MAX_EVENTS||!l.index||Object.keys(l.index).length>MAX_OBJECTS)throw fail('invalid-ledger','invalid shared change ledger')
  return l
}
const objectKey=(kind,id)=>`${kind}/${id}`
export function boxOf(v){if(Array.isArray(v.min)&&Array.isArray(v.max))return [v.min,v.max];if(['x','y','z'].every(k=>Number.isFinite(v[k])))return [[v.x,v.y,v.z],[v.x,v.y,v.z]];return null}
function meta(d){const v=d.value;return {kind:d.kind,id:d.id,revision:d.revision,scope:d.scope,owner:v.owner??v.by,status:name(v.status),type:name(v.kind??v.type),box:boxOf(v),until:v.until}}
function event(l,obj,op,by,source,previous){const e={seq:++l.seq,'time-ms':Date.now(),...obj,op,by,source,...(previous?{'previous-revision':previous}:{})};l.events.push(e);l.events=l.events.slice(-MAX_EVENTS);return e}
function scan(ctx,l) {
  const now={};scanBudget=67108864
  try{for(const kind of ['marker','zone','claim','plan','blueprint'])for(const d of listDocuments(ctx,kind)){const key=objectKey(kind,d.id);now[key]=meta(d)}}finally{scanBudget=null}
  if(Object.keys(now).length>MAX_OBJECTS)throw fail('object-limit','too many shared objects')
  for(const [key,obj]of Object.entries(now))if(l.index[key]?.revision!==obj.revision)event(l,obj,l.index[key]?'edit':'add',null,'external',l.index[key]?.revision)
  for(const [key,obj]of Object.entries(l.index))if(!now[key])event(l,{...obj,revision:null},'remove',null,'external',obj.revision)
  l.index=now
}
async function recover(ctx,global=false) {
  const file=global?path.join(ctx.blueprintDir,'.agent-pending.edn'):path.join(ctx.metadataDir,'pending.edn'),text=readText(file,10*MAX_FILE_BYTES);if(text===null)return
  const tx=readEDN(text)
  if(global){if(tx.kind!=='blueprint'||!tx.context||path.resolve(tx.context.repoRoot)!==ctx.repoRoot)throw fail('invalid-journal','invalid global blueprint journal');ctx=context(tx.context)}
  if(typeof tx.file!=='string'||!tx.ledger||!['plan','blueprint','marker','zone','claim'].includes(tx.kind)||documentPath(ctx,tx.kind,tx.id)!==tx.file)throw fail('invalid-journal','invalid pending shared write')
  await locked(tx.file,()=>{const current=revision(readText(tx.file));if(current!==tx.before&&current!==revision(tx.after))throw fail('recovery-conflict','a pending write conflicts with a later external edit; inspect .agent-data/pending.edn')
    if(tx.after===null){if(fs.existsSync(tx.file))fs.unlinkSync(tx.file)}else atomicText(tx.file,tx.after)
    atomicText(ledgerFile(ctx),writeEDN(tx.ledger)+'\n');fs.unlinkSync(file)
  })
}
async function transaction(ctx,run){return locked(path.join(ctx.blueprintDir,'.agent-transactions'),async()=>{await recover(ctx,true);return locked(ledgerFile(ctx),async()=>{await recover(ctx);return run(loadLedger(ctx))})})}
export async function synchronize(ctx){return transaction(ctx,l=>{scan(ctx,l);atomicText(ledgerFile(ctx),writeEDN(l)+'\n');return {stream:l.stream,seq:l.seq}})}
export async function mutateDocument(ctx,{kind,id,expectedRevision,value,text,by,source='intent',validate,dryRun=false}) {
  if(text!==undefined){if(!['plan','blueprint'].includes(kind)||typeof text!=='string'||Buffer.byteLength(text)>MAX_FILE_BYTES)throw fail('invalid-text','exact text is only supported for bounded plan/blueprint documents');let decoded;try{decoded=readEDN(text)}catch{throw fail('invalid-text','unreadable document text')}if(value!==undefined&&writeEDN(decoded)!==writeEDN(value))throw fail('text-mismatch','text and decoded value disagree');value=decoded}
  documentPath(ctx,kind,id);if(typeof by!=='string'||!by.trim()||by.length>80)throw fail('actor-required','mutations need explicit --by (up to80 characters)')
  if(expectedRevision===undefined)throw fail('revision-required','mutation needs expected revision; null means create only')
  if(!['intent','observation'].includes(source))throw fail('invalid-source','source must be intent or observation')
  const file=documentPath(ctx,kind,id)
  return transaction(ctx,l=>locked(file,async()=>{
    scan(ctx,l)
    const previous=readDocument(ctx,kind,id)
    if((previous?.revision??null)!==expectedRevision)throw fail('revision-conflict','object changed; read it before editing',{current:previous?.revision??null,id,kind})
    if(validate){const result=await validate(value,ctx);if(result?.errors?.length)throw fail('validation',result.errors.join('; '),{errors:result.errors})}
    const before=readText(file)
    let after
    if(['marker','zone','claim'].includes(kind)){const values=parseCollection(kind,before).filter(v=>(kind==='claim'?v.id:v.name)!==id);if(value!==null)values.push(value);after=(kind==='marker'?JSON.stringify(values,null,1):writeEDN(values))+'\n'}
    else after=value===null?null:text!==undefined?text:writeEDN(value)+'\n'
    if(after!==null&&Buffer.byteLength(after)>MAX_FILE_BYTES)throw fail('file-too-large','updated file would exceed8MiB')
    const nextRevision=value===null?null:revision(['marker','zone','claim'].includes(kind)?kind==='marker'?JSON.stringify(value):writeEDN(value):after)
    const obj=meta({kind,id,value:value??previous?.value??{},revision:nextRevision,scope:kind==='blueprint'?'global':'world'})
    const op=value===null?'remove':previous?'edit':'add'
    if(dryRun){const keys=new Set([...Object.keys(previous?.value??{}),...Object.keys(value??{})]);const fields=[...keys].filter(k=>writeEDN(previous?.value?.[k]??null)!==writeEDN(value?.[k]??null));return {ok:true,preview:true,kind:keyword(kind),id,scope:keyword(obj.scope),revision:previous?.revision??null,'next-revision':nextRevision,op:keyword(op),fields}}
    const e=event(l,obj,op,by,source,previous?.revision)
    if(value===null)delete l.index[objectKey(kind,id)];else l.index[objectKey(kind,id)]=obj
    const pending=kind==='blueprint'?path.join(ctx.blueprintDir,'.agent-pending.edn'):path.join(ctx.metadataDir,'pending.edn')
    atomicText(pending,writeEDN({context:{state:ctx.state,world:ctx.world,repoRoot:ctx.repoRoot},file,kind,id,before:revision(before),after,ledger:l})+'\n')
    if(after===null){if(fs.existsSync(file))fs.unlinkSync(file)}else atomicText(file,after)
    atomicText(ledgerFile(ctx),writeEDN(l)+'\n');fs.unlinkSync(pending)
    return {ok:true,kind:keyword(kind),id,scope:keyword(obj.scope),revision:nextRevision,op:keyword(op),cursor:{stream:l.stream,seq:e.seq}}
  }))
}
function distance(box,center){if(!box)return Infinity;return Math.hypot(...[0,1,2].map(i=>Math.max(box[0][i]-center[i],0,center[i]-box[1][i])))}
export function query(records,{center,radius=128,type,owner,status,text,limit=10,offset=0,now=Date.now()}={}) {
  if(!Number.isInteger(limit)||limit<1||limit>100||!Number.isInteger(offset)||offset<0)throw fail('invalid-page','limit1..100 and nonnegative offset required')
  if(center&&(!Array.isArray(center)||center.length!==3||!center.every(Number.isFinite)||!Number.isFinite(radius)||radius<0))throw fail('invalid-center','center needs [x y z], radius nonnegative')
  const filtered=records.map(d=>{const m=d.value?{...meta(d),...(d.box?{box:d.box}:{})}:d;return {...d,...m,...(m.status==='active'&&m.until&&m.until<=now?{status:'expired'}:{}),...(center?{distance:distance(m.box,center)}:{})}})
    .filter(d=>(!type||d.kind===type||d.type===type)&&(!owner||String(d.owner??'').toLowerCase()===owner.toLowerCase())&&(!status||d.status===status)&&(!text||`${d.id} ${d.value?.note??''} ${d.value?.name??''}`.toLowerCase().includes(text.toLowerCase()))&&(!center||d.distance<=radius))
    .sort((a,b)=>(center?a.distance-b.distance:0)||a.kind.localeCompare(b.kind)||a.id.localeCompare(b.id))
  const items=filtered.slice(offset,offset+limit)
  return {total:filtered.length,items,...(offset+items.length<filtered.length?{'next-offset':offset+items.length}:{})}
}
export async function readChanges(ctx,{cursor,limit=10,...filters}={}) {
  if(!Number.isInteger(limit)||limit<1||limit>100)throw fail('invalid-page','limit must be1..100')
  query([],{...filters,limit})
  return transaction(ctx,l=>{scan(ctx,l);atomicText(ledgerFile(ctx),writeEDN(l)+'\n')
    const oldest=l.events[0]?.seq??l.seq+1,end={stream:l.stream,seq:l.seq}
    if(!cursor)return {cursor:end,total:0,items:[]}
    if(cursor.stream!==l.stream||!Number.isSafeInteger(cursor.seq)||cursor.seq<oldest-1||cursor.seq>l.seq)return {ok:false,reason:keyword('cursor-gap'),cursor:end,'oldest-seq':oldest}
    const available=l.events.filter(e=>e.seq>cursor.seq)
    const matching=e=>query([e],{...filters,limit:1}).total>0
    const selected=new Set();let through=l.seq,more=false
    for(const e of available){if(!matching(e))continue;const key=objectKey(e.kind,e.id)
      if(!selected.has(key)&&selected.size>=limit){through=e.seq-1;more=true;break}selected.add(key)
    }
    const grouped=new Map()
    for(const e of available){if(e.seq>through||!matching(e))continue;const key=objectKey(e.kind,e.id),prior=grouped.get(key);grouped.set(key,{...e,changes:(prior?.changes??0)+1})}
    const items=[...grouped.values()].map(e=>({...e,kind:keyword(e.kind),op:keyword(e.op),source:keyword(e.source)}))
    return {cursor:{stream:l.stream,seq:through},total:new Set(available.filter(matching).map(e=>objectKey(e.kind,e.id))).size,items,...(more?{'more?':true}:{})}
  })
}
export const withFileLock=locked
export function rawBound(value,max=65536){if(Buffer.byteLength(writeEDN(value))>max)throw fail('output-too-large',`raw result exceeds ${max} bytes; scope or page the query`);return value}
