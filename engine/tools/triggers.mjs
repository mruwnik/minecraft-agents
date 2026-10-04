#!/usr/bin/env node
import path from 'node:path'
import http from 'node:http'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import edn from 'edn-data'
import { NAME, bodyDir, missingWorldError } from '../js/bodies.mjs'
import { defaultStateDir } from './drive-lib.mjs'
import { get, unsupportedObserveRoute } from './observe.mjs'
import { readEDN, writeEDN, keyword } from './observe-lib.mjs'

export const usage = `usage: triggers.mjs <body> --world <world> <command> [id] [options]
  list [--limit 8 --offset 0] | show <id>
  add|put <id> --trigger <predefined> [--args EDN] [--job EDN]
  add|put <id> --when EDN --job EDN
    [--persistence stop|retry|cooldown] [--cooldown-s N] [--for 10m] [--backoff EDN]
  remove <id> | mute <id> [--for 10m] | unmute <id>
  move <id> --before <anchor>|--after <anchor> [--for 10m]
  reset <id> --property position|mute
  [--by agent] [--state DIR]
Add and put both create or replace a custom entry; built-in entries cannot be replaced or removed.`
export function identifier (s) {
  const name = typeof s === 'string' ? s.replace(/^:/, '') : ''
  if (!/^[a-z][a-z0-9-]{0,39}$/.test(name)) throw new Error('trigger ID must be a short lowercase name')
  return keyword(name)
}
export function oneForm (text) {
  if (typeof text !== 'string' || Buffer.byteLength(text) > 12000) throw new Error('EDN input must be at most 12000 bytes')
  const parsed = edn.parseEDNString(`(${text})`)
  if (parsed?.list?.length !== 1) throw new Error('expected one EDN form')
  return parsed.list[0]
}
export function duration (text) {
  const m = /^(\d+(?:\.\d+)?)(ms|s|m|h|d)?$/.exec(text ?? '')
  const seconds = m ? Number(m[1]) * ({ms: 0.001, s: 1, m: 60, h: 3600, d: 86400}[m[2] ?? 's']) : NaN
  if (!Number.isFinite(seconds) || seconds <= 0 || seconds > 31536000) throw new Error('--for must be a positive duration up to 365 days, e.g. 30s or 10m')
  return seconds
}
export function requestFor (argv) {
  try {
    const p = parseArgs({args: argv, allowPositionals: true, options: {
      state: {type:'string', default:defaultStateDir}, world:{type:'string'}, by:{type:'string',default:'agent'},
      limit:{type:'string'}, offset:{type:'string'}, trigger:{type:'string'}, when:{type:'string'}, job:{type:'string'}, args:{type:'string'},
      persistence:{type:'string'}, 'cooldown-s':{type:'string'}, for:{type:'string'}, backoff:{type:'string'}, before:{type:'string'}, after:{type:'string'}, property:{type:'string'}
    }})
    const [body, command='list', name, ...extra] = p.positionals, v=p.values
    if (!body || !NAME.test(body)) throw new Error('body must be a valid name')
    if (v.world === undefined) throw new Error(missingWorldError('--world'))
    if (!NAME.test(v.world)) throw new Error('world must be a valid name')
    if (!['list','show','add','put','remove','mute','unmute','move','reset'].includes(command)) throw new Error('unknown command')
    if (extra.length || (command==='list' ? name!==undefined : name===undefined)) throw new Error(`${command} ${command==='list'?'takes no ID':'needs exactly one ID'}`)
    const allowed = {list:['limit','offset'], show:[], add:['trigger','when','job','args','persistence','cooldown-s','for','backoff'], put:['trigger','when','job','args','persistence','cooldown-s','for','backoff'],
      remove:[], mute:['for'], unmute:[], move:['before','after','for'], reset:['property']}[command]
    for (const k of Object.keys(v)) if (!['state','world','by',...allowed].includes(k)) throw new Error(`--${k} is not valid for ${command}`)
    if (!v.by || v.by.length>40) throw new Error('--by must be 1..40 characters')
    const state=path.resolve(v.state), socketPath=path.join(bodyDir(state,v.world,body),'engine','events.sock')
    const base={body,world:v.world,state,socketPath,command}
    if (command==='list') {
      const limit=Number(v.limit??8),offset=Number(v.offset??0)
      if (!Number.isInteger(limit)||limit<1||limit>32||!Number.isInteger(offset)||offset<0||offset>10000) throw new Error('limit 1..32 and offset 0..10000 required')
      return {...base,path:'/triggers',limit,offset,mutating:false}
    }
    const id=identifier(name)
    if(command==='show')return {...base,path:`/triggers?id=${encodeURIComponent(id.key)}`,id,mutating:false}
    const request={op:keyword({add:'put',put:'put',unmute:'clear',reset:'clear'}[command]??command),id,by:v.by}
    if (v.for!==undefined) request['ttl-s']=duration(v.for)
    if (['add','put'].includes(command)) {
      if((v.trigger!==undefined)===(v.when!==undefined))throw new Error('give exactly one of --trigger or --when')
      if(v.trigger!==undefined)request.trigger=identifier(v.trigger)
      else { request.when=oneForm(v.when);if(v.job===undefined)throw new Error('--when needs --job') }
      if(v.job!==undefined){const job=oneForm(v.job);if(!job?.list?.[0]?.sym)throw new Error('--job must be a native EDN job list');request.job=job}
      if(v.args!==undefined){if(v.when!==undefined)throw new Error('ad hoc conditions use job args, not --args');const args=oneForm(v.args);if(!args?.map)throw new Error('--args must be an EDN map');request.args=readEDN(v.args)}
      if(v.persistence!==undefined){if(!['stop','retry','cooldown'].includes(v.persistence))throw new Error('invalid persistence');request.persistence=keyword(v.persistence)}
      if(v['cooldown-s']!==undefined){const n=Number(v['cooldown-s']);if(!Number.isFinite(n)||n<0)throw new Error('cooldown must be nonnegative');request['cooldown-s']=n}
      if(v.backoff!==undefined){oneForm(v.backoff);request.backoff=readEDN(v.backoff)}
    }
    if(command==='move') {if((v.before!==undefined)===(v.after!==undefined))throw new Error('move needs exactly one of --before or --after');request[v.before!==undefined?'above':'below']=identifier(v.before??v.after);if((request.above??request.below).key===id.key)throw new Error('cannot move a trigger relative to itself')}
    if(command==='unmute')request.property=keyword('mute')
    if(command==='reset'){if(!['position','mute'].includes(v.property))throw new Error('reset needs --property position or mute');request.property=keyword(v.property)}
    if(Buffer.byteLength(writeEDN(request))>15000)throw new Error('combined request exceeds 15000 bytes')
    return {...base,path:'/triggers',id,mutating:true,request}
  }catch(error){return {error:error.message}}
}
function bounded (value, budget={left:128}, depth=0) {
  if(--budget.left<0||depth>6)return keyword('truncated')
  if(typeof value==='string')return value.slice(0,240)
  if(Array.isArray(value))return value.slice(0,16).map(v=>bounded(v,budget,depth+1))
  if(value&&typeof value==='object') {
    if(value.key||value.sym)return value
    if(value.list)return {list:value.list.slice(0,16).map(v=>bounded(v,budget,depth+1))}
    return Object.fromEntries(Object.entries(value).slice(0,16).map(([k,v])=>[k,bounded(v,budget,depth+1)]))
  }
  return value
}
export function compact (r,value) {
  if(value.ok===false)return bounded(value)
  if(r.command==='list') {
    const all=value.items??[],selected=all.slice(r.offset,r.offset+r.limit)
    const ranks=new Map((value.order??[]).map((id,index)=>[id.key,index+1]))
    return {total:value.total??all.length,items:selected.map(e=>({id:e.id,trigger:e.trigger,...(e.when!==undefined?{when:bounded(e.when)}:{}),...(ranks.has(e.id?.key)?{priority:ranks.get(e.id.key)}:{}),
      ...(e.job?.list?.[0]?.sym?{job:e.job.list[0].sym}:{}),...(e['builtin?']?{'builtin?':true}:{}),
      ...(e.muted?{muted:true}:{}),...(e['stopped?']?{'stopped?':true}:{}),...(e['cooling-until']?{cooling:true}:{}),...(e['backing-off']?{'backing-off':true}:{})})),
      ...((r.offset+selected.length)<all.length?{'next-offset':r.offset+selected.length}:{})}
  }
  if(r.command==='show') {
    const entry=(value.items??[]).find(e=>e.id?.key===r.id.key)
    if(!entry)return {ok:false,reason:keyword('trigger-not-found'),id:r.id}
    const result=bounded(entry)
    if(value.explain?.terms)result.explain=bounded(value.explain.terms)
    return result
  }
  return {ok:true,id:value.id,op:value.op,...(value['created?']!==undefined?{'created?':value['created?']}:{}),...(value.trigger?.muted?{muted:true}:{}),...(value.trigger?.moved?{moved:bounded(value.trigger.moved)}:{})}
}
export function post (socketPath, body, {timeoutMs=3000,requestImpl=http.request}={}) {
  return new Promise((resolve,reject)=>{
    const payload=writeEDN(body);let settled=false,timer
    const finish=(fn,value)=>{if(!settled){settled=true;clearTimeout(timer);fn(value)}}
    const req=requestImpl({socketPath,path:'/triggers',method:'POST',headers:{'content-type':'application/edn','content-length':Buffer.byteLength(payload)}},res=>{
      let size=0,text='';res.setEncoding('utf8')
      res.on('data',c=>{size+=Buffer.byteLength(c);if(size>262144)req.destroy(Object.assign(new Error('too large'),{code:'ERESPONSETOOLARGE'}));else text+=c})
      res.on('error',e=>finish(reject,e));res.on('aborted',()=>finish(reject,Object.assign(new Error('aborted'),{code:'ECONNRESET'})))
      res.on('end',()=>finish(resolve,{status:res.statusCode,contentType:res.headers['content-type'],text}))
    })
    req.on('error',e=>finish(reject,e));timer=setTimeout(()=>req.destroy(Object.assign(new Error('timeout'),{code:'ETIMEDOUT'})),timeoutMs);req.end(payload)
  })
}
export async function main (argv=process.argv.slice(2)) {
  const r=requestFor(argv)
  if(r.error){console.error(`${r.error}\n${usage}`);return 2}
  let sent=false
  try {
    let response
    if(r.mutating) {
      const current=await get(r.socketPath,'/triggers')
      if(unsupportedObserveRoute(current)){process.stdout.write(writeEDN({ok:false,reason:keyword('triggers-unavailable'),action:keyword('restart-with-current-build')})+'\n');return 2}
      if(current.status!==200||!/^application\/edn(?:;|$)/i.test(current.contentType??''))throw new Error('trigger API unavailable')
      const generation=readEDN(current.text)['generation-id'];if(typeof generation!=='string')throw new Error('no generation')
      sent=true;response=await post(r.socketPath,{...r.request,'generation-id':generation})
    }else response=await get(r.socketPath,r.path)
    if(!/^application\/edn(?:;|$)/i.test(response.contentType??''))throw new Error('bad content type')
    const value=readEDN(response.text)
    if(response.status===404&&value.reason?.key==='not-found') {process.stdout.write(writeEDN({ok:false,reason:keyword('triggers-unavailable'),action:keyword('restart-with-current-build')})+'\n');return 2}
    const projected=compact(r,value)
    process.stdout.write(writeEDN(projected)+'\n');return response.status===200&&projected.ok!==false?0:1
  }catch(error) {
    process.stdout.write(writeEDN({ok:false,reason:keyword(error.code==='ERESPONSETOOLARGE'?'response-too-large':['ENOENT','ECONNREFUSED'].includes(error.code)?'no-running-body':'transport-error'),
      ...(sent?{confirmation:keyword('unknown'),message:'Inspect trigger show/list before retrying; mutations are not automatically retried.'}:{})})+'\n');return 2
  }
}
if(process.argv[1]&&path.resolve(process.argv[1])===fileURLToPath(import.meta.url))process.exitCode=await main()
