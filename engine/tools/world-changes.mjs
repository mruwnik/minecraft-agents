#!/usr/bin/env node
import fs from 'node:fs'
import path from 'node:path'
import {fileURLToPath} from 'node:url'
import {parseArgs} from 'node:util'
import {setTimeout as delay} from 'node:timers/promises'
import {defaultStateDir} from './drive-lib.mjs'
import {readEDN,writeEDN,keyword} from './observe-lib.mjs'
import {context,readChanges,rawBound,withFileLock,fail} from './world-data.mjs'
import {filters} from './map.mjs'
export const usage='world-changes.mjs --world WORLD [--cursor EDN | --observer NAME] [--wait --timeout 60s] [--raw] [--type TYPE --owner OWNER --status STATUS --text TEXT --center EDN --place NAME --radius 128 --limit 10 --state DIR]'
export function options(argv){const {positionals,values:v}=parseArgs({args:argv,allowPositionals:true,options:{world:{type:'string'},state:{type:'string',default:defaultStateDir},'repo-root':{type:'string'},cursor:{type:'string'},observer:{type:'string'},wait:{type:'boolean'},raw:{type:'boolean'},timeout:{type:'string'},'poll-ms':{type:'string'},center:{type:'string'},place:{type:'string'},radius:{type:'string'},type:{type:'string'},owner:{type:'string'},status:{type:'string'},text:{type:'string'},limit:{type:'string'}}})
  if(positionals.length||v.cursor&&v.observer)throw fail('invalid-options',usage)
  if(v.cursor&&Buffer.byteLength(v.cursor)>500)throw fail('invalid-cursor','cursor too large')
  const observer=v.observer??'agent';if(!/^[A-Za-z0-9_-]{1,64}$/.test(observer))throw fail('invalid-observer','invalid observer name')
  const m=/^(\d+(?:\.\d+)?)(ms|s|m)?$/.exec(v.timeout??'60s'),timeoutMs=m?Number(m[1])*({ms:1,s:1000,m:60000}[m[2]??'s']):NaN,pollMs=Number(v['poll-ms']??250)
  if(!Number.isFinite(timeoutMs)||timeoutMs<10||timeoutMs>3600000||!Number.isInteger(pollMs)||pollMs<50||pollMs>5000)throw fail('invalid-timeout','timeout10ms..60m and poll50..5000ms required')
  if(!v.wait&&(v.timeout||v['poll-ms']))throw fail('invalid-options','timeout/poll need --wait')
  const ctx=context({state:v.state,world:v.world,repoRoot:v['repo-root']}),filter=filters(ctx,v)
  return {ctx,filter,observer,cursor:v.cursor?readEDN(v.cursor):undefined,explicitCursor:!!v.cursor,wait:!!v.wait,raw:!!v.raw,timeoutMs,pollMs}
}
function checkpoint(file,cursor){fs.mkdirSync(path.dirname(file),{recursive:true});const temp=`${file}.${process.pid}.tmp`;try{fs.writeFileSync(temp,writeEDN(cursor)+'\n',{mode:0o600});fs.renameSync(temp,file)}finally{if(fs.existsSync(temp))fs.unlinkSync(temp)}}
export async function execute(r,{signal,output=async result=>{await new Promise((resolve,reject)=>process.stdout.write(writeEDN(rawBound(result))+'\n',e=>e?reject(e):resolve()))}}={}){
  const file=path.join(r.ctx.metadataDir,'observers',`${r.observer}.edn`)
  const run=async()=>{
    let cursor=r.cursor
    if(!r.explicitCursor&&fs.existsSync(file))cursor=readEDN(fs.readFileSync(file,'utf8'))
    if(!cursor){const baseline=await readChanges(r.ctx);cursor=baseline.cursor;if(!r.explicitCursor)checkpoint(file,cursor)}
    const deadline=Date.now()+r.timeoutMs
    while(true){signal?.throwIfAborted();const result=await readChanges(r.ctx,{cursor,...r.filter});cursor=result.cursor
      if(result.ok===false||result.items.length||!r.wait||Date.now()>=deadline){const answer={...result,...(r.wait&&result.ok!==false&&!result.items.length?{timeout:true}:{})};await output(answer);if(!r.explicitCursor)checkpoint(file,cursor);return answer}
      await delay(Math.min(r.pollMs,Math.max(1,deadline-Date.now())),undefined,{signal})
    }
  }
  return r.explicitCursor?run():withFileLock(file,run,{timeoutMs:0})
}
export async function main(argv=process.argv.slice(2)){const controller=new AbortController(),stop=()=>controller.abort();process.once('SIGINT',stop);process.once('SIGTERM',stop)
  try{await execute(options(argv),{signal:controller.signal});return 0}catch(e){if(controller.signal.aborted)return 130;process.stdout.write(writeEDN({ok:false,reason:keyword(e.reason??'invalid-request'),message:e.message.slice(0,300)})+'\n');return 1}finally{process.removeListener('SIGINT',stop);process.removeListener('SIGTERM',stop)}}
if(process.argv[1]&&path.resolve(process.argv[1])===fileURLToPath(import.meta.url))process.exitCode=await main()
