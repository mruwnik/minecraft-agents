import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { manifestId } from '../blueprint/manifest.mjs'
import { atomicMapWrite,withMapLock } from '../map-store.mjs'
export const VILLAGE_INSPECTION_DIR=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'../../state/village-inspections')
export function listVillageInspections(dir=VILLAGE_INSPECTION_DIR){
  if(!fs.existsSync(dir))return[]
  const files=fs.readdirSync(dir).filter(n=>/^[a-f0-9]{32}\.json$/.test(n))
  if(files.length>1000)throw new Error('village inspection list exceeds 1000 records')
  return files.flatMap(name=>{const file=path.join(dir,name);if(fs.statSync(file).size>4*1024*1024)throw new Error('village inspection exceeds 4 MiB');const r=JSON.parse(fs.readFileSync(file,'utf8'));return r.version===1&&typeof r.place==='string'&&r.at&&['x','y','z'].every(k=>Number.isInteger(r.at[k]))?[r]:[]})
}
export function readVillageInspection(place,at,dir=VILLAGE_INSPECTION_DIR){
  at={x:at.x,y:at.y,z:at.z}
  const file=path.join(dir,`${manifestId(place,at)}.json`)
  if(!fs.existsSync(file))return null
  const record=JSON.parse(fs.readFileSync(file,'utf8'))
  if(record.version!==1||record.place!==place||['x','y','z'].some(k=>record.at?.[k]!==at[k]))throw new Error('village inspection identity/version mismatch')
  return record
}
export function saveVillageInspection(plan,report,dir=VILLAGE_INSPECTION_DIR){
  if(!plan.saved)return null
  const file=path.join(dir,`${manifestId(plan.saved.name,plan.at)}.json`)
  fs.mkdirSync(dir,{recursive:true})
  const record={version:1,place:plan.saved.name,at:plan.at,sourceHash:plan.ir.hash,buildSourceHash:plan.manifest?.sourceHash,source:plan.source,population:plan.intent,observedAt:new Date().toISOString(),report}
  withMapLock(file,()=>atomicMapWrite(file,record));return record
}
