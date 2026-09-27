import fs from 'node:fs'
import path from 'node:path'
import { BLUEPRINT_LIMITS, validateBlueprintDocument } from './schema.mjs'
export function blueprintFileArguments (action, args, cwd = process.cwd()) {
  if (!/^(blueprint\.(show|check|build)|village\.(check|maintain))$/.test(action) || args.file === undefined) return args
  if (args.name !== undefined || args.plan !== undefined) throw new Error('blueprint needs exactly one of name=, file= or plan=')
  if (typeof args.file !== 'string') throw new Error('blueprint file= must be a path')
  const file = path.resolve(cwd, args.file), stat = fs.statSync(file)
  if (!stat.isFile() || stat.size > BLUEPRINT_LIMITS.bytes) throw new Error('blueprint file must be a regular JSON file at most 1 MiB')
  const plan = validateBlueprintDocument(JSON.parse(fs.readFileSync(file, 'utf8')))
  const { file: _file, ...rest } = args
  return { ...rest, plan, origin: file }
}
export function readBlueprintSource (args, catalogDir) {
  if (args.file !== undefined) throw new Error('resolve file= in the calling CLI; HTTP callers must submit plan=')
  if (Number(args.name !== undefined) + Number(args.plan !== undefined) !== 1) throw new Error('blueprint needs exactly one of name= or plan=')
  if (args.plan !== undefined) return { document: validateBlueprintDocument(args.plan), origin: args.origin ?? 'inline' }
  if (!/^[a-z0-9][a-z0-9-]{0,63}$/.test(args.name)) throw new Error('invalid blueprint catalog name')
  const file = path.join(catalogDir, `${args.name}.blueprint.json`)
  if (!fs.existsSync(file)) throw new Error(`no v2 blueprint called ${args.name}`)
  if (fs.statSync(file).size > BLUEPRINT_LIMITS.bytes) throw new Error('blueprint catalog file exceeds 1 MiB')
  const document = validateBlueprintDocument(JSON.parse(fs.readFileSync(file, 'utf8')))
  if (document.id !== args.name) throw new Error('blueprint catalog filename and document id differ')
  return { document, origin: `catalog:${args.name}` }
}
export function blueprintDocumentFiles (dir) {
  return fs.existsSync(dir) ? fs.readdirSync(dir).filter(name => name.endsWith('.blueprint.json')).map(name => name.slice(0, -15)).sort() : []
}
export function loadBlueprintDocuments (dir) {
  return blueprintDocumentFiles(dir).map(name => ({ name, ...readBlueprintSource({ name }, dir) }))
}
