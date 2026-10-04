import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'
import { fileURLToPath, pathToFileURL } from 'node:url'
import edn from 'edn-data'
import { context, documentPath, fail, listDocuments, mutateDocument, query, readDocument, rawBound } from './world-data.mjs'
import { defaultStateDir } from './drive-lib.mjs'
import { keyword, readEDN, writeEDN } from './observe-lib.mjs'

const require = createRequire(import.meta.url)
const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..')
const bridgeFile = path.join(repoRoot, 'dashboard', 'out', 'agent-plan-tools.cjs')
const names = /^[A-Za-z0-9_-]{1,100}$/
const statuses = new Set(['proposed', 'active', 'completed', 'retired'])
export const RAW_BYTES = 65536
const rawEdnKey = Symbol('raw-edn')
const rawEdn = text => ({ [rawEdnKey]: text })

export const usage = Object.freeze({
  plans: `usage: plans.mjs --world <world> <command> [options]
  list [--status proposed|active|completed|retired] [--limit 10 --offset 0] [--raw [--large]]
  find <text> [--limit 10 --offset 0] [--raw [--large]] | show <id> [--raw [--large]] [--geometry [--large]]
  add <id> --edn '<plan-map>' --by <name> [--dry-run] [--raw]
  edit <id> --edn '<plan-map>' --by <name> --revision <digest> [--dry-run] [--raw]
  status <id> --status proposed|active|completed|retired --by <name> --revision <digest> [--dry-run] [--raw]
  remove <id> --by <name> --revision <digest> [--dry-run] [--raw]
  validate <id> --edn '<plan-map>' [--blueprint <id>=<blueprint-map>]... [--raw] [--geometry [--large]]
  check <id> [--inventory '<block-count-map>'] [--blueprint <id>=<blueprint-map>]... [--raw] [--geometry [--large]]
  Common: --state <dir> --repo <dir> --limit 1..100 --offset 0..10000
  Large raw output is opt-in with --large; raw output otherwise stops at 64 KiB. Plan/world edits need --by.`,
  blueprints: `usage: blueprints.mjs --world <world> <command> [options]
  list [--limit 10 --offset 0] [--raw [--large]] | find <text> [--limit 10 --offset 0] [--raw [--large]]
  show <id> [--raw [--large]] | save <id> --edn '<blueprint-map>' --by <name> [--revision <digest>] [--dry-run] [--raw]
  validate <id> --edn '<blueprint-map>' [--raw] [--large]
  Common: --state <dir> --repo <dir> --limit 1..100 --offset 0..10000
  Blueprints are shared globally; each result reports :scope :global. Writes need --by. Build the CLJS checks once with ` +
    '`cd dashboard && npm run build-agent-tools`'
})

export function loadBridge () {
  try { return require(bridgeFile) } catch (error) {
    if (error.code === 'MODULE_NOT_FOUND') throw fail('build-required', 'compiled plan checks are missing; run `cd dashboard && npm run build-agent-tools` once')
    throw error
  }
}

export function oneForm (text) {
  if (typeof text !== 'string' || Buffer.byteLength(text) > 8 * 1024 * 1024) throw fail('bad-edn', 'EDN input must be a string at most 8 MiB')
  let parsed
  try { parsed = edn.parseEDNString(`(${text})`) } catch (error) { throw fail('bad-edn', `invalid EDN: ${error.message}`) }
  if (parsed?.list?.length !== 1) throw fail('bad-edn', 'give exactly one EDN form')
  return readEDN(text)
}

export function blueprintForm (arg) {
  const at = arg.indexOf('=')
  if (at < 1) throw fail('bad-blueprint', '--blueprint must be <id>=<EDN map>')
  const id = arg.slice(0, at)
  if (!names.test(id)) throw fail('invalid-id', `invalid blueprint ID ${id}`)
  const value = oneForm(arg.slice(at + 1))
  if (!value || typeof value !== 'object' || Array.isArray(value) || value.map) throw fail('bad-blueprint', 'inline blueprint must be one map')
  return { id, text: arg.slice(at + 1), value }
}

function ednValue (text, field) {
  const value = oneForm(text)
  if (!value || typeof value !== 'object' || Array.isArray(value) || value.map) throw fail(`bad-${field}`, `--${field} must be one EDN map`)
  return value
}

function parsedArgs (kind, argv) {
  const { parseArgs } = require('node:util')
  const p = parseArgs({ args: argv, allowPositionals: true, options: {
    world: { type: 'string' }, state: { type: 'string', default: defaultStateDir }, repo: { type: 'string', default: repoRoot },
    by: { type: 'string' }, revision: { type: 'string' }, edn: { type: 'string' }, status: { type: 'string' },
    limit: { type: 'string' }, offset: { type: 'string' }, query: { type: 'string' }, inventory: { type: 'string' },
    blueprint: { type: 'string', multiple: true }, raw: { type: 'boolean', default: false }, geometry: { type: 'boolean', default: false },
    large: { type: 'boolean', default: false }, 'dry-run': { type: 'boolean', default: false }
  } })
  const [command, id, ...extra] = p.positionals
  const v = p.values
  if (!v.world || !/^[A-Za-z0-9_-]{1,64}$/.test(v.world)) throw fail('invalid-world', 'supply an explicit valid --world')
  const allowedCommands = kind === 'plan' ? ['list', 'find', 'show', 'add', 'edit', 'remove', 'status', 'validate', 'check'] : ['list', 'find', 'show', 'save', 'validate']
  if (!allowedCommands.includes(command)) throw fail('usage', `unknown ${kind} command ${command ?? ''}`)
  const noId = ['list'].includes(command)
  if (noId ? id !== undefined || extra.length : command === 'find' ? id === undefined || extra.length : id === undefined || extra.length) throw fail('usage', `${command} ${noId ? 'takes no ID' : command === 'find' ? 'needs one search string' : 'needs exactly one ID'}`)
  if (id !== undefined && command !== 'find' && !names.test(id)) throw fail('invalid-id', 'ID must use letters, digits, _ or - (up to 100)')
  const allowed = new Set(['world', 'state', 'repo'])
  const add = (...keys) => keys.forEach(k => allowed.add(k))
  if (['list', 'find'].includes(command)) add('limit', 'offset', 'raw', 'large', ...(command === 'list' && kind === 'plan' ? ['status'] : []))
  if (command === 'show') add('raw', 'geometry', 'large')
  if (command === 'validate') add('edn', ...(kind === 'plan' ? ['blueprint', 'limit', 'offset'] : ['limit', 'offset']), 'raw', 'geometry', 'large')
  if (command === 'check') add('inventory', 'blueprint', 'raw', 'geometry', 'large', 'limit', 'offset')
  if (['add', 'edit', 'save'].includes(command)) add('edn', 'by', 'revision', 'dry-run', 'raw', 'large')
  if (['remove', 'status'].includes(command)) add('by', 'revision', 'dry-run', 'raw', 'large', ...(command === 'status' ? ['status'] : []))
  for (const key of Object.keys(v)) if (!allowed.has(key) && v[key] !== false) throw fail('bad-option', `--${key} is not valid for ${command}`)
  if (v.limit !== undefined && (!Number.isInteger(Number(v.limit)) || Number(v.limit) < 1 || Number(v.limit) > 100)) throw fail('bad-page', '--limit must be 1..100')
  if (v.offset !== undefined && (!Number.isInteger(Number(v.offset)) || Number(v.offset) < 0 || Number(v.offset) > 10000)) throw fail('bad-page', '--offset must be 0..10000')
  if (v.large && !v.raw && !v.geometry) throw fail('bad-option', '--large requires --raw or --geometry')
  if (v.geometry && command !== 'show' && command !== 'validate' && command !== 'check') throw fail('bad-option', '--geometry is only valid for show, validate or check')
  if (v['dry-run'] && !['add', 'edit', 'save', 'remove', 'status'].includes(command)) throw fail('bad-option', '--dry-run is only valid for mutations')
  if (['add', 'edit', 'save', 'validate'].includes(command) && v.edn === undefined) throw fail('bad-option', `${command} requires --edn`)
  if (['add', 'edit', 'save', 'remove', 'status'].includes(command) && (!v.by || !v.by.trim() || v.by.length > 80)) throw fail('actor-required', 'mutations require --by (up to 80 characters)')
  if (command === 'status' && !statuses.has(v.status)) throw fail('bad-status', '--status must be proposed, active, completed, or retired')
  if (command === 'add' && v.revision !== undefined) throw fail('bad-option', 'add is create-only and does not take --revision')
  if (['edit', 'remove', 'status'].includes(command) && v.revision === undefined) throw fail('revision-required', `${command} requires the --revision from list/show`)
  if (v.inventory !== undefined && command !== 'check') throw fail('bad-option', '--inventory is only valid for check')
  if (v.blueprint?.length && !(kind === 'plan' && ['validate', 'check'].includes(command))) throw fail('bad-option', '--blueprint is only valid for plan validate/check')
  if (command === 'status' && kind !== 'plan') throw fail('bad-option', 'status applies only to plans')
  if (v.status && command !== 'status' && !(kind === 'plan' && command === 'list')) throw fail('bad-option', '--status is only valid for plan status/list')
  if (command === 'find') return { kind, command, search: id, ...v }
  return { kind, command, id, ...v }
}

export function requestFor (kind, argv) {
  try { return parsedArgs(kind, argv) } catch (error) { return { error: error.message, reason: error.reason ?? 'bad-request' } }
}

function bridgeCall (fn, ...args) {
  const bridge = loadBridge()
  try { return bridge[fn](...args) } catch (error) { throw fail('plan-check-failed', error.message) }
}

function docText (documents) { return JSON.stringify(documents.filter(Boolean).map(({ id, text }) => ({ id, text }))) }
function planRaw (doc, world, large) {
  const value = { ok: true, kind: keyword('plan'), id: doc.id, world, scope: keyword('world'), revision: doc.revision, document: rawEdn(doc.text) }
  return value
}
function blueprintRaw (doc, large) {
  const value = { ok: true, kind: keyword('blueprint'), id: doc.id, scope: keyword('global'), revision: doc.revision, document: rawEdn(doc.text) }
  return value
}

function errorsPage (errors, offset = 0, limit = 10) {
  const items = errors.slice(offset, offset + limit)
  return { total: errors.length, items, ...(offset + items.length < errors.length ? { 'next-offset': offset + items.length } : {}) }
}
function planSummary (doc, large = false) {
  const p = doc.value
  const parts = Array.isArray(p.parts) ? p.parts : []
  return { id: doc.id, revision: doc.revision, status: p.status, note: p.note, parts: parts.length,
    ...(large ? { document: p } : {}) }
}
function blueprintSummary (doc, errors = []) {
  const bp = doc.value
  return { id: doc.id, scope: keyword('global'), revision: doc.revision, title: bp.title, front: bp.front,
    width: bp.layers?.[0]?.[0]?.length ?? 0, depth: bp.layers?.[0]?.length ?? 0, height: bp.layers?.length ?? 0,
    errors: errorsPage(errors) }
}

function currentDocs (ctx, kind) { return listDocuments(ctx, kind).filter(Boolean) }
function bridgePrepare (planText, id, blueprints, plans, zonesText, claims) {
  return bridgeCall('prepare', planText, id, docText(blueprints), docText(plans), zonesText, JSON.stringify(claims))
}

function centerOf (region) { return region?.min?.map((n, i) => (n + region.max[i]) / 2) ?? [0, 0, 0] }
function radiusOf (region) { return region ? Math.hypot(...region.min.map((n, i) => (region.max[i] - n) / 2)) : 0 }

async function checkPlan (ctx, planText, id, inlineBlueprints, { inventory, geometry, large, offset = 0, limit = 10 } = {}) {
  const blueprints = currentDocs(ctx, 'blueprint')
  const plans = currentDocs(ctx, 'plan')
  const byId = new Map(blueprints.map(d => [d.id, d]))
  for (const doc of inlineBlueprints ?? []) byId.set(doc.id, doc)
  const zoneFile = documentPath(ctx, 'zone', 'collection')
  let zonesText = null
  try { zonesText = fs.readFileSync(zoneFile, 'utf8') } catch (error) { if (error.code !== 'ENOENT') throw error }
  const initial = bridgePrepare(planText, id, [...byId.values()], plans, zonesText, [])
  const rawClaims = currentDocs(ctx, 'claim')
  const center = centerOf(initial.region)
  const radius = radiusOf(initial.region)
  const first = query(rawClaims, { type: 'claim', status: 'active', center, radius, limit: 100 })
  const claims = [...first.items]
  for (let offset = 100; offset < first.total; offset += 100) claims.push(...query(rawClaims, { type: 'claim', status: 'active', center, radius, limit: 100, offset }).items)
  const prepared = bridgePrepare(planText, id, [...byId.values()], plans, zonesText, claims)
  if (!prepared.ok || !prepared['expansion-edn']) return { ...prepared, world: ctx.world, worldEvidence: 'saved-column-dumps', liveLoaded: false }
  if (prepared.cells.length > (large ? 200000 : 1000)) throw fail('geometry-limit', `plan expands to ${prepared.cells.length} cells; ${large ? 'the shared shape limit is 200000 cells' : 'use --large to inspect up to 200000 cells'}`)
  const worldblocksPath = path.join(repoRoot, 'dashboard', 'js', 'worldblocks.mjs')
  const worldblocks = await import(pathToFileURL(worldblocksPath).href)
  const columns = worldblocks.createWorldBlocks({ stateDir: ctx.state, world: ctx.world })
  const blocks = {}
  const mtimes = {}
  try {
    for (const pos of prepared.cells) {
      const [x, y, z] = pos
      const block = columns.blockAt(x, y, z)
      if (block) blocks[pos.join(',')] = block
      const cx = Math.floor(x / 16), cz = Math.floor(z / 16)
      const key = `${cx},${cz}`
      if (!(key in mtimes)) {
        const file = path.join(ctx.columnsDir, `${cx}.${cz}.bin`)
        try { mtimes[key] = fs.statSync(file).mtimeMs } catch (error) { if (error.code !== 'ENOENT') throw error }
      }
    }
  } finally { columns.close() }
  const inv = inventory === undefined ? {} : inventory
  const scored = bridgeCall('score', prepared['expansion-edn'], JSON.stringify(blocks), JSON.stringify(mtimes), JSON.stringify(inv), inventory !== undefined, offset, limit)
  const { cells, 'expansion-edn': _expansion, ...detail } = prepared
  const checked = scored.checked ?? {}
  scored.checked = { ...checked,
    'newest-age-ms': Number.isFinite(checked.newest) ? Math.max(0, checked.now - checked.newest) : null,
    freshness: checked.dumped < checked.chunks ? keyword('incomplete-saved-dumps') : keyword('saved-dumps') }
  if (scored.materials?.availability) scored.materials.availability = keyword(scored.materials.availability)
  if (scored.materials?.basis) scored.materials.basis = keyword(scored.materials.basis)
  return { ...detail, world: ctx.world, evidence: keyword('saved-column-dumps'), 'live-loaded?': false, score: scored,
    ...(geometry ? { geometry: cells } : {}), ...(large ? { large: true } : {}) }
}

function page (values, limit, offset) {
  const items = values.slice(offset, offset + limit)
  return { total: values.length, items, ...(offset + items.length < values.length ? { 'next-offset': offset + items.length } : {}) }
}

function statusName (value) { return value?.key ?? value }
function formatError (error, sent = false) {
  return { ok: false, reason: keyword(error.reason ?? 'internal-error'), message: String(error.message ?? error).slice(0, 240),
    ...(error.errors ? { errors: error.errors.slice(0, 10), ...(error.errors.length > 10 ? { 'next-offset': 10 } : {}) } : {}),
    ...(error.current ? { current: error.current } : {}), ...(sent ? { confirmation: keyword('unknown') } : {}) }
}

function printValue (value, large) {
  try {
    const raw = []
    const scrub = value => {
      if (value && typeof value === 'object' && rawEdnKey in value) {
        const token = `RAW_EDN_${raw.length}_${Math.random().toString(36).slice(2)}_END`
        raw.push([token, value[rawEdnKey]])
        return token
      }
      if (Array.isArray(value)) return value.map(scrub)
      if (value && typeof value === 'object') return Object.fromEntries(Object.entries(value).map(([k, v]) => [k, scrub(v)]))
      return value
    }
    let output = writeEDN(rawBound(scrub(value), large ? 8 * 1024 * 1024 : RAW_BYTES))
    for (const [token, fragment] of raw) output = output.replace(JSON.stringify(token), fragment)
    if (Buffer.byteLength(output) > (large ? 8 * 1024 * 1024 : RAW_BYTES)) throw fail('output-too-large', `raw result exceeds ${large ? 8 * 1024 * 1024 : RAW_BYTES} bytes; scope or page the query`)
    return `${output}\n`
  } catch (error) {
    return `${writeEDN({ ok: false, reason: keyword(error.reason ?? 'output-too-large'), message: String(error.message).slice(0, 240), next: keyword('add --large or request a smaller page') })}\n`
  }
}

function inventoryValue (text) {
  if (text === undefined) return undefined
  const value = ednValue(text, 'inventory')
  for (const [item, count] of Object.entries(value)) {
    const name = item.replace(/^:/, '')
    if (!/^(?:minecraft:)?[a-z][a-z0-9_]*$/.test(name) || !Number.isSafeInteger(count) || count < 0) {
      throw fail('bad-inventory', 'inventory is a map of block/item names to nonnegative integer counts')
    }
    if (name.startsWith('minecraft:')) { delete value[item]; value[name.slice(10)] = count }
  }
  return value
}

function inlineBlueprints (values = []) { return values.map(blueprintForm) }

function validPlan (id, text, ctx, blueprintsOverride = []) {
  const bps = new Map(currentDocs(ctx, 'blueprint').map(d => [d.id, d]))
  for (const d of blueprintsOverride) bps.set(d.id, d)
  const plans = currentDocs(ctx, 'plan')
  const result = bridgePrepare(text, id, [...bps.values()], plans, null, [])
  return result.ok ? null : { errors: result.errors }
}

function validateBlueprintValue (id, text) {
  const result = bridgeCall('validateBlueprint', text, id)
  return result.errors?.length ? { errors: result.errors } : null
}

function planDocumentOutput (doc, ctx, raw, large) {
  if (raw) return planRaw(doc, ctx.world, large)
  const validation = bridgeCall('validatePlan', doc.text, doc.id)
  return { ...planSummary(doc), validation: errorsPage(validation.errors ?? []), scope: keyword('world') }
}

function blueprintDocumentOutput (doc, raw, large) {
  if (raw) return blueprintRaw(doc, large)
  const validation = bridgeCall('validateBlueprint', doc.text, doc.id)
  return blueprintSummary(doc, validation.errors ?? [])
}

function listResult(kind, records, req) {
  const status = req.status
  const search = req.search?.toLowerCase()
  const filtered = records.filter(doc => {
    const v = doc.value
    if (status && statusName(v.status) !== status) return false
    if (search && !`${doc.id} ${v.note ?? ''} ${v.title ?? ''} ${v.status?.key ?? ''}`.toLowerCase().includes(search)) return false
    return true
  })
  const limit = Number(req.limit ?? 10), offset = Number(req.offset ?? 0)
  const chosen = filtered.slice(offset, offset + limit).map(doc => req.raw
    ? kind === 'plan'
      ? { ...planSummary(doc), world: req.world, scope: keyword('world'), document: rawEdn(doc.text) }
      : { ...blueprintSummary(doc), document: rawEdn(doc.text) }
    : kind === 'plan' ? planSummary(doc) : blueprintSummary(doc, []))
  return { ok: true, kind: keyword(kind), ...(kind === 'blueprint' ? { scope: keyword('global') } : { world: req.world }), ...page(chosen, chosen.length, 0), total: filtered.length,
    ...(offset + chosen.length < filtered.length ? { 'next-offset': offset + chosen.length, 'more?': true } : {}) }
}

async function mutatePlan (req, ctx, write) {
  const old = readDocument(ctx, 'plan', req.id)
  const { command, by, revision: supplied } = req
  if (command === 'add' && old) throw fail('already-exists', `plan ${req.id} already exists; use edit`)
  if (['edit', 'remove', 'status'].includes(command) && !old) throw fail('not-found', `no plan ${req.id} in ${ctx.world}`)
  if (supplied !== undefined && !/^[a-f0-9]{24}$/.test(supplied)) throw fail('bad-revision', '--revision must be the 24-character digest from show/list')
  const expectedRevision = command === 'add' ? null : supplied ?? old?.revision ?? null
  let value = command === 'remove' ? null : command === 'status' ? { ...old.value, status: keyword(req.status) } : ednValue(req.edn, 'edn')
  if (value !== null && (!value || typeof value !== 'object' || Array.isArray(value) || value.map)) throw fail('bad-plan', 'plan EDN must be one map')
  if (value !== null && value.id !== req.id) throw fail('bad-plan', `plan :id must be "${req.id}"`)
  const source = command === 'status' ? writeEDN(value) : req.edn
  const validate = value === null ? undefined : async candidate => validPlan(req.id, source, ctx)
  const result = await mutateDocument(ctx, { kind: 'plan', id: req.id, expectedRevision, value,
    ...(command !== 'status' ? { text: req.edn } : {}), by, validate, dryRun: Boolean(req['dry-run']) })
  const updated = command === 'remove' ? old : readDocument(ctx, 'plan', req.id)
  const rawDocument = req['dry-run'] && value !== null ? source : updated?.text
  write({ ...result, world: ctx.world, ...(req['dry-run'] ? { next: keyword('review-preview') } : {}),
    ...(req.raw && rawDocument ? { document: rawEdn(rawDocument) } : {}) })
  return 0
}

async function mutateBlueprint (req, ctx, write) {
  const old = readDocument(ctx, 'blueprint', req.id)
  if (req.revision !== undefined && !/^[a-f0-9]{24}$/.test(req.revision)) throw fail('bad-revision', '--revision must be the 24-character digest from show/list')
  if (old && req.revision === undefined) throw fail('revision-required', 'updating a blueprint requires the --revision from list/show')
  const expectedRevision = req.revision ?? old?.revision ?? null
  const value = ednValue(req.edn, 'edn')
  if (!value || typeof value !== 'object' || Array.isArray(value) || value.map) throw fail('bad-blueprint', 'blueprint EDN must be one map')
  if (value.id !== req.id) throw fail('bad-blueprint', `blueprint :id must be "${req.id}"`)
  const validate = async candidate => validateBlueprintValue(req.id, req.edn)
  const result = await mutateDocument(ctx, { kind: 'blueprint', id: req.id, expectedRevision, value, text: req.edn, by: req.by, validate, dryRun: Boolean(req['dry-run']) })
  write({ ...result, next: req['dry-run'] ? keyword('review-preview') : keyword('show'), ...(req.raw ? { document: rawEdn(req.edn) } : {}) })
  return 0
}

async function executePlan (req, write) {
  const ctx = context({ state: req.state, world: req.world, repoRoot: req.repo })
  const limit = Number(req.limit ?? 10), offset = Number(req.offset ?? 0)
  if (['list', 'find'].includes(req.command)) {
    const records = currentDocs(ctx, 'plan')
    const result = listResult('plan', records, req)
    write(result)
    return 0
  }
  if (['add', 'edit', 'remove', 'status'].includes(req.command)) return mutatePlan(req, ctx, write)
  if (req.command === 'show') {
    const doc = readDocument(ctx, 'plan', req.id)
    if (!doc) throw fail('not-found', `no plan ${req.id} in ${ctx.world}`)
    const result = planDocumentOutput(doc, ctx, req.raw, req.large)
    if (req.geometry) {
      const prepared = bridgePrepare(doc.text, doc.id, currentDocs(ctx, 'blueprint'), currentDocs(ctx, 'plan'), null, [])
      if (!prepared.ok) { result.validation = errorsPage(prepared.errors); write(result); return 1 }
      if (prepared.cells.length > (req.large ? 200000 : 1000)) throw fail('geometry-limit', 'use --large for more than 1000 cells')
      result.geometry = prepared.cells
    }
    write(result)
    return 0
  }
  if (req.command === 'validate' || req.command === 'check') {
    const source = req.command === 'check' ? readDocument(ctx, 'plan', req.id)?.text : req.edn
    if (source === undefined || source === null) throw fail('not-found', `no plan ${req.id}; validate an inline plan with --edn`)
    const parsed = req.command === 'validate' ? ednValue(req.edn, 'edn') : readDocument(ctx, 'plan', req.id).value
    if (parsed.id !== req.id) throw fail('bad-plan', `plan :id must be "${req.id}"`)
    const result = req.command === 'check'
      ? await checkPlan(ctx, source, req.id, inlineBlueprints(req.blueprint), { inventory: inventoryValue(req.inventory), geometry: req.geometry, large: req.large, offset, limit })
      : bridgePrepare(source, req.id, [...new Map([...currentDocs(ctx, 'blueprint'), ...inlineBlueprints(req.blueprint)].map(d => [d.id, d])).values()], currentDocs(ctx, 'plan'), null, [])
    if (req.command === 'check') result.revision = readDocument(ctx, 'plan', req.id).revision
    const { 'expansion-edn': _expansion, cells, ...compact } = result
    for (const field of ['conflicts', 'claims']) if (Array.isArray(compact[field])) compact[field] = page(compact[field], limit, offset)
    if (compact.zones?.overlaps) compact.zones = { ...compact.zones, overlaps: page(compact.zones.overlaps, limit, offset) }
    if (Array.isArray(compact.parts)) compact.parts = page(compact.parts, limit, offset)
    if (req.command === 'check') {
      compact.errors = errorsPage(compact.errors ?? [], offset, limit)
      if (req.geometry) compact.geometry = cells
    } else {
      compact.errors = errorsPage(compact.errors ?? [], offset, limit)
      if (req.geometry) {
        if (cells.length > (req.large ? 200000 : 1000)) throw fail('geometry-limit', 'use --large for more than 1000 cells')
        compact.geometry = cells
      }
    }
    if (compact['blueprint-errors']?.length) compact['blueprint-errors'] = errorsPage(compact['blueprint-errors'], offset, limit)
    if (compact['plan-errors']?.length) compact['plan-errors'] = errorsPage(compact['plan-errors'], offset, limit)
    if (req.raw) compact.document = rawEdn(source)
    write(compact)
    return result.ok === false ? 1 : 0
  }
  throw fail('usage', 'unknown plan command')
}

async function executeBlueprint (req, write) {
  const ctx = context({ state: req.state, world: req.world, repoRoot: req.repo })
  if (['list', 'find'].includes(req.command)) {
    write(listResult('blueprint', currentDocs(ctx, 'blueprint'), req))
    return 0
  }
  if (req.command === 'show') {
    const doc = readDocument(ctx, 'blueprint', req.id)
    if (!doc) throw fail('not-found', `no global blueprint ${req.id}`)
    write(blueprintDocumentOutput(doc, req.raw, req.large))
    return 0
  }
  if (req.command === 'save') return mutateBlueprint(req, ctx, write)
  if (req.command === 'validate') {
    const value = ednValue(req.edn, 'edn')
    if (value.id !== req.id) throw fail('bad-blueprint', `blueprint :id must be "${req.id}"`)
    const result = bridgeCall('validateBlueprint', req.edn, req.id)
    const output = { ok: !(result.errors?.length), id: req.id, scope: keyword('global'), errors: errorsPage(result.errors ?? [], offsetOf(req), Number(req.limit ?? 10)) }
    if (req.raw) output.document = rawEdn(req.edn)
    write(output)
    return output.ok ? 0 : 1
  }
  throw fail('usage', 'unknown blueprint command')
}

function offsetOf (req) { return Number(req.offset ?? 0) }

export async function execute (kind, argv, output = () => {}) {
  const req = requestFor(kind, argv)
  const emit = value => output(printValue(value, req.large))
  if (req.error) { emit({ ok: false, reason: keyword(req.reason), message: req.error, usage: usage[kind] }); return 2 }
  try { return kind === 'plan' ? await executePlan(req, emit) : await executeBlueprint(req, emit) } catch (error) { emit(formatError(error)); return error.reason === 'not-found' ? 1 : 2 }
}
