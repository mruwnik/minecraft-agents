// The blueprint format's walking half (docs/superpowers/specs/2026-09-26-blueprint-format-design.md, section 3): the
// files in blueprints/, the anchor a build is judged at, and the build itself. Every judgement is src/blueprint/format.mjs's;
// this walks to each job's standing cell, calls the primitive, and looks at the cell afterwards. The world is the only
// progress record: a run recomputes its jobs from what stands, so a second run after a stop, a death or a restart picks
// up where the blocks say. In the style of src/build/plan.mjs, and built on the same api.
import fs from 'node:fs'
import { representativeBlueprint } from './palette.mjs'
export { representativeBlueprint } from './palette.mjs'
import { readBlueprintSource, blueprintDocumentFiles } from './source.mjs'
import { canonicalBlueprint, semanticBlueprintHash } from './schema.mjs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { parseBlueprint, resolve, rotate, turnsFor, lint, bill, counts, enclosure, renderLayer, flatGround, jobsFor, orderJobs, stages, shortfall, stageLine, siteCheck, blueprintHash, buildNote, parseNote, matchesCell, blueprintCells, isSecondPart, isAir, faceWord, alongFace, hashMatches, CARRY_MARGIN, DEFAULT_SCAFFOLD, DEFAULT_FILL, DIRS, scaffoldMatches } from './format.mjs'
import { hasWaterSource, mapRefusal, workRefusal, shortLine } from '../lib.mjs'
import { canPlaceFromHere } from '../lib/place.mjs'
import { placementSight } from './visibility.mjs'
import { REGISTRY } from './format.mjs'

export const BLUEPRINT_DIR = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', '..', '..', 'blueprints')

// ---------------------------------------------------------------- the library on disk

export const blueprintFiles = (dir = BLUEPRINT_DIR) => blueprintDocumentFiles(dir)
export const readBlueprint = (name, dir = BLUEPRINT_DIR) => {
  if (!blueprintFiles(dir).includes(String(name))) throw new Error(`no blueprint called ${name}: blueprint.list shows ${blueprintFiles(dir).join(', ') || 'nothing: blueprints/ is empty'}`)
  const { document } = readBlueprintSource({ name }, dir)
  return { name: String(name), document, text: canonicalBlueprint(document), hash: semanticBlueprintHash(document) }
}
export const loadAll = (dir = BLUEPRINT_DIR) => blueprintFiles(dir).map(name => readBlueprint(name, dir))

// ---------------------------------------------------------------- concrete-engine compatibility arguments
// Injected text fixtures still exercise the concrete engine. Public commands
// exclusively use v2 documents and durable manifests, never this legacy dispatch.

// what the composites take that is not a blueprint parameter: everything else given is one, and resolve refuses it by
// the blueprint's own list when it is not
const OWN_ARGS = new Set(['name', 'place', 'x', 'y', 'z', 'facing', 'supply', 'clear', 'partial', 'layer', 'tag', 'q', 'until', 'timeout', 'force', 'dig'])
export const paramsOf = a => Object.fromEntries(Object.entries(a).filter(([k, v]) => !OWN_ARGS.has(k) && v !== undefined).map(([k, v]) => [k, String(v)]))

// the file parsed, resolved with its parameters and turned to face `facing`, with what lint and the placement table say
export function prepare ({ name, facing, params = {} }, read = readBlueprint) {
  const { text, hash, document } = read(name)
  const parsed = document ? representativeBlueprint(document) : parseBlueprint(text)
  if (document && Object.keys(params).length) throw new Error('blueprint v2 material choices belong in plan.materials, not legacy parameters')
  if (parsed.errors.length) throw new Error(`${name} does not parse: ${parsed.errors[0]}${parsed.errors.length > 1 ? ` (and ${parsed.errors.length - 1} more)` : ''}`)
  const way = facing ?? parsed.front
  if (!DIRS.includes(way)) throw new Error(`facing=${way} is not a direction: ${DIRS.join(', ')}`)
  const bp = rotate(resolve(parsed, params), turnsFor(parsed.front, way))
  const checked = lint(bp)
  return { bp, text, hash, front: parsed.front, facing: way, params, lint: checked }
}

// the supply chest: x,y,z or the name of a marked place
export const supplyOf = (api, supply) => {
  if (supply === undefined) return null
  const m = /^(-?\d+),(-?\d+),(-?\d+)$/.exec(String(supply).trim())
  if (m) return { x: Number(m[1]), y: Number(m[2]), z: Number(m[3]), label: `${m[1]},${m[2]},${m[3]}` }
  const place = api.places().find(p => p.name === String(supply))
  if (!place) throw new Error(`supply=${supply} is neither x,y,z nor a marked place: ./mc places lists them`)
  return { x: place.x, y: place.y, z: place.z, label: `${place.x},${place.y},${place.z} (${place.name})` }
}

const cellText = c => `${c.x},${c.y},${c.z}`

// where a build stands and what it is: a marked build resumes from its own note (blueprint, facing, parameters, the
// file's hash); a new one is a blueprint at an anchor, the north-west corner of its y0 layer
export function siteOf (api, a, read, command = 'blueprint.build') {
  const saved = a.place === undefined ? null : api.places().find(p => p.name === String(a.place))
  const note = saved ? parseNote(saved.note) : null
  // somebody else's build goes on only when its note invites work (the one question every composite asks, #144); a
  // place of theirs that is no build would be renamed into one, and that is their record to change
  const refusal = !saved ? null : note ? workRefusal(saved, api.me()) : mapRefusal(saved, api.me())
  if (refusal) throw new Error(refusal)
  if (note && a.name !== undefined && a.name !== note.blueprint) throw new Error(`${saved.name} was started from ${note.blueprint}: run ${command} place=${saved.name} alone to go on with it, or mark a new name`)
  if (note && a.x !== undefined && (a.x !== saved.x || a.y !== saved.y || a.z !== saved.z)) throw new Error(`${saved.name} is marked at ${cellText(saved)}: run ${command} place=${saved.name} without x= y= z= to go on with it, or mark a new name`)
  // name= and the anchor restated over a build's own mark start it afresh from the file as it is now (the mark is
  // written again); place= alone goes on with the file the mark was made from
  const restated = note && a.name !== undefined && a.x !== undefined
  if (note && !restated) {
    const prep = prepare({ name: note.blueprint, facing: note.facing, params: note.params }, read)
    if (!hashMatches(note.hash, prep.hash)) throw new Error(`the blueprint changed since this build started: ${saved.name} was marked from ${note.blueprint} h=${note.hash} and the file is now h=${prep.hash}; run ${command} name=${note.blueprint} x=${saved.x} y=${saved.y} z=${saved.z} place=${saved.name} to go on with the new version`)
    return { ...prep, at: { x: saved.x, y: saved.y, z: saved.z }, saved, resume: true }
  }
  if (a.name === undefined) throw new Error(`${command} needs name= x= y= z= (a blueprint at its anchor), or place=<a build already marked>`)
  const missing = ['x', 'y', 'z'].find(k => typeof a[k] !== 'number')
  if (missing) throw new Error(`${command} needs ${missing}=: the anchor is the north-west corner of the y0 layer`)
  const prep = prepare({ name: String(a.name), facing: a.facing, params: paramsOf(a) }, read)
  return { ...prep, at: { x: Math.floor(a.x), y: Math.floor(a.y), z: Math.floor(a.z) }, saved, resume: false }
}

// ---------------------------------------------------------------- list and show

const itemsLine = items => Object.entries(items).map(([k, n]) => `${k}:${n}`).join(' ')
const sum = items => Object.values(items).reduce((n, v) => n + v, 0)
const height = bp => bp.layers.at(-1).y - bp.layers[0].y + 1

// one line per file: name, size, tags, total items, title. tag= keeps the ones tagged so, q= the ones whose name,
// title or description say it
export function listText ({ tag, q } = {}, files = loadAll()) {
  const lines = files.map(({ name, text }) => {
    const bp = (() => { try { return JSON.parse(text).schemaVersion === 2 ? representativeBlueprint(JSON.parse(text)) : parseBlueprint(text) } catch { return parseBlueprint(text) } })()
    if (bp.errors.length) return { name, line: `${name}: does not parse (${bp.errors[0]})`, tags: [], words: name }
    const words = `${name} ${bp.title} ${bp.description}`.toLowerCase()
    return { name, tags: bp.tags, words, line: `${name} ${bp.width}x${bp.depth}x${height(bp)} tags=${bp.tags.join(',')} items=${sum(bill(bp).total)} ${bp.title}` }
  })
  const kept = lines.filter(l => (tag === undefined || l.tags.includes(String(tag))) && (q === undefined || l.words.includes(String(q).toLowerCase())))
  if (!kept.length) return files.length ? `no blueprint matches${tag !== undefined ? ` tag=${tag}` : ''}${q !== undefined ? ` q=${q}` : ''}: ${files.map(f => f.name).join(', ')}` : 'no blueprints: blueprints/ is empty'
  return kept.map(l => l.line).join('\n')
}

// the stages a fresh build of this blueprint would take over flat ground, for the inventory as it is now
const flatStages = (api, bp) => {
  const at = { x: 0, y: 0, z: 0 }
  const { jobs } = orderJobs(jobsFor(bp, at, flatGround(-1)), bp, at, flatGround(-1))
  return stagesOf(jobs, api, bp)
}

// everything a driver wants to know before choosing a blueprint: metadata, the bill per layer and in total, the stages
// for the current inventory, what lint says, and the layers as they will stand after facing= turns them
export function showText (api, a, read = readBlueprint) {
  const found = prepare({ name: String(a.name), facing: a.facing, params: paramsOf(a) }, read)
  const { bp } = found
  const cost = bill(bp)
  const kinds = counts(bp)
  const room = enclosure(bp)
  const wanted = a.layer === undefined ? bp.layers.map(l => l.y) : [Number(a.layer)]
  const params = Object.entries(bp.params).map(([k, v]) => `${k}=${found.params[k] ?? v}`).join(' ')
  const lines = [
    `${bp.name}: ${bp.title} (${bp.width}x${bp.depth}x${height(bp)}, y${bp.layers[0].y}..y${bp.layers.at(-1).y}, front=${found.front} facing=${found.facing}, foundation=${bp.foundation}, clearance=${bp.clearance})`,
    bp.description,
    `tags=${bp.tags.join(',')}${params ? ` params: ${params}` : ''}${bp.difficulty ? ` difficulty=${bp.difficulty}` : ''}${bp.by ? ` by=${bp.by}` : ''}`,
    ...(bp.notes ? [`notes: ${bp.notes}`] : []),
    `doors=${kinds.doors} beds=${kinds.beds} containers=${kinds.containers} workstations=${kinds.workstations} lights=${kinds.lights} enclosed=${room.enclosed} lit=${room.lit} spawnSafe=${room.spawnSafe}`,
    ...cost.layers.map(l => `y${l.y}: ${itemsLine(l.items) || 'nothing'}`),
    `total: ${itemsLine(cost.total)}${cost.tools.length ? ` tools=${cost.tools.join(',')}` : ''}${cost.scaffold ? ` scaffold=${itemsLine(cost.scaffold)} (left outside the build)` : ''}`,
    ...flatStages(api, bp).map(stage => stageLine(stage, api.inv())),
    ...found.lint.errors.map(e => `error: ${e}`),
    ...found.lint.warnings.map(w => `warning: ${w}`),
    ...wanted.map(y => renderLayer(bp, y))
  ]
  return lines.join('\n')
}

// ---------------------------------------------------------------- the jobs at a site

const scaffoldItem = bp => bp.params.scaffold ?? DEFAULT_SCAFFOLD

// what the world still needs, in the order of work, and what the site says: a foundation=any blueprint fills the
// ground under its lowest layer first
export function planJobs (api, { bp, at, saved }, { clear = false } = {}) {
  const worldAt = api.block
  // the build's own mark is the one place it never overlaps, whoever holds it
  const places = api.places().filter(p => p.name !== saved?.name)
  const site = siteCheck(bp, at, worldAt, { zones: api.zones?.() ?? [], places, me: api.me(), clear })
  const fill = bp.params.fill ?? DEFAULT_FILL
  const foundation = site.foundation.filter(f => f.fill).map(f => ({ x: f.x, y: f.y, z: f.z, token: 'foundation', tags: [], layer: bp.layers[0].y - 1, do: 'place', item: fill, block: { name: fill, states: {} }, class: 'full' }))
  const { jobs, unreachable } = orderJobs([...foundation, ...jobsFor(bp, at, worldAt)], bp, at, worldAt)
  return { site, jobs, unreachable }
}

// the blueprint's blocks already standing: a `.` is no block, and a door's upper half is its lower half's doing
export const standingCount = (bp, at, worldAt) => blueprintCells(bp).filter(({ dx, dy, dz, spec }) => {
  const alt = spec.alts[0]
  return !isSecondPart(alt) && !isAir(alt.name) && matchesCell(worldAt(at.x + dx, at.y + dy, at.z + dz), spec.alts)
}).length

const carryOf = api => Math.max(1, api.freeSlots() - CARRY_MARGIN)
const stagesOf = (jobs, api, bp) => stages(jobs, carryOf(api), undefined, scaffoldItem(bp))
const siteLine = site => [
  `obstacles=${site.obstacles.length}${site.obstacles.length ? ` (${site.obstacles.map(o => `${o.name} ${cellText(o)}`).join('; ')})` : ''}`,
  `foundation=${site.foundation.length ? site.foundation.map(f => `${f.name} ${cellText(f)}${f.fill ? ` (${f.fill} goes in)` : ''}`).join('; ') : 'ok'}`,
  `clearance=${site.clearance.length ? site.clearance.map(c => `${c.name} ${cellText(c)}`).join('; ') : 'ok'}`,
  `overlaps=${site.overlaps.length}${site.overlaps.length ? ` (${site.overlaps.join('; ')})` : ''}`,
  `unloaded=${site.unloaded.length}${site.unloaded.length ? ` (${cellText(site.unloaded[0])} ...)` : ''}`
].join(' ')

// everything build would refuse over, nothing built: the stage table first, so the driver fetches everything before
// starting, then the site and the lint
export async function checkBlueprint (api, a, io = {}) {
  const read = io.read ?? readBlueprint
  const found = io.found ?? siteOf(api, a, read, 'blueprint.check')
  const { bp, at, facing, saved } = found
  const supply = supplyOf(api, a.supply)
  const have = { ...api.inv() }
  if (supply) {
    const { items = {} } = await api.act('chest_contents', { x: supply.x, y: supply.y, z: supply.z })
    for (const [item, n] of Object.entries(items)) have[item] = (have[item] ?? 0) + n
  }
  const { site, jobs, unreachable } = planJobs(api, found, { clear: a.clear === true })
  const total = jobs.filter(j => j.item).length
  const standing = standingCount(bp, at, api.block)
  const span = `y${bp.layers[0].y}..y${bp.layers.at(-1).y}`
  const head = `${bp.name} at ${cellText(at)} facing=${facing} ${bp.width}x${bp.depth}, ${span}, ${total} items${saved ? ` (place ${saved.name}, ${standing} stand)` : ''}`
  const table = stagesOf(jobs, api, bp).map(stage => stageLine(stage, have))
  const verdict = site.refusal ? `refusal: ${site.refusal}`
    : found.lint.errors.length ? `refusal: ${bp.name} does not lint: ${found.lint.errors[0]}`
    : jobs.length ? 'ok: build would start' : `already: everything ${bp.name} asks for stands at ${cellText(at)}`
  const lines = [
    head,
    ...(table.length ? table : ['nothing left to build']),
    siteLine(site),
    ...(unreachable.length ? [`unreachable=${unreachable.length} (${unreachable.slice(0, 3).map(cellText).join('; ')})`] : []),
    ...found.lint.errors.map(e => `error: ${e}`),
    ...found.lint.warnings.map(w => `warning: ${w}`),
    verdict
  ]
  return { text: lines.join('\n') }
}

// ---------------------------------------------------------------- the build

// the argument place takes for a job: the item, the cell, and the way to look, the half or the neighbour to click when
// the state needs it
const placeArgs = job => ({ item: job.item, x: job.x, y: job.y, z: job.z, ...(job.facing ? { facing: job.facing } : {}), ...(job.half ? { half: job.half } : {}), ...(job.against ? { against: faceWord(job.against) } : {}) })
const hasTool = (items, tool) => Object.keys(items).some(name => name.endsWith(`_${tool}`))
const stageSentence = (stage, short, supply, place) =>
  `stage ${stage.n} of ${stage.of} needs ${shortLine(short)} more: ${supply ? `put it in the supply chest at ${supply.label}` : 'fetch it, or put it in a chest and pass supply=x,y,z'}${supply ? ' and' : ', then'} run blueprint.build place=${place} again`

export async function buildBlueprint (api, a, io = {}) {
  const read = io.read ?? readBlueprint
  if (a.place === undefined) throw new Error('blueprint.build needs place=<a name for this build on the shared map>')
  const place = String(a.place)
  const found = io.found ?? siteOf(api, a, read)
  const { bp, at, hash, facing, params, saved, resume } = found
  if (found.lint.errors.length) throw new Error(`${bp.name} does not lint: ${found.lint.errors[0]}`)
  const supply = supplyOf(api, a.supply)
  const note = io.note ?? buildNote({ blueprint: bp.name, facing, params, hash })
  // the site has to be in sight before it can be judged: a body far from it walks up first, and only then decides
  if (!api.block(at.x, at.y, at.z)) await api.act('goto', { x: at.x - 1, y: at.y, z: at.z - 1, range: 6 })
  const { site, jobs, unreachable } = planJobs(api, found, { clear: a.clear === true })
  if (site.refusal) throw new Error(site.refusal)
  const kind = bp.tags[0] ?? 'build'
  const mine = !saved || String(saved.by ?? '').toLowerCase() === String(api.me()).toLowerCase()
  // done: the place takes the blueprint's first tag as its kind. Only its owner may change that, so a build finished on
  // an invitation leaves the kind as it was
  const finished = async () => {
    if (jobsFor(bp, at, api.block).length) return false
    if (!io.deferFinalMark && mine && (!saved || saved.kind === 'build' || !resume)) await api.act('mark', { name: place, kind, x: at.x, y: at.y, z: at.z, note })
    return true
  }
  if (!jobs.length) {
    await finished()
    return { already: `everything ${bp.name} asks for stands at ${cellText(at)}`, skipped: standingCount(bp, at, api.block) }
  }
  // the mark first: no other agent builds over the site meanwhile, and the build resumes by this name alone
  if (!resume) await api.act('mark', { name: place, kind: 'build', x: at.x, y: at.y, z: at.z, note })

  // what stood before the first block moved: the blocks this run skips
  const skipped = standingCount(bp, at, api.block)
  const counts = { built: 0, dug: 0, tilled: 0, poured: 0 }
  const missing = {}
  const wrong = []
  const obstacles = site.obstacles.map(o => `${o.name} ${cellText(o)}`)
  const scaffold = []
  let stuck = null
  let current = null
  const summary = () => ({
    built: counts.built,
    skipped,
    ...(counts.dug ? { dug: counts.dug } : {}),
    ...(counts.tilled ? { tilled: counts.tilled } : {}),
    ...(counts.poured ? { poured: counts.poured } : {}),
    ...(current ? { stage: `${current.n}/${current.of}` } : {}),
    ...(Object.keys(missing).length ? { missing: shortLine(missing) } : {}),
    ...(obstacles.length ? { obstacles: `${obstacles.length} (${obstacles.join('; ')})` } : {}),
    ...(wrong.length ? { wrong: `${wrong.length} (${wrong.slice(0, 3).join('; ')})` } : {}),
    ...(unreachable.length ? { unreachable: `${unreachable.length} (${unreachable.slice(0, 3).map(cellText).join('; ')})` } : {}),
    ...(stuck ? { stuck } : {}),
    ...(scaffold.length ? { scaffold: scaffold.join('; ') } : {})
  })
  const stop = extra => ({ ...summary(), ...extra })
  const night = () => api.clock().night
  const NIGHT = { night: `stopped at dusk: run blueprint.build place=${place} again at dawn` }

  // the cell after the primitive: the block asked for, or the same block in another state (never retried), or nothing
  const verify = job => {
    const cell = cellText(job)
    const now = api.block(job.x, job.y, job.z)
    const mate = job.second ? api.block(job.x + job.second.dx, job.y + job.second.dy, job.z + job.second.dz) : null
    const mateOk = !job.second || matchesCell(mate, [{ name: job.block.name, states: job.second.states }])
    if (matchesCell(now, [job.block]) && mateOk) { counts.built++; return }
    if (now?.name === job.block.name) { wrong.push(`${job.block.name} at ${cell}`); return }
    stuck = stuck ?? `${job.do} ${cell}: ${now?.name ?? 'nothing'} stands there after placing ${job.item}`
  }
  // a bed or a door the game refused: what still stands in its other half, the one cause a rerun cannot get past
  const mateBlocked = job => {
    if (!job.second) return ''
    const x = job.x + job.second.dx; const y = job.y + job.second.dy; const z = job.z + job.second.dz
    const there = api.block(x, y, z)?.name
    if (!there || isAir(there)) return ''
    return `: ${there} stands in its other half at ${x},${y},${z}: dig it, then run blueprint.build place=${place} again`
  }
  const tryJob = async job => {
    const cell = cellText(job)
    // one bucket does all the water; anything else is one item per cell
    if (job.item && (api.inv()[job.item] ?? 0) < 1) { missing[job.item] = job.item.endsWith('_bucket') ? 1 : (missing[job.item] ?? 0) + 1; return }
    if (job.tool && !hasTool(api.inv(), job.tool)) { missing[job.tool] = 1; return }
    // a cover is only real over a settled source (src/build/plan.mjs): held back, tried again next run
    if (job.do === 'cover' && !hasWaterSource(api.block(job.x, job.y, job.z))) { stuck = stuck ?? `cover ${cell}: not holding water yet, so the cover was held back`; return }
    const side = job.against ?? (job.along ? alongFace(job, api.block) : null)
    if (job.along && (!side || !alongFace(job, api.block))) { stuck = stuck ?? `${job.do} ${cell}: ${job.item} on its side (axis=${job.along}) is clicked onto a block beside it along ${job.along}, and neither side holds one yet: run blueprint.build place=${place} again once one stands`; return }
    const stand = job.stand
    const walk = async () => {
      if (stand.scaffoldCells?.length) {
        // The planner proved this outside column clear. Construct precisely that
        // column/material; an unrestricted dig=true route may spend other blocks.
        // A later job may use the same column. Stand on its verified existing
        // top instead of trying to pillar through the blocks already placed.
        const item = scaffoldItem(bp)
        let built = 0
        while (built < stand.scaffoldCells.length && scaffoldMatches(api.block(stand.scaffoldCells[built].x, stand.scaffoldCells[built].y, stand.scaffoldCells[built].z)?.name, item)) built++
        if (stand.scaffoldCells.slice(built).some(c => scaffoldMatches(api.block(c.x, c.y, c.z)?.name, item))) throw new Error(`scaffold at ${stand.x},${stand.z} has a gap; inspect it before continuing`)
        await api.act('goto', { x: stand.x, y: at.y + built, z: stand.z, range: 0, into: true })
        for (let remaining = stand.scaffold - built; remaining > 0; remaining -= 4) {
          // Exact goto can finish during the last step down. Let ordinary
          // physics settle on the proved full support before pillar_up checks
          // onGround; it must never place while the body is still airborne.
          if (api.pause) await api.pause(0.25)
          await api.checkpoint()
          await api.act('pillar_up', { steps: Math.min(4, remaining), item: scaffoldItem(bp) })
          await api.checkpoint()
        }
        scaffold.push(`${stand.scaffold} ${scaffoldItem(bp)} at ${stand.x},${at.y},${stand.z}`)
      }
      return api.act('goto', { x: stand.x, y: stand.y, z: stand.z, range: 0, into: true })
    }
    const walked = await walk().then(r => ({ r }), e => ({ e }))
    if (walked.e) { stuck = stuck ?? `${job.do} ${cell} from ${stand.x},${stand.y},${stand.z}: ${walked.e.message}`; return }
    if (walked.r?.scaffold) scaffold.push(walked.r.scaffold)
    if (!['dig', 'till', 'pour'].includes(job.do)) {
      const actual = api.pos()
      const visible = side && placementSight({ ...job, against: side }, actual, api.block, name => REGISTRY.blocksByName[name]?.boundingBox === 'block', true)
      if (!canPlaceFromHere(actual, job) || !visible) { stuck = stuck ?? `${job.do} ${cell}: the selected placement face is no longer visible or within reach`; return }
    }
    const call = job.do === 'dig' ? ['dig', { x: job.x, y: job.y, z: job.z }]
      : job.do === 'till' ? ['till', { x: job.x, y: job.y, z: job.z }]
      : job.do === 'pour' ? ['pour', { x: job.x, y: job.y - 1, z: job.z }]
      : ['place', placeArgs(side ? { ...job, against: side } : job)]
    const failed = await api.act(...call).then(() => null, e => e.message)
    if (failed) { stuck = stuck ?? `${job.do} ${cell}: ${failed}${mateBlocked(job)}`; return }
    if (job.do === 'dig') { counts.dug++; return }
    if (job.do === 'till') { counts.tilled++; return }
    if (job.do === 'pour') { counts.poured++; return }
    verify(job)
  }

  for (const stage of stagesOf(jobs, api, bp)) {
    current = stage
    if (night()) return stop(NIGHT)
    // the stage's shortfall comes out of the supply chest, exactly and nothing more; what it cannot cover stops the build
    const short = shortfall(stage.bill, api.inv())
    if (Object.keys(short).length && supply) {
      await api.act('goto', { x: supply.x, y: supply.y, z: supply.z, range: 2 })
      await api.act('withdraw', { items: short, x: supply.x, y: supply.y, z: supply.z }).catch(() => null)
    }
    const still = shortfall(stage.bill, api.inv())
    const sentence = Object.keys(still).length ? stageSentence(stage, still, supply, place) : null
    // short before the first block moves: refused, not half done. Later stages have built what they could, and stop
    if (sentence && a.partial !== true && stage.n === 1) throw new Error(sentence)
    if (sentence && a.partial !== true) return stop({ stopped: sentence })
    for (const job of stage.jobs) {
      if (night()) return stop(NIGHT)
      await tryJob(job)
      api.report(summary())
      await api.checkpoint()
    }
    if (sentence) return stop({ stopped: sentence })
  }
  const done = await finished()
  const out = summary()
  api.report(out)
  return done ? out : { ...out, left: `run blueprint.build place=${place} again for what is still missing` }
}
