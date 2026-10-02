import { hasPlan, jobGroundKey } from '../lib/plan.mjs'
// The engine both build composites run on: a saved plan is a job list, and the same list builds a farm from bare ground
// and raises a pen. Only the pure judgements live in lib.mjs; this is the part that walks, digs and places.
import { billShortfall, farmJobs, groundJobs, hasHoe, hasWaterSource, hydrated, jobsBill, needsWaterLine, NO_HOE, openingJobs, outOfSight, penOpenRefusal, penProbes, planAnchor, planBeside, planCells, PLAN_LEGEND, planSpec, sameFamily, shortLine } from '../lib.mjs'
import { lowSlabs, lowSlabLine } from './cover.mjs'
import { standingSpots, workFrom } from '../navigation/stand.mjs'
import { digGuard, fieldLeg, footprintOf } from '../farm/leg.mjs'
import { drainJobs, reopenJobs, shoreOrder, wetFooting } from './water.mjs'
import { recoverFarm } from '../farm/attention.mjs'
import { missingGround } from '../lib/fill.mjs'

const WATER_RANGE = 32
const WATER_CANDIDATES = 32

// A bucket may be refilled from external water, never by borrowing a source the
// current or another saved plan needs. Otherwise each pour merely moves the same
// water between channels and a maintenance pass can never finish irrigation.
const waterKey = p => `${p.x},${p.y},${p.z}`
const irrigationSources = (api, cells) => new Set([
  ...cells,
  ...(api.places?.() ?? []).filter(p => hasPlan(p)).flatMap(planCells)
].filter(c => planSpec(c)?.kind === 'water').map(waterKey))

export const NO_BUCKET = 'no bucket: craft item=bucket (3 iron_ingot)'
export const noWaterLine = range => `no water within ${range} blocks`
// why no bucket of water can be had, or null once one is carried: fetched from the nearest still source within range
export async function waterShortfall (api, range = WATER_RANGE, cells = []) {
  if ((api.inv().water_bucket ?? 0) > 0) return null
  if ((api.inv().bucket ?? 0) < 1) return NO_BUCKET
  const protectedSources = irrigationSources(api, cells)
  const find = count => api.act('find_blocks', { block: 'water', maxDistance: range, count }).then(r => r.positions ?? [], recoverFarm(() => []))
  let positions = await find(WATER_CANDIDATES)
  // A large field can occupy the entire first search result. Widen once so its
  // own sources do not hide an external pond; the search remains bounded.
  if (positions.length >= WATER_CANDIDATES && positions.some(p => protectedSources.has(waterKey(p)))) {
    positions = await find(Math.min(512, WATER_CANDIDATES + protectedSources.size))
  }
  const preserved = positions.some(p => protectedSources.has(waterKey(p)) && hasWaterSource(api.block(p.x, p.y, p.z)))
  const here = api.pos()
  const away = p => Math.hypot(p.x - here.x, p.y - here.y, p.z - here.z)
  const sources = positions.filter(p => !protectedSources.has(waterKey(p)) && hasWaterSource(api.block(p.x, p.y, p.z))).sort((p, q) => away(p) - away(q))
  let inaccessible = null
  for (const p of sources) {
    // A range-only fill can stop below a bank and aim into its wall. Work from
    // close dry shore above the source, with a clear view down onto the water.
    const shores = standingSpots({ target: p, blockAt: api.block, range: Math.sqrt(5) })
      .filter(spot => spot.y >= p.y + 1 && spot.y <= p.y + 2)
      .sort((a, b) => away(a) - away(b)).slice(0, 2)
    let reached = false
    for (const shore of shores) {
      reached = await api.act('goto', { ...shore, range: 0 }).then(() => true, recoverFarm(() => false))
      if (reached) break
    }
    if (!reached) { inaccessible = `no reachable dry shore beside water at ${waterKey(p)}: provide access 1-2 blocks away with a clear view down onto the source`; continue }
    const failure = await api.act('fill', { x: p.x, y: p.y, z: p.z }).then(() => null, recoverFarm(e => e.message))
    if (!failure && (api.inv().water_bucket ?? 0) > 0) return null
    // Do not repeat this exact click failure: the composite correctly hands
    // back after two identical action failures. Other field work can continue.
    if (!failure || /bucket is still empty/i.test(failure)) return `water at ${waterKey(p)} could not fill the bucket from shore: ${failure ?? 'bucket still empty'}; supply a filled bucket or repair shore access`
  }
  if (inaccessible) return inaccessible
  return preserved ? `no usable external water within ${range} blocks: preserved planned irrigation; provide an external source or filled bucket` : noWaterLine(range)
}
export const fetchWaterBucket = async (api, range = WATER_RANGE, cells = []) => (await waterShortfall(api, range, cells)) === null

const COUNT_OF = { fill: 'levelled', clear: 'levelled', till: 'tilled', pour: 'poured', cover: 'covered', plant: 'planted', place: 'built' }
// a dam (src/build/water.mjs) is a fill into standing water, counted apart: it is dug out again before the plan's own jobs
const countOf = job => job.dam ? 'dammed' : COUNT_OF[job.do]

// The pen the plan's cells lie in, if one stands there with animals in it: pen.check from over the plan's own floor
// cells and from one level down (see penProbes). A cell that is no spot to stand on is no pen, and pen.check says so
// by failing, so that answer is taken as "no pen here" rather than passed on.
// Every probe asks about a different cell, so the runner's "failed twice in a row" rule never fires on these: it counts
// repeats of the SAME message, and each refusal names its own cell.
const penUnderPlan = async (api, cells) => {
  for (const at of penProbes(cells)) {
    const found = await api.act('pen.check', at).then(r => r, recoverFarm(() => null))
    if (found?.inside) return found
  }
  return null
}

export async function buildFromPlan (api, a, { farm = false } = {}) {
  const recover = handler => farm ? recoverFarm(handler) : handler
  const plan = api.plan(a.place)
  // the plan's y is the ground block, so the body stands one above it
  const middle = { x: plan.x + Math.floor((plan.parsed.width - 1) / 2), y: Math.floor((Math.min(...plan.cells.map(c => c.y)) + Math.max(...plan.cells.map(c => c.y))) / 2) + 1, z: plan.z + Math.floor((plan.parsed.height - 1) / 2) }
  const counts = {}
  const missing = {}
  // a walk to a standing cell that finds no path is walked once more with dig=true inside the plan's own footprint,
  // sparing every cell the plan lists and the ground of the whole footprint (src/farm/leg.mjs digGuard), as
  // farm.maintain's sweep does. What it dug is reported
  const box = footprintOf(plan.cells)
  const guard = digGuard(plan.cells)
  const dug = []
  const leg = to => fieldLeg(api, to, box, guard).then(r => { if (r?.dug) dug.push(r.dug); return r })
  // water standing in a plan cell is dammed with dirt and the dam dug out again once nothing can flow back into it. A
  // dam that has to stay (water from outside the plan against it) is named on kept=, and its column is left as it is:
  // the plan's own jobs there would dig the dam and flood the cell again (src/build/water.mjs)
  const dams = []
  const kept = []
  const keptColumns = new Set()
  const notKept = job => !keptColumns.has(jobGroundKey(job))
  // a job with no dry cell in reach to work from is left for the driver, named with the water in the way
  const blocked = []
  // tills held for want of water (card 72e49b3d): farm_needs_water= names why and where, and the held cells are
  // never read back as unfinished
  const dryBeds = []
  const dryKeys = new Set()
  // set true for a dry till that goes ahead because its plant is right behind it, seed in hand: the field loop below
  // skips its own checkpoint once so that plant runs before anything can hand back or sleep the night on the bare bed
  let sowNext = false
  // a till that failed leaves grass or dirt under its plant, and place refuses that outright, ending the whole build
  const failedKeys = new Set()
  const ground = () => groundJobs({ cells: plan.cells, worldAt: api.block, solid: api.solid }).filter(notKept)
  // a cell the plan's own water stands over is never dug (dig refuses it, rightly): it is reported as skipped= instead
  const field = () => farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() }).filter(j => j.do !== 'skip').filter(notKept)
  const drowned = () => farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() }).filter(j => j.do === 'skip')
  const left = () => [...ground(), ...field()].filter(j => !j.item || (api.inv()[j.item] ?? 0) > 0).filter(j => !dryKeys.has(jobGroundKey(j)))
  const skipLine = () => drowned().map(j => `${j.x},${j.y},${j.z} (${j.why})`).join('; ')
  // a cell that was SKIPPED for want of what it needs is still something the field is short of: a dry channel the body
  // carries no water for never becomes a job, so nothing would otherwise say why the plan is not finished
  const shortOfSkipped = () => drowned().reduce((m, j) => !j.item || j.have ? m : { ...m, [j.item]: j.item === 'water_bucket' ? 1 : (m[j.item] ?? 0) + 1 }, { ...missing })
  // channels capped the old way, with a bottom slab, still hold their water and are left alone: no churn on a working
  // field. But they walk worse than a top slab would, so they are named, once per report, with how to raise them -
  // farm.build ran this same job list all along but never said so, unlike farm.maintain (jizo-melon-patch, 09-26)
  const low = () => lowSlabs(plan.cells, api.block)
  // waterShortfall's own reason, read by both tryJob (a pour run dry) and summary (a till held dry): declared above
  // both so neither closes over a binding that is still a `let` with nothing in it yet
  let water = null
  const summary = () => {
    const short = shortOfSkipped()
    const slabs = low()
    return {
      ...counts,
      ...(Object.keys(short).length ? { missing: shortLine(short) } : {}),
      ...(drowned().length ? { skipped: skipLine() } : {}),
      ...(slabs.length ? { lowSlabs: lowSlabLine(slabs.length) } : {}),
      ...(kept.length ? { kept: kept.join('; ') } : {}),
      ...(blocked.length ? { blocked: blocked.join('; ') } : {}),
      ...(dug.length ? { dug: dug.join('; ') } : {}),
      ...(dryBeds.length ? { farm_needs_water: needsWaterLine(water, dryBeds) } : {})
    }
  }

  const tryJob = async (job, next) => {
    if (job.do === 'place' && job.item === 'torch' && !sameFamily('oak_fence', api.block(job.x, job.y - 1, job.z)?.name)) {
      counts.stuck = counts.stuck ?? `torch ${job.x},${job.y},${job.z}: support missing; place the fence post first`
      return false
    }
    // Partial builds can run out of floor material. Leave those beds for a later
    // pass instead of trying to hoe air or the grass growing in a terrain dip.
    if (farm && (job.do === 'till' || job.do === 'plant')) {
      const y = job.do === 'plant' ? job.y - 1 : job.y
      const ground = api.block(job.x, y, job.z)
      if (ground && missingGround(ground.name)) {
        const reason = `unfilled bed ${job.x},${y},${job.z}: ${ground.name} where solid ground is needed`
        if (!blocked.includes(reason)) blocked.push(reason)
        return false
      }
      // a bed tilled before its channel holds water dries back to dirt and re-tills forever (card 72e49b3d): held
      // back instead, and the plant on the same ground held with it - UNLESS the very next job is that plant, seed
      // in hand, which goes in at once and makes the bed safe. Either way it counts for farm_needs_water
      if (job.do === 'till' && !hydrated(api.block, job)) {
        dryBeds.push(job)
        const sownAtOnce = next?.do === 'plant' && jobGroundKey(next) === jobGroundKey(job) && (api.inv()[next.item] ?? 0) > 0
        if (!sownAtOnce) { dryKeys.add(jobGroundKey(job)); return false }
        sowNext = true
      }
      if (job.do === 'till' && !hasHoe(api.inv())) {
        failedKeys.add(jobGroundKey(job))
        counts.stuck = counts.stuck ?? NO_HOE
        return false
      }
      if (job.do === 'plant' && (dryKeys.has(jobGroundKey(job)) || failedKeys.has(jobGroundKey(job)))) return false
    }
    // a missing water_bucket is fetched, not just reported: see fetchWaterBucket above
    if (job.item === 'water_bucket' && (api.inv().water_bucket ?? 0) < 1) {
      const why = water ?? await waterShortfall(api, undefined, plan.cells)
      water = why
      if (why) { missing.water_bucket = 1; counts.stuck = counts.stuck ?? why; return }
    }
    // counted the way jobsBill counts: one bucket does a whole field, so a dry channel is short one bucket, not one per cell
    if (job.item && job.item !== 'water_bucket' && (api.inv()[job.item] ?? 0) < 1) { missing[job.item] = (missing[job.item] ?? 0) + 1; return }
    // a cover is only real once the cell it caps is actually holding its own water. farmJobs queues pour and cover
    // together for a dry cell on the assumption the pour just before it lands, but a pour that misses (a neighbour's
    // flow crept in first, the wrong y, out of reach) used to be covered anyway: `place` only checks that its own
    // block ended up there, not that it sits on real water. That capped dry ground with a slab, and the next pass
    // read the slab as an obstruction sitting where the channel belongs and dug it straight back out - a slab broken,
    // reflooded by the neighbour and recapped forever. A first fix checked holdsWater here, which stopped the fully
    // dry case, but a cell a neighbour's flow was merely passing through (not dry, but no source of its own either)
    // still read as fine and got capped - flow has no source in that cell and can recede a tick later, leaving the
    // slab dry anyway. Only a settled source is safe to cap, so this checks hasWaterSource instead: the cover is
    // retried next time, once the water standing here is really this cell's own.
    if (job.do === 'cover' && !hasWaterSource(api.block(job.x, job.y, job.z))) {
      counts.stuck = counts.stuck ?? `cover ${job.x},${job.y},${job.z}: not holding water yet, so the slab was held back`
      return
    }
    // from a cell that sees the target (src/navigation/stand.mjs): a pour from wherever "within 3" landed the body looked at the
    // next slab or a crop instead, twice on jizo-melon-patch (09-26)
    const failed = await workFrom(api, job, spot => leg({ ...spot, range: 0 })).then(() => null, recover(e => e.message))
    if (failed) {
      counts.stuck = counts.stuck ?? failed
      if (job.do === 'till') failedKeys.add(jobGroundKey(job))
      return false
    }
    counts[countOf(job)] = (counts[countOf(job)] ?? 0) + 1
    return true
  }
  // a levelling job (a fill, a dam) is worked only from dry footing that sees it: the shore order puts such jobs
  // first and makes the ones behind them workable, and a job still without footing when its turn comes is named
  const tryLevelling = async job => {
    const wet = wetFooting(job, api.block)
    if (wet) { blocked.push(wet); return }
    if (await tryJob(job) && job.dam) dams.push(job)
  }

  // The ground has to be in sight before any of this can be judged, so the walk comes before the preflight. But the
  // middle of a FINISHED pen is inside its fence with a shut gate in the way, and the walk there answers "no walkable
  // path": that is how a complete pen came to fail instead of saying already= (Perrin, item 16). Standing beside it is
  // enough to read it, so a walk that cannot get in settles for near, and only a plan that cannot be READ is refused.
  const reach = async range => api.act('goto', { x: middle.x, y: middle.y, z: middle.z, range }).then(() => true, recover(() => false))
  if (!await reach(2)) await reach(8)
  // and a plan whose middle reads as nothing at all, floor and ground and the cell above it, is a plan in chunks this
  // body was never sent: judging that would be guessing (item 14). The whole column, because a plan anchored one level
  // off still has a loaded world around it and is a different fault, with its own answer a few lines down
  const seen = [-1, 0, 1].some(dy => api.block(middle.x, middle.y + dy, middle.z))
  if (!seen) {
    const message = `${plan.name}: ${outOfSight(null, middle, api.pos())}`
    if (farm) return { unreachable: message }
    throw new Error(message)
  }
  // what the plan describes already stands a block away from where the plan puts it: building would lay a second copy
  // of it over or under the first one. Say which y to re-save with and touch nothing
  const anchor = planAnchor(plan.cells, api.block)
  if (anchor.off) throw new Error(`${plan.name} is not where its plan says: ${anchor.note}`)
  // ...and the same question sideways: the ring, wall or field the plan describes standing a few cells across from it
  const beside = planBeside(plan.cells, api.block, { name: plan.name })
  if (beside) throw new Error(`${plan.name} is not where its plan says: ${beside.note}`)
  await api.checkpoint()
  // farmJobs decides pour vs. skip from whether water_bucket is ALREADY carried at the moment it is called: a dry
  // cell with none becomes a 'skip' job, not a 'pour' one, and no amount of fetching water inside tryJob later
  // reaches a job list that never named the cell. So this is tried once up front, before that list is built, not
  // down in tryJob - the one there is only a fallback for a bucket a pour used up earlier in the same run
  // named before the water: without a hoe not one bed can be tilled, water or none
  if (!hasHoe(api.inv()) && field().some(j => j.do === 'till')) counts.stuck = counts.stuck ?? NO_HOE
  water = await waterShortfall(api, undefined, plan.cells)
  if (water && drowned().some(j => j.item === 'water_bucket')) counts.stuck = counts.stuck ?? water
  const todo = [...ground(), ...drainJobs(plan.cells, api.block), ...field()]
  // a build that digs or fills inside a pen holding animals empties it long before the fences go back up: refuse while
  // nothing has been touched and say how to get out of it (Chani's 4 sheep). A build that only places is safe
  if (openingJobs(todo).length) {
    const refusal = penOpenRefusal(plan.name, todo, await penUnderPlan(api, plan.cells))
    if (refusal) throw new Error(refusal)
  }
  // counted before a single block is moved: half a build is worse than none. What is asked for is what is still missing
  // from the GROUND, not the whole plan, so a half-built one is picked up where it stopped. The skipped jobs are counted
  // in too, and the count comes before the "nothing to do" answer: a dry channel the body carries no water for never
  // becomes a job at all, so a plan that is only channel would otherwise report itself finished with no water in it
  const short = billShortfall(jobsBill([...todo, ...drowned()]), api.inv())
  if (Object.keys(short).length && a.partial !== true) {
    if (farm) return { ...(counts.stuck ? { stuck: counts.stuck } : {}), missing: shortLine(short), attention: `${a.place} still needs ${shortLine(short)} more than I carry: fetch them, or partial=true to build what I can now` }
    throw new Error(`${a.place} still needs ${shortLine(short)} more than I carry: fetch them, or partial=true to build what I can now`)
  }
  if (!todo.length) return (drowned().length || low().length) ? summary() : { already: 'everything the plan asks for is already there' }
  // the ground first: nothing can be tilled, planted or stood on until the cell has a floor and open air. The plan's own
  // jobs are read again afterwards, because a cell buried under stone has no job to show until the stone is gone
  // ...and the ground in the order it can be stood on: from the shore in, when the plan lies in water (src/build/water.mjs).
  // A plan whose levelling could not even start for water in the way is refused, naming the water
  for (const job of shoreOrder([...ground(), ...drainJobs(plan.cells, api.block)], api.block)) { await tryLevelling(job); await api.checkpoint() }
  if (blocked.length && !counts.levelled && !counts.dammed) {
    if (farm) return summary()
    throw new Error(`${plan.name}: ${blocked[0]}`)
  }
  const reopen = reopenJobs(dams, api.block)
  kept.push(...reopen.kept)
  reopen.kept.forEach(line => keptColumns.add(line.match(/^-?\d+,-?\d+,-?\d+/)?.[0]))
  for (const job of reopen.open) { await tryJob(job); await api.checkpoint() }
  for (const job of ground()) { await tryJob(job); await api.checkpoint() }
  // tryJob only sees one job; the next one in this same list is what tells a dry till whether its own plant is
  // right behind it, seed in hand, so the bed goes in at once instead of being held
  const fieldJobs = field()
  for (let i = 0; i < fieldJobs.length; i++) {
    const ok = await tryJob(fieldJobs[i], fieldJobs[i + 1])
    // a checkpoint right here could hand back or sleep the night on bare, dry farmland: skipped once, for the till
    // just above, so its own plant (the very next job in this list) runs before any of that can happen - but only
    // when that till actually went in. A till that failed never gets its plant (dryKeys or failedKeys sends it straight past,
    // below) and still needs its own checkpoint, or a run of failing dry tills loses every checkpoint in between
    const sowing = sowNext && ok
    sowNext = false
    if (!sowing) await api.checkpoint()
  }
  const unfinished = left()
  // one look back: a build says what it could not finish rather than running the whole list again
  if (unfinished.length) counts.unfinished = unfinished.map(j => `${j.do} ${j.x},${j.y},${j.z} (${j.why})`).join('; ')
  api.report(summary())
  return summary()
}
