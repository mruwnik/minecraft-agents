// The engine both build composites run on: a saved plan is a job list, and the same list builds a farm from bare ground
// and raises a pen. Only the pure judgements live in lib.mjs; this is the part that walks, digs and places.
import { billShortfall, farmJobs, groundJobs, hasWaterSource, jobsBill, openingJobs, outOfSight, penOpenRefusal, penProbes, planAnchor, planBeside, shortLine } from './lib.mjs'
import { lowSlabs, lowSlabLine } from './cover.mjs'
import { workFrom } from './stand.mjs'

const WATER_RANGE = 32
const WATER_CANDIDATES = 32

// A dry channel cell's job carries water_bucket, and until now a body that reached one empty-handed just reported
// missing=water_bucket and gave up - the driver had to notice, walk to a lake, fill a bucket by hand and run the
// build again. `fill` already walks to its target itself (see bot.mjs), so all this adds is finding one: the nearest
// water within reach, tried in order until one is a settled source, rather than something this body has to work out
// first. Shared with maintain_farm, which imports it from here rather than duplicate it.
// find_blocks matches by NAME and returns the nearest first - standing beside an unfinished channel, the nearest
// water is that channel's own flowing cells, the very thing this build is trying to fix, not the lake further off.
// `fill` refuses flowing water, but only after walking to it (Chani's field, 09-26: a dozen-odd flowing channel
// cells all closer than the real lake, each one a wasted walk). So each candidate's OWN level is read first, and
// only a settled source is ever walked to; a generous count keeps a real source in the list past a field's own mess.
export async function fetchWaterBucket (api, range = WATER_RANGE) {
  if ((api.inv().water_bucket ?? 0) > 0) return true
  if ((api.inv().bucket ?? 0) < 1) return false
  const { positions = [] } = await api.act('find_blocks', { block: 'water', maxDistance: range, count: WATER_CANDIDATES }).then(r => r, () => ({}))
  const sources = positions.filter(p => hasWaterSource(api.block(p.x, p.y, p.z)))
  for (const p of sources) {
    const filled = await api.act('fill', { x: p.x, y: p.y, z: p.z }).then(() => true, () => false)
    if (filled) return true
  }
  return false
}

const COUNT_OF = { fill: 'levelled', clear: 'levelled', till: 'tilled', pour: 'poured', cover: 'covered', plant: 'planted', place: 'built' }

// The pen the plan's cells lie in, if one stands there with animals in it: pen.check from over the plan's own floor
// cells and from one level down (see penProbes). A cell that is no spot to stand on is no pen, and pen.check says so
// by failing, so that answer is taken as "no pen here" rather than passed on.
// Every probe asks about a different cell, so the runner's "failed twice in a row" rule never fires on these: it counts
// repeats of the SAME message, and each refusal names its own cell.
const penUnderPlan = async (api, cells) => {
  for (const at of penProbes(cells)) {
    const found = await api.act('pen.check', at).then(r => r, () => null)
    if (found?.inside) return found
  }
  return null
}

export async function buildFromPlan (api, a) {
  const plan = api.plan(a.place)
  // the plan's y is the ground block, so the body stands one above it
  const middle = { x: plan.x + Math.floor((plan.parsed.width - 1) / 2), y: plan.y + 1, z: plan.z + Math.floor((plan.parsed.height - 1) / 2) }
  const counts = {}
  const missing = {}
  const ground = () => groundJobs({ cells: plan.cells, worldAt: api.block, solid: api.solid })
  // a cell the plan's own water stands over is never dug (dig refuses it, rightly): it is reported as skipped= instead
  const field = () => farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() }).filter(j => j.do !== 'skip')
  const drowned = () => farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() }).filter(j => j.do === 'skip')
  const left = () => [...ground(), ...field()].filter(j => !j.item || (api.inv()[j.item] ?? 0) > 0)
  const skipLine = () => drowned().map(j => `${j.x},${j.y},${j.z} (${j.why})`).join('; ')
  // a cell that was SKIPPED for want of what it needs is still something the field is short of: a dry channel the body
  // carries no water for never becomes a job, so nothing would otherwise say why the plan is not finished
  const shortOfSkipped = () => drowned().reduce((m, j) => !j.item || j.have ? m : { ...m, [j.item]: j.item === 'water_bucket' ? 1 : (m[j.item] ?? 0) + 1 }, { ...missing })
  // channels capped the old way, with a bottom slab, still hold their water and are left alone: no churn on a working
  // field. But they walk worse than a top slab would, so they are named, once per report, with how to raise them -
  // farm.build ran this same job list all along but never said so, unlike farm.maintain (jizo-melon-patch, 09-26)
  const low = () => lowSlabs(plan.cells, api.block)
  const summary = () => {
    const short = shortOfSkipped()
    const slabs = low()
    return { ...counts, ...(Object.keys(short).length ? { missing: shortLine(short) } : {}), ...(drowned().length ? { skipped: skipLine() } : {}), ...(slabs.length ? { lowSlabs: lowSlabLine(slabs.length) } : {}) }
  }

  const tryJob = async job => {
    // a missing water_bucket is fetched, not just reported: see fetchWaterBucket above
    if (job.item === 'water_bucket' && (api.inv().water_bucket ?? 0) < 1 && !(await fetchWaterBucket(api))) { missing.water_bucket = 1; return }
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
    // from a cell that sees the target (src/stand.mjs): a pour from wherever "within 3" landed the body looked at the
    // next slab or a crop instead, twice on jizo-melon-patch (09-26)
    const failed = await workFrom(api, job).then(() => null, e => e.message)
    if (failed) { counts.stuck = counts.stuck ?? failed; return }
    counts[COUNT_OF[job.do]] = (counts[COUNT_OF[job.do]] ?? 0) + 1
  }

  // The ground has to be in sight before any of this can be judged, so the walk comes before the preflight. But the
  // middle of a FINISHED pen is inside its fence with a shut gate in the way, and the walk there answers "no walkable
  // path": that is how a complete pen came to fail instead of saying already= (Perrin, item 16). Standing beside it is
  // enough to read it, so a walk that cannot get in settles for near, and only a plan that cannot be READ is refused.
  const reach = async range => api.act('goto', { x: middle.x, y: middle.y, z: middle.z, range }).then(() => true, () => false)
  if (!await reach(2)) await reach(8)
  // and a plan whose middle reads as nothing at all, floor and ground and the cell above it, is a plan in chunks this
  // body was never sent: judging that would be guessing (item 14). The whole column, because a plan anchored one level
  // off still has a loaded world around it and is a different fault, with its own answer a few lines down
  const seen = [-1, 0, 1].some(dy => api.block(middle.x, middle.y + dy, middle.z))
  if (!seen) throw new Error(`${plan.name}: ${outOfSight(null, middle, api.pos())}`)
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
  await fetchWaterBucket(api)
  const todo = [...ground(), ...field()]
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
    throw new Error(`${a.place} still needs ${shortLine(short)} more than I carry: fetch them, or partial=true to build what I can now`)
  }
  if (!todo.length) return (drowned().length || low().length) ? summary() : { already: 'everything the plan asks for is already there' }
  // the ground first: nothing can be tilled, planted or stood on until the cell has a floor and open air. The plan's own
  // jobs are read again afterwards, because a cell buried under stone has no job to show until the stone is gone
  for (const job of ground()) { await tryJob(job); await api.checkpoint() }
  for (const job of field()) { await tryJob(job); await api.checkpoint() }
  const unfinished = left()
  // one look back: a build says what it could not finish rather than running the whole list again
  if (unfinished.length) counts.unfinished = unfinished.map(j => `${j.do} ${j.x},${j.y},${j.z} (${j.why})`).join('; ')
  api.report(summary())
  return summary()
}
