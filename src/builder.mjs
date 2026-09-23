// The engine both build composites run on: a saved plan is a job list, and the same list builds a farm from bare ground
// and raises a pen. Only the pure judgements live in lib.mjs; this is the part that walks, digs and places.
import { billShortfall, farmJobs, groundJobs, jobCall, jobsBill, openingJobs, penOpenRefusal, penProbes, planAnchor, planBeside, shortLine } from './lib.mjs'

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
  const summary = () => {
    const short = shortOfSkipped()
    return { ...counts, ...(Object.keys(short).length ? { missing: shortLine(short) } : {}), ...(drowned().length ? { skipped: skipLine() } : {}) }
  }

  const tryJob = async job => {
    // counted the way jobsBill counts: one bucket does a whole field, so a dry channel is short one bucket, not one per cell
    if (job.item && (api.inv()[job.item] ?? 0) < 1) { missing[job.item] = job.item === 'water_bucket' ? 1 : (missing[job.item] ?? 0) + 1; return }
    const [action, args] = jobCall(job)
    const failed = await api.act(action, args).then(() => null, e => e.message)
    if (failed) { counts.stuck = counts.stuck ?? failed; return }
    counts[COUNT_OF[job.do]] = (counts[COUNT_OF[job.do]] ?? 0) + 1
  }

  // the ground has to be in sight before any of this can be judged, so the walk comes before the preflight
  await api.act('goto', { x: middle.x, y: middle.y, z: middle.z, range: 2 })
  // what the plan describes already stands a block away from where the plan puts it: building would lay a second copy
  // of it over or under the first one. Say which y to re-save with and touch nothing
  const anchor = planAnchor(plan.cells, api.block)
  if (anchor.off) throw new Error(`${plan.name} is not where its plan says: ${anchor.note}`)
  // ...and the same question sideways: the ring, wall or field the plan describes standing a few cells across from it
  const beside = planBeside(plan.cells, api.block, { name: plan.name })
  if (beside) throw new Error(`${plan.name} is not where its plan says: ${beside.note}`)
  await api.checkpoint()
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
  if (!todo.length) return drowned().length ? summary() : { already: 'everything the plan asks for is already there' }
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
