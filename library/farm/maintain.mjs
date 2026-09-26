// Keep one farm going: harvest what is ripe, put back whatever the plan says should be there, store the surplus.
// The plan is the truth of what should be there; the world is the truth of what is (see `./mc plan`).
import { fetchWaterBucket } from '../../src/builder.mjs'
import { farmJobs, farmSurplus, farmWaste, hasWaterSource, planAnchor, planBill, planStructure, seedDrop, seedTarget, shortLine, SEED_ITEMS } from '../../src/lib.mjs'
import { lowSlabs, lowSlabLine } from '../../src/cover.mjs'
import { cellOf, fieldEdge } from '../../src/field.mjs'
import { workFrom } from '../../src/stand.mjs'
import { clutterBlocks, clutterLine } from './shared/clutter.mjs'

const add = (into, from = {}) => { for (const [k, n] of Object.entries(from)) into[k] = (into[k] ?? 0) + n }
// enough seed to sow the whole plan twice over stays in my pockets; the rest goes in the chest
const seedReserve = plan => Object.fromEntries(Object.entries(planBill(plan.parsed)).filter(([item]) => SEED_ITEMS.has(item)).map(([item, n]) => [item, n * 2]))

export default {
  doc: 'farm.maintain place= [days=] [within=] [compost=] [trample=]: harvest, replant, re-till, refill the channels, compost the spare seed and store the surplus of one saved farm plan. compost= is a composter or chest-like block, as x,y,z or a marked place (default: the plan\'s K cell; false keeps the seed with the harvest). trample=true lets its walks step on crop cells: the last resort out of a crop pocket',
  stops: 'days= done, a step that failed twice, or nothing left it can do',
  args: { place: 'string!', days: 'number', until: 'number', within: 'number', deposit: 'boolean', compost: 'any', trample: 'boolean' },

  async run (api, a) {
    const plan = api.plan(a.place)
    const within = a.within ?? Math.max(8, Math.ceil(Math.hypot(plan.parsed.width, plan.parsed.height)) + 2)
    // the plan's y is the ground block; the body stands one above it, and so do the chest and composter the plan marks
    const middle = { x: plan.x + Math.floor((plan.parsed.width - 1) / 2), y: plan.y + 1, z: plan.z + Math.floor((plan.parsed.height - 1) / 2) }
    const chest = planStructure(plan.cells, 'C')
    const composter = seedTarget(planStructure(plan.cells, 'K'), api.places(), a.compost)
    if (composter?.error) throw new Error(composter.error)
    const summary = { sweeps: 0, harvested: {}, replanted: 0, tilled: 0, poured: 0, covered: 0, built: 0 }
    const keep = seedReserve(plan)

    const tryJob = async job => {
      // a cover is only real once the cell it caps is actually holding its OWN water, a settled source, not merely a
      // neighbour's flow passing through - see src/builder.mjs's tryJob for the full story of the slab that kept
      // getting broken, reflooded by a neighbour's flow and blindly recapped
      if (job.do === 'cover' && !hasWaterSource(api.block(job.x, job.y, job.z))) {
        summary.stuck = summary.stuck ?? `cover ${job.x},${job.y},${job.z}: not holding water yet, so the slab was held back`
        return
      }
      // from a cell that sees the target (src/stand.mjs): a pour from wherever "within 3" landed the body looked at the
      // next slab or a crop instead, twice on jizo-melon-patch (09-26)
      const failed = await workFrom(api, job).then(() => null, e => e.message)
      if (failed) { summary.stuck = summary.stuck ?? failed; return }
      if (job.do === 'plant') summary.replanted++
      if (job.do === 'till') summary.tilled++
      if (job.do === 'pour') summary.poured++
      if (job.do === 'cover') summary.covered++
      if (job.do === 'place') summary.built++
    }

    // Where the sweep starts. This used to walk to within 2 of the plan's middle: in a finished field that is a bed
    // walled in by crops on every side, and a walk steps round crops, so the pathfinder had no node to end in and ran
    // its time out, or its search radius ("no walkable path"), where farm.harvest right after it walks to the field's
    // EDGE and works (Jizo, 09-26). The edge is the nearest cell the body can stand in, dry, within work range of the
    // plan (src/field.mjs fieldEdge). With nothing loaded round the plan there is no edge to read yet: then a walk to
    // the middle at a range that reaches the rim (half the plan's diagonal, and 2 at least) loads it, and the edge is read again
    const cellAt = (x, y, z) => cellOf(api.block(x, y, z))
    const approach = Math.max(2, Math.ceil(Math.hypot(plan.parsed.width, plan.parsed.height) / 2))
    // trample=true: a walk steps round crops, so a body a sweep left deep in its own rows (a crop pocket, card 68f4e331)
    // can walk nowhere; asked for by name, every walk of this sweep may step on crop cells instead. Only when asked:
    // the sweep's calls read the same as ever otherwise
    const tread = a.trample === true ? { trample: true } : {}
    const walkToEdge = async () => {
      const edge = () => fieldEdge(cellAt, plan.cells, api.pos())
      const spot = edge() ?? await api.act('goto', { x: middle.x, y: middle.y, z: middle.z, range: approach, ...tread }).then(edge)
      if (spot) await api.act('goto', { x: spot.x, y: spot.y, z: spot.z, range: 1, ...tread })
    }

    const sweep = async () => {
      await walkToEdge()
      // the plan is only worth following once the ground is in sight and it points at the right level: a plan anchored
      // one block low would have me till the dirt UNDER somebody's farm and plant seed inside their farmland
      const anchor = planAnchor(plan.cells, api.block)
      if (anchor.off) throw new Error(`${plan.name} is not where its plan says: ${anchor.note}`)
      // rubble over the beds is nobody's job here (maintain only puts back what the plan asks for), but the driver should be told
      const rubble = clutterBlocks(plan.cells, api.block)
      if (rubble.length) summary.clutter = `${clutterLine(rubble)} standing over the plan: ./mc farm.tidy place=${a.place}`
      await api.checkpoint({ canDeposit: Boolean(chest) })
      const cut = await api.act('farm.harvest', { within, ...tread }).catch(e => { summary.stuck = summary.stuck ?? e.message; return {} })
      add(summary.harvested, cut.harvested)
      summary.replanted += cut.replanted ?? 0
      await api.checkpoint({ canDeposit: Boolean(chest) })

      // ONE pass over the job list. This used to run twice, because a plant could report ok and leave the bed bare: the
      // place primitive counted a click the server quietly dropped as a block placed. It now reads the cell back and skips
      // instead, so a job that says it worked did work, and the field is only looked at once more to say what is still off.
      // a cell the plan's own water stands over is not work, it is a warning: dig refuses it and the sweep would give
      // up on "twice in a row". It is handed back as skipped= for the driver to drain
      // farmJobs decides pour vs. skip from whether water_bucket is ALREADY carried right here: a dry cell with none
      // becomes 'skip', not 'pour', and fetching water further down never reaches a job list that never named the
      // cell. Tried once up front so a fetchable bucket turns skip back into pour before the list is even read
      await fetchWaterBucket(api)
      const all = farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() })
      const jobs = all.filter(j => j.do !== 'skip')
      const skipped = all.filter(j => j.do === 'skip')
      if (skipped.length) summary.skipped = skipped.map(j => `${j.x},${j.y},${j.z} (${j.why})`).join('; ')
      // channels capped the old way, with a bottom slab, still hold their water and are left alone: no churn on a
      // working field. But they walk worse than a top slab would (a half-step down into every one), so they are
      // counted, once per sweep, with how to raise them
      const low = lowSlabs(plan.cells, api.block)
      if (low.length) summary.lowSlabs = lowSlabLine(low.length)
      const short = {}
      for (const job of jobs) {
        // a missing water_bucket is fetched, not just reported: see fetchWaterBucket in src/builder.mjs
        if (job.item === 'water_bucket' && !job.have && !(await fetchWaterBucket(api))) { short.water_bucket = (short.water_bucket ?? 0) + 1; continue }
        if (job.item && job.item !== 'water_bucket' && !job.have) { short[job.item] = (short[job.item] ?? 0) + 1; continue }
        await tryJob(job)
        await api.checkpoint({ canDeposit: Boolean(chest) })
      }
      if (Object.keys(short).length) summary.missing = shortLine(short)
      // whatever a done job left undone: reported, never silently repeated (the next sweep picks it up)
      const left = jobs.length ? farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() }).filter(j => j.do !== 'skip' && (!j.item || j.have)) : []
      if (left.length) summary.unfinished = left.map(j => `${j.do} ${j.x},${j.y},${j.z} (${j.why})`).join('; ')

      // the spare seed goes to the composter (or the chest-like block compost= names) BEFORE the harvest is stored:
      // stored seed was the whole complaint (09-26). A target that cannot take it is said, and the seed stays with the harvest
      const waste = composter ? farmWaste(farmSurplus(api.inv(), keep)) : {}
      const drop = Object.keys(waste).length ? seedDrop(api.block(composter.x, composter.y, composter.z)) : null
      if (Object.keys(waste).length && !drop) summary.compost = `${api.block(composter.x, composter.y, composter.z)?.name ?? 'nothing'} at ${composter.x},${composter.y},${composter.z} is neither a composter nor a chest, so the seed stays with the harvest`
      const composted = drop ? await api.act(drop, { items: waste, x: composter.x, y: composter.y, z: composter.z }).then(done => done?.fed ?? Object.entries(waste).map(([k, n]) => `${k}:${n}`).join(' '), e => { summary.compost = e.message; return null }) : null
      if (composted) summary.composted = composted
      if (chest && a.deposit !== false) {
        const surplus = farmSurplus(api.inv(), keep)
        for (const name of composted ? Object.keys(waste) : []) delete surplus[name]
        if (Object.keys(surplus).length) {
          const failed = await api.act('deposit', { items: surplus, x: chest.x, y: chest.y, z: chest.z }).then(() => null, e => e.message)
          if (failed) summary.stuck = summary.stuck ?? failed
          else summary.deposited = { ...(summary.deposited ?? {}), ...surplus }
        }
      }
      summary.sweeps++
      api.report(summary)
    }

    // one sweep a day: wait out the daylight, let the runner put me to bed, then go again at dawn
    const nextDay = async () => {
      await api.until(() => api.clock().night, { timeout: 1200, every: 10, what: 'the day never ended' })
      await api.checkpoint({ canDeposit: Boolean(chest) })
      await api.until(() => api.clock().day, { timeout: 1200, every: 10, what: 'the night never ended' })
    }

    for (;;) {
      await sweep()
      await api.checkpoint({ canDeposit: Boolean(chest) })
      if (!(a.days > 0)) return summary
      await nextDay()
    }
  }
}
