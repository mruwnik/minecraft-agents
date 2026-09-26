// Keep one farm going: harvest what is ripe, put back whatever the plan says should be there, store the surplus.
// The plan is the truth of what should be there; the world is the truth of what is (see `./mc plan`).
import { waterShortfall } from '../../src/builder.mjs'
import { bareLine, bareWhy, farmJobs, farmSurplus, farmWaste, hasHoe, hasWaterSource, NO_HOE, PLAN_LEGEND, planAnchor, planStructure, seedDrop, seedReserve, seedTarget, shortLine } from '../../src/lib.mjs'
import { lowSlabs, lowSlabLine } from '../../src/cover.mjs'
import { cellOf, fieldEdge } from '../../src/field.mjs'
import { fieldLeg, footprintOf, spareCells } from '../../src/fieldleg.mjs'
import { jobSight, standingSpots, workFrom } from '../../src/stand.mjs'
import { loadedAround } from '../../src/walk.mjs'
import { clutterBlocks, clutterLine } from './shared/clutter.mjs'

const add = (into, from = {}) => { for (const [k, n] of Object.entries(from)) into[k] = (into[k] ?? 0) + n }
// the plans whose seed stays in my pockets (src/lib/farm.mjs seedReserve): this one, and every other reserve_for=
// names (a homestead's, from the routine's $places), each once
const reservePlans = (api, own, names) => [own.name, ...String(names ?? '').split(',').map(n => n.trim()).filter(Boolean)]
  .filter((name, i, all) => all.indexOf(name) === i).map(name => name === own.name ? own : api.plan(name))
// the counts first, bare= right after them: the routine keeps 120 characters of a step's summary, and what stood
// behind lowSlabs= and clutter= was the line nobody read (09-26)
const SAID_FIRST = ['sweeps', 'harvested', 'replanted', 'bare', 'tilled', 'poured', 'covered', 'built']
const ordered = summary => Object.fromEntries([
  ...SAID_FIRST.filter(k => summary[k] !== undefined).map(k => [k, summary[k]]),
  ...Object.entries(summary).filter(([k]) => !SAID_FIRST.includes(k))
])
// a bed is one cell of the plan whatever its level: the till works the ground, the plant the cell above it
const bedKey = job => `${job.x},${job.z}`

export default {
  doc: 'farm.maintain place= [days=] [within=] [compost=] [reserve_for=]: harvest, replant, re-till, refill the channels, compost the spare seed and store the surplus of one saved farm plan. compost= is a composter or chest-like block, as x,y,z or a marked place (default: the plan\'s K cell; false keeps the seed with the harvest). reserve_for=a,b names the other plans whose seed is kept out of the compost too (a routine fills it with $places)',
  stops: 'days= done, a step that failed twice, or nothing left it can do',
  args: { place: 'string!', days: 'number', until: 'number', within: 'number', deposit: 'boolean', compost: 'any', reserve_for: 'string' },

  async run (api, a) {
    const plan = api.plan(a.place)
    const within = a.within ?? Math.max(8, Math.ceil(Math.hypot(plan.parsed.width, plan.parsed.height)) + 2)
    // the plan's y is the ground block; the body stands one above it, and so do the chest and composter the plan marks
    const middle = { x: plan.x + Math.floor((plan.parsed.width - 1) / 2), y: plan.y + 1, z: plan.z + Math.floor((plan.parsed.height - 1) / 2) }
    const chest = planStructure(plan.cells, 'C')
    const composter = seedTarget(planStructure(plan.cells, 'K'), api.places(), a.compost)
    if (composter?.error) throw new Error(composter.error)
    const summary = { sweeps: 0, harvested: {}, replanted: 0, tilled: 0, poured: 0, covered: 0, built: 0 }
    // every walk of the sweep is a leg of src/fieldleg.mjs: plain first, once more with dig=true when the path fails
    // inside the plan's footprint (never a plan block), and both failing is one stuck= line naming the cell (card 72e49b3d)
    const box = footprintOf(plan.cells)
    const spare = spareCells(plan.cells)
    const dug = []
    const leg = to => fieldLeg(api, to, box, spare).then(r => { if (r?.dug) dug.push(r.dug); return r })
    const keep = seedReserve(reservePlans(api, plan, a.reserve_for).map(p => p.parsed))

    const tryJob = async job => {
      // a cover is only real once the cell it caps is actually holding its OWN water, a settled source, not merely a
      // neighbour's flow passing through - see src/builder.mjs's tryJob for the full story of the slab that kept
      // getting broken, reflooded by a neighbour's flow and blindly recapped
      if (job.do === 'cover' && !hasWaterSource(api.block(job.x, job.y, job.z))) {
        summary.stuck = summary.stuck ?? `cover ${job.x},${job.y},${job.z}: not holding water yet, so the slab was held back`
        return null
      }
      // from a cell that sees the target (src/stand.mjs): a pour from wherever "within 3" landed the body looked at the
      // next slab or a crop instead, twice on jizo-melon-patch (09-26)
      const failed = await workFrom(api, job, spot => leg({ ...spot, range: 0 })).then(() => null, e => e.message)
      if (failed) { summary.stuck = summary.stuck ?? failed; return failed }
      if (job.do === 'plant') summary.replanted++
      if (job.do === 'till') summary.tilled++
      if (job.do === 'pour') summary.poured++
      if (job.do === 'cover') summary.covered++
      if (job.do === 'place') summary.built++
      return null
    }

    // a bed the sweep will leave empty, and why (bareLine's words). A planted cell whose bed is loaded all round and
    // has nothing dry to stand on within work range is unreachable before a step is taken: the primitive's own walk
    // would answer the same "no walkable path" for every such bed, and two of those in a row end the sweep
    const unreachable = job => {
      const sight = jobSight(job)
      return loadedAround(cellAt, sight.at, sight.range) && !standingSpots({ target: sight.at, blockAt: api.block, range: sight.range, see: false }).length
    }
    // water standing on a bed is no job of farmJobs (nothing to dig, nothing to plant into): counted here, and named
    const flooded = () => plan.cells.filter(c => PLAN_LEGEND[c.ch]?.kind === 'crop' && api.block(c.x, c.y + 1, c.z)?.name === 'water')
      .map(c => ({ why: 'water', note: `${c.x},${c.y + 1},${c.z}` }))

    // Where the sweep starts. This used to walk to within 2 of the plan's middle: in a finished field that is a bed
    // walled in by crops on every side, and a walk steps round crops, so the pathfinder had no node to end in and ran
    // its time out, or its search radius ("no walkable path"), where farm.harvest right after it walks to the field's
    // EDGE and works (Jizo, 09-26). The edge is the nearest cell the body can stand in, dry, within work range of the
    // plan (src/field.mjs fieldEdge). With nothing loaded round the plan there is no edge to read yet: then a walk to
    // the middle at a range that reaches the rim (half the plan's diagonal, and 2 at least) loads it, and the edge is read again
    const cellAt = (x, y, z) => cellOf(api.block(x, y, z))
    const approach = Math.max(2, Math.ceil(Math.hypot(plan.parsed.width, plan.parsed.height) / 2))
    const walkToEdge = async () => {
      const edge = () => fieldEdge(cellAt, plan.cells, api.pos())
      const spot = edge() ?? await leg({ x: middle.x, y: middle.y, z: middle.z, range: approach }).then(edge)
      if (spot) await leg({ x: spot.x, y: spot.y, z: spot.z, range: 1 })
    }

    const sweep = async () => {
      // no way to the field is the sweep's stuck= line, not its crash: the next day tries again
      const noWay = await walkToEdge().then(() => null, e => e.message)
      if (noWay) { summary.stuck = summary.stuck ?? noWay; summary.sweeps++; api.report(ordered(summary)); return }
      // the plan is only worth following once the ground is in sight and it points at the right level: a plan anchored
      // one block low would have me till the dirt UNDER somebody's farm and plant seed inside their farmland
      const anchor = planAnchor(plan.cells, api.block)
      if (anchor.off) throw new Error(`${plan.name} is not where its plan says: ${anchor.note}`)
      // rubble over the beds is nobody's job here (maintain only puts back what the plan asks for), but the driver should be told
      const rubble = clutterBlocks(plan.cells, api.block)
      if (rubble.length) summary.clutter = `${clutterLine(rubble)} standing over the plan: ./mc farm.tidy place=${a.place}`
      await api.checkpoint({ canDeposit: Boolean(chest) })
      const cut = await api.act('farm.harvest', { within }).catch(e => { summary.stuck = summary.stuck ?? e.message; return {} })
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
      // cell. So when any job wants water and none is carried, a bucket is fetched (the nearest still source within
      // range: waterShortfall in src/builder.mjs) BEFORE the list is read, and a channel that still stays dry is skipped
      // with the one reason there is: no bucket at all, or no water within range (card 72e49b3d)
      const listed = () => farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() })
      const water = listed().some(j => j.item === 'water_bucket') ? await waterShortfall(api) : null
      const all = listed()
      const jobs = all.filter(j => j.do !== 'skip')
      // a dry channel's skip is the water reason alone; a flowing cell's keeps its own words (a source first), then the reason
      const dryWhy = j => !water || j.item !== 'water_bucket' ? j.why : /is dry and I carry no water/.test(j.why) ? water : `${j.why}; ${water}`
      const dry = all.filter(j => j.do === 'skip').map(j => `${j.x},${j.y},${j.z} (${dryWhy(j)})`)
      // channels capped the old way, with a bottom slab, still hold their water and are left alone: no churn on a
      // working field. But they walk worse than a top slab would (a half-step down into every one), so they are
      // counted, once per sweep, with how to raise them
      const low = lowSlabs(plan.cells, api.block)
      if (low.length) summary.lowSlabs = lowSlabLine(low.length)
      const short = {}
      const bare = flooded()
      const bareBeds = new Set()
      const leave = (job, why, note) => { bare.push({ why, note }); bareBeds.add(bedKey(job)) }
      // a till with no hoe is not tried: the whole sweep used to die on the second one (the runner's "twice in a row")
      // before a single plant job on ready farmland had run. Its bed is counted untilled, and so is the seed meant
      // for it: seed thrown on dirt is a failure too, and a wasted one
      const hoe = hasHoe(api.inv())
      const untilled = new Set(hoe ? [] : jobs.filter(j => j.do === 'till').map(bedKey))
      for (const job of jobs) {
        if (job.do === 'till' && !hoe) { leave(job, 'untilled', NO_HOE); continue }
        if (job.do === 'plant' && untilled.has(bedKey(job))) continue
        // one bucket bills for a whole field but empties on the first pour: the pockets are asked again before every
        // job that wants water, the bucket filled again when they are empty, and a cell that cannot have it is skipped
        // with the reason. A pour names the block under the channel cell; the cell itself is what is said
        if (job.item === 'water_bucket' && !((api.inv().water_bucket ?? 0) > 0)) {
          const why = await waterShortfall(api)
          if (why) { dry.push(`${job.x},${job.do === 'pour' ? job.y + 1 : job.y},${job.z} (${why})`); continue }
        }
        // the pockets now, not the job list: seed runs out halfway through a field, and the place primitive's own
        // "you carry no" would be the same failure for every bed after it
        const seedless = job.do === 'plant' && !((api.inv()[job.item] ?? 0) > 0)
        if (seedless) leave(job, 'no seed', job.item)
        if (seedless || (job.item && job.item !== 'water_bucket' && !job.have)) { short[job.item] = (short[job.item] ?? 0) + 1; continue }
        if (job.do === 'plant' && unreachable(job)) { leave(job, 'unreachable', `${job.x},${job.y},${job.z}`); continue }
        const failed = await tryJob(job)
        if (failed && job.do === 'till') { untilled.add(bedKey(job)); leave(job, 'untilled', failed) }
        if (failed && job.do === 'plant') leave(job, bareWhy(failed), `${job.x},${job.y},${job.z}`)
        await api.checkpoint({ canDeposit: Boolean(chest) })
      }
      if (dry.length) summary.skipped = dry.join('; ')
      if (dug.length) summary.dug = dug.join('; ')
      if (Object.keys(short).length) summary.missing = shortLine(short)
      const bareSaid = bareLine(bare)
      if (bareSaid) summary.bare = bareSaid
      else delete summary.bare
      // whatever a done job left undone: reported, never silently repeated (the next sweep picks it up). A bed
      // bare= already accounts for is not "unfinished" as well
      const left = jobs.length ? farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() }).filter(j => j.do !== 'skip' && (!j.item || j.have) && !bareBeds.has(bedKey(j))) : []
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
      api.report(ordered(summary))
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
      if (!(a.days > 0)) return ordered(summary)
      await nextDay()
    }
  }
}
