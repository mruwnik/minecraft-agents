// Keep one farm going: harvest what is ripe, put back whatever the plan says should be there, store the surplus.
// The plan is the truth of what should be there; the world is the truth of what is (see `./mc plan`).
import { fertilizePlanned } from '../../src/farm/fertilize.mjs'
import { waterShortfall } from '../../src/build/plan.mjs'
import { bareLine, bareWhy, farmJobs, farmSurplus, farmWaste, hasHoe, hasWaterSource, NO_HOE, PLAN_LEGEND, planSpec, planAnchor, planStructure, sameFamily, seedDrop, seedReserve, seedTarget, shortLine } from '../../src/lib.mjs'
import { lowSlabs, lowSlabLine } from '../../src/build/cover.mjs'
import { cellOf, fieldEdge, parkSpot } from '../../src/farm/field.mjs'
import { digGuard, fieldLeg, footprintOf } from '../../src/farm/leg.mjs'
import { jobSight, standingSpots, workFrom } from '../../src/navigation/stand.mjs'
import { canStore, storeInto, storeSurplus } from '../../src/storage.mjs'
import { depositTarget } from '../../src/lib/storage.mjs'
import { fillShortfall, holeJobs, missingGround } from '../../src/lib/fill.mjs'
import { loadedAround } from '../../src/navigation/walk.mjs'
import { clutterBlocks, clutterLine, clearJobs, clearStrays, strays, zoneLine, asideLine } from './shared/clutter.mjs'
import { farmApi, recoverFarm, assertFarmRecoverable, reportFarmAttention } from '../../src/farm/attention.mjs'
import { CompositeHandBack } from '../../src/composite.mjs'
import { holdsWater } from '../../src/lib/world.mjs'
import { overheadTreeBlocks, overheadTreeLine } from '../../src/farm/overhead.mjs'

const add = (into, from = {}) => { for (const [k, n] of Object.entries(from)) into[k] = (into[k] ?? 0) + n }
// Leave room for distinct produce/drop stacks before the child harvest reaches
// its own inventory checkpoint. One pre-harvest unload per sweep is bounded.
const HARVEST_FREE_SLOTS = 4
// the plans whose seed stays in my pockets (src/lib/farm.mjs seedReserve): this one, and every other reserve_for=
// names (a homestead's, from the routine's $places), each once
const reservePlans = (api, own, names) => [own.name, ...String(names ?? '').split(',').map(n => n.trim()).filter(Boolean)]
  .filter((name, i, all) => all.indexOf(name) === i).map(name => name === own.name ? own : api.plan(name))
// the counts first, bare= right after them: the routine keeps 120 characters of a step's summary, and what stood
// behind lowSlabs= and clutter= was the line nobody read (09-26)
const SAID_FIRST = ['sweeps', 'harvested', 'replanted', 'bare', 'filled', 'tilled', 'poured', 'covered', 'built']
const ordered = summary => Object.fromEntries([
  ...SAID_FIRST.filter(k => summary[k] !== undefined).map(k => [k, summary[k]]),
  ...Object.entries(summary).filter(([k]) => !SAID_FIRST.includes(k))
])
// a bed is one cell of the plan whatever its level: the till works the ground, the plant the cell above it
const bedKey = job => `${job.x},${job.z}`
// the floor blocks the fills want that the pockets lack, from the plan's chest (its first C cell): withdraw takes
// what there is and complains of the rest, and the rest is the sweep's missing= line, not its failure
const fetchFloor = async (api, short, chest) => {
  if (!chest || !Object.keys(short).length) return
  await api.act('withdraw', { items: short, x: chest.x, y: chest.y, z: chest.z }).catch(recoverFarm(e => api.note(`no ${shortLine(short)} to fill the beds with from the chest at ${chest.x},${chest.y},${chest.z}: ${e.message}`)))
}

export default {
  doc: 'farm.maintain place= [days=] [within=] [deposit=] [compost=] [reserve_for=] [bone_meal=false]: provision a hoe, spare and food from the farm chest or carried materials, harvest the plan, clear misplaced crops and stray blocks, repair and replant the beds, refill the channels, compost the spare seed and store the surplus of one saved farm plan. deposit= is where the harvest goes: the plan\'s C chests (the default), a chest cell x,y,z, or a marked storage place; a full chest spills into the next and what nothing takes is storage_full=. compost= is a composter or chest-like block, as x,y,z or a marked place (default: the plan\'s K cell; false keeps the seed with the harvest). reserve_for=a,b names the other plans whose seed is kept out of the compost too (a routine fills it with $places). bone_meal=true optionally applies bone meal once per matching immature wheat, carrot, potato or beetroot bed before harvest, using inventory and configured compost output; compost=false keeps this inventory-only; default false leaves crops to grow naturally',
  stops: 'days= done, a step that failed twice, or nothing left it can do',
  args: { place: 'string!', days: 'number', until: 'number', within: 'number', deposit: 'any', compost: 'any', reserve_for: 'string', bone_meal: 'boolean' },

  async run (api, a) {
    if (a.bone_meal !== undefined && typeof a.bone_meal !== 'boolean') throw new Error('bone_meal must be true or false')
    api = farmApi(api)
    const plan = api.plan(a.place)
    const within = a.within ?? Math.max(8, Math.ceil(Math.hypot(plan.parsed.width, plan.parsed.height)) + 2)
    // the plan's y is the ground block; the body stands one above it, and so do the chest and composter the plan marks
    const middle = { x: plan.x + Math.floor((plan.parsed.width - 1) / 2), y: plan.y + 1, z: plan.z + Math.floor((plan.parsed.height - 1) / 2) }
    const store = depositTarget(a.deposit, api.places())
    if (store?.error) throw new Error(store.error)
    const canDeposit = canStore(store, plan.cells)
    const composter = seedTarget(planStructure(plan.cells, 'K'), api.places(), a.compost)
    const chest = planStructure(plan.cells, 'C')
    if (composter?.error) throw new Error(composter.error)
    const summary = { sweeps: 0, harvested: {}, replanted: 0, tilled: 0, poured: 0, covered: 0, built: 0 }
    // every walk of the sweep is a leg of src/farm/leg.mjs: plain first, once more with dig=true when the path fails
    // inside the plan's footprint (never a plan block, never the ground: src/farm/leg.mjs digGuard), and both failing is
    // one stuck= line naming the cell (card 72e49b3d)
    const box = footprintOf(plan.cells)
    const guard = digGuard(plan.cells)
    const dug = []
    const leg = to => fieldLeg(api, to, box, guard).then(r => { if (r?.dug) dug.push(r.dug); return r })
    const reserve = reservePlans(api, plan, a.reserve_for).map(p => p.parsed)
    const kit = async () => {
      const result = await api.act('kit', { tools: 'stone_hoe', food: 12, place: plan.name })
        .catch(recoverFarm(e => ({ kit_short: e.message })))
      if (result.kit) summary.kit = result.kit
      if (result.kit_short) summary.kit_short = result.kit_short
      else delete summary.kit_short
    }

    const tryJob = async job => {
      // a cover is only real once the cell it caps is actually holding its OWN water, a settled source, not merely a
      // neighbour's flow passing through - see src/build/plan.mjs's tryJob for the full story of the slab that kept
      // getting broken, reflooded by a neighbour's flow and blindly recapped
      if (job.do === 'cover' && !hasWaterSource(api.block(job.x, job.y, job.z))) {
        summary.stuck = summary.stuck ?? `cover ${job.x},${job.y},${job.z}: not holding water yet, so the slab was held back`
        return null
      }
      // from a cell that sees the target (src/navigation/stand.mjs): a pour from wherever "within 3" landed the body looked at the
      // next slab or a crop instead, twice on jizo-melon-patch (09-26)
      let failed = await workFrom(api, job, spot => leg({ ...spot, range: 0 })).then(() => null, recoverFarm(e => e.message))
      const after = job.do === 'fill' ? api.block(job.x, job.y, job.z) : null
      if (!failed && after && missingGround(after.name)) failed = `fill ${job.x},${job.y},${job.z} did not take: supporting ground is still ${after.name}`
      if (failed) { summary.stuck = summary.stuck ?? failed; return failed }
      if (job.do === 'plant') summary.replanted++
      if (job.do === 'till') summary.tilled++
      if (job.do === 'pour') summary.poured++
      if (job.do === 'cover') summary.covered++
      if (job.do === 'place') summary.built++
      if (job.do === 'fill') summary.filled = (summary.filled ?? 0) + 1
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
    const flooded = () => plan.cells.filter(c => planSpec(c)?.kind === 'crop' && api.block(c.x, c.y + 1, c.z)?.name === 'water')
      .map(c => ({ why: 'water', note: `${c.x},${c.y + 1},${c.z}` }))

    // Where the sweep starts. This used to walk to within 2 of the plan's middle: in a finished field that is a bed
    // walled in by crops on every side, and a walk steps round crops, so the pathfinder had no node to end in and ran
    // its time out, or its search radius ("no walkable path"), where farm.harvest right after it walks to the field's
    // EDGE and works (Jizo, 09-26). The edge is the nearest cell the body can stand in, dry, within work range of the
    // plan (src/farm/field.mjs fieldEdge). With nothing loaded round the plan there is no edge to read yet: then a walk to
    // the middle at a range that reaches the rim (half the plan's diagonal, and 2 at least) loads it, and the edge is read again
    const cellAt = (x, y, z) => cellOf(api.block(x, y, z))
    const approach = Math.max(2, Math.ceil(Math.hypot(plan.parsed.width, plan.parsed.height) / 2))
    const walkToEdge = async () => {
      const edge = () => fieldEdge(cellAt, plan.cells, api.pos())
      const spot = edge() ?? await leg({ x: middle.x, y: middle.y, z: middle.z, range: approach }).then(edge)
      if (spot) await leg({ x: spot.x, y: spot.y, z: spot.z, range: 1 })
    }

    // the report brought up to date before every checkpoint: a hand-back there (spoken to, low health) ends the task
    // with what was reported, and a sweep that reported only at its end said nothing of 43 seed sown (Jizo, 09-26 23:43Z)
    let attentionSent = false
    let overheadAttentionSent = false
    const attention = () => {
      const remaining = overheadAttentionSent ? Object.fromEntries(Object.entries(summary).filter(([key]) => key !== 'overhead_tree')) : summary
      if (!attentionSent) attentionSent = Boolean(reportFarmAttention(api, { action: 'farm.maintain', place: plan.name, summary: remaining, carried: farmSurplus(api.inv(), seedReserve(reserve, api.inv())) }))
    }
    const inventoryStop = error => {
      if (!(error instanceof CompositeHandBack) || !/^inventory full\b/i.test(error.reason)) return
      summary.inventoryFull = `${error.reason}; make room in configured storage or choose an authorized deposit= destination before retrying`
      api.report(ordered(summary))
      // An earlier warning must not hide this new capacity stop.
      reportFarmAttention(api, { action: 'farm.maintain', place: plan.name, reasons: { inventoryFull: summary.inventoryFull }, carried: farmSurplus(api.inv(), seedReserve(reserve, api.inv())) })
      attentionSent = true
    }
    const pause = () => {
      api.report(ordered(summary))
      return api.checkpoint({ canDeposit }).catch(e => { inventoryStop(e); if (/^twice in a row/.test(e.reason ?? '')) attention(); throw e })
    }
    const sweep = async () => {
      attentionSent = false
      overheadAttentionSent = false
      // no way to the field is the sweep's stuck= line, not its crash: the next day tries again
      const noWay = await walkToEdge().then(() => null, recoverFarm(e => e.message))
      if (noWay) { summary.stuck = summary.stuck ?? noWay; summary.sweeps++; api.report(ordered(summary)); attention(); return }
      // the plan is only worth following once the ground is in sight and it points at the right level: a plan anchored
      // one block low would have me till the dirt UNDER somebody's farm and plant seed inside their farmland
      const anchor = planAnchor(plan.cells, api.block)
      if (anchor.off) throw new Error(`${plan.name} is not where its plan says: ${anchor.note}`)
      // The kit and harvest both checkpoint before the normal end-of-sweep deposit. Make room using only the
      // configured store first, keeping every homestead's seed. With no room after that, report before stopping.
      if (api.freeSlots() <= HARVEST_FREE_SLOTS) {
        if (canDeposit) {
          const surplus = farmSurplus(api.inv(), seedReserve(reserve, api.inv()))
          storeInto(summary, await storeSurplus(api, { surplus, target: store, cells: plan.cells, checkError: assertFarmRecoverable }))
        }
        if (api.freeSlots() === 0) {
          summary.inventoryFull = canDeposit ? 'configured storage could not free a slot' : 'no configured storage to free a slot'
          api.report(ordered(summary))
          attention()
          throw new CompositeHandBack(`inventory full: ${summary.inventoryFull}`)
        }
      }
      // A maintenance pass owns its supplies, including when called outside a role routine. A short kit must not
      // prevent planting ready farmland: report it and do the work the carried tools and seed still permit.
      await kit()
      await pause()
      if (a.bone_meal === true) {
        const before = { fetched: summary.bone_meal_fetched ?? 0, used: summary.bone_meal_used ?? 0, attempted: summary.bone_meal_attempted ?? 0, fertilized: summary.fertilized ?? 0 }
        delete summary.bone_meal_attention
        await fertilizePlanned(api, plan.cells, growth => {
          Object.assign(summary, growth, { bone_meal_fetched: before.fetched + growth.bone_meal_fetched, bone_meal_used: before.used + growth.bone_meal_used, bone_meal_attempted: before.attempted + growth.bone_meal_attempted, fertilized: before.fertilized + growth.fertilized })
          api.report(ordered(summary))
        }, pause, composter)
      }
      const cut = await api.act('farm.harvest', { place: plan.name, within, replant: false }).catch(e => {
        if (e instanceof CompositeHandBack && /^inventory full\b/i.test(e.reason)) {
          add(summary.harvested, e.report?.harvested)
          summary.replanted += e.report?.replanted ?? 0
          inventoryStop(e)
        }
        return recoverFarm(error => { summary.stuck = summary.stuck ?? error.message; return {} })(e)
      })
      add(summary.harvested, cut.harvested)
      summary.replanted += cut.replanted ?? 0
      for (const key of ['lost', 'unreachable', 'stalksOutOfReach', 'inventoryFull']) {
        if (cut[key]) summary[key] = cut[key]
      }
      await pause()

      // after harvesting and before sowing, the rubble over the plan comes off, farm.tidy's jobs dug from this sweep's legs: it used to be counted and
      // left, and a log standing in the melon rows was where a sweep parked Jizo, one above the field (09-26 23:24Z). A
      // light, somebody's chest and a block in a zone that is not mine are named and left; the drops stay in the pockets
      // (farmSurplus stores farm goods only). The zones are asked for only when there is something to dig
      if (strays(plan.cells, api.block).length) {
        const zones = (await api.act('zones')).zones ?? []
        const { todo, guarded, aside } = clearJobs(plan.cells, api.block, zones, api.me?.())
        const { cleared, stopped } = await clearStrays(api, todo, to => leg({ ...to, range: 3 }), done => { summary.cleared = clutterLine(done); return pause() })
        if (cleared.length) {
          summary.cleared = clutterLine(cleared)
          await api.act('collect', { range: 8 }).catch(recoverFarm(e => { summary.lost = e.message; api.note(`the cleared blocks' drops were not picked up: ${e.message}`) }))
        }
        if (stopped) summary.stuck = summary.stuck ?? `clear ${stopped}`
        const standing = clutterBlocks(plan.cells, api.block).filter(b => !guarded.some(g => g.x === b.x && g.y === b.y && g.z === b.z))
        if (standing.length) summary.clutter = `${clutterLine(standing)} still standing over the plan`
        if (guarded.length) summary.inZone = zoneLine(guarded)
        if (aside.length) summary.leftAlone = asideLine(aside)
      }
      // Low clearing is not whole-tree removal. Keep the upper trunk/canopy
      // visible to the agent while continuing the ordinary repairs and sowing.
      const overhead = overheadTreeLine(overheadTreeBlocks(plan.cells, api.block))
      if (overhead) {
        summary.overhead_tree = overhead
        api.report(ordered(summary))
        // Surface the finding before a long sowing pass can be interrupted. This
        // separate notice does not suppress later supply/storage attention.
        reportFarmAttention(api, { action: 'farm.maintain', place: plan.name, reasons: { overhead_tree: overhead } })
        overheadAttentionSent = true
      }
      else delete summary.overhead_tree
      // ONE pass over the job list. This used to run twice, because a plant could report ok and leave the bed bare: the
      // place primitive counted a click the server quietly dropped as a block placed. It now reads the cell back and skips
      // instead, so a job that says it worked did work, and the field is only looked at once more to say what is still off.
      // a cell the plan's own water stands over is not work, it is a warning: dig refuses it and the sweep would give
      // up on "twice in a row". It is handed back as skipped= for the driver to drain
      // farmJobs decides pour vs. skip from whether water_bucket is ALREADY carried right here: a dry cell with none
      // becomes 'skip', not 'pour', and fetching water further down never reaches a job list that never named the
      // cell. So when any job wants water and none is carried, a bucket is fetched (the nearest still source within
      // range: waterShortfall in src/build/plan.mjs) BEFORE the list is read, and a channel that still stays dry is skipped
      // with the one reason there is: no bucket at all, or no water within range (card 72e49b3d)
      // beds, lanes and chest cells whose ground is gone (air, or water the plan never asked for: src/lib/fill.mjs) are
      // filled before anything else is tried on them, the floor block from the pockets and what those lack from the
      // plan's chest, so a field with holes is put back in one sweep instead of read as unreachable bed by bed, and the
      // lane the sweep parks on has no pit in it (jizo-melon-patch, 09-26)
      const holes = () => holeJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() })
      await fetchFloor(api, fillShortfall(holes(), api.inv()), chest)
      const listed = () => [...holes(), ...farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() })]
      let water = listed().some(j => j.item === 'water_bucket') ? await waterShortfall(api, undefined, plan.cells) : null
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
      let hadHoe = hasHoe(api.inv())
      let rekitTried = false
      const untilled = new Set()
      // a hole that stayed one (nothing to fill it with, or the fill failed) is one bare bed, said once: its till
      // and its plant are not tried on air
      const unfilled = new Set()
      const seedJobs = new Map(jobs.filter(job => job.do === 'plant').map(job => [bedKey(job), job.item]))
      const stillHole = job => { unfilled.add(bedKey(job)); leave(job, 'unfilled', `${job.x},${job.y},${job.z}`) }
      // what the jobs so far left: said after every job and at the end alike
      const sayJobs = () => {
        if (dry.length) summary.skipped = dry.join('; ')
        if (dug.length) summary.dug = dug.join('; ')
        if (Object.keys(short).length) summary.missing = shortLine(short)
        const bareSaid = bareLine(bare)
        if (bareSaid) summary.bare = bareSaid
        else delete summary.bare
      }
      for (const job of jobs) {
        if (['till', 'plant', 'place'].includes(job.do) && unfilled.has(bedKey(job))) continue
        // Empty seedless beds soon turn back to dirt. Leave them until planting
        // is possible; the following plant job reports the seed shortage once.
        if (job.do === 'till' && seedJobs.has(bedKey(job)) && !(api.inv()[seedJobs.get(bedKey(job))] > 0)) continue
        if (job.do === 'place' && job.item === 'torch' && !sameFamily('oak_fence', api.block(job.x, job.y - 1, job.z)?.name)) {
          dry.push(`${job.x},${job.y},${job.z} (torch support missing: place the fence post first)`)
          continue
        }
        if (job.do === 'till' && !hasHoe(api.inv()) && hadHoe && !rekitTried) {
          rekitTried = true
          await kit()
          summary.rekit = hasHoe(api.inv()) ? 'hoe replaced' : 'hoe broke, no spare'
        }
        if (job.do === 'till' && !hasHoe(api.inv())) { untilled.add(bedKey(job)); leave(job, 'untilled', NO_HOE); continue }
        hadHoe ||= hasHoe(api.inv())
        if (job.do === 'plant' && untilled.has(bedKey(job))) continue
        if (job.do === 'plant' && job.item === 'sugar_cane' && ![[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) => holdsWater(api.block(job.x + dx, job.y - 1, job.z + dz)))) {
          leave(job, 'water', `no adjacent water at ${job.x},${job.y},${job.z}`)
          continue
        }
        // one bucket bills for a whole field but empties on the first pour: the pockets are asked again before every
        // job that wants water, the bucket filled again when they are empty, and a cell that cannot have it is skipped
        // with the reason. A pour names the block under the channel cell; the cell itself is what is said
        if (job.item === 'water_bucket' && !((api.inv().water_bucket ?? 0) > 0)) {
          const why = water ?? await waterShortfall(api, undefined, plan.cells)
          water = why
          if (why) { dry.push(`${job.x},${job.do === 'pour' ? job.y + 1 : job.y},${job.z} (${why})`); continue }
        }
        // the pockets now, not the job list: seed (or dirt) runs out halfway through a field, and the place primitive's
        // own "you carry no" would be the same failure for every bed after it
        const outOf = (job.do === 'plant' || job.do === 'fill') && !((api.inv()[job.item] ?? 0) > 0)
        if (outOf && job.do === 'plant') leave(job, 'no seed', job.item)
        if (outOf && job.do === 'fill') stillHole(job)
        if (outOf || (job.item && job.item !== 'water_bucket' && !job.have)) { short[job.item] = (short[job.item] ?? 0) + 1; continue }
        if (job.do === 'plant' && unreachable(job)) { leave(job, 'unreachable', `${job.x},${job.y},${job.z}`); continue }
        const failed = await tryJob(job)
        if (failed && job.do === 'fill') stillHole(job)
        if (failed && job.do === 'till') { untilled.add(bedKey(job)); leave(job, 'untilled', failed) }
        if (failed && job.do === 'plant') leave(job, bareWhy(failed), `${job.x},${job.y},${job.z}`)
        sayJobs()
        await pause()
      }
      sayJobs()
      // whatever a done job left undone: reported, never silently repeated (the next sweep picks it up). A bed
      // bare= already accounts for is not "unfinished" as well
      const left = jobs.length ? farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() }).filter(j => j.do !== 'skip' && (!j.item || j.have) && !bareBeds.has(bedKey(j))) : []
      if (left.length) summary.unfinished = left.map(j => `${j.do} ${j.x},${j.y},${j.z} (${j.why})`).join('; ')

      // the spare seed goes to the composter (or the chest-like block compost= names) BEFORE the harvest is stored:
      // stored seed was the whole complaint (09-26). A target that cannot take it is said, and the seed stays with the harvest
      // Generic beds can use different seed after a harvest; reserve from what is carried now, not what was
      // carried before cutting and sowing the field.
      const keep = seedReserve(reserve, api.inv())
      const waste = composter ? farmWaste(farmSurplus(api.inv(), keep)) : {}
      const drop = Object.keys(waste).length ? seedDrop(api.block(composter.x, composter.y, composter.z)) : null
      if (Object.keys(waste).length && !drop) summary.compost = `${api.block(composter.x, composter.y, composter.z)?.name ?? 'nothing'} at ${composter.x},${composter.y},${composter.z} is neither a composter nor a chest, so the seed stays with the harvest`
      const composted = drop ? await api.act(drop, { items: waste, x: composter.x, y: composter.y, z: composter.z }).then(done => done?.fed ?? Object.entries(waste).map(([k, n]) => `${k}:${n}`).join(' '), recoverFarm(e => { summary.compost = e.message; return null })) : null
      if (composted) summary.composted = composted
      // the harvest goes where deposit= says (src/storage.mjs): the plan's chests in order, a chest cell, or a storage
      // place; a chest that fills spills into the next, and what nothing takes is storage_full=, never stuck
      const surplus = farmSurplus(api.inv(), keep)
      for (const name of composted ? Object.keys(waste) : []) delete surplus[name]
      const stored = await storeSurplus(api, { surplus, target: store, cells: plan.cells, checkError: assertFarmRecoverable })
      storeInto(summary, stored)
      // the last walk of a sweep: off the beds, onto the plan's lane (or the field's edge, at the range the sweep's
      // first walk uses for it), so the body is never left in the middle of a planted pocket for the night or the next
      // step (src/farm/field.mjs parkSpot). A plain walk, no dig retry: a parking walk that fails is said, not stuck
      const park = parkSpot(cellAt, plan.cells, api.pos())
      if (park) {
        await api.act('goto', { x: park.x, y: park.y, z: park.z, range: park.why === 'lane' ? 0 : 1 })
          .then(() => { summary.parked = `${park.x},${park.y},${park.z} (${park.why})` }, recoverFarm(e => { summary.parking = e.message; api.note(`could not end the sweep on the ${park.why} cell at ${park.x},${park.y},${park.z}: ${e.message}`) }))
      }
      summary.sweeps++
      api.report(ordered(summary))
      attention()
    }

    // one sweep a day: wait out the daylight, let the runner put me to bed, then go again at dawn
    const nextDay = async () => {
      await api.until(() => api.clock().night, { timeout: 1200, every: 10, what: 'the day never ended' })
      await api.checkpoint({ canDeposit })
      await api.until(() => api.clock().day, { timeout: 1200, every: 10, what: 'the night never ended' })
    }

    for (;;) {
      await sweep()
      await api.checkpoint({ canDeposit })
      if (!(a.days > 0)) return ordered(summary)
      await nextDay()
    }
  }
}
