// Run a list of steps in order, once per game day: the chore list of a homestead.
// `routine` is itself a composite, so a role can ship one (roles/farmer/homestead.json) and `routine name=farmer/homestead` runs it.
import fs from 'node:fs'
import path from 'node:path'
import { routinePlan, unmarkedPlaces, placesRefusal, stepLabel, stopEvent, dayEvent, bedWalkEvent, nightLine, rekitVerdict, outcomeText, outcomeStalled } from '../src/routine.mjs'
import { toolsLost, toolList } from '../src/inventory/kit.mjs'
import { ownBed, nightPlan, BED_RANGE } from '../src/lib/sleep.mjs'
import { farmAct } from '../src/farm/attention.mjs'

const ROLES_DIR = path.join(import.meta.dirname, '..', 'roles')
const readRole = name => {
  const file = path.join(ROLES_DIR, `${name}.json`)
  return fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null
}

export default {
  doc: 'routine steps=|name= [place=a,b,c] [vars=\'{"compost":"shared-composter"}\'] [days=1] [store=<place|x,y,z>] [bed=<place>] [bed_range=200] [dry=true]: run a list of steps in order, once per game day (a kit step first, when the role ships one: its tools are watched through the day, and a step that wears one out gets the kit again and one more try), sleeping through the nights (a bed within 32 blocks as always; otherwise it walks to its own bed when that is within bed_range blocks: bed=<place>, else the nearest mark of kind=bed by this body, else where it last woke this run); several places run the routine once per place, in order; $place in a step is filled from place=, $places with the whole place= list, $store from store= (where the produce goes), any other $name from vars= (a JSON object; a $name nobody gave in vars= is dropped, so the step\'s own default holds); days=0 runs until stopped; dry=true only prints the expanded steps',
  stops: 'days= done (a step that fails is noted, and the next one still runs); every stop writes a routine_stopped event with its reason and advice, every day a routine_day one',
  args: { steps: 'any', name: 'string', place: 'string', store: 'string', vars: 'any', days: 'number', until: 'number', bed: 'string', bed_range: 'number', dry: 'boolean' },

  async run (api, a) {
    const { steps, places, error } = routinePlan(a, readRole)
    if (error) throw new Error(error)
    // every step would refuse on its own, but a routine is days long: it stops before day one rather than failing the
    // same way once a round for three days (#144). A dry run reads and changes nothing, so it asks no owner: it only
    // shows the day's steps (and still names a place nobody marked).
    const refusal = a.dry ? unmarkedPlaces(api.places(), places) : placesRefusal(api.places(), places, api.me?.())
    if (refusal) throw new Error(refusal)
    // bed=<place> is walked to at nightfall, days from now: a mark nobody made is refused now, like a place
    const noBed = a.bed ? unmarkedPlaces(api.places(), [a.bed]) : null
    if (noBed) throw new Error(noBed)
    if (a.dry) return { dry: true, places, steps }
    // days=0 runs until stopped. The runner's own days rule (handBackReason) reads a.days at every checkpoint and would
    // end the routine at once, so the number comes off the args the routine shares with the runner
    const forever = a.days === 0
    if (forever) delete a.days
    const summary = { days: 0, ran: 0 }
    // the day's kit step (roles ship one first): the tools it lists are watched through every other step, and when a
    // step ends with fewer of a kind than it began, a tool wore out under it. The kit runs again, and a step that
    // failed is tried once more when the kit put the tool back; the day line says "hoe replaced" or "hoe broke, no
    // spare" either way (card 6cf481c0: a hoe broke mid-routine, and the side craft for it superseded the routine)
    const kitStep = steps.find(step => step.action === 'kit') ?? null
    const tools = kitStep ? toolList(kitStep.tools) : []
    const attempt = async (action, args) => {
      if (!action.startsWith('farm.')) return api.act(action, args).then(r => r, e => ({ failed: e.message }))
      try { return await farmAct(api, action, args) } catch (error) {
        // Recoverable farm problems return attention summaries. Exceptions must reach the driver. Night is the
        // one resumable hand-back: use the existing bed commute and retry this same field once at dawn.
        if (!/^night and no bed/.test(error.reason ?? '')) throw error
        await nightfall(error)
        return farmAct(api, action, args)
      }
    }
    const rekit = async (action, args, outcome, lost) => {
      const { action: kitAction, ...kitArgs } = kitStep
      api.note(`${lost.join(', ')} wore out during ${action}: running the kit again`)
      await attempt(kitAction, kitArgs)
      const verdict = rekitVerdict(lost, api.inv())
      const again = outcome.failed && !verdict.missing.length ? await attempt(action, args) : outcome
      return { ...again, rekit: verdict.text }
    }
    // what the stuck watch is told (src/navigation/stuck.mjs): when the day began, which steps failed on which day, which days
    // ended with a full store (the harvest carried round: an alert after two), and whether the routine is stepping or
    // waiting for dusk or dawn (a wait stands still by design)
    const failedSteps = []
    const storageFull = []
    let lastDayStartedAt = Date.now()
    const progress = phase => api.progress({ routine: { lastDayStartedAt, day: summary.days, failedSteps, storageFull, phase } })
    let current = null // the step in flight, for routine_stopped; null between days

    // ---- nightfall with no bed within 32 blocks (card bebf3a5f). The runner's checkpoint sleeps when a bed is near
    // and hands back "night and no bed within 32 blocks" when none is; five nights of a farm's autopilot ended that way
    // with its bed 60 blocks off. So that hand-back is caught once a night: the own bed (src/lib/sleep.mjs ownBed)
    // within bed_range is walked to, the checkpoint is taken again there (the runner then sleeps, as it always did),
    // and at dawn the body walks back to the day's first place. Beyond bed_range, or with no bed known, the stop
    // stands and routine_stopped says which bed it knew of and why it stayed
    let sleptAt = null // where the body woke this run: the fallback bed
    let bedStop = null // why the night stop stood, for routine_stopped
    let night = null // the walk in flight { bed, distance, seconds, back }, or the last one for the day summary
    let away = false // walked to the bed tonight already: a second hand-back stands
    const walk = to => api.act('goto', { x: to.x, y: to.y, z: to.z, range: 2 })
    const timed = async fn => { const t0 = Date.now(); await fn(); return Math.round((Date.now() - t0) / 1000) }
    // from the bed to the day's first place, or to where nightfall found the body when the routine has no places
    const walkBack = async (bed, nightfallAt) => {
      const first = places.length ? api.places().find(p => p.name === places[0]) : null
      const to = first ?? nightfallAt
      const from = bed
      const distance = Math.round(Math.hypot(to.x - from.x, to.y - from.y, to.z - from.z))
      const back = await timed(() => api.act('goto', { x: to.x, y: to.y, z: to.z, range: 3 }))
        .catch(e => { api.note(`the walk back to ${first ? first.name : 'where nightfall found it'} failed (${e.message}): the steps walk to their places themselves`); return null })
      night.back = back
      api.emit('routine_bed_walk', bedWalkEvent({ leg: 'back', bed: night.bed, from, to, distance, seconds: back ?? 0 }))
    }
    const nightfall = async e => {
      if (!/^night and no bed/.test(e.reason ?? '') || away) throw e
      const from = api.pos()
      const bed = ownBed(api.places(), api.me?.(), { bed: a.bed, sleptAt, from })
      const plan = nightPlan({ near: false, bed, from, bedRange: a.bed_range ?? BED_RANGE })
      if (plan.do !== 'walk') { bedStop = plan.why; throw e }
      away = true
      progress('bed')
      const seconds = await timed(() => walk(plan.to)).catch(err => { bedStop = `the walk to ${bed.name} failed: ${err.message}`; throw e })
      night = { bed, distance: plan.distance, seconds, back: null }
      summary.bedWalks = (summary.bedWalks ?? 0) + 1
      api.emit('routine_bed_walk', bedWalkEvent({ leg: 'bed', bed, from, to: plan.to, distance: plan.distance, seconds }))
      api.note(`no bed within 32: walked ${plan.distance} blocks to ${bed.name} (${seconds}s) to sleep`)
      await api.checkpoint() // the runner sleeps here, or hands back again (a bed that is gone)
      sleptAt = api.pos()
      progress('dawn')
      await walkBack(plan.to, from)
      away = false
    }
    // every checkpoint of the routine's own goes through the night rule
    const checkpoint = () => api.checkpoint().catch(nightfall)

    const day = async () => {
      const dayNo = summary.days + 1
      lastDayStartedAt = Date.now()
      progress('steps')
      const outcomes = []
      const lastNight = night ? nightLine(night) : null
      night = null
      for (const { action, ...args } of steps) {
        current = { step: stepLabel(action, args), place: args.place ?? null }
        await checkpoint()
        const before = { ...api.inv() }
        const first = await attempt(action, args)
        const lost = action === 'kit' ? [] : toolsLost(before, api.inv(), tools)
        const outcome = lost.length && kitStep ? await rekit(action, args, first, lost) : first
        summary.ran++
        if (outcome.failed) {
          summary.failed = summary.failed ?? `${action}: ${outcome.failed}`
        }
        if (outcomeStalled(outcome)) failedSteps.push({ day: dayNo, step: current.step })
        if (outcome.storage_full && !storageFull.includes(dayNo)) storageFull.push(dayNo)
        api.note(`${current.step} ${outcomeText(outcome)}`)
        outcomes.push({ action, place: current.place, outcome })
      }
      current = null
      summary.days++
      api.report(summary)
      progress('steps')
      api.emit('routine_day', dayEvent(dayNo, outcomes, lastNight))
    }

    // A missing dusk is not a failure of the chores. Twice the machine napped through one and once a human set the time
    // to day (09-24), and each time the routine died with "the day never ended" while the apiary sat ripe. So a clock
    // that jumps backwards counts as the day having turned, and a wait that gives up starts the next round and says so.
    const nextDay = async () => {
      const start = api.clock().time
      const dusk = () => api.clock().night || api.clock().time < start
      const came = await api.until(dusk, { timeout: 1200, every: 10, what: 'the day never ended' }).then(() => true, () => false)
      if (!came) return api.note('no dusk came in 1200s of waiting (the time was set?): starting the next round')
      await checkpoint()
      sleptAt = api.pos() // the checkpoint returns at dawn, at the bed it slept in
      progress('dawn')
      await api.until(() => api.clock().day, { timeout: 1200, every: 10, what: 'the night never ended' })
        .catch(() => api.note('no dawn came in 1200s of waiting: starting the next round'))
    }

    // every way out writes routine_stopped: the routine's own days, and whatever the runner or ./mc stop threw
    const stopped = reason => api.emit('routine_stopped', stopEvent({ reason, ...current, days: a.days ?? 1, bed: bedStop }))
    try {
      for (;;) {
        await day()
        await checkpoint()
        if (!forever && !((a.days ?? 1) > summary.days)) { stopped('days'); return summary }
        progress('dusk')
        await nextDay()
      }
    } catch (e) {
      stopped(e.reason ?? e.message)
      throw e
    }
  }
}
