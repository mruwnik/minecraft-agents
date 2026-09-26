// Run a list of steps in order, once per game day: the chore list of a homestead.
// `routine` is itself a composite, so a role can ship one (roles/farmer/homestead.json) and `routine name=farmer/homestead` runs it.
import fs from 'node:fs'
import path from 'node:path'
import { routinePlan, unmarkedPlaces, placesRefusal, stepLabel, stopEvent, dayEvent } from '../src/routine.mjs'

const ROLES_DIR = path.join(import.meta.dirname, '..', 'roles')
const readRole = name => {
  const file = path.join(ROLES_DIR, `${name}.json`)
  return fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null
}

export default {
  doc: 'routine steps=|name= [place=a,b,c] [vars=\'{"compost":"shared-composter"}\'] [days=1] [dry=true]: run a list of steps in order, once per game day, sleeping through the nights; several places run the routine once per place, in order; $place in a step is filled from place=, any other $name from vars= (a JSON object; a $name nobody gave in vars= is dropped, so the step\'s own default holds); days=0 runs until stopped; dry=true only prints the expanded steps',
  stops: 'days= done (a step that fails is noted, and the next one still runs); every stop writes a routine_stopped event with its reason and advice, every day a routine_day one',
  args: { steps: 'any', name: 'string', place: 'string', vars: 'any', days: 'number', until: 'number', dry: 'boolean' },

  async run (api, a) {
    const { steps, places, error } = routinePlan(a, readRole)
    if (error) throw new Error(error)
    // every step would refuse on its own, but a routine is days long: it stops before day one rather than failing the
    // same way once a round for three days (#144). A dry run reads and changes nothing, so it asks no owner: it only
    // shows the day's steps (and still names a place nobody marked).
    const refusal = a.dry ? unmarkedPlaces(api.places(), places) : placesRefusal(api.places(), places, api.me?.())
    if (refusal) throw new Error(refusal)
    if (a.dry) return { dry: true, places, steps }
    // days=0 runs until stopped. The runner's own days rule (handBackReason) reads a.days at every checkpoint and would
    // end the routine at once, so the number comes off the args the routine shares with the runner
    const forever = a.days === 0
    if (forever) delete a.days
    const summary = { days: 0, ran: 0 }
    // what the stuck watch is told (src/stuck.mjs): when the day began, which steps failed on which day, and whether
    // the routine is stepping or waiting for dusk or dawn (a wait stands still by design)
    const failedSteps = []
    let lastDayStartedAt = Date.now()
    const progress = phase => api.progress({ routine: { lastDayStartedAt, day: summary.days, failedSteps, phase } })
    let current = null // the step in flight, for routine_stopped; null between days

    const day = async () => {
      const dayNo = summary.days + 1
      lastDayStartedAt = Date.now()
      progress('steps')
      const outcomes = []
      for (const { action, ...args } of steps) {
        current = { step: stepLabel(action, args), place: args.place ?? null }
        await api.checkpoint()
        const outcome = await api.act(action, args).then(r => r, e => ({ failed: e.message }))
        summary.ran++
        if (outcome.failed) {
          summary.failed = summary.failed ?? `${action}: ${outcome.failed}`
          failedSteps.push({ day: dayNo, step: current.step })
        }
        api.note(`${action}${outcome.failed ? ` FAILED ${outcome.failed}` : ' ok'}`)
        outcomes.push({ action, place: current.place, outcome })
      }
      current = null
      summary.days++
      api.report(summary)
      progress('steps')
      api.emit('routine_day', dayEvent(dayNo, outcomes))
    }

    // A missing dusk is not a failure of the chores. Twice the machine napped through one and once a human set the time
    // to day (09-24), and each time the routine died with "the day never ended" while the apiary sat ripe. So a clock
    // that jumps backwards counts as the day having turned, and a wait that gives up starts the next round and says so.
    const nextDay = async () => {
      const start = api.clock().time
      const dusk = () => api.clock().night || api.clock().time < start
      const came = await api.until(dusk, { timeout: 1200, every: 10, what: 'the day never ended' }).then(() => true, () => false)
      if (!came) return api.note('no dusk came in 1200s of waiting (the time was set?): starting the next round')
      await api.checkpoint()
      progress('dawn')
      await api.until(() => api.clock().day, { timeout: 1200, every: 10, what: 'the night never ended' })
        .catch(() => api.note('no dawn came in 1200s of waiting: starting the next round'))
    }

    // every way out writes routine_stopped: the routine's own days, and whatever the runner or ./mc stop threw
    const stopped = reason => api.emit('routine_stopped', stopEvent({ reason, ...current, days: a.days ?? 1 }))
    try {
      for (;;) {
        await day()
        await api.checkpoint()
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
