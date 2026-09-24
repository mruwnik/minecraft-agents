// Run a list of steps in order, once per game day: the chore list of a homestead.
// `routine` is itself a composite, so a role can ship one (roles/farmer/homestead.json) and `routine name=farmer/homestead` runs it.
import fs from 'node:fs'
import path from 'node:path'
import { routinePlan, unmarkedPlaces, placesRefusal } from '../src/routine.mjs'

const ROLES_DIR = path.join(import.meta.dirname, '..', 'roles')
const readRole = name => {
  const file = path.join(ROLES_DIR, `${name}.json`)
  return fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null
}

export default {
  doc: 'routine steps=|name= [place=a,b,c] [days=1] [dry=true]: run a list of steps in order, once per game day, sleeping through the nights; several places run the routine once per place, in order; dry=true only prints the expanded steps',
  stops: 'days= done (a step that fails is noted, and the next one still runs)',
  args: { steps: 'any', name: 'string', place: 'string', days: 'number', until: 'number', dry: 'boolean' },

  async run (api, a) {
    const { steps, places, error } = routinePlan(a, readRole)
    if (error) throw new Error(error)
    // every step would refuse on its own, but a routine is days long: it stops before day one rather than failing the
    // same way once a round for three days (#144). A dry run reads and changes nothing, so it asks no owner: it only
    // shows the day's steps (and still names a place nobody marked).
    const refusal = a.dry ? unmarkedPlaces(api.places(), places) : placesRefusal(api.places(), places, api.me?.())
    if (refusal) throw new Error(refusal)
    if (a.dry) return { dry: true, places, steps }
    const summary = { days: 0, ran: 0 }

    const day = async () => {
      for (const { action, ...args } of steps) {
        await api.checkpoint()
        const outcome = await api.act(action, args).then(r => r, e => ({ failed: e.message }))
        summary.ran++
        if (outcome.failed) summary.failed = summary.failed ?? `${action}: ${outcome.failed}`
        api.note(`${action}${outcome.failed ? ` FAILED ${outcome.failed}` : ' ok'}`)
      }
      summary.days++
      api.report(summary)
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
      await api.until(() => api.clock().day, { timeout: 1200, every: 10, what: 'the night never ended' })
        .catch(() => api.note('no dawn came in 1200s of waiting: starting the next round'))
    }

    for (;;) {
      await day()
      await api.checkpoint()
      if (!(a.days > summary.days)) return summary
      await nextDay()
    }
  }
}
