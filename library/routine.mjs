// Run a list of steps in order, once per game day: the chore list of a homestead.
// `routine` is itself a composite, so a role can ship one (roles/farmer/homestead.json) and `routine name=farmer/homestead` runs it.
import fs from 'node:fs'
import path from 'node:path'
import { routineSteps } from '../src/lib.mjs'

const ROLES_DIR = path.join(import.meta.dirname, '..', 'roles')
const readRole = name => {
  const file = path.join(ROLES_DIR, `${name}.json`)
  return fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null
}

export default {
  doc: 'routine steps=|name= [place=] [days=1]: run a list of steps in order, once per game day, sleeping through the nights',
  stops: 'days= done (a step that fails is noted, and the next one still runs)',
  args: { steps: 'any', name: 'string', place: 'string', days: 'number', until: 'number' },

  async run (api, a) {
    const { steps, error } = routineSteps(a, readRole)
    if (error) throw new Error(error)
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

    // A missing dusk is not a failure of the chores. Twice the machine napped through one and once Dan set the time
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
