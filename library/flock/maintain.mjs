// Keep one pen's flock at the size it should be: breed it up, shear the sheep, cull what is over (never the last pair),
// pick up what fell and put the produce in the pen's chest. The pen is the truth of how many there are, not my memory.
import { flockPlan, flockSurplus, planStructure, placeTarget } from '../../src/lib.mjs'
import { penHolds } from '../../src/pens.mjs'

export default {
  doc: 'flock.maintain mob= place=|x= y= z= size= [days=] [cull=] [shear=] [deposit=]: keep a pen at the flock size you name, shear it and put the produce away',
  stops: 'days= done, a step that failed twice, or nothing left it can do',
  args: { mob: 'string!', place: 'string', x: 'number', y: 'number', z: 'number', size: 'number!', days: 'number', until: 'number', within: 'number', cull: 'boolean', shear: 'boolean', deposit: 'boolean' },

  async run (api, a) {
    const aim = placeTarget(api.places(), a, 'flock.maintain')
    if (aim.error) throw new Error(aim.error)
    const at = aim.at
    const within = a.within ?? 24
    // the pen's own chest, when the place has a plan that marks one
    const marked = a.place ? api.places().find(p => p.name === a.place) : null
    const chest = marked?.plan?.includes('C') ? planStructure(api.plan(a.place).cells, 'C') : null
    const summary = { rounds: 0, bred: 0, culled: 0, sheared: 0 }
    const tryAct = async (action, args) => {
      const done = await api.act(action, args).then(r => r, e => { summary.stuck = summary.stuck ?? e.message; return null })
      await api.checkpoint({ canDeposit: Boolean(chest) })
      return done
    }

    const round = async () => {
      await api.act('goto', { ...at, range: 1 })
      await api.checkpoint({ canDeposit: Boolean(chest) })
      // the gate I just walked through stands open, and an open gate makes the pen read as open country: shut it, or
      // every animal in here counts as outside (a sheep sheared and a census of 0, 09-22)
      const pen = await penHolds(api, at)
      summary.pen = pen.held.pen
      // counted from the pen itself: the ones outside the fence are not mine to breed or cull
      const seen = await api.act('animals', { mob: a.mob, within, ...at })
      const mine = seen.found.filter(e => e.inMyPen)
      const grown = mine.filter(e => e.grown)
      const plan = flockPlan({ mob: a.mob, grown: grown.length, young: mine.length - grown.length, size: a.size, cull: a.cull !== false })
      summary.flock = plan.why
      summary.short = plan.do === 'wait' ? plan.why : undefined
      if (plan.do === 'breed' && await tryAct('flock.breed', { mob: a.mob, within })) summary.bred++
      for (const e of plan.do === 'cull' ? grown.slice(0, plan.count) : []) {
        await tryAct('attack', { mob: a.mob, id: e.id })
        summary.culled++
      }
      // a sheep sheared yesterday has no wool yet: the normal state of a tidy flock, not a failure worth reporting
      const wool = a.shear !== false && a.mob === 'sheep' ? await api.act('shear', { within: 8 }).then(r => r, e => ({ error: e.message })) : null
      summary.sheared += wool?.tried ?? 0
      summary.stuck = summary.stuck ?? (wool?.error && !/no sheep with wool/.test(wool.error) ? wool.error : undefined)
      await api.checkpoint({ canDeposit: Boolean(chest) })
      await tryAct('collect', {})
      if (chest && a.deposit !== false) {
        const surplus = flockSurplus(api.inv())
        if (Object.keys(surplus).length && await tryAct('deposit', { items: surplus, x: chest.x, y: chest.y, z: chest.z })) {
          summary.deposited = { ...(summary.deposited ?? {}), ...surplus }
        }
      }
      summary.rounds++
      api.report(summary)
    }

    // one round a day: wait out the daylight, let the runner put me to bed, then look again at dawn
    const nextDay = async () => {
      await api.until(() => api.clock().night, { timeout: 1200, every: 10, what: 'the day never ended' })
      await api.checkpoint({ canDeposit: Boolean(chest) })
      await api.until(() => api.clock().day, { timeout: 1200, every: 10, what: 'the night never ended' })
    }

    for (;;) {
      await round()
      await api.checkpoint({ canDeposit: Boolean(chest) })
      if (!(a.days > 0)) return summary
      await nextDay()
    }
  }
}
