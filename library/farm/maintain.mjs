// Keep one farm going: harvest what is ripe, put back whatever the plan says should be there, store the surplus.
// The plan is the truth of what should be there; the world is the truth of what is (see `./mc plan`).
import { farmJobs, farmSurplus, jobCall, planAnchor, planBill, shortLine, SEED_ITEMS } from '../../src/lib.mjs'

const add = (into, from = {}) => { for (const [k, n] of Object.entries(from)) into[k] = (into[k] ?? 0) + n }
const cellOf = (plan, ch) => plan.cells.find(c => c.ch === ch)
// enough seed to sow the whole plan twice over stays in my pockets; the rest goes in the chest
const seedReserve = plan => Object.fromEntries(Object.entries(planBill(plan.parsed)).filter(([item]) => SEED_ITEMS.has(item)).map(([item, n]) => [item, n * 2]))

export default {
  doc: 'farm.maintain place= [days=] [within=]: harvest, replant, re-till, refill the channels and store the surplus of one saved farm plan',
  stops: 'days= done, a step that failed twice, or nothing left it can do',
  args: { place: 'string!', days: 'number', until: 'number', within: 'number', deposit: 'boolean', compost: 'boolean' },

  async run (api, a) {
    const plan = api.plan(a.place)
    const within = a.within ?? Math.max(8, Math.ceil(Math.hypot(plan.parsed.width, plan.parsed.height)) + 2)
    const middle = { x: plan.x + Math.floor((plan.parsed.width - 1) / 2), y: plan.y, z: plan.z + Math.floor((plan.parsed.height - 1) / 2) }
    const chest = cellOf(plan, 'C')
    const composter = cellOf(plan, 'K')
    const summary = { sweeps: 0, harvested: {}, replanted: 0, tilled: 0, poured: 0, built: 0 }
    const keep = seedReserve(plan)

    const tryJob = async job => {
      const [action, args] = jobCall(job)
      const failed = await api.act(action, args).then(() => null, e => e.message)
      if (failed) { summary.stuck = summary.stuck ?? failed; return }
      if (job.do === 'plant') summary.replanted++
      if (job.do === 'till') summary.tilled++
      if (job.do === 'pour') summary.poured++
      if (job.do === 'place') summary.built++
    }

    const sweep = async () => {
      await api.act('goto', { x: middle.x, y: middle.y, z: middle.z, range: 2 })
      // the plan is only worth following once the ground is in sight and it points at the right level: a plan anchored
      // one block low would have me till the dirt UNDER somebody's farm and plant seed inside their farmland
      const anchor = planAnchor(plan.cells, api.block)
      if (anchor.off) throw new Error(`${plan.name} is not where its plan says: ${anchor.note}`)
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
      const all = farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() })
      const jobs = all.filter(j => j.do !== 'skip')
      const skipped = all.filter(j => j.do === 'skip')
      if (skipped.length) summary.skipped = skipped.map(j => `${j.x},${j.y},${j.z} (${j.why})`).join('; ')
      const short = {}
      for (const job of jobs) {
        if (job.item && !job.have) { short[job.item] = (short[job.item] ?? 0) + 1; continue }
        await tryJob(job)
        await api.checkpoint({ canDeposit: Boolean(chest) })
      }
      if (Object.keys(short).length) summary.missing = shortLine(short)
      // whatever a done job left undone: reported, never silently repeated (the next sweep picks it up)
      const left = jobs.length ? farmJobs({ cells: plan.cells, worldAt: api.block, items: api.inv() }).filter(j => j.do !== 'skip' && (!j.item || j.have)) : []
      if (left.length) summary.unfinished = left.map(j => `${j.do} ${j.x},${j.y},${j.z} (${j.why})`).join('; ')

      if (chest && a.deposit !== false) {
        const surplus = farmSurplus(api.inv(), keep)
        if (Object.keys(surplus).length) {
          const failed = await api.act('deposit', { items: surplus, x: chest.x, y: chest.y, z: chest.z }).then(() => null, e => e.message)
          if (failed) summary.stuck = summary.stuck ?? failed
          else summary.deposited = { ...(summary.deposited ?? {}), ...surplus }
        }
      }
      if (composter && a.compost !== false) {
        const done = await api.act('farm.compost', { x: composter.x, y: composter.y, z: composter.z }).catch(() => null)
        if (done?.fed) summary.composted = done.fed
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
