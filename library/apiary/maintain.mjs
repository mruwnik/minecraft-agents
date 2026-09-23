// The beekeeper's round: inspect, take only safe ripe honey, then breed when a visible colony is under the requested size.
// It never treats "no visible bees" as an empty hive: occupants are commonly indoors at dawn, dusk or in rain.
import { apiaryGoods } from '../../src/lib.mjs'
import { carpetCarried } from './shared/common.mjs'
import { campfireCarried, replaceCensus } from './shared/hive.mjs'

export default {
  doc: 'apiary.maintain place= [size=6] [mode=comb] [breed=true] [deposit=false]: inspect, sink and carpet fires, safely harvest and tend one apiary',
  stops: 'one round is complete, a ripe hive is unsafe, or a step fails twice',
  args: { place: 'string!', size: 'number', mode: 'string', breed: 'boolean', deposit: 'boolean', range: 'number', until: 'number' },

  async run (api, a) {
    const summary = { harvested: 0, bred: 0 }
    const inspect = await api.act('apiary.inspect', { place: a.place, range: a.range })
    // forwarded whole rather than picked apart, so the round's line says exactly what the inspect said - including the
    // coordinates behind each count, which are what a reader checks a count against (item 18)
    const { details, grownVisible, ...census } = inspect
    Object.assign(summary, census)
    api.report(summary)
    await api.checkpoint()

    // an open fire burns bees: carpet it before any honey is touched, and stop the round if there is nothing to carpet it
    // with. A raised fire (a side in the open) is moved one block underground when a campfire is carried, else left as it is.
    if (inspect.openFires && !carpetCarried(api.inv())) throw new Error(`${inspect.openFires} open fire${inspect.openFires > 1 ? 's' : ''} under the hives and no carpet carried: bees burn in open fire, bring a carpet (2 wool make 3) and run apiary.guard`)
    if (inspect.openFires || (inspect.raisedFires && campfireCarried(api.inv()))) {
      const guard = await api.act('apiary.guard', { place: a.place, range: a.range })
      summary.carpeted = guard.carpeted ?? 0
      summary.sunk = guard.sunk ?? 0
      summary.openFires = guard.left ?? 0
      summary.raisedFires = (guard.raised ?? 0) - (guard.sunk ?? 0)
      // the fires moved, so the coordinates the inspect gave for them are no longer where they are: a stale answer is
      // worse than none, and these two counts are the guard's now, not the census's
      delete summary.openFiresAt
      delete summary.raisedFiresAt
      api.report(summary)
      await api.checkpoint()
    }

    if (inspect.ripe) {
      const cut = await api.act('apiary.harvest', { place: a.place, mode: a.mode, range: a.range })
      summary.harvested += cut.harvested ?? 0
      // the harvest read the hives again after its last click, so ITS census is the fresher one and replaces the
      // inspect's whole - coordinate lists included, or the round would still point at the hive it just emptied
      const { harvested: taken, mode: cutMode, ...fresh } = cut
      replaceCensus(summary, fresh)
      api.report(summary)
      await api.checkpoint()
    }

    const size = a.size ?? 6
    if (a.breed !== false && api.clock().day && !api.clock().raining && inspect.grownVisible >= 2 && inspect.beesVisible < size) {
      const bred = await api.act('apiary.breed', { place: a.place, count: 2, range: a.range }).catch(e => ({ skipped: e.message }))
      summary.bred += bred.fed >= 2 ? 1 : 0
      summary.breedSkipped = bred.skipped
    }
    if (a.deposit) {
      const goods = apiaryGoods(api.inv())
      if (Object.keys(goods).length) {
        await api.act('deposit', { items: goods })
        summary.deposited = goods
      }
    }
    api.report(summary)
    return summary
  }
}
