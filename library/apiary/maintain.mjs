// The beekeeper's round: inspect, take only safe ripe honey, then breed when a visible colony is under the requested size.
// It never treats "no visible bees" as an empty hive: occupants are commonly indoors at dawn, dusk or in rain.
import { apiaryGoods } from '../../src/lib.mjs'

export default {
  doc: 'apiary.maintain place= [size=6] [mode=comb] [breed=true] [deposit=false]: inspect, safely harvest and tend one apiary',
  stops: 'one round is complete, a ripe hive is unsafe, or a step fails twice',
  args: { place: 'string!', size: 'number', mode: 'string', breed: 'boolean', deposit: 'boolean', range: 'number', until: 'number' },

  async run (api, a) {
    const summary = { harvested: 0, bred: 0 }
    const inspect = await api.act('apiary.inspect', { place: a.place, range: a.range })
    Object.assign(summary, { hives: inspect.hives, ripe: inspect.ripe, unsafe: inspect.unsafe, blocked: inspect.blocked, beesVisible: inspect.beesVisible, flowers: inspect.flowers })
    api.report(summary)
    await api.checkpoint()

    if (inspect.ripe) {
      const cut = await api.act('apiary.harvest', { place: a.place, mode: a.mode, range: a.range })
      summary.harvested += cut.harvested ?? 0
      summary.unsafe = cut.unsafe
      summary.blocked = cut.blocked
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
