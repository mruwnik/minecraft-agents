// Build a saved farm plan on the ground it names: level what is in the way, lay the floor, then till, pour, plant and
// place everything the plan asks for. The same job list farm.maintain uses to mend a farm builds one from bare ground.
import { buildFromPlan } from '../../src/build/plan.mjs'
import { farmApi, reportFarmAttention } from '../../src/farm/attention.mjs'

export default {
  doc: 'farm.build place= [partial=]: level the ground a saved plan needs, then till, pour, plant and place everything on it',
  stops: 'the plan is built, or remaining work needs attention; without partial= a material shortage leaves the build untouched',
  args: { place: 'string!', partial: 'boolean', until: 'number' },

  async run (api, a) {
    const summary = await buildFromPlan(farmApi(api), a, { farm: true })
    reportFarmAttention(api, { action: 'farm.build', place: a.place, summary })
    return summary
  }
}
