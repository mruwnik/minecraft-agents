// Build a saved farm plan on the ground it names: level what is in the way, lay the floor, then till, pour, plant and
// place everything the plan asks for. The same job list farm.maintain uses to mend a farm builds one from bare ground.
import { buildFromPlan } from '../../src/builder.mjs'

export default {
  doc: 'farm.build place= [partial=]: level the ground a saved plan needs, then till, pour, plant and place everything on it',
  stops: 'the plan is built, a step that failed twice, or (without partial=) the materials are short before it starts',
  args: { place: 'string!', partial: 'boolean', until: 'number' },

  run: (api, a) => buildFromPlan(api, a)
}
