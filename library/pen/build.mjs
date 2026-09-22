// Raise a saved pen plan and prove it holds. A pen that looks finished and leaks is worse than no pen: the animals are
// gone by morning and nobody knows why, so this one fails rather than report done while an animal could walk out.
import { buildFromPlan } from '../../src/builder.mjs'
import { penInside } from '../../src/lib.mjs'

export default {
  doc: 'pen.build place= [partial=]: raise a saved pen plan, then stand inside and check it holds',
  stops: 'the pen stands and holds, a step that failed twice, or (without partial=) the materials are short before it starts',
  args: { place: 'string!', partial: 'boolean', until: 'number' },

  async run (api, a) {
    const built = await buildFromPlan(api, a)
    const inside = penInside(api.plan(a.place).cells)
    if (!inside) return { ...built, advice: 'the plan marks no floor inside the walls (a `.` cell), so I cannot stand in it to check that it holds' }
    await api.checkpoint()
    const held = await api.act('pen.check', inside)
    api.report({ ...built, ...held })
    if (held.pen !== 'holds') throw new Error(`${a.place} stands but leaks via ${held.via}: ${held.advice}`)
    return { ...built, ...held }
  }
}
