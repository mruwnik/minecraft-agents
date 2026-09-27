// The kit a day's work needs, before the work: the role's tools with a spare, and rations above a threshold, taken from
// the plot's C chest or made on the grid from what is carried and what the chest holds (card 6cf481c0: a hoe broke
// mid-routine with no spare and the side craft superseded the routine). What it could not provide is said in
// kit_short=, and the routine runs it again when a step wears a tool out.
import { kitPlan, kitLine, toolList } from '../src/inventory/kit.mjs'
import { planStructure, placeRefusal } from '../src/lib.mjs'
import { REGISTRY } from '../src/blueprint/format.mjs'

const isFood = name => Boolean(REGISTRY.foodsByName[name])
const cellOf = text => { const [x, y, z] = String(text).split(',').map(Number); return [x, y, z].every(Number.isFinite) ? { x, y, z } : null }
const key = c => `${c.x},${c.y},${c.z}`

// the chest: chest=x,y,z, else the C cell of the place's plan (a place with no plan, or no C in it, has no chest)
const chestCell = (api, a) => {
  if (a.chest) return cellOf(a.chest)
  if (!a.place) return null
  let plan = null
  try { plan = api.plan(a.place) } catch { return null }
  return planStructure(plan.cells, 'C')
}

export default {
  doc: 'kit [tools=stone_hoe,shears] [spare=1] [food=12] [place=<a plan with a C chest>] [chest=x,y,z]: carry each tool with spare= more (any tier of the kind counts), and food= rations, taking from the chest or crafting from what is carried and what the chest holds (sticks from planks, planks from a log, bread from wheat); says kit= and kit_short=',
  stops: 'the kit is complete, or what stays short is named',
  args: { tools: 'any', spare: 'number', food: 'number', place: 'string', chest: 'string' },

  async run (api, a) {
    const refusal = a.place ? placeRefusal(api.places(), a.place, api.me?.()) : null
    if (refusal) throw new Error(refusal)
    const tools = toolList(a.tools)
    const chest = chestCell(api, a)
    const contents = chest ? (await api.act('chest_contents', chest)).items ?? {} : null
    const plan = kitPlan({ tools, spare: a.spare ?? 1, food: a.food ?? 12, carried: api.inv(), chest: contents, chestAt: chest ? key(chest) : null, isFood })
    const notes = []
    if (Object.keys(plan.take).length) {
      await api.act('withdraw', { items: plan.take, ...chest }).catch(e => notes.push(`withdraw: ${e.message}`))
      await api.checkpoint()
    }
    for (const { item, count } of plan.craft) {
      await api.act('craft', { item, count }).catch(e => notes.push(`craft ${item}: ${e.message}`))
    }
    await api.checkpoint()
    // the world is the verdict: the line is read off what is carried now, not off the plan
    const short = [...plan.short, ...notes]
    return {
      kit: kitLine(tools, api.inv(), isFood),
      ...(short.length ? { kit_short: short.join('; ') } : {}),
      ...(Object.keys(plan.take).length ? { took: Object.entries(plan.take).map(([n, c]) => `${n}:${c}`).join(' ') } : {}),
      ...(plan.craft.length ? { crafted: plan.craft.map(c => `${c.item}:${c.count}`).join(' ') } : {}),
      ...(chest ? { chest: key(chest) } : {})
    }
  }
}
