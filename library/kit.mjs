// The kit a day's work needs, before the work: the role's tools with a spare, and rations above a threshold, taken from
// the plot's C chest or made on the grid from what is carried and what the chest holds (card 6cf481c0: a hoe broke
// mid-routine with no spare and the side craft superseded the routine). What it could not provide is said in
// kit_short=, and the routine runs it again when a step wears a tool out.
import { kitPlan, kitLine, toolList } from '../src/inventory/kit.mjs'
import { planStructure, placeRefusal } from '../src/lib.mjs'
import { REGISTRY } from '../src/blueprint/format.mjs'
import { STORAGE_BLOCKS } from '../src/lib/storage.mjs'
import { farmApi, recoverFarm } from '../src/farm/attention.mjs'

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
    api = farmApi(api)
    const refusal = a.place ? placeRefusal(api.places(), a.place, api.me?.()) : null
    if (refusal) throw new Error(refusal)
    const tools = toolList(a.tools)
    const chest = chestCell(api, a)
    const notes = []
    let contents = null
    if (chest) {
      const block = api.block(chest.x, chest.y, chest.z)
      // Plans can mark a chest that has not been built. Do not try to open known air (and poison the failure
      // counter); a missing or inaccessible chest still leaves the carried supplies available for crafting.
      if (block && !STORAGE_BLOCKS.includes(block.name)) notes.push(`chest at ${key(chest)} is missing (${block.name} there)`)
      else contents = await api.act('chest_contents', chest).then(r => r.items ?? {}, recoverFarm(e => { notes.push(`chest at ${key(chest)}: ${e.message}`); return null }))
    }
    const plan = kitPlan({ tools, spare: a.spare ?? 1, food: a.food ?? 12, carried: api.inv(), chest: contents, chestAt: chest ? key(chest) : null, isFood })
    if (Object.keys(plan.take).length) {
      await api.act('withdraw', { items: plan.take, ...chest }).catch(recoverFarm(e => notes.push(`withdraw: ${e.message}`)))
      await api.checkpoint()
    }
    for (const { item, count } of plan.craft) {
      await api.act('craft', { item, count }).catch(recoverFarm(e => notes.push(`craft ${item}: ${e.message}`)))
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
