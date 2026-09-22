// Feed a composter what a farm cannot use, and take the bone meal out when it fills.
// 7 raises fill a composter; each item has its own chance of raising it (COMPOST_CHANCE), so this is a gamble per item, not a count.
import { compostPlan, planStructure } from '../../src/lib.mjs'

const RANGE = 32
// the composter to walk to: the one I was given, the one the plan marks with K, or the nearest one
async function composterAt (api, a) {
  if (a.x !== undefined) return { x: a.x, y: a.y, z: a.z }
  if (a.place) {
    const cell = planStructure(api.plan(a.place).cells, 'K')
    if (!cell) throw new Error(`${a.place} has no K (composter) cell in its plan`)
    return cell
  }
  const { positions = [] } = await api.act('find_blocks', { block: 'composter', maxDistance: a.range ?? RANGE })
  const found = positions[0]
  if (!found) throw new Error(`no composter within ${a.range ?? RANGE} blocks: craft item=composter (7 wooden slabs), place it, or pass x= y= z=`)
  return { x: found.x, y: found.y, z: found.z }
}

export default {
  doc: 'farm.compost [items=] [place=] [x= y= z=]: feed a composter the produce a farm cannot use and pocket the bone meal it makes',
  stops: 'nothing left to feed it, or no composter within range',
  args: { items: 'any', place: 'string', keep: 'any', x: 'number', y: 'number', z: 'number', range: 'number' },

  async run (api, a) {
    const at = await composterAt(api, a)
    const level = () => api.block(at.x, at.y, at.z)?.properties?.level ?? 0
    const { feed, skipped } = compostPlan(api.inv(), { want: a.items, keep: a.keep })
    const summary = { at: `${at.x},${at.y},${at.z}` }
    if (skipped.length) summary.skipped = skipped.map(s => `${s.name} (${s.why})`).join(', ')
    if (!feed.length) return { ...summary, nothing: 'nothing I carry a composter would take: seed is kept back to sow, tools and stone it refuses' }

    let boneMeal = 0
    const fed = {}
    const takeIfFull = async () => {
      if (level() < 8) return
      await api.act('use', { x: at.x, y: at.y, z: at.z })
      boneMeal++
    }
    for (const item of feed) {
      await api.act('equip', { item: item.name })
      for (let n = 0; n < item.count; n++) {
        await api.act('use', { x: at.x, y: at.y, z: at.z })
        fed[item.name] = (fed[item.name] ?? 0) + 1
        await takeIfFull()
        await api.checkpoint()
      }
      api.report({ ...summary, fed: Object.entries(fed).map(([k, n]) => `${k}:${n}`).join(' '), boneMeal })
    }
    await takeIfFull()
    return {
      ...summary,
      fed: Object.entries(fed).map(([k, n]) => `${k}:${n}`).join(' ') || undefined,
      boneMeal,
      level: level(),
      short: level() < 8 && !boneMeal ? `${8 - level()} more raises to fill it (every item is a chance, not a level)` : undefined
    }
  }
}
