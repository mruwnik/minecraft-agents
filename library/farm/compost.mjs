// Feed a composter what a farm cannot use, and take the bone meal out when it fills.
// 7 raises fill a composter; each item has its own chance of raising it (COMPOST_CHANCE), so this is a gamble per item, not a count.
import { compostPlan, planStructure } from '../../src/lib.mjs'
import { farmApi, recoverFarm, reportFarmAttention } from '../../src/farm/attention.mjs'

const RANGE = 32
// the composter to walk to: the one I was given, the one the plan marks with K, or the nearest one. A plan drawn
// before it had a composter of its own (or one meant to share a neighbour's, standing outside every plan's own
// footprint) is not stranded: it falls through to the same range search a bare call gets, rather than refusing
// by name for a K cell nobody ever asked the plan to have
async function composterAt (api, a) {
  if (a.x !== undefined) return { x: a.x, y: a.y, z: a.z }
  if (a.place) {
    const cell = planStructure(api.plan(a.place).cells, 'K')
    if (cell) return cell
  }
  const { positions = [] } = await api.act('find_blocks', { block: 'composter', maxDistance: a.range ?? RANGE })
  const found = positions[0]
  if (!found) return null
  return { x: found.x, y: found.y, z: found.z }
}

export default {
  doc: 'farm.compost [items=] [place=] [x= y= z=]: feed a composter the produce a farm cannot use and pocket the bone meal it makes',
  stops: 'nothing left to feed it, or no composter within range',
  args: { items: 'any', place: 'string', keep: 'any', x: 'number', y: 'number', z: 'number', range: 'number' },

  async run (api, a) {
    api = farmApi(api)
    if (['x', 'y', 'z'].some(k => a[k] !== undefined) && !['x', 'y', 'z'].every(k => Number.isFinite(a[k]))) throw new Error('give all three finite coordinates: x= y= z=')
    if (a.range !== undefined && (!Number.isFinite(a.range) || a.range <= 0)) throw new Error('range= must be a positive number')
    const at = await composterAt(api, a)
    if (!at) {
      const attention = `no composter within ${a.range ?? RANGE} blocks: craft item=composter (7 wooden slabs), place it, or pass x= y= z=`
      reportFarmAttention(api, { action: 'farm.compost', place: a.place, reasons: { composter_missing: attention } })
      return { attention }
    }
    const level = () => api.block(at.x, at.y, at.z)?.properties?.level ?? 0
    const { feed, skipped } = compostPlan(api.inv(), { want: a.items, keep: a.keep })
    const summary = { at: `${at.x},${at.y},${at.z}` }
    const problems = new Set()
    const finish = result => {
      reportFarmAttention(api, { action: 'farm.compost', place: a.place, reasons: {
        ...(a.items && skipped.length ? { requested_items: summary.skipped } : {}),
        ...(problems.size ? { compost: [...problems].join('; ') } : {})
      } })
      return { ...result, ...(problems.size ? { attention: [...problems].join('; ') } : {}) }
    }
    if (skipped.length) summary.skipped = skipped.map(s => `${s.name} (${s.why})`).join(', ')
    if (!feed.length) return finish({ ...summary, nothing: 'nothing I carry a composter would take: seed is kept back to sow, tools and stone it refuses' })
    const target = api.block(at.x, at.y, at.z)
    if (!target) {
      problems.add(`the composter at ${summary.at} is not loaded: walk within sight and retry; no items were fed`)
      return finish(summary)
    }
    if (target && target.name !== 'composter') {
      if (a.x !== undefined) throw new Error(`${summary.at} is ${target.name}, not a composter: choose the correct x= y= z=`)
      problems.add(`the composter at ${summary.at} is missing: repair it or choose another composter`)
      return finish(summary)
    }

    let boneMeal = 0
    const fed = {}
    const takeIfFull = async () => {
      if (level() < 8) return
      await api.act('use', { x: at.x, y: at.y, z: at.z })
      boneMeal++
    }
    const emptyFull = () => takeIfFull().then(() => true, recoverFarm(e => { problems.add(e.message); return false }))
    const progress = () => ({ ...summary, fed: Object.entries(fed).map(([k, n]) => `${k}:${n}`).join(' ') || undefined, boneMeal })
    // Empty a ready composter before feeding; taking bone meal consumes no seed.
    if (!await emptyFull()) return finish(progress())
    feeding: for (const item of feed) {
      const equipped = await api.act('equip', { item: item.name }).then(() => true, recoverFarm(e => { problems.add(e.message); return false }))
      if (!equipped) continue
      for (let n = 0; n < item.count; n++) {
        const before = api.inv()[item.name] ?? 0
        const used = await api.act('use', { x: at.x, y: at.y, z: at.z }).then(() => true, recoverFarm(e => { problems.add(e.message); return false }))
        if (!used) break
        // a clean resolve is not proof the composter took it: the server can reject the use (range, wrong hand,
        // a stale block state) and still answer without error, so only an item that actually left the inventory counts
        const consumed = before - (api.inv()[item.name] ?? 0)
        if (consumed <= 0) { problems.add(`${item.name} did not leave my inventory feeding the composter at ${summary.at}: check range and that it is equipped`); break feeding }
        fed[item.name] = (fed[item.name] ?? 0) + consumed
        if (!await emptyFull()) break feeding
        api.report(progress())
        await api.checkpoint()
      }
      api.report({ ...summary, fed: Object.entries(fed).map(([k, n]) => `${k}:${n}`).join(' '), boneMeal })
    }
    return finish({
      ...summary,
      fed: Object.entries(fed).map(([k, n]) => `${k}:${n}`).join(' ') || undefined,
      boneMeal,
      level: level(),
      short: level() < 8 && !boneMeal ? `${8 - level()} more raises to fill it (every item is a chance, not a level)` : undefined
    })
  }
}
