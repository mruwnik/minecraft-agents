import { treeSupplies, provisionTreeBasics, provisionTreeScaffold } from '../../src/tree/provision.mjs'
import { maintainTreeServices } from '../../src/tree/services.mjs'
import { cleanupScaffold, scaffoldId } from '../../src/scaffold/access.mjs'
import { workRefusal } from '../../src/lib/places.mjs'
import { planSpec } from '../../src/lib/plan.mjs'
import { treeSpec } from '../../src/tree/inspect.mjs'
import { treeProfile } from '../../src/tree/profiles.mjs'
import { runTree, treeAttention } from '../../src/tree/actions.mjs'
import { farmApi, recoverFarm, assertFarmRecoverable } from '../../src/farm/attention.mjs'
import { depositTarget } from '../../src/lib/storage.mjs'
import { canStore, storeSurplus } from '../../src/storage.mjs'

export default {
  doc: 'forestry.maintain place= [days=] [deposit=] [source=] [bone_meal=false]: inspect planned trees, safely harvest complete accessible trees, collect, restore adjacent planned flowers and replant. Uses the same saved plan/legend as farm.plan. Supplies come only from planned chests by default; source= selects an explicit chest/marked storage point and source=false is inventory-only. Crafts needed scaffolding from available bamboo/string at a planned crafting table. Protected/ambiguous trees and missing supplies/access emit forestry_attention; safety stops remain hard',
  args: { place: 'string!', days: 'number', deposit: 'any', source: 'any', bone_meal: 'boolean' },
  async run (api, a) {
    if (a.days !== undefined && (!Number.isInteger(a.days) || a.days < 0)) throw new Error('days must be a nonnegative integer')
    if (a.bone_meal !== undefined && typeof a.bone_meal !== 'boolean') throw new Error('bone_meal must be true or false')
    api = farmApi(api)
    const plan = api.plan(a.place)
    const refusal = workRefusal(plan, api.me?.())
    if (refusal) throw new Error(refusal)
    const trees = plan.cells.filter(c => treeSpec(c))
    if (!trees.length) throw new Error(`${a.place} has no tree planting cells in its plan`)
    const target = depositTarget(a.deposit, api.places())
    if (target?.error) throw new Error(target.error)
    const canDeposit = canStore(target, plan.cells)
    const summary = { place: a.place, sweeps: 0, harvested: 0, planted: 0, attention: [] }
    const checkpoint = () => api.checkpoint({ canDeposit })
    for (;;) {
      summary.attention = []
      const supply = await treeSupplies(api, plan, a.source, summary)
      await provisionTreeBasics(api, plan, supply, summary)
      await maintainTreeServices(api, plan.cells, summary)
      treeAttention(api, 'forestry.maintain', summary)
      for (const cell of trees) {
        await checkpoint()
        const spec = treeSpec(cell)
        const flower = plan.cells.find(c => planSpec(c)?.kind === 'flower' && Math.max(Math.abs(c.x - cell.x), Math.abs(c.z - cell.z)) <= 2 && c.y === cell.y)
        const args = { x: cell.x, y: cell.y, z: cell.z, place: a.place, species: spec.species, form: spec.form ?? 'auto', ...(flower ? { flower: planSpec(flower).item } : {}) }
        if (api.scaffolds?.(scaffoldId(cell))) {
          const recovery = { root: { x: cell.x, y: cell.y, z: cell.z }, attention: [] }
          await cleanupScaffold(api, scaffoldId(cell), recovery)
          if (recovery.cleanup_left.length || recovery.attention.length) { treeAttention(api, 'forestry.maintain', recovery); summary.attention.push(...recovery.attention); summary.cleanup_left = recovery.cleanup_left; continue }
        }
        let check = await runTree(api, args, 'check')
        if (check.attention.length) { treeAttention(api, 'forestry.maintain', check); summary.attention.push(...check.attention); continue }
        if (check.state === 'mature') {
          await provisionTreeScaffold(api, plan, cell, supply, summary)
          const cut = await runTree(api, args, 'harvest')
          summary.harvested += cut.harvested ?? 0
          summary.attention.push(...cut.attention)
          if (cut.attention.length || cut.remaining?.length) continue
        }
        const planted = await runTree(api, args, 'plant')
        summary.planted += planted.planted ?? 0
        summary.attention.push(...planted.attention)
        if (!planted.attention.length && treeProfile(spec.species, spec.form).requiresMeal && !a.bone_meal) summary.attention.push(`${spec.species} at ${cell.x},${cell.y},${cell.z} requires bone meal to grow; enable bone_meal=true with supplies`)
        if (!planted.attention.length && a.bone_meal) {
          if (!(api.inv().bone_meal > 0)) summary.attention.push(`missing bone_meal for ${spec.species} at ${cell.x},${cell.y},${cell.z}`)
          else {
            await checkpoint()
            const fresh = await runTree(api, args, 'check')
            if (!fresh.attention.length && fresh.state === 'sapling') {
              const before = api.inv().bone_meal
              await api.act('fertilize', { x: cell.x, y: cell.y + 1, z: cell.z }).catch(recoverFarm(e => { summary.attention.push(e.message) }))
              summary.bone_meal_used = (summary.bone_meal_used ?? 0) + Math.max(0, before - (api.inv().bone_meal ?? 0))
            } else summary.attention.push(...fresh.attention)
          }
        }
        api.report?.(summary)
      }
      const keep = {}
      for (const cell of trees) {
        const spec = treeSpec(cell)
        const p = treeProfile(spec.species, spec.form)
        keep[p.plant] = (keep[p.plant] ?? 0) + p.width ** 2 * 2
      }
      for (const cell of plan.cells) if (planSpec(cell)?.kind === 'flower') keep[planSpec(cell).item] = (keep[planSpec(cell).item] ?? 0) + 1
      const produce = new Set(trees.flatMap(c => { const s = treeSpec(c); const p = treeProfile(s.species, s.form); return [...p.wood, p.plant, 'stick', 'apple'] }))
      const surplus = Object.fromEntries(Object.entries(api.inv()).filter(([item]) => produce.has(item)).map(([item, n]) => [item, Math.max(0, n - (keep[item] ?? 0))]).filter(([, n]) => n > 0))
      if (target) {
        const stored = await storeSurplus(api, { surplus, target, cells: plan.cells, checkError: assertFarmRecoverable }).catch(recoverFarm(e => ({ stuck: e.message })))
        Object.assign(summary, stored)
        for (const k of ['stuck', 'storage_full', 'chest_missing']) if (stored[k]) summary.attention.push(stored[k])
      }
      summary.sweeps++
      api.report?.(summary)
      treeAttention(api, 'forestry.maintain', summary)
      await checkpoint()
      if (!(a.days > 0) || api.clock().elapsedDays >= a.days) return summary
      const completedDay = api.clock().elapsedDays
      await api.until(() => api.clock().night || api.clock().elapsedDays > completedDay, { timeout: 1200, every: 10, what: 'the day never ended' })
      await checkpoint()
      await api.until(() => api.clock().day, { timeout: 1200, every: 10, what: 'the night never ended' })
      if (api.clock().elapsedDays >= a.days) return summary
    }
  }
}
