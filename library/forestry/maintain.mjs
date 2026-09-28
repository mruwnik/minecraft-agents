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
import { cleanupScaffoldRecoverably } from '../../src/scaffold/recovery.mjs'
import { recoverOwnDigHole } from '../../src/survival/recover-hole.mjs'

export default {
  doc: 'forestry.maintain place= [days=] [deposit=] [source=] [bone_meal=false]: inspect planned trees, harvest accessible wood, leave leaves to decay, return after two minutes for drops, restore adjacent planned flowers and replant. A nest in this body\'s claimed forest is moved intact with Silk Touch when available; otherwise maintain uses a lit campfire 1–5 blocks directly below with a clear smoke column, destroys that claimed nest, and removes the campfire before cutting wood. If smoke cannot be verified, it retains the hive and reports the missing setup. Uses the same saved plan/legend as farm.plan. Supplies come only from planned chests by default; source= selects an explicit chest/marked storage point and source=false is inventory-only. Uses and cleans temporary dirt/wood pillars when scaffolding is unavailable; can also craft scaffolding from available bamboo/string at a planned crafting table. Protected/ambiguous trees and missing supplies/access emit forestry_attention; safety stops remain hard',
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
    const forestScope = plan.kind === 'forest' && plan.by === api.me?.()
      ? { place: a.place, owner: plan.by, x: plan.x, z: plan.z }
      : null
    const summary = { place: a.place, sweeps: 0, harvested: 0, planted: 0, attention: [] }
    const ownHole = await recoverOwnDigHole(api, api.ownSurvivalHole?.())
    if (ownHole.attempted) {
      if (!ownHole.recovered) {
        summary.attention.push(...ownHole.attention)
        api.report?.(summary)
        treeAttention(api, 'forestry.maintain', summary)
        return summary
      }
      summary.survival_hole_restored = ownHole.at
      api.report?.(summary)
    }
    const checkpoint = () => api.checkpoint({ canDeposit })
    const journalled = typeof api.forestry === 'function'
    const recordFor = cell => journalled ? api.forestry(scaffoldId(cell)) : null
    const retainedAttachments = new Map()
    const waitForDecay = async readyAt => {
      // Checkpoints remain live while a tree decays. Persisted records use wall-clock
      // deadlines, so a retry waits only for the part of the interval still due.
      while (Date.now() < readyAt) {
        await checkpoint()
        const seconds = Math.min(10, Math.ceil((readyAt - Date.now()) / 1000))
        await api.pause?.(seconds)
      }
      await checkpoint()
    }
    const plantedAt = args => {
      const p = treeProfile(args.species, args.form)
      for (let dx = 0; dx < p.width; dx++) for (let dz = 0; dz < p.width; dz++) {
        const block = api.block(args.x + dx, args.y + 1, args.z + dz)?.name
        if (![p.plant, ...(p.species === 'azalea' ? ['flowering_azalea'] : [])].includes(block)) return false
      }
      return true
    }
    const acknowledge = name => api.acknowledgeFailure?.(name)
    const recoverRecord = async (cell, record, args) => {
      await waitForDecay(Number(record.readyAt) || Date.now())
      const arrived = await api.act('goto', { x: args.x, y: args.y + 1, z: args.z, range: 3, dig: false }).then(
        () => true,
        recoverFarm(e => { summary.attention.push(e.message); return false })
      )
      if (!arrived) { await acknowledge('goto'); return false }
      await checkpoint()
      const collected = await api.act('collect', { range: Math.min(16, treeProfile(args.species, args.form).radius + 2) }).then(
        result => result,
        recoverFarm(e => { summary.attention.push(e.message); return null })
      )
      if (collected === null) { await acknowledge('collect'); return false }
      summary.decay_collected = (summary.decay_collected ?? 0) + 1
      await plantSite(args, { decayRecovery: true })
      // Never discard the only recovery record on an unverified or partial replant.
      if (!plantedAt(args)) return false
      if (journalled) api.forestry(scaffoldId(cell), null)
      summary.decaying = (summary.decaying ?? []).filter(p => p.x !== cell.x || p.y !== cell.y || p.z !== cell.z)
      api.report?.(summary)
      return true
    }
    const plantSite = async (args, { decayRecovery = false } = {}) => {
      const planted = await runTree(api, args, 'plant', decayRecovery ? { decayRecovery: true } : undefined)
      summary.planted += planted.planted ?? 0
      summary.attention.push(...planted.attention)
      if (!planted.attention.length && treeProfile(args.species, args.form).requiresMeal && !a.bone_meal) summary.attention.push(`${args.species} at ${args.x},${args.y},${args.z} requires bone meal to grow; enable bone_meal=true with supplies`)
      if (!planted.attention.length && a.bone_meal) {
        if (!(api.inv().bone_meal > 0)) summary.attention.push(`missing bone_meal for ${args.species} at ${args.x},${args.y},${args.z}`)
        else {
          await checkpoint()
          const fresh = await runTree(api, args, 'check')
          if (!fresh.attention.length && fresh.state === 'sapling') {
            const before = api.inv().bone_meal
            await api.act('fertilize', { x: args.x, y: args.y + 1, z: args.z }).catch(recoverFarm(e => { summary.attention.push(e.message) }))
            summary.bone_meal_used = (summary.bone_meal_used ?? 0) + Math.max(0, before - (api.inv().bone_meal ?? 0))
          } else summary.attention.push(...fresh.attention)
        }
      }
    }
    for (;;) {
      summary.attention = []
      const decaying=[]
      const blocked = new Set()
      const newlyDecaying = new Set()
      const supply = await treeSupplies(api, plan, a.source, summary)
      await provisionTreeBasics(api, plan, supply, summary)
      await maintainTreeServices(api, plan.cells, summary)
      treeAttention(api, 'forestry.maintain', summary)
      // A previous invocation may have stopped after cutting wood. Drain those
      // planned records before inspecting any sites for a new harvest.
      if (journalled) for (const cell of trees) {
        let record = recordFor(cell)
        if (!record || !['harvesting', 'decaying'].includes(record.phase)) continue
        const spec = treeSpec(cell)
        const args = { x: cell.x, y: cell.y, z: cell.z, place: a.place, species: record.species ?? spec.species, form: record.form ?? spec.form ?? 'auto' }
        const flower = plan.cells.find(c => planSpec(c)?.kind === 'flower' && Math.max(Math.abs(c.x - cell.x), Math.abs(c.z - cell.z)) <= 2 && c.y === cell.y)
        if (flower) args.flower = planSpec(flower).item
        if (record.phase === 'harvesting') {
          await checkpoint()
          const cut = await runTree(api, args, 'harvest', { allowForestHives: true })
          summary.harvested += cut.harvested ?? 0
          if (cut.hivesMoved?.length) summary.hivesMoved = [...(summary.hivesMoved ?? []), ...cut.hivesMoved]
          if (cut.hivesDestroyed?.length) summary.hivesDestroyed = [...(summary.hivesDestroyed ?? []), ...cut.hivesDestroyed]
          summary.attention.push(...cut.attention)
          record = recordFor(cell)
          if (!record || record.phase !== 'decaying') { blocked.add(scaffoldId(cell)); continue }
        }
        summary.decaying = [...(summary.decaying ?? []), { x: cell.x, y: cell.y, z: cell.z }]
        if (!await recoverRecord(cell, record, args)) blocked.add(scaffoldId(cell))
      }
      for (const cell of trees) {
        if (blocked.has(scaffoldId(cell))) continue
        await checkpoint()
        const spec = treeSpec(cell)
        const flower = plan.cells.find(c => planSpec(c)?.kind === 'flower' && Math.max(Math.abs(c.x - cell.x), Math.abs(c.z - cell.z)) <= 2 && c.y === cell.y)
        const args = { x: cell.x, y: cell.y, z: cell.z, place: a.place, species: spec.species, form: spec.form ?? 'auto', ...(flower ? { flower: planSpec(flower).item } : {}) }
        const id = scaffoldId(cell)
        const attachment = retainedAttachments.get(id)
        if (attachment && api.scaffolds?.(id) && api.block(attachment.x, attachment.y, attachment.z)?.name === 'scaffolding') {
          summary.attention.push(`unrecorded scaffold connects at ${attachment.x},${attachment.y},${attachment.z}; retained until ownership changes`)
          continue
        }
        retainedAttachments.delete(id)
        if (api.scaffolds?.(scaffoldId(cell))) {
          if (forestScope) {
            const record = api.scaffolds(id)
            if (record) api.scaffolds(id, { ...record, forestScope })
          }
          const recovery = { root: { x: cell.x, y: cell.y, z: cell.z }, attention: [] }
          await cleanupScaffoldRecoverably(api, cleanupScaffold, scaffoldId(cell), recovery)
          if (recovery.cleanup_left.length || recovery.attention.length) {
            const linked = recovery.attention.join(' ').match(/unrecorded scaffold connects at (-?\d+),(-?\d+),(-?\d+)/)
            if (linked) retainedAttachments.set(id, { x: Number(linked[1]), y: Number(linked[2]), z: Number(linked[3]) })
            treeAttention(api, 'forestry.maintain', recovery); summary.attention.push(...recovery.attention); summary.cleanup_left = recovery.cleanup_left; continue
          }
        }
        let check = await runTree(api, args, 'check', { allowForestHives: true })
        if (check.attention.length) { treeAttention(api, 'forestry.maintain', check); summary.attention.push(...check.attention); continue }
        if (check.state === 'mature') {
          await provisionTreeScaffold(api, plan, cell, supply, summary)
          const cut = await runTree(api, args, 'harvest', { allowForestHives: true })
          summary.harvested += cut.harvested ?? 0
          if (cut.hivesMoved?.length) summary.hivesMoved = [...(summary.hivesMoved ?? []), ...cut.hivesMoved]
          if (cut.hivesDestroyed?.length) summary.hivesDestroyed = [...(summary.hivesDestroyed ?? []), ...cut.hivesDestroyed]
          summary.attention.push(...cut.attention)
          if (cut.attention.length || cut.remaining?.length) continue
          if(cut.decay_wait){
            if (journalled) {
              const record = recordFor(cell)
              if (record?.phase === 'decaying') { blocked.add(scaffoldId(cell)); newlyDecaying.add(scaffoldId(cell)) }
            } else decaying.push(args)
            continue
          }
        }
        await plantSite(args)
        api.report?.(summary)
      }
      if (journalled) for (const cell of trees) {
        if (!newlyDecaying.has(scaffoldId(cell))) continue
        const record = recordFor(cell)
        if (!record || record.phase !== 'decaying') continue
        const spec = treeSpec(cell)
        const args = { x: cell.x, y: cell.y, z: cell.z, place: a.place, species: record.species ?? spec.species, form: record.form ?? spec.form ?? 'auto' }
        const flower = plan.cells.find(c => planSpec(c)?.kind === 'flower' && Math.max(Math.abs(c.x - cell.x), Math.abs(c.z - cell.z)) <= 2 && c.y === cell.y)
        if (flower) args.flower = planSpec(flower).item
        summary.decaying = [...(summary.decaying ?? []), { x: cell.x, y: cell.y, z: cell.z }]
        await recoverRecord(cell, record, args)
      }
      if(decaying.length){
        summary.decaying=decaying.map(({x,y,z})=>({x,y,z}))
        api.report?.(summary)
        // One shared decay interval after the last cut; other sites were worked
        // first. Checkpoints keep cancellation, hunger and night handling active.
        for(let seconds=0;seconds<120;seconds+=10){await checkpoint();await api.pause?.(10)}
        for(const args of decaying){
          await checkpoint()
          const arrived=await api.act('goto',{x:args.x,y:args.y+1,z:args.z,range:3,dig:false}).then(()=>true,recoverFarm(e=>{summary.attention.push(e.message);return false}))
          if(!arrived){await acknowledge('goto');continue}
          const collected=await api.act('collect',{range:Math.min(16,treeProfile(args.species,args.form).radius+2)}).then(()=>true,recoverFarm(e=>{summary.attention.push(e.message);return false}))
          if(!collected){await acknowledge('collect');continue}
          summary.decay_collected=(summary.decay_collected??0)+1
          await plantSite(args)
          if(plantedAt(args)) summary.decaying=summary.decaying.filter(p=>p.x!==args.x||p.y!==args.y||p.z!==args.z)
          api.report?.(summary)
        }
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
      // A tree can grow or leaves can drop well before the next dawn. Revisit
      // the whole plot at a bounded cadence so multi-day maintenance remains
      // responsive while the runner still reports an intentional wait.
      const nextSweepAt = Date.now() + 45000
      await api.until(() => api.clock().night || api.clock().elapsedDays >= a.days || Date.now() >= nextSweepAt,
        { timeout: 60, every: 5, what: 'next forestry sweep' })
      await checkpoint()
      if (api.clock().elapsedDays >= a.days) return summary
    }
  }
}
