// Clear the rubble off a farm. A walk that bridged a gap, a pathfinder that towered on cobblestone, a tree that grew
// into the field: they leave blocks standing over the beds and paths that the plan never asked for, shading the crops
// and breaking the walk. This digs each of them and picks the drops up. It never touches what the plan DOES ask for,
// never the crop its plan allows, never a light or somebody's chest, and never a block inside a protected
// zone that is not this body's: that ground belongs to whoever named the zone.
import { planAnchor, planCells, workRefusal } from '../../src/lib.mjs'
import { clutterBlocks, clutterKinds, clearJobs, clearStrays, zoneLine, asideLine } from './shared/clutter.mjs'
import { farmApi, recoverFarm, reportFarmAttention } from '../../src/farm/attention.mjs'
import { overheadTreeBlocks, overheadTreeLine } from '../../src/farm/overhead.mjs'

const RANGE = 48

export default {
  doc: 'farm.tidy [place=] [range=48]: dig stray blocks and mismatched crops over a saved farm plan and pick up the drops. It never digs crops allowed by the plan, a light, somebody\'s chest, anything inside a zone that is not mine, or a field whose owner does not invite work. Partial clearing reports farm_attention; invalid plans and safety stops halt the action',
  stops: 'the plan is clear, a block it cannot reach, stop, hurt, hungry, or a full inventory',
  args: { place: 'string', range: 'number' },

  async run (api, a) {
    api = farmApi(api)
    const here = api.pos()
    const range = a.range ?? RANGE
    const named = api.places().filter(p => p.plan && (a.place ? p.name === a.place : Math.hypot(p.x - here.x, p.z - here.z) <= range))
    if (!named.length) {
      throw new Error(a.place
        ? `no plan called ${a.place}: ./mc places kind=farm lists the ones there are`
        : `no farm plan within ${range} blocks: save one with ./mc farm.plan`)
    }

    // somebody else's field is theirs unless the note they wrote says otherwise: the same question farm.harvest asks,
    // asked once for every tool that works marked ground (#144). A sweep of every plan in range skips theirs quietly
    // and sweeps the rest; a sweep of one field by name says why it will not touch it
    const me = api.me?.()
    const refusals = named.map(p => workRefusal(p, me)).filter(Boolean)
    const allowed = named.filter(p => !workRefusal(p, me))
    if (!allowed.length) throw new Error(refusals[0])

    // a plan whose y is a block off would have me dig the block over somebody's crops, or the crops themselves
    const anchors = allowed.map(p => ({ p, anchor: planAnchor(planCells(p), api.block) }))
    const off = anchors.filter(({ anchor }) => anchor.off)
    if (off.length && a.place) throw new Error(`${a.place} is not where its plan says: ${off[0].anchor.note}`)
    const plans = anchors.filter(({ anchor }) => !anchor.off).map(({ p }) => p)
    if (!plans.length) throw new Error(`${off[0].p.name} is not where its plan says: ${off[0].anchor.note}`)

    const zones = (await api.act('zones')).zones ?? []
    const { todo, guarded, aside } = clearJobs(plans.flatMap(p => planCells(p)), api.block, zones, me)
    const names = plans.map(p => p.name).join(',')
    const zoneNames = [...new Set(guarded.map(b => b.zone))]
    if (guarded.length && !todo.length) {
      throw new Error(`the ${guarded.length} stray block${guarded.length > 1 ? 's' : ''} over ${names} ${guarded.length > 1 ? 'stand' : 'stands'} inside the protected zone ${zoneNames.join(' and ')}, which is not mine: ask whoever named it, or have them unprotect it`)
    }

    const report = extra => ({
      plans: names,
      cleared: 0,
      left: clutterBlocks(plans.flatMap(p => planCells(p)), api.block).length,
      inZone: zoneLine(guarded),
      leftAlone: asideLine(aside),
      ...extra
    })
    const finish = extra => {
      const summary = report(extra)
      const overhead = overheadTreeLine(overheadTreeBlocks(plans.flatMap(p => planCells(p)), api.block))
      if (overhead) {
        summary.overhead_tree = overhead
        delete summary.already
      }
      reportFarmAttention(api, { action: 'farm.tidy', place: names, summary })
      return summary
    }
    if (!todo.length) return finish({ already: `${names} has nothing over it that its plan does not ask for` })

    const { cleared, stopped } = await clearStrays(api, todo, where => api.act('goto', { ...where, range: 3 }))
    const { picked, lost } = await api.act('collect', { range: 8 }).catch(recoverFarm(e => ({ lost: e.message })))
    return finish({
      cleared: cleared.length,
      kinds: clutterKinds(cleared) || undefined,
      picked,
      ...(lost ? { lost } : {}),
      ...(stopped ? { stuck: stopped } : {})
    })
  }
}
