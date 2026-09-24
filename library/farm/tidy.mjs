// Clear the rubble off a farm. A walk that bridged a gap, a pathfinder that towered on cobblestone, a tree that grew
// into the field: they leave blocks standing over the beds and paths that the plan never asked for, shading the crops
// and breaking the walk. This digs each of them and picks the drops up. It never touches what the plan DOES ask for,
// never a crop (that is farm.harvest's work), never a light or somebody's chest, and never a block inside a protected
// zone that is not this body's: that ground belongs to whoever named the zone.
import { harvestOrder, planAnchor, planCells, workRefusal } from '../../src/lib.mjs'
import { strays, clutterBlocks, clutterKinds, foreignZone } from './shared/clutter.mjs'

const RANGE = 48

// top down, then row by row across the field, so the body sweeps it once instead of criss-crossing it and a stack of
// rubble comes off from the top (gravel and sand under a dug block would otherwise fall into the cell below)
const sweepOrder = blocks => harvestOrder([...blocks].sort((a, b) => b.y - a.y))

export default {
  doc: 'farm.tidy [place=] [range=48]: dig every stray block standing over a saved farm plan (dirt, cobblestone, logs, a stray sapling) and pick up the drops. It never digs what the plan asks for, a crop, a light, somebody\'s chest, anything inside a zone that is not mine, or a field somebody else marked whose note does not invite the work',
  stops: 'the plan is clear, a block it cannot reach, stop, hurt, hungry, or a full inventory',
  args: { place: 'string', range: 'number' },

  async run (api, a) {
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

    const found = plans.flatMap(p => strays(planCells(p), api.block))
    const zones = (await api.act('zones')).zones ?? []
    const guarded = []
    const todo = []
    for (const block of found.filter(b => !b.keep)) {
      const zone = foreignZone(zones, me, block)
      if (zone) guarded.push({ ...block, zone: zone.name })
      else todo.push(block)
    }
    const names = plans.map(p => p.name).join(',')
    const zoneNames = [...new Set(guarded.map(b => b.zone))]
    if (guarded.length && !todo.length) {
      throw new Error(`the ${guarded.length} stray block${guarded.length > 1 ? 's' : ''} over ${names} ${guarded.length > 1 ? 'stand' : 'stands'} inside the protected zone ${zoneNames.join(' and ')}, which is not mine: ask whoever named it, or have them unprotect it`)
    }

    const aside = found.filter(b => b.keep)
    const report = extra => ({
      plans: names,
      cleared: 0,
      left: clutterBlocks(plans.flatMap(p => planCells(p)), api.block).length,
      ...(zoneNames.length ? { inZone: zoneNames.map(z => `${z}:${guarded.filter(b => b.zone === z).length}`).join(' ') } : {}),
      ...(aside.length ? { leftAlone: aside.map(b => `${b.name}@${b.x},${b.y},${b.z}`).join(' ') } : {}),
      ...extra
    })
    if (!todo.length) return report({ already: `${names} has nothing over it that its plan does not ask for` })

    const cleared = []
    let stopped
    for (const block of sweepOrder(todo)) {
      const where = { x: block.x, y: block.y, z: block.z }
      const failed = await api.act('goto', { ...where, range: 3 }).then(() => null, e => e.message) ??
        await api.act('dig', where).then(() => null, e => e.message)
      if (failed) { stopped = `${block.name} at ${block.x},${block.y},${block.z}: ${failed}`; break }
      // believe the world, not the click: a dig the server quietly dropped leaves the block standing
      if (api.block(block.x, block.y, block.z)?.name !== block.name) cleared.push(block)
      api.report({ cleared: cleared.length })
      await api.checkpoint()
    }
    const { picked } = await api.act('collect', { range: 8 }).catch(() => ({}))
    return report({
      cleared: cleared.length,
      kinds: clutterKinds(cleared) || undefined,
      picked,
      ...(stopped ? { stopped } : {})
    })
  }
}
