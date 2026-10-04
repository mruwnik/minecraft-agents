// Farming in one call: dig every RIPE crop nearby, put its seed straight back in the ground, cut bamboo and sugar cane
// at the second segment so the base regrows, then pick the drops up. It works where I STAND: goto the field first.
// A crop behind a fence or across water must not cost the whole harvest, so what it cannot reach is reported, not thrown.
import { cropNames, ripeCrop, harvestOrder, isStalkCut, stalkReplant, STALKS, workRefusal, planCells, replantBatch } from '../../src/lib.mjs'
import { WORK_RANGE } from '../../src/navigation/walk.mjs'
import { cellOf, fieldEdge, standingLine, plantOrder, fieldCrops, bareReason, notReplantedLine } from '../../src/farm/field.mjs'
import { farmApi, recoverFarm, reportFarmAttention } from '../../src/farm/attention.mjs'
import { FARM_FRUITS, farmFruitAt } from '../../src/farm/fruit.mjs'

const WITHIN = 24
const GIVE_UP = 4

const key = c => `${c.x},${c.y},${c.z}`
const add = (into, name) => { into[name] = (into[name] ?? 0) + 1 }

// where to stand at a named field: the nearest cell on its edge (or a lane through it) the body can stand in, and how far
// to reach from there to cover the plan. Never its centre: a walk steps round crops, so a dense field's centre has no cell a
// walk can end in, and the search timed out (card f30fd998). A plan with no cells is walked to as marked
const walkToField = async (api, field) => {
  const cells = planCells(field)
  if (!cells.length) { await api.act('goto', { x: field.x, y: field.y, z: field.z, range: 2 }); return { cells: null, edge: null, span: WITHIN } }
  const cellAt = (x, y, z) => cellOf(api.block(x, y, z))
  const chosen = () => fieldEdge(cellAt, cells, api.pos())
  // nothing seen from here: the anchor is a rim cell of the plan, and its chunks are loaded once the body stands near it
  const edge = chosen() ?? await api.act('goto', { x: field.x, y: field.y + 1, z: field.z, range: 4 }).then(chosen)
  if (!edge) throw new Error(`${field.name}: nowhere to stand within ${WORK_RANGE} of any cell of its plan: leave a . path or a covered channel through the field, or walk to its edge and call farm.harvest within=`)
  await api.act('goto', { x: edge.x, y: edge.y, z: edge.z, range: 1 })
  return { cells, edge, span: edge.span }
}

export default {
  doc: 'farm.harvest [place=] [within=24] [replant=true]: dig ripe crops, harvest melon/pumpkin fruit beside compatible stems without cutting the stems, cut stalks above the base and pick up the drops. replant=false leaves sowing to farm.maintain after it clears the field. place= walks to the edge of a saved farm first and cuts only the crops of that plan; fruit and its stem must both lie inside its footprint. harvested= counts what I cut, lost= what never reached my pockets',
  stops: 'nothing ripe left within reach, or four crops in a row it cannot walk to',
  args: { within: 'number', place: 'string', replant: 'boolean' },

  async run (api, a) {
    api = farmApi(api)
    const field = a.place ? api.places().find(p => p.name === a.place) : null
    if (a.place && !field) throw new Error(`no place called ${a.place}: ./mc places kind=farm lists the farms there are`)
    // somebody else's field is theirs unless the note they wrote says otherwise, and that question is asked in exactly
    // one place for every tool that works marked ground (#144)
    const refusal = field && workRefusal(field, api.me?.())
    if (refusal) throw new Error(refusal)
    const at = field ? await walkToField(api, field).catch(recoverFarm(e => ({ stuck: e.message }))) : { cells: null, edge: null }
    if (at.stuck) { reportFarmAttention(api, { action: 'farm.harvest', place: a.place, summary: at }); return at }
    const within = a.within ?? at.span ?? WITHIN
    const standing = at.edge ? standingLine(at.edge) : undefined
    if (standing) api.report({ standing })
    const nameAt = c => api.block(c.x, c.y, c.z)?.name
    const cellAt = (x, y, z) => cellOf(api.block(x, y, z))
    const harvested = {}
    const unreachable = []
    let replanted = 0

    // ---- crops
    const { positions: found = [] } = await api.act('find_blocks', { block: [...cropNames, ...FARM_FRUITS], maxDistance: within, count: 2000 })
    const cells = fieldCrops(at.cells, found).filter(c => cropNames.includes(nameAt(c)))
    const ripeAt = c => {
      const block = api.block(c.x, c.y, c.z)
      const seed = block && ripeCrop(block.name, block.properties?.age)
      return seed ? { name: block.name, seed } : null
    }
    const ripe = cells.filter(ripeAt)
    const plant = async (c, seed) => api.act('place', { item: seed, x: c.x, y: c.y, z: c.z }).then(() => true, recoverFarm(() => false))

    // The human, 14:26Z: "run and cut everything, then collect everything, then replant everything". Cut, chase the drop and
    // replant cell by cell was 3 s a cell. Sweep one: cut in row order, a batch dig neither waits nor chases its drop
    const cutCells = []
    let row = null
    for (const c of harvestOrder(ripe)) {
      if (unreachable.length >= GIVE_UP) break
      const crop = ripeAt(c)
      if (!crop) continue
      if (row !== null && c.z !== row) { api.report({ harvested }); await api.checkpoint() }
      row = c.z
      const cut = await api.act('dig', { x: c.x, y: c.y, z: c.z, batch: true }).then(() => true, recoverFarm(() => false))
      if (!cut) { unreachable.push(key(c)); continue }
      add(harvested, crop.name)
      cutCells.push({ x: c.x, y: c.y, z: c.z, seed: crop.seed })
    }
    if (row !== null) { api.report({ harvested }); await api.checkpoint() }

    // Melons and pumpkins have no age property: the fruit block is the harvest.
    // Its mature/attached stem remains planted and can produce the next fruit.
    for (const c of harvestOrder(found.filter(c => farmFruitAt(c, at.cells, api.block)))) {
      if (unreachable.length >= GIVE_UP) break
      const fruit = farmFruitAt(c, at.cells, api.block)
      if (!fruit) continue
      const cut = await api.act('dig', { x: c.x, y: c.y, z: c.z, batch: true }).then(() => true, recoverFarm(() => false))
      if (!cut) { unreachable.push(key(c)); continue }
      add(harvested, fruit)
      api.report({ harvested })
      await api.checkpoint()
    }

    // ---- bamboo and sugar cane: cut the second segment, the rest falls and the base grows back
    const { positions: stalks = [] } = await api.act('find_blocks', { block: STALKS, maxDistance: within, count: 2000 })
    const cutable = c => isStalkCut(nameAt(c), nameAt({ ...c, y: c.y - 1 }), nameAt({ ...c, y: c.y - 2 }))
    const cutDone = []
    let outOfReach = 0
    for (const c of harvestOrder(fieldCrops(at.cells, stalks, 2).filter(cutable))) {
      // look again now that the ones under it may be gone: this block could be the BASE by now
      if (!cutable(c)) continue
      const stalk = nameAt(c)
      const cut = await api.act('dig', { x: c.x, y: c.y, z: c.z, batch: true }).then(() => true, recoverFarm(() => false))
      if (!cut) { outOfReach++; continue }
      add(harvested, stalk)
      cutDone.push({ stalk, at: { x: c.x, y: c.y - 1, z: c.z } })
      await api.checkpoint()
    }

    // inventoryFull / inWater: a harvest that cut 23 bamboo with no free slot used to say nothing about the 23 on the ground
    // sweep two: every drop at once, over the whole field
    const { inventoryFull, inWater, couldNotReach, stillLying } = await api.act('collect', { range: within }).catch(recoverFarm(e => ({ couldNotReach: e.message })))

    // every base must still stand: one that went all the same is planted again from what was just cut
    const bases = stalkReplant(cutDone.map(c => ({ stalk: c.stalk, base: [c.at.x, c.at.y, c.at.z], baseNow: nameAt(c.at) })), Object.keys(api.inv()))
    const sow = a.replant !== false
    const stalksReplanted = sow && bases.plant.length ? (await api.act('place', { blocks: bases.plant }).catch(recoverFarm(() => ({ placed: 0 })))).placed ?? 0 : 0

    // sweep three: the seeds are in my pockets now, one place batch far end first from the edge, so the cells still bare
    // are always the ones nearer it and there is one to stand in within reach of the next (cut order walled the body in)
    const ordered = plantOrder(cutCells, at.edge)
    const batch = sow ? replantBatch(ordered, api.inv()) : []
    if (batch.length) await api.act('place', { blocks: batch }).catch(recoverFarm(() => {}))
    // what the batch missed gets one more try each, from the edge again: the cell the body stood in is what it stumbled on most
    const bare = batch.filter(c => nameAt(c) === 'air')
    if (bare.length && at.edge) await api.act('goto', { x: at.edge.x, y: at.edge.y, z: at.edge.z, range: 1 }).catch(recoverFarm(() => {}))
    const failed = []
    for (const c of bare) {
      if (await plant(c, c.item) && nameAt(c) !== 'air') continue
      failed.push({ x: c.x, z: c.z, why: bareReason(cellAt, c) })
    }
    // what I have no seed for stays bare too, and is said with the rest
    const sown = new Set(batch.map(key))
    const noSeed = sow ? ordered.filter(c => !sown.has(key(c))).map(c => ({ x: c.x, z: c.z, why: `no ${c.seed} left in my pockets` })) : []
    replanted = batch.length - failed.length

    const summary = {
      harvested,
      replanted,
      ...(!sow ? { deferred: cutCells.length + bases.plant.length } : {}),
      notReplanted: notReplantedLine([...failed, ...noSeed]),
      standing,
      stillGrowing: cells.length - ripe.length,
      // harvested counts what I CUT. A crop I dug and then could not pick up is not harvested, and saying it was sent
      // a driver away happy from a carrot still lying in the dirt (#142): what stayed on the ground is named here.
      lost: [couldNotReach, stillLying].filter(Boolean).join(' ') || undefined,
      unreachable: unreachable.length
        ? `${unreachable.length >= GIVE_UP ? 'gave up after 4' : unreachable.length} ripe crops I could not get to (first ${unreachable[0]}): stand nearer, or look for a fence or water in the way`
        : undefined,
      inventoryFull,
      inWater,
      stalkBases: cutDone.length
        ? (bases.lost
            ? `${bases.lost} of ${cutDone.length} bases were gone after the cut, ${stalksReplanted} planted again${stalksReplanted < bases.lost ? ': plant the rest by hand (place item=sugar_cane or bamboo on the bare spot, cane needs water beside it)' : ''}`
            : `all ${cutDone.length} still stand and will regrow`)
        : undefined,
      stalksOutOfReach: outOfReach
        ? `${outOfReach} stalks stand too deep in their plot to cut from outside: plant stalk plots at most 6 wide, or leave a path through`
        : undefined
    }
    reportFarmAttention(api, { action: 'farm.harvest', place: a.place, summary })
    return summary
  }
}
