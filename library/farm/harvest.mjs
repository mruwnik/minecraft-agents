// Farming in one call: dig every RIPE crop nearby, put its seed straight back in the ground, cut bamboo and sugar cane
// at the second segment so the base regrows, then pick the drops up. It works where I STAND: goto the field first.
// A crop behind a fence or across water must not cost the whole harvest, so what it cannot reach is reported, not thrown.
import { cropNames, ripeCrop, harvestOrder, isStalkCut, stalkReplant, STALKS } from '../../src/lib.mjs'

const WITHIN = 24
const GIVE_UP = 4

const key = c => `${c.x},${c.y},${c.z}`
const add = (into, name) => { into[name] = (into[name] ?? 0) + 1 }

export default {
  doc: 'farm.harvest [within=24]: dig every ripe crop around me, replant it, cut stalks above the base and pick up the drops',
  stops: 'nothing ripe left within reach, or four crops in a row it cannot walk to',
  args: { within: 'number' },

  async run (api, a) {
    const within = a.within ?? WITHIN
    const nameAt = c => api.block(c.x, c.y, c.z)?.name
    const harvested = {}
    const unreachable = []
    const bare = []
    let replanted = 0

    // ---- crops
    const { positions: cells = [] } = await api.act('find_blocks', { block: cropNames, maxDistance: within, count: 2000 })
    const ripeAt = c => {
      const block = api.block(c.x, c.y, c.z)
      const seed = block && ripeCrop(block.name, block.properties?.age)
      return seed ? { name: block.name, seed } : null
    }
    const ripe = cells.filter(ripeAt)
    const plant = async (c, seed) => api.act('place', { item: seed, x: c.x, y: c.y, z: c.z }).then(() => true, () => false)

    for (const c of harvestOrder(ripe)) {
      if (unreachable.length >= GIVE_UP) break
      const crop = ripeAt(c)
      if (!crop) continue
      const cut = await api.act('dig', { x: c.x, y: c.y, z: c.z }).then(() => true, () => false)
      if (!cut) { unreachable.push(key(c)); continue }
      add(harvested, crop.name)
      // the seed drops where the crop stood: plant one I already carry, or come back for this spot once the drops are in
      if (await plant(c, crop.seed)) replanted++
      else bare.push({ c, seed: crop.seed })
      api.report({ harvested, replanted })
      await api.checkpoint()
    }

    // ---- bamboo and sugar cane: cut the second segment, the rest falls and the base grows back
    const { positions: stalks = [] } = await api.act('find_blocks', { block: STALKS, maxDistance: within, count: 2000 })
    const cutable = c => isStalkCut(nameAt(c), nameAt({ ...c, y: c.y - 1 }), nameAt({ ...c, y: c.y - 2 }))
    const cutDone = []
    let outOfReach = 0
    for (const c of harvestOrder(stalks.filter(cutable))) {
      // look again now that the ones under it may be gone: this block could be the BASE by now
      if (!cutable(c)) continue
      const stalk = nameAt(c)
      const cut = await api.act('dig', { x: c.x, y: c.y, z: c.z }).then(() => true, () => false)
      if (!cut) { outOfReach++; continue }
      add(harvested, stalk)
      cutDone.push({ stalk, at: { x: c.x, y: c.y - 1, z: c.z } })
      await api.checkpoint()
    }

    // inventoryFull / inWater: a harvest that cut 23 bamboo with no free slot used to say nothing about the 23 on the ground
    const { inventoryFull, inWater } = await api.act('collect', { range: 10 })

    // every base must still stand: one that went all the same is planted again from what was just cut
    const bases = stalkReplant(cutDone.map(c => ({ stalk: c.stalk, base: [c.at.x, c.at.y, c.at.z], baseNow: nameAt(c.at) })), Object.keys(api.inv()))
    const stalksReplanted = bases.plant.length ? (await api.act('place', { blocks: bases.plant }).catch(() => ({ placed: 0 }))).placed ?? 0 : 0

    // second round, now that the seeds are in my pockets: a body that arrived with none used to replant nothing at all
    for (const { c, seed } of bare) {
      if (nameAt(c) !== 'air') continue
      if (await plant(c, seed)) replanted++
    }

    const notReplanted = Object.entries(harvested).filter(([name]) => !STALKS.includes(name)).reduce((n, [, count]) => n + count, 0) - replanted
    return {
      harvested,
      replanted,
      notReplanted: notReplanted || null,
      stillGrowing: cells.length - ripe.length,
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
  }
}
