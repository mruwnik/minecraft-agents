// Bees take flowers, but are not a flock: no gate, pen floor or ground escort is involved.
import { BEE_FLOWERS, creatureFood, placeRefusal } from '../../src/lib.mjs'
import { apiarySnapshot } from './shared/common.mjs'

export default {
  doc: 'apiary.breed place=|x= y= z= [count=2] [range=16]: feed grown bees flowers at an apiary',
  stops: 'the requested grown bees ate, fewer are visible, or no flower is carried',
  args: { place: 'string', x: 'number', y: 'number', z: 'number', count: 'number', range: 'number' },

  async run (api, a) {
    // whose ground this is, first: an agent told "you carry no wheat" fixes that and comes back to find the pen was
    // never theirs to walk into. The decisive answer goes first (#144)
    const refusal = placeRefusal(api.places(), a.place, api.me?.())
    if (refusal) throw new Error(refusal)
    const flower = creatureFood('bee', Object.keys(api.inv()))
    if (!flower) throw new Error(`a bee eats a flower: carry one of ${BEE_FLOWERS.join(', ')}`)
    const seen = await apiarySnapshot(api, a, 'apiary.breed')
    if (api.clock().night || api.clock().raining) {
      throw new Error(`bees stay in their hives ${api.clock().night ? 'at night' : 'in rain'}: breed them in dry daylight`)
    }
    const grown = seen.bees.filter(b => b.grown).sort((p, q) => p.dist - q.dist)
    const want = a.count ?? 2
    if (grown.length < want) throw new Error(`breeding needs ${want} grown bees in sight: only ${grown.length} visible (hive occupants cannot be counted until they come out)`)
    let fed = 0
    for (const bee of grown.slice(0, want)) {
      const done = await api.act('feed', { mob: 'bee', id: bee.id })
      fed += done.fed ?? 0
      api.report({ fed })
      await api.checkpoint()
    }
    return { fed, with: flower, beesVisible: seen.bees.length }
  }
}
