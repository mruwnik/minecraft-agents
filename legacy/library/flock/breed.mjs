// Feed two grown animals their breeding food so they pair off. Standing in a pen, only the ones in here with me count:
// food in hand outside the gate walks the whole flock out at my heels (Vivenna lost a cow that way two mornings running).
import { BREEDING_FOOD, breedingFood } from '../../src/lib.mjs'

const WITHIN = 24

export default {
  doc: 'flock.breed mob= [count=2] [within=24]: feed two grown animals their food so they pair off',
  stops: 'both have eaten, fewer than two grown ones are about, or the usual hand-backs',
  args: { mob: 'string!', count: 'number', within: 'number' },

  async run (api, a) {
    if (!BREEDING_FOOD[a.mob]) throw new Error(`cannot breed ${a.mob}: one of ${Object.keys(BREEDING_FOOD).join(', ')}`)
    const food = breedingFood(a.mob, Object.keys(api.inv()))
    if (!food) throw new Error(`a ${a.mob} eats ${BREEDING_FOOD[a.mob].join(' or ')}: you carry none`)

    const within = a.within ?? WITHIN
    const pen = api.pen()
    const { found = [] } = await api.act('animals', { mob: a.mob, within })
    const herd = found.filter(e => e.grown && (!pen?.enclosed || e.inMyPen))
    if (herd.length < 2) throw new Error(`breeding takes two: ${herd.length} grown ${a.mob} ${pen?.enclosed ? 'in this pen with you' : `within ${within} blocks`}`)

    let fed = 0
    for (const animal of herd.slice(0, a.count ?? 2)) {
      // the game takes the food only from a grown one that is ready: `fed` is how many really ate
      const eaten = await api.act('feed', { mob: a.mob, id: animal.id })
      fed += eaten.fed ?? 0
      api.report({ fed })
      await api.checkpoint()
    }
    return { fed, with: food, herd: herd.length }
  }
}
