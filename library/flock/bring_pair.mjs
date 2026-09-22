// Put a breeding pair in a pen: count who is in it already, fetch only what is still needed, shut the gate behind them
// and say who is in at the end. Leading is `flock.lead`'s job; what this decides is how many to fetch and whether it worked.
import { BREEDING_FOOD, breedingFood, insideCount, pairPlan, placeTarget } from '../../src/lib.mjs'
import { penHolds } from '../../src/pens.mjs'

const given = obj => Object.fromEntries(Object.entries(obj).filter(([, v]) => v !== undefined))

export default {
  doc: 'flock.bring_pair mob= place=|x= y= z= [count=2] [within=32] [penned=]: fetch enough grown animals to breed in a pen, shut the gate and count them in',
  stops: 'the pen holds the pair, there are not enough within reach, or they will not follow',
  args: { mob: 'string!', place: 'string', x: 'number', y: 'number', z: 'number', count: 'number', within: 'number', penned: 'boolean', range: 'number', until: 'number' },

  async run (api, a) {
    // empty-handed is worth knowing before the walk, not after it: nothing follows a bare hand
    if (!BREEDING_FOOD[a.mob]) throw new Error(`cannot lead ${a.mob}: one of ${Object.keys(BREEDING_FOOD).join(', ')}`)
    if (!breedingFood(a.mob, Object.keys(api.inv()))) throw new Error(`a ${a.mob} follows ${BREEDING_FOOD[a.mob].join(' or ')}: you carry none`)
    const aim = placeTarget(api.places(), a, 'flock.bring_pair')
    if (aim.error) throw new Error(aim.error)
    const at = aim.at
    const before = (await penHolds(api, at)).held
    const inside = insideCount(before.inside, a.mob)
    // counted against the TARGET pen, not the ground I am standing on: from out here its sheep would look fetchable
    const seen = await api.act('animals', { mob: a.mob, within: a.within ?? 32, ...at })
    // a calf will not breed, and one already in the pen is not one to fetch
    const grown = seen.found.filter(e => e.grown && !e.inMyPen).length
    const plan = pairPlan({ mob: a.mob, inside, grown, want: a.count ?? 2 })
    if (plan.refuse) throw new Error(plan.refuse)
    if (!plan.fetch) return { ...before, already: plan.note }

    await api.checkpoint()
    const led = await api.act('flock.lead', { mob: a.mob, count: plan.fetch, ...at, ...given({ within: a.within, penned: a.penned, range: a.range }) })
    await api.checkpoint()
    const held = (await penHolds(api, at)).held
    const now = insideCount(held.inside, a.mob)
    const summary = { ...held, fetched: now - inside, stuck: led.stuck, short: now < (a.count ?? 2) ? `${now} grown ${a.mob} in the pen, not ${a.count ?? 2}: ${led.why ?? 'lead them again, or look further afield'}` : undefined }
    api.report(summary)
    return summary
  }
}
