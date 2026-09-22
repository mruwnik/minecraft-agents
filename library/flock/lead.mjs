// Walk animals to a spot with their food in my hand. The walk itself is `escort`; what this decides is where the spot is,
// whether it is a spot at all, whether a gate stands open that would let the pen empty while I am away, and who is inside at the end.
import { BREEDING_FOOD, breedingFood, leadTargetError, gateLeak, placeTarget } from '../../src/lib.mjs'

const given = obj => Object.fromEntries(Object.entries(obj).filter(([, v]) => v !== undefined))

export default {
  doc: 'flock.lead mob= place=|x= y= z= [count=2] [within=32] [penned=] [range=]: walk animals to a spot, into a pen if the goal is inside one',
  stops: 'the animals are in, they will not follow, or there is no way there on foot',
  args: { mob: 'string!', place: 'string', x: 'number', y: 'number', z: 'number', count: 'number', within: 'number', penned: 'boolean', range: 'number' },

  async run (api, a) {
    if (!BREEDING_FOOD[a.mob]) throw new Error(`cannot lead ${a.mob}: one of ${Object.keys(BREEDING_FOOD).join(', ')}`)
    const food = breedingFood(a.mob, Object.keys(api.inv()))
    if (!food) throw new Error(`a ${a.mob} follows ${BREEDING_FOOD[a.mob].join(' or ')}: you carry none`)

    const aim = placeTarget(api.places(), a, 'flock.lead')
    if (aim.error) throw new Error(aim.error)
    const at = aim.at
    // a marker set on the fence line ends the walk OUTSIDE the pen
    const targetError = leadTargetError(at, api.block(at.x, at.y, at.z))
    if (targetError) throw new Error(targetError)

    // a gate left open makes the pen look like open country: shut it first, or I would lead off the animals that are in it
    // a goal in open country is no pen and has nothing to count; anything else that goes wrong there is worth saying out loud
    const check = async () => {
      const r = await api.act('pen.check', at).then(ok => ok, e => ({ error: e.message }))
      if (r.error && !/not a spot to stand on/.test(r.error)) api.note(r.error)
      return r.error ? null : r
    }
    const ajar = await check()
    const gate = ajar?.pen === 'LEAKS' && ajar.via
      ? gateLeak(ajar.via, (x, y, z) => { const b = api.block(x, y, z); return b && { name: b.name, open: b.properties?.open } })
      : null
    if (gate) await api.act('toggle', { x: gate[0], y: gate[1], z: gate[2], open: false }).catch(() => {})
    const shutFirst = gate ? `the gate at ${gate.join(',')} stood open: shut it before fetching them` : undefined
    if (gate) await check()

    // escort counts the pen from the cells it walked, before the gate we came through is shut behind us
    const walked = await api.act('escort', { mob: a.mob, ...at, ...given({ count: a.count, within: a.within, penned: a.penned, range: a.range }) })
    api.report({ ...walked, shutFirst })
    return { ...walked, shutFirst }
  }
}
