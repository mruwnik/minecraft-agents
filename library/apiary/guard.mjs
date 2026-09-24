// An open fire burns the bees that land in it, and a fire with a side in the open burns the ones that fly into it. This
// makes the standard column stand at every lit campfire near the apiary: the fire one block underground, a carpet on it,
// air, hive. A raised fire is dug out and put back one block down when a campfire is carried (mining one gives charcoal,
// not the fire), then every lit fire with nothing on it gets a carpet. A fire with a hive or a full block straight on it
// is already covered.
import { apiaryFires, carpetCarried } from './shared/common.mjs'
import { campfireCarried, CAMPFIRE_RECIPE } from './shared/hive.mjs'
import { placeRefusal } from '../../src/lib.mjs'

const at = f => `${f.x},${f.y},${f.z}`
const isCarpet = name => /_carpet$/.test(String(name))

// dig the fire and the ground under it, put the fire back down there; the carpet that sat on it (if any) is taken first so
// it is not lost when the fire goes
async function sink (api, fire, campfire) {
  const { x, y, z } = fire
  if (isCarpet(api.block(x, y + 1, z)?.name)) await api.act('dig', { x, y: y + 1, z })
  await api.act('dig', { x, y, z })
  await api.act('dig', { x, y: y - 1, z })
  await api.act('place', { x, y: y - 1, z, item: campfire })
  return api.block(x, y - 1, z)?.name === campfire
}

export default {
  doc: 'apiary.guard place=|x= y= z= [range=16]: move every raised lit campfire near an apiary one block underground (needs a spare campfire) and put a carpet on every one that has nothing on it (bees burn in open fire); a hive straight on its fire needs no carpet',
  stops: 'every lit fire in range is underground with something on it, or one is open and no carpet is carried',
  args: { place: 'string', x: 'number', y: 'number', z: 'number', range: 'number' },

  async run (api, a) {
    // whose ground this is, first: an agent told "you carry no wheat" fixes that and comes back to find the pen was
    // never theirs to walk into. The decisive answer goes first (#144)
    const refusal = placeRefusal(api.places(), a.place, api.me?.())
    if (refusal) throw new Error(refusal)
    const first = await apiaryFires(api, a, 'apiary.guard')
    const raised = first.filter(f => f.lit && !f.sunk)
    const campfire = campfireCarried(api.inv())
    let sunk = 0
    for (const fire of campfire ? raised : []) {
      const moved = await sink(api, fire, campfire).catch(e => { api.note(`could not sink the fire at ${at(fire)}: ${e.message}`); return false })
      if (moved) sunk++
      api.report({ sunk })
      await api.checkpoint()
    }
    // read the world again rather than trust the digging: the moved fires now need their carpet one block lower
    const fires = sunk ? await apiaryFires(api, a, 'apiary.guard') : first
    const lit = fires.filter(f => f.lit)
    const open = fires.filter(f => f.open)
    const craft = raised.length > sunk ? { craft: `${CAMPFIRE_RECIPE} to sink ${raised.length - sunk} raised fire${raised.length - sunk > 1 ? 's' : ''}` } : {}
    if (!open.length) return { fires: lit.length, raised: raised.length, sunk, open: 0, carpeted: 0, left: 0, ...craft }
    const carpet = carpetCarried(api.inv())
    if (!carpet) throw new Error(`${open.length} open fire${open.length > 1 ? 's' : ''} at ${open.map(at).join(' ')} and no carpet carried: craft one (2 wool make 3 carpets) and come back`)
    let carpeted = 0
    for (const fire of open) {
      await api.act('place', { x: fire.x, y: fire.y + 1, z: fire.z, item: carpet })
      if (api.block(fire.x, fire.y + 1, fire.z)?.name === carpet) carpeted++
      api.report({ carpeted })
      await api.checkpoint()
    }
    return { fires: lit.length, raised: raised.length, sunk, open: open.length, carpeted, left: open.length - carpeted, with: carpet, ...craft }
  }
}
