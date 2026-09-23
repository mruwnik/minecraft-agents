// An open fire burns the bees that fly through it. This puts a carpet on every lit campfire near the apiary that has
// none, so the standard column stands: fire, carpet on it, air, hive.
import { apiaryFires, carpetCarried } from './shared/common.mjs'

export default {
  doc: 'apiary.guard place=|x= y= z= [range=16]: put a carpet on every open lit campfire near an apiary (bees burn in open fire)',
  stops: 'every lit fire in range has a carpet on it, or one is open and no carpet is carried',
  args: { place: 'string', x: 'number', y: 'number', z: 'number', range: 'number' },

  async run (api, a) {
    const fires = await apiaryFires(api, a, 'apiary.guard')
    const open = fires.filter(f => f.open)
    const lit = fires.filter(f => f.lit)
    if (!open.length) return { fires: lit.length, open: 0, carpeted: 0, left: 0 }
    const carpet = carpetCarried(api.inv())
    if (!carpet) throw new Error(`${open.length} open fire${open.length > 1 ? 's' : ''} at ${open.map(f => `${f.x},${f.y},${f.z}`).join(' ')} and no carpet carried: craft one (2 wool make 3 carpets) and come back`)
    let carpeted = 0
    for (const fire of open) {
      await api.act('place', { x: fire.x, y: fire.y + 1, z: fire.z, item: carpet })
      if (api.block(fire.x, fire.y + 1, fire.z)?.name === carpet) carpeted++
      api.report({ carpeted })
      await api.checkpoint()
    }
    return { fires: lit.length, open: open.length, carpeted, left: open.length - carpeted, with: carpet }
  }
}
