// One apiary census: honey, smoke, entrances, visible bees and forage. Hive occupants are server-side and are not
// sent reliably to this client, so this deliberately says visible= rather than pretending it counted the colony.
import { apiarySnapshot, hiveLine } from './shared/common.mjs'

export default {
  doc: 'apiary.inspect place=|x= y= z= [range=16]: inspect nearby hives, smoke, open fires, entrances, flowers and visible bees',
  stops: 'the census is complete, or the apiary cannot be reached',
  args: { place: 'string', x: 'number', y: 'number', z: 'number', range: 'number' },

  async run (api, a) {
    const seen = await apiarySnapshot(api, a, 'apiary.inspect')
    const ripe = seen.hives.filter(h => h.ripe).length
    const unsafe = seen.hives.filter(h => !h.smoked).length
    const blocked = seen.hives.filter(h => !h.entranceClear).length
    const openFires = seen.fires.filter(f => f.open).length
    return {
      hives: seen.hives.length,
      ripe,
      unsafe,
      blocked,
      openFires,
      beesVisible: seen.bees.length,
      grownVisible: seen.bees.filter(b => b.grown).length,
      flowers: seen.flowers,
      details: seen.hives.length ? seen.hives.map(hiveLine).join(' ') : 'no beehive or bee_nest within range'
    }
  }
}
