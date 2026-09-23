// One apiary census: honey, smoke, entrances, visible bees and forage. Hive occupants are server-side and are not
// sent reliably to this client, so this deliberately says visible= rather than pretending it counted the colony.
import { apiarySnapshot, hiveLine } from './shared/common.mjs'
import { apiaryCensus } from './shared/hive.mjs'

export default {
  doc: 'apiary.inspect place=|x= y= z= [range=16]: inspect nearby hives, smoke, open or raised fires, entrances, flowers and visible bees',
  stops: 'the census is complete, or the apiary cannot be reached',
  args: { place: 'string', x: 'number', y: 'number', z: 'number', range: 'number' },

  async run (api, a) {
    const seen = await apiarySnapshot(api, a, 'apiary.inspect')
    // the counts and the details are two readings of ONE list, and every count that is not zero names its hives, so
    // that a reader can check the top line against the line below it instead of trusting it (Mariel, item 18)
    return {
      ...apiaryCensus(seen.hives, seen.fires),
      beesVisible: seen.bees.length,
      grownVisible: seen.bees.filter(b => b.grown).length,
      flowers: seen.flowers,
      details: seen.hives.length ? seen.hives.map(hiveLine).join(' ') : 'no beehive or bee_nest within range'
    }
  }
}
