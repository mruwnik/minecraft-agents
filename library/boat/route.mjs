import { villagerBoatRoute } from '../../src/lib.mjs'

export default {
  doc: 'boat.route fromX= fromY= fromZ= x= y= z= [margin=8]: read-only check of a planned adult-villager boat launch and landing; reports the full 1.375-wide nonascending hull route or its first blocker',
  stops: 'a boat route is verified through loaded terrain without moving or placing anything, or the blocking step is reported',
  args: { fromX: 'number!', fromY: 'number!', fromZ: 'number!', x: 'number!', y: 'number!', z: 'number!', margin: 'number' },

  async run (api, a) {
    if (![a.fromX, a.fromY, a.fromZ, a.x, a.y, a.z].every(Number.isInteger)) throw new Error('route launch and landing cells must be integers')
    const margin = a.margin ?? 8
    if (!Number.isInteger(margin) || margin < 2 || margin > 16) throw new Error('route margin= must be 2..16')
    const launch = api.block(a.fromX, a.fromY, a.fromZ)
    if (!launch) throw new Error(`boat launch terrain is unloaded at ${a.fromX},${a.fromY},${a.fromZ}`)
    const water = launch.name === 'water'
    const air = ['air', 'cave_air', 'void_air'].includes(launch.name)
    const below = api.block(a.fromX, a.fromY - 1, a.fromZ)
    if (!water && !(air && below?.solid)) throw new Error(`boat launch at ${a.fromX},${a.fromY},${a.fromZ} needs water or air over solid ground (found ${launch.name})`)
    const from = { x: a.fromX + 0.5, y: a.fromY + (water ? 0.5 : 0), z: a.fromZ + 0.5 }
    const to = { x: a.x + 0.5, y: a.y, z: a.z + 0.5 }
    const route = villagerBoatRoute({ from, to, margin, blockAt: (x, y, z) => api.block(x, y, z) })
    if (route.error) throw new Error(`boat route blocked at ${route.at.x},${route.at.y},${route.at.z}: ${route.error}`)
    api.report({ route: 'clear', launch: `${a.fromX},${a.fromY},${a.fromZ}`, landing: `${a.x},${a.y},${a.z}`, steps: route.points.length })
    return { clear: true, launch: from, landing: to, route: route.points }
  }
}
