// Why JavaScript: Mineflayer boundary; the cost policy has to run inside mineflayer-pathfinder's getNeighbors.
import vec3 from 'vec3'
import pf from 'mineflayer-pathfinder'

const { Vec3 } = vec3
const { Movements } = pf

// extra cost, not a ban: crossing stays possible when it is the only way, and a detour of up to about this many blocks is preferred
const BODY_COSTS = Object.freeze({ powder_snow: 30, cobweb: 40, sweet_berry_bush: 20, wither_rose: 20 })
const FLOOR_COSTS = Object.freeze({ magma_block: 20, campfire: 40, soul_campfire: 40 })
const PORTALS = ['nether_portal', 'end_portal', 'end_gateway'] // walking in teleports the body away; never entered
const JUMP_HAZARDS = new Set(['lava', 'fire', 'soul_fire'])

const nameAt = (bot, x, y, z) => bot.blockAt(new Vec3(x, y, z), false)?.name

const hazardCost = (bot, move) => (
  (BODY_COSTS[nameAt(bot, move.x, move.y, move.z)] ?? 0) +
  (BODY_COSTS[nameAt(bot, move.x, move.y + 1, move.z)] ?? 0) +
  (FLOOR_COSTS[nameAt(bot, move.x, move.y - 1, move.z)] ?? 0)
)

// parkour moves are straight, so the columns strictly between node and landing lie along one axis
const jumpsOverHazard = (bot, node, move) => {
  const dx = Math.sign(move.x - node.x)
  const dz = Math.sign(move.z - node.z)
  const steps = Math.max(Math.abs(move.x - node.x), Math.abs(move.z - node.z))
  return range(1, steps - 1).some(d => [1, 2, 3].some(down => JUMP_HAZARDS.has(nameAt(bot, node.x + dx * d, node.y - down, node.z + dz * d))))
}

// farmland turns to dirt when landed on from over half a block up: parkour and drops
const tramplesFarmland = (bot, node, move) => (
  (move.parkour || move.y < node.y) && nameAt(bot, move.x, move.y - 1, move.z) === 'farmland'
)

const range = (from, to) => Array.from({ length: Math.max(0, to - from + 1) }, (_, i) => from + i)

export class SafeMovements extends Movements {
  constructor (bot) {
    super(bot)
    this.canDig = false
    this.allow1by1towers = false
    this.scafoldingBlocks = [] // sic: the pathfinder's own spelling
    this.infiniteLiquidDropdownDistance = false
    this.blocksToAvoid.delete(bot.registry.blocksByName.cobweb.id)
    this.blocksToAvoid.add(bot.registry.blocksByName.soul_fire.id)
    for (const name of PORTALS) this.blocksToAvoid.add(bot.registry.blocksByName[name].id)
  }

  getNeighbors (node) {
    return super.getNeighbors(node)
      .filter(move => !move.parkour || !jumpsOverHazard(this.bot, node, move))
      .filter(move => !tramplesFarmland(this.bot, node, move))
      .map(move => {
        move.cost += hazardCost(this.bot, move)
        return move
      })
  }
}
