// Why JavaScript: replaces Mineflayer's block_change handlers at the library boundary; raw packet fields.
// Mineflayer's handler builds four full Blocks per changed block (each read forces the column's biome and light
// sections), though it only needs the old and new type. This one writes the new state and emits blockUpdate with
// two small {type, stateId, position} records: every general listener (view, raw world, pathfinder) reads only those.
// Full Blocks are built only when someone waits on that cell's own `blockUpdate:(x, y, z)` event (dig, place).
// It does not drop a painting when the block under it changes; the server destroys the painting entity itself.
import { createRequire } from 'node:module'
import prismarineBlock from 'prismarine-block'
import vec3 from 'vec3'

const { Vec3 } = vec3
const require = createRequire(import.meta.url)

const posInChunk = pos => new Vec3(Math.floor(pos.x) & 15, Math.floor(pos.y), Math.floor(pos.z) & 15)

// `bot` is a Mineflayer bot whose plugins are loaded; bot.world exists from the login packet on.
export function installFastBlockUpdates (bot) {
  const registry = bot.registry ?? require('prismarine-registry')(bot.version)
  const Block = prismarineBlock(registry)
  const client = bot._client
  const singleLong = bot.supportFeature('usesMultiblockSingleLong')
  const chunk3d = bot.supportFeature('usesMultiblock3DChunkCoords')

  const stateType = stateId => registry.blocksByStateId[stateId]?.id ?? 0
  const light = (stateId, position) => ({ type: stateType(stateId), stateId, position })

  // count of `blockUpdate:(x, y, z)` listeners, kept by the bot's listener events (Mineflayer forwards them to the world) so the common update builds no key string
  let keyed = 0
  const isKeyed = event => typeof event === 'string' && event.startsWith('blockUpdate:')
  bot.on('newListener', event => { if (isKeyed(event)) keyed++ })
  bot.on('removeListener', event => { if (isKeyed(event)) keyed-- })

  const withPosition = (block, position) => { block.position = position; return block }

  const update = (point, stateId) => {
    const world = bot.world
    const column = world.getColumnAt(point)
    if (!column) return // the server may send block updates before the column
    const at = posInChunk(point)
    const old = column.getBlockStateId(at)
    column.setBlockStateId(at, stateId)
    world.async.saveAt(point)
    const position = point // packet coordinates are whole numbers
    world.emit('blockUpdate', light(old, position), light(stateId, position))
    if (keyed > 0) world.emit(`blockUpdate:${position}`, withPosition(Block.fromStateId(old), position), withPosition(Block.fromStateId(stateId), position))
  }

  client.removeAllListeners('block_change')
  client.removeAllListeners('multi_block_change')
  client.on('block_change', packet => update(new Vec3(packet.location.x, packet.location.y, packet.location.z), packet.type))
  client.on('multi_block_change', packet => {
    const origin = (chunk3d ? new Vec3(packet.chunkCoordinates.x, packet.chunkCoordinates.y, packet.chunkCoordinates.z) : new Vec3(packet.chunkX, 0, packet.chunkZ)).scale(16)
    for (const record of packet.records) {
      if (singleLong) update(origin.offset((record >> 8) & 0x0f, record & 0x0f, (record >> 4) & 0x0f), record >> 12)
      else update(origin.offset((record.horizontalPos >> 4) & 0x0f, record.y, record.horizontalPos & 0x0f), record.blockId)
    }
  })
}

// Mineflayer injects its plugins on 'inject_allowed' (after createBot returns), so only a listener registered
// after createBot runs after blocks.js has added its own block_change handlers, which this must remove.
export function installFastBlockUpdatesOnInject (bot) {
  bot.once('inject_allowed', () => installFastBlockUpdates(bot))
}
