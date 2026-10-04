// Why JavaScript: Mineflayer boundary; patches bot.blockAt for the physics engine.
// Server-true collision for bamboo and pointed dripstone: minecraft-data has one fixed box, vanilla shifts it per position.
import { OFFSET_MAX, blockOffset } from './offsets.mjs'

const WRAPPED = Symbol('offsetShapesWrapped')

const recentre = (box, cx, cz) => {
  const sx = cx - (box[0] + box[3]) / 2
  const sz = cz - (box[2] + box[5]) / 2
  return [box[0] + sx, box[1], box[2] + sz, box[3] + sx, box[4], box[5] + sz]
}

// New shapes array with every box centred on the cell plus the vanilla offset; other blocks keep their shapes
export function serverShapes (block) {
  if (!(block.name in OFFSET_MAX) || !block.shapes) return block.shapes
  const { x, z } = block.position
  const { dx, dz } = blockOffset(x, z, OFFSET_MAX[block.name])
  return block.shapes.map(box => recentre(box, 0.5 + dx, 0.5 + dz))
}

// Assigns a new array: the block's own shapes array is shared with the registry
export function withServerShapes (block) {
  if (!block) return block
  if (block.name in OFFSET_MAX) block.shapes = serverShapes(block)
  return block
}

// mineflayer's physics reads blocks through bot.blockAt for every nearby cell every tick: one type-id comparison, no allocation; idempotent
export function wrapBlockAt (bot) {
  if (!bot.blockAt || bot.blockAt[WRAPPED] || !bot.registry?.blocksByName) return // fake bots have no registry
  const original = bot.blockAt
  const bambooType = bot.registry.blocksByName.bamboo.id
  const dripstoneType = bot.registry.blocksByName.pointed_dripstone.id
  bot.blockAt = (pos, extraInfos) => {
    const block = original.call(bot, pos, extraInfos)
    if (block && (block.type === bambooType || block.type === dripstoneType)) block.shapes = serverShapes(block)
    return block
  }
  bot.blockAt[WRAPPED] = true
}
