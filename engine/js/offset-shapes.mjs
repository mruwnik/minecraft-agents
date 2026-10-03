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

// mineflayer's physics reads blocks through bot.blockAt; idempotent
export function wrapBlockAt (bot) {
  if (bot.blockAt[WRAPPED]) return
  const original = bot.blockAt
  bot.blockAt = (...args) => withServerShapes(original.apply(bot, args))
  bot.blockAt[WRAPPED] = true
}
