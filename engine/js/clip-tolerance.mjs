// Why JavaScript: Mineflayer boundary; replaces the collision clip of prismarine-physics, the body's client physics.
// The stock clip compares box faces exactly, so a rounding step decides it: a box that far into a block face is no longer
// stopped and walks into the block (the server throws every such move back), and one resting that far into a floor is
// stopped by the next floor block. Boxes within clipTolerance of each other count as touching, as in vanilla.
import { createRequire } from 'node:module'
import constants from '../src/engine/constants.json' with { type: 'json' }

const AABB = createRequire(import.meta.url)('prismarine-physics/lib/aabb')
const T = constants.clipTolerance

// how far a box may move by `offset` along an axis towards one whose near faces there are lo and hi
const clip = (offset, lo, hi, otherLo, otherHi) => {
  if (offset > 0 && otherHi <= lo + T) return Math.min(lo - otherHi, offset)
  if (offset < 0 && otherLo >= hi - T) return Math.max(hi - otherLo, offset)
  return offset
}
const overlap = (lo, hi, otherLo, otherHi) => otherHi - lo > T && hi - otherLo > T

AABB.prototype.computeOffsetX = function (other, offset) {
  return overlap(this.minY, this.maxY, other.minY, other.maxY) && overlap(this.minZ, this.maxZ, other.minZ, other.maxZ)
    ? clip(offset, this.minX, this.maxX, other.minX, other.maxX)
    : offset
}
AABB.prototype.computeOffsetY = function (other, offset) {
  return overlap(this.minX, this.maxX, other.minX, other.maxX) && overlap(this.minZ, this.maxZ, other.minZ, other.maxZ)
    ? clip(offset, this.minY, this.maxY, other.minY, other.maxY)
    : offset
}
AABB.prototype.computeOffsetZ = function (other, offset) {
  return overlap(this.minX, this.maxX, other.minX, other.maxX) && overlap(this.minY, this.maxY, other.minY, other.maxY)
    ? clip(offset, this.minZ, this.maxZ, other.minZ, other.maxZ)
    : offset
}
