// Why JavaScript: shared with the per-tick physics wrapper (offset-shapes.mjs, Mineflayer boundary) and the path code; vanilla per-position offset hash.
// Vanilla's per-position horizontal block offset (bamboo, pointed dripstone), shared by the pathfinder and client physics.
// The server shifts these blocks' collision by a hash of the block position, so the box moves cell to cell.

// bamboo's and pointed dripstone's max horizontal offset in blocks; only blocks with collision matter here
export const OFFSET_MAX = { bamboo: 0.25, pointed_dripstone: 0.125 }

// Mth.getSeed(x, 0, z) in exact 64-bit arithmetic. x * 3129871 wraps as a 32-bit int first (Math.imul) as in Java;
// any mismatch puts a box somewhere the server does not have it.
const seed = (x, z) => {
  let l = BigInt.asIntN(64, BigInt(Math.imul(x, 3129871)) ^ (BigInt(z) * 116129781n))
  l = BigInt.asIntN(64, l * l * 42317861n + l * 11n)
  return l >> 16n
}

const axis = (l, maxH) => Math.max(-maxH, Math.min(maxH, (Number(l & 15n) / 15 - 0.5) * 0.5))

// { dx, dz } in blocks for the block at integer (x, z)
export function blockOffset (x, z, maxH) {
  const l = seed(x, z)
  return { dx: axis(l, maxH), dz: axis(l >> 8n, maxH) }
}

// Server-true bamboo collision box, block-local [x0, y0, z0, x1, y1, z1]: Block.box(6.5, 0, 6.5, 9.5, 16, 9.5) shifted
export function bambooBox (x, z) {
  const { dx, dz } = blockOffset(x, z, OFFSET_MAX.bamboo)
  return [0.40625 + dx, 0, 0.40625 + dz, 0.59375 + dx, 1, 0.59375 + dz]
}
