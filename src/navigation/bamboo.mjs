// Vanilla's Mth.getSeed(x, 0, z) in exact 64-bit arithmetic (Block.box offset, up to 0.25 each axis). x * 3129871
// wraps as a 32-bit int first (Math.imul), matching the server's own hitbox so the pathfinder stops disagreeing with it.
const seed = (x, z) => {
  let l = BigInt.asIntN(64, BigInt(Math.imul(x, 3129871)) ^ (BigInt(z) * 116129781n))
  l = BigInt.asIntN(64, l * l * 42317861n + l * 11n)
  return l >> 16n
}

const clampedAxis = l => Math.max(-0.25, Math.min(0.25, (Number(l & 15n) / 15 - 0.5) * 0.5))

// Server-true bamboo collision box, block-local: Block.box(6.5, 0, 6.5, 9.5, 16, 9.5) shifted by the per-block seed.
export const stalkShape = (x, z) => {
  const l = seed(x, z)
  const dx = clampedAxis(l)
  const dz = clampedAxis(l >> 8n)
  return [0.40625 + dx, 0, 0.40625 + dz, 0.59375 + dx, 1, 0.59375 + dz]
}
