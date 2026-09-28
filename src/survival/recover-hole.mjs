import { isAir } from '../lib/world.mjs'

const key = p => `${p.x},${p.y},${p.z}`
const shaft = (at, dy) => ({ x: at.x, y: at.y + dy, z: at.z })
const cell = p => ({ x: Math.floor(p.x), y: Math.floor(p.y), z: Math.floor(p.z) })

// Events are written by this body into its own HOME. A later death or hole
// invalidates the old location; a copied coordinate supplied by a caller is
// never accepted as provenance for digging outside a managed plot.
export function latestOwnClosedDigHole (jsonl, owner) {
  let last = null
  for (const line of String(jsonl).split('\n')) {
    if (!line.trim()) continue
    let event
    try { event = JSON.parse(line) } catch { continue }
    if (event.type === 'died' || event.type === 'respawned') last = null
    if (event.type !== 'holed_up') continue
    last = null
    if (event.way !== 'dig' || event.open !== false || typeof event.at !== 'string') continue
    const match = event.at.match(/^(-?\d+),(-?\d+),(-?\d+)$/)
    if (!match) continue
    const [x, y, z] = match.slice(1).map(Number)
    if (![x, y, z].every(Number.isSafeInteger)) continue
    last = { owner, at: { x, y, z }, seq: event.seq, t: event.t }
  }
  return last
}

// The original hole-up event proves only the three dug shaft cells. It does
// not say which neighboring dirt blocks were placed, so recovery leaves the
// ring alone. The cap is opened from inside, and pillar_up fills the same
// three shaft cells with dirt while lifting the body to the surface.
export async function recoverOwnDigHole (api, record) {
  if (!record || record.owner !== api.me?.()) return { attempted: false }
  const at = record.at
  if (!at || !['x', 'y', 'z'].every(axis => Number.isSafeInteger(at[axis]))) return { attempted: false }
  const feet = cell(api.pos())
  if (feet.x !== at.x || feet.z !== at.z || feet.y < at.y || feet.y > at.y + 3) return { attempted: false }
  const fail = reason => ({ attempted: true, recovered: false, attention: [`own survival shaft ${key(at)}: ${reason}`] })
  if (api.clock?.().night) return fail('wait for daylight before opening the cap')
  const bottom = api.block(at.x, at.y - 1, at.z)
  if (!bottom?.solid || /water|lava/.test(bottom.name)) return fail('bottom support changed; shaft retained')
  for (let dy = 0; dy < Math.min(3, feet.y - at.y); dy++) {
    if (api.block(at.x, at.y + dy, at.z)?.name !== 'dirt') return fail(`previously filled shaft cell ${key(shaft(at, dy))} changed`)
  }
  if (feet.y === at.y + 3) {
    if (![0, 1, 2].every(dy => api.block(at.x, at.y + dy, at.z)?.name === 'dirt')) return fail('surface reached but shaft is not fully restored')
    return { attempted: true, recovered: true, at }
  }
  const cap = shaft(at, 2)
  const capName = api.block(cap.x, cap.y, cap.z)?.name
  // Grass can grow over the exact dirt cap while the body waits underground.
  // A grass_block still represents the authorized cap cell and drops dirt.
  const capIsDirt = capName === 'dirt' || capName === 'grass_block'
  if (!capIsDirt && !isAir(capName)) return fail(`cap changed to ${capName ?? 'unloaded'}; leave it intact`)
  // A closed event plus the body's position inside the exact shaft proves the
  // cap. Require enough carried dirt before opening it; the dug cap supplies
  // one more block after pickup.
  const steps = at.y + 3 - feet.y
  const held = api.inv().dirt ?? 0
  if (held + (capIsDirt ? 1 : 0) < steps) return fail(`need ${steps} dirt to fill and leave this shaft; carry ${held}${capIsDirt ? ' plus its cap' : ''}`)
  for (let y = feet.y; y <= at.y + 1; y++) {
    if (!isAir(api.block(at.x, y, at.z)?.name)) return fail(`feet/head cell ${at.x},${y},${at.z} changed`)
  }
  if (capIsDirt) {
    // This is the one exact cap cell the user authorized us to open. The body
    // can be weak from waiting underground, so use the dig primitive's force
    // override only for this verified recovery cell; normal forestry digs
    // keep the usual health gate.
    await api.act('dig', { ...cap, batch: true, force: true })
    if (!isAir(api.block(cap.x, cap.y, cap.z)?.name)) return fail('cap removal was not verified')
    if ((api.inv().dirt ?? 0) < steps) await api.act('collect', { range: 2 })
    if ((api.inv().dirt ?? 0) < steps) return fail('cap dirt was not picked up; leave the opened shaft for retry')
  }
  for (let y = feet.y; y <= at.y + 2; y++) {
    const now = cell(api.pos())
    if (now.x !== at.x || now.z !== at.z || now.y !== y || !isAir(api.block(at.x, y, at.z)?.name)) return fail(`ascent position changed before ${at.x},${y},${at.z}`)
    if ((api.inv().dirt ?? 0) < 1) return fail('ran out of dirt during ascent; shaft retained for retry')
    await api.act('pillar_up', { steps: 1, item: 'dirt' })
    const after = cell(api.pos())
    if (api.block(at.x, y, at.z)?.name !== 'dirt' || after.x !== at.x || after.z !== at.z || after.y !== y + 1) return fail(`dirt step ${at.x},${y},${at.z} did not verify; stop in place`)
  }
  if (![0, 1, 2].every(dy => api.block(at.x, at.y + dy, at.z)?.name === 'dirt')) return fail('shaft refill did not verify')
  return { attempted: true, recovered: true, at }
}
