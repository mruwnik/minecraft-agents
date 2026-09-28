const point = p => ({ x: p.x, y: p.y, z: p.z })
const distance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
class RefreshHorseTarget extends Error {}
class HorseWithinReach extends Error {}

// Retarget only between settled, reversible surface walks. Never install an
// unrestricted dynamic follow goal or fall back to ordinary goNear.
export async function approachHorseSurface ({ bot, entity, check, surfaceWalk, now = Date.now, report = () => {} }) {
  if (!surfaceWalk?.walk) throw new Error('checked horse surface approach is unavailable')
  const origin = point(bot.entity.position), health = bot.health, until = now() + 8000
  const guard = () => {
    check()
    if (bot.vehicle) throw new Error('horse approach requires an unmounted traveler')
    if (bot.entities[entity.id] !== entity || entity.isValid === false) throw new Error('horse identity changed during approach')
    if (bot.health < 16 || bot.health < health) throw new Error('horse approach stopped for damage or low health')
    if (Object.values(bot.entities).some(e => e.isValid !== false && e.position && (e.type === 'hostile' || e.kind === 'Hostile mobs') &&
      (distance(e.position, bot.entity.position) < 12 || distance(e.position, entity.position) < 12))) throw new Error('horse approach refused nearby threats; no chase')
    if (now() >= until) throw new Error('horse surface approach reached its eight-second budget; inspect before another attempt')
    if (distance(origin, entity.position) > 16 || distance(origin, bot.entity.position) > 16) throw new Error('horse surface approach is limited to a local 16-block area; choose checked checkpoints before approaching')
  }
  const reached = attempts => {
    if (!bot.entity.onGround) throw new Error('horse approach ended airborne; wait for checked footing before boarding')
    return { withinReach: true, attempts, at: point(bot.entity.position) }
  }
  guard()
  for (let attempt = 0; attempt < 4; attempt++) {
    guard()
    if (distance(bot.entity.position, entity.position) <= 2.5) return reached(attempt)
    const target = point(entity.position), legUntil = Math.min(until, now() + 3000)
    report({ action: 'ride', status: 'checked surface boarding approach', attempt: attempt + 1, horse: entity.id, target, retreat: origin })
    try {
      const result = await surfaceWalk.walk({ surface: 'horse', x: Math.floor(target.x), y: Math.floor(target.y), z: Math.floor(target.z), range: 1 }, {
        check: () => {
          guard()
          if (distance(bot.entity.position, entity.position) <= 2.5) throw new HorseWithinReach('horse reached; settling native approach before boarding')
          if (distance(target, entity.position) > 1.25 || now() >= legUntil) throw new RefreshHorseTarget('horse approach checkpoint changed; settling before a checked refresh')
        }
      })
      if (!result.arrived) throw new Error(`no checked surface approach to horse: ${result.why ?? result.status}`)
    } catch (error) {
      // SurfaceWalk releases its native goal and settles its promise before
      // either internal signal reaches here. Safety/cancellation retains identity.
      if (!(error instanceof RefreshHorseTarget) && !(error instanceof HorseWithinReach)) throw error
    }
    guard()
    if (distance(bot.entity.position, entity.position) <= 2.5) return reached(attempt + 1)
  }
  throw new Error('horse kept moving beyond four checked approach updates; stopped without boarding')
}
