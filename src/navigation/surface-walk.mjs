import { createRequire } from 'node:module'
import { configureEscortMoves } from './escort.mjs'
import { thinkBudget } from './walk.mjs'

const require = createRequire(import.meta.url)
const nativeGoto = require('mineflayer-pathfinder/lib/goto')

export function surfaceRequest (args, from) {
  if (args.surface !== 'horse') throw new Error('surface= currently accepts horse only')
  if (args.dig || args.player || args.place || args.into || args.live) throw new Error('surface=horse needs an explicit, non-digging x/y/z checkpoint; no player, place, into or live override')
  if (!['x', 'y', 'z'].every(k => Number.isInteger(args[k]))) throw new Error('surface=horse needs integer x= y= z= feet coordinates')
  const range = args.range ?? 1
  if (!Number.isFinite(range) || range < 0 || range > 2) throw new Error('surface=horse range must be between zero and two blocks')
  const distance = Math.hypot(args.x + 0.5 - from.x, args.y - from.y, args.z + 0.5 - from.z)
  if (distance > 16) throw new Error('surface=horse accepts short checkpoints within 16 blocks; inspect the next loaded leg from there')
  return { x: args.x, y: args.y, z: args.z, range, distance }
}

// One profile serves both preview and execution. No normal goto fallback, rim
// retargeting, digging, or straight-line player-only shortcut is used here.
export function makeSurfaceWalkRuntime ({ getBot, Vec3, goals, makeMoves, cancelGuard, dangerous = () => false,
  reportPerformance = () => {}, report = () => {}, pause = ms => new Promise(resolve => setTimeout(resolve, ms)), now = Date.now,
  go = nativeGoto } = {}) {
  const guard = (bot, initialHealth, check) => {
    check()
    if (bot.vehicle) throw new Error('surface walk cannot run while mounted')
    if (bot.health < 16 || bot.health < initialHealth || dangerous()) throw new Error('surface walk stopped for damage, nearby danger or night; inspect the reported retreat before moving')
  }
  const planning = args => {
    const wantRoute = args.route === true
    const bot = getBot(), from = bot.entity.position.clone(), aim = surfaceRequest(args, from)
    if (bot.vehicle) throw new Error('surface preview requires an unmounted starting stance')
    if (!bot.entity.onGround) throw new Error('surface preview needs a grounded starting stance')
    const moves = configureEscortMoves(makeMoves(false), {
      blockAt: (x, y, z) => bot.blockAt(new Vec3(x, y, z)), from, to: aim, width: 1.4, height: 1.8,
      maxY: (bot.game.minY ?? -64) + (bot.game.height ?? 384), reportPerformance
    })
    if (!moves.escortCorridor().stance(from)) throw new Error('starting stance has no checked horse-width dry surface corridor')
    const goal = new goals.GoalNear(aim.x, aim.y, aim.z, aim.range)
    const began = now(), budget = thinkBudget(aim.distance)
    let visited = 0
    const search = (start, end) => {
      let result
      // Native post-processing consults global live movements. Resolve raw
      // nodes ourselves so read-only previews never change the active profile.
      const iterator = bot.pathfinder.getPathFromTo(moves, start, end, { timeout: budget, optimizePath: false })
      for (const step of iterator) { result = step.result; if (result.status !== 'partial') break }
      visited += result?.visitedNodes ?? 0
      if (!result) throw new Error('surface planner returned no search result')
      const points = result.path.map(p => moves.resolveTerrainWaypoint?.(p) ?? { x: p.x + 0.5, y: p.y, z: p.z + 0.5 })
      return { status: result.status, points, cost: result.cost }
    }
    let out, back
    try {
      out = search(from, goal)
      if (out.status !== 'success') return { status: out.status, surface: 'horse', why: 'no complete checked surface approach; no movement started', ms: now() - began }
      if ([from, ...out.points].some(p => dangerous(p))) return { status: 'refused', surface: 'horse', why: 'night or nearby danger along the planned surface route; no movement started', ms: now() - began }
      const end = out.points.at(-1) ?? from
      const first = out.points[0] ?? from
      if (!moves.escortCorridor().edge(from, first)) return { status: 'refused', surface: 'horse', why: 'actual starting footprint cannot reach the first checked waypoint', ms: now() - began }
      const origin = moves.resolveTerrainStart?.(from, true) ?? from.floored()
      back = search(new Vec3(end.x, end.y, end.z), new goals.GoalBlock(origin.x, origin.y, origin.z))
      if (back.status !== 'success') return { status: back.status, surface: 'horse', why: 'forward route exists but no complete checked retreat was found; no movement started', ms: now() - began }
      if (back.points.some(p => dangerous(p))) return { status: 'refused', surface: 'horse', why: 'nearby danger along the checked retreat; no movement started', ms: now() - began }
      const report = { status: 'success', surface: 'horse', ms: now() - began, nodes: out.points.length, visited,
        ends: `${end.x},${end.y},${end.z}`, retreat: { x: from.x, y: from.y, z: from.z }, reverseNodes: back.points.length,
        ...(wantRoute ? { waypoints: out.points, reverseWaypoints: back.points } : {}),
        note: 'Checked loaded geometry in both directions; terrain and threats are checked again before execution. Retreat is reported, never automatic.' }
      return { ...report, internal: { moves, goal, from, points: out.points } }
    } finally { reportPerformance('surface.plan', now() - began, { forward: out?.status, reverse: back?.status, visited, goal: aim }) }
  }
  const preview = args => {
    const { internal, ...report } = planning(args)
    return report
  }
  const walk = async args => {
    const bot = getBot(), check = cancelGuard(), initialHealth = bot.health
    guard(bot, initialHealth, check)
    const plan = planning(args), { internal, ...previewReport } = plan
    if (!internal) return { ...previewReport, arrived: false }
    await pause(0)
    guard(bot, initialHealth, check)
    const { moves, goal, from, points } = internal, previous = bot.pathfinder.movements
    bot.pathfinder.setMovements(moves)
    let result, settled = false, lastProgress = now(), best = Infinity, failure
    // Use the native promise directly. The normal goto wrapper can manually
    // step off a perch after any failure; that is not an authorized surface route.
    let pending
    try {
      pending = Promise.resolve(go(bot, goal)).then(() => { settled = true }, error => { settled = true; failure = error })
      while (!settled) {
        guard(bot, initialHealth, check)
        if (points.some(p => dangerous(p))) throw new Error('surface walk stopped: danger entered the planned corridor')
        const p = bot.entity.position, distance = Math.hypot(p.x - args.x - 0.5, p.y - args.y, p.z - args.z - 0.5)
        if (distance < best - 0.5) { best = distance; lastProgress = now() }
        if (now() - lastProgress >= 20000) throw new Error('surface walk made no sustained progress for 20s')
        moves.refreshEscortTerrain()
        if (bot.entity.onGround && !moves.escortCorridor().stance(p)) throw new Error('current stance no longer has a checked surface corridor')
        await pause(100)
      }
      guard(bot, initialHealth, check)
      if (failure) throw failure
      // Native waypoint arrival allows nearly a block of vertical tolerance.
      // A jump may resolve its goal while the feet are still settling; wait
      // boundedly for real grounded arrival without starting another walk.
      const settleUntil = now() + 1500
      while (!bot.entity.onGround && now() < settleUntil) {
        const p = bot.entity.position, cell = p.floored()
        if (!goal.isEnd({ x: cell.x, y: goal.y, z: cell.z }) || Math.abs(p.y - goal.y) > 1.25) break
        guard(bot, initialHealth, check)
        await pause(50)
      }
      guard(bot, initialHealth, check)
      const p = bot.entity.position, cell = moves.resolveTerrainStart?.(p, bot.entity.onGround) ?? p.floored()
      if (!bot.entity.onGround || !goal.isEnd(cell)) throw new Error('surface walk ended before reaching the checked goal')
      result = { ...previewReport, arrived: true, at: { x: p.x, y: p.y, z: p.z } }
    } catch (error) {
      // Cancellation and other safety errors retain their identity for the
      // composite runner; the checkpoint is diagnostic, never a forced retreat.
      error.retreat = { x: from.x, y: from.y, z: from.z }
      report({ status: 'stopped', why: error.message, retreat: error.retreat, automaticRetreat: false })
      throw error
    } finally {
      if (bot.pathfinder.goal === goal) bot.pathfinder.setGoal(null)
      await pending
      if (bot.pathfinder.movements === moves) bot.pathfinder.setMovements(previous)
    }
    return result
  }
  return { preview, walk }
}
