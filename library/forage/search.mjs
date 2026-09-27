import { searchWaypoints, searchSurface, outwardCandidates, SEARCH_HEADINGS, connectedFrontiers } from '../../src/forage.mjs'
import { CompositeHandBack } from '../../src/composite.mjs'

// Inspect the leading failure, not safety words in the pathfinder's advisory paragraph.
function navigationFailure (error) {
  const text = String(error?.message ?? '').replace(/^forage\.search\/goto:\s*/, '')
  const leading = text.split(/[.!?\n]/)[0]
  if (error instanceof CompositeHandBack || error?.reason || error?.name !== 'Error' || /cancel|abort|supersed|dead|died|health|offline|disconnect|sleep|protected|spoken to/i.test(leading) || !/no path|no walkable|nowhere to stand|not a spot to stand|unreachable|ran out of time|no first move|cannot reach|could not reach/i.test(leading)) throw error
  return error.message
}
async function act (api, name, args) {
  const result = await api.act(name, args)
  if (result?.stopped) throw new CompositeHandBack(result.stopped)
  return result
}

export default {
  doc: 'forage.search block= | mob= [count=1] [pattern=outward|spiral|sweep] [heading=north|east|south|west] [radius=512] [spacing=16] [steps=64] [minutes=10] [range=24] [origin=x,y,z]: discover targets on a bounded expedition; outward advances in short loaded legs; origin resumes the original radius boundary',
  stops: 'enough targets, distance/step/time budget, eight rounds without progress, or safety/cancellation hand-back',
  args: { block: 'string', mob: 'string', count: 'number', pattern: 'string', heading: 'string', radius: 'number', spacing: 'number', steps: 'number', minutes: 'number', range: 'number', origin: 'string' },
  async run (api, a) {
    if (!!a.block === !!a.mob) throw new Error('give exactly one of block= or mob=')
    const count = a.count ?? 1
    const pattern = a.pattern ?? 'outward'
    const heading = a.heading ?? 'north'
    const radius = a.radius ?? 512
    const spacing = a.spacing ?? 16
    const steps = a.steps ?? 64
    const minutes = a.minutes ?? 10
    const range = a.range ?? 24
    if (!['outward', 'spiral', 'sweep'].includes(pattern) || !Object.hasOwn(SEARCH_HEADINGS, heading) || !Number.isInteger(count) || count < 1 || count > 256 || !Number.isInteger(steps) || steps < 0 || steps > 512 || ![radius, spacing, range, minutes].every(n => Number.isFinite(n) && n > 0) || radius > 4096 || spacing < 4 || spacing > 32 || range > 64 || minutes > 60) throw new Error('invalid search bounds: count 1..256, steps 0..512, radius <=4096, spacing 4..32, range <=64, minutes <=60; choose outward/spiral/sweep and cardinal heading')
    const originValues = a.origin?.split(',').map(Number)
    if (originValues && (originValues.length !== 3 || !originValues.every(Number.isFinite))) throw new Error('origin= must be x,y,z')
    const origin = originValues ? Object.fromEntries(['x', 'y', 'z'].map((k, i) => [k, originValues[i]])) : { ...api.pos() }
    const found = new Map()
    const visited = []
    const failures = []
    const slowReported = new Set()
    const slowScan = (action, ms, workload) => {
      if (api.performance) { api.performance(action, ms, workload); return }
      if (ms <= 1000 || slowReported.has(action)) return
      slowReported.add(action)
      api.report({ performance_bug: `${action} took ${ms}ms (target <=1000ms); result retained` })
      api.emit?.('performance_bug', { action, ms, targetMs: 1000, workload, resultRetained: true })
    }
    const attempted = new Set()
    const seen = new Set()
    const started = Date.now()
    let rounds = 0
    let stalled = 0
    let reason = 'step budget exhausted'
    const distance = () => Math.round(Math.hypot(api.pos().x - origin.x, api.pos().z - origin.z))
    const progress = () => api.report({ found: found.size, scans: visited.length, travelled: distance(), heading, search: `found ${found.size}/${count}; ${rounds}/${steps} rounds; ${distance()} blocks from origin; ${failures.length} failed waypoints`, resume: `pattern=${pattern} heading=${heading} origin=${origin.x},${origin.y},${origin.z} radius=${radius}`, lastFailure: failures.at(-1)?.reason ?? 'none' })
    const scan = async () => {
      progress()
      await api.checkpoint()
      visited.push({ ...api.pos() })
      seen.add(`${Math.floor(api.pos().x)},${Math.floor(api.pos().z)}`)
      const scanStarted = Date.now()
      const result = a.block
        ? await act(api, 'find_blocks', { block: a.block, maxDistance: range, count: 256 })
        : await act(api, 'animals', { mob: a.mob, within: range })
      const scanMs = Date.now() - scanStarted
      api.report({ resource_scan_ms: scanMs })
      slowScan(a.block ? 'find_blocks' : 'animals', scanMs, { range, count: 256, target: a.block ?? a.mob })
      for (const target of (a.block ? result.positions : result.found) ?? []) {
        const key = a.block ? `${target.x},${target.y},${target.z}` : target.id ?? target.at
        if (key !== undefined) found.set(key, { ...target })
      }
      progress()
    }
    await scan()
    const waypoints = pattern === 'outward' ? [] : searchWaypoints(origin, { pattern, radius, spacing, steps })
    expedition: for (; rounds < steps && found.size < count; rounds++) {
      if (Date.now() - started >= minutes * 60000) { reason = 'time budget exhausted'; break }
      progress()
      await api.checkpoint()
      const columns = pattern === 'outward' ? outwardCandidates(api.pos(), heading, spacing) : waypoints[rounds] ? [waypoints[rounds]] : []
      // A radial expedition yields its boundary checkpoint instead of spending the
      // remaining budget tracing the circumference via lateral alternatives.
      if (pattern === 'outward' && columns[0] &&
          Math.hypot(api.pos().x - origin.x, api.pos().z - origin.z) >= radius - Math.min(16, spacing) &&
          Math.hypot(columns[0].x - origin.x, columns[0].z - origin.z) > radius) {
        reason = 'distance budget exhausted'; break
      }
      const frontier = pattern === 'outward' ? connectedFrontiers((...args) => api.block(...args), api.pos(), columns, api.navigationCapabilities?.() ?? {}) : null
      const candidates = frontier ? frontier.goals : columns
      if (frontier) {
        slowScan('forage.search.frontier', frontier.ms, { expanded: frontier.expanded, reads: frontier.reads, candidates: columns.length })
        api.report({ frontier_ms: frontier.ms, terrain: `${frontier.goals.length}/${columns.length} connected goals; ${frontier.expanded} cells in ${frontier.ms}ms${frontier.capped ? `; incomplete: ${frontier.limited}` : ''}` })
      }
      let moved = false
      let within = false
      for (const waypoint of candidates) {
        if (Date.now() - started >= minutes * 60000) { reason = 'time budget exhausted'; break expedition }
        if (Math.hypot(waypoint.x - origin.x, waypoint.z - origin.z) > radius) continue
        within = true
        const key = `${waypoint.x},${waypoint.z}`
        if (attempted.has(key) || seen.has(key)) continue
        attempted.add(key)
        await api.checkpoint()
        const goal = frontier ? { x: waypoint.x, y: waypoint.y, z: waypoint.z } : searchSurface((...args) => api.block(...args), waypoint.x, waypoint.z, api.pos().y)
        if (!goal) { failures.push({ ...waypoint, reason: 'no loaded safe surface' }); progress(); continue }
        try { await act(api, 'goto', { ...goal, range: 1, dig: false }) } catch (error) {
          const failure = navigationFailure(error)
          failures.push({ ...goal, reason: failure })
          if (api.recoverNavigationFailure && !api.recoverNavigationFailure({ ...goal, reason: failure })) throw error
          progress()
          continue
        }
        const here = api.pos()
        if (seen.has(`${Math.floor(here.x)},${Math.floor(here.z)}`)) { failures.push({ ...goal, reason: 'navigation made no progress' }); progress(); continue }
        moved = true
        await scan()
        break
      }
      if (!within && columns.every(p => Math.hypot(p.x - origin.x, p.z - origin.z) > radius)) { reason = 'distance budget exhausted'; break }
      if (frontier && !candidates.length) {
        const failure = { ...api.pos(), reason: frontier.capped ? `local terrain scan incomplete: ${frontier.limited}; unexamined goals may be reachable` : frontier.originReason ?? 'no locally connected safe ground; inspect terrain or choose another heading' }
        if (failures.at(-1)?.reason !== failure.reason) failures.push(failure)
        progress()
      }
      stalled = moved ? 0 : stalled + 1
      if (stalled >= 8) { reason = 'eight rounds without a reachable new waypoint'; break }
    }
    progress()
    return { target: a.block ?? a.mob, kind: a.block ? 'block' : 'mob', found: [...found.values()], wanted: count, complete: found.size >= count, reason: found.size >= count ? 'targets observed' : reason, origin, heading, distance: distance(), visited, failures, resume: `pattern=${pattern} heading=${heading} origin=${origin.x},${origin.y},${origin.z} radius=${radius}`, note: 'Targets are observations; recheck before harvesting or leading. Outward can resume from current position using resume arguments; increase radius to continue farther. Spiral and sweep restart their waypoint list.' }
  }
}
