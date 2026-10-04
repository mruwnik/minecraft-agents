// The fake's steer and pathWorld. steer is a toy kinematic walker, not physics: every tick (a setImmediate, so tests are
// fast) it asks decide(pose), then moves 0.2 blocks (0.26 sprinting) along the yaw, steps up at most 0.6 (1.25 with jump),
// drops to the next floor at once, and climbs or descends a ladder. A tick budget of timeoutS * 20 (at most 2400) stands
// in for the real time bound. Results have the real steer's shapes.
import { fixtureSnapshot } from './path/fixture.mjs'
import { defaultStateTable } from './path/blocks.mjs'
import * as space from './path/space.mjs'
import { dragLeashed } from './fake-leash.mjs'
import { temptFollow } from './fake-tempt.mjs'
import { isOpen, climbsThrough, pathProps } from './fake-doors.mjs'

const MAX_TICKS = 2400
const TICKS_PER_S = 20
const WALK = 0.2
const SPRINT = 0.26
const STEP = 0.6
const JUMP_STEP = 1.25
const CLIMB = 0.2
const SLIP = 0.15
const HEIGHT = 1.8
const SCAN_DOWN = 4
const FALL_LIMIT = 256
const BURY = 3
const PASSABLE = new Set(['air', 'water', 'ladder', 'vine', 'short_grass', 'tall_grass', 'rail', 'powered_rail', 'detector_rail', 'activator_rail'])
// small blocks with no collision box: the planner and the game walk through them
const NO_COLLISION = /^(lever|torch|wall_torch|redstone_torch|redstone_wall_torch)$|_(button|pressure_plate|sign|wall_sign|hanging_sign)$/
const CLIMBABLE = new Set(['ladder', 'vine'])
const key = (x, y, z) => `${x},${y},${z}`
const round = n => Math.round(n * 1e6) / 1e6
const isNum = n => typeof n === 'number' && Number.isFinite(n)
const badArgs = message => Object.assign(new Error(message), { code: 'bad-args', badArgs: true })

// The old walker gave up on a target in spec.unreachable or spec.noPath whatever the range: the planner has no such
// switch, so the cells within BURY of one (not the body's own) are stone in the planner's view, which leaves no spot to
// stand on within a walk's range of it. Cells that hold a block already stay what they are.
function buried (s) {
  const body = s.self.pos
  const bodyCell = (x, y, z) => x === body.x && z === body.z && (y === body.y || y === body.y + 1)
  const span = Array.from({ length: 2 * BURY + 1 }, (_, i) => i - BURY)
  return [...s.unreachable, ...s.noPath].flatMap(k => {
    const [cx, cy, cz] = k.split(',').map(Number)
    return span.flatMap(dx => span.flatMap(dy => span.map(dz => [cx + dx, cy + dy, cz + dz])))
  }).filter(([x, y, z]) => !bodyCell(x, y, z) && !s.blocks.has(key(x, y, z))).map(c => [c, 'stone'])
}

export function fakePathWorld (s) {
  const entries = [...[...s.blocks].map(([k, name]) => [k.split(',').map(Number), name]), ...buried(s)]
  // the fixture joins fences, walls and panes to their neighbours, as the server does: a lone post is a gap a body slips through
  const snapshot = fixtureSnapshot({ blocks: entries.map(([[x, y, z], name]) => [x, y, z, name, pathProps(s, key(x, y, z))]) })
  return { snapshot, table: defaultStateTable(), space }
}

export function fakeSteer (s, ownerOf, CutError) {
  const nameAt = (x, y, z) => s.blocks.get(key(x, y, z)) ?? 'air'
  const solid = (x, y, z) => !PASSABLE.has(nameAt(x, y, z)) && !NO_COLLISION.test(nameAt(x, y, z)) && !isOpen(s, key(x, y, z))
  const climbable = (x, y, z) => CLIMBABLE.has(nameAt(x, y, z)) || climbsThrough(s, x, y, z)

  // ground level of the cell (x, z) for a body whose feet are at y: the first y' from one above down to SCAN_DOWN below
  // with a solid cell under it and two free cells above, else null
  const groundAt = (x, y, z) => {
    const top = Math.floor(y) + 1
    return Array.from({ length: SCAN_DOWN + 2 }, (_, i) => top - i).find(g => solid(x, g - 1, z) && !solid(x, g, z) && !solid(x, g + 1, z)) ?? null
  }

  const horizontal = (body, { forward, sprint, jump }, yaw) => {
    if (!forward) return { ...body, collided: false }
    const step = sprint ? SPRINT : WALK
    const x = body.x - Math.sin(yaw) * step
    const z = body.z - Math.cos(yaw) * step
    const [cx, cz] = [Math.floor(x), Math.floor(z)]
    if (cx === Math.floor(body.x) && cz === Math.floor(body.z)) return { ...body, x, z, collided: false }
    const g = groundAt(cx, body.y, cz)
    const rise = g === null ? Infinity : g - body.y
    const allowed = rise <= STEP || (rise <= JUMP_STEP && jump)
    return allowed ? { ...body, x, y: g, z, collided: false } : { ...body, collided: true }
  }

  // [body after the vertical part of the tick, whether the body climbed]
  const vertical = (body, { jump }) => {
    const [cx, cz] = [Math.floor(body.x), Math.floor(body.z)]
    const cell = Math.floor(body.y)
    if (climbable(cx, cell, cz)) {
      if (jump) return solid(cx, Math.floor(body.y + CLIMB + HEIGHT), cz) ? { ...body, vy: 0 } : { ...body, y: round(body.y + CLIMB), vy: CLIMB }
      const lower = round(body.y - SLIP)
      const below = Math.floor(lower)
      return solid(cx, below, cz) ? { ...body, y: below + 1, vy: below + 1 - body.y } : { ...body, y: lower, vy: -SLIP }
    }
    const ground = Array.from({ length: FALL_LIMIT }, (_, i) => cell - 1 - i).find(y => solid(cx, y, cz))
    if (solid(cx, cell - 1, cz) || ground === undefined) return { ...body, vy: 0 }
    return { ...body, y: ground + 1, vy: ground + 1 - body.y }
  }

  const poseOf = (body, yaw) => {
    const [cx, cz] = [Math.floor(body.x), Math.floor(body.z)]
    return {
      x: body.x,
      y: body.y,
      z: body.z,
      vy: body.vy,
      onGround: solid(cx, Math.floor(body.y) - 1, cz) && body.y === Math.floor(body.y),
      onClimbable: climbable(cx, Math.floor(body.y), cz),
      inWater: nameAt(cx, Math.floor(body.y), cz) === 'water',
      collided: body.collided,
      yaw,
      t: Date.now()
    }
  }

  return (token, a = {}) => {
    const decide = a.decide
    if (typeof decide !== 'function') throw badArgs('steer needs decide, a function')
    const timeoutS = a.timeoutS ?? 60
    if (!isNum(timeoutS) || timeoutS <= 0 || timeoutS > 120) throw badArgs('steer timeoutS must be a number in (0, 120]')
    const budget = Math.min(MAX_TICKS, Math.ceil(timeoutS * TICKS_PER_S))
    return new Promise((resolve, reject) => {
      let body = { x: s.self.pos.x + 0.5, y: s.self.pos.y, z: s.self.pos.z + 0.5, vy: 0, collided: false }
      const from = { ...s.self.pos }
      let yaw = 0
      let ticks = 0
      const finish = (settle, value) => {
        s.controls = {}
        if (settle === resolve) { dragLeashed(s, Math.hypot(s.self.pos.x - from.x, s.self.pos.z - from.z)); temptFollow(s) } // as the fake moveTo, once the walk is over
        settle(value)
      }
      const tick = () => {
        if (ownerOf() !== token) return finish(reject, new CutError())
        if (ticks >= budget) return finish(resolve, { status: 'timeout', pose: poseOf(body, yaw) })
        ticks += 1
        const out = decideSafely(poseOf(body, yaw))
        if (out.failed) return finish(resolve, { status: 'failed', reason: out.failed })
        if (out.done) return finish(resolve, { status: 'done', result: out.done, ticks })
        const controls = out.controls ?? {}
        s.controls = { ...controls }
        yaw = isNum(out.yaw) ? out.yaw : yaw
        body = vertical(horizontal(body, controls, yaw), controls)
        s.self.pos = { ...s.self.pos, x: Math.floor(body.x), y: Math.floor(body.y), z: Math.floor(body.z) }
        setImmediate(tick)
      }
      const decideSafely = pose => {
        try { return decide(pose) ?? {} } catch (err) { return { failed: String(err).slice(0, 200) } }
      }
      setImmediate(tick)
    })
  }
}
