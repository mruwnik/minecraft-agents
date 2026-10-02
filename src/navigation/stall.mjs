// A walk that presses forward while the position does not change (card 962beec2: five stalls in one day at 119,72,-66,
// every one during flock.lead, a valid path each time, block_at showing nothing but leaf_litter and air). The 12 s
// stall alarm says only "the legs were pressing forward". These are the pieces of evidence that say why: where the
// head faces against where the path goes, what the legs push into, who is pressed against the body, and where the
// SERVER last put it. Pure: src/body/connection.mjs reads the body and hands the numbers in.

import { progressed } from '../lib/path.mjs'

// Time only the current walk. A routine can spend minutes in place between
// walks, while replanning the same goal must retain its original stall clock.
// Preserve object identity while still: kickedFor/frozenFor refer to this episode.
export function walkStandstill (previous, { task, goal, pos, now }) {
  if (task == null || !goal || !previous || previous.task !== task || previous.goal !== goal || progressed(previous.pos, pos)) {
    return { pos: { x: pos.x, y: pos.y, z: pos.z }, at: now, task, goal }
  }
  return previous
}

export const WALK_PROGRESS_MS = 40000
// Small swimming oscillations reset the standstill clock, but cannot keep a
// nearest-goal record improving. This clock belongs to an awaited walking leg,
// including one inside a composite; it does not cancel the owning expedition.
export function walkProgress (previous, { leg, goal, pos, now, busy = false }) {
  if (!leg || goal?.entity || !Number.isFinite(goal?.x) || !Number.isFinite(goal?.z)) return null
  const target = [goal.constructor?.name, goal.x, goal.y, goal.z, goal.range, goal.rangeSq].join(':')
  const distance = Math.hypot(goal.x - pos.x, Number.isFinite(goal.y) ? goal.y - pos.y : 0, goal.z - pos.z)
  if (!previous || previous.leg !== leg || previous.target !== target || distance <= previous.best - 1) {
    return { leg, target, best: distance, at: now, sampledAt: now, busy, stalled: false }
  }
  // Mining and construction legitimately work in place; don't spend their
  // time allowance on the walk, including the first sample after they finish.
  const at = previous.at + (busy || previous.busy ? now - previous.sampledAt : 0)
  return { ...previous, at, sampledAt: now, busy, stalled: !busy && now - at >= WALK_PROGRESS_MS }
}

// The client's registry is one protocol behind the server (26.1 speaking to 26.2 through a bridge): a block state past
// the registry comes back with no name at all, and a nameless block must never be reported as ground.
export const blockName = block => (block === null || block === undefined) ? 'unloaded' : (block.name || `unknown(state ${block.stateId})`)

// forward held with the body not moving: 2 s of that is a frozen walk, long before the 12 s alarm
export const frozenWalk = ({ keys, moved, seconds }) => keys.includes('forward') && moved < 0.05 && seconds >= 2

// prismarine-physics walks forward along (-sin yaw, -cos yaw): yaw 0 faces north (-z), pi/2 faces west (-x)
const heading = yaw => ({ dx: -Math.sin(yaw), dz: -Math.cos(yaw) })

const parseNode = text => { const [x, y, z] = text.split(',').map(Number); return { x, y, z } }

// the first path node the body does not already stand on (nodes are the "x,y,z" strings the stall evidence keeps)
export const nextNode = (from, nodes) => nodes.map(parseNode).find(n => Math.hypot(n.x - from.x, n.z - from.z) > 0.3) ?? null

// how far the head is turned from the next node, in degrees: 0 walks at it, 180 walks away from it
export function facingOff ({ yaw, from, nodes }) {
  const next = nextNode(from, nodes)
  if (!next) return null
  const f = heading(yaw)
  const dx = next.x - from.x
  const dz = next.z - from.z
  const degrees = Math.round(Math.abs(Math.atan2(f.dx * dz - f.dz * dx, f.dx * dx + f.dz * dz)) * 180 / Math.PI)
  return { degrees, node: `${next.x},${next.y},${next.z}` }
}

// the cell one step ahead of the body, at feet and at head height: what forward presses into
export function aheadCells (from, yaw) {
  const f = heading(yaw)
  const x = Math.floor(from.x + f.dx * 0.7)
  const z = Math.floor(from.z + f.dz * 0.7)
  const y = Math.floor(from.y)
  return [{ x, y, z }, { x, y: y + 1, z }]
}

// the last position packet the server sent (a correction: the server only speaks when it disagrees), with its age
export const serverSide = (last, now) => last ? { x: last.x, y: last.y, z: last.z, agoMs: now - last.at } : null

// everything within two blocks of the body, nearest first: a mob pressed against the legs pushes back as fast as the
// body walks, and the client cannot see that push
export function nearBy (entities, me, reach = 2) {
  return entities
    .filter(e => e !== me && e.id !== me.id && e.position)
    .map(e => ({ name: e.username ?? e.name ?? e.type, dist: Math.hypot(e.position.x - me.position.x, e.position.y - me.position.y, e.position.z - me.position.z) }))
    .filter(e => e.dist <= reach)
    .sort((a, b) => a.dist - b.dist)
    .map(e => `${e.name} ${Math.round(e.dist * 10) / 10}m`)
}

const AIRY = /^(air|cave_air|void_air|water|short_grass|tall_grass|fern|large_fern|dead_bush|leaf_litter|snow|unloaded)$/
const solidAhead = ahead => ahead.map(text => text.split('@')).find(([name]) => !AIRY.test(name))

// one line that says what froze the walk, from the evidence: the cases seen so far, most telling first
export function frozenAdvice ({ exact, facing, ahead = [], near = [], server }) {
  const wall = solidAhead(ahead)
  const wallText = wall ? `${wall[0]} at ${wall[1]}` : null
  if (wall && facing && facing.degrees > 45) return `the head faces ${facing.degrees} degrees off the next node (${facing.node}) and the legs push into ${wallText}: something else is turning the head (a lookAt in the task, or the fence nudge)`
  if (wall && facing) return `the next node (${facing.node}) lies beyond ${wallText}: the path was planned from a cell the body is not really in. goto two blocks back the way it came, then retry`
  if (near.length) return `nothing solid ahead but ${near.join(', ')} is pressed against the body: it is being pushed back as fast as it walks. Step sideways first (goto), then retry`
  if (server) return `the server placed the body at ${server.x},${server.y},${server.z} ${Math.round(server.agoMs / 100) / 10} s ago while the client walks from ${exact.join(',')}: the two disagree on where it stands. Copy this into ../../BUGS.md`
  return 'nothing solid ahead, nothing near, no server correction: the physics itself is not moving the body (a speed of 0 from the server?). Copy this into ../../BUGS.md'
}
