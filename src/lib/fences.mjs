// Gates and fences: opening/shutting them, who left one open, and a pen or herd standing behind one.

// shut a door or gate I opened: once I am past it, or as soon as I have stopped beside it (but never on myself in the doorway)
// doors are mine to open on a walk; gates are the pathfinder's, except the one whose cell I already stand in: pressed against its closed panel the body floors into the gate's cell, the path starts there and never includes opening it
export const openNow = ({ near, open, door, moving, inDoorway }) => near && !open && moving && (door || inDoorway)
// a gate is only ever the body's to shut when its own walk opened it, never one another player stands by or touched
// within the minute (Perrin's body shut the human's gate 16 ms after they opened it, 13:30Z), and never with reflexes off
export const GATE_OTHERS_NEAR = 4
export const GATE_HANDS_OFF_MS = 60000
// A gate `held` open by toggle is never shut either: the gates.log listener takes the toggle's own click for a walk's (mine), and the
// reflex shut it behind the body three times over, "still closed after 3 tries" (Kettricken 22:00Z, my test gate 17:01Z)
export const shutNow = ({ near, open, mine, leading, moving, inDoorway, reflexes = true, otherNear = false, otherToggledMsAgo = Infinity, held = false }) =>
  reflexes && open && mine && !held && !leading && !otherNear && otherToggledMsAgo >= GATE_HANDS_OFF_MS && (!near || (!moving && !inDoorway))
// prismarine-block gives an OPEN fence gate boundingBox 'block' with no shapes, so to the pathfinder it is a wall: it routes through a
// gate only while the gate is shut (it opens it itself, useOne), and once the gate stands open, held by toggle or opened by the walk
// itself before a replan, every path through it is gone. Chani's goto to the cell beyond her open gate answered "no walkable path"
// (13:19Z), and walks replanned in the gate cell stalled there 12 s (14:19Z-16:03Z). What movements.getBlock adds to an open gate: air
export const openGateWalk = block => block?.name?.endsWith('_fence_gate') && block.open === true ? { safe: true, physical: false } : null
// bot.blockAt now mirrors the server's true per-block bamboo hitbox (navigation/bamboo.mjs), but the pathfinder's A* only
// knows a cell holds bamboo, not where in it the stalk sits, so a path that hugs one can still clip it: a cell beside one costs extra
// my centre lies inside a fence's (wall's, shut gate's) cell: the free neighbour cell whose edge I am nearest to is where I really stand. free(x, y, z)
// One line for gates.log when a fence gate at `at` changes between open and shut: who stood nearest (players: [{name, dist}], myself included)
// `me`, `moving` (my walk has a goal) and `clicking` (my own hand is on a door) say who did it. mine: it opened under my
// own walk or click, within my reach, with nobody else within 4. byOther: anything that was not my own doing
export function gateChange (at, before, after, players, { me = null, moving = false, clicking = false } = {}) {
  const isGate = b => b?.name?.endsWith('_fence_gate')
  if (!isGate(before) || !isGate(after) || before.open === after.open) return null
  // a hand reaches about 5 blocks: anyone further off did not do it (the one who did is out of my sight; their own body logs it)
  const nearest = players.filter(p => p.dist <= 6).sort((a, b) => a.dist - b.dist)[0]
  const myDist = players.find(p => p.name === me)?.dist ?? Infinity
  const otherNear = players.some(p => p.name !== me && p.dist <= GATE_OTHERS_NEAR)
  const mine = after.open && (moving || clicking) && myDist <= 3 && !otherNear
  const byOther = !mine && !clicking
  return { gate: `${at.x},${at.y},${at.z}`, now: after.open ? 'open' : 'shut', nearest: nearest?.name ?? null, dist: nearest ? Math.round(nearest.dist) : null, mine, byOther }
}

// gates (keyed by String(Vec3)) that I walked through and that still stand open, as [x, y, z]: the gate reflex only reaches 5 blocks, so after leading animals in, the gate behind us needs shutting by hand
export const gatesLeftOpen = (opened, held, isOpen) => [...opened].filter(key => !held.has(key) && isOpen(key)).map(key => key.match(/-?\d+/g).map(Number))

// Can an animal walk out of this pen? Flood-fill the way a cow moves from `start` [x, y, z]: to a neighbouring column whose surface is at
// most 1 higher (a fence or wall top counts 1.5 above its foot, so it holds from level ground but not from a block beside it) or any
// amount lower. topsAt(x, z) gives the heights it could stand at in that column. Leaving `radius` columns from the start = out (24: at 12 a 17-long pen read LEAKS from its far end)
// rimsAt(x, z): the tops of blocks that carry a fence or wall: a post leaves a ledge beside it. An animal gets onto one only by walking (at most half a step up), and
// from a rim it goes on along the wall only: up onto a fence or wall top (a lower fence next along is half a step) or level to the next rim, never through its own
// fence onto plain ground, be that lower, level or a hillside higher
// `pen.check` walks from ONE cell and never said which. From 112,68,-126, outside claude-test-pen's fence, it answers
// `LEAKS via=116,68,-126`; from 114,68,-124, two blocks away and inside it, the same pen answers `holds cells=4`. Both
// are true of where they started, and neither said where that was, so a reading taken from outside got read as a hole
// in one's own fence - by Chani, then by Perrin, then by me (backlog #125). The cell goes in the reply now, and with it
// an answer to the question that matters: was that cell inside a fence ring at all?
// Counted the way you count whether a point is inside any closed outline. Cast out in the four directions and count how
// many times the way crosses a fence - a run of fence columns, so a wall two thick is one crossing, and a fence top is
// what ends on a half (topsAt gives 1.5 for a fence, whole numbers for ground). A cell inside a ring crosses it an ODD
// number of times whichever way it goes out; a cell outside crosses an even number, nought included. Four directions
// rather than one because a gate is a hole in the ring and a ray through the gateway miscounts, and because other
// people's fences lie across the long rays near a base.
// I tried a cheaper rule first - how many ways out run clear to the edge of the search - and it answered 0 at
// 112,68,-126, which reads as "inside a wall" at the very cell that had fooled three of us. Hilly, built-up ground
// stops every ray. A number that is true and says the opposite of what it means is worse than no number.
export function penStance ({ start, topsAt, radius = 24 }) {
  const [sx, , sz] = start
  const fence = (x, z) => topsAt(x, z).some(top => top % 1 !== 0)
  const crossings = ([dx, dz]) => {
    let runs = 0
    let was = false
    for (let n = 1; n <= radius; n++) {
      const now = fence(sx + dx * n, sz + dz * n)
      if (now && !was) runs++
      was = now
    }
    return runs
  }
  return [[1, 0], [-1, 0], [0, 1], [0, -1]].filter(d => crossings(d) % 2 === 1).length
}

// Said as evidence, with the count in it, because the count is checkable and the verdict is not: a gate in every wall,
// or a pen wider than the search, can still fool it.
export const stanceNote = (from, votes) => {
  if (votes >= 3) return `I walked from ${from}, and the fences around it count that cell INSIDE a ring on ${votes} of the four sides`
  if (votes <= 1) return `I walked from ${from}, and the fences around it count that cell OUTSIDE any ring on ${4 - votes} of the four sides: this is a reading of the ground outside the pen, so a leak found here says nothing about your fence. Check again from a cell inside it`
  return `I walked from ${from}, and the fences around it split two against two: I cannot tell whether that cell is inside the pen or outside it. Check again from a cell you know is inside`
}

export function penLeak ({ start, topsAt, rimsAt = () => [], radius = 24, withFloor = false }) {
  const key = c => c.join(',')
  const from = new Map([[key(start), null]])
  const queue = [start]
  while (queue.length) {
    const cell = queue.shift()
    const [x, y, z] = cell
    if (Math.max(Math.abs(x - start[0]), Math.abs(z - start[2])) > radius) {
      const path = []
      for (let c = cell; c; c = from.get(key(c))) path.unshift(c)
      // the telling part is the first climb (the step, the barrier top, the far side) or, on level ground, the first gap with a barrier on both sides
      const open = (c, dx, dz) => topsAt(c[0] + dx, c[2] + dz).some(top => top - c[1] <= 1)
      const gap = (c, prev) => prev[0] !== c[0] ? !open(c, 0, 1) && !open(c, 0, -1) : !open(c, 1, 0) && !open(c, -1, 0)
      const told = path.findIndex((c, i) => i > 0 && (c[1] !== path[i - 1][1] || gap(c, path[i - 1])))
      // ...or, through a gap too wide for that on level ground, the last cell beside the fence ring (a fence or wall top ends on .5) before the way leads off
      // into the open: without it the "first climb" was a slope 15-20 blocks from Ganesha's pen
      const fenced = (x, z) => topsAt(x, z).some(top => top % 1 !== 0)
      const byRing = c => fenced(c[0], c[2]) || [[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) => fenced(c[0] + dx, c[2] + dz))
      const leaves = path.findIndex((c, i) => i > 0 && byRing(c) && !path.slice(i + 1, i + 3).some(byRing))
      const found = [told, leaves].filter(i => i > 0)
      const at = found.length ? Math.min(...found) : -1
      const climbs = at > 0 && path[at][1] !== path[at - 1][1]
      return { enclosed: false, via: path.slice(Math.max(at, 0), climbs ? at + 3 : at + 1).map(key).join(' ') }
    }
    // Where a step into the next column can put it. Up to a half step it climbs; otherwise it steps off the edge and
    // falls to the FIRST surface below, never through one: a pen on a skin of ground over a cave used to read as LEAKS
    // by a path under its own floor (claude-test-pen, 113,67,-125), and the block a fence stands on is no way down either.
    const landingIn = (x, z, from) => {
      const under = [...topsAt(x, z), ...rimsAt(x, z)].filter(top => top <= from)
      if (!under.length) return []
      const first = Math.max(...under)
      return topsAt(x, z).includes(first) ? [first] : []
    }
    const waysInto = (x, z, from, onRim) => [
      ...topsAt(x, z).filter(top => top > from && top - from <= 1 && (!onRim || top % 1 !== 0)),
      ...(onRim ? [] : landingIn(x, z, from))
    ]
    // round a corner too, when at least one side of it is open: between two posts that only touch at the corner nothing squeezes
    const passable = (dx, dz) => waysInto(x + dx, z + dz, y, false).length > 0
    const steps = [[1, 0], [-1, 0], [0, 1], [0, -1], ...[[1, 1], [1, -1], [-1, 1], [-1, -1]].filter(([dx, dz]) => passable(dx, 0) || passable(0, dz))]
    for (const [dx, dz] of steps) {
      const onRim = rimsAt(x, z).includes(y)
      const ways = [...waysInto(x + dx, z + dz, y, onRim), ...rimsAt(x + dx, z + dz).filter(rim => rim - y <= 0.5 && (!onRim || rim >= y))]
      for (const top of ways) {
        const next = [x + dx, top, z + dz]
        if (from.has(key(next))) continue
        from.set(key(next), cell)
        queue.push(next)
      }
    }
  }
  const floor = [...from.keys()].filter(k => { const [x, y, z] = k.split(',').map(Number); return !rimsAt(x, z).includes(y) })
  return { enclosed: true, cells: floor.length, ...(withFloor ? { floor } : {}) }
}

// Gates of a pen that nothing can walk through. A way through has pen floor on one side of the gate, level with it, and straight across a
// spot an animal can stand on: a column top within one step up or down, and not a fence or wall top (those end on .5). A gate set in the
// corner of the ring has pen floor on no side at all (Ganesha's pen: a day of leads that ended outside it).
// Chani's gate at 101,71,-68 was flagged by every pen.check although flock.lead walked sheep through it twice (2026-09-24): outside it the
// flat grass stands one block higher than the pen floor, and the old test read the single block at the GATE's own level and called that
// step a wall. Heights now, and each blind gate says what it found rather than guessing corner-or-obstruction
const oneStep = (top, gateY) => top % 1 === 0 && Math.abs(top - gateY) <= 1
const gateWhy = (far, gateY, tops) => {
  const where = `${far[0]},${far[1]}`
  if (tops.some(top => top % 1 !== 0)) return `a fence or wall stands straight across from it at ${where}`
  if (!tops.length) return `nothing an animal can stand on straight across from it at ${where}: a solid block with no room over it, or a sheer drop`
  const step = tops.map(top => top - gateY).reduce((a, b) => Math.abs(a) <= Math.abs(b) ? a : b)
  return `the ground straight across from it at ${where} is ${Math.abs(step)} blocks ${step > 0 ? 'up' : 'down'}: more than the one step an animal climbs`
}
export const blindGates = (gates, floorAt, topsAt) => gates.flatMap(g => {
  const at = `${g.x},${g.y},${g.z}`
  const ways = [[1, 0], [0, 1]].flatMap(([dx, dz]) => [1, -1].map(s => [[g.x + s * dx, g.z + s * dz], [g.x - s * dx, g.z - s * dz]]))
    .filter(([near]) => floorAt(...near).some(y => Math.abs(y - g.y) <= 1))
  if (!ways.length) return [{ at, why: "no pen floor on any side of it, level with the gate: it stands in a CORNER of the fence ring, or on no wall of this pen at all" }]
  if (ways.some(([, far]) => topsAt(...far).some(top => oneStep(top, g.y)))) return []
  return [{ at, why: ways.map(([, far]) => gateWhy(far, g.y, topsAt(...far))).join('; ') }]
})

// Is this enclosure (penLeak's floor: "x,y,z" keys) a PEN, or just a sealed pocket of rock? A pen has a fence or wall beside its floor: a top ending on .5, more than a step up
export const fencedIn = (floor, topsAt) => floor.some(k => {
  const [x, y, z] = k.split(',').map(Number)
  return [[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) => topsAt(x + dx, z + dz).some(top => top % 1 === 0.5 && top - y > 1))
})

// leading: has the herd come through this gate? Every follower must be nearer to me than the gate is, with a block to spare. Until then the gate stays open;
// after that it is shut AT ONCE, not at the end of the lead (Ganesha's body walked 170 blocks back to shut two)
const gap = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1], a[2] - b[2])
export const herdPassed = (me, gate, animals) => animals.every(a => gap(a, me) < gap(gate, me) - 1)
// gates still open when a task ends: the ones within reach get shut, the far ones are only named (no silent cross-country walk)
// inside: gates my hitbox (0.3 each side, 1.8 tall) overlaps, as a shut gate stands 1.5 tall. Shutting one of those shut it on me:
// collect ended in the gate at 101,71,-68 and the next goto stalled there. The door reflex shuts it once I have walked off
const inGate = ([x, y, z], [mx, my, mz]) => Math.abs(mx - (x + 0.5)) < 0.8 && Math.abs(mz - (z + 0.5)) < 0.8 && my < y + 1.5 && my + 1.8 > y
export const gatesByReach = (gates, me, reach = 32) => ({
  near: gates.filter(g => gap(g, me) <= reach && !inGate(g, me)),
  far: gates.filter(g => gap(g, me) > reach).map(g => g.join(',')).join(' '),
  inside: gates.filter(g => inGate(g, me))
})
