// The fake's food lure: while the body holds a breeding food of an animal that is not on a lead and stands within
// TEMPT_RANGE, the animal walks to TEMPT_STOP from the body by its own path: straight when the way is free, else
// through one open fence gate (the cell before it, the gate, the cell after it); a fence or a shut gate stops it
// on its side. Run after the body walks and after each wait.
import { BREEDING_FOOD } from './fake-interact.mjs'
import { walkLine, walkable } from './fake-walkline.mjs'

const TEMPT_RANGE = 10
const TEMPT_STOP = 2.5
const GATE_SEARCH = 16
const AXES = [[1, 0], [0, 1]]

const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const centre = ([x, y, z], dx = 0, dz = 0) => ({ x: x + dx + 0.5, y, z: z + dz + 0.5 })

function openGates (s) {
  return [...s.blocks.entries()]
    .filter(([k, name]) => /_fence_gate$/.test(name) && s.states.get(k)?.open === true)
    .map(([k]) => k.split(',').map(Number))
    .filter(g => dist(centre(g), s.self.pos) <= GATE_SEARCH)
}

// Straight first, then each way through an open gate, both directions; the first that gets there, else the
// straight walk as far as it went (pressed against the fence).
function walkTo (s, e) {
  const enters = walkable(s)
  const body = s.self.pos
  const straight = walkLine([e.pos, body], enters, TEMPT_STOP)
  if (straight.clear) return straight.pos
  const viaGates = openGates(s).flatMap(g => AXES.flatMap(([dx, dz]) => [
    [centre(g, -dx, -dz), centre(g), centre(g, dx, dz)],
    [centre(g, dx, dz), centre(g), centre(g, -dx, -dz)]
  ]))
  const through = viaGates
    .map(way => walkLine([e.pos, ...way.map(p => ({ ...p, y: e.pos.y })), body], enters, TEMPT_STOP))
    .find(r => r.clear)
  return (through ?? straight).pos
}

export function temptFollow (s) {
  const held = s.self.held
  if (!held) return
  s.entities
    .filter(e => !e.leashed && (BREEDING_FOOD[e.name] ?? []).includes(held) && dist(e.pos, s.self.pos) <= TEMPT_RANGE)
    .forEach(e => { e.pos = walkTo(s, e) })
}
