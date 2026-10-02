// Pens read off the world round the body (the rules are src/pens.mjs and src/lib.mjs penLeak).
import { blindGates, fencedIn, strays, penCensus, penLeak, BREEDING_FOOD } from '../lib.mjs'
import { Vec3, bot } from './state.mjs'

// given: the plain arguments, for the log (printing the tracked ones would count as reading them all)
// the gate reflex only reaches 5 blocks and can miss at a sprint: whatever I opened and is still open when a task ends gets shut now.
// An open gate empties a pen (the human's sheep after lead, Kettricken's after flock.breed, Miles' after shear and goto)
// gates on the ring of this pen (floor: "x,y,z" keys) that nothing can walk through: see blindGates. topsAt is penAround's own column
// reader, the one penLeak walks by: heights an animal can stand at, so a step up outside a gate reads as the step it is (Chani's report, 2026-09-24)
export function blindGateAdvice (floor, topsAt) {
  const cells = floor.map(k => k.split(',').map(Number))
  const floorAt = (x, z) => cells.filter(c => c[0] === x && c[2] === z).map(c => c[1])
  const around = [-1, 0, 1].flatMap(dx => [-1, 0, 1].map(dz => [dx, dz]))
  const gates = [...new Map(cells.flatMap(([x, y, z]) => around.map(([dx, dz]) => new Vec3(x + dx, Math.floor(y), z + dz)))
    .filter(p => bot.blockAt(p)?.name.endsWith('_fence_gate')).map(p => [String(p), p])).values()]
  const blind = blindGates(gates, floorAt, topsAt)
  return blind.length ? { blindGates: `${blind.map(b => `${b.at} (${b.why})`).join('; ')}. Nothing can walk through such a gate: put it in the middle of a wall with ground straight across at the pen floor's own level (one step up or down is fine)` } : {}
}

// the pen around a floor cell, walked the way an animal can (see penLeak); null when that cell is no spot to stand on
export function penAround (feet, radius) {
  const columns = new Map()
  const rims = new Map()
  const thin = shape => shape[3] - shape[0] < 0.8 || shape[5] - shape[2] < 0.8
  const shapesAt = (x, y, z) => bot.blockAt(new Vec3(x, y, z))?.shapes ?? []
  // the heights an animal could stand at in a column: the top of anything solid with 1.4 of free room above it
  const topsAt = (x, z) => {
    const known = columns.get(`${x},${z}`)
    if (known) return known
    const tops = []
    for (let y = feet.y - 6; y <= feet.y + 4; y++) {
      const shapes = shapesAt(x, y, z)
      if (!shapes.length) continue
      const top = y + Math.max(...shapes.map(shape => shape[4]))
      const free = [Math.floor(top), Math.floor(top + 1.4)].every(c => c <= y || !shapesAt(x, c, z).length)
      if (free) tops.push(top)
      // a full block carrying a fence, wall, pane or door: the post leaves a ledge an animal can stand on (Vivenna's second leak, see penLeak)
      else if (top === y + 1 && shapesAt(x, y + 1, z).length && [y + 1, y + 2].every(c => shapesAt(x, c, z).every(thin))) rims.set(`${x},${z}`, [...(rims.get(`${x},${z}`) ?? []), top])
    }
    columns.set(`${x},${z}`, tops)
    return tops
  }
  const rimsAt = (x, z) => { topsAt(x, z); return rims.get(`${x},${z}`) ?? [] }
  if (!topsAt(feet.x, feet.z).includes(feet.y)) return null
  const found = penLeak({ start: [feet.x, feet.y, feet.z], topsAt, rimsAt, radius, withFloor: true })
  // rock all round is no pen (Kettricken's cave pocket): `fenced` says a fence or wall stands beside the floor
  // topsAt goes back with it so a caller can ask penStance which side of the fence the walk started on (backlog #125)
  return found.enclosed ? { ...found, topsAt, fenced: fencedIn(found.floor, topsAt) } : { ...found, topsAt }
}
// after walking through a pen gate: farm animals standing outside it, within 6 blocks. The pen is whichever side of the gate is enclosed
export function straysAt (gateKey) {
  const gate = new Vec3(...gateKey.match(/-?\d+/g).map(Number))
  const pen = [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => penAround(gate.offset(dx, 0, dz))).find(found => found?.enclosed)
  if (!pen) return null
  const animals = Object.values(bot.entities).filter(e => BREEDING_FOOD[e.name]).map(e => ({ name: e.name, x: e.position.x, y: e.position.y, z: e.position.z }))
  return strays(pen.floor, animals, [gate.x, gate.y, gate.z])
}
// who is in a pen and who stands outside it, within 16 blocks of its floor: counted from the cells just walked, not judged by eye
export function censusOf (floor) {
  const cells = floor.map(k => k.split(',').map(Number))
  const close = e => cells.some(([x, , z]) => Math.abs(e.position.x - x) <= 16 && Math.abs(e.position.z - z) <= 16)
  return penCensus(floor, Object.values(bot.entities).filter(e => BREEDING_FOOD[e.name] && close(e)).map(e => ({ name: e.name, x: e.position.x, y: e.position.y, z: e.position.z })))
}
